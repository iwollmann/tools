use crate::parser::{ParsedSyntax, ParserProgress};
use crate::state::{
	AllowObjectExpression, ChangeParserState, InAsync, InConstructor, InFunction, InGenerator,
	NewLabelsScope,
};
use crate::syntax::binding::{
	parse_binding, parse_binding_pattern, parse_binding_pattern_with_optional_default,
};
use crate::syntax::expr::parse_expr_or_assignment;
use crate::syntax::js_parse_error;
use crate::syntax::js_parse_error::expected_binding;
use crate::syntax::stmt::{is_semi, parse_block_impl};
use crate::syntax::typescript::{
	maybe_eat_incorrect_modifier, ts_type, ts_type_or_type_predicate_ann, ts_type_params,
};
use crate::JsSyntaxFeature::TypeScript;
use crate::ParsedSyntax::{Absent, Present};
use crate::{Marker, ParseRecovery, Parser, SyntaxFeature};
use bitflags::bitflags;
use rslint_syntax::JsSyntaxKind::*;
use rslint_syntax::{JsSyntaxKind, T};

/// A function declaration, this could be async and or a generator. This takes a marker
/// because you need to first advance over async or start a marker and feed it in.
// test function_decl
// function foo() {}
// function *foo() {}
// async function *foo() {}
// async function foo() {}
// function *foo() {
//   yield foo;
// }
//
// test function_declaration_script
// // SCRIPT
// function test(await) {}
//
// test_err function_decl_err
// function() {}
// function foo {}
// function {}
// function *() {}
// async function() {}
// async function *() {}
// function *foo() {}
// yield foo;
// function test(): number {}
// function foo(await) {}
// function foo(yield) {}
//
// test_err function_broken
// function foo())})}{{{  {}
pub(super) fn parse_function_statement(p: &mut Parser) -> ParsedSyntax {
	let m = p.start();
	parse_function(p, m, FunctionKind::Statement)
}

pub(super) fn parse_function_expression(p: &mut Parser) -> ParsedSyntax {
	let m = p.start();
	parse_function(p, m, FunctionKind::Expression)
}

// test export_function_clause
// export function test(a, b) {}
// export function* test(a, b) {}
// export async function test(a, b, ) {}
pub(super) fn parse_export_function_clause(p: &mut Parser) -> ParsedSyntax {
	let m = p.start();
	parse_function(p, m, FunctionKind::Export)
}

// test export_default_function_clause
// export default function test(a, b) {}
pub(super) fn parse_export_default_function_case(p: &mut Parser) -> ParsedSyntax {
	if !(p.at(T![default]) || p.nth_at(1, T![function]) || p.nth_src(1) == "async") {
		return Absent;
	}

	let m = p.start();
	p.bump(T![default]);
	parse_function(p, m, FunctionKind::ExportDefault)
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum FunctionKind {
	Statement,
	Expression,
	Export,
	ExportDefault,
}

impl FunctionKind {
	fn is_id_optional(&self) -> bool {
		matches!(self, FunctionKind::Expression | FunctionKind::ExportDefault)
	}
}

impl From<FunctionKind> for JsSyntaxKind {
	fn from(kind: FunctionKind) -> Self {
		match kind {
			FunctionKind::Statement => JS_FUNCTION_STATEMENT,
			FunctionKind::Expression => JS_FUNCTION_EXPRESSION,
			FunctionKind::Export => JS_EXPORT_FUNCTION_CLAUSE,
			FunctionKind::ExportDefault => JS_EXPORT_DEFAULT_FUNCTION_CLAUSE,
		}
	}
}

fn parse_function(p: &mut Parser, m: Marker, kind: FunctionKind) -> ParsedSyntax {
	let uses_invalid_syntax =
		kind == FunctionKind::Statement && p.eat(T![declare]) && TypeScript.is_unsupported(p);
	let mut flags = SignatureFlags::empty();

	let in_async = is_at_async_function(p, LineBreak::DoNotCheck);
	if in_async {
		p.bump_remap(T![async]);
		flags |= SignatureFlags::ASYNC;
	}

	p.expect(T![function]);

	if p.eat(T![*]) {
		flags |= SignatureFlags::GENERATOR;
	}

	let id = parse_function_id(p);

	if !kind.is_id_optional() {
		id.or_add_diagnostic(p, |p, range| {
			p.err_builder(
				"expected a name for the function in a function declaration, but found none",
			)
			.primary(range, "")
		});
	}

	TypeScript
		.parse_exclusive_syntax(p, parse_ts_parameter_types, |p, marker| {
			p.err_builder("type parameters can only be used in TypeScript files")
				.primary(marker.range(p), "")
		})
		.ok();

	parse_parameter_list(p, flags).or_add_diagnostic(p, js_parse_error::expected_parameters);

	TypeScript
		.parse_exclusive_syntax(p, parse_ts_type_annotation_or_error, |p, marker| {
			p.err_builder("return types can only be used in TypeScript files")
				.primary(marker.range(p), "")
		})
		.ok();

	if kind == FunctionKind::Statement {
		function_body_or_declaration(p, flags);
	} else {
		parse_function_body(p, flags).or_add_diagnostic(p, js_parse_error::expected_function_body);
	}

	let mut function = m.complete(p, kind.into());

	if uses_invalid_syntax {
		function.change_to_unknown(p);
	}

	Present(function)
}

pub(super) fn parse_function_body(p: &mut Parser, flags: SignatureFlags) -> ParsedSyntax {
	p.with_state(
		InFunction(true)
			.and(InConstructor(flags.contains(SignatureFlags::CONSTRUCTOR)))
			.and(InAsync(flags.contains(SignatureFlags::ASYNC)))
			.and(InGenerator(flags.contains(SignatureFlags::GENERATOR)))
			.and(NewLabelsScope),
		|p| parse_block_impl(p, JS_FUNCTION_BODY),
	)
}

// test function_id
// // SCRIPT
// function test() {}
// function await(test) {}
// async function await(test) {}
// function yield(test) {}
// function* yield(test) {}
// async function test() {
//   function await(test) {}
// }
pub(super) fn parse_function_id(p: &mut Parser) -> ParsedSyntax {
	p.with_state(InAsync(false).and(InGenerator(false)), parse_binding)
}

// TODO 1725 This is probably not ideal (same with the `declare` keyword). We should
// use a different AST type for function declarations. For example, a function declaration should
// never have a body but that would be allowed with this approach. Same for interfaces, interface
// methods should never have a body
/// Either parses a typescript declaration body or the function body
pub(super) fn function_body_or_declaration(p: &mut Parser, flags: SignatureFlags) {
	// omitting the body is allowed in ts
	if p.typescript() && !p.at(T!['{']) && is_semi(p, 0) {
		p.eat(T![;]);
	} else {
		let body = parse_function_body(p, flags);
		body.or_add_diagnostic(p, js_parse_error::expected_function_body);
	}
}

pub(crate) fn parse_ts_parameter_types(p: &mut Parser) -> ParsedSyntax {
	if p.at(T![<]) {
		Present(ts_type_params(p).unwrap())
	} else {
		Absent
	}
}

pub(crate) fn ts_parameter_types(p: &mut Parser) {
	if p.at(T![<]) {
		if let Some(ref mut ty) = ts_type_params(p) {
			ty.err_if_not_ts(p, "type parameters can only be used in TypeScript files");
		}
	}
}

pub(crate) fn parse_ts_type_annotation_or_error(p: &mut Parser) -> ParsedSyntax {
	if p.at(T![:]) {
		let return_type = p.start();
		if let Some(ref mut ty) = ts_type_or_type_predicate_ann(p, T![:]) {
			ty.err_if_not_ts(p, "return types can only be used in TypeScript files");
		}
		Present(return_type.complete(p, TS_TYPE_ANNOTATION))
	} else {
		Absent
	}
}

/// Tells [is_at_async_function] if it needs to check line breaks
#[derive(PartialEq)]
#[repr(u8)]
pub(super) enum LineBreak {
	// check line breaks
	DoCheck,
	// do not check line break
	DoNotCheck,
}

#[inline]
/// Checks if the parser is inside a "async function"
pub(super) fn is_at_async_function(p: &mut Parser, should_check_line_break: LineBreak) -> bool {
	let async_function_tokens = p.cur_src() == "async" && p.nth_at(1, T![function]);
	if should_check_line_break == LineBreak::DoCheck {
		async_function_tokens && !p.has_linebreak_before_n(1)
	} else {
		async_function_tokens
	}
}

pub(super) fn parse_arrow_body(p: &mut Parser, flags: SignatureFlags) -> ParsedSyntax {
	if p.at(T!['{']) {
		parse_function_body(p, flags)
	} else {
		p.with_state(
			InFunction(true)
				.and(InAsync(flags.contains(SignatureFlags::ASYNC)))
				.and(InGenerator(false)),
			parse_expr_or_assignment,
		)
	}
}

#[allow(clippy::unnecessary_unwrap)]
pub(super) fn parse_formal_param_pat(p: &mut Parser) -> ParsedSyntax {
	if p.typescript() {
		if let Some(modifier) = maybe_eat_incorrect_modifier(p) {
			let err = p
				.err_builder("modifiers on parameters are only allowed in constructors")
				.primary(modifier.range(p), "");

			p.error(err);
		}
	}

	parse_binding_pattern_with_optional_default(p)
}

// test parameter_list
// function evalInComputedPropertyKey({ [computed]: ignored }) {}
/// parse the whole list of parameters, brackets included
pub(super) fn parse_parameter_list(p: &mut Parser, flags: SignatureFlags) -> ParsedSyntax {
	if !p.at(T!['(']) {
		return Absent;
	}
	let m = p.start();
	p.with_state(
		InAsync(flags.contains(SignatureFlags::ASYNC))
			.and(InGenerator(flags.contains(SignatureFlags::GENERATOR))),
		|p| {
			parse_parameters_list(p, parse_formal_param_pat, JS_PARAMETER_LIST);
		},
	);

	Present(m.complete(p, JS_PARAMETERS))
}

/// Parses a (param, param) list into the current active node
pub(super) fn parse_parameters_list(
	p: &mut Parser,
	parse_param: impl Fn(&mut Parser) -> ParsedSyntax,
	list_kind: JsSyntaxKind,
) {
	let mut first = true;
	let has_l_paren = p.expect(T!['(']);

	p.with_state(AllowObjectExpression(has_l_paren), |p| {
		let parameters_list = p.start();
		let mut progress = ParserProgress::default();

		while !p.at(EOF) && !p.at(T![')']) {
			progress.assert_progressing(p);

			if first {
				first = false;
			} else {
				p.expect(T![,]);
			}

			if p.at(T![')']) {
				break;
			}

			if p.at(T![...]) {
				let m = p.start();
				p.bump_any();
				parse_binding_pattern(p).or_add_diagnostic(p, expected_binding);

				// TODO #1725 Review error handling and recovery
				// rest patterns cannot be optional: `...foo?: number[]`
				if p.at(T![?]) {
					let err = p
						.err_builder("rest patterns cannot be optional")
						.primary(p.cur_tok().range(), "");

					p.error(err);
					let m = p.start();
					p.bump_any();
					m.complete(p, JS_UNKNOWN_BINDING);
				}

				// type annotation `...foo: number[]`
				if p.eat(T![:]) {
					let complete = ts_type(p);
					if let Some(mut res) = complete {
						res.err_if_not_ts(
							p,
							"type annotations can only be used in TypeScript files",
						);
					}
				}

				if p.at(T![=]) {
					let start = p.cur_tok().start();
					let m = p.start();
					p.bump_any();

					let end = parse_expr_or_assignment(&mut *p)
						.ok()
						.map(|marker| usize::from(marker.range(p).end()))
						.unwrap_or_else(|| p.cur_tok().start());

					let err = p
						.err_builder("rest elements may not have default initializers")
						.primary(start..end, "");

					p.error(err);
					m.complete(p, JS_UNKNOWN);
				}

				m.complete(p, JS_REST_PARAMETER);

				// FIXME: this should be handled better, we should keep trying to parse params but issue an error for each one
				// which would allow for better recovery from `foo, ...bar, foo`
				if p.at(T![,]) {
					let m = p.start();
					let range = p.cur_tok().range();
					p.bump_any();
					m.complete(p, JS_UNKNOWN);
					let err = p
						.err_builder("rest elements may not have trailing commas")
						.primary(range, "");

					p.error(err);
				}
			} else {
				// test_err formal_params_no_binding_element
				// function foo(true) {}

				// test_err formal_params_invalid
				// function (a++, c) {}
				let recovered_result = parse_param(p).or_recover(
					p,
					&ParseRecovery::new(
						JS_UNKNOWN_BINDING,
						token_set![
							T![ident],
							T![await],
							T![yield],
							T![,],
							T!['['],
							T![...],
							T![')'],
						],
					)
					.enable_recovery_on_line_break(),
					js_parse_error::expected_parameter,
				);

				if recovered_result.is_err() {
					break;
				}
			}
		}

		parameters_list.complete(p, list_kind);
	});

	p.expect(T![')']);
}

bitflags! {
	pub(crate) struct SignatureFlags: u8 {
		const ASYNC 			= 0b00001;
		const GENERATOR 	= 0b00010;
		const CONSTRUCTOR = 0b00100;
	}
}
