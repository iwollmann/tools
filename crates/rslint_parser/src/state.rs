use crate::{FileKind, Parser, Syntax};
use bitflags::bitflags;
use num_bigint::Sign;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Range};

/// State kept by the parser while parsing.
/// It is required for things such as strict mode or async functions
#[derive(Debug, PartialEq)]
pub struct ParserState {
	parsing_context: ParsingContextFlags,
	/// A list of labels for labelled statements used to report undefined label errors
	/// for break and continue, as well as duplicate labels
	pub labels: HashMap<String, Range<usize>>,
	/// Whether we are in strict mode code
	strict: Option<StrictMode>,
	/// The exported default item, used for checking duplicate defaults
	pub default_item: Option<Range<usize>>,
	/// If set, the parser reports bindings with identical names. The option stores the name of the
	/// node that disallows duplicate bindings, for example `let`, `const` or `import`.
	pub duplicate_binding_parent: Option<&'static str>,
	pub name_map: HashMap<String, Range<usize>>,
	pub(crate) no_recovery: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StrictMode {
	Module,
	Explicit(Range<usize>),
	Class(Range<usize>),
}

impl Default for ParserState {
	fn default() -> Self {
		Self {
			parsing_context: ParsingContextFlags::ALLOW_OBJECT_EXPRESSION
				| ParsingContextFlags::INCLUDE_IN,
			labels: HashMap::new(),
			strict: None,
			default_item: None,
			name_map: HashMap::with_capacity(3),
			duplicate_binding_parent: None,
			no_recovery: false,
		}
	}
}

impl ParserState {
	pub fn new(syntax: Syntax) -> Self {
		// TODO(RDambrosio016): Does TypeScript imply Module/Strict?
		let strict = if syntax.file_kind == FileKind::Module {
			Some(StrictMode::Module)
		} else {
			None
		};

		Self {
			strict,
			..ParserState::default()
		}
	}

	pub fn in_function(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::IN_FUNCTION)
	}

	pub fn in_generator(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::IN_GENERATOR)
	}

	pub fn in_async(&self) -> bool {
		self.parsing_context.contains(ParsingContextFlags::IN_ASYNC)
	}

	pub fn in_constructor(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::IN_CONSTRUCTOR)
	}

	pub fn continue_allowed(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::CONTINUE_ALLOWED)
	}
	pub fn break_allowed(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::BREAK_ALLOWED)
	}

	pub fn include_in(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::INCLUDE_IN)
	}

	pub fn in_condition_expression(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::IN_CONDITION_EXPRESSION)
	}

	pub fn potential_arrow_start(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::POTENTIAL_ARROW_START)
	}

	pub fn allow_object_expression(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::ALLOW_OBJECT_EXPRESSION)
	}

	pub fn strict(&self) -> Option<&StrictMode> {
		self.strict.as_ref()
	}

	pub fn in_binding_list_for_signature(&self) -> bool {
		self.parsing_context
			.contains(ParsingContextFlags::IN_BINDING_LIST_FOR_SIGNATURE)
	}
}

impl<'t> Parser<'t> {
	/// Applies the passed in change to the parser's state and reverts the
	/// changes when the returned [ParserStateGuard] goes out of scope.
	pub fn with_scoped_state<'p, C: ChangeParserState>(
		&'p mut self,
		change: C,
	) -> ParserStateGuard<'p, 't, C> {
		let snapshot = change.apply(&mut self.state);
		ParserStateGuard::new(self, snapshot)
	}

	/// Applies the passed in change to the parser state before applying the passed `func` and
	/// restores the state to before the change before returning the result.
	#[inline]
	pub fn with_state<C, F, R>(&mut self, change: C, func: F) -> R
	where
		C: ChangeParserState,
		F: FnOnce(&mut Parser) -> R,
	{
		let snapshot = change.apply(&mut self.state);
		let result = func(self);
		C::restore(&mut self.state, snapshot);
		result
	}
}

/// Reverts state changes to their previous value when it goes out of scope.
/// Can be used like a regular parser.
pub struct ParserStateGuard<'parser, 't, C>
where
	C: ChangeParserState,
{
	snapshot: C::Snapshot,
	inner: &'parser mut Parser<'t>,
}

impl<'parser, 't, C: ChangeParserState> ParserStateGuard<'parser, 't, C> {
	fn new(parser: &'parser mut Parser<'t>, snapshot: C::Snapshot) -> Self {
		Self {
			snapshot,
			inner: parser,
		}
	}
}

impl<'parser, 't, C: ChangeParserState> Drop for ParserStateGuard<'parser, 't, C> {
	fn drop(&mut self) {
		let snapshot = std::mem::take(&mut self.snapshot);

		C::restore(&mut self.inner.state, snapshot);
	}
}

impl<'parser, 't, C: ChangeParserState> Deref for ParserStateGuard<'parser, 't, C> {
	type Target = Parser<'t>;

	fn deref(&self) -> &Self::Target {
		self.inner
	}
}

impl<'parser, 't, C: ChangeParserState> DerefMut for ParserStateGuard<'parser, 't, C> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.inner
	}
}

/// Implements a specific modification to the parser state that can later be reverted.
pub trait ChangeParserState {
	type Snapshot: Default;

	/// Applies the change to the passed in state and returns snapshot that allows restoring the previous state.
	fn apply(self, state: &mut ParserState) -> Self::Snapshot;

	/// Restores the state to its previous value
	fn restore(state: &mut ParserState, value: Self::Snapshot);

	/// Allows composing two changes.
	/// The returned change first applies this modifier and then `other`.
	fn and<O>(self, other: O) -> ComposedParserStateChange<Self, O>
	where
		Self: Sized,
		O: ChangeParserState,
	{
		ComposedParserStateChange::new(self, other)
	}
}

#[derive(Debug, Default)]
pub struct ComposedSnapshot<A, B>
where
	A: Default,
	B: Default,
{
	a: A,
	b: B,
}

#[derive(Debug)]
pub struct ComposedParserStateChange<A, B> {
	a: A,
	b: B,
}

impl<A, B> ComposedParserStateChange<A, B>
where
	A: ChangeParserState,
	B: ChangeParserState,
{
	pub fn new(a: A, b: B) -> Self {
		Self { a, b }
	}
}

impl<A, B> ChangeParserState for ComposedParserStateChange<A, B>
where
	A: ChangeParserState,
	B: ChangeParserState,
{
	type Snapshot = ComposedSnapshot<A::Snapshot, B::Snapshot>;

	fn apply(self, state: &mut ParserState) -> Self::Snapshot {
		ComposedSnapshot {
			a: self.a.apply(state),
			b: self.b.apply(state),
		}
	}

	fn restore(state: &mut ParserState, value: Self::Snapshot) {
		B::restore(state, value.b);
		A::restore(state, value.a);
	}
}

/// Macro for creating a [ChangeParserState] that changes the value of a single [ParserState] field.
/// * `$name`: The name of the [ChangeParserState] implementation
/// * `$field`: The [ParserState] field's name that the implementation *changes*
/// * `$type`: The [ParserState] field's type
/// * `snapshot`: The name of the snapshot struct
macro_rules! gen_change_parser_state {
	($name:ident, $flag:expr) => {
		/// Changes the [ParserState] `$field` field
		#[derive(Debug)]
		pub(crate) struct $name(pub(crate) bool);

		impl ChangeParserState for $name {
			type Snapshot = ParsingContextFlagsSnapshot;

			#[inline]
			fn apply(self, state: &mut ParserState) -> Self::Snapshot {
				let new_flags = if self.0 {
					state.parsing_context | $flag
				} else {
					state.parsing_context - $flag
				};
				ParsingContextFlagsSnapshot(std::mem::replace(
					&mut state.parsing_context,
					new_flags,
				))
			}

			#[inline]
			fn restore(state: &mut ParserState, value: Self::Snapshot) {
				state.parsing_context = value.0
			}
		}
	};
}

gen_change_parser_state!(InGenerator, ParsingContextFlags::IN_GENERATOR);
gen_change_parser_state!(InAsync, ParsingContextFlags::IN_ASYNC);
gen_change_parser_state!(BreakAllowed, ParsingContextFlags::BREAK_ALLOWED);
gen_change_parser_state!(ContinueAllowed, ParsingContextFlags::CONTINUE_ALLOWED);
gen_change_parser_state!(IncludeIn, ParsingContextFlags::INCLUDE_IN);
gen_change_parser_state!(
	InConditionExpression,
	ParsingContextFlags::IN_CONDITION_EXPRESSION
);
gen_change_parser_state!(
	AllowObjectExpression,
	ParsingContextFlags::ALLOW_OBJECT_EXPRESSION
);
gen_change_parser_state!(
	InBindingListForSignature,
	ParsingContextFlags::IN_BINDING_LIST_FOR_SIGNATURE
);
gen_change_parser_state!(
	PotentialArrowStart,
	ParsingContextFlags::POTENTIAL_ARROW_START
);

#[derive(Debug, Clone, Default)]
pub struct SeenLabelsSnapshot(HashMap<String, Range<usize>>);

/// Resets the [ParserState] `labels` field to an empty map
pub struct NewLabelsScope;

impl ChangeParserState for NewLabelsScope {
	type Snapshot = SeenLabelsSnapshot;

	#[inline]
	fn apply(self, state: &mut ParserState) -> Self::Snapshot {
		SeenLabelsSnapshot(std::mem::take(&mut state.labels))
	}

	#[inline]
	fn restore(state: &mut ParserState, value: Self::Snapshot) {
		state.labels = value.0
	}
}

#[derive(Default, Debug)]
pub struct EnableStrictModeSnapshot(Option<StrictMode>);

/// Enables strict mode
pub struct EnableStrictMode(pub StrictMode);

impl ChangeParserState for EnableStrictMode {
	type Snapshot = EnableStrictModeSnapshot;

	#[inline]
	fn apply(self, state: &mut ParserState) -> Self::Snapshot {
		EnableStrictModeSnapshot(std::mem::replace(&mut state.strict, Some(self.0)))
	}

	#[inline]
	fn restore(state: &mut ParserState, value: Self::Snapshot) {
		state.strict = value.0
	}
}

bitflags! {
	pub(crate) struct SignatureFlags: u8 {
		const ASYNC 			= 0b00001;
		const GENERATOR 	= 0b00010;
		const CONSTRUCTOR = 0b00100;
	}
}

impl SignatureFlags {
	pub fn to_parsing_context_flags(self) -> ParsingContextFlags {
		let mut flags = ParsingContextFlags::empty();

		if self.contains(SignatureFlags::ASYNC) {
			flags |= ParsingContextFlags::IN_ASYNC;
		}

		if self.contains(SignatureFlags::GENERATOR) {
			flags |= ParsingContextFlags::IN_GENERATOR;
		}

		if self.contains(SignatureFlags::CONSTRUCTOR) {
			flags |= ParsingContextFlags::IN_CONSTRUCTOR;
		}

		flags
	}
}

bitflags! {
	#[derive(Default)]
	struct ParsingContextFlags: u16 {
		/// Whether the parser is in a generator function like `function* a() {}`
		const IN_GENERATOR 									= 0b0000000000000001;
		/// Whether the parser is inside of a function
		const IN_FUNCTION  									= 0b0000000000000010;
		/// Whatever the parser is inside of a constructor
		const IN_CONSTRUCTOR								= 0b0000000000000100;

		/// Is async allowed in this context. Either because it's an async function or top level await is supported
		const IN_ASYNC 											= 0b0000000000001000;
		const IN_PARAMETERS 								= 0b0000000000010000;

		/// Whether `in` should be counted in a binary expression
		/// this is for `for...in` statements to prevent ambiguity.
		const INCLUDE_IN 										= 0b0000000000100000;
		const IN_BINDING_LIST_FOR_SIGNATURE = 0b0000000001000000;
		/// Whether the parser is in a conditional expr (ternary expr)
		const IN_CONDITION_EXPRESSION				= 0b0000000010000000;

		/// Whether the parser is in an iteration or switch statement and
		/// `break` is allowed.
		const BREAK_ALLOWED 								= 0b0000000100000000;

		/// Whether the parser is in an iteration statement and `continue` is allowed.
		const CONTINUE_ALLOWED 							= 0b0000001000000000;

		/// If false, object expressions are not allowed to be parsed
		/// inside an expression.
		///
		/// Also applies for object patterns
		const ALLOW_OBJECT_EXPRESSION 			= 0b0000010000000000;

		/// Whether we potentially are in a place to parse an arrow expression
		const POTENTIAL_ARROW_START 				= 0b0000100000000000;

		const LOOP = Self::BREAK_ALLOWED.bits | Self::CONTINUE_ALLOWED.bits;
		const FUNCTION_RESET_MASK = Self::BREAK_ALLOWED.bits | Self::CONTINUE_ALLOWED.bits | Self::IN_CONSTRUCTOR.bits | Self::IN_ASYNC.bits | Self::IN_GENERATOR.bits;
	}
}

#[derive(Debug, Default, Copy, Clone)]
pub(crate) struct ParsingContextFlagsSnapshot(ParsingContextFlags);

#[derive(Debug, Clone, Default)]
pub struct EnterFunctionSnapshot {
	parsing_context: ParsingContextFlags,
	labels: HashMap<String, Range<usize>>,
}

pub(crate) struct EnterFunction(pub(crate) SignatureFlags);

impl ChangeParserState(SignatureFlags) for EnterFunction {
	type Snapshot = EnterFunctionSnapshot;

	#[inline]
	fn apply(self, state: &mut ParserState) -> Self::Snapshot {
		let new_flags = (state.parsing_context - ParsingContextFlags::FUNCTION_RESET_MASK)
			| ParsingContextFlags::IN_FUNCTION
			| self.0.to_parsing_context_flags();

		EnterFunctionSnapshot {
			parsing_context: std::mem::replace(&mut state.parsing_context, new_flags),
			labels: std::mem::take(&mut state.name_map),
		}
	}

	#[inline]
	fn restore(state: &mut ParserState, value: Self::Snapshot) {
		state.parsing_context = value.parsing_context;
		state.labels = value.labels;
	}
}

pub(crate) struct EnterParameters(pub(crate) SignatureFlags);

impl ChangeParserState for EnterParameters {
	type Snapshot = ParsingContextFlagsSnapshot;

	fn apply(self, state: &mut ParserState) -> Self::Snapshot {
		let flags = (state.parsing_context
			- ParsingContextFlags::FUNCTION_RESET_MASK
			- ParsingContextFlags::IN_FUNCTION)
			| ParsingContextFlags::IN_PARAMETERS
			| self.0.to_parsing_context_flags();

		ParsingContextFlagsSnapshot(std::mem::replace(&mut state.parsing_context, flags))
	}

	fn restore(state: &mut ParserState, value: Self::Snapshot) {
		state.parsing_context = value.0;
	}
}
