use rslint_parser::ast::JsExpressionStatement;

use crate::{format_elements, token, FormatElement, FormatResult, Formatter, ToFormatElement};

impl ToFormatElement for JsExpressionStatement {
	fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
		let semicolon = if let Some(semicolon) = self.semicolon_token() {
			formatter.format_token(&semicolon)?
		} else {
			token(";")
		};

		Ok(format_elements![
			formatter.format_node(self.expression()?)?,
			semicolon
		])
	}
}
