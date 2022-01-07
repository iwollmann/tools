use crate::{format_elements, token, FormatElement, FormatResult, Formatter, ToFormatElement};
use rslint_parser::ast::JsDebuggerStatement;

impl ToFormatElement for JsDebuggerStatement {
	fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
		let semicolon = if let Some(semicolon) = self.semicolon_token() {
			formatter.format_token(&semicolon)?
		} else {
			token(";")
		};

		Ok(format_elements![
			formatter.format_token(&self.debugger_token()?)?,
			semicolon
		])
	}
}
