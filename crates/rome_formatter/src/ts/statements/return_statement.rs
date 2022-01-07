use crate::{
	concat_elements, space_token, token, FormatElement, FormatResult, Formatter, ToFormatElement,
};
use rslint_parser::ast::JsReturnStatement;

impl ToFormatElement for JsReturnStatement {
	fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
		let mut tokens = vec![formatter.format_token(&self.return_token()?)?];

		if let Some(argument) = self.argument() {
			tokens.push(space_token());
			tokens.push(formatter.format_node(argument)?);
		}

		if let Some(semicolon) = self.semicolon_token() {
			tokens.push(formatter.format_token(&semicolon)?);
		} else {
			tokens.push(token(";"));
		}

		Ok(concat_elements(tokens))
	}
}
