const Token = @import("lex.zig").Token;

pub var had_error: bool = false;
pub var had_runtime_error: bool = false;
pub var type_error_token: Token = undefined;
pub var undefined_variable_lexeme: []const u8 = undefined;
