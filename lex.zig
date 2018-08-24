const std  = @import("std");
const warn = std.debug.warn;
const mem  = std.mem;
const os   = std.os;
const io   = std.io;
const ArrayList = @import("std").ArrayList;
const globals = @import("globals.zig");

pub var global: u32 = 2;

pub const TokenType = enum {
    // single character
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // one or two characters
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // literals
    IDENTIFIER, STRING, NUMBER,

    // keywords
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF
};

pub var x: u32 = 0;

pub fn global_var_test() void {
    warn("x: {}\n", x);
    warn("had-erro: {}\n", main.had_error);
    main.had_error = true;
}

pub const Token = struct {
    type: TokenType,
    lexeme:  []const u8,
    literal: ?[]const u8, // not a string, to be cast on use.  Union?
    line: u32,

    pub fn init(token_type: TokenType,
                lexeme: []const u8,
                literal: ?[]const u8,
                line: u32) Token {
        return Token {
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    // caller needs to free.
    pub fn to_string(self: *Token, allocator: *mem.Allocator) ![]const u8 {
        var buf = try allocator.alloc(u8,
                                      "type: ".len +
                                      @tagName(self.type).len +
                                      " lexeme: ".len +
                                      self.lexeme.len);
        const typeName =  @tagName(self.type);
        var i: u32 = 0;
        mem.copy(u8, buf, "type: ");
        i += @intCast(u32, "type: ".len);
        mem.copy(u8, buf[i..], typeName);
        i += @intCast(u32, typeName.len);
        mem.copy(u8, buf[i..], " lexeme: ");
        i += @intCast(u32, " lexeme: ".len);
        mem.copy(u8, buf[i..], self.lexeme);
        return buf;
    }
};

pub const Scanner = struct {
    source: []const u8,
    tokens: ArrayList(Token),
    start: u32,
    current: u32,
    line: u32,

    pub fn init(allocator: *mem.Allocator, source: []const u8) Scanner {
        return Scanner {
            .source  = source,
            .tokens  = ArrayList(Token).init(allocator),
            .start   = 0,
            .current = 0,
            .line    = 0,
        };
    }

    pub fn scan(self: *Scanner) !ArrayList(Token) {
        // TODO(cgag): actually do the scanning
        while (!self.is_at_end()) {
            self.start = self.current;
            try self.scan_token();
        }
        try self.tokens.append(Token.init(TokenType.EOF, "", null, self.line));
        return self.tokens;
    }

    fn scan_token(self: *Scanner) !void {
        var c = self.advance();
        switch (c) {
            '(' => {
                try self.add_simple_token(TokenType.LEFT_PAREN);
            },
            ')' => {
                try self.add_simple_token(TokenType.RIGHT_PAREN);
            },
            '{' => {
                try self.add_simple_token(TokenType.LEFT_BRACE);
            },
            '}' => {
                try self.add_simple_token(TokenType.RIGHT_BRACE);
            },
            ',' => {
                try self.add_simple_token(TokenType.COMMA);
            },
            '.' => {
                try self.add_simple_token(TokenType.DOT);
            },
            '-' => {
                try self.add_simple_token(TokenType.MINUS);
            },
            '+' => {
                try self.add_simple_token(TokenType.PLUS);
            },
            ';' => {
                try self.add_simple_token(TokenType.SEMICOLON);
            },
            '*' => {
                try self.add_simple_token(TokenType.STAR);
            },
            else => {
                // TODO(cgag):  error handling
                err(self.line, "Unexpected character");
            },
        }
    }

    fn is_at_end(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn add_simple_token(self: *Scanner, token_type: TokenType) !void {
        try self.add_token(token_type, null);
    }

    fn add_token(self: *Scanner, token_type: TokenType, literal: ?[]const u8) !void {
        var text = self.source[self.start..self.current];
        try self.tokens.append(Token.init(token_type, text, literal, self.line));
    }
};


fn err(line_number: i64, msg: []const u8) void {
    report(line_number, "", msg);
}

fn report(line_number: i64, location: []const u8, msg: []const u8) void {
    if (location.len == 0) {
        warn("[line {}] Error: {}\n", line_number, msg);
    } else {
        warn("[line {}] Error ({}): {}\n", line_number, location, msg);
    }
    globals.had_error = true;
}
