const std       = @import("std");
const warn      = std.debug.warn;
const mem       = std.mem;
const os        = std.os;
const io        = std.io;
const ArrayList = std.ArrayList;
const Map       = std.AutoHashMap;

const globals   = @import("globals.zig");
const atof      = @import("atof.zig");

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

pub const LiteralType = enum {
    String,
    Number,
};

pub const Literal = union(LiteralType) {
    String: []const u8,
    Number: f64,
};

pub const Token = struct {
    type:    TokenType,
    lexeme:  []const u8,
    literal: ?Literal,
    line:    u32,

    pub fn init(token_type: TokenType,
                lexeme: []const u8,
                literal: ?Literal,
                line: u32) Token {
        return Token {
            .type    = token_type,
            .lexeme  = lexeme,
            .literal = literal,
            .line    = line,
        };
    }

    // caller needs to free.
    pub fn to_string(self: *Token, allocator: *mem.Allocator) ![]const u8 {
        const buf = try std.fmt.allocPrint(
            allocator,
            "type: {}, lexeme: {}",
            @tagName(self.type), self.lexeme
        );
        errdefer allocator.free(buf);

        return buf;
    }
};

pub const Scanner = struct {
    source:  []const u8,
    tokens:  ArrayList(Token),
    start:   u32,
    current: u32,
    line:    u32,
    keywords: Map([]const u8, TokenType),

    pub fn init(allocator: *mem.Allocator, source: []const u8) !Scanner {
        var keywords = Map([]const u8, TokenType).init(allocator);
        _ = try keywords.put("and",    TokenType.AND);
        _ = try keywords.put("class",  TokenType.CLASS);
        _ = try keywords.put("else",   TokenType.ELSE);
        _ = try keywords.put("false",  TokenType.FALSE);
        _ = try keywords.put("for",    TokenType.FOR);
        _ = try keywords.put("fun",    TokenType.FUN);
        _ = try keywords.put("if",     TokenType.IF);
        _ = try keywords.put("nil",    TokenType.NIL);
        _ = try keywords.put("or",     TokenType.OR);
        _ = try keywords.put("print",  TokenType.PRINT);
        _ = try keywords.put("return", TokenType.RETURN);
        _ = try keywords.put("super",  TokenType.SUPER);
        _ = try keywords.put("this",   TokenType.THIS);
        _ = try keywords.put("true",   TokenType.TRUE);
        _ = try keywords.put("var",    TokenType.VAR);
        _ = try keywords.put("while",  TokenType.WHILE);

        return Scanner {
            .source   = source,
            .tokens   = ArrayList(Token).init(allocator),
            .start    = 0,
            .current  = 0,
            .line     = 0,
            .keywords = keywords,
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.keywords.deinit();
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
            '(' => { try self.add_simple_token(TokenType.LEFT_PAREN);  },
            ')' => { try self.add_simple_token(TokenType.RIGHT_PAREN); },
            '{' => { try self.add_simple_token(TokenType.LEFT_BRACE);  },
            '}' => { try self.add_simple_token(TokenType.RIGHT_BRACE); },
            ',' => { try self.add_simple_token(TokenType.COMMA);       },
            '.' => { try self.add_simple_token(TokenType.DOT);         },
            '-' => { try self.add_simple_token(TokenType.MINUS);       },
            '+' => { try self.add_simple_token(TokenType.PLUS);        },
            ';' => { try self.add_simple_token(TokenType.SEMICOLON);   },
            '*' => { try self.add_simple_token(TokenType.STAR);        },

            '!' => {
                var token = if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
                try self.add_simple_token(token);
            },
            '=' => {
                var token = if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL;
                try self.add_simple_token(token);
             },
            '<' => {
                var token = if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS;
                try self.add_simple_token(token);
            },
            '>' => {
                var token = if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER;
                try self.add_simple_token(token);
            },

            // comments and whitespace
            '/' => {
                if (self.match('/')) {
                    // comment goes until end of the line
                    while (self.peek() != '\n' and !self.is_at_end()) {
                        _ = self.advance();
                    } else {
                        try self.add_simple_token(TokenType.SLASH);
                    }
                }
            },
            ' ' => {},
            '\r' => {},
            '\t' => {},
            '\n' => { self.line += 1; },

            '"' => { try self.string(); },
            '0'...'9' => {
                _ = try self.number();
            },

            else => {
                if (is_alpha(c))  {
                    try self.identifier_or_keyword();
                } else {
                    warn("unexpected char: {c}\n", c);
                    err(self.line, "Unexpected character");
                }
            },
        }
    }

    fn string(self: *Scanner) !void {
        while (self.peek() != '"' and !self.is_at_end()) {
            if (self.peek() == '\n') { self.line += 1; }
            _ = self.advance();
        }

        if (self.is_at_end()) {
            err(self.line, "unterminated string");
            return;
        }

        // consume closing ", TODO(cgag): assert(match('"'))?
        _ = self.advance();

        const lit = Literal { .String = self.source[self.start+1..self.current-1] };
        try self.add_token(TokenType.STRING, lit);
    }

    fn identifier_or_keyword(self: *Scanner) !void {
        while (is_alphanumeric(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];

        // check for reserved words
        // TODO(cgag): lowercase??
        if (self.keywords.get(text)) |reserved_token_type_kv| {
            try self.add_simple_token(reserved_token_type_kv.value);
        } else {
            try self.add_simple_token(TokenType.IDENTIFIER);
        }
    }

    fn number(self: *Scanner) !void {
        while (is_digit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.') {
            // consume '.'
            _ = self.advance();
            while (is_digit(self.peek())) {
                _ = self.advance();
            }
        }

        try self.add_token(
            TokenType.NUMBER,
            Literal { .Number = try atof.atof(self.source[self.start..self.current]) },
        );
    }

    fn is_at_end(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Scanner, char: u8) bool {
        if (self.is_at_end()) return false;
        if (self.source[self.current] != char) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: *Scanner) u8 {
        if (self.is_at_end()) { return 0; }
        return self.source[self.current];
    }

    fn add_simple_token(self: *Scanner, token_type: TokenType) !void {
        try self.add_token(token_type, null);
    }

    fn add_token(self: *Scanner, token_type: TokenType, literal: ?Literal) !void {
        var text = self.source[self.start..self.current];
        try self.tokens.append(Token.init(token_type, text, literal, self.line));
    }
};


fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
           (c >= 'A' and c <= 'Z') or
           (c == '_');
}

fn is_alphanumeric(c: u8) bool {
    return is_alpha(c) or is_digit(c);
}

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
