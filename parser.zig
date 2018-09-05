const std  = @import("std");
const warn = std.debug.warn;
const fmt  = std.fmt;
const mem  = std.mem;
const io   = std.io;
const ArrayList = std.ArrayList;

const Token            = @import("lex.zig").Token;
const TokenType        = @import("lex.zig").TokenType;
const TokenLiteral     = @import("lex.zig").Literal;
const TokenLiteralType = @import("lex.zig").LiteralType;

// TODO(cgag): get rid of these globals
var alloc = &std.heap.DirectAllocator.init().allocator;

pub const ExprType = enum {
    Binary,
    Literal,
    Grouping,
    Unary,
};

pub const Expr = union(ExprType) {
    Binary: Binary,
    Literal: Literal,
    Grouping: Grouping,
    Unary: Unary,
};

// TODO(cgag): if left and right aren't pointers, we get the error
// parser.zig:17:20: error: struct 'Binary' contains itself, which sort of
// makes sense, but it's indirect and confusing.
pub const Binary = struct {
    left:  *Expr,
    right: *Expr,
    operator: Token,
};

pub const Literal = struct {
    value: TokenLiteral,
};

pub const Grouping = struct {
    expr: *Expr,
};

pub const Unary = struct {
    token: Token,
    right: *Expr,
};

// caller owns returned memory
pub fn expr_print(a: *mem.Allocator, e: Expr) ![]const u8 {
    const s = switch (e) {
        ExprType.Binary => try parenthesize(a, @tagName(e.Binary.operator.type), e),
        ExprType.Literal => blk: {
            switch(e.Literal.value) {
                TokenLiteralType.String => {
                    break :blk try fmt.allocPrint(a, "{}", e.Literal.value.String);
                },
                TokenLiteralType.Number => {
                    break :blk try fmt.allocPrint(a, "{.}", e.Literal.value.Number);
                }
            }
        },
        ExprType.Grouping => try parenthesize(a, "group", e),
        ExprType.Unary => try parenthesize(a, e.Unary.token.lexeme, e),
    };
    return s;
}

// caller owns returned memory
pub fn parenthesize(a: *mem.Allocator, name: []const u8, e: Expr) fmt.AllocPrintError![]const u8 {
    // TODO(cgag): is there a zig equiv to stringbuilder?
    const buf = switch (e) {
        ExprType.Binary => blk: {
            var left = try expr_print(a, e.Binary.left.*);
            var right = try expr_print(a, e.Binary.right.*);
            defer a.free(left);
            defer a.free(right);

            break :blk try fmt.allocPrint(a, "({} {} {})", name, left, right);
        },
        ExprType.Literal => unreachable,
        ExprType.Grouping => blk: {
            var printed_expr = try expr_print(a, e.Grouping.expr.*);
            defer a.free(printed_expr);
            break :blk try fmt.allocPrint(a, "({} {})", name, printed_expr);
        },
        ExprType.Unary => blk: {
            var right = try expr_print(a, e.Unary.right.*);
            defer a.free(right);
            break :blk try fmt.allocPrint(a, "({} {})", name, right);
        }
    };

    return buf;
}

pub const Parser = struct {
    tokens: ArrayList(Token),
    current: u64,

    pub fn init(tokens: ArrayList(Token)) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
        };
    }

    fn expression(self: *Parser) Expr {
        return self.equality();
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    // TODO(cgag): need to think about how all this memory is going to work,
    // I think we're putting things like "e" on the stack and taking their address.  Dangerous.
    fn equality(self: *Parser) Expr {
        var e = self.comparison();

        const target_tokens = []TokenType{TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL};
        while (self.match(target_tokens[0..])) {
            var operator: Token = self.previous();
            var right = self.comparison();
            e = Expr {
                    .Binary = Binary {
                        .left = &e,
                        .operator = operator,
                        .right = &right,
                    },
                };
        }

        return e;
    }

    fn comparison(self: *Parser) Expr {
        return Expr{.Literal=Literal{.value=TokenLiteral{.String="hello"}}};
    }

    // TODO(cgag): varargs
    fn match(self: *Parser, token_targets: []const TokenType) bool {
        return false;
    }

    fn check(self: *Parser, type: TokenType) bool {
        if (self.is_at_end()) { return false; }
        return self.peek().type == type;
    }

    fn previous(self: *Parser) Token {
        return self.tokens.at(self.current - 1);
    }

    fn advance(self: *Parser) void {
        if (!self.is_at_end) {
            self.current += 1;
        }
        return self.previous();
    }

    fn is_at_end(self: *Parser) bool {
        return self.peek().type == TokenType.EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens.at(current);
    }

    pub fn parse(self: *Parser) Expr {
        return self.expression();
    }
};


test "parser whatever" {
    var l = Expr{
        .Literal = Literal {
            .value = TokenLiteral{ .String = "left-value" },
        }
    };
    var r = Expr{
        .Literal = Literal {
            .value = TokenLiteral{ .Number = 20.0 },
        }
    };
    var e = Expr {
        .Binary = Binary{
            .left  = &l,
            .right = &r,
            .operator = Token.init(TokenType.AND, "+", null, 10),
        }
    };
    var s = try expr_print(alloc, e);
    warn("\n--\nprinted expr: {}\n", s);

    {
        const Scanner = @import("lex.zig").Scanner;
        var src     = try io.readFileAlloc(alloc, "test/parse.lox");
        var scanner = try Scanner.init(alloc, src);
        var tokens  = try scanner.scan();
        var p       = Parser.init(tokens);
        var parsed_expr = p.parse();
        var printed_expr = try expr_print(alloc, parsed_expr);
        warn("printed real expr: {}\n", printed_expr);
    }
}
