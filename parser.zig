const std  = @import("std");
const warn = std.debug.warn;
const fmt  = std.fmt;
const mem  = std.mem;

const Token        = @import("lex.zig").Token;
const TokenType    = @import("lex.zig").TokenType;
const TokenLiteral = @import("lex.zig").Literal;
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
    left: *Expr,
    right: *Expr,
    token: Token,
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
        ExprType.Binary => try parenthesize(a, @tagName(e.Binary.token.type), e),
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
    // TODO(cgag): how the hell do you free recursively allocated memory?
    const buf = switch (e) {
        ExprType.Binary => blk: {
            var left = try expr_print(a, e.Binary.left.*);
            var right = try expr_print(a, e.Binary.right.*);
            defer a.free(left);
            defer a.free(right);

            break :blk try fmt.allocPrint(a, "({} {} {})", name, left, right);
        },
        ExprType.Literal => blk: {
            // break :blk try fmt.allocPrint(a, "({})", e.Binary.token.type);
            unreachable;
        },
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

pub fn parse() !Expr {
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
            .token = Token.init(TokenType.AND, "and", null, 10),
        }
    };
    var e2 = Expr {
        .Unary = Unary {
            .right = &r,
            .token = Token.init(TokenType.MINUS, "-", null, 10),
        }
    };
    // TODO(cgag): expec
    // var s = try expr_print(alloc, e);
    var s = try expr_print(alloc, e2);
    warn("printed expr: {}\n", s);
    return e;
}
