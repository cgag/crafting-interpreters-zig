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


pub const ParserError = error{
    OutOfMemory
};

pub const ExprType = enum {
    Binary,
    Literal,
    Grouping,
    Unary,
};

pub const Expr = union(ExprType) {
    Binary:   Binary,
    Literal:  Literal,
    Grouping: Grouping,
    Unary:    Unary,
};

// TODO(cgag): if left and right aren't pointers, we get the error
// parser.zig:17:20: error: struct 'Binary' contains itself, which sort of
// makes sense, but it's indirect and confusing.
// TODO(cgag): how do we free this
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
    operator: Token,
    right: *Expr,
};

// caller owns returned memory
pub fn expr_print(a: *mem.Allocator, e: Expr) ![]const u8 {
    switch (e) {
        ExprType.Binary  => return try parenthesize(a, @tagName(e.Binary.operator.type), e),
        ExprType.Literal => {
            switch(e.Literal.value) {
                TokenLiteralType.String => return try fmt.allocPrint(a, "{}", e.Literal.value.String),
                TokenLiteralType.Number => return try fmt.allocPrint(a, "{.}", e.Literal.value.Number),
                TokenLiteralType.Bool   => return try fmt.allocPrint(a, "{}", e.Literal.value.Bool),
                TokenLiteralType.Nil    => return try fmt.allocPrint(a, "{}", "NIL"),
            }
        },
        ExprType.Grouping => return try parenthesize(a, "group", e),
        ExprType.Unary    => return try parenthesize(a, e.Unary.operator.lexeme, e),
    }
}

// caller owns returned memory
pub fn parenthesize(a: *mem.Allocator, name: []const u8, e: Expr) fmt.AllocPrintError![]const u8 {
    // TODO(cgag): is there a zig equiv to stringbuilder?
    const buf = switch (e) {
        ExprType.Binary => blk: {
            var left  = try expr_print(a, e.Binary.left.*);
            var right = try expr_print(a, e.Binary.right.*);
            defer a.free(left);
            defer a.free(right);
            break :blk try fmt.allocPrint(a, "({} {} {})", name, left, right);
        },
        ExprType.Literal => unreachable,
        ExprType.Grouping => blk: {
            warn("how'd we even get in here??\n");
            warn("grouping: {}", e);
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
    allocator: *mem.Allocator,

    pub fn init(allocator: *mem.Allocator, tokens: ArrayList(Token)) Parser {
        return Parser{
            .tokens    = tokens,
            .current   = 0,
            .allocator = allocator,
        };
    }

    // TODO(cgag): maybe these should all be returning *Expr, not Expr?
    fn expression(self: *Parser) !Expr {
        return try self.equality();
    }

    // TODO(cgag): how to recursively free all these?
    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(self: *Parser) !Expr {
        var left = try self.allocator.createOne(Expr);
        left.* = try self.comparison();
        warn("left: {}\n", left);

        const target_tokens = []TokenType{TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL};
        while (self.match(target_tokens[0..])) {
            var operator = self.previous();
            warn("previous: {}\n", operator);
            var right = try self.allocator.createOne(Expr);
            right.* = try self.comparison();

            var new_left = try self.allocator.createOne(Expr);
            new_left.* = Expr {
                .Binary = Binary {
                    .left = left,
                    .operator = operator,
                    .right = right,
                },
            };

            left = new_left;
        }

        return left.*;
    }

    fn comparison(self: *Parser) !Expr {
        warn("in comparision\n");
        var e = try self.allocator.createOne(Expr);
        e.* = try self.addition();

        const target_tokens = []TokenType{
            TokenType.GREATER,
            TokenType.GREATER_EQUAL,
            TokenType.LESS,
            TokenType.LESS_EQUAL,
        };
        while(self.match(target_tokens[0..])) {
            var operator = self.previous();
            var right = try self.allocator.createOne(Expr);
            right.* = try self.addition();
            warn("e currently: {}", e);

            var new_e =  try self.allocator.createOne(Expr);
            new_e.* = Expr {
                .Binary = Binary {
                    .left = e,
                    .operator = operator,
                    .right = right,
                }
            };
            e = new_e;
        }

        return e.*;
    }

    fn addition(self: *Parser) !Expr {
        warn("in addition\n");
        var e = try self.allocator.createOne(Expr);
        e.* = try self.multiplication();

        const target_tokens = []TokenType{TokenType.MINUS, TokenType.PLUS};
        while (self.match(target_tokens[0..])) {
            var operator = self.previous();
            var right  = try self.allocator.createOne(Expr);
            right.* = try self.multiplication();

            var new_e = try self.allocator.createOne(Expr);
            new_e.* = Expr {
                .Binary = Binary {
                    .left = e,
                    .operator = operator,
                    .right = right,
                }
            };
            e = new_e;
        }

        return e.*;
    }

    fn multiplication(self: *Parser) !Expr {
        warn("in multiplication\n");
        var e = try self.allocator.createOne(Expr);
        e.* = try self.unary();

        const target_tokens = []TokenType{TokenType.STAR, TokenType.SLASH};
        while (self.match(target_tokens[0..])) {
            var operator = self.previous();
            var right  = try self.allocator.createOne(Expr);
            right.* = try self.unary();
            var new_e  = try self.allocator.createOne(Expr);
            new_e.* = Expr {
                .Binary = Binary {
                    .left = e,
                    .operator = operator,
                    .right = right,
                }
            };
            e = new_e;
        }

        return e.*;
    }

    fn unary(self: *Parser) ParserError!Expr {
        warn("in unary\n");
        const target_tokens = []TokenType{TokenType.BANG,TokenType.MINUS};
        if (self.match(target_tokens[0..])) {
            var operator = self.previous();
            var right  = try self.allocator.createOne(Expr);
            // TODO(cgag): zig doesn't like this recursion because it can't infer the type of the
            // errorset yet.  How to make it explitily the global error set?
            right.* = try self.unary();

            return Expr {
                .Unary = Unary {
                    .operator = operator,
                    .right = right,
                }
            };
        }

        return self.primary();
    }

    fn primary(self: *Parser) !Expr{
        warn("in primary\n");

        var t_false            = []TokenType{TokenType.FALSE};
        var t_true             = []TokenType{TokenType.TRUE};
        var t_nil              = []TokenType{TokenType.NIL};
        var t_number_or_string = []TokenType{TokenType.NUMBER, TokenType.STRING};
        var t_left_paren       = []TokenType{TokenType.LEFT_PAREN};

        const lit_false = Expr {
            .Literal = Literal {
                .value = TokenLiteral {
                    .Bool = false,
                }
            }
        };
        const lit_true = Expr {
            .Literal = Literal {
                .value = TokenLiteral {
                    .Bool = true,
                }
            }
        };
        const lit_nil = Expr {
            .Literal = Literal {
                .value = TokenLiteral {
                    .Nil = true,
                }
            }
        };

        if (self.match(t_false)) { return lit_false; }
        if (self.match(t_true))  { return lit_true;  }
        if (self.match(t_nil))   { return lit_nil;   }

        if (self.match(t_number_or_string)) {
            // TODO(cgag): holy shit are we fucked here?
            warn("it's a number or a string\n");
            var prev = self.previous();
            warn("prev: {}\n", prev);
            return Expr{
                .Literal = Literal {
                    .value = prev.literal.?,
                }
            };
        }

        if (self.match(t_left_paren)) {
            var e = try self.allocator.createOne(Expr);
            e.* = try self.expression();

            _ = self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
            return Expr {
                .Grouping = Grouping {
                    .expr = e,
                },
            };
        }

        // TODO(cgag): delete
        return lit_false;
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) Token {
        if (self.check(token_type)) {
            return self.advance();
        }

        unreachable;
        // TODO(cgag): implement the rest
    }

    // TODO(cgag): varargs
    fn match(self: *Parser, target_types: []const TokenType) bool {
        for (target_types) |target_type| {
            if (self.check(target_type)) {
                // consume it
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, target_type: TokenType) bool {
        if (self.is_at_end()) { return false; }
        return self.peek().type == target_type;
    }

    fn previous(self: *Parser) Token {
        return self.tokens.at(self.current - 1);
    }

    fn advance(self: *Parser) Token {
        if (!self.is_at_end()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn is_at_end(self: *Parser) bool {
        return self.peek().type == TokenType.EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens.at(self.current);
    }

    pub fn parse(self: *Parser) !Expr {
        return try self.expression();
    }
};


test "parser whatever" {
    // var l = Expr{
    //     .Literal = Literal {
    //         .value = TokenLiteral{ .String = "left-value" },
    //     }
    // };
    // var r = Expr{
    //     .Literal = Literal {
    //         .value = TokenLiteral{ .Number = 20.0 },
    //     }
    // };
    // var e = Expr {
    //     .Binary = Binary{
    //         .left  = &l,
    //         .right = &r,
    //         .operator = Token.init(TokenType.AND, "+", null, 10),
    //     }
    // };
    // var s = try expr_print(alloc, e);
    // warn("\n--\nprinted expr: {}\n", s);

    {
        const Scanner = @import("lex.zig").Scanner;
        var src     = try io.readFileAlloc(alloc, "test/parse.lox");
        var scanner = try Scanner.init(alloc, src);
        var tokens  = try scanner.scan();
        for (tokens.toSlice()) |t| {
            warn("{} ({})\n", @tagName(t.type), t.lexeme);
        }
        var p = Parser.init(alloc, tokens);
        var parsed_expr  = try p.parse();
        warn("parsed expr: {}\n", parsed_expr);
        var printed_expr = try expr_print(alloc, parsed_expr);
        warn("printed real expr: {}\n", printed_expr);
    }
}
