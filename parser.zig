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

// TODO(cgag): i think we can just put union(enum) and the compiler will 
// create these types implicitly
// pub const ExprType = enum {
//     Binary,
//     Literal,
//     Grouping,
//     Unary,
// };

pub const Expr = union(enum) {
    Binary:   Binary,
    Literal:  Literal,
    Grouping: Grouping,
    Unary:    Unary,
};

// TODO(cgag): need to be able to free
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
        //  TODO(cgag): this expliity Expr.Binary shouldn't be necessary, should it?
        // It's not really ambiguous since e is known to be an Expr.
        Expr.Binary  => return try parenthesize(a, @tagName(e.Binary.operator.type), e),
        Expr.Literal => {
            warn("literal branch of expr_print: {}\n", e);
            switch(e.Literal.value) {
                TokenLiteralType.Nil    => return try fmt.allocPrint(a, "{}", "NIL"),
                TokenLiteralType.Bool   => {
                    warn("literal bool\n");
                    return try fmt.allocPrint(a, "{}", e.Literal.value.Bool);
                },
                TokenLiteralType.String => return try fmt.allocPrint(a, "{}", e.Literal.value.String),
                TokenLiteralType.Number => {
                    warn("literal number\n");
                    return try fmt.allocPrint(a, "{.}", e.Literal.value.Number);
                },
            }
        },
        Expr.Grouping => return try parenthesize(a, "group", e),
        Expr.Unary    => return try parenthesize(a, e.Unary.operator.lexeme, e),
    }
}

// caller owns returned memory
pub fn parenthesize(a: *mem.Allocator, name: []const u8, e: Expr) fmt.AllocPrintError![]const u8 {
    const buf = switch (e) {
        Expr.Binary => blk: {
            var left  = try expr_print(a, e.Binary.left.*);
            var right = try expr_print(a, e.Binary.right.*);
            defer a.free(left);
            defer a.free(right);
            break :blk try fmt.allocPrint(a, "({} {} {})", name, left, right);
        },
        Expr.Literal => unreachable,
        Expr.Grouping => blk: {
            warn("how'd we even get in here??\n");
            warn("grouping: {}", e);
            var printed_expr = try expr_print(a, e.Grouping.expr.*);
            defer a.free(printed_expr);
            break :blk try fmt.allocPrint(a, "({} {})", name, printed_expr);
        },
        Expr.Unary => blk: {
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
            // TODO(cgag): wtf, right is coming out as binary??
            right.* = try self.unary();

            // TODO(cgag): how do recursiion work here, we're doing something wrong for sure.
            return Expr {
                .Unary = Unary {
                    .operator = operator,
                    .right = right,
                }
            };
        }

        const tmp = try self.primary();
        const tmp_s = try expr_print(alloc, tmp);
        defer alloc.free(tmp_s);

        const lit_true = Expr {
            .Literal = Literal {
                .value = TokenLiteral {
                    .Bool = true,
                }
            }
        };
        // warn("tmp primary 0: {}\n", lit_true);
        // warn("tmp primary 1: {}\n", tmp_s);
        // warn("tmp primary 2: {}\n", tmp);
        return tmp;
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

        // inlining this "true" instead of creating a var triggers a bug
        var t = true;
        const lit_true = Expr {
            .Literal = Literal {
                .value = TokenLiteral {
                    .Bool = t,
                }
            }
        };

        // TODO(cgag): this works, but not .Bool!??
        // const lit_true = Expr {
        //     .Literal = Literal {
        //         .value = TokenLiteral {
        //             .Number = 1,
        //         }
        //     }
        // };


        const lit_nil = Expr {
            .Literal = Literal {
                .value = TokenLiteral {
                    .Nil = true,
                }
            }
        };

        if (self.match(t_false)) {
            warn("it's literally false\n");
            return lit_false;
        }

        if (self.match(t_true)) {
            warn("it's literally true\n");
            return lit_true;
        }
        if (self.match(t_nil)) {
            warn("it's literally nil\n");
            return lit_nil;
        }

        if (self.match(t_number_or_string)) {
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


test "parser whatever\n" {

//     // TODO(cgag): this works, but not .Bool!??
//     const test_lit_true_int = Expr {
//         .Literal = Literal {
//             .value = TokenLiteral {
//                 .Number = 1,
//             }
//         }
//     };
//     var printed_lit_int  = try expr_print(alloc, test_lit_true_int);
//     defer alloc.free(printed_lit_int);
//     warn("printed_lit_int: {}\n", printed_lit_int);

//     const test_lit_true_bool = Expr {
//         .Literal = Literal {
//             .value = TokenLiteral {
//                 .Bool = true,
//             }
//         }
//     };
//     var printed_lit_bool = try expr_print(alloc, test_lit_true_bool);
//     defer alloc.free(printed_lit_bool);
//     warn("printed_lit_bool: {}\n", printed_lit_bool);


//     const boolPrint = try fmt.allocPrint(alloc, "{}", true);
//     warn("bool print: {}\n", boolPrint);

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
