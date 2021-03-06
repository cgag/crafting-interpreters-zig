const std  = @import("std");
const warn = std.debug.warn;
const os   = std.os;
const fmt  = std.fmt;
const mem  = std.mem;
const io   = std.io;
const ArrayList = std.ArrayList;

const Token        = @import("lex.zig").Token;
const TokenType    = @import("lex.zig").TokenType;
const TokenLiteral = @import("lex.zig").Literal;

use @import("utils.zig");

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "false" | "true" | "nil"
//                | "(" expression ")" ;


// TODO(cgag): get rid of these globals
var alloc = &std.heap.DirectAllocator.init().allocator;

pub const ParserError = error{
    OutOfMemory,
    UnexpectedToken,
    ExpectExpression,
};

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
    // TODO(cgag): we could have more fine-grained types.  Operator
    // can not just be any given token.
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

// TODO(cgag): where  should this statement stuff go?
pub const Stmt = union(enum) {
    Expression: Expr,
    Print: Expr,
};

// caller owns returned memory
pub fn expr_print(a: *mem.Allocator, e: Expr) ![]const u8 {
    switch (e) {
        //  TODO(cgag): this expliity Expr.Binary shouldn't be necessary, should it?
        // It's not really ambiguous since e is known to be an Expr.
        Expr.Binary  => return try parenthesize(a, @tagName(e.Binary.operator.type), e),
        Expr.Literal => {
            switch(e.Literal.value) {
                TokenLiteral.Nil    => return try fmt.allocPrint(a, "{}",  "NIL"),
                TokenLiteral.Bool   => return try fmt.allocPrint(a, "{}",  e.Literal.value.Bool),
                TokenLiteral.String => return try fmt.allocPrint(a, "{}",  e.Literal.value.String),
                TokenLiteral.Number => return try fmt.allocPrint(a, "{.}", e.Literal.value.Number),
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
            warn("we're in grouping, what???\n");
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

    fn statement(self: *Parser) !Stmt {
        if (self.match([]TokenType{TokenType.PRINT})) {
            return self.print_statement();
        }
        return self.expression_statement();
    }

    fn print_statement(self: *Parser) !Stmt {
        const expr = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .Print = expr };
    }

    fn expression_statement(self: *Parser) !Stmt {
        const expr = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .Expression = expr };
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

        const target_tokens = []TokenType{TokenType.BANG_EQUAL,
                                          TokenType.EQUAL_EQUAL};
        while (self.match(target_tokens[0..])) {
            var operator = self.previous();
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
        const target_tokens = []TokenType{TokenType.BANG,TokenType.MINUS};
        if (self.match(target_tokens[0..])) {
            var operator = self.previous();
            var right  = try self.allocator.createOne(Expr);
            right.* = try self.unary();

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

        return tmp;
    }

    fn primary(self: *Parser) !Expr{
        const t_false            = []TokenType{TokenType.FALSE};
        const t_true             = []TokenType{TokenType.TRUE};
        const t_nil              = []TokenType{TokenType.NIL};
        const t_number_or_string = []TokenType{TokenType.NUMBER, TokenType.STRING};
        const t_left_paren       = []TokenType{TokenType.LEFT_PAREN};

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
        if (self.match(t_true))  { return lit_true; }
        if (self.match(t_nil))   { return lit_nil; }

        if (self.match(t_number_or_string)) {
            var prev = self.previous();
            return Expr{
                .Literal = Literal {
                    .value = prev.literal.?,
                }
            };
        }

        if (self.match(t_left_paren)) {
            var e = try self.allocator.createOne(Expr);
            e.* = try self.expression();

            _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
            return Expr {
                .Grouping = Grouping {
                    .expr = e,
                },
            };
        }

        parser_error(self.peek(), "expect expression");
        return ParserError.ExpectExpression;
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) {
            return self.advance();
        }

        parser_error(self.peek(), message);
        return ParserError.UnexpectedToken;
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

    fn synchronize(self: *Parser) void {
        _ = advance();
        while(!self.is_at_end()) {
            if (self.previous().type == TokenType.SEMICOLON) { return; }

            switch(self.peek().type) {
                TokenType.CLASS,
                TokenType.FUN,
                TokenType.VAR,
                TokenType.FOR,
                TokenType.IF,
                TokenType.WHILE,
                TokenType.PRINT,
                TokenType.RETURN => {
                    return;
                },
                _ => {
                    // nothing, keep eating tokens
                }
            }

            _ = advance();
        }
    }

    // TODO(cgag): caller owns returned arraylist, needs to call deinit
    pub fn parse(self: *Parser) !ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(self.allocator);
        while (!self.is_at_end()) {
            try statements.append(try self.statement());
        }
        return statements;
    }
};

fn parser_error(token: Token, message: []const u8) void {
    err(token.line, message);
}


test "parser whatever" {
    const Scanner = @import("lex.zig").Scanner;
    var src     = try io.readFileAlloc(alloc, "test/parse.lox");
    var scanner = try Scanner.init(alloc, src);
    var tokens  = try scanner.scan();
    // for (tokens.toSlice()) |t| {
    //     warn("{} ({})\n", @tagName(t.type), t.lexeme);
    // }
    var p = Parser.init(alloc, tokens);
    var parsed_statements  = p.parse() catch |e| {
        warn("hit parser error: {}", e);
        os.exit(65);
    };
    var printed_expr = try expr_print(alloc, parsed_statements.at(0).Expression);
    warn("\nprinted expr: {}\n", printed_expr);
}
