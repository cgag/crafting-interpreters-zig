const std = @import("std");
const assert = std.debug.assert;
const warn = std.debug.warn;
const mem = std.mem;
const fmt = std.fmt;
const os  = std.os;

const Expr    = @import("parser.zig").Expr;
const Stmt    = @import("parser.zig").Stmt;
const E       = @import("parser.zig");
const Token   = @import("lex.zig").Token;
const TT      = @import("lex.zig").TokenType;
const globals = @import("globals.zig");
const TokenLiteral = @import("lex.zig").Literal;

const NilStruct = struct{};



const LoxVal = union(enum) {
    Number: f64,
    Bool: bool,
    Nil: NilStruct,
    String: []const u8,

    // TODO(cgag): i don't think i should have to write this?
    fn equal(self: *const LoxVal, o: LoxVal) bool {
        // TODO(cgag): should o be passed as a pointer?
        switch(self.*) {
            LoxVal.Number => switch(o) {
                LoxVal.Number => return self.Number == o.Number,
                else => return false,
            },
            LoxVal.Bool => switch(o) {
                LoxVal.Bool => return self.Bool == o.Bool,
                else => return false,
            },
            LoxVal.Nil => switch(o) {
                LoxVal.Nil => return true,
                else => return false,
            },
            LoxVal.String => switch(o) {
                LoxVal.String => return mem.eql(u8, self.String, o.String),
                else => return false,
            }
        }
    }
};

// have to wrap global error set values because evaluate is recursive
const EvalError = error {
    OutOfMemory,
    TypeErrorNumbers,
    TypeErrorStrings,
    TypeErrorPlusInvalidType,
};

pub fn execute(alloc: *mem.Allocator, s: Stmt) !void {
    switch(s) {
        Stmt.Print => {
            var val = try evaluate(alloc, s.Print);
            warn("PRINTING: {}\n", val);
        },
        Stmt.Expression => {
            _ = try evaluate(alloc, s.Expression);
            return;
        },
    }
}

pub fn evaluate(alloc: *mem.Allocator, e: Expr) EvalError!LoxVal {
    switch(e) {
        Expr.Literal => {
            switch(e.Literal.value) {
                TokenLiteral.String => return LoxVal{ .String = e.Literal.value.String },
                TokenLiteral.Number => return LoxVal{ .Number = e.Literal.value.Number },
                TokenLiteral.Bool   => return LoxVal{ .Bool   = e.Literal.value.Bool } ,
                // TODO(cgag): I think we can just make the type .Nil be void?
                TokenLiteral.Nil    => return LoxVal{ .Nil    = NilStruct{} },
            }
        },

        Expr.Unary => {
            const right_val = try evaluate(alloc, e.Unary.right.*);
            switch(e.Unary.operator.type) {
                TT.MINUS => {
                    switch(right_val) {
                        LoxVal.Number => { return lox_num(-right_val.Number); },
                        // TODO(cgag): error handling
                        else => unreachable,
                    }
                },
                TT.BANG => {
                    switch(right_val) {
                        LoxVal.Bool   => { return lox_bool(!right_val.Bool); },
                        // TODO(cgag): error handling
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },

        Expr.Binary => {
            const left_val  = try evaluate(alloc, e.Binary.left.*);
            const right_val = try evaluate(alloc, e.Binary.right.*);

            try type_check_binary(e.Binary.operator, left_val, right_val);

            switch(e.Binary.operator.type) {
                TT.STAR  => return lox_num(left_val.Number * right_val.Number),
                TT.SLASH => return lox_num(left_val.Number / right_val.Number),
                TT.MINUS => return lox_num(left_val.Number - right_val.Number),
                // TODO(cgag): string cat operator?
                TT.PLUS => {
                    switch(left_val) {
                        LoxVal.Number => return lox_num(left_val.Number + right_val.Number),
                        LoxVal.String => {
                            // TODO(cgag): i think we're leaking memory all over the fuckin place.
                            // Does it matter?  Can you call free on stack allocated strings?
                            // when are we supposed to call ree?
                            const new_s = try fmt.allocPrint(alloc, "{}{}", left_val.String, right_val.String);
                            return lox_str(new_s);
                        },
                        else => unreachable, // type error
                    }
                },

                TT.GREATER       => return lox_bool(left_val.Number > right_val.Number),
                TT.LESS          => return lox_bool(left_val.Number < right_val.Number),
                TT.GREATER_EQUAL => return lox_bool(left_val.Number >= right_val.Number),
                TT.LESS_EQUAL    => return lox_bool(left_val.Number <= right_val.Number),

                TT.EQUAL_EQUAL   => return lox_bool(are_equal(left_val, right_val)),
                TT.BANG_EQUAL    => return lox_bool(!are_equal(left_val, right_val)),

                else => unreachable,
            }
        },
        else => unreachable,
    }

    unreachable;
}

fn type_check_binary(token: Token, left: LoxVal, right: LoxVal) !void {
    globals.type_error_token = token;
    switch(token.type) {
        TT.STAR,
        TT.SLASH,
        TT.MINUS,
        TT.GREATER,
        TT.LESS,
        TT.GREATER_EQUAL,
        TT.LESS_EQUAL => {
            return number_operands(token, left, right);
        },

        // TODO(cgag): do something here
        TT.PLUS => {
            switch(left) {
                LoxVal.Number => return number_operands(token, left, right),
                LoxVal.String => return string_operands(token, left, right),
                else => {
                    return error.TypeErrorPlusInvalidType;
                }
            }
        },

        else => return,
    }
}

fn number_operands(token: Token, left: LoxVal, right: LoxVal) !void {
    // TODO(cgag): setting here again is redundant
    globals.type_error_token = token;
    switch(left) {
        LoxVal.Number => {
            switch(right) {
                LoxVal.Number => return,
                else => return EvalError.TypeErrorNumbers
            }
        },
        else => return EvalError.TypeErrorNumbers,
    }
}

fn string_operands(token: Token, left: LoxVal, right: LoxVal) !void {
    // TODO(cgag): setting here again is redundant
    globals.type_error_token = token;
    switch(left) {
        LoxVal.String => {
            switch(right) {
                LoxVal.String => return,
                else => return EvalError.TypeErrorStrings
            }
        },
        else => return EvalError.TypeErrorStrings,
    }
}


pub fn report_runtime_error(e: error) void {
    globals.had_runtime_error = true;
    const msg = switch(e) {
        error.TypeErrorNumbers => "both operands must be numbers",
        error.TypeErrorStrings => "both operands must be strings",
        error.TypeErrorPlusInvalidType => "+ operator only works on strings and numbers",
        else => "some other error, idk",
    };
    warn("[line: {}] {}\n", globals.type_error_token.line, msg);
}

// lox_val.equal is just for comparing two enums in zig, this is for
// testing using actual lox semantics
// TODO(cgag): actually, are they the same anyway? I thinks so.
fn are_equal(left: LoxVal, right: LoxVal) bool {
    return left.equal(right);
}

fn lox_num(n: f64) LoxVal {
    return LoxVal{.Number=n};
}

fn lox_str(s: []const u8) LoxVal {
    return LoxVal{.String=s};
}

fn lox_bool(b: bool) LoxVal {
    return LoxVal{.Bool=b};
}

fn is_truthy(value: LoxVal) bool {
    switch(value) {
        LoxVal.Bool => return value.Bool,
        LoxVal.Nil  => return false,
        else => return true,
    }
}

// TODO(cgag): don't just dump everything into one test
test "interpreter" {
    assert(is_truthy(LoxVal{.Nil  = NilStruct{}}) == false);
    assert(is_truthy(LoxVal{.Bool = false})       == false);

    assert(is_truthy(LoxVal{ .Bool   = true }));
    assert(is_truthy(LoxVal{ .Number = 0 }));
    assert(is_truthy(LoxVal{ .Number = 1 }));

    // TODO(cgag): table and loop
    assert((try eval_str("true;")).equal(lox_bool(true)));
    assert((try eval_str("false;")).equal(lox_bool(false)));
    assert((try eval_str("!false;")).equal(lox_bool(true)));
    assert((try eval_str("!true;")).equal(lox_bool(false)));
    assert((try eval_str("1;")).equal(lox_num(1)));
    assert((try eval_str("-1;")).equal(lox_num(-1)));

    assert((try eval_str("10 > 11;")).equal(lox_bool(false)));
    assert((try eval_str("10 < 11;")).equal(lox_bool(true)));
    assert((try eval_str("10 >= 10;")).equal(lox_bool(true)));
    assert((try eval_str("10 <= 10;")).equal(lox_bool(true)));
    assert((try eval_str("10 <= 11;")).equal(lox_bool(true)));
    assert((try eval_str("10 <= 9;")).equal(lox_bool(false)));

    assert((try eval_str("10 == 9;")).equal(lox_bool(false)));
    assert((try eval_str("10 == 10;")).equal(lox_bool(true)));
    assert((try eval_str("10 != 10;")).equal(lox_bool(false)));
    assert((try eval_str("10 != 9;")).equal(lox_bool(true)));

    assert((try eval_str("1+1;")).equal(lox_num(2)));
    assert((try eval_str("2*3;")).equal(lox_num(6)));
    assert((try eval_str("6/3;")).equal(lox_num(2)));
    assert((try eval_str("6-3;")).equal(lox_num(3)));

    assert((try eval_str("\"hello\" + \" world\";")).equal(lox_str("hello world")));

    // _ = eval_str("4 * \"hello\"") catch |e| {
    //     report_runtime_error(globals.type_error_token, e);
    //     // os.exit(64);
    //     // switch(e) {
    //     //     error.TypeError => {
    //     //         warn("\ntype error baby: {}\n", @tagName(globals.type_error_token.type));
    //     //         os.exit(64);
    //     //     },
    //     //     else => unreachable,
    //     // }
    // };
}

fn eval_str(src: []const u8) !LoxVal {
    const Scanner = @import("lex.zig").Scanner;
    const Parser  = @import("parser.zig").Parser;
    var alloc = &std.heap.DirectAllocator.init().allocator;

    var scanner  = try Scanner.init(alloc, src);
    const tokens = try scanner.scan();
    var p = Parser.init(alloc, tokens);
    const statements = try p.parse();
    return evaluate(alloc, statements.at(0).Expression);
}
