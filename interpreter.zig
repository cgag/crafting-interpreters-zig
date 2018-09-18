const std = @import("std");
const assert = std.debug.assert;
const warn = std.debug.warn;
const mem = std.mem;
const fmt = std.fmt;

const Expr = @import("parser.zig").Expr;
const E    = @import("parser.zig");
const TokenLiteral = @import("lex.zig").Literal;
const TT    = @import("lex.zig").TokenType;

const NilStruct = struct{};

const LoxVal = union(enum) {
    Number: f64,
    Bool: bool,
    Nil: NilStruct,
    String: []const u8,

    fn equal(self: *LoxVal, o: LoxVal) bool {
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
};

fn evaluate(alloc: *mem.Allocator, e: Expr) EvalError!LoxVal {
    switch(e) {
        Expr.Literal => {
            switch(e.Literal.value) {
                TokenLiteral.String => return LoxVal{ .String = e.Literal.value.String },
                TokenLiteral.Number => return LoxVal{ .Number = e.Literal.value.Number },
                TokenLiteral.Bool   => return LoxVal{ .Bool   = e.Literal.value.Bool } ,
                TokenLiteral.Nil    => return LoxVal{ .Nil    = NilStruct{} },
            }
        },

        Expr.Unary => {
            const right_val = try evaluate(alloc, e.Unary.right.*);
            switch(e.Unary.operator.type) {
                TT.MINUS => {
                    switch(right_val) {
                        LoxVal.Number => { return LoxVal{.Number = -right_val.Number}; },
                        // TODO(cgag): error handling
                        else => unreachable,
                    }
                },
                TT.BANG => {
                    switch(right_val) {
                        LoxVal.Bool   => { return LoxVal{.Bool = !right_val.Bool };    },
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
                            // when are we supposed to call free?
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
                TT.EQUAL_EQUAL   => return lox_bool(left_val.Number == right_val.Number),
                TT.BANG_EQUAL    => return lox_bool(left_val.Number != right_val.Number),

                else => unreachable,
            }
        },
        else => unreachable,
    }

    unreachable;
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

    unreachable;
}

test "interpreter" {
    assert(is_truthy(LoxVal{.Nil  = NilStruct{}}) == false);
    assert(is_truthy(LoxVal{.Bool = false})       == false);

    assert(is_truthy(LoxVal{ .Bool   = true }));
    assert(is_truthy(LoxVal{ .Number = 0 }));
    assert(is_truthy(LoxVal{ .Number = 1 }));

    assert((try eval_str("true")).equal(lox_bool(true)));
    assert((try eval_str("false")).equal(lox_bool(false)));
    assert((try eval_str("!false")).equal(lox_bool(true)));
    assert((try eval_str("!true")).equal(lox_bool(false)));
    assert((try eval_str("1")).equal(lox_num(1)));
    assert((try eval_str("-1")).equal(lox_num(-1)));

    assert((try eval_str("10 > 11")).equal(lox_bool(false)));
    assert((try eval_str("10 < 11")).equal(lox_bool(true)));
    assert((try eval_str("10 >= 10")).equal(lox_bool(true)));
    assert((try eval_str("10 <= 10")).equal(lox_bool(true)));
    assert((try eval_str("10 <= 11")).equal(lox_bool(true)));
    assert((try eval_str("10 <= 9")).equal(lox_bool(false)));

    assert((try eval_str("10 == 9")).equal(lox_bool(false)));
    assert((try eval_str("10 == 10")).equal(lox_bool(true)));
    assert((try eval_str("10 != 10")).equal(lox_bool(false)));
    assert((try eval_str("10 != 9")).equal(lox_bool(true)));

    assert((try eval_str("1+1")).equal(lox_num(2)));
    assert((try eval_str("2*3")).equal(lox_num(6)));
    assert((try eval_str("6/3")).equal(lox_num(2)));
    assert((try eval_str("6-3")).equal(lox_num(3)));

    assert((try eval_str("\"hello\" + \" world\"")).equal(lox_str("hello world")));
}

fn eval_str(src: []const u8) !LoxVal {
    const Scanner = @import("lex.zig").Scanner;
    const Parser  = @import("parser.zig").Parser;
    var alloc = &std.heap.DirectAllocator.init().allocator;

    var scanner  = try Scanner.init(alloc, src);
    const tokens = try scanner.scan();
    var p = Parser.init(alloc, tokens);
    const e = try p.parse();
    return evaluate(alloc, e);
}
