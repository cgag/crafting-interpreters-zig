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


fn evaluate(alloc: *mem.Allocator, e: Expr) LoxVal {
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
            const right_val = evaluate(e.Unary.right.*);
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
            const left_val  = evaluate(e.Binary.left.*);
            const right_val = evaluate(e.Binary.right.*);

            switch(e.Binary.operator.type) {
                TT.STAR  => return left_val.Number * right_val.Number,
                TT.SLASH => return left_val.Number / right_val.Number,
                TT.MINUS => return left_val.Number - right_val.Number,
                // TODO(cgag): string cat operator?
                TT.PLUS => {
                    switch(left_val) {
                        LoxVal.Number => return left_val.Number + right_val.Number,
                        LoxVal.String => {
                            // TODO(cgag): i think we're leaking memory all over the fuckin place.
                            // Does it matter?  Can you call free on stack allocated strings?
                            // when are we supposed to call free?
                            const new_s = fmt.allocPrint(alloc, "{}{}", left_val.String, right_val.String);
                        }
                    }
                }

                TT.GREATER => return left_val.Bool > right_val.Bool,
                TT.LESS    => return left_val.Bool < right_val.Bool,
                TT.GREATER_EQUAL => return left_val.Bool >= right_val.Bool,
                TT.LESS_EQUAL    => return left_val.Bool <= right_val.Bool,
                TT.EQUAL_EQUAL   => return left_val.Bool == right_val.Bool,
                TT.BANG_EQUAL    => return left_val.Bool == right_val.Bool,
            }
        }

        else => unreachable,
    }
    unreachable;
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

    assert((try eval_str("true")).equal(LoxVal{.Bool = true}));
    assert((try eval_str("false")).equal(LoxVal{.Bool = false}));
    assert((try eval_str("!false")).equal(LoxVal{.Bool = true}));
    assert((try eval_str("!true")).equal(LoxVal{.Bool = false}));
    assert((try eval_str("1")).equal(LoxVal{.Number = 1}));
    assert((try eval_str("-1")).equal(LoxVal{.Number = -1}));
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
