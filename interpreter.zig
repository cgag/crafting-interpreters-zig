const std = @import("std");
const assert = std.debug.assert;
const warn = std.debug.warn;
const mem = std.mem;

const Expr = @import("parser.zig").Expr;
const E    = @import("parser.zig");
const TokenLiteral = @import("lex.zig").Literal;
const TokenType    = @import("lex.zig").TokenType;

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


fn evaluate(e: Expr) LoxVal {
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
                TokenType.MINUS => {
                    switch(right_val) {
                        LoxVal.Number => { return LoxVal{.Number = -right_val.Number}; },
                        // TODO(cgag): error handling
                        else => unreachable,
                    }
                },
                TokenType.BANG => {
                    switch(right_val) {
                        LoxVal.Bool   => { return LoxVal{.Bool = !right_val.Bool };    },
                        // TODO(cgag): error handling
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
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
    return evaluate(e);
}
