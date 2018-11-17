const std    = @import("std");
const assert = std.debug.assert;
const warn   = std.debug.warn;
const mem    = std.mem;
const fmt    = std.fmt;
const os     = std.os;
const Map    = std.AutoHashMap;

const Expr    = @import("parser.zig").Expr;
const Stmt    = @import("parser.zig").Stmt;
const E       = @import("parser.zig");
const Token   = @import("lex.zig").Token;
const TT      = @import("lex.zig").TokenType;
const globals = @import("globals.zig");
const TokenLiteral = @import("lex.zig").Literal;

// TODO(cgag): I think we can just make the type .Nil be void?
const NilStruct = struct{};


pub const LoxVal = union(enum) {
    Number: f64,
    Bool:   bool,
    Nil:    NilStruct,
    String: []const u8,

    // TODO(cgag): i don't think i should have to write this?
    fn equal(self: *const LoxVal, o: LoxVal) bool {
        // TODO(cgag): should o be passed as a pointer?
        switch(self.*) {
            LoxVal.Number => switch(o) {
                LoxVal.Number => return self.Number == o.Number,
                else          => return false,
            },
            LoxVal.Bool => switch(o) {
                LoxVal.Bool => return self.Bool == o.Bool,
                else        => return false,
            },
            LoxVal.Nil => switch(o) {
                LoxVal.Nil => return true,
                else       => return false,
            },
            LoxVal.String => switch(o) {
                LoxVal.String => return mem.eql(u8, self.String, o.String),
                else          => return false,
            }
        }
    }

    // caller owns mem
    fn to_str(self: *const LoxVal, allocator: *mem.Allocator) ![]const u8 {
        switch(self.*) {
            LoxVal.Number => return try fmt.allocPrint(allocator, "{.}", self.Number),
            LoxVal.Bool   => return try fmt.allocPrint(allocator, "{}", self.Bool),
            LoxVal.Nil    => return "<nil>"[0..],
            LoxVal.String => return try fmt.allocPrint(allocator, "{}", self.String),
        }
    }
};

// have to wrap global error set values because evaluate is recursive
const EvalError = error {
    OutOfMemory,
    TypeErrorNumbers,
    TypeErrorStrings,
    TypeErrorPlusInvalidType,
    UndefinedVariable,
};

pub fn execute(env: *Map([]const u8, LoxVal), alloc: *mem.Allocator, stmt: Stmt) !void {
    switch(stmt) {
        Stmt.Print => {
            const val = try evaluate(env, alloc, stmt.Print);
            const s = try val.to_str(alloc);
            defer alloc.free(s);
            warn("{}\n", s);
        },
        Stmt.Expression => {
            _ = try evaluate(env, alloc, stmt.Expression);
            return;
        },
        Stmt.VarDecl => {
            var init_val = LoxVal { .Nil = NilStruct{} };
            if (stmt.VarDecl.initializer) |init_expr| {
                init_val = try evaluate(env, alloc, init_expr);
            }
            warn("adding {} to env\n", stmt.VarDecl.name.lexeme);
            _ = try env.put(stmt.VarDecl.name.lexeme, init_val);
            return;
        },
    }
}

pub fn evaluate(env: *Map([]const u8, LoxVal),  alloc: *mem.Allocator, e: Expr) EvalError!LoxVal {
    switch(e) {
        Expr.Literal => {
            switch(e.Literal.value) {
                TokenLiteral.String => return LoxVal{ .String = e.Literal.value.String },
                TokenLiteral.Number => return LoxVal{ .Number = e.Literal.value.Number },
                TokenLiteral.Bool   => return LoxVal{ .Bool   = e.Literal.value.Bool   },
                TokenLiteral.Nil    => return LoxVal{ .Nil    = NilStruct{}            },
            }
        },

        Expr.Unary => {
            const right_val = try evaluate(env, alloc, e.Unary.right.*);
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
            const left_val  = try evaluate(env, alloc, e.Binary.left.*);
            const right_val = try evaluate(env, alloc, e.Binary.right.*);

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

        Expr.Grouping => { // TODO(cgag): implement
            unreachable;
        },

        Expr.Variable => {
            // why is global_env empty with the repl? Something with how global_env gets initialized?
            warn("evaluating variable {}:\n", e.Variable.name.lexeme);
            // TODO(cgag): how the fuck is this 16?
            warn("env len: {}\n", env.count());
            var it = env.iterator();
            while (it.next()) |kv| {
                // TODO(cgag): ok, how the fuck is the key "t" using the repl??
                warn("entry: {} \n", kv);
            }

            var maybe_kv_ptr = env.get(e.Variable.name.lexeme);
            if (maybe_kv_ptr) |kv| {
                return kv.value;
            }
            globals.undefined_variable_lexeme = e.Variable.name.lexeme;
            crash(error.UndefinedVariable);
        },

        Expr.Assign => unreachable,
    }

    unreachable;
}

// TODO(cgag): one word?
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

pub fn crash(e: anyerror) void {
    globals.had_runtime_error = true;
    var exit_code: u8 = undefined;
    switch(e) {
        error.TypeErrorNumbers => {
            exit_code = 66;
            const msg = "both operands must be numbers";
            warn("[line: {}] {}\n", globals.type_error_token.line, msg);
        },
        error.TypeErrorStrings => {
            exit_code = 67;
            const msg = "both operands must be strings";
            warn("[line: {}] {}\n", globals.type_error_token.line, msg);
        },
        error.TypeErrorPlusInvalidType => {
            exit_code = 68;
            const msg = "+ operator only works on strings and numbers";
            warn("[line: {}] {}\n", globals.type_error_token.line, msg);
        },
        error.UndefinedVariable => {
            exit_code = 69;
            warn("Undefined variable: {}", globals.undefined_variable_lexeme);
        },
        else => {
            warn("some other error, idk");
        }
    }
    os.exit(exit_code);
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
// TODO(cgag): need ot test executing statments

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
