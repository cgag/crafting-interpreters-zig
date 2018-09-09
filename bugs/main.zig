const std  = @import("std");
const warn = std.debug.warn;
const fmt  = std.fmt;
const mem  = std.mem;

var alloc = &std.heap.DirectAllocator.init().allocator;

pub const ExprType = enum {
    Binary,
    Literal,
}

pub const Expr = union(ExprType) {
    Binary: Binary,
    Literal: Literal,
}

pub const Binary = struct {
}

pub const LiteralType = enum {
    Number,
    Bool,
}

pub const Literal = union(LiteralType) {
    Number: f64,
    Bool: bool,
}

pub fn main() !void {
}
