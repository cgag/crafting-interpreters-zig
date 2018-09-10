const std  = @import("std");
const warn = std.debug.warn;
const fmt  = std.fmt;
const mem  = std.mem;

var alloc = &std.heap.DirectAllocator.init().allocator;

pub const Expr = union(enum) {
    Literal: Literal,
};

pub const Literal = union(enum) {
    Number: f64,
    Bool: bool,
};

pub fn main() !void {
    const s = try fmt.allocPrint(alloc, "{}", Expr{.Literal=Literal{.Bool=true}});
    warn("{}\n",s);
}
