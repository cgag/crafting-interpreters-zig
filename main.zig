const std  = @import("std");
const warn = std.debug.warn;
const mem  = std.mem;
const os   = std.os;
const io   = std.io;
const ArrayList = @import("std").ArrayList;
const globals = @import("globals.zig");

use @import("lex.zig");

const str = []const u8;
// c_allocator doesn't work, causes ldd to crash with duplicate symbol "_start"
// var alloc = std.heap.c_allocator;
var alloc = &std.heap.DirectAllocator.init().allocator;
// defer std.heap.DirectAlloctor.deinit();

pub fn main() !void {
    var args = try os.argsAlloc(alloc);
    defer os.argsFree(alloc, args);
    // drop the program name argument from the count
    const useful_args = args.len - 1;

    if (useful_args > 1) {
        try println("usage: lox [script]");
        os.exit(64);
    } else if (useful_args == 1) {
        try runFile(args[1]);
    } else {
        try repl();
    }
}

fn print(s: str) !void {
    var stdout = try std.io.getStdOut();
    try stdout.write(s);
}

fn println(s: str) !void {
    const with_newline_buf = try alloc.alloc(u8, s.len+1);
    defer alloc.free(with_newline_buf);
    for (s[0..s.len])  |b, i| with_newline_buf[i] = b;
    with_newline_buf[s.len] = '\n';
    var stdout = try std.io.getStdOut();
    try stdout.write(with_newline_buf);
}

fn runFile(path: str) !void {
    const contents = try io.readFileAlloc(alloc, path);
    defer alloc.free(contents);
    try run(contents);
}

fn run(src: str) !void {
    var scanner = Scanner.init(alloc, src);
    var tokens = try scanner.scan();

    for (tokens.toSlice()) |token| {
        try println(@tagName(token.type));
    }

    if (globals.had_error) {
        os.exit(65);
    }
}

fn repl() !void {
    var line_buf: [50000]u8 = undefined;

    // TODO(cgag): tmp
    var t = Token { .type = TokenType.LEFT_PAREN,
                        .lexeme = "fuck",
                        .line = 10,
                        .literal = []u8{10},
                      };
    const s = try t.to_string(alloc);
    defer alloc.free(s);
    try println(s);

    //---
    while (true) {
        try print("> ");
        _ = try io.readLine(line_buf[0..]);
        if (line_buf[0] != 0) {
            try run(line_buf);
        }
        for (line_buf) |_, i| line_buf[i] = 0;
        globals.had_error = false;
    }
}
