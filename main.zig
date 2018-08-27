const std  = @import("std");
const warn = std.debug.warn;
const mem  = std.mem;
const os   = std.os;
const io   = std.io;
const ArrayList = @import("std").ArrayList;
const globals = @import("globals.zig");
const atof = @import("atof.zig");

use @import("lex.zig");

// c_allocator doesn't work, causes ldd to crash with duplicate symbol "_start"
// var alloc = std.heap.c_allocator;
var alloc = &std.heap.DirectAllocator.init().allocator;
// defer std.heap.DirectAlloctor.deinit();


// TODO(cgag): don't have alloc as a global
// TODO(cgag): make atof return an error union with a potential overflow
// error instead of just returning 0 (!)
pub fn main() !void {
    var args = try os.argsAlloc(alloc);
    defer os.argsFree(alloc, args);
    // drop the program name argument from the count
    const useful_args = args.len - 1;

    warn("test: {}\n", @intCast(i64,0)>>1);
    warn("test: {}\n", @intCast(i64,0)>>2);
    warn("test: {}\n", @intCast(i64,0)>>10);
    warn("test: {}\n", @intCast(i64,0)>>20);

    if (useful_args > 1) {
        try println("usage: lox [script]");
        os.exit(64);
    } else if (useful_args == 1) {
        try runFile(args[1]);
    } else {
        try repl();
    }
}

fn print(s: []const u8) !void {
    var stdout = try std.io.getStdOut();
    try stdout.write(s);
}

fn println(s: []const u8) !void {

    // TODO(cgag): basically make warn but with stdout
    // const buf = try std.fmt.allocPrint(
    //     alloc,

    const with_newline_buf = try alloc.alloc(u8, s.len+1);
    defer alloc.free(with_newline_buf);
    for (s[0..s.len])  |b, i| with_newline_buf[i] = b;
    with_newline_buf[s.len] = '\n';
    var stdout = try std.io.getStdOut();
    try stdout.write(with_newline_buf);
}

fn runFile(path: []const u8) !void {
    const contents = try io.readFileAlloc(alloc, path);
    defer alloc.free(contents);
    try run(contents);
}

fn run(src: []const u8) !void {
    var scanner = Scanner.init(alloc, src);
    const tokens = try scanner.scan();

    for (tokens.toSlice()) |token| {
        try println(@tagName(token.type));
        if (token.literal) |literal| {
            switch (literal) {
                // TODO(cgag): fix compiler so that format just prints the active field in the union
                LiteralType.Number => {
                    warn("token: literal({}), lexeme({}): {.}\n",
                         @tagName(token.type),
                         token.lexeme,
                         literal.Number,
                         );
                },
                LiteralType.String => {
                    warn("printing token literal ({}): {}\n", @tagName(token.type), literal.String);
                },
            }
        }
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
                    .literal = Literal { .Number = 10 },
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
