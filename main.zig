const std  = @import("std");
const warn = std.debug.warn;
const mem  = std.mem;
const os   = std.os;
const io   = std.io;
const ArrayList = std.ArrayList;

const globals   = @import("globals.zig");
const atof      = @import("atof.zig");

use @import("lex.zig");
const Parser = @import("parser.zig").Parser;
const I = @import("interpreter.zig");

// TODO(cgag): not sure this belongs in parser.zig
const expr_print = @import("parser.zig").expr_print;

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
    var stdout = try std.io.getStdOut();
    const buf = try std.fmt.allocPrint(alloc, "{}\n", s);
    defer alloc.free(buf);
    try stdout.write(buf);
}

fn runFile(path: []const u8) !void {
    const contents = try io.readFileAlloc(alloc, path);
    defer alloc.free(contents);
    try run(contents);
    if (globals.had_error) os.exit(65);
    if (globals.had_runtime_error) os.exit(70);
}

fn run(src: []const u8) !void {
    var scanner = try Scanner.init(alloc, src);
    defer scanner.deinit();

    const tokens = try scanner.scan();

    for (tokens.toSlice()) |token| {
        if (token.literal) |literal| {
            switch (literal) {
                // TODO(cgag): fix compiler so that format just prints the active field in the union
                Literal.Number => {
                    warn("{}, (\"{}\"): {.}\n",
                         @tagName(token.type),
                         token.lexeme,
                         literal.Number,
                         );
                },
                Literal.String => {
                    warn("({}): {}\n", @tagName(token.type), literal.String);
                },
                Literal.Nil  => warn("({}): {}\n", @tagName(token.type), literal.Nil),
                Literal.Bool => warn("({}): {}\n", @tagName(token.type), literal.Bool),
            }
        } else {
            warn("{} ({})\n", @tagName(token.type), token.lexeme);
        }
    }

    var parser = Parser.init(alloc, tokens);
    var expr = parser.parse() catch |e| {
        warn("hit parser error: {}", e);
        os.exit(65);
    };

    var val = I.evaluate(alloc, expr) catch |e| {
        I.report_runtime_error(e);
        return;
    };
    warn("{}\n", val);

    if (globals.had_error) {
        os.exit(65);
    }
}

fn repl() !void {
    var line_buf: [50000]u8 = undefined;

    while (true) {
        for (line_buf) |_, i| line_buf[i] = 0;
        try print("> ");
        _ = try io.readLine(line_buf[0..]);
        if (line_buf[0] != 0) {
            var end_index: u64 = 0;
            for (line_buf) |b, i| {
                if (b == 0) {
                    end_index = i;
                    break;
                }
            }
            try run(line_buf[0..end_index]);
        }
        globals.had_error = false;
    }
}
