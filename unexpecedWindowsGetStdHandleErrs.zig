const std = @import("std");
const warn = std.debug.warn;

pub fn main() !void {
    const env = std.os.getEnvPosix("HELLO");
    if (env) |e| {
        o(e);
    }
}

fn o(s: []const u8) void {
    var stdout = try std.io.getStdOut();
    stdout.write(s);
}
