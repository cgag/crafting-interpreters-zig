const std = @import("std");
const warn = std.debug.warn;

const globals = @import("globals.zig");

pub fn err(line_number: i64, msg: []const u8) void {
    report(line_number, "", msg);
}

pub fn report(line_number: i64, location: []const u8, msg: []const u8) void {
    if (location.len == 0) {
        warn("[line {}] Error: {}\n", line_number, msg);
    } else {
        warn("[line {}] Error ({}): {}\n", line_number, location, msg);
    }
    globals.had_error = true;
}
