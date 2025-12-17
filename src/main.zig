const std = @import("std");
const fs = std.fs;

pub fn main() !void {
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.print("Habu Lisp v0.1.0\n", .{});
    try w.flush();
}
