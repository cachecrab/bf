const std = @import("std");

const bf = @import("bf.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const ally = gpa.allocator();

    const args = try std.process.argsAlloc(ally);
    defer std.process.argsFree(ally, args);

    if (args.len != 2) {
        std.debug.print("Usage: {s} <file_path>\n", .{args[0]});
        return;
    }

    const file_path = args[1];
    var file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_contents = try file.readToEndAlloc(ally, 4 * 1024 * 1024 * 1024);
    defer ally.free(file_contents);

    var inst_alloc = std.heap.ArenaAllocator.init(ally);
    defer inst_alloc.deinit();

    const root = try bf.parse(file_contents, &inst_alloc, ally);
    if (root) |i| {
        const tape = try ally.alloc(u8, 4096);
        defer ally.free(tape);
        @memset(tape, 0);

        try bf.interpret(i, tape, std.io.getStdOut().writer().any());
    }
}
