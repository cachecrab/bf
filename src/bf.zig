const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Inst = struct {
    add: i32 = 0,
    shift: i32 = 0,
    input: i32 = 0,
    output: i32 = 0,
    jez: ?*Inst = undefined,
    jnz: ?*Inst = undefined,
};

const Token = enum {
    Incr,
    Decr,
    Right,
    Left,
    BegLoop,
    EndLoop,
    Input,
    Output,
};

fn consumeToken(cursor: *[]const u8) ?Token {
    var copy = cursor.*;
    defer cursor.* = copy;

    while (copy.len > 0) {
        defer copy = copy[1..];

        switch (copy[0]) {
            '+' => return .Incr,
            '-' => return .Decr,
            '>' => return .Right,
            '<' => return .Left,
            '[' => return .BegLoop,
            ']' => return .EndLoop,
            ',' => return .Input,
            '.' => return .Output,
            else => {},
        }
    }

    return null;
}

test "consumeToken" {
    const expect = std.testing.expectEqual;

    const program =
        \\+ + a+
        \\- - b-
        \\> > c>
        \\< < d<
        \\[ [ e[
        \\] ] f]
        \\, , g,
        \\. . h.
        \\whatever else
    ;
    var cursor: []const u8 = program;

    const lines: [8]Token = .{ .Incr, .Decr, .Right, .Left, .BegLoop, .EndLoop, .Input, .Output };
    inline for (lines) |expected| {
        inline for (0..3) |_| {
            try expect(expected, consumeToken(&cursor));
        }
    }

    // just to be sure
    inline for (0..69) |_| {
        try expect(null, consumeToken(&cursor));
    }
}
