const std = @import("std");
const Allocator = std.mem.Allocator;

inline fn fallibleAdd(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const res, const overflow = @addWithOverflow(a, b);
    return if (overflow == 1) null else res;
}

inline fn fallibleSub(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const res, const overflow = @subWithOverflow(a, b);
    return if (overflow == 1) null else res;
}

pub const Inst = struct {
    add: i32 = 0,
    shift: i32 = 0,
    input: u32 = 0,
    output: u32 = 0,
    jez: ?*Inst = null,
    jnz: ?*Inst = null,
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

/// A Block is a partial instruction parsing result, namely as many
/// tokens have been fused into the Inst as possible,
/// excluding loop constructs (jez and jnz both null).
///
/// .next_tok is provided for both:
///     - Convenience: We don't need to backtrack the cursor when
///                    we have a token we can't fuse.
///     - Performance: Why do more work when we've already done the
///                    work and have the result
const Block = struct {
    inst: Inst,
    next_tok: ?Token,
};

/// As we return .next_tok, we should also offer a way for the caller
/// to give it back to us for use before parsing through cursor; we
/// do this through the `prepend` parameter.
fn consumeBlock(cursor: *[]const u8, prepend: ?Token) ?Block {
    if (cursor.len == 0 and prepend == null) {
        return null;
    }

    var prepend_once = prepend;

    const ParseState = enum { Add, Shift, Input, Output };

    var parseState = ParseState.Add;

    var inst = Inst{};
    var token: ?Token = null;

    while (true) {
        token = prepend_once orelse consumeToken(cursor);
        prepend_once = null;

        switch (token orelse break) {
            .Incr => switch (parseState) {
                .Add => inst.add = fallibleAdd(inst.add, 1) orelse break,
                else => break,
            },
            .Decr => switch (parseState) {
                .Add => inst.add = fallibleSub(inst.add, 1) orelse break,
                else => break,
            },
            .Right => switch (parseState) {
                .Add => {
                    parseState = ParseState.Shift;
                    inst.shift = fallibleAdd(inst.shift, 1) orelse break;
                },
                .Shift => inst.shift = fallibleAdd(inst.shift, 1) orelse break,
                else => break,
            },
            .Left => switch (parseState) {
                .Add => {
                    parseState = ParseState.Shift;
                    inst.shift = fallibleSub(inst.shift, 1) orelse break;
                },
                .Shift => inst.shift = fallibleSub(inst.shift, 1) orelse break,
                else => break,
            },
            .Input => switch (parseState) {
                .Add, .Shift => {
                    parseState = ParseState.Input;
                    inst.input = fallibleAdd(inst.input, 1) orelse break;
                },
                .Input => inst.input = fallibleAdd(inst.input, 1) orelse break,
                else => return Block{ .inst = inst, .next_tok = token },
            },
            .Output => switch (parseState) {
                .Add, .Shift, .Input => {
                    parseState = ParseState.Output;
                    inst.output = fallibleAdd(inst.output, 1) orelse break;
                },
                .Output => inst.output = fallibleAdd(inst.output, 1) orelse break,
            },
            .BegLoop => break,
            .EndLoop => break,
        }
    }

    return Block{ .inst = inst, .next_tok = token };
}

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

test "consumeBlock" {
    // todo better test but this should be good enough for development,
    // gives me high confidence all of it is correct

    const expect = std.testing.expectEqual;

    const program =
        \\++- > -
        \\--<<<>...,
    ;

    const expectedInstructions = .{
        Inst{
            .add = 1,
            .shift = 1,
        },
        Inst{
            .add = -3,
            .shift = -2,
            .output = 3,
        },
        Inst{
            .input = 1,
        },
    };

    var cursor: []const u8 = program;
    var prepend: ?Token = null;

    inline for (expectedInstructions) |inst| {
        const res = consumeBlock(&cursor, prepend).?;
        try expect(inst, res.inst);
        prepend = res.next_tok;
    }

    inline for (0..69) |_| {
        try expect(null, consumeBlock(&cursor, prepend));
    }
}
