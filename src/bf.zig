const std = @import("std");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

inline fn fallibleAdd(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const res, const overflow = @addWithOverflow(a, b);
    return if (overflow == 1) null else res;
}

inline fn fallibleSub(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const res, const overflow = @subWithOverflow(a, b);
    return if (overflow == 1) null else res;
}

pub const Inst = struct {
    add: i8 = 0,
    shift: i32 = 0,
    input: u32 = 0,
    output: u32 = 0,
    jez: ?*Inst = null,
    jnz: ?*Inst = null,
};

pub const RuntimeError = error{
    StdinReadError,
    StdoutWriteError,
    OutOfBounds,
};

pub fn interpret(root: *Inst, tape: []u8, out: std.io.AnyWriter) RuntimeError!void {
    const stdin = std.io.getStdIn().reader();

    const upperTapeBound = tape.len;

    var tapeIndex: usize = 0;

    var inst: ?*Inst = root;
    while (inst) |i| {
        const copy = i.*;

        // With a wrapping add we can just bitCast to preserve two's compliment
        // and get subtraction for free
        //
        // tape[tapeIndex]: u8
        // copy.add: i8
        tape[tapeIndex] +%= @bitCast(copy.add);

        // Putting old version in version control so I can reference in article
        // if (copy.add > 0) {
        //     const asu8: u8 = @intCast(copy.add);
        //     tape[tapeIndex], _ = @addWithOverflow(tape[tapeIndex], asu8);
        // } else {
        //     const asu8: u8 = @intCast(-copy.add);
        //     tape[tapeIndex], _ = @subWithOverflow(tape[tapeIndex], asu8);
        // }
        // tape[tapeIndex] +%= @bitCast(copy.add);

        if (copy.shift > 0) {
            tapeIndex += @intCast(copy.shift);
            if (tapeIndex > upperTapeBound) {
                return RuntimeError.OutOfBounds;
            }
        } else if (copy.shift < 0) {
            const lShift: usize = @intCast(-copy.shift);
            if (lShift > tapeIndex) {
                return RuntimeError.OutOfBounds;
            }
            tapeIndex -= lShift;
        }

        for (0..copy.input) |_| {
            tape[tapeIndex] = stdin.readByte() catch return RuntimeError.StdinReadError;
        }

        if (copy.output > 0) {
            out.writeByteNTimes(tape[tapeIndex], copy.output) catch return RuntimeError.StdoutWriteError;
        }

        inst = if (tape[tapeIndex] == 0) copy.jez else copy.jnz;
    }
}

test "interpret" {
    const testing = std.testing;

    const helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

    var instAlloc = ArenaAllocator.init(testing.allocator);
    defer instAlloc.deinit();

    const parsed = try parse(helloWorld, &instAlloc, testing.allocator);
    const root = parsed.?;

    var stdoutBuf = std.ArrayList(u8).init(testing.allocator);
    defer stdoutBuf.deinit();
    const stdout = stdoutBuf.writer().any();

    const tape = try testing.allocator.alloc(u8, 1024);
    for (tape) |*b| {
        b.* = 0;
    }
    defer testing.allocator.free(tape);

    try interpret(root, tape, stdout);

    const helloWorldOuput: []const u8 = "Hello World!\n";
    try testing.expectEqualStrings(helloWorldOuput, stdoutBuf.items);
}

pub const ParseError = error{
    UnexpectedClosingBracket,
    MissingClosingBracket,
} || Allocator.Error;

pub fn parse(program: []const u8, instAlloc: *ArenaAllocator, tempAlloc: Allocator) ParseError!?*Inst {
    if (program.len == 0) {
        return null;
    }

    const arena = instAlloc.allocator();

    // keep track of who's `jez` we need to set when we encounter an end loop
    var beforeLoopBeginStack = std.ArrayList(*Inst).init(tempAlloc);
    defer beforeLoopBeginStack.deinit();

    var cursor: []const u8 = program;

    const firstBlock = consumeBlock(&cursor, null) orelse return null;

    var inst: *Inst = try arena.create(Inst);
    inst.* = firstBlock.inst;

    const root = inst;

    var block = firstBlock;
    while (true) {
        switch (block.next_tok orelse break) {
            .Incr, .Decr, .Right, .Left, .Input, .Output => {
                // Chain next block to current

                // Safe .? as this switch prong means at least one fusable instruction in .next_tok
                const next = consumeBlock(&cursor, block.next_tok).?;
                defer block = next;

                const nextInst: *Inst = try arena.create(Inst);
                nextInst.* = next.inst;
                defer inst = nextInst;

                inst.jez = nextInst;
                inst.jnz = nextInst;
            },
            .BegLoop => {
                // Don't prepend b.next_tok, we want to skip the [
                const next = consumeBlock(&cursor, null) orelse return ParseError.MissingClosingBracket;
                defer block = next;

                const nextInst: *Inst = try arena.create(Inst);
                nextInst.* = next.inst;
                defer inst = nextInst;

                try beforeLoopBeginStack.append(inst);
                inst.jnz = nextInst;
            },
            .EndLoop => {
                const beforeLoopBegin = beforeLoopBeginStack.popOrNull() orelse return ParseError.UnexpectedClosingBracket;
                inst.jnz = beforeLoopBegin.jnz;

                // Don't prepend b.next_tok, we want to skip the ]
                const next = consumeBlock(&cursor, null) orelse break;
                defer block = next;

                const nextInst: *Inst = try arena.create(Inst);
                nextInst.* = next.inst;
                defer inst = nextInst;

                inst.jez = nextInst;
                beforeLoopBegin.jez = nextInst;
            },
        }
    }

    if (beforeLoopBeginStack.items.len != 0) {
        return ParseError.MissingClosingBracket;
    }

    return root;
}

test "parse" {
    const testing = std.testing;

    const expect = testing.expectEqual;
    const assert = testing.expect;

    var instAlloc = ArenaAllocator.init(testing.allocator);
    defer instAlloc.deinit();

    const tempAlloc = testing.allocator;

    const res = try parse("++[->++<]--.", &instAlloc, tempAlloc);
    const inst = res.?;

    try expect(2, inst.add);
    try assert(null != inst.jez);
    try assert(null != inst.jnz);

    const inner = inst.jnz.?;
    const end = inst.jez.?;

    try expect(-1, inner.add);
    try expect(1, inner.shift);
    try assert(null != inner.jnz);
    try assert(inner.jnz.? == inner.jez.?);

    const inner2 = inner.jnz.?;
    try expect(2, inner2.add);
    try expect(-1, inner2.shift);
    try expect(inner, inner2.jnz.?);
    try expect(end, inner2.jez);

    try expect(-2, end.add);
    try expect(1, end.output);
}

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

    var state = ParseState.Add;

    var inst = Inst{};
    var token: ?Token = null;

    while (true) {
        token = prepend_once orelse consumeToken(cursor);
        prepend_once = null;

        switch (token orelse break) {
            .Incr => switch (state) {
                .Add => inst.add = fallibleAdd(inst.add, 1) orelse break,
                else => break,
            },
            .Decr => switch (state) {
                .Add => inst.add = fallibleSub(inst.add, 1) orelse break,
                else => break,
            },
            .Right => switch (state) {
                .Add => {
                    state = ParseState.Shift;
                    inst.shift = fallibleAdd(inst.shift, 1) orelse break;
                },
                .Shift => inst.shift = fallibleAdd(inst.shift, 1) orelse break,
                else => break,
            },
            .Left => switch (state) {
                .Add => {
                    state = ParseState.Shift;
                    inst.shift = fallibleSub(inst.shift, 1) orelse break;
                },
                .Shift => inst.shift = fallibleSub(inst.shift, 1) orelse break,
                else => break,
            },
            .Input => switch (state) {
                .Add, .Shift => {
                    state = ParseState.Input;
                    inst.input = fallibleAdd(inst.input, 1) orelse break;
                },
                .Input => inst.input = fallibleAdd(inst.input, 1) orelse break,
                else => return Block{ .inst = inst, .next_tok = token },
            },
            .Output => switch (state) {
                .Add, .Shift, .Input => {
                    state = ParseState.Output;
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
