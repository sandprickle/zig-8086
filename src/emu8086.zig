const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const assert = std.debug.assert;

const REGISTERS = [2][8][]const u8{
    [8][]const u8{ "al", "cl", "dl", "bl", "ah", "ch", "dh", "dh" },
    [8][]const u8{ "ax", "cx", "dx", "bx", "sp", "bp", "si", "di" },
};

const EAC = [8][]const u8{
    "bx + si",
    "bx + di",
    "bp + si",
    "bp + di",
    "si",
    "di",
    "bp",
    "bx",
};

pub const DisassemblyError = error{ MissingBytes, UnknownOpcode };

pub fn disassembleBytes(allocator: Allocator, bytes: []u8, output: std.fs.File.Writer) !void {
    _ = try output.write("bits 16\n\n");

    var iter = ByteIter.fromSlice(bytes);
    while (iter.len() > 0) {
        const start = try iter.next();

        if (start & 0b11110000 == 0b10110000) {
            // this is an immediate
            const imWordReg: ImWordReg = @bitCast(start);
            _ = try output.write(try movImToReg(allocator, &iter, imWordReg));
        } else {
            // this is a normal instruction
            const opDirWord: OpDirWord = @bitCast(start);

            switch (opDirWord.op) {
                0b100010 => {
                    _ = try output.write(try movRMReg(allocator, &iter, opDirWord));
                },
                else => return DisassemblyError.UnknownOpcode,
            }
        }
    }
}

/// Special case for opcode 1011.
fn movImToReg(allocator: Allocator, bytes: *ByteIter, imWordReg: ImWordReg) ![]u8 {
    const register = REGISTERS[imWordReg.word][imWordReg.reg];
    const format = "mov {s}, {d}\n";

    if (imWordReg.word == 0) {
        const data8: i8 = @bitCast(try bytes.next());
        return std.fmt.allocPrint(allocator, format, .{ register, data8 });
    } else {
        const dataLo = try bytes.next();
        const dataHi = @as(u16, try bytes.next());
        const data16: i16 = @bitCast((dataLo | (dataHi << 8)));
        return std.fmt.allocPrint(allocator, format, .{ register, data16 });
    }
}

fn movRMReg(allocator: Allocator, bytes: *ByteIter, opDirWord: OpDirWord) ![]u8 {
    const modRegRM: ModRegRM = @bitCast(try bytes.next());
    const MOV_FMT = "mov {s}, {s}\n";
    const EAC_FMT = "[{s} {s} {d}]";
    const BRACES_FMT = "[{s}]";

    switch (modRegRM.mod) {
        0b00 => {
            // Mem 0-bit displacement
            const eac = try std.fmt.allocPrint(allocator, BRACES_FMT, .{EAC[modRegRM.rm]});
            const register = REGISTERS[opDirWord.word][modRegRM.reg];

            return if (opDirWord.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ eac, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, eac });
        },
        0b01 => {
            // Mem 8-bit displacement
            const register = REGISTERS[opDirWord.word][modRegRM.reg];
            const eacStr = EAC[modRegRM.rm];

            const disp8: i8 = @bitCast(try bytes.next());

            const operator = if (disp8 >= 0) "+" else "-";

            const eac = if (disp8 == 0)
                try std.fmt.allocPrint(allocator, BRACES_FMT, .{eacStr})
            else
                try std.fmt
                    .allocPrint(allocator, EAC_FMT, .{ eacStr, operator, @abs(disp8) });

            return if (opDirWord.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ eac, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, eac });
        },

        0b10 => {
            // Mem 16-bit displacement
            const register = REGISTERS[opDirWord.word][modRegRM.reg];
            const eacStr = EAC[modRegRM.rm];

            const dispLo = try bytes.next();
            const dispHi = @as(u16, try bytes.next());
            const disp16: i16 = @bitCast(dispLo | (dispHi << 8));

            const operator = if (disp16 >= 0) "+" else "-";

            const eac = if (disp16 == 0)
                try std.fmt.allocPrint(allocator, BRACES_FMT, .{eacStr})
            else
                try std.fmt
                    .allocPrint(allocator, EAC_FMT, .{ eacStr, operator, @abs(disp16) });

            return if (opDirWord.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ eac, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, eac });
        },
        0b11 => {
            // Register

            const register = REGISTERS[opDirWord.word][modRegRM.reg];
            const r_m = REGISTERS[opDirWord.word][modRegRM.rm];

            return if (opDirWord.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ r_m, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, r_m });
        },
    }

    return error.Help;
}

// Structs

/// Typical first byte of an instruction, featuring a 6-bit opcode, direction bit, and word bit.
const OpDirWord = packed struct {
    word: u1,
    dir: u1,
    op: u6,

    comptime {
        assert(@bitSizeOf(OpDirWord) == @bitSizeOf(u8));

        const byte: u8 = 0b10001001;
        const castByte: OpDirWord = @bitCast(byte);
        // assertion fails
        assert(castByte.op == (byte & 0b11111100) >> 2);
    }
};

/// First byte of an immediate to reg instruction.
/// The opcode is 4 bits, the W field is 1 bit, and the REG field is 3 bits.
const ImWordReg = packed struct {
    reg: u3,
    word: u1,
    op: u4,
};

/// Typical second byte of an instruction.
/// The MOD field is 2 bits, the REG field is 3 bits, and the R/M field is 3 bits.
const ModRegRM = packed struct {
    rm: u3,
    reg: u3,
    mod: u2,
};

// Helpers

/// Helper for iterating over a slice of bytes.
const ByteIter = struct {
    bytes: []u8,
    pos: usize,

    fn fromSlice(bytes: []u8) ByteIter {
        return ByteIter{ .bytes = bytes, .pos = 0 };
    }

    /// Returns the next byte in the iterator, or null if there are no more bytes.
    fn next(self: *ByteIter) !u8 {
        if (self.pos >= self.bytes.len) {
            return DisassemblyError.MissingBytes;
        }

        defer self.pos += 1;
        return self.bytes[self.pos];
    }

    /// Number of remaining bytes
    fn len(self: ByteIter) usize {
        return self.bytes.len - self.pos;
    }
};
const Full = packed struct {
    x: u8,
};
test "OpDirWord packed struct" {
    try expect(@sizeOf(OpDirWord) == 1);
    try expect(@bitSizeOf(OpDirWord) == 8);
    try expect(@bitOffsetOf(OpDirWord, "op") == 0);
    try expect(@bitOffsetOf(OpDirWord, "dir") == 6);
    try expect(@bitOffsetOf(OpDirWord, "word") == 7);

    const byte: u8 = 0b10001001;
    const full = Full{ .x = byte };

    const opDirWord: OpDirWord = @bitCast(full);

    try expect(opDirWord.op == 0b100010);
}
