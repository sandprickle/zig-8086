const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const assert = std.debug.assert;

const OpCodes = enum(u6) {
    // Data Transfer
    mov = 0b100010,
    reg_mem_mov = 0b100010,
    // Arithmetic
    reg_mem_add = 0b000000, // 000
    reg_mem_sub = 0b001010, // 101
    reg_mem_cmp = 0b001110, // 111
    arith_immed = 0b100000, // Covers ADD, SUB, CMP
};

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

const INST_FMT = "{s} {s}, {s}\n";
const EAC_FMT = "[{s} {s} {d}]";
const BRACES_FMT = "[{s}]";

pub const DisassemblyError = error{ MissingBytes, UnknownOpcode };

pub fn disassembleBytes(
    allocator: Allocator,
    bytes: []u8,
    output: std.fs.File.Writer,
) !void {
    _ = try output.write("bits 16\n\n");

    var iter = ByteIter.fromSlice(bytes);
    while (iter.len() > 0) {
        const start = try iter.next();

        if (start & 0b11110000 == 0b10110000) {
            // this is an immediate to register
            const imm_word_reg: ImWordReg = @bitCast(start);
            _ = try output.write(try movImmediateToReg(
                allocator,
                &iter,
                imm_word_reg,
            ));
        } else {
            // this is a normal instruction
            const op_dir_word: OpDirectionWord = @bitCast(start);

            switch (op_dir_word.op) {
                OpCodes.reg_mem_mov => {
                    _ = try output.write(try movRMReg(
                        allocator,
                        &iter,
                        op_dir_word,
                    ));
                },
                else => return DisassemblyError.UnknownOpcode,
            }
        }
    }
}

/// Special case for opcode 1011.
fn movImmediateToReg(allocator: Allocator, bytes: *ByteIter, im_word_reg: ImWordReg) ![]u8 {
    const register = REGISTERS[im_word_reg.word][im_word_reg.reg];
    const format = "mov {s}, {d}\n";

    if (im_word_reg.word == 0) {
        const data_8: i8 = @bitCast(try bytes.next());
        return std.fmt.allocPrint(allocator, format, .{ register, data_8 });
    } else {
        const data_lo = try bytes.next();
        const data_hi = @as(u16, try bytes.next());
        const data_16: i16 = @bitCast((data_lo | (data_hi << 8)));
        return std.fmt.allocPrint(allocator, format, .{ register, data_16 });
    }
}

/// MOV Register/Memory to/from register
fn movRMReg(allocator: Allocator, bytes: *ByteIter, op_dir_word: OpDirectionWord) ![]u8 {
    const mod_reg_rm: ModRegRM = @bitCast(try bytes.next());
    const MOV_FMT = "mov {s}, {s}\n";

    switch (mod_reg_rm.mod) {
        0b00 => {
            // Mem 0-bit displacement
            const eac = try std.fmt.allocPrint(
                allocator,
                BRACES_FMT,
                .{EAC[mod_reg_rm.rm]},
            );
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ eac, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, eac });
        },
        0b01 => {
            // Mem 8-bit displacement
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];
            const eacStr = EAC[mod_reg_rm.rm];

            const disp_8: i8 = @bitCast(try bytes.next());

            const operator = if (disp_8 >= 0) "+" else "-";

            const eac = if (disp_8 == 0)
                try std.fmt.allocPrint(allocator, BRACES_FMT, .{eacStr})
            else
                try std.fmt
                    .allocPrint(allocator, EAC_FMT, .{ eacStr, operator, @abs(disp_8) });

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ eac, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, eac });
        },
        0b10 => {
            // Mem 16-bit displacement
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];
            const eac_str = EAC[mod_reg_rm.rm];

            const disp_lo = try bytes.next();
            const disp_hi = @as(u16, try bytes.next());
            const disp_16: i16 = @bitCast(disp_lo | (disp_hi << 8));

            const operator = if (disp_16 >= 0) "+" else "-";

            const eac = if (disp_16 == 0)
                try std.fmt.allocPrint(allocator, BRACES_FMT, .{eac_str})
            else
                try std.fmt
                    .allocPrint(allocator, EAC_FMT, .{ eac_str, operator, @abs(disp_16) });

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ eac, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, eac });
        },
        0b11 => {
            // Register

            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];
            const r_m = REGISTERS[op_dir_word.word][mod_reg_rm.rm];

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(allocator, MOV_FMT, .{ r_m, register })
            else
                // REG is destination
                std.fmt.allocPrint(allocator, MOV_FMT, .{ register, r_m });
        },
    }
}

/// Todo: This should work for MOVs too
fn arithmeticRegMem(
    allocator: Allocator,
    bytes: *ByteIter,
    op_dir_word: OpDirectionWord,
) ![]u8 {
    const instruction = switch (op_dir_word) {
        OpCodes.reg_mem_add => "add",
        OpCodes.reg_mem_sub => "sub",
        OpCodes.reg_mem_cmp => "cmp",
    };

    const mod_reg_rm: ModRegRM = @bitCast(try bytes.next());

    switch (mod_reg_rm.mod) {
        0b00 => {
            // Memory no displacement
            const eac = try std.fmt.allocPrint(
                allocator,
                BRACES_FMT,
                .{EAC[mod_reg_rm.rm]},
            );
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, eac, register },
                )
            else
                // REG is destination
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, register, eac },
                );
        },
        0b01 => {
            // Mem 8-bit displacement
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];
            const eacStr = EAC[mod_reg_rm.rm];

            const disp_8: i8 = @bitCast(try bytes.next());

            const operator = if (disp_8 >= 0) "+" else "-";

            const eac = if (disp_8 == 0)
                try std.fmt.allocPrint(allocator, BRACES_FMT, .{eacStr})
            else
                try std.fmt
                    .allocPrint(allocator, EAC_FMT, .{ eacStr, operator, @abs(disp_8) });

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, eac, register },
                )
            else
                // REG is destination
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, register, eac },
                );
        },
        0b10 => {
            // Mem 16-bit displacement
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];
            const eac_str = EAC[mod_reg_rm.rm];

            const disp_lo = try bytes.next();
            const disp_hi = @as(u16, try bytes.next());
            const disp_16: i16 = @bitCast(disp_lo | (disp_hi << 8));

            const operator = if (disp_16 >= 0) "+" else "-";

            const eac = if (disp_16 == 0)
                try std.fmt.allocPrint(allocator, BRACES_FMT, .{eac_str})
            else
                try std.fmt
                    .allocPrint(allocator, EAC_FMT, .{ eac_str, operator, @abs(disp_16) });

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, eac, register },
                )
            else
                // REG is destination
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, register, eac },
                );
        },
        0b11 => {
            // Register
            const register = REGISTERS[op_dir_word.word][mod_reg_rm.reg];
            const r_m = REGISTERS[op_dir_word.word][mod_reg_rm.rm];

            return if (op_dir_word.dir == 0)
                // REG is source
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, r_m, register },
                )
            else
                // REG is destination
                std.fmt.allocPrint(
                    allocator,
                    INST_FMT,
                    .{ instruction, register, r_m },
                );
        },
    }
}

fn arithmeticImmediate(
    allocator: Allocator,
    bytes: *ByteIter,
    op_dir_word: OpDirectionWord,
) ![]u8 {}

// Structs

/// Typical first byte of an instruction, featuring a 6-bit opcode, direction bit, and word bit.
const OpDirectionWord = packed struct {
    /// Word bit, 0 for byte, 1 for word.
    word: u1,
    /// Direction bit, 0 for (REG is) source, 1 for destination.
    dir: u1,
    /// Opcode, 6 bits.
    op: u6,

    comptime {
        assert(@bitSizeOf(OpDirectionWord) == @bitSizeOf(u8));

        const byte: u8 = 0b10001001;
        const cast_byte: OpDirectionWord = @bitCast(byte);
        // assertion fails
        assert(cast_byte.op == (byte & 0b11111100) >> 2);
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
    try expect(@sizeOf(OpDirectionWord) == 1);
    try expect(@bitSizeOf(OpDirectionWord) == 8);
    try expect(@bitOffsetOf(OpDirectionWord, "op") == 0);
    try expect(@bitOffsetOf(OpDirectionWord, "dir") == 6);
    try expect(@bitOffsetOf(OpDirectionWord, "word") == 7);

    const byte: u8 = 0b10001001;
    const full = Full{ .x = byte };

    const op_dir_word: OpDirectionWord = @bitCast(full);

    try expect(op_dir_word.op == 0b100010);
}
