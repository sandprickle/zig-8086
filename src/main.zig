const std = @import("std");
const emu8086 = @import("./emu8086.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdout = std.io.getStdOut();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len <= 1) {
        std.debug.print("Please specify a file to disassemble!\n", .{});
    } else {
        const path = args[1];

        const outputWriter = stdout.writer();

        var splitPath = std.mem.splitBackwardsSequence(u8, path, "/");
        const filename = splitPath.first();
        if (filename.len > 0) {
            _ = try stdout.write(try std.fmt.allocPrint(allocator, "; Disassembly of {s}\n\n", .{filename}));
        }

        const bytes = std.fs.cwd()
            .readFileAlloc(allocator, path, 2000) catch |err| {
            std.debug.print("Unable to read file: {}\n", .{err});
            return;
        };

        emu8086.disassembleBytes(allocator, bytes, outputWriter) catch |err| {
            
            
            std.debug.print("Unable to proceed with disassembly: {}\n", .{err});
            return;
        };
    }
}
