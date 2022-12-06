const std = @import("std");
const builtin = @import("builtin");
const Gigatron = @import("gigatron.zig");

//@TODO: implement all the windows features using SDL, as that's the most
// reasonable way to get support for X, Wayland, the 600 different sound
// daemons, etc.

pub fn main() !void {
    var vm: Gigatron.VirtualMachine = undefined; //std.mem.zeroes(Gigatron.VirtualMachine);
    var vga = Gigatron.VgaMonitor{};
    vga.init();
    var babelfish = Gigatron.BabelFish{};
    var tape = [_]u8{0} ** 512;
    babelfish.init(&tape);
    
    const current_dir = std.fs.cwd();
    const rom_file = try current_dir.openFile("gigatron.rom", .{ .mode = .read_only });
    var reader = rom_file.reader();
    _ = try vm.loadRom(reader);
    rom_file.close();

    vm.start();

    var i = @as(u32, 0);
    while(i < 1_000_000_000):(i += 1) {
        vm.cycle();
        babelfish.cycle(&vm);
        vga.cycle(&vm);
        
        if(vm.vsync == .falling) {
            std.debug.print("==render==\n", .{});
        }
    }
}
