const std = @import("std");
const builtin = @import("builtin");
const Gigatron = @import("gigatron.zig");

//@TODO: implement all the windows features using SDL, as that's the most
// reasonable way to get support for X, Wayland, the 600 different sound
// daemons, etc.

pub fn main() !void {
    var vm: Gigatron.VirtualMachine = undefined; //std.mem.zeroes(Gigatron.VirtualMachine);
    var blinken: Gigatron.BlinkenLights = undefined; //std.mem.zeroes(Gigatron.Leds);
    var vga: Gigatron.VgaMonitor = undefined; //std.mem.zeroes(Gigatron.VgaMonitor);
    var plugface: Gigatron.PluggyMcPlugface = undefined;
    
    const current_dir = std.fs.cwd();
    const rom_file = try current_dir.openFile("ROMv5a.rom", .{ .read = true });
    var reader = rom_file.reader();
    _ = try vm.loadRom(reader);
    rom_file.close();

    vm.start();
    
    plugface.init();

    var i = @as(u32, 0);
    while(i < 1_000_000_000):(i += 1) {
        vm.cycle();
        blinken.cycle(&vm);
        if(blinken.last == @truncate(u4, vm.reg.xout & 0x0F)) std.debug.print("LED: {any}\n", .{blinken.leds});
        plugface.cycle(&vm);
        if(vga.cycle(&vm)) {
            std.debug.print("==render==\n", .{});
        }
    }
}