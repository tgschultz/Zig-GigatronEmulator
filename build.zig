const std = @import("std");
const CrossTarget = std.zig.CrossTarget;

const BuildTarget = struct {
    target: CrossTarget,
    src_name: []const u8,
    exe_name: []const u8,
};

const build_targets = [_]BuildTarget{
    .{
        .target = .{.cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu},
        .exe_name = "zge-linux-x64",
        .src_name = "zge-linux.zig",
    },
    .{
        .target = .{.cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc},
        .exe_name = "zge-windows-x64",
        .src_name = "zge-windows.zig",
    },
    //.{
    //    .target = .{.cpu_arch = .mips, .os_tag = .linux, .abi = .gnu},
    //    .exe_name = "zge-linux-mips",
    //    .src_name = "zge-linux.zig",
    //},
};

pub fn build(b: *std.build.Builder) !void {
    const mode = b.standardReleaseOptions();

    // Until async support is added to Stage 2 we need .use_stage1 = true
    //for `anyframe` which is used in the BabelFish coroutine.
    b.use_stage1 = true;

    for(build_targets) |bt| {
        const zge = b.addExecutable(bt.exe_name, bt.src_name);
        zge.setTarget(bt.target);
        zge.setBuildMode(mode);
        zge.install();
    }
}
