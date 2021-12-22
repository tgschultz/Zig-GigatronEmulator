//https://github.com/kervinck/gigatron-rom
//https://gigatron.io/media/Gigatron-manual
//https://cdn.hackaday.io/files/20781889094304/Schematics%202020-03-20.pdf
//https://faculty-web.msoe.edu/johnsontimoj/EE3921/files3921/vga_basics.pdf
//https://forum.gigatron.io/viewtopic.php?f=4&t=248
//https://github.com/kervinck/gigatron-rom/blob/master/Contrib/pkupper/BabelFish/BabelFish.ino

const std = @import("std");
const builtin = @import("builtin");
const native_endian = builtin.target.cpu.arch.endian();

//@TODO: Allow for other clock rates
pub const clock_rate = 6_250_000; //6.25Mhz

//@TODO: Include a transalted version of the C VM for speed comparison and testing

//@TODO: Discuss with Zig whether or not endian-dependent
//bitfield ordering is actually a good idea.

pub const Instruction = switch(native_endian) {
    //instruction format:
    // [Operation][Mode][Bus]
    // 3-3-2 bit

    .Big => packed struct {
        usingnamespace InstructionCommon;
        operation: @This().Operation,
        mode:      @This().Mode,
        bus:       @This().Bus,
    },
    .Little => packed struct {
        usingnamespace InstructionCommon;
        bus:       @This().Bus,
        mode:      @This().Mode,
        operation: @This().Operation,
    },
};

const InstructionCommon = struct {
    pub const Operation = enum(u3) {
        load   = 0,
        @"and" = 1,
        @"or"  = 2,
        xor    = 3,
        add    = 4,
        sub    = 5,
        store  = 6,
        jump   = 7,
    };
    
    pub const Bus = enum(u2) {
        d   = 0,
        ram = 1,
        ac  = 2,
        in  = 3,
    };

    pub const Mode = enum(u3) {
        d    = 0,
        x    = 1,
        yd   = 2,
        yx   = 3,
        d_x  = 4,
        d_y  = 5,
        d_o  = 6,
        yxi  = 7,
    };

    pub const JumpMode = enum(u3) {
        jmp = 0,
        gt  = 1,
        lt  = 2,
        ne  = 3,
        eq  = 4,
        ge  = 5,
        le  = 6,
        bra = 7,
    };

    pub fn format(
        self: anytype,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const fmt_str = "{s: <5} {s: <7} {s: <7}";
        
        const op = switch(self.operation) {
            .load   => "LD",
            .@"and" => "AND",
            .@"or"  => "OR",
            .xor    => "XOR",
            .add    => "ADD",
            .sub    => "SUB",
            .store  => "ST",
            .jump   => "JMP",
        };
        
        switch(self.operation) {
            .load,
            .@"and",
            .@"or",
            .xor,
            .add,
            .sub => {
                const dest = switch(self.mode) {
                    .d    => "AC",
                    .x    => "AC",
                    .yd   => "AC",
                    .yx   => "AC",
                    .d_x  => "X",
                    .d_y  => "Y",
                    .d_o  => "OUT",
                    .yxi  => "OUT",
                };
                
                const src = switch(self.bus) {
                    .d   => "D",
                    .ram => switch(self.mode) {
                        .d    => "[D]",
                        .x    => "[X]",
                        .yd   => "[Y:D]",
                        .yx   => "[Y:X]",
                        .d_x  => "[D],X",
                        .d_y  => "[D],Y",
                        .d_o  => "[D]",
                        .yxi  => "[Y:X++]",
                    },
                    .ac  => "AC",
                    .in  => "IN",
                };
                try std.fmt.format(writer, fmt_str, .{op, dest, src});
            },
            .store => {
                const dest = switch(self.mode) {
                    .d    => "[D]",
                    .x    => "[X]",
                    .yd   => "[Y:D]",
                    .yx   => "[Y:X]",
                    .d_x  => "[D],X",
                    .d_y  => "[D],Y",
                    .d_o  => "[D]",
                    .yxi  => "[Y:X++]",
                };
                
                const src = switch(self.bus) {
                    .d   => "D",
                    .ram => "CTRL",
                    .ac  => "AC",
                    .in  => "IN",
                };
                try std.fmt.format(writer, fmt_str, .{op, dest, src});
            },
            .jump  => {
                const jump_mode = @intToEnum(Instruction.JumpMode, @enumToInt(self.mode));
                const jmp = switch(jump_mode) {
                    .jmp => "FAR",
                    .gt  => ">0",
                    .lt  => "<0",
                    .ne  => "!=0",
                    .eq  => "==0",
                    .ge  => ">=0",
                    .le  => "<=0",
                    .bra => "",
                };
                
                const dest = switch(self.bus) {
                    .d   => if(jump_mode == .jmp) "Y:D"   else "d",
                    .ram => if(jump_mode == .jmp) "Y:[D]" else "[0:d]",
                    .ac  => if(jump_mode == .jmp) "Y:AC"  else "ac",
                    .in  => if(jump_mode == .jmp) "Y:IN"  else "in",
                };
                try std.fmt.format(writer, fmt_str, .{op, jmp, dest});
            },
        }
    }
};

//Generate a dedicated function for the given 8bit instruction
pub fn instructionFn(comptime instr: Instruction) fn(vm: *VirtualMachine)void {
    return struct {
        //Get the value from whatever bus is valid in this
        // instruction.
        pub inline fn getBus(vm: *VirtualMachine) u8 {
            //@TODO: Allow for 64kb ram option
            const page = vm.reg.y & 0x7F;
            return switch(instr.bus) {
                .d   => vm.reg.d,
                .ram => switch(instr.operation) {
                        .jump => vm.ram[0][vm.reg.d],
                        //@TODO: some actual randomness. Tehcnically this is accurate because
                        // 'undefined' means whatever we want it to mean, but come on.
                        //.store => //undefined, 
                        else => switch(instr.mode) {
                            .d    => vm.ram[0][vm.reg.d],
                            .x    => vm.ram[0][vm.reg.x],
                            .yd   => vm.ram[page][vm.reg.d],
                            .yx   => vm.ram[page][vm.reg.x],
                            .d_x  => vm.ram[0][vm.reg.d],
                            .d_y  => vm.ram[0][vm.reg.d],
                            .d_o  => vm.ram[0][vm.reg.d],
                            .yxi  => v: {
                                const val = vm.ram[page][vm.reg.x];
                                vm.reg.x +%= 1;
                                break :v val;
                            },
                        },
                },
                .ac  => vm.reg.ac,
                .in  => vm.reg.in,
            };
        }
        
        pub inline fn setPcLow(vm: *VirtualMachine, delay_slot: u16) void {
            vm.reg.pc = (delay_slot & 0xFF00) | getBus(vm);
        }
        
        pub fn instFn(vm: *VirtualMachine) void {
            const delay_slot = vm.reg.pc;
            vm.reg.pc +%= 1;
            
            const dest_reg = switch(instr.mode) {
                .d, .x, .yd, .yx => "ac",
                .d_x             => "x",
                .d_y             => "y",
                .d_o, .yxi       => "out",
            };
            
            switch(instr.operation) {
                .load   => @field(vm.reg, dest_reg) = getBus(vm),
                .@"and" => @field(vm.reg, dest_reg) = vm.reg.ac & getBus(vm),
                .@"or"  => @field(vm.reg, dest_reg) = vm.reg.ac | getBus(vm),
                .xor    => @field(vm.reg, dest_reg) = vm.reg.ac ^ getBus(vm),
                .add    => @field(vm.reg, dest_reg) = vm.reg.ac +% getBus(vm),
                .sub    => @field(vm.reg, dest_reg) = vm.reg.ac -% getBus(vm),
                .store  => {
                    //@TODO: Allow for 64kb ram option
                    const page = vm.reg.y & 0x7F;
                    switch(instr.mode) {
                        .d    => vm.ram[0][vm.reg.d] = getBus(vm),
                        .x    => vm.ram[0][vm.reg.x] = getBus(vm),
                        .yd   => vm.ram[page][vm.reg.d] = getBus(vm),
                        .yx   => vm.ram[page][vm.reg.x] = getBus(vm),
                        .d_x  => {
                            vm.ram[0][vm.reg.d] = getBus(vm);
                            vm.reg.x = vm.reg.ac;
                        },
                        .d_y  => {
                            vm.ram[0][vm.reg.d] = getBus(vm);
                            vm.reg.y = vm.reg.ac;
                        },
                        .d_o  => vm.ram[0][vm.reg.d] = getBus(vm),
                        .yxi  => {
                            vm.ram[page][vm.reg.x] = getBus(vm);
                            vm.reg.x +%= 1;
                        },
                    }
                },
                .jump  => {
                    const jump_mode = @intToEnum(Instruction.JumpMode, @enumToInt(instr.mode));
                    const signed_ac = @bitCast(i8, vm.reg.ac);
                    switch(jump_mode) {
                        .jmp => vm.reg.pc = (@as(u16, vm.reg.y) << 8) | getBus(vm),
                        .gt  => if(signed_ac > 0)  setPcLow(vm, delay_slot),
                        .lt  => if(signed_ac < 0)  setPcLow(vm, delay_slot),
                        .ne  => if(vm.reg.ac != 0) setPcLow(vm, delay_slot),
                        .eq  => if(vm.reg.ac == 0) setPcLow(vm, delay_slot),
                        .ge  => if(signed_ac >= 0) setPcLow(vm, delay_slot),
                        .le  => if(signed_ac <= 0) setPcLow(vm, delay_slot),
                        .bra => setPcLow(vm, delay_slot),
                    }
                },
            }
            
            //For the convenience of peripherals we keep track of the
            // vsync and hsync pin state.
            const hsync = vm.reg.out & 0x40 > 0;
            const vsync = vm.reg.out & 0x80 > 0;
            switch(vm.hsync) {
                .rising, .high => vm.hsync = if(hsync) .high else .falling,
                .falling, .low => vm.hsync = if(hsync) .rising else .low,
            }
            switch(vm.vsync) {
                .rising, .high => vm.vsync = if(vsync) .high else .falling,
                .falling, .low => vm.vsync = if(vsync) .rising else .low,
            }
            
            if(vm.hsync == .rising) vm.reg.xout = vm.reg.ac;

            vm.reg.ir = vm.rom[delay_slot][0];
            vm.reg.d = vm.rom[delay_slot][1];
        }
    }.instFn;
}

pub const VirtualMachine = struct {
    rom: [65536][2]u8, //ROM 128k, 0 is instruction, 1 is data
    ram: [128][256]u8, //RAM is 32k. 128 pages of 256 bytes each. @TODO: Allow for 64k ram option
    reg: Registers,
    
    vsync: Signal,
    hsync: Signal,

    pub const Signal = enum {
        rising,
        falling,
        high,
        low,
    };
    
    pub const Registers = struct {
        ac:   u8,
        d:    u8,
        x:    u8,
        y:    u8,
        in:   u8,
        out:  u8,
        xout: u8,
        
        pc:  u16,
        ir:  u8,
    };
    
    //Generate a separate function for every possible instruction
    pub const instructions = tab: {
        var table: [256]fn(vm: *VirtualMachine)void = undefined;
        for(table) |*e, i| e.* = instructionFn(@bitCast(Instruction, @as(u8, i)));
        break :tab table;
    };
    
    pub fn loadRom(self: *@This(), reader: anytype) !usize {
        const rom_as_bytes = std.mem.asBytes(&self.rom);
        const used_size = try reader.read(rom_as_bytes);
        return used_size;
    }
    
    //@TODO: randomize the values in other registers and ram.
    // Although, again, 'undefined' means this is correct too.
    pub fn start(self: *@This()) void {
        self.reg.pc = 0;
        self.reg.ir = self.rom[0][0];
        self.reg.d = self.rom[0][1];
        self.reg.in = 0xFF;
    }

    pub fn cycle(self: *@This()) void {
        instructions[self.reg.ir](self);
    }
};

//@TODO: Peripherals:
// Loader
// rest of the BabelFish modes
//////////////////////


//@TODO: This could be cleaner, and it could
// also be more compatible with alternative timings
// and cpu clock rates.
//Consider double buffering here so theoretically
// a thread could pull video at any point during
// instead of waiting for a signal here.
//
// why 28 instead of 33 lines of vsync back porch?
////Explained in ROMv5a.asm.py lines 187-206:
////https://github.com/kervinck/gigatron-rom/blob/2fedec7804e4d0ed809dc780e44f2fa01583cc3d/Core/ROMv5a.asm.py#L187-L206
// # VGA 640x480 defaults (to be adjusted below!)
// vFront = 10     # Vertical front porch
// vPulse = 2      # Vertical sync pulse
// vBack  = 33     # Vertical back porch
// vgaLines = vFront + vPulse + vBack + 480
// vgaClock = 25.175e+06
// 
// # Video adjustments for Gigatron
// # 1. Our clock is (slightly) slower than 1/4th VGA clock. Not all monitors will
// #    accept the decreased frame rate, so we restore the frame rate to above
// #    minimum 59.94 Hz by cutting some lines from the vertical front porch.
// vFrontAdjust = vgaLines - int(4 * cpuClock / vgaClock * vgaLines)
// vFront -= vFrontAdjust
// # 2. Extend vertical sync pulse so we can feed the game controller the same
// #    signal. This is needed for controllers based on the 4021 instead of 74165
// vPulseExtension = max(0, 8-vPulse)
// vPulse += vPulseExtension
// # 3. Borrow these lines from the back porch so the refresh rate remains
// #    unaffected
// vBack -= vPulseExtension
pub const VgaMonitor = struct {
    pixels: [vid_width * vid_height]Pixel = undefined,
    state: union(enum) {
        v_blank:      void, //wait for vsync to go high
        v_back_porch: u5, //count hsync pulses
        v_visible:    struct {
            y: u9,
            state: union(enum) {
                h_blank:       void, //wait for hsync to go high
                h_back_porch:  u4, //count cycles
                h_visible:     u10, //count pixels
                h_front_porch: void, //wait for h blank
            },
        },
        v_front_porch: void, //wait for vsync to go low
    } = .{.v_blank = void{}},

    pub const vid_width  = 640;
    pub const vid_height = 480;
    //in gigatron clocks @TODO: needs overhaul for alternative clock rates
    const h_visible      = 160;
    const h_back_porch   = 12;
    const h_front_porch  = 40;
    const h_cycle = h_back_porch + h_visible + h_front_porch;
    
    //in h_cycles
    const v_visible     = 480;
    const v_back_porch  = 28; //(normally 33, ROM adjusts)
    const v_front_porch = 7; //(normally 10, ROM adjusts)
    const v_cycle = v_back_porch + v_visible + v_front_porch; //ignores the vblank
    
    pub const Pixel = switch(native_endian) {
        //out byte is VHBBGGRR
        .Big => packed struct {
            vsync: u1,
            hsync: u1,
            b:     u2,
            g:     u2,
            r:     u2,
        },
        .Little => packed struct {
            r:     u2,
            g:     u2,
            b:     u2,
            hsync: u1,
            vsync: u1,
        },
    };
    
    //convert a single color to another type, rescaling
    // to that type's range as appropriate
    fn rescale(comptime To: type, from: anytype) To {
        const From = @TypeOf(from);
        
        if(To == From) return from;
        
        const to_max = std.math.maxInt(To);
        const from_max = std.math.maxInt(From);
    
        if(@bitSizeOf(From) == 0) return to_max;
        if(@bitSizeOf(To) == 0) return 0;
        
        const Big = std.meta.Int(.unsigned, @bitSizeOf(To) + @bitSizeOf(From));
        return @intCast(To, (@as(Big, from) * to_max) / from_max);
    }

    //Allow the caller to specify whatever (rgb, uncompressed)
    // pixel format they want and convert the color value for them
    pub fn convert(comptime Out: type, in: anytype) Out {
        const out: Out = undefined;
        const R = @TypeOf(@field(out, "r"));
        const G = @TypeOf(@field(out, "g"));
        const B = @TypeOf(@field(out, "b"));
        
        return .{
            .r = rescale(R, @field(in, "r")),
            .g = rescale(G, @field(in, "g")),
            .b = rescale(B, @field(in, "b")),
        };
    }

    pub fn cycle(self: *@This(), vm: *VirtualMachine) bool {
        const px = @bitCast(Pixel, vm.reg.out);
        
        switch(self.state) {
            .v_blank => {
                if(vm.vsync == .rising) self.state = .{.v_back_porch = 0};
            },
            .v_back_porch => |*h_pulses| {
                if(vm.hsync == .rising) {
                    if(h_pulses.* >= v_back_porch - 1) {
                        self.state = .{.v_visible = .{
                            .y = 0,
                            //not h_blank because we're already rising, which means
                            // we're already 1 cycle in
                            .state = .{.h_back_porch = 1}, 
                        }};
                        return false;
                    }
                    h_pulses.* += 1;
                }
            },
            .v_visible => |*vis| switch(vis.state) {
                .h_blank => if(vm.hsync == .rising) {
                    vis.y += 1;
                    if(vis.y >= vid_height) {
                        self.state = .{.v_front_porch = void{}};
                        return false;
                    }
                    //already rising, so already 1 cycle in
                    vis.state = .{.h_back_porch = 1}; 
                },
                .h_back_porch => |*count| {
                    if(count.* >= h_back_porch - 1) {
                        vis.state = .{.h_visible = 0,};
                        return false;
                    }
                    count.* += 1;
                },
                .h_visible => |*x| {
                    if(x.* >= vid_width) {
                        vis.state = .{.h_front_porch = void{}};
                        return false;
                    }
                    const idx: usize = (@as(usize, vis.y) * vid_width) + @as(usize, x.*);
                    for(self.pixels[idx..idx + 4]) |*p| p.* = px;
                    x.* += 4;
                },
                .h_front_porch => {
                    if(vm.hsync == .falling) vis.state = .{.h_blank = void{}};
                }
            },
            .v_front_porch => if(vm.vsync == .falling) {
                self.state = .{.v_blank = void{}};
                return true;
            },
        }
        return false;
    }
};

//By your command
pub const BlinkenLights = struct {
    pub fn sample(vm: *VirtualMachine) [4]bool {
        const leds = @truncate(u4, vm.reg.xout & 0x0F);
        return .{
            leds & 0x1 > 0,
            leds & 0x2 > 0,
            leds & 0x4 > 0,
            leds & 0x8 > 0,
        };
    }
};

//used by both gamepad and mcplugface
// to track gamepad and emulated gamepad
// input states
pub const Buttons = switch(native_endian) {
    .Big => packed struct {
        a:      u1 = 1,
        b:      u1 = 1,
        select: u1 = 1,
        start:  u1 = 1,
        up:     u1 = 1,
        down:   u1 = 1,
        left:   u1 = 1,
        right:  u1 = 1,
    },
    .Little => packed struct {
        right:  u1 = 1,
        left:   u1 = 1,
        down:   u1 = 1,
        up:     u1 = 1,
        start:  u1 = 1,
        select: u1 = 1,
        b:      u1 = 1,
        a:      u1 = 1,
    },
};

//Gamepad uses 4012B shift register
//While the Paralell/Serial control line (latch) is held high
// the clock is ignored and the register is latched to
// the button inputs (which are active-low).
//VSYNC is tied to latch and HSYNC to clock.
// on HSYNC rising edge *only when VSYNC is low*, shift one bit
// from top of register to bottom of IN
pub const Gamepad = struct {
    register: u8,
    buttons: Buttons,

    pub fn cycle(self: *@This(), vm: *VirtualMachine) void {
        //clock is disabled when vsync is high
        //buttons are latched when it is falling
        switch(vm.vsync) {
            .falling => self.register = @bitCast(u8, self.buttons),
            .low     => {
                if(vm.hsync == .rising) {
                    const bit = (self.register & 0x80) >> 7;
                    vm.reg.in <<= 1;
                    vm.reg.in |= bit;
                    self.register <<= 1;
                }
            },
            else => {},
        }
    }
};

//@TODO: MSBASIC save still not working...
// pretty sure it is broken on Gigatron ROM end
// https://github.com/kervinck/gigatron-rom/issues/205
pub const BabelFish = struct {
    frame:     anyframe        = undefined,
    coroutine: @Frame(run)     = undefined,
    vm:        *VirtualMachine = undefined,
    tape:      Tape            = .{},
    buttons:   Buttons         = .{},
    key:       Key             = .{ .none = void{}, },
    
    const Tape = struct {
        data:       []u8  = std.mem.zeroes([]u8),
        pos:        usize = 0,
        line_empty: bool  = true,
    };
    
    const Key = union(enum) {
        none:  void,
        //technically u7, but who needs all that casting?
        ascii: u8,
        ctrl:  ControlKey,
    };
    
    const ControlKey = enum {
        load,
    };
    
    pub fn init(self: *@This(), tape: []u8) void {
        self.tape.data = tape;
        self.coroutine = async run(self);
    }

    pub fn cycle(self: *@This(), vm: *VirtualMachine) void {
        self.vm = vm;
        resume self.frame;
    }
    
    pub fn run(self: *@This()) void {
        suspend {self.frame = @frame();}
        
        var byte: u8 = 0;
        var bits: u4 = 0;
        while(true) {
            //count vsync pulses trying to form a byte
            
            //wait for vsync to be falling so we know we're at the
            //start, in case we looped back here in the middle of a pulse
            while(self.vm.vsync != .falling) { suspend {} }
            
            var count: u4 = 0;
            while(self.vm.vsync != .rising) {
                if(self.vm.hsync == .rising) count += 1;
                //BabelFish sends ones by default
                self.vm.reg.in <<= 1;
                self.vm.reg.in |= 1;
                suspend {}
            }
            
            switch(count) {
                7 => {
                    byte >>= 1;
                    bits += 1;
                },
                9 => {
                    byte >>= 1;
                    byte |= 0x80;
                    bits += 1;
                },
                else => {
                    byte = 0;
                    bits = 0;
                },
            }
            
            if(bits == 8) {
                self.recordTapeByte(byte);
                bits = 0;
                byte = 0;
            }
            
            //handle input stuff
            
            //repeat inputs without returning to the rest
            // of the loop as long as there is input
            // to process

            while(@bitCast(u8, self.buttons) != 0xFF) {
                self.sendKey(@bitCast(u8, self.buttons), 1);
                suspend {}
            }
            
            while(self.key != .none) {
                switch(self.key) {
                    .none => unreachable,
                    .ascii => |k| {
                        //we do this before so when we come back we don't overwrite a waiting key
                        self.key = .{ .none = void{}, };
                        self.sendKey(k, 2);
                    },
                    .ctrl => {
                        self.key = .{ .none = void{}, };
                        self.replayTape();
                    }
                }
                suspend {}
            }
        }
    }
    
    //wait for given number of emulated milliseconds
    fn waitMs(self: *@This(), ms: usize) void {
        const prev_frame = self.frame;
        self.frame = @frame();
        defer self.frame = prev_frame;

        const clk_per_ms = clock_rate / 1000;
        var cycles = ms * clk_per_ms;
        while(cycles > 0) : (cycles -= 1) { suspend {} }
    }
    
    //send controller data or keyboard key for frames
    fn sendKey(self: *@This(), byte: u8, frames: u8) void {
        const prev_frame = self.frame;
        self.frame = @frame();
        defer self.frame = prev_frame;
    
        while(self.vm.vsync != .falling) { suspend {} }
        suspend {} //suspend one more cycle so we're in the pulse
        
        var register = byte;
        var f = frames;
        while(f > 0) : (f -= 1) {
            while(self.vm.vsync == .low) {
                if(self.vm.hsync == .rising) {
                    const bit = (register & 0x80) >> 7;
                    self.vm.reg.in <<= 1;
                    self.vm.reg.in |= bit;
                    register <<= 1;
                    //BabelFish sends ones by default
                    register |= 1;
                }
                suspend {}
            }
        }
    }
    
    //replay the text stored in the buffer (usually a basic program)
    fn replayTape(self: *@This()) void {
        var line_delay: usize = 50;
        var line_idx: usize = 0;
        for(self.tape.data[0..self.tape.pos]) |byte| {
            line_idx += 1;
            self.sendKey(byte, 2);
            
            //delay extra at end of displayable line
            const delay: usize = if(line_idx % 26 == 0) 300 else 20;
            self.waitMs(delay);
            
            //additional line delays because MSBASIC
            // in particular is slow
            if(byte == '\r') {
                line_delay = 300 + (line_idx * 50);
            } else if(byte == '\n') {
                self.waitMs(line_delay);
                line_idx = 0;
            }
        }
    }
    
    //append a byte to the buffer (usually a basic program)
    fn recordTapeByte(self: *@This(), byte: u8) void {
        //if we're out of space send a long break key,
        // unless the line is empty
        if(self.tape.pos != self.tape.data.len) {
            self.tape.data[self.tape.pos] = byte;
            self.tape.pos += 1;
        } else if(!self.tape.line_empty) {
            self.sendKey(0x03, 10);
        }
        
        if(byte >= 32) {
            self.tape.line_empty = false;
        } else if(byte == '\n') {
            //Two blank lines means: clear the tape.
            if(self.tape.line_empty) {
                self.tape.pos = 0;
            }
            self.tape.line_empty = true;
        }
    }
    
    //pub fn loadGt1(self: *@This()) void {
    //    //60-bytes per frame
    //    // 
    //    
    //}
    
    ///////////////
    
    pub fn asciiKeyPress(self: *@This(), key: u8) void {
        self.key = .{ .ascii = key };
    }
    
    pub fn controlKeyPress(self: *@This(), control_key: ControlKey) void {
        self.key = .{ .ctrl = control_key, };
    }
};


//Given a sample rate, the Audio peripheral will
// handle the bandpass filtering of the output.
//The shcematic indicates a low pass filter of 700Hz
// and a high pass of 160Hz. However, I am too tone
// deaf to determine from recordings if these values
// produce correct results when compared to youtube
// recordings of real Gigatrons, assuming of course my
// algorithms are even correct. 
pub const Audio = struct {
    lpf_pv: f32 = 0.0,
    hpf_pv: f32 = 0.0,
    lpf_a:  f32,
    hpf_a:  f32,
    volume: f32 = 1.0,
    
    const lpf_tau_ms = (1.0 / 700.0) * 1000.0;
    const hpf_tau_ms = (1.0 / 160.0) * 1000.0;
    
    pub fn init(rate: u32) @This() {
        const dt = std.time.ms_per_s / @intToFloat(f32, rate);
        return .{
            .lpf_a = dt / lpf_tau_ms,
            .hpf_a = dt / hpf_tau_ms,
        };
    }

    pub fn sample(self: *@This(), vm: *VirtualMachine) f32 {
        const level = (vm.reg.xout & 0xF0) >> 4;
        
        //normalize the 4-bit int to a float [-1.0,1.0]
        const level_norm = ((@intToFloat(f32, level) / 15.0) * 2.0) - 1.0;
        
        self.lpf_pv += self.lpf_a * (level_norm - self.lpf_pv);
        self.hpf_pv += self.hpf_a * (level_norm - self.hpf_pv);
        
        return (self.hpf_pv - self.lpf_pv) * self.volume;
    }
};