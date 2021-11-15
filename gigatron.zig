//https://github.com/kervinck/gigatron-rom
//https://gigatron.io/media/Gigatron-manual
//https://cdn.hackaday.io/files/20781889094304/Schematics%202020-03-20.pdf
//https://faculty-web.msoe.edu/johnsontimoj/EE3921/files3921/vga_basics.pdf
//https://forum.gigatron.io/viewtopic.php?f=4&t=248

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
        operation: InstructionCommon.Operation,
        mode:      InstructionCommon.Mode,
        bus:       InstructionCommon.Bus,
    },
    .Little => packed struct {
        usingnamespace InstructionCommon;
        bus:       InstructionCommon.Bus,
        mode:      InstructionCommon.Mode,
        operation: InstructionCommon.Operation,
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
// rest of the BableFish modes
//////////////////////


//@TODO: rejigger the logic
// try to avoid so many branches and mod/div
// Alternatively: rewrite so it is not dependent
// on gigatron rom timings and instead performs
// more like a real vga monitor would for non-std
// software. For that, I guess do timings from the
// *start* of the pulses?
//
//Consider rewriting as a state machine
//
//Consider double buffering here so theoretically
// a thread could pull video while emulation continues
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
    clock:  usize,
    pixels: [vid_width * vid_height]Pixel,

    pub const vid_width = 640;
    pub const vid_height = 480;
    //in gigatron clocks @TODO: needs overhaul for alternative clock rates
    const h_visible = 160;
    const h_back_porch = 12;
    const h_front_porch = 40;
    const h_cycle = h_back_porch + h_visible + h_front_porch;
    
    //in h_cycles
    const v_visible = 480;
    const v_back_porch = 28; //(normally 33, ROM adjusts)
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
        
        switch(vm.vsync) {
            .falling => return true,
            .low => return false,
            .rising => {
                self.clock = 0;
                return false;
            },
            .high => switch(vm.hsync) {
                .falling, .low => return false,
                .rising => {
                    //align clock to the nearest H-cycle
                    self.clock = self.clock - (self.clock % h_cycle) + h_cycle;
                },
                .high => {},
            },
        }

        //Top edge of visible
        if(self.clock < (v_back_porch * h_cycle)) return false;
        //Bottom edge of visibe
        if(self.clock >= ((v_cycle - v_front_porch) * h_cycle)) return false;
                    
        const x = self.clock % h_cycle;
        //left edge of visible
        if(x < h_back_porch) {
            self.clock += 1;
            return false;
        }
        //right edge of visible
        if(x >= (h_cycle - h_front_porch)) return false;
        
        const y = self.clock / h_cycle;
        const y_index = (y - v_back_porch) * vid_width;
        const x_offset = (x - h_back_porch) * 4; //4 vga color clocks per gigatron clock
        const i = y_index + x_offset;
        
        for(self.pixels[i..(i + 4)]) |*p| p.* = px;
        
        self.clock += 1;
        return false;
    }
};

//By your command
pub const BlinkenLights = struct {
    last: u4,
    leds: [4]bool,
    
    pub fn cycle(self: *@This(), vm: *VirtualMachine) void {
        const curr = @truncate(u4, vm.reg.xout & 0x0F);
        //only bother to update leds if anything actually changed
        if(curr == self.last) return;
        self.last = curr;
        self.leds[0] = curr & 0x1 > 0;
        self.leds[1] = curr & 0x2 > 0;
        self.leds[2] = curr & 0x4 > 0;
        self.leds[3] = curr & 0x8 > 0;
    }
};

//used by both gamepad and mcplugface
// to track gamepad and emulated gamepad
// input states
pub const Buttons = switch(native_endian) {
    .Big => packed struct {
        a:      u1,
        b:      u1,
        select: u1,
        start:  u1,
        up:     u1,
        down:   u1,
        left:   u1,
        right:  u1,
    },
    .Little => packed struct {
        right:  u1,
        left:   u1,
        down:   u1,
        up:     u1,
        start:  u1,
        select: u1,
        b:      u1,
        a:      u1,
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

//@TODO: All behavior involving delays after sending characters
// (mostly in the loading), is not really correct as I'm just
// using extended frame counts instead of waiting.

//@TODO: I'm not doing the things necesary to load
// programs in MSBASIC (which requires very very long delays.)

//@TODO: Plugface load TinyBasic from on-board 8k flash
pub const PluggyMcPlugface = struct {
    state: State,
    buttons: Buttons,

    data: [510]u8, //double check this value, why not 512?
    data_end: u10,

    pub const State = union(enum) {
        idle: struct {
            pulses: u4 = 0,
        },
        send: struct {
            value: u8,
            register: u8,
            bits: u4 = 8,
            frames: u8 = 1,
        },
        save: struct {
            register: u8,
            bits: u4,
            pulses: u4 = 0,
            line_empty: bool = true,
        },
        load: struct {
            index: u10,
            frames: u8 = 4,
            register: u8,
            bits: u4 = 8,
        },
    };
    
    pub const ControlKey = enum {
        load,
    };
    
    pub fn init(self: *@This()) void {
        self.state = .{.idle = .{}};
        self.buttons = @bitCast(Buttons, @as(u8, 0xFF));
        self.data_end = 0;
    }

    //Mimics a 4012B shift register
    // see gamepad notes for more details
    //McPlugace emulates the gamepad using up/down/left/right arrow
    // for dpad, pgdown/up for select/start, end/del/bksp for A and
    // home or insert for B
    pub fn cycle(self: *@This(), vm: *VirtualMachine) void {
        switch(self.state) {
            .idle => |*idle| switch(vm.vsync) {
                .falling => {
                    const buttons_byte = @bitCast(u8, self.buttons);
                    if(buttons_byte != 0xFF) {
                        self.state = .{.send = .{
                            .value = buttons_byte,
                            .register = buttons_byte,
                        }};
                    } else idle.pulses = 0;
                },
                .low => if(vm.hsync == .rising) {
                    idle.pulses += 1;
                    //by default, BabelFish holds input high unless sending
                    vm.reg.in <<= 1;
                    vm.reg.in |= 0x01;
                },
                .rising => {
                    switch(idle.pulses) {
                        7 => self.state = .{.save = .{
                            .register = 0,
                            .bits = 1,
                        }},
                        9 => self.state = .{.save = .{
                            .register = 0x80, //bits are sent low-to-high
                            .bits = 1,
                        }},
                        else => {},
                    }
                },
                else => {}
            },

            .send => |*send| switch(vm.vsync) {
                .falling => {
                    send.register = send.value;
                    send.bits = 8;
                },
                .low => if(vm.hsync == .rising) {
                    //shift out register
                    const bit = (send.register & 0x80) >> 7;
                    vm.reg.in <<= 1;
                    vm.reg.in |= bit;
                    send.register <<= 1;
                    send.bits -= 1;
                    if(send.bits == 0) send.frames -= 1;
                },
                else => if(send.frames == 0) {
                    self.state = .{.idle = .{}};
                },
            },
            
            .save => |*save| switch(vm.vsync) {
                .falling => save.pulses = 0,
                .low => if(vm.hsync == .rising) {
                    save.pulses += 1;
                    //by default, BabelFish holds input high unless sending
                    vm.reg.in <<= 1;
                    vm.reg.in |= 0x01;
                },
                .rising => {
                     //bits are sent low-to-high
                    switch(save.pulses) {
                        7 => {
                            save.register >>= 1;
                            save.bits += 1;
                            if(save.bits == 8) {
                                self.saveChar(save.register);
                                save.bits = 0;
                            }
                        },
                        8 => {
                            if(save.bits !=0 ) self.breakSave();
                            self.state = .{.idle = .{}};
                            std.debug.print("saved data: \n==\n{s}\n==\n", .{self.data[0..self.data_end]});
                        },
                        9 => {
                            save.register >>= 1;
                            save.register |= 0x80;
                            save.bits += 1;
                            if(save.bits == 8) {
                                self.saveChar(save.register);
                                save.bits = 0;
                            }
                        },
                        else => self.breakSave(),
                    }
                },
                .high => { 
                    if(self.inputReady()) self.state = .{.idle = .{}};
                },
            },
            
            .load => |*load| switch(vm.vsync) {
                .falling => {
                    load.register = self.data[load.index];
                    load.bits = 8;
                },
                .low => if(vm.hsync == .rising) {
                    if(load.bits == 0) {
                        vm.reg.in <<= 1;
                        vm.reg.in |= 0x1;
                    } else {
                        //shift out register
                        const bit = (load.register & 0x80) >> 7;
                        vm.reg.in <<= 1;
                        vm.reg.in |= bit;
                        load.register <<= 1;
                        load.bits -= 1;
                    }
                },
                .rising => {
                    load.frames -= 1;
                    if(load.frames == 0) {
                        load.index += 1;
                        if(load.index >= self.data_end) {
                            self.state = .{.idle = .{}};
                        } else load.frames = switch(self.data[load.index]) {
                            '\n' => 8,
                            else => 4,
                        };
                    }
                },
                else => {},
            },
        }
    }
    
    fn inputReady(self: *@This()) bool {
        return @bitCast(u8, self.buttons) != 0xFF;
    }
    
    fn breakSave(self: *@This()) void {
        //invalidate save
        self.data_end = 0;
        self.asciiKeyPress(0x03); //ctrl-C (ascii EXT)
        self.state.send.frames = 10; //make it a long one
    }
    
    //Only printable chars count for line content. an empty line deletes
    // the stored program. all stored programs end with a newline, so
    // can check the last stored char >= 32 for empty line or not
    fn saveChar(self: *@This(), char: u8) void {
        const save = &self.state.save;
        if(char == '\n') {
            if(self.data_end == 0) return; //program already cleared, empty line does nothing
            if(save.line_empty) {
                //empty line is a signal to clear the current program
                self.data_end = 0;
                return;
            }
            if(self.data_end == self.data.len) {
                //Out of memory
                self.breakSave();
                return;
            }
            save.line_empty = true;
        } else if(char >= 32) save.line_empty = false;
        
        self.data[self.data_end] = char;
        self.data_end += 1;
    }
    
    pub fn asciiKeyPress(self: *@This(), ascii: u8) void {
        self.state = .{.send = .{
            .value = ascii,
            .register = ascii,
            .frames = 2,
        }};
    }
    
    pub fn controlKeyPress(self: *@This(), control_key: ControlKey) void {
        switch(control_key) {
            .load => {
                if(self.data_end == 0) return;
                self.state = .{.load = .{
                    .index = 0,
                    .register = self.data[0],
                }};
            },
        }
    }
};

//Given a sample rate, the Audio peripheral will
// handle the bandpass filtering of the output.
//The shcematic indicates a low pass filter of 700Hz
// and a high pass of 160Hz. However, I am too tone
// deaf to determine from recordings if these values
// produce correct results, assuming of course my
// algorithms are even correct. 
pub const Audio = struct {
    lpf_pv: f32 = 0.0,
    hpf_pv: f32 = 0.0,
    lpf_a: f32,
    hpf_a: f32,
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
        
        return self.hpf_pv - self.lpf_pv;
    }
};