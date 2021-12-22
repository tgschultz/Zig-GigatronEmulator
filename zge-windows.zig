const std = @import("std");
const builtin = @import("builtin");
const Gigatron = @import("gigatron.zig");

const win32 = @import("win32.zig");
const gl = @import("win32_gl.zig");

pub fn hrErr(hresult: win32.foundation.HRESULT) !void {
    if(hresult >= 0) return;
    std.log.err("HRESULT error: 0x{X:0>8}\n", .{@bitCast(u32, hresult)});
    return error.HResultError;
}

pub fn hrStatus(hresult: win32.foundation.HRESULT) !win32.foundation.HRESULT {
    if(hresult >= 0) return hresult;
    std.log.err("HRESULT error: 0x{X:0>8}\n", .{@bitCast(u32, hresult)});
    return error.HResultError;
}

pub fn intErr(atom: anytype) !@TypeOf(atom) {
    if(atom != 0) return atom;
    const err = win32.foundation.GetLastError();
    const err_code = @truncate(u16, @enumToInt(err));
    std.log.err("INT error: {}, last error: 0x{X:0>4} - {s}\n", .{atom, err_code, err});
    return error.IntError;
}

pub fn boolErr(b: win32.foundation.BOOL) !void {
    if(b != 0) return;
    const err = win32.foundation.GetLastError();
    const err_code = @truncate(u16, @enumToInt(err));
    std.log.err("BOOL error: 0x{X:0>4} - {s}\n", .{err_code, err});
    return error.BoolError;
}

//@NOTE: Calls to this function may force thread
// syncs that could negatively impact performance.
pub fn glErr(v: void) !void {
    _ = v;
    const err = gl.glGetError();
    if(err == gl.GL_NO_ERROR) return;
    return switch(err) {
        1280 => error.GL_INVALID_ENUM,
        1281 => error.GL_INVALID_VALUE,
        1282 => error.GL_INVALID_OPERATION,
        1283 => error.GL_STACK_OVERFLOW,
        1284 => error.GL_STACK_UNDERFLOW,
        1285 => error.GL_OUT_OF_MEMORY,
        
        else => {
            std.debug.panic("Unknown GL Error Code: {}\n", .{err});
        },
    };
}

//@TODO: Allow adjustable emulation speed

//@TODO: We want to be able to track multiple windows and allow them to use instance fields
// instead of globals. We *could* do https://devblogs.microsoft.com/oldnewthing/20140203-00/?p=1893
// but keep in mind we need to save the hWnd in the CREATE message for that to work.
// Alternatively we can just have a global for each window.
//
// Main window: vga output, blinkenlights
// Debugger: step, watch, disassembly, ram view, rom view, register view, babelfish data view

const gigatron_clock_rate = Gigatron.clock_rate;

var main_window: MainWindow = undefined;
var sound: Sound = undefined;

//@TODO: Allow for selectable resolution, fullscreen, handle HiDPI
//Handles main display and blinkenlights
const MainWindow = struct {
    const wf = win32.foundation;
    const wg = win32.graphics;
    const wnm = win32.ui.windows_and_messaging;
    
    hModule: wf.HINSTANCE,
    wndClass: wnm.WNDCLASSEXW,
    hWnd: wf.HWND,
    
    hDC: wg.gdi.HDC,
    wglRC: wg.open_gl.HGLRC,
    
    gl_texture: gl.GLuint,
    frame_buffer: [vid_width * (vid_height + blinken_height)]RGB888,
    
    buttons: *Gigatron.Buttons,
    babelfish: *Gigatron.BabelFish,

    const vid_width = Gigatron.VgaMonitor.vid_width;
    const vid_height = Gigatron.VgaMonitor.vid_height;
    const blinken_height = 10;
    
    const RGB888 = packed struct {
        r: u8,
        g: u8,
        b: u8,
    };
    
    fn init(b: *Gigatron.Buttons, bf: *Gigatron.BabelFish) !void {
        const self = &main_window;
        self.buttons = b;
        self.babelfish = bf;
        
        self.hModule = win32.system.library_loader.GetModuleHandleW(null) orelse return error.InvalidModuleHanlde;
        
        self.wndClass = std.mem.zeroes(wnm.WNDCLASSEXW);
        self.wndClass.cbSize = @sizeOf(wnm.WNDCLASSEXW);
        self.wndClass.style = wnm.CS_OWNDC;
        self.wndClass.lpfnWndProc = winProc;
        self.wndClass.hInstance = @ptrCast(
            wf.HINSTANCE,
            self.hModule,
        );
        self.wndClass.hCursor = wnm.LoadCursorW(
            null,
            wnm.IDC_ARROW,
        );
        self.wndClass.hbrBackground = @ptrCast(
            wg.gdi.HBRUSH,
            wg.gdi.GetStockObject(wg.gdi.BLACK_BRUSH),
        );
        self.wndClass.lpszClassName = std.unicode.utf8ToUtf16LeStringLiteral("ZGE");

        _ = try intErr(wnm.RegisterClassExW(&self.wndClass));
        
        var display_rect = wf.RECT{
            .left   = 0,
            .top    = 0,
            .right  = vid_width,
            .bottom = (vid_height + blinken_height),
        };
        
        //@TODO: use GetSystemMetrics instead of AdjustWindowRectEx because the latter is stupid. Alternatively
        // just create the window and then resize it afterwards by the difference between clientrect and its rect.
        try boolErr(wnm.AdjustWindowRectEx(
            &display_rect, 
            wnm.WS_CAPTION,
            win32.zig.FALSE,
            @intToEnum(wnm.WINDOW_EX_STYLE, 0),
        ));
        
        const window_style = @intToEnum(wnm.WINDOW_STYLE,
            @enumToInt(wnm.WS_OVERLAPPED)  |
            @enumToInt(wnm.WS_MINIMIZEBOX) |
            @enumToInt(wnm.WS_SYSMENU)
        );
        
        self.hWnd = wnm.CreateWindowExW(
            @intToEnum(wnm.WINDOW_EX_STYLE, 0),
            self.wndClass.lpszClassName,
            std.unicode.utf8ToUtf16LeStringLiteral("Zig Gigatron Emulator"), //why not Zigatron? Because that's tacky.
            window_style,
            wnm.CW_USEDEFAULT,
            wnm.CW_USEDEFAULT,
            display_rect.right - display_rect.left,
            display_rect.bottom - display_rect.top,
            null,
            null,
            @ptrCast(wf.HINSTANCE, self.hModule),
            null
        ) orelse return error.CreateWindowFailed;
        errdefer _ = wnm.DestroyWindow(self.hWnd);

        //We're going to use OpenGL to put a simple frame buffer on the screen because
        // frankly, it's the simplest way.
        self.hDC = wg.gdi.GetDC(self.hWnd) orelse return error.InvalidDc;
        errdefer _ = wg.gdi.ReleaseDC(self.hWnd, self.hDC);
        var pfd = std.mem.zeroes(wg.open_gl.PIXELFORMATDESCRIPTOR);
        pfd.nSize = @sizeOf(wg.open_gl.PIXELFORMATDESCRIPTOR);
        pfd.nVersion = 1;
        pfd.dwFlags = 
            wg.gdi.PFD_DRAW_TO_WINDOW
            | wg.gdi.PFD_SUPPORT_OPENGL
            | wg.gdi.PFD_DOUBLEBUFFER
            | wg.gdi.PFD_TYPE_RGBA
        ;
        pfd.cColorBits = 24;
        pfd.cDepthBits = 0;
        pfd.iLayerType = wg.gdi.PFD_MAIN_PLANE;
        
        const pfi = try intErr(wg.open_gl.ChoosePixelFormat(self.hDC, &pfd));
        try boolErr(wg.open_gl.SetPixelFormat(self.hDC, pfi, &pfd));
        
        self.wglRC = wg.open_gl.wglCreateContext(self.hDC) orelse return error.CreateGlContextFailure;
        errdefer _ = wg.open_gl.wglDeleteContext(self.wglRC);

        try boolErr(wg.open_gl.wglMakeCurrent(self.hDC, self.wglRC));

        try glErr(gl.glGenTextures(1, &self.gl_texture));
        
        //return indicates if window is already visible or not, we don't care.
        _ = wnm.ShowWindow(self.hWnd, wnm.SW_SHOW); 
        
        //This disables vsync, assuming the driver isn't forcing it
        // if we get forced to vsync then missed frames are likely.
        // We should probably do something about that, like run
        // the display copy in a different thread.
        const wglSwapIntervalEXT_proc_addr = wg.open_gl.wglGetProcAddress("wglSwapIntervalEXT") orelse return error.GetProcAddressFailure;
        const wglSwapIntervalEXT = @intToPtr(
            gl.PFNWGLSWAPINTERVALEXTPROC,
            @ptrToInt(wglSwapIntervalEXT_proc_addr)
        ).?;
        try boolErr(wglSwapIntervalEXT(0));
    }
    
    //Ye olde WNDPROC
    pub fn winProc(
        hWnd: wf.HWND,
        uMsg: c_uint,
        wParam: wf.WPARAM,
        lParam: wf.LPARAM
    ) callconv(std.os.windows.WINAPI) wf.LRESULT {
        const wkm = win32.ui.input.keyboard_and_mouse;
        
        const self = &main_window;
    
        //@TODO: actual gamepads, configurable controls
        switch(uMsg) {
            wnm.WM_CLOSE => win32.system.threading.ExitProcess(1),

            wnm.WM_SYSKEYDOWN,
            wnm.WM_KEYDOWN => {
                const key_param = @intToEnum(wkm.VIRTUAL_KEY, wParam);
                switch(key_param) {
                    wkm.VK_UP     => self.buttons.up     = 0,
                    wkm.VK_DOWN   => self.buttons.down   = 0,
                    wkm.VK_LEFT   => self.buttons.left   = 0,
                    wkm.VK_RIGHT  => self.buttons.right  = 0,
                    wkm.VK_NEXT   => self.buttons.select = 0, //pg down
                    wkm.VK_PRIOR  => self.buttons.start  = 0, //pg up
                    wkm.VK_HOME,
                    wkm.VK_INSERT => self.buttons.b      = 0, //home or insert
                    wkm.VK_END,
                    wkm.VK_DELETE,
                    wkm.VK_BACK   => self.buttons.a      = 0, //end, del, bksp
                    
                    //@TODO: this *should* be control+f3
                    wkm.VK_F3     => self.babelfish.controlKeyPress(.load),
                    else => return wnm.DefWindowProcW(hWnd, uMsg, wParam, lParam),
                }
            },
            wnm.WM_SYSKEYUP,
            wnm.WM_KEYUP => {
                const key_param = @intToEnum(wkm.VIRTUAL_KEY, wParam);
                switch(key_param) {
                    wkm.VK_UP     => self.buttons.up     = 1,
                    wkm.VK_DOWN   => self.buttons.down   = 1,
                    wkm.VK_LEFT   => self.buttons.left   = 1,
                    wkm.VK_RIGHT  => self.buttons.right  = 1,
                    wkm.VK_NEXT   => self.buttons.select = 1, //pg down
                    wkm.VK_PRIOR  => self.buttons.start  = 1, //pg up
                    wkm.VK_HOME,
                    wkm.VK_INSERT => self.buttons.b      = 1, //home or insert
                    wkm.VK_END,
                    wkm.VK_DELETE,
                    wkm.VK_BACK   => self.buttons.a      = 1, //end, del, bksp
                    
                    else => return wnm.DefWindowProcW(hWnd, uMsg, wParam, lParam),
                }
            },
            wnm.WM_CHAR => {
                if(wParam >=0 and wParam <= 127) {
                    //lparam:
                    //0-15 repeat count, 16-23 scan code, 24 extended key, 25-28 reserved, 29 alt, 30 prev state, 31 transition
                    //seems like it is never sent for UP transitions, but that's ok because the babelfish doesn't seem to 
                    //care anyway
                    const ascii = @truncate(u8, wParam);
                    
                    const key = if(ascii == '\r') '\n' else ascii; //return generates cr, but babelfish treats it as lf
                    self.babelfish.asciiKeyPress(key);
                } else return wnm.DefWindowProcW(hWnd, uMsg, wParam, lParam);
            },
            
            else => return wnm.DefWindowProcW(hWnd, uMsg, wParam, lParam),
        }
        
        return 0;
    }
    
    fn handleMessages(self: *@This()) void {
        var message: wnm.MSG = undefined;
        while(
            wnm.PeekMessageW(
                &message, 
                self.hWnd, 
                0, 
                0, 
                wnm.PM_REMOVE
            ) 
        != win32.zig.FALSE) {
            _ = wnm.TranslateMessage(&message);
            _ = wnm.DispatchMessageW(&message);
        }
    }
    
    fn destroy(self: *@This()) void {
        _ = wg.open_gl.wglMakeCurrent(null, null);
        _ = wg.open_gl.wglDeleteContext(self.wglRC);
        _ = wg.gdi.ReleaseDC(self.hWnd, self.hDC);
        _ = wnm.DestroyWindow(self.hWnd);
    }
    
    fn render(self: *@This(), vga: *Gigatron.VgaMonitor, leds: [4]bool) !void {
        //copy the monitor pixels into the frame buffer:
        for(vga.pixels[0..(vid_width * vid_height)]) |p, i| {
            self.frame_buffer[i] = Gigatron.VgaMonitor.convert(RGB888, p);
        }

        //draw the blinkenlights in an ugly 10px tall panel
        // at the bottom of the screen
        for(self.frame_buffer[(vid_width * vid_height)..]) |*p, i| {
            const x = i % vid_width;
            const black = RGB888{.r=0,  .g=0,  .b=0};
            const red = RGB888{.r=255,  .g=0,.b=0};
            const grey  = RGB888{.r=128,.g=128,.b=128};
            
            const spacing = 10;
            const width = 10;
            const start_0 = vid_width - ((width + spacing) * 4);
            const start_1 = start_0 + width + spacing;
            const start_2 = start_1 + width + spacing;
            const start_3 = start_2 + width + spacing;

            p.* = switch(x) {
                start_0...start_0 + (width - 1) => if(leds[0]) red else black,
                start_1...start_1 + (width - 1) => if(leds[1]) red else black,
                start_2...start_2 + (width - 1) => if(leds[2]) red else black,
                start_3...start_3 + (width - 1) => if(leds[3]) red else black,
                else => grey,
            };
        }
        
        try glErr({
            gl.glEnable(gl.GL_TEXTURE_2D);
            gl.glBindTexture(gl.GL_TEXTURE_2D, self.gl_texture);
            gl.glTexParameteri(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_MAG_FILTER, gl.GL_NEAREST);
            gl.glTexParameteri(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_MIN_FILTER, gl.GL_NEAREST);
            gl.glTexImage2D(
                gl.GL_TEXTURE_2D,
                0,
                gl.GL_RGB,
                vid_width,
                vid_height + blinken_height,
                0,
                gl.GL_RGB,
                gl.GL_UNSIGNED_BYTE,
                &self.frame_buffer,
            );
        });
        
        try glErr({
            gl.glBegin(gl.GL_QUADS);
                gl.glTexCoord2f(0.0, 0.0); 
                gl.glVertex2f(-1.0, 1.0);
                gl.glTexCoord2f(1.0, 0.0); 
                gl.glVertex2f(1.0, 1.0);
                gl.glTexCoord2f(1.0, 1.0); 
                gl.glVertex2f(1.0, -1.0);
                gl.glTexCoord2f(0.0, 1.0); 
                gl.glVertex2f(-1.0, -1.0);
            gl.glEnd();
            
            gl.glDisable(gl.GL_TEXTURE_2D);
        });
        
        try boolErr(wg.open_gl.SwapBuffers(self.hDC));
    }
};

//After some experimentation with different syncing strategies, I settled on
// using the size of the audio sample buffer to determine the passage of
// time. This was weirdly consistent compared to the alternatives.
pub const Sound = struct {
    const wca = win32.media.audio;
    const wf = win32.foundation;
    
    device_enumerator: *wca.IMMDeviceEnumerator,
    device: *wca.IMMDevice,
    client: *wca.IAudioClient,
    render_client: *wca.IAudioRenderClient,
    samples: Samples,
    sample_lock: bool,
    buffer_frames: c_uint,
    buffer_event: wf.HANDLE,
    
    const Samples = std.fifo.LinearFifo(f32, .{.Static = 2048});
    
    pub fn init() !@This() {
        var self: @This() = undefined; //someday we can use explicit result location
        
        //tell windows we want a 1ms scheduler granularity instead of the
        // 15ms eternity it uses by defualt. We do it here because we use
        // the sound buffer for timing now.
        //ignore any errors, shouldn't be possible and we can't do anything
        // about them anyway
        _ = win32.media.timeBeginPeriod(1);
        
        try hrErr(win32.system.com.CoCreateInstance(
            wca.CLSID_MMDeviceEnumerator,
            null,
            win32.system.com.CLSCTX_INPROC_SERVER,
            wca.IID_IMMDeviceEnumerator,
            @ptrCast(*?*anyopaque, &self.device_enumerator),
        ));
        errdefer _ = self.device_enumerator.IUnknown_Release();
        
        try hrErr(self.device_enumerator.IMMDeviceEnumerator_GetDefaultAudioEndpoint(
            .eRender,
            .eConsole,
            @ptrCast(*?@TypeOf(self.device), &self.device),
        ));
        errdefer _ = self.device.IUnknown_Release();

        try hrErr(self.device.IMMDevice_Activate(
            wca.IID_IAudioClient,
            @enumToInt(win32.system.com.CLSCTX_INPROC_SERVER),
            null,
            @ptrCast(*?*anyopaque, &self.client),
        ));
        errdefer _ = self.client.IUnknown_Release();
        
        //We can force the driver to handle sample conversion, so for
        // simplicity we set our sample rate to an even division of
        // the gigatron clock rate. Float is used because why not.
        const target_format = win32.media.audio.WAVEFORMATEX{
            .wFormatTag = win32.media.multimedia.WAVE_FORMAT_IEEE_FLOAT,
            .nChannels = 1,
            .nSamplesPerSec = gigatron_clock_rate / 100, 
            .nAvgBytesPerSec = (gigatron_clock_rate / 100) * @sizeOf(f32) * 1,
            .nBlockAlign = @sizeOf(f32) * 1,
            .wBitsPerSample = @bitSizeOf(f32),
            .cbSize = 0,
        };

        try hrErr(self.client.IAudioClient_Initialize(
            .SHARED,
            wca.AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM | //should force the mixer to convert for us. Vista and above.
            wca.AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY | //better quality conversion, may add latency?
            wca.AUDCLNT_STREAMFLAGS_EVENTCALLBACK, 
            0,
            0,
            &target_format,
            null,
        ));
        
        try hrErr(self.client.IAudioClient_GetService(
            wca.IID_IAudioRenderClient,
            @ptrCast(*?*anyopaque, &self.render_client),
        ));
        errdefer _ = self.render_client.IUnknown_Release();
        
        self.buffer_event = win32.system.threading.CreateEventExW(
            null,
            null,
            @intToEnum(win32.system.threading.CREATE_EVENT, 0),
            0x1F0003, //2031619 = EVENT_ALL_ACCESS
        ) orelse return error.CreateEventFailed;
        errdefer _ = wf.CloseHandle(self.buffer_event);
        
        try hrErr(self.client.IAudioClient_SetEventHandle(self.buffer_event));
        try hrErr(self.client.IAudioClient_GetBufferSize(&self.buffer_frames));
        
        self.samples = Samples.init();
        
        self.sample_lock = false;
        
        return self;
    }

    //move samples from our buffer to the actual audio buffer
    pub fn writeSamples(self: *@This()) !void {
        var buffer: [*]f32 = undefined;
        var padding_frames: u32 = undefined;

        try hrErr(self.client.IAudioClient_GetCurrentPadding(&padding_frames));
        const available_frames = self.buffer_frames - padding_frames;
        
        if(available_frames == 0) return;
        if(self.samples.count == 0) return;
        
        while (@cmpxchgWeak(bool, &self.sample_lock, false, true, .Acquire, .Monotonic) != null) {}
        defer @atomicStore(bool, &self.sample_lock, false, .Release);
        
        try hrErr(self.render_client.IAudioRenderClient_GetBuffer(available_frames, @ptrCast(*?*u8, &buffer)));
        const count = self.samples.read(buffer[0..available_frames]);
        try hrErr(self.render_client.IAudioRenderClient_ReleaseBuffer(@truncate(u32, count), 0));
    }
    
    //The buffer will tell us when it is hungry for more.
    //@NOTE: In some cases the buffer is still hungry despite being full. SoML.
    pub fn waitBuffer(self: *@This()) void {
        const wait_return_cause = win32.system.threading.WaitForSingleObject(
            self.buffer_event,
            win32.system.windows_programming.INFINITE,
        );
        switch(wait_return_cause) {
            win32.system.threading.WAIT_OBJECT_0 => {},
            win32.system.threading.WAIT_ABANDONED, 
            @enumToInt(win32.foundation.WAIT_TIMEOUT),
            @enumToInt(win32.foundation.WAIT_FAILED) => std.debug.panic("Wait failed: {}\n", .{wait_return_cause}),
            else => std.debug.panic("Unknown wait status: {}\n", .{wait_return_cause}),
        }
    }
    
    pub fn audioThread(self: *@This()) !void {
        while(self.samples.count == 0) {win32.system.threading.Sleep(1);}
        try hrErr(self.client.IAudioClient_Start());
        while(true) {
            self.waitBuffer();
            try self.writeSamples();
        }
    }
    
    //write a single sample to our buffer for later copying to the real one
    pub fn writeSample(self: *@This(), sample: f32) !void {
        while (@cmpxchgWeak(bool, &self.sample_lock, false, true, .Acquire, .Monotonic) != null) {}
        defer @atomicStore(bool, &self.sample_lock, false, .Release);
        try self.samples.writeItem(sample);
    }
    
    //if we start to get ahead of the audio buffer (2 sync's worth)
    // then wait until we're under that.
    pub fn sync(self: *@This(), sync_hz: u32) void {
        const sample_hz = (gigatron_clock_rate / 100);
        const samples_per_sync = sample_hz / sync_hz;

        while(self.samples.count >= (2 * samples_per_sync)) {
            const sleep_time = std.time.ms_per_s / sync_hz;
            win32.system.threading.Sleep(sleep_time); 
        }
    }
    
    pub fn deinit(self: *@This()) void {
        _ = wf.CloseHandle(self.buffer_event);
        _ = self.client.IAudioClient_Stop();
        _ = self.render_client.IUnknown_Release();
        _ = self.client.IUnknown_Release();
        _ = self.device.IUnknown_Release();
        _ = self.device_enumerator.IUnknown_Release();
        _ = win32.media.timeEndPeriod(1);
    }
};



pub fn main() !void {
    var vm: Gigatron.VirtualMachine = undefined;
    var babelfish = Gigatron.BabelFish{};
    var tape = [_]u8{0} ** 512;
    babelfish.init(&tape);
    var vga = Gigatron.VgaMonitor{};
    var audio = Gigatron.Audio.init(gigatron_clock_rate / 100);
    
    //@TODO: Selectable rom
    const current_dir = std.fs.cwd();
    const rom_file = try current_dir.openFile("gigatron.rom", .{ .read = true });
    var reader = rom_file.reader();
    _ = try vm.loadRom(reader);
    rom_file.close();

    vm.start();

    try MainWindow.init(&babelfish.buttons, &babelfish);
    defer main_window.destroy();
    
    sound = try Sound.init();
    defer sound.deinit();
    
    const sound_thread = try std.Thread.spawn(.{}, Sound.audioThread, .{&sound});
    _ = sound_thread;
    //@NOTE: This would cause any thrown error to simply wait forever
    //for a join that never happens
    //defer sound_thread.join();

    //@NOTE: This will glitch on rollover (except in debug, where it'll crash)
    // after about 93k years or so, if running in real time
    var cycle: u64 = 0; 
    while(true) : (cycle += 1) {
        vm.cycle();
        babelfish.cycle(&vm);
        
        const render = vga.cycle(&vm);
        if(render) {
            const leds = Gigatron.BlinkenLights.sample(&vm);
            //@TODO: skip rendering if behind on frames
            try main_window.render(&vga, leds);
        }
        
        //62500hz
        if(cycle % 100 == 0) {
            try sound.writeSample(audio.sample(&vm));
        }

        //100hz
        if(cycle % (gigatron_clock_rate / 100) == 0) {
            main_window.handleMessages();
            sound.sync(100);
        }
    }
}