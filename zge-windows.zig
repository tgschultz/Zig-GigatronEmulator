const std = @import("std");
const builtin = @import("builtin");
const Gigatron = @import("gigatron.zig");

const win32 = @import("win32_all.zig");

//@TODO: Allow adjustable emulation speed

//@TODO: We want to be able to track multiple windows and allow them to use instance fields
// instead of globals. We *could* do https://devblogs.microsoft.com/oldnewthing/20140203-00/?p=1893
// but keep in mind we need to save the hWnd in the CREATE message for that to work.
// Alternatively we can just have a global for each window.
//
// Main window: vga output, blinkenlights
// Debugger: step, watch, disassembly, ram view, rom view, register view, plugface data view

const gigatron_clock_rate = Gigatron.clock_rate;

var main_window: MainWindow = undefined;
var sound: Sound = undefined;

//@TODO: Allow for selectable resolution, handle HiDPI
//Handles main display and blinkenlights
const MainWindow = struct {
    hModule: win32.HINSTANCE,
    wndClass: win32.WNDCLASSEXW,
    hWnd: win32.HWND,
    
    hDC: win32.HDC,
    wglRC: win32.HGLRC,
    
    gl_texture: win32.GLuint,
    frame_buffer: [vid_width * (vid_height + blinken_height)]RGB888,
    
    buttons: *Gigatron.Buttons,
    plugface: *Gigatron.PluggyMcPlugface,

    const vid_width = Gigatron.VgaMonitor.vid_width;
    const vid_height = Gigatron.VgaMonitor.vid_height;
    const blinken_height = 10;
    
    const RGB888 = packed struct {
        r: u8,
        g: u8,
        b: u8,
    };
    
    fn init(b: *Gigatron.Buttons, p: *Gigatron.PluggyMcPlugface) !void {
        const self = &main_window;
        self.buttons = b;
        self.buttons.* = .{
            .a      = 1,
            .b      = 1,
            .select = 1,
            .start  = 1,
            .up     = 1,
            .down   = 1,
            .left   = 1,
            .right  = 1,
        };
        self.plugface = p;
        
        self.hModule = try win32.hErr(win32.GetModuleHandleW(null));
        
        self.wndClass = std.mem.zeroes(win32.WNDCLASSEXW);
        self.wndClass.cbSize = @sizeOf(win32.WNDCLASSEXW);
        self.wndClass.style = win32.CS_OWNDC;
        self.wndClass.lpfnWndProc = winProc;
        self.wndClass.hInstance = @ptrCast(win32.HINSTANCE, self.hModule);
        self.wndClass.hCursor = win32.LoadCursorW(null, win32.IDC_ARROW);
        self.wndClass.hbrBackground = @ptrCast(win32.HBRUSH, win32.GetStockObject(win32.BLACK_BRUSH));
        self.wndClass.lpszClassName = std.unicode.utf8ToUtf16LeStringLiteral("ZGE");

        _ = try win32.intErr(win32.RegisterClassExW(&self.wndClass));
        
        var display_rect = win32.RECT{
            .left   = 0,
            .top    = 0,
            .right  = vid_width,
            .bottom = (vid_height + blinken_height),
        };
        
        //@TODO: use GetSystemMetrics instead of AdjustWindowRectEx because the latter is stupid. Alternatively
        // just create the window and then resize it afterwards by the difference between clientrect and its rect.
        try win32.boolErr(win32.AdjustWindowRectEx(
            &display_rect, 
            win32.WS_CAPTION,
            win32.FALSE,
            @intToEnum(win32.WINDOW_EX_STYLE, 0),
        ));
        
        const window_style = @intToEnum(win32.WINDOW_STYLE,
            @enumToInt(win32.WS_OVERLAPPED)  |
            @enumToInt(win32.WS_MINIMIZEBOX) |
            @enumToInt(win32.WS_SYSMENU)
        );
        
        self.hWnd = try win32.hErr(win32.CreateWindowExW(
            @intToEnum(win32.WINDOW_EX_STYLE, 0),
            self.wndClass.lpszClassName,
            std.unicode.utf8ToUtf16LeStringLiteral("Zig Gigatron Emulator"), //why not Zigatron? Because that's tacky.
            window_style,
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            display_rect.right - display_rect.left,
            display_rect.bottom - display_rect.top,
            null,
            null,
            @ptrCast(win32.HINSTANCE, self.hModule),
            null
        ));
        errdefer _ = win32.DestroyWindow(self.hWnd);

        //We're going to use OpenGL to put a simple frame buffer on the screen because
        // frankly, it's the simplest way.
        self.hDC = try win32.hErr(win32.GetDC(self.hWnd));
        errdefer _ = win32.ReleaseDC(self.hWnd, self.hDC);
        var pfd = std.mem.zeroes(win32.PIXELFORMATDESCRIPTOR);
        pfd.nSize = @sizeOf(win32.PIXELFORMATDESCRIPTOR);
        pfd.nVersion = 1;
        pfd.dwFlags = 
            win32.PFD_DRAW_TO_WINDOW
            | win32.PFD_SUPPORT_OPENGL
            | win32.PFD_DOUBLEBUFFER
            | win32.PFD_TYPE_RGBA
        ;
        pfd.cColorBits = 24;
        pfd.cDepthBits = 0;
        pfd.iLayerType = win32.PFD_MAIN_PLANE;
        
        const pfi = try win32.intErr(win32.ChoosePixelFormat(self.hDC, &pfd));
        try win32.boolErr(win32.SetPixelFormat(self.hDC, pfi, &pfd));
        
        self.wglRC = try win32.hErr(win32.wglCreateContext(self.hDC));
        errdefer _ = win32.wglDeleteContext(self.wglRC);

        try win32.boolErr(win32.wglMakeCurrent(self.hDC, self.wglRC));

        try win32.glErr(win32.glGenTextures(1, &self.gl_texture));
        
        //return indicates if window is already visible or not, we don't care.
        _ = win32.ShowWindow(self.hWnd, win32.SW_SHOW); 
        
        //This disables vsync, assuming the driver isn't forcing it
        // if we get forced to vsync then missed frames are likely.
        // We should probably do something about that, like run it in
        // the display copy in a different thread.
        const wglSwapIntervalEXT_proc_addr = try win32.hErr(win32.wglGetProcAddress("wglSwapIntervalEXT"));
        const wglSwapIntervalEXT = @intToPtr(
            win32.PFNWGLSWAPINTERVALEXTPROC,
            @ptrToInt(wglSwapIntervalEXT_proc_addr)
        ).?;
        try win32.boolErr(wglSwapIntervalEXT(0));
    }
    
    //Ye olde WNDPROC
    pub fn winProc(
        hWnd: win32.HWND,
        uMsg: c_uint,
        wParam: win32.WPARAM,
        lParam: win32.LPARAM
    ) callconv(win32.WINAPI) win32.LRESULT {
        const self = &main_window;
    
        //@TODO: actual gamepads, configurable controls
        switch(uMsg) {
            win32.WM_CLOSE => win32.ExitProcess(1),

            win32.WM_SYSKEYDOWN,
            win32.WM_KEYDOWN => {
                const key_param = @intToEnum(win32.VIRTUAL_KEY, wParam);
                switch(key_param) {
                    win32.VK_UP     => self.buttons.up     = 0,
                    win32.VK_DOWN   => self.buttons.down   = 0,
                    win32.VK_LEFT   => self.buttons.left   = 0,
                    win32.VK_RIGHT  => self.buttons.right  = 0,
                    win32.VK_NEXT   => self.buttons.select = 0, //pg down
                    win32.VK_PRIOR  => self.buttons.start  = 0, //pg up
                    win32.VK_HOME,
                    win32.VK_INSERT => self.buttons.b      = 0, //home or insert
                    win32.VK_END,
                    win32.VK_DELETE,
                    win32.VK_BACK   => self.buttons.a      = 0, //end, del, bksp
                    
                    //@TODO: this *should* be control+f3
                    win32.VK_F3     => self.plugface.controlKeyPress(.load),
                    else => return win32.DefWindowProcW(hWnd, uMsg, wParam, lParam),
                }
            },
            win32.WM_SYSKEYUP,
            win32.WM_KEYUP => {
                const key_param = @intToEnum(win32.VIRTUAL_KEY, wParam);
                switch(key_param) {
                    win32.VK_UP     => self.buttons.up     = 1,
                    win32.VK_DOWN   => self.buttons.down   = 1,
                    win32.VK_LEFT   => self.buttons.left   = 1,
                    win32.VK_RIGHT  => self.buttons.right  = 1,
                    win32.VK_NEXT   => self.buttons.select = 1, //pg down
                    win32.VK_PRIOR  => self.buttons.start  = 1, //pg up
                    win32.VK_HOME,
                    win32.VK_INSERT => self.buttons.b      = 1, //home or insert
                    win32.VK_END,
                    win32.VK_DELETE,
                    win32.VK_BACK   => self.buttons.a      = 1, //end, del, bksp
                    
                    else => return win32.DefWindowProcW(hWnd, uMsg, wParam, lParam),
                }
            },
            win32.WM_CHAR => {
                if(wParam >=0 and wParam <= 127) {
                    //lparam:
                    //0-15 repeat count, 16-23 scan code, 24 extended key, 25-28 reserved, 29 alt, 30 prev state, 31 transition
                    //seems like it is never sent for UP transitions, but that's ok because the plugface doesn't seem to 
                    //care anyway
                    const ascii = @truncate(u8, wParam);
                    
                    const key = if(ascii == '\r') '\n' else ascii; //return generates cr, but plugface treats it as lf
                    self.plugface.asciiKeyPress(key);
                } else return win32.DefWindowProcW(hWnd, uMsg, wParam, lParam);
            },
            
            else => return win32.DefWindowProcW(hWnd, uMsg, wParam, lParam),
        }
        
        return 0;
    }
    
    fn handleMessages(self: *@This()) void {
        var message: win32.MSG = undefined;
        while(win32.PeekMessageW(&message, self.hWnd, 0, 0, win32.PM_REMOVE) != win32.FALSE) {
            _ = win32.TranslateMessage(&message);
            _ = win32.DispatchMessageW(&message);
        }
    }
    
    fn destroy(self: *@This()) void {
        _ = win32.wglMakeCurrent(null, null);
        _ = win32.wglDeleteContext(self.wglRC);
        _ = win32.ReleaseDC(self.hWnd, self.hDC);
        _ = win32.DestroyWindow(self.hWnd);
    }
    
    fn render(self: *@This(), vga: *Gigatron.VgaMonitor, bl: *Gigatron.BlinkenLights) !void {
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
                start_0...start_0 + (width - 1) => if(bl.leds[0]) red else black,
                start_1...start_1 + (width - 1) => if(bl.leds[1]) red else black,
                start_2...start_2 + (width - 1) => if(bl.leds[2]) red else black,
                start_3...start_3 + (width - 1) => if(bl.leds[3]) red else black,
                else => grey,
            };
        }
        
        try win32.glErr({
            win32.glEnable(win32.GL_TEXTURE_2D);
            win32.glBindTexture(win32.GL_TEXTURE_2D, self.gl_texture);
            win32.glTexParameteri(win32.GL_TEXTURE_2D, win32.GL_TEXTURE_MAG_FILTER, win32.GL_NEAREST);
            win32.glTexParameteri(win32.GL_TEXTURE_2D, win32.GL_TEXTURE_MIN_FILTER, win32.GL_NEAREST);
            win32.glTexImage2D(
                win32.GL_TEXTURE_2D,
                0,
                win32.GL_RGB,
                vid_width,
                vid_height + blinken_height,
                0,
                win32.GL_RGB,
                win32.GL_UNSIGNED_BYTE,
                &self.frame_buffer,
            );
        });
        
        try win32.glErr({
            win32.glBegin(win32.GL_QUADS);
                win32.glTexCoord2f(0.0, 0.0); 
                win32.glVertex2f(-1.0, 1.0);
                win32.glTexCoord2f(1.0, 0.0); 
                win32.glVertex2f(1.0, 1.0);
                win32.glTexCoord2f(1.0, 1.0); 
                win32.glVertex2f(1.0, -1.0);
                win32.glTexCoord2f(0.0, 1.0); 
                win32.glVertex2f(-1.0, -1.0);
            win32.glEnd();
            
            win32.glDisable(win32.GL_TEXTURE_2D);
        });
        
        try win32.boolErr(win32.SwapBuffers(self.hDC));
    }
};

//After some experimentation with different syncing strategies, I settled on
// using the size of the audio sample buffer to determine the passage of
// time. This was weirdly consistent compared to the alternatives.
pub const Sound = struct {
    device_enumerator: *win32.IMMDeviceEnumerator,
    device: *win32.IMMDevice,
    client: *win32.IAudioClient,
    render_client: *win32.IAudioRenderClient,
    samples: Samples,
    sample_lock: bool,
    buffer_frames: c_uint,
    buffer_event: win32.HANDLE,
    
    const Samples = std.fifo.LinearFifo(f32, .{.Static = 2048});
    
    pub fn init() !@This() {
        var self: @This() = undefined; //someday we can use explicit result location
        
        //tell windows we want a 1ms scheduler granularity instead of the
        // 15ms eternity it uses by defualt. We do it here because we use
        // the sound buffer for timing now.
        //ignore any errors, we can't do anything about them anyway
        _ = win32.timeBeginPeriod(1);
        
        try win32.hrErr(win32.CoCreateInstance(
            win32.CLSID_MMDeviceEnumerator,
            null,
            win32.CLSCTX_INPROC_SERVER,
            win32.IID_IMMDeviceEnumerator,
            @ptrCast(*?*c_void, &self.device_enumerator),
        ));
        errdefer _ = self.device_enumerator.IUnknown_Release();
        
        try win32.hrErr(self.device_enumerator.IMMDeviceEnumerator_GetDefaultAudioEndpoint(
            .eRender,
            .eConsole,
            @ptrCast(*?@TypeOf(self.device), &self.device),
        ));
        errdefer _ = self.device.IUnknown_Release();

        try win32.hrErr(self.device.IMMDevice_Activate(
            win32.IID_IAudioClient,
            @enumToInt(win32.CLSCTX_INPROC_SERVER),
            null,
            @ptrCast(*?*c_void, &self.client),
        ));
        errdefer _ = self.client.IUnknown_Release();
        
        //We can force the driver to handle sample conversion, so for
        // simplicity we set our sample rate to an even division of
        // the gigatron clock rate. Float is used because why not.
        const target_format = win32.WAVEFORMATEX{
            .wFormatTag = win32.WAVE_FORMAT_IEEE_FLOAT,
            .nChannels = 1,
            .nSamplesPerSec = gigatron_clock_rate / 100, 
            .nAvgBytesPerSec = (gigatron_clock_rate / 100) * @sizeOf(f32) * 1,
            .nBlockAlign = @sizeOf(f32) * 1,
            .wBitsPerSample = @bitSizeOf(f32),
            .cbSize = 0,
        };

        try win32.hrErr(self.client.IAudioClient_Initialize(
            .SHARED,
            win32.AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM | //should force the mixer to convert for us. Vista and above.
            win32.AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY | //better quality conversion, may add latency?
            win32.AUDCLNT_STREAMFLAGS_EVENTCALLBACK, 
            0,
            0,
            &target_format,
            null,
        ));
        
        try win32.hrErr(self.client.IAudioClient_GetService(
            win32.IID_IAudioRenderClient,
            @ptrCast(*?*c_void, &self.render_client),
        ));
        errdefer _ = self.render_client.IUnknown_Release();
        
        self.buffer_event = try win32.hErr(win32.CreateEventExW(
            null,
            null,
            @intToEnum(win32.CREATE_EVENT, 0),
            win32.EVENT_ALL_ACCESS,
        ));
        errdefer _ = win32.CloseHandle(self.buffer_event);
        
        try win32.hrErr(self.client.IAudioClient_SetEventHandle(self.buffer_event));
        try win32.hrErr(self.client.IAudioClient_GetBufferSize(&self.buffer_frames));
        
        self.samples = Samples.init();
        
        self.sample_lock = false;
        
        return self;
    }

    //move samples from our buffer to the actual audio buffer
    pub fn writeSamples(self: *@This()) !void {
        var buffer: [*]f32 = undefined;
        var padding_frames: u32 = undefined;

        try win32.hrErr(self.client.IAudioClient_GetCurrentPadding(&padding_frames));
        const available_frames = self.buffer_frames - padding_frames;
        
        if(available_frames == 0) return;
        if(self.samples.count == 0) return;
        
        while (@cmpxchgWeak(bool, &self.sample_lock, false, true, .Acquire, .Monotonic) != null) {}
        defer @atomicStore(bool, &self.sample_lock, false, .Release);
        
        try win32.hrErr(self.render_client.IAudioRenderClient_GetBuffer(available_frames, @ptrCast(*?*u8, &buffer)));
        const count = self.samples.read(buffer[0..available_frames]);
        try win32.hrErr(self.render_client.IAudioRenderClient_ReleaseBuffer(@truncate(u32, count), 0));
    }
    
    //The buffer will tell us when it is hungry for more.
    //@NOTE: In some cases the buffer is still hungry despite being full. SoML.
    pub fn waitBuffer(self: *@This()) void {
        const wait_return_cause = win32.WaitForSingleObject(self.buffer_event, win32.INFINITE);
        switch(wait_return_cause) {
            .OBJECT_0 => {},
            .ABANDONED, .TIMEOUT, .FAILED => std.debug.panic("Wait failed: {}\n", .{wait_return_cause}),
            else => std.debug.panic("Unknown wait status: {}\n", .{wait_return_cause}),
        }
    }
    
    pub fn audioThread(self: *@This()) !void {
        while(self.samples.count == 0) {win32.Sleep(1);}
        try win32.hrErr(self.client.IAudioClient_Start());
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
            win32.Sleep(sleep_time); 
        }
    }
    
    pub fn deinit(self: *@This()) void {
        _ = win32.CloseHandle(self.buffer_event);
        _ = self.client.IAudioClient_Stop();
        _ = self.render_client.IUnknown_Release();
        _ = self.client.IUnknown_Release();
        _ = self.device.IUnknown_Release();
        _ = self.device_enumerator.IUnknown_Release();
        _ = win32.timeEndPeriod(1);
    }
};



pub fn main() !void {
    var vm: Gigatron.VirtualMachine = undefined;
    var plugface: Gigatron.PluggyMcPlugface = undefined; 
    var blinken: Gigatron.BlinkenLights = undefined;
    var vga: Gigatron.VgaMonitor = undefined;
    var audio = Gigatron.Audio.init(gigatron_clock_rate / 100);
    //audio.volume = 0.1;
    
    //@TODO: Selectable rom
    const current_dir = std.fs.cwd();
    const rom_file = try current_dir.openFile("ROMv5a.rom", .{ .read = true });
    var reader = rom_file.reader();
    _ = try vm.loadRom(reader);
    rom_file.close();

    vm.start();

    try MainWindow.init(&plugface.buttons, &plugface);
    defer main_window.destroy();
    
    sound = try Sound.init();
    defer sound.deinit();
    
    const sound_thread = try std.Thread.spawn(.{}, Sound.audioThread, .{&sound});
    _ = sound_thread;
    //@NOTE: This would cause any thrown error to simply wait forever
    //for a join that never happens
    //defer sound_thread.join();

    plugface.init();

    //@TODO: This will glitch on rollover (except in debug, where it'll crash)
    // after about 93k years or so of real time
    var cycle: u64 = 0; 
    while(true) : (cycle += 1) {
        vm.cycle();
        plugface.cycle(&vm);
        blinken.cycle(&vm);
        
        const render = vga.cycle(&vm);
        if(render) {
            //@TODO: skip rendering if behind on frames
            try main_window.render(&vga, &blinken);
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