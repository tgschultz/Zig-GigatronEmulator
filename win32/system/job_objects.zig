//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (0)
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Section: Types (4)
//--------------------------------------------------------------------------------
pub const JOB_OBJECT_LIMIT = enum(u32) {
    LIMIT_WORKINGSET = 1,
    LIMIT_PROCESS_TIME = 2,
    LIMIT_JOB_TIME = 4,
    LIMIT_ACTIVE_PROCESS = 8,
    LIMIT_AFFINITY = 16,
    LIMIT_PRIORITY_CLASS = 32,
    LIMIT_PRESERVE_JOB_TIME = 64,
    LIMIT_SCHEDULING_CLASS = 128,
    LIMIT_PROCESS_MEMORY = 256,
    LIMIT_JOB_MEMORY = 512,
    // LIMIT_JOB_MEMORY_HIGH = 512, this enum value conflicts with LIMIT_JOB_MEMORY
    LIMIT_DIE_ON_UNHANDLED_EXCEPTION = 1024,
    LIMIT_BREAKAWAY_OK = 2048,
    LIMIT_SILENT_BREAKAWAY_OK = 4096,
    LIMIT_KILL_ON_JOB_CLOSE = 8192,
    LIMIT_SUBSET_AFFINITY = 16384,
    LIMIT_JOB_MEMORY_LOW = 32768,
    LIMIT_JOB_READ_BYTES = 65536,
    LIMIT_JOB_WRITE_BYTES = 131072,
    LIMIT_RATE_CONTROL = 262144,
    // LIMIT_CPU_RATE_CONTROL = 262144, this enum value conflicts with LIMIT_RATE_CONTROL
    LIMIT_IO_RATE_CONTROL = 524288,
    LIMIT_NET_RATE_CONTROL = 1048576,
    LIMIT_VALID_FLAGS = 524287,
    BASIC_LIMIT_VALID_FLAGS = 255,
    EXTENDED_LIMIT_VALID_FLAGS = 32767,
    NOTIFICATION_LIMIT_VALID_FLAGS = 2064900,
    _,
    pub fn initFlags(o: struct {
        LIMIT_WORKINGSET: u1 = 0,
        LIMIT_PROCESS_TIME: u1 = 0,
        LIMIT_JOB_TIME: u1 = 0,
        LIMIT_ACTIVE_PROCESS: u1 = 0,
        LIMIT_AFFINITY: u1 = 0,
        LIMIT_PRIORITY_CLASS: u1 = 0,
        LIMIT_PRESERVE_JOB_TIME: u1 = 0,
        LIMIT_SCHEDULING_CLASS: u1 = 0,
        LIMIT_PROCESS_MEMORY: u1 = 0,
        LIMIT_JOB_MEMORY: u1 = 0,
        LIMIT_DIE_ON_UNHANDLED_EXCEPTION: u1 = 0,
        LIMIT_BREAKAWAY_OK: u1 = 0,
        LIMIT_SILENT_BREAKAWAY_OK: u1 = 0,
        LIMIT_KILL_ON_JOB_CLOSE: u1 = 0,
        LIMIT_SUBSET_AFFINITY: u1 = 0,
        LIMIT_JOB_MEMORY_LOW: u1 = 0,
        LIMIT_JOB_READ_BYTES: u1 = 0,
        LIMIT_JOB_WRITE_BYTES: u1 = 0,
        LIMIT_RATE_CONTROL: u1 = 0,
        LIMIT_IO_RATE_CONTROL: u1 = 0,
        LIMIT_NET_RATE_CONTROL: u1 = 0,
        LIMIT_VALID_FLAGS: u1 = 0,
        BASIC_LIMIT_VALID_FLAGS: u1 = 0,
        EXTENDED_LIMIT_VALID_FLAGS: u1 = 0,
        NOTIFICATION_LIMIT_VALID_FLAGS: u1 = 0,
    }) JOB_OBJECT_LIMIT {
        return @intToEnum(JOB_OBJECT_LIMIT,
              (if (o.LIMIT_WORKINGSET == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_WORKINGSET) else 0)
            | (if (o.LIMIT_PROCESS_TIME == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_PROCESS_TIME) else 0)
            | (if (o.LIMIT_JOB_TIME == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_JOB_TIME) else 0)
            | (if (o.LIMIT_ACTIVE_PROCESS == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_ACTIVE_PROCESS) else 0)
            | (if (o.LIMIT_AFFINITY == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_AFFINITY) else 0)
            | (if (o.LIMIT_PRIORITY_CLASS == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_PRIORITY_CLASS) else 0)
            | (if (o.LIMIT_PRESERVE_JOB_TIME == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_PRESERVE_JOB_TIME) else 0)
            | (if (o.LIMIT_SCHEDULING_CLASS == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_SCHEDULING_CLASS) else 0)
            | (if (o.LIMIT_PROCESS_MEMORY == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_PROCESS_MEMORY) else 0)
            | (if (o.LIMIT_JOB_MEMORY == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_JOB_MEMORY) else 0)
            | (if (o.LIMIT_DIE_ON_UNHANDLED_EXCEPTION == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_DIE_ON_UNHANDLED_EXCEPTION) else 0)
            | (if (o.LIMIT_BREAKAWAY_OK == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_BREAKAWAY_OK) else 0)
            | (if (o.LIMIT_SILENT_BREAKAWAY_OK == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_SILENT_BREAKAWAY_OK) else 0)
            | (if (o.LIMIT_KILL_ON_JOB_CLOSE == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_KILL_ON_JOB_CLOSE) else 0)
            | (if (o.LIMIT_SUBSET_AFFINITY == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_SUBSET_AFFINITY) else 0)
            | (if (o.LIMIT_JOB_MEMORY_LOW == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_JOB_MEMORY_LOW) else 0)
            | (if (o.LIMIT_JOB_READ_BYTES == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_JOB_READ_BYTES) else 0)
            | (if (o.LIMIT_JOB_WRITE_BYTES == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_JOB_WRITE_BYTES) else 0)
            | (if (o.LIMIT_RATE_CONTROL == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_RATE_CONTROL) else 0)
            | (if (o.LIMIT_IO_RATE_CONTROL == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_IO_RATE_CONTROL) else 0)
            | (if (o.LIMIT_NET_RATE_CONTROL == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_NET_RATE_CONTROL) else 0)
            | (if (o.LIMIT_VALID_FLAGS == 1) @enumToInt(JOB_OBJECT_LIMIT.LIMIT_VALID_FLAGS) else 0)
            | (if (o.BASIC_LIMIT_VALID_FLAGS == 1) @enumToInt(JOB_OBJECT_LIMIT.BASIC_LIMIT_VALID_FLAGS) else 0)
            | (if (o.EXTENDED_LIMIT_VALID_FLAGS == 1) @enumToInt(JOB_OBJECT_LIMIT.EXTENDED_LIMIT_VALID_FLAGS) else 0)
            | (if (o.NOTIFICATION_LIMIT_VALID_FLAGS == 1) @enumToInt(JOB_OBJECT_LIMIT.NOTIFICATION_LIMIT_VALID_FLAGS) else 0)
        );
    }
};
pub const JOB_OBJECT_LIMIT_WORKINGSET = JOB_OBJECT_LIMIT.LIMIT_WORKINGSET;
pub const JOB_OBJECT_LIMIT_PROCESS_TIME = JOB_OBJECT_LIMIT.LIMIT_PROCESS_TIME;
pub const JOB_OBJECT_LIMIT_JOB_TIME = JOB_OBJECT_LIMIT.LIMIT_JOB_TIME;
pub const JOB_OBJECT_LIMIT_ACTIVE_PROCESS = JOB_OBJECT_LIMIT.LIMIT_ACTIVE_PROCESS;
pub const JOB_OBJECT_LIMIT_AFFINITY = JOB_OBJECT_LIMIT.LIMIT_AFFINITY;
pub const JOB_OBJECT_LIMIT_PRIORITY_CLASS = JOB_OBJECT_LIMIT.LIMIT_PRIORITY_CLASS;
pub const JOB_OBJECT_LIMIT_PRESERVE_JOB_TIME = JOB_OBJECT_LIMIT.LIMIT_PRESERVE_JOB_TIME;
pub const JOB_OBJECT_LIMIT_SCHEDULING_CLASS = JOB_OBJECT_LIMIT.LIMIT_SCHEDULING_CLASS;
pub const JOB_OBJECT_LIMIT_PROCESS_MEMORY = JOB_OBJECT_LIMIT.LIMIT_PROCESS_MEMORY;
pub const JOB_OBJECT_LIMIT_JOB_MEMORY = JOB_OBJECT_LIMIT.LIMIT_JOB_MEMORY;
pub const JOB_OBJECT_LIMIT_JOB_MEMORY_HIGH = JOB_OBJECT_LIMIT.LIMIT_JOB_MEMORY;
pub const JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION = JOB_OBJECT_LIMIT.LIMIT_DIE_ON_UNHANDLED_EXCEPTION;
pub const JOB_OBJECT_LIMIT_BREAKAWAY_OK = JOB_OBJECT_LIMIT.LIMIT_BREAKAWAY_OK;
pub const JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK = JOB_OBJECT_LIMIT.LIMIT_SILENT_BREAKAWAY_OK;
pub const JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = JOB_OBJECT_LIMIT.LIMIT_KILL_ON_JOB_CLOSE;
pub const JOB_OBJECT_LIMIT_SUBSET_AFFINITY = JOB_OBJECT_LIMIT.LIMIT_SUBSET_AFFINITY;
pub const JOB_OBJECT_LIMIT_JOB_MEMORY_LOW = JOB_OBJECT_LIMIT.LIMIT_JOB_MEMORY_LOW;
pub const JOB_OBJECT_LIMIT_JOB_READ_BYTES = JOB_OBJECT_LIMIT.LIMIT_JOB_READ_BYTES;
pub const JOB_OBJECT_LIMIT_JOB_WRITE_BYTES = JOB_OBJECT_LIMIT.LIMIT_JOB_WRITE_BYTES;
pub const JOB_OBJECT_LIMIT_RATE_CONTROL = JOB_OBJECT_LIMIT.LIMIT_RATE_CONTROL;
pub const JOB_OBJECT_LIMIT_CPU_RATE_CONTROL = JOB_OBJECT_LIMIT.LIMIT_RATE_CONTROL;
pub const JOB_OBJECT_LIMIT_IO_RATE_CONTROL = JOB_OBJECT_LIMIT.LIMIT_IO_RATE_CONTROL;
pub const JOB_OBJECT_LIMIT_NET_RATE_CONTROL = JOB_OBJECT_LIMIT.LIMIT_NET_RATE_CONTROL;
pub const JOB_OBJECT_LIMIT_VALID_FLAGS = JOB_OBJECT_LIMIT.LIMIT_VALID_FLAGS;
pub const JOB_OBJECT_BASIC_LIMIT_VALID_FLAGS = JOB_OBJECT_LIMIT.BASIC_LIMIT_VALID_FLAGS;
pub const JOB_OBJECT_EXTENDED_LIMIT_VALID_FLAGS = JOB_OBJECT_LIMIT.EXTENDED_LIMIT_VALID_FLAGS;
pub const JOB_OBJECT_NOTIFICATION_LIMIT_VALID_FLAGS = JOB_OBJECT_LIMIT.NOTIFICATION_LIMIT_VALID_FLAGS;

pub const JOB_OBJECT_UILIMIT = enum(u32) {
    NONE = 0,
    HANDLES = 1,
    READCLIPBOARD = 2,
    WRITECLIPBOARD = 4,
    SYSTEMPARAMETERS = 8,
    DISPLAYSETTINGS = 16,
    GLOBALATOMS = 32,
    DESKTOP = 64,
    EXITWINDOWS = 128,
    _,
    pub fn initFlags(o: struct {
        NONE: u1 = 0,
        HANDLES: u1 = 0,
        READCLIPBOARD: u1 = 0,
        WRITECLIPBOARD: u1 = 0,
        SYSTEMPARAMETERS: u1 = 0,
        DISPLAYSETTINGS: u1 = 0,
        GLOBALATOMS: u1 = 0,
        DESKTOP: u1 = 0,
        EXITWINDOWS: u1 = 0,
    }) JOB_OBJECT_UILIMIT {
        return @intToEnum(JOB_OBJECT_UILIMIT,
              (if (o.NONE == 1) @enumToInt(JOB_OBJECT_UILIMIT.NONE) else 0)
            | (if (o.HANDLES == 1) @enumToInt(JOB_OBJECT_UILIMIT.HANDLES) else 0)
            | (if (o.READCLIPBOARD == 1) @enumToInt(JOB_OBJECT_UILIMIT.READCLIPBOARD) else 0)
            | (if (o.WRITECLIPBOARD == 1) @enumToInt(JOB_OBJECT_UILIMIT.WRITECLIPBOARD) else 0)
            | (if (o.SYSTEMPARAMETERS == 1) @enumToInt(JOB_OBJECT_UILIMIT.SYSTEMPARAMETERS) else 0)
            | (if (o.DISPLAYSETTINGS == 1) @enumToInt(JOB_OBJECT_UILIMIT.DISPLAYSETTINGS) else 0)
            | (if (o.GLOBALATOMS == 1) @enumToInt(JOB_OBJECT_UILIMIT.GLOBALATOMS) else 0)
            | (if (o.DESKTOP == 1) @enumToInt(JOB_OBJECT_UILIMIT.DESKTOP) else 0)
            | (if (o.EXITWINDOWS == 1) @enumToInt(JOB_OBJECT_UILIMIT.EXITWINDOWS) else 0)
        );
    }
};
pub const JOB_OBJECT_UILIMIT_NONE = JOB_OBJECT_UILIMIT.NONE;
pub const JOB_OBJECT_UILIMIT_HANDLES = JOB_OBJECT_UILIMIT.HANDLES;
pub const JOB_OBJECT_UILIMIT_READCLIPBOARD = JOB_OBJECT_UILIMIT.READCLIPBOARD;
pub const JOB_OBJECT_UILIMIT_WRITECLIPBOARD = JOB_OBJECT_UILIMIT.WRITECLIPBOARD;
pub const JOB_OBJECT_UILIMIT_SYSTEMPARAMETERS = JOB_OBJECT_UILIMIT.SYSTEMPARAMETERS;
pub const JOB_OBJECT_UILIMIT_DISPLAYSETTINGS = JOB_OBJECT_UILIMIT.DISPLAYSETTINGS;
pub const JOB_OBJECT_UILIMIT_GLOBALATOMS = JOB_OBJECT_UILIMIT.GLOBALATOMS;
pub const JOB_OBJECT_UILIMIT_DESKTOP = JOB_OBJECT_UILIMIT.DESKTOP;
pub const JOB_OBJECT_UILIMIT_EXITWINDOWS = JOB_OBJECT_UILIMIT.EXITWINDOWS;

pub const JOB_OBJECT_SECURITY = enum(u32) {
    NO_ADMIN = 1,
    RESTRICTED_TOKEN = 2,
    ONLY_TOKEN = 4,
    FILTER_TOKENS = 8,
    VALID_FLAGS = 15,
    _,
    pub fn initFlags(o: struct {
        NO_ADMIN: u1 = 0,
        RESTRICTED_TOKEN: u1 = 0,
        ONLY_TOKEN: u1 = 0,
        FILTER_TOKENS: u1 = 0,
        VALID_FLAGS: u1 = 0,
    }) JOB_OBJECT_SECURITY {
        return @intToEnum(JOB_OBJECT_SECURITY,
              (if (o.NO_ADMIN == 1) @enumToInt(JOB_OBJECT_SECURITY.NO_ADMIN) else 0)
            | (if (o.RESTRICTED_TOKEN == 1) @enumToInt(JOB_OBJECT_SECURITY.RESTRICTED_TOKEN) else 0)
            | (if (o.ONLY_TOKEN == 1) @enumToInt(JOB_OBJECT_SECURITY.ONLY_TOKEN) else 0)
            | (if (o.FILTER_TOKENS == 1) @enumToInt(JOB_OBJECT_SECURITY.FILTER_TOKENS) else 0)
            | (if (o.VALID_FLAGS == 1) @enumToInt(JOB_OBJECT_SECURITY.VALID_FLAGS) else 0)
        );
    }
};
pub const JOB_OBJECT_SECURITY_NO_ADMIN = JOB_OBJECT_SECURITY.NO_ADMIN;
pub const JOB_OBJECT_SECURITY_RESTRICTED_TOKEN = JOB_OBJECT_SECURITY.RESTRICTED_TOKEN;
pub const JOB_OBJECT_SECURITY_ONLY_TOKEN = JOB_OBJECT_SECURITY.ONLY_TOKEN;
pub const JOB_OBJECT_SECURITY_FILTER_TOKENS = JOB_OBJECT_SECURITY.FILTER_TOKENS;
pub const JOB_OBJECT_SECURITY_VALID_FLAGS = JOB_OBJECT_SECURITY.VALID_FLAGS;

pub const JOBOBJECT_IO_RATE_CONTROL_INFORMATION = extern struct {
    MaxIops: i64,
    MaxBandwidth: i64,
    ReservationIops: i64,
    VolumeName: ?[*:0]const u16,
    BaseIoSize: u32,
    ControlFlags: JOB_OBJECT_IO_RATE_CONTROL_FLAGS,
};


//--------------------------------------------------------------------------------
// Section: Functions (13)
//--------------------------------------------------------------------------------
// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn IsProcessInJob(
    ProcessHandle: ?HANDLE,
    JobHandle: ?HANDLE,
    Result: ?*BOOL,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn CreateJobObjectW(
    lpJobAttributes: ?*SECURITY_ATTRIBUTES,
    lpName: ?[*:0]const u16,
) callconv(@import("std").os.windows.WINAPI) ?HANDLE;

// TODO: this type is limited to platform 'windows10.0.10240'
pub extern "KERNEL32" fn FreeMemoryJobObject(
    Buffer: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) void;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn OpenJobObjectW(
    dwDesiredAccess: u32,
    bInheritHandle: BOOL,
    lpName: ?[*:0]const u16,
) callconv(@import("std").os.windows.WINAPI) ?HANDLE;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn AssignProcessToJobObject(
    hJob: ?HANDLE,
    hProcess: ?HANDLE,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn TerminateJobObject(
    hJob: ?HANDLE,
    uExitCode: u32,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn SetInformationJobObject(
    hJob: ?HANDLE,
    JobObjectInformationClass: JOBOBJECTINFOCLASS,
    // TODO: what to do with BytesParamIndex 3?
    lpJobObjectInformation: ?*c_void,
    cbJobObjectInformationLength: u32,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows10.0.10240'
pub extern "KERNEL32" fn SetIoRateControlInformationJobObject(
    hJob: ?HANDLE,
    IoRateControlInfo: ?*JOBOBJECT_IO_RATE_CONTROL_INFORMATION,
) callconv(@import("std").os.windows.WINAPI) u32;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn QueryInformationJobObject(
    hJob: ?HANDLE,
    JobObjectInformationClass: JOBOBJECTINFOCLASS,
    // TODO: what to do with BytesParamIndex 3?
    lpJobObjectInformation: ?*c_void,
    cbJobObjectInformationLength: u32,
    lpReturnLength: ?*u32,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows10.0.10240'
pub extern "KERNEL32" fn QueryIoRateControlInformationJobObject(
    hJob: ?HANDLE,
    VolumeName: ?[*:0]const u16,
    InfoBlocks: ?*?*JOBOBJECT_IO_RATE_CONTROL_INFORMATION,
    InfoBlockCount: ?*u32,
) callconv(@import("std").os.windows.WINAPI) u32;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "USER32" fn UserHandleGrantAccess(
    hUserHandle: ?HANDLE,
    hJob: ?HANDLE,
    bGrant: BOOL,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn CreateJobObjectA(
    lpJobAttributes: ?*SECURITY_ATTRIBUTES,
    lpName: ?[*:0]const u8,
) callconv(@import("std").os.windows.WINAPI) ?HANDLE;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "KERNEL32" fn OpenJobObjectA(
    dwDesiredAccess: u32,
    bInheritHandle: BOOL,
    lpName: ?[*:0]const u8,
) callconv(@import("std").os.windows.WINAPI) ?HANDLE;


//--------------------------------------------------------------------------------
// Section: Unicode Aliases (2)
//--------------------------------------------------------------------------------
const thismodule = @This();
pub usingnamespace switch (@import("../zig.zig").unicode_mode) {
    .ansi => struct {
        pub const CreateJobObject = thismodule.CreateJobObjectA;
        pub const OpenJobObject = thismodule.OpenJobObjectA;
    },
    .wide => struct {
        pub const CreateJobObject = thismodule.CreateJobObjectW;
        pub const OpenJobObject = thismodule.OpenJobObjectW;
    },
    .unspecified => if (@import("builtin").is_test) struct {
        pub const CreateJobObject = *opaque{};
        pub const OpenJobObject = *opaque{};
    } else struct {
        pub const CreateJobObject = @compileError("'CreateJobObject' requires that UNICODE be set to true or false in the root module");
        pub const OpenJobObject = @compileError("'OpenJobObject' requires that UNICODE be set to true or false in the root module");
    },
};
//--------------------------------------------------------------------------------
// Section: Imports (7)
//--------------------------------------------------------------------------------
const BOOL = @import("../foundation.zig").BOOL;
const HANDLE = @import("../foundation.zig").HANDLE;
const JOB_OBJECT_IO_RATE_CONTROL_FLAGS = @import("../system/system_services.zig").JOB_OBJECT_IO_RATE_CONTROL_FLAGS;
const JOBOBJECTINFOCLASS = @import("../system/system_services.zig").JOBOBJECTINFOCLASS;
const PSTR = @import("../foundation.zig").PSTR;
const PWSTR = @import("../foundation.zig").PWSTR;
const SECURITY_ATTRIBUTES = @import("../security.zig").SECURITY_ATTRIBUTES;

test {
    @setEvalBranchQuota(
        @import("std").meta.declarations(@This()).len * 3
    );

    // reference all the pub declarations
    if (!@import("builtin").is_test) return;
    inline for (@import("std").meta.declarations(@This())) |decl| {
        if (decl.is_pub) {
            _ = decl;
        }
    }
}
