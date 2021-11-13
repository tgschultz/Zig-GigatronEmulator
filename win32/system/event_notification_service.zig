//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (4)
//--------------------------------------------------------------------------------
pub const NETWORK_ALIVE_LAN = @as(u32, 1);
pub const NETWORK_ALIVE_WAN = @as(u32, 2);
pub const NETWORK_ALIVE_AOL = @as(u32, 4);
pub const NETWORK_ALIVE_INTERNET = @as(u32, 8);

//--------------------------------------------------------------------------------
// Section: Types (8)
//--------------------------------------------------------------------------------
pub const SENS_CONNECTION_TYPE = enum(u32) {
    LAN = 0,
    WAN = 1,
};
pub const CONNECTION_LAN = SENS_CONNECTION_TYPE.LAN;
pub const CONNECTION_WAN = SENS_CONNECTION_TYPE.WAN;

pub const QOCINFO = extern struct {
    dwSize: u32,
    dwFlags: u32,
    dwInSpeed: u32,
    dwOutSpeed: u32,
};

const CLSID_SENS_Value = @import("../zig.zig").Guid.initString("d597cafe-5b9f-11d1-8dd2-00aa004abd5e");
pub const CLSID_SENS = &CLSID_SENS_Value;

pub const SENS_QOCINFO = extern struct {
    dwSize: u32,
    dwFlags: u32,
    dwOutSpeed: u32,
    dwInSpeed: u32,
};

// TODO: this type is limited to platform 'windows5.1.2600'
const IID_ISensNetwork_Value = @import("../zig.zig").Guid.initString("d597bab1-5b9f-11d1-8dd2-00aa004abd5e");
pub const IID_ISensNetwork = &IID_ISensNetwork_Value;
pub const ISensNetwork = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        ConnectionMade: fn(
            self: *const ISensNetwork,
            bstrConnection: ?BSTR,
            ulType: u32,
            lpQOCInfo: ?*SENS_QOCINFO,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ConnectionMadeNoQOCInfo: fn(
            self: *const ISensNetwork,
            bstrConnection: ?BSTR,
            ulType: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ConnectionLost: fn(
            self: *const ISensNetwork,
            bstrConnection: ?BSTR,
            ulType: SENS_CONNECTION_TYPE,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        DestinationReachable: fn(
            self: *const ISensNetwork,
            bstrDestination: ?BSTR,
            bstrConnection: ?BSTR,
            ulType: u32,
            lpQOCInfo: ?*SENS_QOCINFO,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        DestinationReachableNoQOCInfo: fn(
            self: *const ISensNetwork,
            bstrDestination: ?BSTR,
            bstrConnection: ?BSTR,
            ulType: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensNetwork_ConnectionMade(self: *const T, bstrConnection: ?BSTR, ulType: u32, lpQOCInfo: ?*SENS_QOCINFO) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensNetwork.VTable, self.vtable).ConnectionMade(@ptrCast(*const ISensNetwork, self), bstrConnection, ulType, lpQOCInfo);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensNetwork_ConnectionMadeNoQOCInfo(self: *const T, bstrConnection: ?BSTR, ulType: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensNetwork.VTable, self.vtable).ConnectionMadeNoQOCInfo(@ptrCast(*const ISensNetwork, self), bstrConnection, ulType);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensNetwork_ConnectionLost(self: *const T, bstrConnection: ?BSTR, ulType: SENS_CONNECTION_TYPE) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensNetwork.VTable, self.vtable).ConnectionLost(@ptrCast(*const ISensNetwork, self), bstrConnection, ulType);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensNetwork_DestinationReachable(self: *const T, bstrDestination: ?BSTR, bstrConnection: ?BSTR, ulType: u32, lpQOCInfo: ?*SENS_QOCINFO) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensNetwork.VTable, self.vtable).DestinationReachable(@ptrCast(*const ISensNetwork, self), bstrDestination, bstrConnection, ulType, lpQOCInfo);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensNetwork_DestinationReachableNoQOCInfo(self: *const T, bstrDestination: ?BSTR, bstrConnection: ?BSTR, ulType: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensNetwork.VTable, self.vtable).DestinationReachableNoQOCInfo(@ptrCast(*const ISensNetwork, self), bstrDestination, bstrConnection, ulType);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows5.1.2600'
const IID_ISensOnNow_Value = @import("../zig.zig").Guid.initString("d597bab2-5b9f-11d1-8dd2-00aa004abd5e");
pub const IID_ISensOnNow = &IID_ISensOnNow_Value;
pub const ISensOnNow = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        OnACPower: fn(
            self: *const ISensOnNow,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        OnBatteryPower: fn(
            self: *const ISensOnNow,
            dwBatteryLifePercent: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        BatteryLow: fn(
            self: *const ISensOnNow,
            dwBatteryLifePercent: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensOnNow_OnACPower(self: *const T) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensOnNow.VTable, self.vtable).OnACPower(@ptrCast(*const ISensOnNow, self));
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensOnNow_OnBatteryPower(self: *const T, dwBatteryLifePercent: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensOnNow.VTable, self.vtable).OnBatteryPower(@ptrCast(*const ISensOnNow, self), dwBatteryLifePercent);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensOnNow_BatteryLow(self: *const T, dwBatteryLifePercent: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensOnNow.VTable, self.vtable).BatteryLow(@ptrCast(*const ISensOnNow, self), dwBatteryLifePercent);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows5.1.2600'
const IID_ISensLogon_Value = @import("../zig.zig").Guid.initString("d597bab3-5b9f-11d1-8dd2-00aa004abd5e");
pub const IID_ISensLogon = &IID_ISensLogon_Value;
pub const ISensLogon = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        Logon: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        Logoff: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        StartShell: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        DisplayLock: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        DisplayUnlock: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        StartScreenSaver: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        StopScreenSaver: fn(
            self: *const ISensLogon,
            bstrUserName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_Logon(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).Logon(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_Logoff(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).Logoff(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_StartShell(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).StartShell(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_DisplayLock(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).DisplayLock(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_DisplayUnlock(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).DisplayUnlock(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_StartScreenSaver(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).StartScreenSaver(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon_StopScreenSaver(self: *const T, bstrUserName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon.VTable, self.vtable).StopScreenSaver(@ptrCast(*const ISensLogon, self), bstrUserName);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows5.1.2600'
const IID_ISensLogon2_Value = @import("../zig.zig").Guid.initString("d597bab4-5b9f-11d1-8dd2-00aa004abd5e");
pub const IID_ISensLogon2 = &IID_ISensLogon2_Value;
pub const ISensLogon2 = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        Logon: fn(
            self: *const ISensLogon2,
            bstrUserName: ?BSTR,
            dwSessionId: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        Logoff: fn(
            self: *const ISensLogon2,
            bstrUserName: ?BSTR,
            dwSessionId: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        SessionDisconnect: fn(
            self: *const ISensLogon2,
            bstrUserName: ?BSTR,
            dwSessionId: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        SessionReconnect: fn(
            self: *const ISensLogon2,
            bstrUserName: ?BSTR,
            dwSessionId: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        PostShell: fn(
            self: *const ISensLogon2,
            bstrUserName: ?BSTR,
            dwSessionId: u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon2_Logon(self: *const T, bstrUserName: ?BSTR, dwSessionId: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon2.VTable, self.vtable).Logon(@ptrCast(*const ISensLogon2, self), bstrUserName, dwSessionId);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon2_Logoff(self: *const T, bstrUserName: ?BSTR, dwSessionId: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon2.VTable, self.vtable).Logoff(@ptrCast(*const ISensLogon2, self), bstrUserName, dwSessionId);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon2_SessionDisconnect(self: *const T, bstrUserName: ?BSTR, dwSessionId: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon2.VTable, self.vtable).SessionDisconnect(@ptrCast(*const ISensLogon2, self), bstrUserName, dwSessionId);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon2_SessionReconnect(self: *const T, bstrUserName: ?BSTR, dwSessionId: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon2.VTable, self.vtable).SessionReconnect(@ptrCast(*const ISensLogon2, self), bstrUserName, dwSessionId);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ISensLogon2_PostShell(self: *const T, bstrUserName: ?BSTR, dwSessionId: u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ISensLogon2.VTable, self.vtable).PostShell(@ptrCast(*const ISensLogon2, self), bstrUserName, dwSessionId);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};


//--------------------------------------------------------------------------------
// Section: Functions (3)
//--------------------------------------------------------------------------------
// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "SensApi" fn IsDestinationReachableA(
    lpszDestination: ?[*:0]const u8,
    lpQOCInfo: ?*QOCINFO,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "SensApi" fn IsDestinationReachableW(
    lpszDestination: ?[*:0]const u16,
    lpQOCInfo: ?*QOCINFO,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "SensApi" fn IsNetworkAlive(
    lpdwFlags: ?*u32,
) callconv(@import("std").os.windows.WINAPI) BOOL;


//--------------------------------------------------------------------------------
// Section: Unicode Aliases (1)
//--------------------------------------------------------------------------------
const thismodule = @This();
pub usingnamespace switch (@import("../zig.zig").unicode_mode) {
    .ansi => struct {
        pub const IsDestinationReachable = thismodule.IsDestinationReachableA;
    },
    .wide => struct {
        pub const IsDestinationReachable = thismodule.IsDestinationReachableW;
    },
    .unspecified => if (@import("builtin").is_test) struct {
        pub const IsDestinationReachable = *opaque{};
    } else struct {
        pub const IsDestinationReachable = @compileError("'IsDestinationReachable' requires that UNICODE be set to true or false in the root module");
    },
};
//--------------------------------------------------------------------------------
// Section: Imports (6)
//--------------------------------------------------------------------------------
const BOOL = @import("../foundation.zig").BOOL;
const BSTR = @import("../foundation.zig").BSTR;
const HRESULT = @import("../foundation.zig").HRESULT;
const IDispatch = @import("../system/ole_automation.zig").IDispatch;
const PSTR = @import("../foundation.zig").PSTR;
const PWSTR = @import("../foundation.zig").PWSTR;

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
