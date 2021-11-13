//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (0)
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Section: Types (7)
//--------------------------------------------------------------------------------
const CLSID_WindowsMediaLibrarySharingServices_Value = @import("../zig.zig").Guid.initString("ad581b00-7b64-4e59-a38d-d2c5bf51ddb3");
pub const CLSID_WindowsMediaLibrarySharingServices = &CLSID_WindowsMediaLibrarySharingServices_Value;

pub const WindowsMediaLibrarySharingDeviceAuthorizationStatus = enum(i32) {
    UNKNOWN = 0,
    ALLOWED = 1,
    DENIED = 2,
};
pub const DEVICE_AUTHORIZATION_UNKNOWN = WindowsMediaLibrarySharingDeviceAuthorizationStatus.UNKNOWN;
pub const DEVICE_AUTHORIZATION_ALLOWED = WindowsMediaLibrarySharingDeviceAuthorizationStatus.ALLOWED;
pub const DEVICE_AUTHORIZATION_DENIED = WindowsMediaLibrarySharingDeviceAuthorizationStatus.DENIED;

// TODO: this type is limited to platform 'windows6.1'
const IID_IWindowsMediaLibrarySharingDeviceProperty_Value = @import("../zig.zig").Guid.initString("81e26927-7a7d-40a7-81d4-bddc02960e3e");
pub const IID_IWindowsMediaLibrarySharingDeviceProperty = &IID_IWindowsMediaLibrarySharingDeviceProperty_Value;
pub const IWindowsMediaLibrarySharingDeviceProperty = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Name: fn(
            self: *const IWindowsMediaLibrarySharingDeviceProperty,
            name: ?*?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Value: fn(
            self: *const IWindowsMediaLibrarySharingDeviceProperty,
            value: ?*VARIANT,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDeviceProperty_get_Name(self: *const T, name: ?*?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDeviceProperty.VTable, self.vtable).get_Name(@ptrCast(*const IWindowsMediaLibrarySharingDeviceProperty, self), name);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDeviceProperty_get_Value(self: *const T, value: ?*VARIANT) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDeviceProperty.VTable, self.vtable).get_Value(@ptrCast(*const IWindowsMediaLibrarySharingDeviceProperty, self), value);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows6.1'
const IID_IWindowsMediaLibrarySharingDeviceProperties_Value = @import("../zig.zig").Guid.initString("c4623214-6b06-40c5-a623-b2ff4c076bfd");
pub const IID_IWindowsMediaLibrarySharingDeviceProperties = &IID_IWindowsMediaLibrarySharingDeviceProperties_Value;
pub const IWindowsMediaLibrarySharingDeviceProperties = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Item: fn(
            self: *const IWindowsMediaLibrarySharingDeviceProperties,
            index: i32,
            property: ?*?*IWindowsMediaLibrarySharingDeviceProperty,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Count: fn(
            self: *const IWindowsMediaLibrarySharingDeviceProperties,
            count: ?*i32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetProperty: fn(
            self: *const IWindowsMediaLibrarySharingDeviceProperties,
            name: ?BSTR,
            property: ?*?*IWindowsMediaLibrarySharingDeviceProperty,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDeviceProperties_get_Item(self: *const T, index: i32, property: ?*?*IWindowsMediaLibrarySharingDeviceProperty) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDeviceProperties.VTable, self.vtable).get_Item(@ptrCast(*const IWindowsMediaLibrarySharingDeviceProperties, self), index, property);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDeviceProperties_get_Count(self: *const T, count: ?*i32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDeviceProperties.VTable, self.vtable).get_Count(@ptrCast(*const IWindowsMediaLibrarySharingDeviceProperties, self), count);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDeviceProperties_GetProperty(self: *const T, name: ?BSTR, property: ?*?*IWindowsMediaLibrarySharingDeviceProperty) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDeviceProperties.VTable, self.vtable).GetProperty(@ptrCast(*const IWindowsMediaLibrarySharingDeviceProperties, self), name, property);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows6.1'
const IID_IWindowsMediaLibrarySharingDevice_Value = @import("../zig.zig").Guid.initString("3dccc293-4fd9-4191-a25b-8e57c5d27bd4");
pub const IID_IWindowsMediaLibrarySharingDevice = &IID_IWindowsMediaLibrarySharingDevice_Value;
pub const IWindowsMediaLibrarySharingDevice = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_DeviceID: fn(
            self: *const IWindowsMediaLibrarySharingDevice,
            deviceID: ?*?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Authorization: fn(
            self: *const IWindowsMediaLibrarySharingDevice,
            authorization: ?*WindowsMediaLibrarySharingDeviceAuthorizationStatus,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_Authorization: fn(
            self: *const IWindowsMediaLibrarySharingDevice,
            authorization: WindowsMediaLibrarySharingDeviceAuthorizationStatus,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Properties: fn(
            self: *const IWindowsMediaLibrarySharingDevice,
            deviceProperties: ?*?*IWindowsMediaLibrarySharingDeviceProperties,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevice_get_DeviceID(self: *const T, deviceID: ?*?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevice.VTable, self.vtable).get_DeviceID(@ptrCast(*const IWindowsMediaLibrarySharingDevice, self), deviceID);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevice_get_Authorization(self: *const T, authorization: ?*WindowsMediaLibrarySharingDeviceAuthorizationStatus) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevice.VTable, self.vtable).get_Authorization(@ptrCast(*const IWindowsMediaLibrarySharingDevice, self), authorization);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevice_put_Authorization(self: *const T, authorization: WindowsMediaLibrarySharingDeviceAuthorizationStatus) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevice.VTable, self.vtable).put_Authorization(@ptrCast(*const IWindowsMediaLibrarySharingDevice, self), authorization);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevice_get_Properties(self: *const T, deviceProperties: ?*?*IWindowsMediaLibrarySharingDeviceProperties) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevice.VTable, self.vtable).get_Properties(@ptrCast(*const IWindowsMediaLibrarySharingDevice, self), deviceProperties);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows6.1'
const IID_IWindowsMediaLibrarySharingDevices_Value = @import("../zig.zig").Guid.initString("1803f9d6-fe6d-4546-bf5b-992fe8ec12d1");
pub const IID_IWindowsMediaLibrarySharingDevices = &IID_IWindowsMediaLibrarySharingDevices_Value;
pub const IWindowsMediaLibrarySharingDevices = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Item: fn(
            self: *const IWindowsMediaLibrarySharingDevices,
            index: i32,
            device: ?*?*IWindowsMediaLibrarySharingDevice,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_Count: fn(
            self: *const IWindowsMediaLibrarySharingDevices,
            count: ?*i32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetDevice: fn(
            self: *const IWindowsMediaLibrarySharingDevices,
            deviceID: ?BSTR,
            device: ?*?*IWindowsMediaLibrarySharingDevice,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevices_get_Item(self: *const T, index: i32, device: ?*?*IWindowsMediaLibrarySharingDevice) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevices.VTable, self.vtable).get_Item(@ptrCast(*const IWindowsMediaLibrarySharingDevices, self), index, device);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevices_get_Count(self: *const T, count: ?*i32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevices.VTable, self.vtable).get_Count(@ptrCast(*const IWindowsMediaLibrarySharingDevices, self), count);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingDevices_GetDevice(self: *const T, deviceID: ?BSTR, device: ?*?*IWindowsMediaLibrarySharingDevice) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingDevices.VTable, self.vtable).GetDevice(@ptrCast(*const IWindowsMediaLibrarySharingDevices, self), deviceID, device);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows6.1'
const IID_IWindowsMediaLibrarySharingServices_Value = @import("../zig.zig").Guid.initString("01f5f85e-0a81-40da-a7c8-21ef3af8440c");
pub const IID_IWindowsMediaLibrarySharingServices = &IID_IWindowsMediaLibrarySharingServices_Value;
pub const IWindowsMediaLibrarySharingServices = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        showShareMediaCPL: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            device: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_userHomeMediaSharingState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingEnabled: ?*i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_userHomeMediaSharingState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingEnabled: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_userHomeMediaSharingLibraryName: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            libraryName: ?*?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_userHomeMediaSharingLibraryName: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            libraryName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_computerHomeMediaSharingAllowedState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingAllowed: ?*i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_computerHomeMediaSharingAllowedState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingAllowed: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_userInternetMediaSharingState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingEnabled: ?*i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_userInternetMediaSharingState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingEnabled: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_computerInternetMediaSharingAllowedState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingAllowed: ?*i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_computerInternetMediaSharingAllowedState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingAllowed: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_internetMediaSharingSecurityGroup: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            securityGroup: ?*?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_internetMediaSharingSecurityGroup: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            securityGroup: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_allowSharingToAllDevices: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingEnabled: ?*i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        put_allowSharingToAllDevices: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            sharingEnabled: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        setDefaultAuthorization: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            MACAddresses: ?BSTR,
            friendlyName: ?BSTR,
            authorization: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        setAuthorizationState: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            MACAddress: ?BSTR,
            authorizationState: i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        getAllDevices: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            devices: ?*?*IWindowsMediaLibrarySharingDevices,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_customSettingsApplied: fn(
            self: *const IWindowsMediaLibrarySharingServices,
            customSettingsApplied: ?*i16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_showShareMediaCPL(self: *const T, device: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).showShareMediaCPL(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), device);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_userHomeMediaSharingState(self: *const T, sharingEnabled: ?*i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_userHomeMediaSharingState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingEnabled);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_userHomeMediaSharingState(self: *const T, sharingEnabled: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_userHomeMediaSharingState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingEnabled);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_userHomeMediaSharingLibraryName(self: *const T, libraryName: ?*?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_userHomeMediaSharingLibraryName(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), libraryName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_userHomeMediaSharingLibraryName(self: *const T, libraryName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_userHomeMediaSharingLibraryName(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), libraryName);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_computerHomeMediaSharingAllowedState(self: *const T, sharingAllowed: ?*i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_computerHomeMediaSharingAllowedState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingAllowed);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_computerHomeMediaSharingAllowedState(self: *const T, sharingAllowed: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_computerHomeMediaSharingAllowedState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingAllowed);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_userInternetMediaSharingState(self: *const T, sharingEnabled: ?*i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_userInternetMediaSharingState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingEnabled);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_userInternetMediaSharingState(self: *const T, sharingEnabled: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_userInternetMediaSharingState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingEnabled);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_computerInternetMediaSharingAllowedState(self: *const T, sharingAllowed: ?*i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_computerInternetMediaSharingAllowedState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingAllowed);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_computerInternetMediaSharingAllowedState(self: *const T, sharingAllowed: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_computerInternetMediaSharingAllowedState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingAllowed);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_internetMediaSharingSecurityGroup(self: *const T, securityGroup: ?*?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_internetMediaSharingSecurityGroup(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), securityGroup);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_internetMediaSharingSecurityGroup(self: *const T, securityGroup: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_internetMediaSharingSecurityGroup(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), securityGroup);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_allowSharingToAllDevices(self: *const T, sharingEnabled: ?*i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_allowSharingToAllDevices(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingEnabled);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_put_allowSharingToAllDevices(self: *const T, sharingEnabled: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).put_allowSharingToAllDevices(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), sharingEnabled);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_setDefaultAuthorization(self: *const T, MACAddresses: ?BSTR, friendlyName: ?BSTR, authorization: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).setDefaultAuthorization(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), MACAddresses, friendlyName, authorization);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_setAuthorizationState(self: *const T, MACAddress: ?BSTR, authorizationState: i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).setAuthorizationState(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), MACAddress, authorizationState);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_getAllDevices(self: *const T, devices: ?*?*IWindowsMediaLibrarySharingDevices) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).getAllDevices(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), devices);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWindowsMediaLibrarySharingServices_get_customSettingsApplied(self: *const T, customSettingsApplied: ?*i16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWindowsMediaLibrarySharingServices.VTable, self.vtable).get_customSettingsApplied(@ptrCast(*const IWindowsMediaLibrarySharingServices, self), customSettingsApplied);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};


//--------------------------------------------------------------------------------
// Section: Functions (0)
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Section: Unicode Aliases (0)
//--------------------------------------------------------------------------------
const thismodule = @This();
pub usingnamespace switch (@import("../zig.zig").unicode_mode) {
    .ansi => struct {
    },
    .wide => struct {
    },
    .unspecified => if (@import("builtin").is_test) struct {
    } else struct {
    },
};
//--------------------------------------------------------------------------------
// Section: Imports (4)
//--------------------------------------------------------------------------------
const BSTR = @import("../foundation.zig").BSTR;
const HRESULT = @import("../foundation.zig").HRESULT;
const IDispatch = @import("../system/ole_automation.zig").IDispatch;
const VARIANT = @import("../system/ole_automation.zig").VARIANT;

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