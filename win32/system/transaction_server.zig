//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (0)
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Section: Types (15)
//--------------------------------------------------------------------------------
const CLSID_Catalog_Value = @import("../zig.zig").Guid.initString("6eb22881-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_Catalog = &CLSID_Catalog_Value;

const CLSID_CatalogObject_Value = @import("../zig.zig").Guid.initString("6eb22882-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_CatalogObject = &CLSID_CatalogObject_Value;

const CLSID_CatalogCollection_Value = @import("../zig.zig").Guid.initString("6eb22883-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_CatalogCollection = &CLSID_CatalogCollection_Value;

const CLSID_ComponentUtil_Value = @import("../zig.zig").Guid.initString("6eb22884-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_ComponentUtil = &CLSID_ComponentUtil_Value;

const CLSID_PackageUtil_Value = @import("../zig.zig").Guid.initString("6eb22885-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_PackageUtil = &CLSID_PackageUtil_Value;

const CLSID_RemoteComponentUtil_Value = @import("../zig.zig").Guid.initString("6eb22886-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_RemoteComponentUtil = &CLSID_RemoteComponentUtil_Value;

const CLSID_RoleAssociationUtil_Value = @import("../zig.zig").Guid.initString("6eb22887-8a19-11d0-81b6-00a0c9231c29");
pub const CLSID_RoleAssociationUtil = &CLSID_RoleAssociationUtil_Value;

const IID_ICatalog_Value = @import("../zig.zig").Guid.initString("6eb22870-8a19-11d0-81b6-00a0c9231c29");
pub const IID_ICatalog = &IID_ICatalog_Value;
pub const ICatalog = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        GetCollection: fn(
            self: *const ICatalog,
            bstrCollName: ?BSTR,
            ppCatalogCollection: ?*?*IDispatch,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        Connect: fn(
            self: *const ICatalog,
            bstrConnectString: ?BSTR,
            ppCatalogCollection: ?*?*IDispatch,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_MajorVersion: fn(
            self: *const ICatalog,
            retval: ?*i32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        // TODO: this function has a "SpecialName", should Zig do anything with this?
        get_MinorVersion: fn(
            self: *const ICatalog,
            retval: ?*i32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ICatalog_GetCollection(self: *const T, bstrCollName: ?BSTR, ppCatalogCollection: ?*?*IDispatch) callconv(.Inline) HRESULT {
            return @ptrCast(*const ICatalog.VTable, self.vtable).GetCollection(@ptrCast(*const ICatalog, self), bstrCollName, ppCatalogCollection);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ICatalog_Connect(self: *const T, bstrConnectString: ?BSTR, ppCatalogCollection: ?*?*IDispatch) callconv(.Inline) HRESULT {
            return @ptrCast(*const ICatalog.VTable, self.vtable).Connect(@ptrCast(*const ICatalog, self), bstrConnectString, ppCatalogCollection);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ICatalog_get_MajorVersion(self: *const T, retval: ?*i32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ICatalog.VTable, self.vtable).get_MajorVersion(@ptrCast(*const ICatalog, self), retval);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn ICatalog_get_MinorVersion(self: *const T, retval: ?*i32) callconv(.Inline) HRESULT {
            return @ptrCast(*const ICatalog.VTable, self.vtable).get_MinorVersion(@ptrCast(*const ICatalog, self), retval);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

const IID_IComponentUtil_Value = @import("../zig.zig").Guid.initString("6eb22873-8a19-11d0-81b6-00a0c9231c29");
pub const IID_IComponentUtil = &IID_IComponentUtil_Value;
pub const IComponentUtil = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        InstallComponent: fn(
            self: *const IComponentUtil,
            bstrDLLFile: ?BSTR,
            bstrTypelibFile: ?BSTR,
            bstrProxyStubDLLFile: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ImportComponent: fn(
            self: *const IComponentUtil,
            bstrCLSID: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ImportComponentByName: fn(
            self: *const IComponentUtil,
            bstrProgID: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetCLSIDs: fn(
            self: *const IComponentUtil,
            bstrDLLFile: ?BSTR,
            bstrTypelibFile: ?BSTR,
            aCLSIDs: ?*?*SAFEARRAY,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IComponentUtil_InstallComponent(self: *const T, bstrDLLFile: ?BSTR, bstrTypelibFile: ?BSTR, bstrProxyStubDLLFile: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IComponentUtil.VTable, self.vtable).InstallComponent(@ptrCast(*const IComponentUtil, self), bstrDLLFile, bstrTypelibFile, bstrProxyStubDLLFile);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IComponentUtil_ImportComponent(self: *const T, bstrCLSID: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IComponentUtil.VTable, self.vtable).ImportComponent(@ptrCast(*const IComponentUtil, self), bstrCLSID);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IComponentUtil_ImportComponentByName(self: *const T, bstrProgID: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IComponentUtil.VTable, self.vtable).ImportComponentByName(@ptrCast(*const IComponentUtil, self), bstrProgID);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IComponentUtil_GetCLSIDs(self: *const T, bstrDLLFile: ?BSTR, bstrTypelibFile: ?BSTR, aCLSIDs: ?*?*SAFEARRAY) callconv(.Inline) HRESULT {
            return @ptrCast(*const IComponentUtil.VTable, self.vtable).GetCLSIDs(@ptrCast(*const IComponentUtil, self), bstrDLLFile, bstrTypelibFile, aCLSIDs);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

const IID_IPackageUtil_Value = @import("../zig.zig").Guid.initString("6eb22874-8a19-11d0-81b6-00a0c9231c29");
pub const IID_IPackageUtil = &IID_IPackageUtil_Value;
pub const IPackageUtil = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        InstallPackage: fn(
            self: *const IPackageUtil,
            bstrPackageFile: ?BSTR,
            bstrInstallPath: ?BSTR,
            lOptions: i32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ExportPackage: fn(
            self: *const IPackageUtil,
            bstrPackageID: ?BSTR,
            bstrPackageFile: ?BSTR,
            lOptions: i32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ShutdownPackage: fn(
            self: *const IPackageUtil,
            bstrPackageID: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IPackageUtil_InstallPackage(self: *const T, bstrPackageFile: ?BSTR, bstrInstallPath: ?BSTR, lOptions: i32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IPackageUtil.VTable, self.vtable).InstallPackage(@ptrCast(*const IPackageUtil, self), bstrPackageFile, bstrInstallPath, lOptions);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IPackageUtil_ExportPackage(self: *const T, bstrPackageID: ?BSTR, bstrPackageFile: ?BSTR, lOptions: i32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IPackageUtil.VTable, self.vtable).ExportPackage(@ptrCast(*const IPackageUtil, self), bstrPackageID, bstrPackageFile, lOptions);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IPackageUtil_ShutdownPackage(self: *const T, bstrPackageID: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IPackageUtil.VTable, self.vtable).ShutdownPackage(@ptrCast(*const IPackageUtil, self), bstrPackageID);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

const IID_IRemoteComponentUtil_Value = @import("../zig.zig").Guid.initString("6eb22875-8a19-11d0-81b6-00a0c9231c29");
pub const IID_IRemoteComponentUtil = &IID_IRemoteComponentUtil_Value;
pub const IRemoteComponentUtil = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        InstallRemoteComponent: fn(
            self: *const IRemoteComponentUtil,
            bstrServer: ?BSTR,
            bstrPackageID: ?BSTR,
            bstrCLSID: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        InstallRemoteComponentByName: fn(
            self: *const IRemoteComponentUtil,
            bstrServer: ?BSTR,
            bstrPackageName: ?BSTR,
            bstrProgID: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IRemoteComponentUtil_InstallRemoteComponent(self: *const T, bstrServer: ?BSTR, bstrPackageID: ?BSTR, bstrCLSID: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IRemoteComponentUtil.VTable, self.vtable).InstallRemoteComponent(@ptrCast(*const IRemoteComponentUtil, self), bstrServer, bstrPackageID, bstrCLSID);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IRemoteComponentUtil_InstallRemoteComponentByName(self: *const T, bstrServer: ?BSTR, bstrPackageName: ?BSTR, bstrProgID: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IRemoteComponentUtil.VTable, self.vtable).InstallRemoteComponentByName(@ptrCast(*const IRemoteComponentUtil, self), bstrServer, bstrPackageName, bstrProgID);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

const IID_IRoleAssociationUtil_Value = @import("../zig.zig").Guid.initString("6eb22876-8a19-11d0-81b6-00a0c9231c29");
pub const IID_IRoleAssociationUtil = &IID_IRoleAssociationUtil_Value;
pub const IRoleAssociationUtil = extern struct {
    pub const VTable = extern struct {
        base: IDispatch.VTable,
        AssociateRole: fn(
            self: *const IRoleAssociationUtil,
            bstrRoleID: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        AssociateRoleByName: fn(
            self: *const IRoleAssociationUtil,
            bstrRoleName: ?BSTR,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IDispatch.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IRoleAssociationUtil_AssociateRole(self: *const T, bstrRoleID: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IRoleAssociationUtil.VTable, self.vtable).AssociateRole(@ptrCast(*const IRoleAssociationUtil, self), bstrRoleID);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IRoleAssociationUtil_AssociateRoleByName(self: *const T, bstrRoleName: ?BSTR) callconv(.Inline) HRESULT {
            return @ptrCast(*const IRoleAssociationUtil.VTable, self.vtable).AssociateRoleByName(@ptrCast(*const IRoleAssociationUtil, self), bstrRoleName);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

pub const __MIDL___MIDL_itf_mtxadmin_0107_0001 = enum(i32) {
    s = 1,
};
pub const mtsInstallUsers = __MIDL___MIDL_itf_mtxadmin_0107_0001.s;

pub const __MIDL___MIDL_itf_mtxadmin_0107_0002 = enum(i32) {
    s = 1,
};
pub const mtsExportUsers = __MIDL___MIDL_itf_mtxadmin_0107_0002.s;

pub const __MIDL___MIDL_itf_mtxadmin_0107_0003 = enum(i32) {
    ObjectErrors = -2146368511,
    ObjectInvalid = -2146368510,
    KeyMissing = -2146368509,
    AlreadyInstalled = -2146368508,
    DownloadFailed = -2146368507,
    PDFWriteFail = -2146368505,
    PDFReadFail = -2146368504,
    PDFVersion = -2146368503,
    CoReqCompInstalled = -2146368496,
    BadPath = -2146368502,
    PackageExists = -2146368501,
    RoleExists = -2146368500,
    CantCopyFile = -2146368499,
    NoTypeLib = -2146368498,
    NoUser = -2146368497,
    // InvalidUserids = -2146368496, this enum value conflicts with CoReqCompInstalled
    NoRegistryCLSID = -2146368495,
    BadRegistryProgID = -2146368494,
    AuthenticationLevel = -2146368493,
    UserPasswdNotValid = -2146368492,
    NoRegistryRead = -2146368491,
    NoRegistryWrite = -2146368490,
    NoRegistryRepair = -2146368489,
    CLSIDOrIIDMismatch = -2146368488,
    RemoteInterface = -2146368487,
    DllRegisterServer = -2146368486,
    NoServerShare = -2146368485,
    NoAccessToUNC = -2146368484,
    DllLoadFailed = -2146368483,
    BadRegistryLibID = -2146368482,
    PackDirNotFound = -2146368481,
    TreatAs = -2146368480,
    BadForward = -2146368479,
    BadIID = -2146368478,
    RegistrarFailed = -2146368477,
    CompFileDoesNotExist = -2146368476,
    CompFileLoadDLLFail = -2146368475,
    CompFileGetClassObj = -2146368474,
    CompFileClassNotAvail = -2146368473,
    CompFileBadTLB = -2146368472,
    CompFileNotInstallable = -2146368471,
    NotChangeable = -2146368470,
    NotDeletable = -2146368469,
    Session = -2146368468,
    CompFileNoRegistrar = -2146368460,
};
pub const mtsErrObjectErrors = __MIDL___MIDL_itf_mtxadmin_0107_0003.ObjectErrors;
pub const mtsErrObjectInvalid = __MIDL___MIDL_itf_mtxadmin_0107_0003.ObjectInvalid;
pub const mtsErrKeyMissing = __MIDL___MIDL_itf_mtxadmin_0107_0003.KeyMissing;
pub const mtsErrAlreadyInstalled = __MIDL___MIDL_itf_mtxadmin_0107_0003.AlreadyInstalled;
pub const mtsErrDownloadFailed = __MIDL___MIDL_itf_mtxadmin_0107_0003.DownloadFailed;
pub const mtsErrPDFWriteFail = __MIDL___MIDL_itf_mtxadmin_0107_0003.PDFWriteFail;
pub const mtsErrPDFReadFail = __MIDL___MIDL_itf_mtxadmin_0107_0003.PDFReadFail;
pub const mtsErrPDFVersion = __MIDL___MIDL_itf_mtxadmin_0107_0003.PDFVersion;
pub const mtsErrCoReqCompInstalled = __MIDL___MIDL_itf_mtxadmin_0107_0003.CoReqCompInstalled;
pub const mtsErrBadPath = __MIDL___MIDL_itf_mtxadmin_0107_0003.BadPath;
pub const mtsErrPackageExists = __MIDL___MIDL_itf_mtxadmin_0107_0003.PackageExists;
pub const mtsErrRoleExists = __MIDL___MIDL_itf_mtxadmin_0107_0003.RoleExists;
pub const mtsErrCantCopyFile = __MIDL___MIDL_itf_mtxadmin_0107_0003.CantCopyFile;
pub const mtsErrNoTypeLib = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoTypeLib;
pub const mtsErrNoUser = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoUser;
pub const mtsErrInvalidUserids = __MIDL___MIDL_itf_mtxadmin_0107_0003.CoReqCompInstalled;
pub const mtsErrNoRegistryCLSID = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoRegistryCLSID;
pub const mtsErrBadRegistryProgID = __MIDL___MIDL_itf_mtxadmin_0107_0003.BadRegistryProgID;
pub const mtsErrAuthenticationLevel = __MIDL___MIDL_itf_mtxadmin_0107_0003.AuthenticationLevel;
pub const mtsErrUserPasswdNotValid = __MIDL___MIDL_itf_mtxadmin_0107_0003.UserPasswdNotValid;
pub const mtsErrNoRegistryRead = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoRegistryRead;
pub const mtsErrNoRegistryWrite = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoRegistryWrite;
pub const mtsErrNoRegistryRepair = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoRegistryRepair;
pub const mtsErrCLSIDOrIIDMismatch = __MIDL___MIDL_itf_mtxadmin_0107_0003.CLSIDOrIIDMismatch;
pub const mtsErrRemoteInterface = __MIDL___MIDL_itf_mtxadmin_0107_0003.RemoteInterface;
pub const mtsErrDllRegisterServer = __MIDL___MIDL_itf_mtxadmin_0107_0003.DllRegisterServer;
pub const mtsErrNoServerShare = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoServerShare;
pub const mtsErrNoAccessToUNC = __MIDL___MIDL_itf_mtxadmin_0107_0003.NoAccessToUNC;
pub const mtsErrDllLoadFailed = __MIDL___MIDL_itf_mtxadmin_0107_0003.DllLoadFailed;
pub const mtsErrBadRegistryLibID = __MIDL___MIDL_itf_mtxadmin_0107_0003.BadRegistryLibID;
pub const mtsErrPackDirNotFound = __MIDL___MIDL_itf_mtxadmin_0107_0003.PackDirNotFound;
pub const mtsErrTreatAs = __MIDL___MIDL_itf_mtxadmin_0107_0003.TreatAs;
pub const mtsErrBadForward = __MIDL___MIDL_itf_mtxadmin_0107_0003.BadForward;
pub const mtsErrBadIID = __MIDL___MIDL_itf_mtxadmin_0107_0003.BadIID;
pub const mtsErrRegistrarFailed = __MIDL___MIDL_itf_mtxadmin_0107_0003.RegistrarFailed;
pub const mtsErrCompFileDoesNotExist = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileDoesNotExist;
pub const mtsErrCompFileLoadDLLFail = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileLoadDLLFail;
pub const mtsErrCompFileGetClassObj = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileGetClassObj;
pub const mtsErrCompFileClassNotAvail = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileClassNotAvail;
pub const mtsErrCompFileBadTLB = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileBadTLB;
pub const mtsErrCompFileNotInstallable = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileNotInstallable;
pub const mtsErrNotChangeable = __MIDL___MIDL_itf_mtxadmin_0107_0003.NotChangeable;
pub const mtsErrNotDeletable = __MIDL___MIDL_itf_mtxadmin_0107_0003.NotDeletable;
pub const mtsErrSession = __MIDL___MIDL_itf_mtxadmin_0107_0003.Session;
pub const mtsErrCompFileNoRegistrar = __MIDL___MIDL_itf_mtxadmin_0107_0003.CompFileNoRegistrar;


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
const IDispatch = @import("../system/com.zig").IDispatch;
const SAFEARRAY = @import("../system/com.zig").SAFEARRAY;

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
