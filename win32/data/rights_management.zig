//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (62)
//--------------------------------------------------------------------------------
pub const DRMHANDLE_INVALID = @as(u32, 0);
pub const DRMENVHANDLE_INVALID = @as(u32, 0);
pub const DRMQUERYHANDLE_INVALID = @as(u32, 0);
pub const DRMHSESSION_INVALID = @as(u32, 0);
pub const DRMPUBHANDLE_INVALID = @as(u32, 0);
pub const DRM_AL_NONSILENT = @as(u32, 1);
pub const DRM_AL_NOPERSIST = @as(u32, 2);
pub const DRM_AL_CANCEL = @as(u32, 4);
pub const DRM_AL_FETCHNOADVISORY = @as(u32, 8);
pub const DRM_AL_NOUI = @as(u32, 16);
pub const DRM_ACTIVATE_MACHINE = @as(u32, 1);
pub const DRM_ACTIVATE_GROUPIDENTITY = @as(u32, 2);
pub const DRM_ACTIVATE_TEMPORARY = @as(u32, 4);
pub const DRM_ACTIVATE_CANCEL = @as(u32, 8);
pub const DRM_ACTIVATE_SILENT = @as(u32, 16);
pub const DRM_ACTIVATE_SHARED_GROUPIDENTITY = @as(u32, 32);
pub const DRM_ACTIVATE_DELAYED = @as(u32, 64);
pub const DRM_EL_MACHINE = @as(u32, 1);
pub const DRM_EL_GROUPIDENTITY = @as(u32, 2);
pub const DRM_EL_GROUPIDENTITY_NAME = @as(u32, 4);
pub const DRM_EL_GROUPIDENTITY_LID = @as(u32, 8);
pub const DRM_EL_SPECIFIED_GROUPIDENTITY = @as(u32, 16);
pub const DRM_EL_EUL = @as(u32, 32);
pub const DRM_EL_EUL_LID = @as(u32, 64);
pub const DRM_EL_CLIENTLICENSOR = @as(u32, 128);
pub const DRM_EL_CLIENTLICENSOR_LID = @as(u32, 256);
pub const DRM_EL_SPECIFIED_CLIENTLICENSOR = @as(u32, 512);
pub const DRM_EL_REVOCATIONLIST = @as(u32, 1024);
pub const DRM_EL_REVOCATIONLIST_LID = @as(u32, 2048);
pub const DRM_EL_EXPIRED = @as(u32, 4096);
pub const DRM_EL_ISSUERNAME = @as(u32, 8192);
pub const DRM_EL_ISSUANCELICENSE_TEMPLATE = @as(u32, 16384);
pub const DRM_EL_ISSUANCELICENSE_TEMPLATE_LID = @as(u32, 32768);
pub const DRM_ADD_LICENSE_NOPERSIST = @as(u32, 0);
pub const DRM_ADD_LICENSE_PERSIST = @as(u32, 1);
pub const DRM_SERVICE_TYPE_ACTIVATION = @as(u32, 1);
pub const DRM_SERVICE_TYPE_CERTIFICATION = @as(u32, 2);
pub const DRM_SERVICE_TYPE_PUBLISHING = @as(u32, 4);
pub const DRM_SERVICE_TYPE_CLIENTLICENSOR = @as(u32, 8);
pub const DRM_SERVICE_TYPE_SILENT = @as(u32, 16);
pub const DRM_SERVICE_LOCATION_INTERNET = @as(u32, 1);
pub const DRM_SERVICE_LOCATION_ENTERPRISE = @as(u32, 2);
pub const DRM_SIGN_ONLINE = @as(u32, 1);
pub const DRM_SIGN_OFFLINE = @as(u32, 2);
pub const DRM_SIGN_CANCEL = @as(u32, 4);
pub const DRM_SERVER_ISSUANCELICENSE = @as(u32, 8);
pub const DRM_AUTO_GENERATE_KEY = @as(u32, 16);
pub const DRM_OWNER_LICENSE_NOPERSIST = @as(u32, 32);
pub const DRM_REUSE_KEY = @as(u32, 64);
pub const DRM_LOCKBOXTYPE_NONE = @as(u32, 0);
pub const DRM_LOCKBOXTYPE_WHITEBOX = @as(u32, 1);
pub const DRM_LOCKBOXTYPE_BLACKBOX = @as(u32, 2);
pub const DRM_AILT_NONSILENT = @as(u32, 1);
pub const DRM_AILT_OBTAIN_ALL = @as(u32, 2);
pub const DRM_AILT_CANCEL = @as(u32, 4);
pub const DRMIDVERSION = @as(u32, 0);
pub const DRMBOUNDLICENSEPARAMSVERSION = @as(u32, 1);
pub const DRMBINDINGFLAGS_IGNORE_VALIDITY_INTERVALS = @as(u32, 1);
pub const DRMLICENSEACQDATAVERSION = @as(u32, 0);
pub const DRMACTSERVINFOVERSION = @as(u32, 0);
pub const DRMCLIENTSTRUCTVERSION = @as(u32, 1);
pub const DRMCALLBACKVERSION = @as(u32, 1);

//--------------------------------------------------------------------------------
// Section: Types (15)
//--------------------------------------------------------------------------------
pub const DRMID = extern struct {
    uVersion: u32,
    wszIDType: ?PWSTR,
    wszID: ?PWSTR,
};

pub const DRMTIMETYPE = enum(i32) {
    UTC = 0,
    LOCAL = 1,
};
pub const DRMTIMETYPE_SYSTEMUTC = DRMTIMETYPE.UTC;
pub const DRMTIMETYPE_SYSTEMLOCAL = DRMTIMETYPE.LOCAL;

pub const DRMENCODINGTYPE = enum(i32) {
    BASE64 = 0,
    STRING = 1,
    LONG = 2,
    TIME = 3,
    UINT = 4,
    RAW = 5,
};
pub const DRMENCODINGTYPE_BASE64 = DRMENCODINGTYPE.BASE64;
pub const DRMENCODINGTYPE_STRING = DRMENCODINGTYPE.STRING;
pub const DRMENCODINGTYPE_LONG = DRMENCODINGTYPE.LONG;
pub const DRMENCODINGTYPE_TIME = DRMENCODINGTYPE.TIME;
pub const DRMENCODINGTYPE_UINT = DRMENCODINGTYPE.UINT;
pub const DRMENCODINGTYPE_RAW = DRMENCODINGTYPE.RAW;

pub const DRMATTESTTYPE = enum(i32) {
    FULLENVIRONMENT = 0,
    HASHONLY = 1,
};
pub const DRMATTESTTYPE_FULLENVIRONMENT = DRMATTESTTYPE.FULLENVIRONMENT;
pub const DRMATTESTTYPE_HASHONLY = DRMATTESTTYPE.HASHONLY;

pub const DRMSPECTYPE = enum(i32) {
    UNKNOWN = 0,
    FILENAME = 1,
};
pub const DRMSPECTYPE_UNKNOWN = DRMSPECTYPE.UNKNOWN;
pub const DRMSPECTYPE_FILENAME = DRMSPECTYPE.FILENAME;

pub const DRMSECURITYPROVIDERTYPE = enum(i32) {
    P = 0,
};
pub const DRMSECURITYPROVIDERTYPE_SOFTWARESECREP = DRMSECURITYPROVIDERTYPE.P;

pub const DRMGLOBALOPTIONS = enum(i32) {
    WINHTTP = 0,
    SERVERSECURITYPROCESSOR = 1,
};
pub const DRMGLOBALOPTIONS_USE_WINHTTP = DRMGLOBALOPTIONS.WINHTTP;
pub const DRMGLOBALOPTIONS_USE_SERVERSECURITYPROCESSOR = DRMGLOBALOPTIONS.SERVERSECURITYPROCESSOR;

pub const DRMBOUNDLICENSEPARAMS = extern struct {
    uVersion: u32,
    hEnablingPrincipal: u32,
    hSecureStore: u32,
    wszRightsRequested: ?PWSTR,
    wszRightsGroup: ?PWSTR,
    idResource: DRMID,
    cAuthenticatorCount: u32,
    rghAuthenticators: ?*u32,
    wszDefaultEnablingPrincipalCredentials: ?PWSTR,
    dwFlags: u32,
};

pub const DRM_LICENSE_ACQ_DATA = extern struct {
    uVersion: u32,
    wszURL: ?PWSTR,
    wszLocalFilename: ?PWSTR,
    pbPostData: ?*u8,
    dwPostDataSize: u32,
    wszFriendlyName: ?PWSTR,
};

pub const DRM_ACTSERV_INFO = extern struct {
    uVersion: u32,
    wszPubKey: ?PWSTR,
    wszURL: ?PWSTR,
};

pub const DRM_CLIENT_VERSION_INFO = extern struct {
    uStructVersion: u32,
    dwVersion: [4]u32,
    wszHierarchy: [256]u16,
    wszProductId: [256]u16,
    wszProductDescription: [256]u16,
};

pub const DRM_STATUS_MSG = enum(i32) {
    ACTIVATE_MACHINE = 0,
    ACTIVATE_GROUPIDENTITY = 1,
    ACQUIRE_LICENSE = 2,
    ACQUIRE_ADVISORY = 3,
    SIGN_ISSUANCE_LICENSE = 4,
    ACQUIRE_CLIENTLICENSOR = 5,
    ACQUIRE_ISSUANCE_LICENSE_TEMPLATE = 6,
};
pub const DRM_MSG_ACTIVATE_MACHINE = DRM_STATUS_MSG.ACTIVATE_MACHINE;
pub const DRM_MSG_ACTIVATE_GROUPIDENTITY = DRM_STATUS_MSG.ACTIVATE_GROUPIDENTITY;
pub const DRM_MSG_ACQUIRE_LICENSE = DRM_STATUS_MSG.ACQUIRE_LICENSE;
pub const DRM_MSG_ACQUIRE_ADVISORY = DRM_STATUS_MSG.ACQUIRE_ADVISORY;
pub const DRM_MSG_SIGN_ISSUANCE_LICENSE = DRM_STATUS_MSG.SIGN_ISSUANCE_LICENSE;
pub const DRM_MSG_ACQUIRE_CLIENTLICENSOR = DRM_STATUS_MSG.ACQUIRE_CLIENTLICENSOR;
pub const DRM_MSG_ACQUIRE_ISSUANCE_LICENSE_TEMPLATE = DRM_STATUS_MSG.ACQUIRE_ISSUANCE_LICENSE_TEMPLATE;

pub const DRM_USAGEPOLICY_TYPE = enum(i32) {
    BYNAME = 0,
    BYPUBLICKEY = 1,
    BYDIGEST = 2,
    OSEXCLUSION = 3,
};
pub const DRM_USAGEPOLICY_TYPE_BYNAME = DRM_USAGEPOLICY_TYPE.BYNAME;
pub const DRM_USAGEPOLICY_TYPE_BYPUBLICKEY = DRM_USAGEPOLICY_TYPE.BYPUBLICKEY;
pub const DRM_USAGEPOLICY_TYPE_BYDIGEST = DRM_USAGEPOLICY_TYPE.BYDIGEST;
pub const DRM_USAGEPOLICY_TYPE_OSEXCLUSION = DRM_USAGEPOLICY_TYPE.OSEXCLUSION;

pub const DRM_DISTRIBUTION_POINT_INFO = enum(i32) {
    LICENSE_ACQUISITION = 0,
    PUBLISHING = 1,
    REFERRAL_INFO = 2,
};
pub const DRM_DISTRIBUTION_POINT_LICENSE_ACQUISITION = DRM_DISTRIBUTION_POINT_INFO.LICENSE_ACQUISITION;
pub const DRM_DISTRIBUTION_POINT_PUBLISHING = DRM_DISTRIBUTION_POINT_INFO.PUBLISHING;
pub const DRM_DISTRIBUTION_POINT_REFERRAL_INFO = DRM_DISTRIBUTION_POINT_INFO.REFERRAL_INFO;

pub const DRMCALLBACK = fn(
    param0: DRM_STATUS_MSG,
    param1: HRESULT,
    param2: ?*c_void,
    param3: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) HRESULT;


//--------------------------------------------------------------------------------
// Section: Functions (84)
//--------------------------------------------------------------------------------
pub extern "msdrm" fn DRMSetGlobalOptions(
    eGlobalOptions: DRMGLOBALOPTIONS,
    pvdata: ?*c_void,
    dwlen: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetClientVersion(
    pDRMClientVersionInfo: ?*DRM_CLIENT_VERSION_INFO,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMInitEnvironment(
    eSecurityProviderType: DRMSECURITYPROVIDERTYPE,
    eSpecification: DRMSPECTYPE,
    wszSecurityProvider: ?PWSTR,
    wszManifestCredentials: ?PWSTR,
    wszMachineCredentials: ?PWSTR,
    phEnv: ?*u32,
    phDefaultLibrary: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMLoadLibrary(
    hEnv: u32,
    eSpecification: DRMSPECTYPE,
    wszLibraryProvider: ?PWSTR,
    wszCredentials: ?PWSTR,
    phLibrary: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateEnablingPrincipal(
    hEnv: u32,
    hLibrary: u32,
    wszObject: ?PWSTR,
    pidPrincipal: ?*DRMID,
    wszCredentials: ?PWSTR,
    phEnablingPrincipal: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCloseHandle(
    handle: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCloseEnvironmentHandle(
    hEnv: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDuplicateHandle(
    hToCopy: u32,
    phCopy: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDuplicateEnvironmentHandle(
    hToCopy: u32,
    phCopy: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMRegisterRevocationList(
    hEnv: u32,
    wszRevocationList: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCheckSecurity(
    hEnv: u32,
    cLevel: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMRegisterContent(
    fRegister: BOOL,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMEncrypt(
    hCryptoProvider: u32,
    iPosition: u32,
    cNumInBytes: u32,
    pbInData: ?*u8,
    pcNumOutBytes: ?*u32,
    pbOutData: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDecrypt(
    hCryptoProvider: u32,
    iPosition: u32,
    cNumInBytes: u32,
    pbInData: ?*u8,
    pcNumOutBytes: ?*u32,
    pbOutData: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateBoundLicense(
    hEnv: u32,
    pParams: ?*DRMBOUNDLICENSEPARAMS,
    wszLicenseChain: ?PWSTR,
    phBoundLicense: ?*u32,
    phErrorLog: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateEnablingBitsDecryptor(
    hBoundLicense: u32,
    wszRight: ?PWSTR,
    hAuxLib: u32,
    wszAuxPlug: ?PWSTR,
    phDecryptor: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateEnablingBitsEncryptor(
    hBoundLicense: u32,
    wszRight: ?PWSTR,
    hAuxLib: u32,
    wszAuxPlug: ?PWSTR,
    phEncryptor: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMAttest(
    hEnablingPrincipal: u32,
    wszData: ?PWSTR,
    eType: DRMATTESTTYPE,
    pcAttestedBlob: ?*u32,
    wszAttestedBlob: [*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetTime(
    hEnv: u32,
    eTimerIdType: DRMTIMETYPE,
    poTimeObject: ?*SYSTEMTIME,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetInfo(
    handle: u32,
    wszAttribute: ?PWSTR,
    peEncoding: ?*DRMENCODINGTYPE,
    pcBuffer: ?*u32,
    pbBuffer: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetEnvironmentInfo(
    handle: u32,
    wszAttribute: ?PWSTR,
    peEncoding: ?*DRMENCODINGTYPE,
    pcBuffer: ?*u32,
    pbBuffer: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetProcAddress(
    hLibrary: u32,
    wszProcName: ?PWSTR,
    ppfnProcAddress: ?*?FARPROC,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetBoundLicenseObjectCount(
    hQueryRoot: u32,
    wszSubObjectType: ?PWSTR,
    pcSubObjects: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetBoundLicenseObject(
    hQueryRoot: u32,
    wszSubObjectType: ?PWSTR,
    iWhich: u32,
    phSubObject: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetBoundLicenseAttributeCount(
    hQueryRoot: u32,
    wszAttribute: ?PWSTR,
    pcAttributes: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetBoundLicenseAttribute(
    hQueryRoot: u32,
    wszAttribute: ?PWSTR,
    iWhich: u32,
    peEncoding: ?*DRMENCODINGTYPE,
    pcBuffer: ?*u32,
    pbBuffer: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateClientSession(
    pfnCallback: ?DRMCALLBACK,
    uCallbackVersion: u32,
    wszGroupIDProviderType: ?PWSTR,
    wszGroupID: ?PWSTR,
    phClient: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMIsActivated(
    hClient: u32,
    uFlags: u32,
    pActServInfo: ?*DRM_ACTSERV_INFO,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMActivate(
    hClient: u32,
    uFlags: u32,
    uLangID: u32,
    pActServInfo: ?*DRM_ACTSERV_INFO,
    pvContext: ?*c_void,
    hParentWnd: ?HWND,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetServiceLocation(
    hClient: u32,
    uServiceType: u32,
    uServiceLocation: u32,
    wszIssuanceLicense: ?PWSTR,
    puServiceURLLength: ?*u32,
    wszServiceURL: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateLicenseStorageSession(
    hEnv: u32,
    hDefaultLibrary: u32,
    hClient: u32,
    uFlags: u32,
    wszIssuanceLicense: ?PWSTR,
    phLicenseStorage: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMAddLicense(
    hLicenseStorage: u32,
    uFlags: u32,
    wszLicense: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMAcquireAdvisories(
    hLicenseStorage: u32,
    wszLicense: ?PWSTR,
    wszURL: ?PWSTR,
    pvContext: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMEnumerateLicense(
    hSession: u32,
    uFlags: u32,
    uIndex: u32,
    pfSharedFlag: ?*BOOL,
    puCertificateDataLen: ?*u32,
    wszCertificateData: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMAcquireLicense(
    hSession: u32,
    uFlags: u32,
    wszGroupIdentityCredential: ?PWSTR,
    wszRequestedRights: ?PWSTR,
    wszCustomData: ?PWSTR,
    wszURL: ?PWSTR,
    pvContext: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDeleteLicense(
    hSession: u32,
    wszLicenseId: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCloseSession(
    hSession: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDuplicateSession(
    hSessionIn: u32,
    phSessionOut: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetSecurityProvider(
    uFlags: u32,
    puTypeLen: ?*u32,
    wszType: ?[*:0]u16,
    puPathLen: ?*u32,
    wszPath: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMEncode(
    wszAlgID: ?PWSTR,
    uDataLen: u32,
    pbDecodedData: ?*u8,
    puEncodedStringLen: ?*u32,
    wszEncodedString: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDecode(
    wszAlgID: ?PWSTR,
    wszEncodedString: ?PWSTR,
    puDecodedDataLen: ?*u32,
    pbDecodedData: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMConstructCertificateChain(
    cCertificates: u32,
    rgwszCertificates: [*]?PWSTR,
    pcChain: ?*u32,
    wszChain: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMParseUnboundLicense(
    wszCertificate: ?PWSTR,
    phQueryRoot: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCloseQueryHandle(
    hQuery: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUnboundLicenseObjectCount(
    hQueryRoot: u32,
    wszSubObjectType: ?PWSTR,
    pcSubObjects: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUnboundLicenseObject(
    hQueryRoot: u32,
    wszSubObjectType: ?PWSTR,
    iIndex: u32,
    phSubQuery: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUnboundLicenseAttributeCount(
    hQueryRoot: u32,
    wszAttributeType: ?PWSTR,
    pcAttributes: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUnboundLicenseAttribute(
    hQueryRoot: u32,
    wszAttributeType: ?PWSTR,
    iWhich: u32,
    peEncoding: ?*DRMENCODINGTYPE,
    pcBuffer: ?*u32,
    pbBuffer: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetCertificateChainCount(
    wszChain: ?PWSTR,
    pcCertCount: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDeconstructCertificateChain(
    wszChain: ?PWSTR,
    iWhich: u32,
    pcCert: ?*u32,
    wszCert: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMVerify(
    wszData: ?PWSTR,
    pcAttestedData: ?*u32,
    wszAttestedData: ?[*:0]u16,
    peType: ?*DRMATTESTTYPE,
    pcPrincipal: ?*u32,
    wszPrincipal: ?[*:0]u16,
    pcManifest: ?*u32,
    wszManifest: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateUser(
    wszUserName: ?PWSTR,
    wszUserId: ?PWSTR,
    wszUserIdType: ?PWSTR,
    phUser: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateRight(
    wszRightName: ?PWSTR,
    pstFrom: ?*SYSTEMTIME,
    pstUntil: ?*SYSTEMTIME,
    cExtendedInfo: u32,
    pwszExtendedInfoName: ?[*]?PWSTR,
    pwszExtendedInfoValue: ?[*]?PWSTR,
    phRight: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMCreateIssuanceLicense(
    pstTimeFrom: ?*SYSTEMTIME,
    pstTimeUntil: ?*SYSTEMTIME,
    wszReferralInfoName: ?PWSTR,
    wszReferralInfoURL: ?PWSTR,
    hOwner: u32,
    wszIssuanceLicense: ?PWSTR,
    hBoundLicense: u32,
    phIssuanceLicense: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMAddRightWithUser(
    hIssuanceLicense: u32,
    hRight: u32,
    hUser: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMClearAllRights(
    hIssuanceLicense: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMSetMetaData(
    hIssuanceLicense: u32,
    wszContentId: ?PWSTR,
    wszContentIdType: ?PWSTR,
    wszSKUId: ?PWSTR,
    wszSKUIdType: ?PWSTR,
    wszContentType: ?PWSTR,
    wszContentName: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMSetUsagePolicy(
    hIssuanceLicense: u32,
    eUsagePolicyType: DRM_USAGEPOLICY_TYPE,
    fDelete: BOOL,
    fExclusion: BOOL,
    wszName: ?PWSTR,
    wszMinVersion: ?PWSTR,
    wszMaxVersion: ?PWSTR,
    wszPublicKey: ?PWSTR,
    wszDigestAlgorithm: ?PWSTR,
    pbDigest: ?*u8,
    cbDigest: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMSetRevocationPoint(
    hIssuanceLicense: u32,
    fDelete: BOOL,
    wszId: ?PWSTR,
    wszIdType: ?PWSTR,
    wszURL: ?PWSTR,
    pstFrequency: ?*SYSTEMTIME,
    wszName: ?PWSTR,
    wszPublicKey: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMSetApplicationSpecificData(
    hIssuanceLicense: u32,
    fDelete: BOOL,
    wszName: ?PWSTR,
    wszValue: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMSetNameAndDescription(
    hIssuanceLicense: u32,
    fDelete: BOOL,
    lcid: u32,
    wszName: ?PWSTR,
    wszDescription: ?PWSTR,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMSetIntervalTime(
    hIssuanceLicense: u32,
    cDays: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetIssuanceLicenseTemplate(
    hIssuanceLicense: u32,
    puIssuanceLicenseTemplateLength: ?*u32,
    wszIssuanceLicenseTemplate: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetSignedIssuanceLicense(
    hEnv: u32,
    hIssuanceLicense: u32,
    uFlags: u32,
    pbSymKey: ?*u8,
    cbSymKey: u32,
    wszSymKeyType: ?PWSTR,
    wszClientLicensorCertificate: ?PWSTR,
    pfnCallback: ?DRMCALLBACK,
    wszURL: ?PWSTR,
    pvContext: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

// TODO: this type is limited to platform 'windows6.1'
pub extern "msdrm" fn DRMGetSignedIssuanceLicenseEx(
    hEnv: u32,
    hIssuanceLicense: u32,
    uFlags: u32,
    // TODO: what to do with BytesParamIndex 4?
    pbSymKey: ?*u8,
    cbSymKey: u32,
    wszSymKeyType: ?PWSTR,
    pvReserved: ?*c_void,
    hEnablingPrincipal: u32,
    hBoundLicenseCLC: u32,
    pfnCallback: ?DRMCALLBACK,
    pvContext: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMClosePubHandle(
    hPub: u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMDuplicatePubHandle(
    hPubIn: u32,
    phPubOut: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUserInfo(
    hUser: u32,
    puUserNameLength: ?*u32,
    wszUserName: ?[*:0]u16,
    puUserIdLength: ?*u32,
    wszUserId: ?[*:0]u16,
    puUserIdTypeLength: ?*u32,
    wszUserIdType: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetRightInfo(
    hRight: u32,
    puRightNameLength: ?*u32,
    wszRightName: ?[*:0]u16,
    pstFrom: ?*SYSTEMTIME,
    pstUntil: ?*SYSTEMTIME,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetRightExtendedInfo(
    hRight: u32,
    uIndex: u32,
    puExtendedInfoNameLength: ?*u32,
    wszExtendedInfoName: ?[*:0]u16,
    puExtendedInfoValueLength: ?*u32,
    wszExtendedInfoValue: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUsers(
    hIssuanceLicense: u32,
    uIndex: u32,
    phUser: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUserRights(
    hIssuanceLicense: u32,
    hUser: u32,
    uIndex: u32,
    phRight: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetMetaData(
    hIssuanceLicense: u32,
    puContentIdLength: ?*u32,
    wszContentId: ?[*:0]u16,
    puContentIdTypeLength: ?*u32,
    wszContentIdType: ?[*:0]u16,
    puSKUIdLength: ?*u32,
    wszSKUId: ?[*:0]u16,
    puSKUIdTypeLength: ?*u32,
    wszSKUIdType: ?[*:0]u16,
    puContentTypeLength: ?*u32,
    wszContentType: ?[*:0]u16,
    puContentNameLength: ?*u32,
    wszContentName: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetApplicationSpecificData(
    hIssuanceLicense: u32,
    uIndex: u32,
    puNameLength: ?*u32,
    wszName: ?[*:0]u16,
    puValueLength: ?*u32,
    wszValue: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetIssuanceLicenseInfo(
    hIssuanceLicense: u32,
    pstTimeFrom: ?*SYSTEMTIME,
    pstTimeUntil: ?*SYSTEMTIME,
    uFlags: u32,
    puDistributionPointNameLength: ?*u32,
    wszDistributionPointName: ?[*:0]u16,
    puDistributionPointURLLength: ?*u32,
    wszDistributionPointURL: ?[*:0]u16,
    phOwner: ?*u32,
    pfOfficial: ?*BOOL,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetRevocationPoint(
    hIssuanceLicense: u32,
    puIdLength: ?*u32,
    wszId: ?[*:0]u16,
    puIdTypeLength: ?*u32,
    wszIdType: ?[*:0]u16,
    puURLLength: ?*u32,
    wszRL: ?[*:0]u16,
    pstFrequency: ?*SYSTEMTIME,
    puNameLength: ?*u32,
    wszName: ?[*:0]u16,
    puPublicKeyLength: ?*u32,
    wszPublicKey: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetUsagePolicy(
    hIssuanceLicense: u32,
    uIndex: u32,
    peUsagePolicyType: ?*DRM_USAGEPOLICY_TYPE,
    pfExclusion: ?*BOOL,
    puNameLength: ?*u32,
    wszName: ?[*:0]u16,
    puMinVersionLength: ?*u32,
    wszMinVersion: ?[*:0]u16,
    puMaxVersionLength: ?*u32,
    wszMaxVersion: ?[*:0]u16,
    puPublicKeyLength: ?*u32,
    wszPublicKey: ?[*:0]u16,
    puDigestAlgorithmLength: ?*u32,
    wszDigestAlgorithm: ?[*:0]u16,
    pcbDigest: ?*u32,
    pbDigest: ?*u8,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetNameAndDescription(
    hIssuanceLicense: u32,
    uIndex: u32,
    pulcid: ?*u32,
    puNameLength: ?*u32,
    wszName: ?[*:0]u16,
    puDescriptionLength: ?*u32,
    wszDescription: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetOwnerLicense(
    hIssuanceLicense: u32,
    puOwnerLicenseLength: ?*u32,
    wszOwnerLicense: ?[*:0]u16,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMGetIntervalTime(
    hIssuanceLicense: u32,
    pcDays: ?*u32,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

pub extern "msdrm" fn DRMRepair(
) callconv(@import("std").os.windows.WINAPI) HRESULT;

// TODO: this type is limited to platform 'windows6.0.6000'
pub extern "msdrm" fn DRMRegisterProtectedWindow(
    hEnv: u32,
    hwnd: ?HWND,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

// TODO: this type is limited to platform 'windows6.0.6000'
pub extern "msdrm" fn DRMIsWindowProtected(
    hwnd: ?HWND,
    pfProtected: ?*BOOL,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

// TODO: this type is limited to platform 'windows6.0.6000'
pub extern "msdrm" fn DRMAcquireIssuanceLicenseTemplate(
    hClient: u32,
    uFlags: u32,
    pvReserved: ?*c_void,
    cTemplates: u32,
    pwszTemplateIds: ?[*]?PWSTR,
    wszUrl: ?PWSTR,
    pvContext: ?*c_void,
) callconv(@import("std").os.windows.WINAPI) HRESULT;


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
// Section: Imports (6)
//--------------------------------------------------------------------------------
const BOOL = @import("../foundation.zig").BOOL;
const FARPROC = @import("../foundation.zig").FARPROC;
const HRESULT = @import("../foundation.zig").HRESULT;
const HWND = @import("../foundation.zig").HWND;
const PWSTR = @import("../foundation.zig").PWSTR;
const SYSTEMTIME = @import("../foundation.zig").SYSTEMTIME;

test {
    // The following '_ = <FuncPtrType>' lines are a workaround for https://github.com/ziglang/zig/issues/4476
    if (@hasDecl(@This(), "DRMCALLBACK")) { _ = DRMCALLBACK; }

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