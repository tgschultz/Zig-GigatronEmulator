//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (51)
//--------------------------------------------------------------------------------
pub const WINTRUST_MAX_HEADER_BYTES_TO_MAP_DEFAULT = @as(u32, 10485760);
pub const WINTRUST_MAX_HASH_BYTES_TO_MAP_DEFAULT = @as(u32, 1048576);
pub const WSS_VERIFY_SEALING = @as(u32, 4);
pub const WSS_INPUT_FLAG_MASK = @as(u32, 7);
pub const WSS_OUT_SEALING_STATUS_VERIFIED = @as(u32, 2147483648);
pub const WSS_OUT_HAS_SEALING_INTENT = @as(u32, 1073741824);
pub const WSS_OUT_FILE_SUPPORTS_SEAL = @as(u32, 536870912);
pub const WSS_OUTPUT_FLAG_MASK = @as(u32, 3758096384);
pub const TRUSTERROR_STEP_WVTPARAMS = @as(u32, 0);
pub const TRUSTERROR_STEP_FILEIO = @as(u32, 2);
pub const TRUSTERROR_STEP_SIP = @as(u32, 3);
pub const TRUSTERROR_STEP_SIPSUBJINFO = @as(u32, 5);
pub const TRUSTERROR_STEP_CATALOGFILE = @as(u32, 6);
pub const TRUSTERROR_STEP_CERTSTORE = @as(u32, 7);
pub const TRUSTERROR_STEP_MESSAGE = @as(u32, 8);
pub const TRUSTERROR_STEP_MSG_SIGNERCOUNT = @as(u32, 9);
pub const TRUSTERROR_STEP_MSG_INNERCNTTYPE = @as(u32, 10);
pub const TRUSTERROR_STEP_MSG_INNERCNT = @as(u32, 11);
pub const TRUSTERROR_STEP_MSG_STORE = @as(u32, 12);
pub const TRUSTERROR_STEP_MSG_SIGNERINFO = @as(u32, 13);
pub const TRUSTERROR_STEP_MSG_SIGNERCERT = @as(u32, 14);
pub const TRUSTERROR_STEP_MSG_CERTCHAIN = @as(u32, 15);
pub const TRUSTERROR_STEP_MSG_COUNTERSIGINFO = @as(u32, 16);
pub const TRUSTERROR_STEP_MSG_COUNTERSIGCERT = @as(u32, 17);
pub const TRUSTERROR_STEP_VERIFY_MSGHASH = @as(u32, 18);
pub const TRUSTERROR_STEP_VERIFY_MSGINDIRECTDATA = @as(u32, 19);
pub const TRUSTERROR_STEP_FINAL_WVTINIT = @as(u32, 30);
pub const TRUSTERROR_STEP_FINAL_INITPROV = @as(u32, 31);
pub const TRUSTERROR_STEP_FINAL_OBJPROV = @as(u32, 32);
pub const TRUSTERROR_STEP_FINAL_SIGPROV = @as(u32, 33);
pub const TRUSTERROR_STEP_FINAL_CERTPROV = @as(u32, 34);
pub const TRUSTERROR_STEP_FINAL_CERTCHKPROV = @as(u32, 35);
pub const TRUSTERROR_STEP_FINAL_POLICYPROV = @as(u32, 36);
pub const TRUSTERROR_STEP_FINAL_UIPROV = @as(u32, 37);
pub const TRUSTERROR_MAX_STEPS = @as(u32, 38);
pub const WSS_OBJTRUST_SUPPORT = @as(u32, 1);
pub const WSS_SIGTRUST_SUPPORT = @as(u32, 2);
pub const WSS_CERTTRUST_SUPPORT = @as(u32, 4);
pub const WT_CURRENT_VERSION = @as(u32, 512);
pub const WT_ADD_ACTION_ID_RET_RESULT_FLAG = @as(u32, 1);
pub const SPC_UUID_LENGTH = @as(u32, 16);
pub const WIN_CERT_REVISION_1_0 = @as(u32, 256);
pub const WIN_CERT_REVISION_2_0 = @as(u32, 512);
pub const WIN_CERT_TYPE_X509 = @as(u32, 1);
pub const WIN_CERT_TYPE_PKCS_SIGNED_DATA = @as(u32, 2);
pub const WIN_CERT_TYPE_RESERVED_1 = @as(u32, 3);
pub const WIN_CERT_TYPE_TS_STACK_SIGNED = @as(u32, 4);
pub const WT_TRUSTDBDIALOG_NO_UI_FLAG = @as(u32, 1);
pub const WT_TRUSTDBDIALOG_ONLY_PUB_TAB_FLAG = @as(u32, 2);
pub const WT_TRUSTDBDIALOG_WRITE_LEGACY_REG_FLAG = @as(u32, 256);
pub const WT_TRUSTDBDIALOG_WRITE_IEAK_STORE_FLAG = @as(u32, 512);

//--------------------------------------------------------------------------------
// Section: Types (75)
//--------------------------------------------------------------------------------
pub const WINTRUST_GET_DEFAULT_FOR_USAGE_ACTION = enum(u32) {
    ALLOCANDFILL = 1,
    FREE = 2,
};
pub const DWACTION_ALLOCANDFILL = WINTRUST_GET_DEFAULT_FOR_USAGE_ACTION.ALLOCANDFILL;
pub const DWACTION_FREE = WINTRUST_GET_DEFAULT_FOR_USAGE_ACTION.FREE;

pub const WINTRUST_POLICY_FLAGS = enum(u32) {
    TRUSTTEST = 32,
    TESTCANBEVALID = 128,
    IGNOREEXPIRATION = 256,
    IGNOREREVOKATION = 512,
    OFFLINEOK_IND = 1024,
    OFFLINEOK_COM = 2048,
    OFFLINEOKNBU_IND = 4096,
    OFFLINEOKNBU_COM = 8192,
    VERIFY_V1_OFF = 65536,
    IGNOREREVOCATIONONTS = 131072,
    ALLOWONLYPERTRUST = 262144,
    _,
    pub fn initFlags(o: struct {
        TRUSTTEST: u1 = 0,
        TESTCANBEVALID: u1 = 0,
        IGNOREEXPIRATION: u1 = 0,
        IGNOREREVOKATION: u1 = 0,
        OFFLINEOK_IND: u1 = 0,
        OFFLINEOK_COM: u1 = 0,
        OFFLINEOKNBU_IND: u1 = 0,
        OFFLINEOKNBU_COM: u1 = 0,
        VERIFY_V1_OFF: u1 = 0,
        IGNOREREVOCATIONONTS: u1 = 0,
        ALLOWONLYPERTRUST: u1 = 0,
    }) WINTRUST_POLICY_FLAGS {
        return @intToEnum(WINTRUST_POLICY_FLAGS,
              (if (o.TRUSTTEST == 1) @enumToInt(WINTRUST_POLICY_FLAGS.TRUSTTEST) else 0)
            | (if (o.TESTCANBEVALID == 1) @enumToInt(WINTRUST_POLICY_FLAGS.TESTCANBEVALID) else 0)
            | (if (o.IGNOREEXPIRATION == 1) @enumToInt(WINTRUST_POLICY_FLAGS.IGNOREEXPIRATION) else 0)
            | (if (o.IGNOREREVOKATION == 1) @enumToInt(WINTRUST_POLICY_FLAGS.IGNOREREVOKATION) else 0)
            | (if (o.OFFLINEOK_IND == 1) @enumToInt(WINTRUST_POLICY_FLAGS.OFFLINEOK_IND) else 0)
            | (if (o.OFFLINEOK_COM == 1) @enumToInt(WINTRUST_POLICY_FLAGS.OFFLINEOK_COM) else 0)
            | (if (o.OFFLINEOKNBU_IND == 1) @enumToInt(WINTRUST_POLICY_FLAGS.OFFLINEOKNBU_IND) else 0)
            | (if (o.OFFLINEOKNBU_COM == 1) @enumToInt(WINTRUST_POLICY_FLAGS.OFFLINEOKNBU_COM) else 0)
            | (if (o.VERIFY_V1_OFF == 1) @enumToInt(WINTRUST_POLICY_FLAGS.VERIFY_V1_OFF) else 0)
            | (if (o.IGNOREREVOCATIONONTS == 1) @enumToInt(WINTRUST_POLICY_FLAGS.IGNOREREVOCATIONONTS) else 0)
            | (if (o.ALLOWONLYPERTRUST == 1) @enumToInt(WINTRUST_POLICY_FLAGS.ALLOWONLYPERTRUST) else 0)
        );
    }
};
pub const WTPF_TRUSTTEST = WINTRUST_POLICY_FLAGS.TRUSTTEST;
pub const WTPF_TESTCANBEVALID = WINTRUST_POLICY_FLAGS.TESTCANBEVALID;
pub const WTPF_IGNOREEXPIRATION = WINTRUST_POLICY_FLAGS.IGNOREEXPIRATION;
pub const WTPF_IGNOREREVOKATION = WINTRUST_POLICY_FLAGS.IGNOREREVOKATION;
pub const WTPF_OFFLINEOK_IND = WINTRUST_POLICY_FLAGS.OFFLINEOK_IND;
pub const WTPF_OFFLINEOK_COM = WINTRUST_POLICY_FLAGS.OFFLINEOK_COM;
pub const WTPF_OFFLINEOKNBU_IND = WINTRUST_POLICY_FLAGS.OFFLINEOKNBU_IND;
pub const WTPF_OFFLINEOKNBU_COM = WINTRUST_POLICY_FLAGS.OFFLINEOKNBU_COM;
pub const WTPF_VERIFY_V1_OFF = WINTRUST_POLICY_FLAGS.VERIFY_V1_OFF;
pub const WTPF_IGNOREREVOCATIONONTS = WINTRUST_POLICY_FLAGS.IGNOREREVOCATIONONTS;
pub const WTPF_ALLOWONLYPERTRUST = WINTRUST_POLICY_FLAGS.ALLOWONLYPERTRUST;

pub const WINTRUST_DATA_UICHOICE = enum(u32) {
    ALL = 1,
    NONE = 2,
    NOBAD = 3,
    NOGOOD = 4,
};
pub const WTD_UI_ALL = WINTRUST_DATA_UICHOICE.ALL;
pub const WTD_UI_NONE = WINTRUST_DATA_UICHOICE.NONE;
pub const WTD_UI_NOBAD = WINTRUST_DATA_UICHOICE.NOBAD;
pub const WTD_UI_NOGOOD = WINTRUST_DATA_UICHOICE.NOGOOD;

pub const WINTRUST_SIGNATURE_SETTINGS_FLAGS = enum(u32) {
    VERIFY_SPECIFIC = 1,
    GET_SECONDARY_SIG_COUNT = 2,
};
pub const WSS_VERIFY_SPECIFIC = WINTRUST_SIGNATURE_SETTINGS_FLAGS.VERIFY_SPECIFIC;
pub const WSS_GET_SECONDARY_SIG_COUNT = WINTRUST_SIGNATURE_SETTINGS_FLAGS.GET_SECONDARY_SIG_COUNT;

pub const WINTRUST_DATA_STATE_ACTION = enum(u32) {
    IGNORE = 0,
    VERIFY = 1,
    CLOSE = 2,
    AUTO_CACHE = 3,
    AUTO_CACHE_FLUSH = 4,
};
pub const WTD_STATEACTION_IGNORE = WINTRUST_DATA_STATE_ACTION.IGNORE;
pub const WTD_STATEACTION_VERIFY = WINTRUST_DATA_STATE_ACTION.VERIFY;
pub const WTD_STATEACTION_CLOSE = WINTRUST_DATA_STATE_ACTION.CLOSE;
pub const WTD_STATEACTION_AUTO_CACHE = WINTRUST_DATA_STATE_ACTION.AUTO_CACHE;
pub const WTD_STATEACTION_AUTO_CACHE_FLUSH = WINTRUST_DATA_STATE_ACTION.AUTO_CACHE_FLUSH;

pub const WINTRUST_DATA_UNION_CHOICE = enum(u32) {
    FILE = 1,
    CATALOG = 2,
    BLOB = 3,
    SIGNER = 4,
    CERT = 5,
};
pub const WTD_CHOICE_FILE = WINTRUST_DATA_UNION_CHOICE.FILE;
pub const WTD_CHOICE_CATALOG = WINTRUST_DATA_UNION_CHOICE.CATALOG;
pub const WTD_CHOICE_BLOB = WINTRUST_DATA_UNION_CHOICE.BLOB;
pub const WTD_CHOICE_SIGNER = WINTRUST_DATA_UNION_CHOICE.SIGNER;
pub const WTD_CHOICE_CERT = WINTRUST_DATA_UNION_CHOICE.CERT;

pub const WINTRUST_DATA_REVOCATION_CHECKS = enum(u32) {
    NONE = 0,
    WHOLECHAIN = 1,
};
pub const WTD_REVOKE_NONE = WINTRUST_DATA_REVOCATION_CHECKS.NONE;
pub const WTD_REVOKE_WHOLECHAIN = WINTRUST_DATA_REVOCATION_CHECKS.WHOLECHAIN;

pub const WINTRUST_DATA_UICONTEXT = enum(u32) {
    EXECUTE = 0,
    INSTALL = 1,
};
pub const WTD_UICONTEXT_EXECUTE = WINTRUST_DATA_UICONTEXT.EXECUTE;
pub const WTD_UICONTEXT_INSTALL = WINTRUST_DATA_UICONTEXT.INSTALL;

pub const WINTRUST_DATA = extern struct {
    cbStruct: u32,
    pPolicyCallbackData: ?*anyopaque,
    pSIPClientData: ?*anyopaque,
    dwUIChoice: WINTRUST_DATA_UICHOICE,
    fdwRevocationChecks: WINTRUST_DATA_REVOCATION_CHECKS,
    dwUnionChoice: WINTRUST_DATA_UNION_CHOICE,
    Anonymous: extern union {
        pFile: ?*WINTRUST_FILE_INFO,
        pCatalog: ?*WINTRUST_CATALOG_INFO,
        pBlob: ?*WINTRUST_BLOB_INFO,
        pSgnr: ?*WINTRUST_SGNR_INFO,
        pCert: ?*WINTRUST_CERT_INFO,
    },
    dwStateAction: WINTRUST_DATA_STATE_ACTION,
    hWVTStateData: ?HANDLE,
    pwszURLReference: ?PWSTR,
    dwProvFlags: u32,
    dwUIContext: WINTRUST_DATA_UICONTEXT,
    pSignatureSettings: ?*WINTRUST_SIGNATURE_SETTINGS,
};

pub const WINTRUST_SIGNATURE_SETTINGS = extern struct {
    cbStruct: u32,
    dwIndex: u32,
    dwFlags: WINTRUST_SIGNATURE_SETTINGS_FLAGS,
    cSecondarySigs: u32,
    dwVerifiedSigIndex: u32,
    pCryptoPolicy: ?*CERT_STRONG_SIGN_PARA,
};

pub const WINTRUST_FILE_INFO = extern struct {
    cbStruct: u32,
    pcwszFilePath: ?[*:0]const u16,
    hFile: ?HANDLE,
    pgKnownSubject: ?*Guid,
};

pub const WINTRUST_CATALOG_INFO = extern struct {
    cbStruct: u32,
    dwCatalogVersion: u32,
    pcwszCatalogFilePath: ?[*:0]const u16,
    pcwszMemberTag: ?[*:0]const u16,
    pcwszMemberFilePath: ?[*:0]const u16,
    hMemberFile: ?HANDLE,
    pbCalculatedFileHash: ?*u8,
    cbCalculatedFileHash: u32,
    pcCatalogContext: ?*CTL_CONTEXT,
    hCatAdmin: isize,
};

pub const WINTRUST_BLOB_INFO = extern struct {
    cbStruct: u32,
    gSubject: Guid,
    pcwszDisplayName: ?[*:0]const u16,
    cbMemObject: u32,
    pbMemObject: ?*u8,
    cbMemSignedMsg: u32,
    pbMemSignedMsg: ?*u8,
};

pub const WINTRUST_SGNR_INFO = extern struct {
    cbStruct: u32,
    pcwszDisplayName: ?[*:0]const u16,
    psSignerInfo: ?*CMSG_SIGNER_INFO,
    chStores: u32,
    pahStores: ?*?*anyopaque,
};

pub const WINTRUST_CERT_INFO = extern struct {
    cbStruct: u32,
    pcwszDisplayName: ?[*:0]const u16,
    psCertContext: ?*CERT_CONTEXT,
    chStores: u32,
    pahStores: ?*?*anyopaque,
    dwFlags: u32,
    psftVerifyAsOf: ?*FILETIME,
};

pub const PFN_CPD_MEM_ALLOC = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        cbSize: u32,
    ) callconv(@import("std").os.windows.WINAPI) ?*anyopaque,
    else => *const fn(
        cbSize: u32,
    ) callconv(@import("std").os.windows.WINAPI) ?*anyopaque,
} ;

pub const PFN_CPD_MEM_FREE = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pvMem2Free: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) void,
    else => *const fn(
        pvMem2Free: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) void,
} ;

pub const PFN_CPD_ADD_STORE = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        hStore2Add: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        hStore2Add: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const PFN_CPD_ADD_SGNR = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        fCounterSigner: BOOL,
        idxSigner: u32,
        pSgnr2Add: ?*CRYPT_PROVIDER_SGNR,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        fCounterSigner: BOOL,
        idxSigner: u32,
        pSgnr2Add: ?*CRYPT_PROVIDER_SGNR,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const PFN_CPD_ADD_CERT = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        idxSigner: u32,
        fCounterSigner: BOOL,
        idxCounterSigner: u32,
        pCert2Add: ?*const CERT_CONTEXT,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        idxSigner: u32,
        fCounterSigner: BOOL,
        idxCounterSigner: u32,
        pCert2Add: ?*const CERT_CONTEXT,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const PFN_CPD_ADD_PRIVDATA = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        pPrivData2Add: ?*CRYPT_PROVIDER_PRIVDATA,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        pPrivData2Add: ?*CRYPT_PROVIDER_PRIVDATA,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const PFN_PROVIDER_INIT_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_OBJTRUST_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_SIGTRUST_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_CERTTRUST_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_FINALPOLICY_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_TESTFINALPOLICY_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_CLEANUP_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const PFN_PROVIDER_CERTCHKPOLICY_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        idxSigner: u32,
        fCounterSignerChain: BOOL,
        idxCounterSigner: u32,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        idxSigner: u32,
        fCounterSignerChain: BOOL,
        idxCounterSigner: u32,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const CRYPT_PROVIDER_DATA = extern struct {
    cbStruct: u32,
    pWintrustData: ?*WINTRUST_DATA,
    fOpenedFile: BOOL,
    hWndParent: ?HWND,
    pgActionID: ?*Guid,
    hProv: usize,
    dwError: u32,
    dwRegSecuritySettings: u32,
    dwRegPolicySettings: u32,
    psPfns: ?*CRYPT_PROVIDER_FUNCTIONS,
    cdwTrustStepErrors: u32,
    padwTrustStepErrors: ?*u32,
    chStores: u32,
    pahStores: ?*?*anyopaque,
    dwEncoding: u32,
    hMsg: ?*anyopaque,
    csSigners: u32,
    pasSigners: ?*CRYPT_PROVIDER_SGNR,
    csProvPrivData: u32,
    pasProvPrivData: ?*CRYPT_PROVIDER_PRIVDATA,
    dwSubjectChoice: u32,
    Anonymous: extern union {
        pPDSip: ?*PROVDATA_SIP,
    },
    pszUsageOID: ?PSTR,
    fRecallWithState: BOOL,
    sftSystemTime: FILETIME,
    pszCTLSignerUsageOID: ?PSTR,
    dwProvFlags: u32,
    dwFinalError: u32,
    pRequestUsage: ?*CERT_USAGE_MATCH,
    dwTrustPubSettings: u32,
    dwUIStateFlags: u32,
    pSigState: ?*CRYPT_PROVIDER_SIGSTATE,
    pSigSettings: ?*WINTRUST_SIGNATURE_SETTINGS,
};

pub const CRYPT_PROVIDER_SIGSTATE = extern struct {
    cbStruct: u32,
    rhSecondarySigs: ?*?*anyopaque,
    hPrimarySig: ?*anyopaque,
    fFirstAttemptMade: BOOL,
    fNoMoreSigs: BOOL,
    cSecondarySigs: u32,
    dwCurrentIndex: u32,
    fSupportMultiSig: BOOL,
    dwCryptoPolicySupport: u32,
    iAttemptCount: u32,
    fCheckedSealing: BOOL,
    pSealingSignature: ?*SEALING_SIGNATURE_ATTRIBUTE,
};

pub const CRYPT_PROVIDER_FUNCTIONS = extern struct {
    cbStruct: u32,
    pfnAlloc: ?PFN_CPD_MEM_ALLOC,
    pfnFree: ?PFN_CPD_MEM_FREE,
    pfnAddStore2Chain: ?PFN_CPD_ADD_STORE,
    pfnAddSgnr2Chain: ?PFN_CPD_ADD_SGNR,
    pfnAddCert2Chain: ?PFN_CPD_ADD_CERT,
    pfnAddPrivData2Chain: ?PFN_CPD_ADD_PRIVDATA,
    pfnInitialize: ?PFN_PROVIDER_INIT_CALL,
    pfnObjectTrust: ?PFN_PROVIDER_OBJTRUST_CALL,
    pfnSignatureTrust: ?PFN_PROVIDER_SIGTRUST_CALL,
    pfnCertificateTrust: ?PFN_PROVIDER_CERTTRUST_CALL,
    pfnFinalPolicy: ?PFN_PROVIDER_FINALPOLICY_CALL,
    pfnCertCheckPolicy: ?PFN_PROVIDER_CERTCHKPOLICY_CALL,
    pfnTestFinalPolicy: ?PFN_PROVIDER_TESTFINALPOLICY_CALL,
    psUIpfns: ?*CRYPT_PROVUI_FUNCS,
    pfnCleanupPolicy: ?PFN_PROVIDER_CLEANUP_CALL,
};

pub const PFN_PROVUI_CALL = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        hWndSecurityDialog: ?HWND,
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        hWndSecurityDialog: ?HWND,
        pProvData: ?*CRYPT_PROVIDER_DATA,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const CRYPT_PROVUI_FUNCS = extern struct {
    cbStruct: u32,
    psUIData: ?*CRYPT_PROVUI_DATA,
    pfnOnMoreInfoClick: ?PFN_PROVUI_CALL,
    pfnOnMoreInfoClickDefault: ?PFN_PROVUI_CALL,
    pfnOnAdvancedClick: ?PFN_PROVUI_CALL,
    pfnOnAdvancedClickDefault: ?PFN_PROVUI_CALL,
};

pub const CRYPT_PROVUI_DATA = extern struct {
    cbStruct: u32,
    dwFinalError: u32,
    pYesButtonText: ?PWSTR,
    pNoButtonText: ?PWSTR,
    pMoreInfoButtonText: ?PWSTR,
    pAdvancedLinkText: ?PWSTR,
    pCopyActionText: ?PWSTR,
    pCopyActionTextNoTS: ?PWSTR,
    pCopyActionTextNotSigned: ?PWSTR,
};

pub const CRYPT_PROVIDER_SGNR = extern struct {
    cbStruct: u32,
    sftVerifyAsOf: FILETIME,
    csCertChain: u32,
    pasCertChain: ?*CRYPT_PROVIDER_CERT,
    dwSignerType: u32,
    psSigner: ?*CMSG_SIGNER_INFO,
    dwError: u32,
    csCounterSigners: u32,
    pasCounterSigners: ?*CRYPT_PROVIDER_SGNR,
    pChainContext: ?*CERT_CHAIN_CONTEXT,
};

pub const CRYPT_PROVIDER_CERT = extern struct {
    cbStruct: u32,
    pCert: ?*const CERT_CONTEXT,
    fCommercial: BOOL,
    fTrustedRoot: BOOL,
    fSelfSigned: BOOL,
    fTestCert: BOOL,
    dwRevokedReason: u32,
    dwConfidence: u32,
    dwError: u32,
    pTrustListContext: ?*CTL_CONTEXT,
    fTrustListSignerCert: BOOL,
    pCtlContext: ?*CTL_CONTEXT,
    dwCtlError: u32,
    fIsCyclic: BOOL,
    pChainElement: ?*CERT_CHAIN_ELEMENT,
};

pub const CRYPT_PROVIDER_PRIVDATA = extern struct {
    cbStruct: u32,
    gProviderID: Guid,
    cbProvData: u32,
    pvProvData: ?*anyopaque,
};

pub const PROVDATA_SIP = extern struct {
    cbStruct: u32,
    gSubject: Guid,
    pSip: ?*SIP_DISPATCH_INFO,
    pCATSip: ?*SIP_DISPATCH_INFO,
    psSipSubjectInfo: ?*SIP_SUBJECTINFO,
    psSipCATSubjectInfo: ?*SIP_SUBJECTINFO,
    psIndirectData: ?*SIP_INDIRECT_DATA,
};

pub const CRYPT_TRUST_REG_ENTRY = extern struct {
    cbStruct: u32,
    pwszDLLName: ?PWSTR,
    pwszFunctionName: ?PWSTR,
};

pub const CRYPT_REGISTER_ACTIONID = extern struct {
    cbStruct: u32,
    sInitProvider: CRYPT_TRUST_REG_ENTRY,
    sObjectProvider: CRYPT_TRUST_REG_ENTRY,
    sSignatureProvider: CRYPT_TRUST_REG_ENTRY,
    sCertificateProvider: CRYPT_TRUST_REG_ENTRY,
    sCertificatePolicyProvider: CRYPT_TRUST_REG_ENTRY,
    sFinalPolicyProvider: CRYPT_TRUST_REG_ENTRY,
    sTestPolicyProvider: CRYPT_TRUST_REG_ENTRY,
    sCleanupProvider: CRYPT_TRUST_REG_ENTRY,
};

pub const PFN_ALLOCANDFILLDEFUSAGE = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pszUsageOID: ?[*:0]const u8,
        psDefUsage: ?*CRYPT_PROVIDER_DEFUSAGE,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pszUsageOID: ?[*:0]const u8,
        psDefUsage: ?*CRYPT_PROVIDER_DEFUSAGE,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const PFN_FREEDEFUSAGE = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pszUsageOID: ?[*:0]const u8,
        psDefUsage: ?*CRYPT_PROVIDER_DEFUSAGE,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
    else => *const fn(
        pszUsageOID: ?[*:0]const u8,
        psDefUsage: ?*CRYPT_PROVIDER_DEFUSAGE,
    ) callconv(@import("std").os.windows.WINAPI) BOOL,
} ;

pub const CRYPT_PROVIDER_REGDEFUSAGE = extern struct {
    cbStruct: u32,
    pgActionID: ?*Guid,
    pwszDllName: ?PWSTR,
    pwszLoadCallbackDataFunctionName: ?PSTR,
    pwszFreeCallbackDataFunctionName: ?PSTR,
};

pub const CRYPT_PROVIDER_DEFUSAGE = extern struct {
    cbStruct: u32,
    gActionID: Guid,
    pDefPolicyCallbackData: ?*anyopaque,
    pDefSIPClientData: ?*anyopaque,
};

pub const SPC_SERIALIZED_OBJECT = extern struct {
    ClassId: [16]u8,
    SerializedData: CRYPTOAPI_BLOB,
};

pub const SPC_SIGINFO = extern struct {
    dwSipVersion: u32,
    gSIPGuid: Guid,
    dwReserved1: u32,
    dwReserved2: u32,
    dwReserved3: u32,
    dwReserved4: u32,
    dwReserved5: u32,
};

pub const SPC_LINK = extern struct {
    dwLinkChoice: u32,
    Anonymous: extern union {
        pwszUrl: ?PWSTR,
        Moniker: SPC_SERIALIZED_OBJECT,
        pwszFile: ?PWSTR,
    },
};

pub const SPC_PE_IMAGE_DATA = extern struct {
    Flags: CRYPT_BIT_BLOB,
    pFile: ?*SPC_LINK,
};

pub const SPC_INDIRECT_DATA_CONTENT = extern struct {
    Data: CRYPT_ATTRIBUTE_TYPE_VALUE,
    DigestAlgorithm: CRYPT_ALGORITHM_IDENTIFIER,
    Digest: CRYPTOAPI_BLOB,
};

pub const SPC_FINANCIAL_CRITERIA = extern struct {
    fFinancialInfoAvailable: BOOL,
    fMeetsCriteria: BOOL,
};

pub const SPC_IMAGE = extern struct {
    pImageLink: ?*SPC_LINK,
    Bitmap: CRYPTOAPI_BLOB,
    Metafile: CRYPTOAPI_BLOB,
    EnhancedMetafile: CRYPTOAPI_BLOB,
    GifFile: CRYPTOAPI_BLOB,
};

pub const SPC_SP_AGENCY_INFO = extern struct {
    pPolicyInformation: ?*SPC_LINK,
    pwszPolicyDisplayText: ?PWSTR,
    pLogoImage: ?*SPC_IMAGE,
    pLogoLink: ?*SPC_LINK,
};

pub const SPC_STATEMENT_TYPE = extern struct {
    cKeyPurposeId: u32,
    rgpszKeyPurposeId: ?*?PSTR,
};

pub const SPC_SP_OPUS_INFO = extern struct {
    pwszProgramName: ?[*:0]const u16,
    pMoreInfo: ?*SPC_LINK,
    pPublisherInfo: ?*SPC_LINK,
};

pub const CAT_NAMEVALUE = extern struct {
    pwszTag: ?PWSTR,
    fdwFlags: u32,
    Value: CRYPTOAPI_BLOB,
};

pub const CAT_MEMBERINFO = extern struct {
    pwszSubjGuid: ?PWSTR,
    dwCertVersion: u32,
};

pub const CAT_MEMBERINFO2 = extern struct {
    SubjectGuid: Guid,
    dwCertVersion: u32,
};

pub const INTENT_TO_SEAL_ATTRIBUTE = extern struct {
    version: u32,
    seal: BOOLEAN,
};

pub const SEALING_SIGNATURE_ATTRIBUTE = extern struct {
    version: u32,
    signerIndex: u32,
    signatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER,
    encryptedDigest: CRYPTOAPI_BLOB,
};

pub const SEALING_TIMESTAMP_ATTRIBUTE = extern struct {
    version: u32,
    signerIndex: u32,
    sealTimeStampToken: CRYPTOAPI_BLOB,
};

pub const WIN_CERTIFICATE = extern struct {
    dwLength: u32,
    wRevision: u16,
    wCertificateType: u16,
    bCertificate: [1]u8,
};

pub const WIN_TRUST_ACTDATA_CONTEXT_WITH_SUBJECT = extern struct {
    hClientToken: ?HANDLE,
    SubjectType: ?*Guid,
    Subject: ?*anyopaque,
};

pub const WIN_TRUST_ACTDATA_SUBJECT_ONLY = extern struct {
    SubjectType: ?*Guid,
    Subject: ?*anyopaque,
};

pub const WIN_TRUST_SUBJECT_FILE = extern struct {
    hFile: ?HANDLE,
    lpPath: ?[*:0]const u16,
};

pub const WIN_TRUST_SUBJECT_FILE_AND_DISPLAY = extern struct {
    hFile: ?HANDLE,
    lpPath: ?[*:0]const u16,
    lpDisplayName: ?[*:0]const u16,
};

pub const WIN_SPUB_TRUSTED_PUBLISHER_DATA = extern struct {
    hClientToken: ?HANDLE,
    lpCertificate: ?*WIN_CERTIFICATE,
};

pub const WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO = extern struct {
    Anonymous: extern union {
        cbStruct: u32,
        cbSize: u32,
    },
    pChainContext: ?*CERT_CHAIN_CONTEXT,
    dwSignerType: u32,
    pMsgSignerInfo: ?*CMSG_SIGNER_INFO,
    dwError: u32,
    cCounterSigner: u32,
    rgpCounterSigner: ?*?*WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO,
};

pub const PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK = switch (@import("builtin").zig_backend) {
    .stage1 => fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        dwStepError: u32,
        dwRegPolicySettings: u32,
        cSigner: u32,
        rgpSigner: ?*?*WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO,
        pvPolicyArg: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    else => *const fn(
        pProvData: ?*CRYPT_PROVIDER_DATA,
        dwStepError: u32,
        dwRegPolicySettings: u32,
        cSigner: u32,
        rgpSigner: ?*?*WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO,
        pvPolicyArg: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) HRESULT,
} ;

pub const WTD_GENERIC_CHAIN_POLICY_CREATE_INFO = extern struct {
    Anonymous: extern union {
        cbStruct: u32,
        cbSize: u32,
    },
    hChainEngine: ?HCERTCHAINENGINE,
    pChainPara: ?*CERT_CHAIN_PARA,
    dwFlags: u32,
    pvReserved: ?*anyopaque,
};

pub const WTD_GENERIC_CHAIN_POLICY_DATA = extern struct {
    Anonymous: extern union {
        cbStruct: u32,
        cbSize: u32,
    },
    pSignerChainInfo: ?*WTD_GENERIC_CHAIN_POLICY_CREATE_INFO,
    pCounterSignerChainInfo: ?*WTD_GENERIC_CHAIN_POLICY_CREATE_INFO,
    pfnPolicyCallback: ?PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK,
    pvPolicyArg: ?*anyopaque,
};

pub const DRIVER_VER_MAJORMINOR = extern struct {
    dwMajor: u32,
    dwMinor: u32,
};

pub const DRIVER_VER_INFO = extern struct {
    cbStruct: u32,
    dwReserved1: usize,
    dwReserved2: usize,
    dwPlatform: u32,
    dwVersion: u32,
    wszVersion: [260]u16,
    wszSignedBy: [260]u16,
    pcSignerCertContext: ?*const CERT_CONTEXT,
    sOSVersionLow: DRIVER_VER_MAJORMINOR,
    sOSVersionHigh: DRIVER_VER_MAJORMINOR,
    dwBuildNumberLow: u32,
    dwBuildNumberHigh: u32,
};

pub const CONFIG_CI_PROV_INFO_RESULT = extern struct {
    hr: HRESULT,
    dwResult: u32,
    dwPolicyIndex: u32,
    fIsExplicitDeny: BOOLEAN,
};

pub const CONFIG_CI_PROV_INFO = extern struct {
    cbSize: u32,
    dwPolicies: u32,
    pPolicies: ?*CRYPTOAPI_BLOB,
    result: CONFIG_CI_PROV_INFO_RESULT,
    dwScenario: u32,
};


//--------------------------------------------------------------------------------
// Section: Functions (18)
//--------------------------------------------------------------------------------
// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WinVerifyTrust(
    hwnd: ?HWND,
    pgActionID: ?*Guid,
    pWVTData: ?*anyopaque,
) callconv(@import("std").os.windows.WINAPI) i32;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WinVerifyTrustEx(
    hwnd: ?HWND,
    pgActionID: ?*Guid,
    pWinTrustData: ?*WINTRUST_DATA,
) callconv(@import("std").os.windows.WINAPI) i32;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustGetRegPolicyFlags(
    pdwPolicyFlags: ?*WINTRUST_POLICY_FLAGS,
) callconv(@import("std").os.windows.WINAPI) void;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustSetRegPolicyFlags(
    dwPolicyFlags: WINTRUST_POLICY_FLAGS,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustAddActionID(
    pgActionID: ?*Guid,
    fdwFlags: u32,
    psProvInfo: ?*CRYPT_REGISTER_ACTIONID,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustRemoveActionID(
    pgActionID: ?*Guid,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustLoadFunctionPointers(
    pgActionID: ?*Guid,
    pPfns: ?*CRYPT_PROVIDER_FUNCTIONS,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustAddDefaultForUsage(
    pszUsageOID: ?[*:0]const u8,
    psDefUsage: ?*CRYPT_PROVIDER_REGDEFUSAGE,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WintrustGetDefaultForUsage(
    dwAction: WINTRUST_GET_DEFAULT_FOR_USAGE_ACTION,
    pszUsageOID: ?[*:0]const u8,
    psUsage: ?*CRYPT_PROVIDER_DEFUSAGE,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WTHelperGetProvSignerFromChain(
    pProvData: ?*CRYPT_PROVIDER_DATA,
    idxSigner: u32,
    fCounterSigner: BOOL,
    idxCounterSigner: u32,
) callconv(@import("std").os.windows.WINAPI) ?*CRYPT_PROVIDER_SGNR;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WTHelperGetProvCertFromChain(
    pSgnr: ?*CRYPT_PROVIDER_SGNR,
    idxCert: u32,
) callconv(@import("std").os.windows.WINAPI) ?*CRYPT_PROVIDER_CERT;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WTHelperProvDataFromStateData(
    hStateData: ?HANDLE,
) callconv(@import("std").os.windows.WINAPI) ?*CRYPT_PROVIDER_DATA;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WTHelperGetProvPrivateDataFromChain(
    pProvData: ?*CRYPT_PROVIDER_DATA,
    pgProviderID: ?*Guid,
) callconv(@import("std").os.windows.WINAPI) ?*CRYPT_PROVIDER_PRIVDATA;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn WTHelperCertIsSelfSigned(
    dwEncoding: u32,
    pCert: ?*CERT_INFO,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows6.0.6000'
pub extern "wintrust" fn WTHelperCertCheckValidSignature(
    pProvData: ?*CRYPT_PROVIDER_DATA,
) callconv(@import("std").os.windows.WINAPI) HRESULT;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn OpenPersonalTrustDBDialogEx(
    hwndParent: ?HWND,
    dwFlags: u32,
    pvReserved: ?*?*anyopaque,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows5.1.2600'
pub extern "wintrust" fn OpenPersonalTrustDBDialog(
    hwndParent: ?HWND,
) callconv(@import("std").os.windows.WINAPI) BOOL;

// TODO: this type is limited to platform 'windows6.0.6000'
pub extern "wintrust" fn WintrustSetDefaultIncludePEPageHashes(
    fIncludePEPageHashes: BOOL,
) callconv(@import("std").os.windows.WINAPI) void;


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
// Section: Imports (26)
//--------------------------------------------------------------------------------
const Guid = @import("../zig.zig").Guid;
const BOOL = @import("../foundation.zig").BOOL;
const BOOLEAN = @import("../foundation.zig").BOOLEAN;
const CERT_CHAIN_CONTEXT = @import("../security/cryptography.zig").CERT_CHAIN_CONTEXT;
const CERT_CHAIN_ELEMENT = @import("../security/cryptography.zig").CERT_CHAIN_ELEMENT;
const CERT_CHAIN_PARA = @import("../security/cryptography.zig").CERT_CHAIN_PARA;
const CERT_CONTEXT = @import("../security/cryptography.zig").CERT_CONTEXT;
const CERT_INFO = @import("../security/cryptography.zig").CERT_INFO;
const CERT_STRONG_SIGN_PARA = @import("../security/cryptography.zig").CERT_STRONG_SIGN_PARA;
const CERT_USAGE_MATCH = @import("../security/cryptography.zig").CERT_USAGE_MATCH;
const CMSG_SIGNER_INFO = @import("../security/cryptography.zig").CMSG_SIGNER_INFO;
const CRYPT_ALGORITHM_IDENTIFIER = @import("../security/cryptography.zig").CRYPT_ALGORITHM_IDENTIFIER;
const CRYPT_ATTRIBUTE_TYPE_VALUE = @import("../security/cryptography.zig").CRYPT_ATTRIBUTE_TYPE_VALUE;
const CRYPT_BIT_BLOB = @import("../security/cryptography.zig").CRYPT_BIT_BLOB;
const CRYPTOAPI_BLOB = @import("../security/cryptography.zig").CRYPTOAPI_BLOB;
const CTL_CONTEXT = @import("../security/cryptography.zig").CTL_CONTEXT;
const FILETIME = @import("../foundation.zig").FILETIME;
const HANDLE = @import("../foundation.zig").HANDLE;
const HCERTCHAINENGINE = @import("../security/cryptography.zig").HCERTCHAINENGINE;
const HRESULT = @import("../foundation.zig").HRESULT;
const HWND = @import("../foundation.zig").HWND;
const PSTR = @import("../foundation.zig").PSTR;
const PWSTR = @import("../foundation.zig").PWSTR;
const SIP_DISPATCH_INFO = @import("../security/cryptography/sip.zig").SIP_DISPATCH_INFO;
const SIP_INDIRECT_DATA = @import("../security/cryptography/sip.zig").SIP_INDIRECT_DATA;
const SIP_SUBJECTINFO = @import("../security/cryptography/sip.zig").SIP_SUBJECTINFO;

test {
    // The following '_ = <FuncPtrType>' lines are a workaround for https://github.com/ziglang/zig/issues/4476
    if (@hasDecl(@This(), "PFN_CPD_MEM_ALLOC")) { _ = PFN_CPD_MEM_ALLOC; }
    if (@hasDecl(@This(), "PFN_CPD_MEM_FREE")) { _ = PFN_CPD_MEM_FREE; }
    if (@hasDecl(@This(), "PFN_CPD_ADD_STORE")) { _ = PFN_CPD_ADD_STORE; }
    if (@hasDecl(@This(), "PFN_CPD_ADD_SGNR")) { _ = PFN_CPD_ADD_SGNR; }
    if (@hasDecl(@This(), "PFN_CPD_ADD_CERT")) { _ = PFN_CPD_ADD_CERT; }
    if (@hasDecl(@This(), "PFN_CPD_ADD_PRIVDATA")) { _ = PFN_CPD_ADD_PRIVDATA; }
    if (@hasDecl(@This(), "PFN_PROVIDER_INIT_CALL")) { _ = PFN_PROVIDER_INIT_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_OBJTRUST_CALL")) { _ = PFN_PROVIDER_OBJTRUST_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_SIGTRUST_CALL")) { _ = PFN_PROVIDER_SIGTRUST_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_CERTTRUST_CALL")) { _ = PFN_PROVIDER_CERTTRUST_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_FINALPOLICY_CALL")) { _ = PFN_PROVIDER_FINALPOLICY_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_TESTFINALPOLICY_CALL")) { _ = PFN_PROVIDER_TESTFINALPOLICY_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_CLEANUP_CALL")) { _ = PFN_PROVIDER_CLEANUP_CALL; }
    if (@hasDecl(@This(), "PFN_PROVIDER_CERTCHKPOLICY_CALL")) { _ = PFN_PROVIDER_CERTCHKPOLICY_CALL; }
    if (@hasDecl(@This(), "PFN_PROVUI_CALL")) { _ = PFN_PROVUI_CALL; }
    if (@hasDecl(@This(), "PFN_ALLOCANDFILLDEFUSAGE")) { _ = PFN_ALLOCANDFILLDEFUSAGE; }
    if (@hasDecl(@This(), "PFN_FREEDEFUSAGE")) { _ = PFN_FREEDEFUSAGE; }
    if (@hasDecl(@This(), "PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK")) { _ = PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK; }

    @setEvalBranchQuota(
        comptime @import("std").meta.declarations(@This()).len * 3
    );

    // reference all the pub declarations
    if (!@import("builtin").is_test) return;
    inline for (comptime @import("std").meta.declarations(@This())) |decl| {
        if (decl.is_pub) {
            _ = @field(@This(), decl.name);
        }
    }
}
