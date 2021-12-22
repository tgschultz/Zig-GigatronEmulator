//! NOTE: this file is autogenerated, DO NOT MODIFY
//--------------------------------------------------------------------------------
// Section: Constants (84)
//--------------------------------------------------------------------------------
pub const WCN_E_PEER_NOT_FOUND = @import("../zig.zig").typedConst(HRESULT, @as(i32, -2147206143));
pub const WCN_E_AUTHENTICATION_FAILED = @import("../zig.zig").typedConst(HRESULT, @as(i32, -2147206142));
pub const WCN_E_CONNECTION_REJECTED = @import("../zig.zig").typedConst(HRESULT, @as(i32, -2147206141));
pub const WCN_E_SESSION_TIMEDOUT = @import("../zig.zig").typedConst(HRESULT, @as(i32, -2147206140));
pub const WCN_E_PROTOCOL_ERROR = @import("../zig.zig").typedConst(HRESULT, @as(i32, -2147206139));
pub const WCN_VALUE_DT_CATEGORY_COMPUTER = @as(u32, 1);
pub const WCN_VALUE_DT_CATEGORY_INPUT_DEVICE = @as(u32, 2);
pub const WCN_VALUE_DT_CATEGORY_PRINTER = @as(u32, 3);
pub const WCN_VALUE_DT_CATEGORY_CAMERA = @as(u32, 4);
pub const WCN_VALUE_DT_CATEGORY_STORAGE = @as(u32, 5);
pub const WCN_VALUE_DT_CATEGORY_NETWORK_INFRASTRUCTURE = @as(u32, 6);
pub const WCN_VALUE_DT_CATEGORY_DISPLAY = @as(u32, 7);
pub const WCN_VALUE_DT_CATEGORY_MULTIMEDIA_DEVICE = @as(u32, 8);
pub const WCN_VALUE_DT_CATEGORY_GAMING_DEVICE = @as(u32, 9);
pub const WCN_VALUE_DT_CATEGORY_TELEPHONE = @as(u32, 10);
pub const WCN_VALUE_DT_CATEGORY_AUDIO_DEVICE = @as(u32, 11);
pub const WCN_VALUE_DT_CATEGORY_OTHER = @as(u32, 255);
pub const WCN_VALUE_DT_SUBTYPE_WIFI_OUI = @as(u32, 5304836);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__PC = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__SERVER = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__MEDIACENTER = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__ULTRAMOBILEPC = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__NOTEBOOK = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__DESKTOP = @as(u32, 6);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__MID = @as(u32, 7);
pub const WCN_VALUE_DT_SUBTYPE_COMPUTER__NETBOOK = @as(u32, 8);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__KEYBOARD = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__MOUSE = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__JOYSTICK = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__TRACKBALL = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__GAMECONTROLLER = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__REMOTE = @as(u32, 6);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__TOUCHSCREEN = @as(u32, 7);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__BIOMETRICREADER = @as(u32, 8);
pub const WCN_VALUE_DT_SUBTYPE_INPUT_DEVICE__BARCODEREADER = @as(u32, 9);
pub const WCN_VALUE_DT_SUBTYPE_PRINTER__PRINTER = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_PRINTER__SCANNER = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_PRINTER__FAX = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_PRINTER__COPIER = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_PRINTER__ALLINONE = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_CAMERA__STILL_CAMERA = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_CAMERA__VIDEO_CAMERA = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_CAMERA__WEB_CAMERA = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_CAMERA__SECURITY_CAMERA = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_STORAGE__NAS = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_NETWORK_INFRASTRUCUTURE__AP = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_NETWORK_INFRASTRUCUTURE__ROUTER = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_NETWORK_INFRASTRUCUTURE__SWITCH = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_NETWORK_INFRASTRUCUTURE__GATEWAY = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_NETWORK_INFRASTRUCUTURE__BRIDGE = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_DISPLAY__TELEVISION = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_DISPLAY__PICTURE_FRAME = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_DISPLAY__PROJECTOR = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_DISPLAY__MONITOR = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_MULTIMEDIA_DEVICE__DAR = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_MULTIMEDIA_DEVICE__PVR = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_MULTIMEDIA_DEVICE__MCX = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_MULTIMEDIA_DEVICE__SETTOPBOX = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_MULTIMEDIA_DEVICE__MEDIA_SERVER_ADAPT_EXT = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_MULTIMEDIA_DEVICE__PVP = @as(u32, 6);
pub const WCN_VALUE_DT_SUBTYPE_GAMING_DEVICE__XBOX = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_GAMING_DEVICE__XBOX360 = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_GAMING_DEVICE__PLAYSTATION = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_GAMING_DEVICE__CONSOLE_ADAPT = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_GAMING_DEVICE__PORTABLE = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_TELEPHONE__WINDOWS_MOBILE = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_TELEPHONE__PHONE_SINGLEMODE = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_TELEPHONE__PHONE_DUALMODE = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_TELEPHONE__SMARTPHONE_SINGLEMODE = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_TELEPHONE__SMARTPHONE_DUALMODE = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__TUNER_RECEIVER = @as(u32, 1);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__SPEAKERS = @as(u32, 2);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__PMP = @as(u32, 3);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__HEADSET = @as(u32, 4);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__HEADPHONES = @as(u32, 5);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__MICROPHONE = @as(u32, 6);
pub const WCN_VALUE_DT_SUBTYPE_AUDIO_DEVICE__HOMETHEATER = @as(u32, 7);
pub const WCN_API_MAX_BUFFER_SIZE = @as(u32, 2096);
pub const WCN_MICROSOFT_VENDOR_ID = @as(u32, 311);
pub const WCN_NO_SUBTYPE = @as(u32, 4294967294);
pub const WCN_FLAG_DISCOVERY_VE = @as(u32, 1);
pub const WCN_FLAG_AUTHENTICATED_VE = @as(u32, 2);
pub const WCN_FLAG_ENCRYPTED_VE = @as(u32, 4);
pub const SID_WcnProvider = Guid.initString("c100beca-d33a-4a4b-bf23-bbef4663d017");

//--------------------------------------------------------------------------------
// Section: Types (22)
//--------------------------------------------------------------------------------
pub const WCN_ATTRIBUTE_TYPE = enum(i32) {
    TYPE_AP_CHANNEL = 0,
    TYPE_ASSOCIATION_STATE = 1,
    TYPE_AUTHENTICATION_TYPE = 2,
    TYPE_AUTHENTICATION_TYPE_FLAGS = 3,
    TYPE_AUTHENTICATOR = 4,
    TYPE_CONFIG_METHODS = 5,
    TYPE_CONFIGURATION_ERROR = 6,
    TYPE_CONFIRMATION_URL4 = 7,
    TYPE_CONFIRMATION_URL6 = 8,
    TYPE_CONNECTION_TYPE = 9,
    TYPE_CONNECTION_TYPE_FLAGS = 10,
    TYPE_CREDENTIAL = 11,
    TYPE_DEVICE_NAME = 12,
    TYPE_DEVICE_PASSWORD_ID = 13,
    TYPE_E_HASH1 = 14,
    TYPE_E_HASH2 = 15,
    TYPE_E_SNONCE1 = 16,
    TYPE_E_SNONCE2 = 17,
    TYPE_ENCRYPTED_SETTINGS = 18,
    TYPE_ENCRYPTION_TYPE = 19,
    TYPE_ENCRYPTION_TYPE_FLAGS = 20,
    TYPE_ENROLLEE_NONCE = 21,
    TYPE_FEATURE_ID = 22,
    TYPE_IDENTITY = 23,
    TYPE_IDENTITY_PROOF = 24,
    TYPE_KEY_WRAP_AUTHENTICATOR = 25,
    TYPE_KEY_IDENTIFIER = 26,
    TYPE_MAC_ADDRESS = 27,
    TYPE_MANUFACTURER = 28,
    TYPE_MESSAGE_TYPE = 29,
    TYPE_MODEL_NAME = 30,
    TYPE_MODEL_NUMBER = 31,
    TYPE_NETWORK_INDEX = 32,
    TYPE_NETWORK_KEY = 33,
    TYPE_NETWORK_KEY_INDEX = 34,
    TYPE_NEW_DEVICE_NAME = 35,
    TYPE_NEW_PASSWORD = 36,
    TYPE_OOB_DEVICE_PASSWORD = 37,
    TYPE_OS_VERSION = 38,
    TYPE_POWER_LEVEL = 39,
    TYPE_PSK_CURRENT = 40,
    TYPE_PSK_MAX = 41,
    TYPE_PUBLIC_KEY = 42,
    TYPE_RADIO_ENABLED = 43,
    TYPE_REBOOT = 44,
    TYPE_REGISTRAR_CURRENT = 45,
    TYPE_REGISTRAR_ESTABLISHED = 46,
    TYPE_REGISTRAR_LIST = 47,
    TYPE_REGISTRAR_MAX = 48,
    TYPE_REGISTRAR_NONCE = 49,
    TYPE_REQUEST_TYPE = 50,
    TYPE_RESPONSE_TYPE = 51,
    TYPE_RF_BANDS = 52,
    TYPE_R_HASH1 = 53,
    TYPE_R_HASH2 = 54,
    TYPE_R_SNONCE1 = 55,
    TYPE_R_SNONCE2 = 56,
    TYPE_SELECTED_REGISTRAR = 57,
    TYPE_SERIAL_NUMBER = 58,
    TYPE_WI_FI_PROTECTED_SETUP_STATE = 59,
    TYPE_SSID = 60,
    TYPE_TOTAL_NETWORKS = 61,
    TYPE_UUID_E = 62,
    TYPE_UUID_R = 63,
    TYPE_VENDOR_EXTENSION = 64,
    TYPE_VERSION = 65,
    TYPE_X_509_CERTIFICATE_REQUEST = 66,
    TYPE_X_509_CERTIFICATE = 67,
    TYPE_EAP_IDENTITY = 68,
    TYPE_MESSAGE_COUNTER = 69,
    TYPE_PUBLIC_KEY_HASH = 70,
    TYPE_REKEY_KEY = 71,
    TYPE_KEY_LIFETIME = 72,
    TYPE_PERMITTED_CONFIG_METHODS = 73,
    TYPE_SELECTED_REGISTRAR_CONFIG_METHODS = 74,
    TYPE_PRIMARY_DEVICE_TYPE = 75,
    TYPE_SECONDARY_DEVICE_TYPE_LIST = 76,
    TYPE_PORTABLE_DEVICE = 77,
    TYPE_AP_SETUP_LOCKED = 78,
    TYPE_APPLICATION_EXTENSION = 79,
    TYPE_EAP_TYPE = 80,
    TYPE_INITIALIZATION_VECTOR = 81,
    TYPE_KEY_PROVIDED_AUTOMATICALLY = 82,
    TYPE_802_1X_ENABLED = 83,
    TYPE_APPSESSIONKEY = 84,
    TYPE_WEPTRANSMITKEY = 85,
    TYPE_UUID = 86,
    TYPE_PRIMARY_DEVICE_TYPE_CATEGORY = 87,
    TYPE_PRIMARY_DEVICE_TYPE_SUBCATEGORY_OUI = 88,
    TYPE_PRIMARY_DEVICE_TYPE_SUBCATEGORY = 89,
    TYPE_CURRENT_SSID = 90,
    TYPE_BSSID = 91,
    TYPE_DOT11_MAC_ADDRESS = 92,
    TYPE_AUTHORIZED_MACS = 93,
    TYPE_NETWORK_KEY_SHAREABLE = 94,
    TYPE_REQUEST_TO_ENROLL = 95,
    TYPE_REQUESTED_DEVICE_TYPE = 96,
    TYPE_SETTINGS_DELAY_TIME = 97,
    TYPE_VERSION2 = 98,
    TYPE_VENDOR_EXTENSION_WFA = 99,
    NUM_ATTRIBUTE_TYPES = 100,
};
pub const WCN_TYPE_AP_CHANNEL = WCN_ATTRIBUTE_TYPE.TYPE_AP_CHANNEL;
pub const WCN_TYPE_ASSOCIATION_STATE = WCN_ATTRIBUTE_TYPE.TYPE_ASSOCIATION_STATE;
pub const WCN_TYPE_AUTHENTICATION_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_AUTHENTICATION_TYPE;
pub const WCN_TYPE_AUTHENTICATION_TYPE_FLAGS = WCN_ATTRIBUTE_TYPE.TYPE_AUTHENTICATION_TYPE_FLAGS;
pub const WCN_TYPE_AUTHENTICATOR = WCN_ATTRIBUTE_TYPE.TYPE_AUTHENTICATOR;
pub const WCN_TYPE_CONFIG_METHODS = WCN_ATTRIBUTE_TYPE.TYPE_CONFIG_METHODS;
pub const WCN_TYPE_CONFIGURATION_ERROR = WCN_ATTRIBUTE_TYPE.TYPE_CONFIGURATION_ERROR;
pub const WCN_TYPE_CONFIRMATION_URL4 = WCN_ATTRIBUTE_TYPE.TYPE_CONFIRMATION_URL4;
pub const WCN_TYPE_CONFIRMATION_URL6 = WCN_ATTRIBUTE_TYPE.TYPE_CONFIRMATION_URL6;
pub const WCN_TYPE_CONNECTION_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_CONNECTION_TYPE;
pub const WCN_TYPE_CONNECTION_TYPE_FLAGS = WCN_ATTRIBUTE_TYPE.TYPE_CONNECTION_TYPE_FLAGS;
pub const WCN_TYPE_CREDENTIAL = WCN_ATTRIBUTE_TYPE.TYPE_CREDENTIAL;
pub const WCN_TYPE_DEVICE_NAME = WCN_ATTRIBUTE_TYPE.TYPE_DEVICE_NAME;
pub const WCN_TYPE_DEVICE_PASSWORD_ID = WCN_ATTRIBUTE_TYPE.TYPE_DEVICE_PASSWORD_ID;
pub const WCN_TYPE_E_HASH1 = WCN_ATTRIBUTE_TYPE.TYPE_E_HASH1;
pub const WCN_TYPE_E_HASH2 = WCN_ATTRIBUTE_TYPE.TYPE_E_HASH2;
pub const WCN_TYPE_E_SNONCE1 = WCN_ATTRIBUTE_TYPE.TYPE_E_SNONCE1;
pub const WCN_TYPE_E_SNONCE2 = WCN_ATTRIBUTE_TYPE.TYPE_E_SNONCE2;
pub const WCN_TYPE_ENCRYPTED_SETTINGS = WCN_ATTRIBUTE_TYPE.TYPE_ENCRYPTED_SETTINGS;
pub const WCN_TYPE_ENCRYPTION_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_ENCRYPTION_TYPE;
pub const WCN_TYPE_ENCRYPTION_TYPE_FLAGS = WCN_ATTRIBUTE_TYPE.TYPE_ENCRYPTION_TYPE_FLAGS;
pub const WCN_TYPE_ENROLLEE_NONCE = WCN_ATTRIBUTE_TYPE.TYPE_ENROLLEE_NONCE;
pub const WCN_TYPE_FEATURE_ID = WCN_ATTRIBUTE_TYPE.TYPE_FEATURE_ID;
pub const WCN_TYPE_IDENTITY = WCN_ATTRIBUTE_TYPE.TYPE_IDENTITY;
pub const WCN_TYPE_IDENTITY_PROOF = WCN_ATTRIBUTE_TYPE.TYPE_IDENTITY_PROOF;
pub const WCN_TYPE_KEY_WRAP_AUTHENTICATOR = WCN_ATTRIBUTE_TYPE.TYPE_KEY_WRAP_AUTHENTICATOR;
pub const WCN_TYPE_KEY_IDENTIFIER = WCN_ATTRIBUTE_TYPE.TYPE_KEY_IDENTIFIER;
pub const WCN_TYPE_MAC_ADDRESS = WCN_ATTRIBUTE_TYPE.TYPE_MAC_ADDRESS;
pub const WCN_TYPE_MANUFACTURER = WCN_ATTRIBUTE_TYPE.TYPE_MANUFACTURER;
pub const WCN_TYPE_MESSAGE_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_MESSAGE_TYPE;
pub const WCN_TYPE_MODEL_NAME = WCN_ATTRIBUTE_TYPE.TYPE_MODEL_NAME;
pub const WCN_TYPE_MODEL_NUMBER = WCN_ATTRIBUTE_TYPE.TYPE_MODEL_NUMBER;
pub const WCN_TYPE_NETWORK_INDEX = WCN_ATTRIBUTE_TYPE.TYPE_NETWORK_INDEX;
pub const WCN_TYPE_NETWORK_KEY = WCN_ATTRIBUTE_TYPE.TYPE_NETWORK_KEY;
pub const WCN_TYPE_NETWORK_KEY_INDEX = WCN_ATTRIBUTE_TYPE.TYPE_NETWORK_KEY_INDEX;
pub const WCN_TYPE_NEW_DEVICE_NAME = WCN_ATTRIBUTE_TYPE.TYPE_NEW_DEVICE_NAME;
pub const WCN_TYPE_NEW_PASSWORD = WCN_ATTRIBUTE_TYPE.TYPE_NEW_PASSWORD;
pub const WCN_TYPE_OOB_DEVICE_PASSWORD = WCN_ATTRIBUTE_TYPE.TYPE_OOB_DEVICE_PASSWORD;
pub const WCN_TYPE_OS_VERSION = WCN_ATTRIBUTE_TYPE.TYPE_OS_VERSION;
pub const WCN_TYPE_POWER_LEVEL = WCN_ATTRIBUTE_TYPE.TYPE_POWER_LEVEL;
pub const WCN_TYPE_PSK_CURRENT = WCN_ATTRIBUTE_TYPE.TYPE_PSK_CURRENT;
pub const WCN_TYPE_PSK_MAX = WCN_ATTRIBUTE_TYPE.TYPE_PSK_MAX;
pub const WCN_TYPE_PUBLIC_KEY = WCN_ATTRIBUTE_TYPE.TYPE_PUBLIC_KEY;
pub const WCN_TYPE_RADIO_ENABLED = WCN_ATTRIBUTE_TYPE.TYPE_RADIO_ENABLED;
pub const WCN_TYPE_REBOOT = WCN_ATTRIBUTE_TYPE.TYPE_REBOOT;
pub const WCN_TYPE_REGISTRAR_CURRENT = WCN_ATTRIBUTE_TYPE.TYPE_REGISTRAR_CURRENT;
pub const WCN_TYPE_REGISTRAR_ESTABLISHED = WCN_ATTRIBUTE_TYPE.TYPE_REGISTRAR_ESTABLISHED;
pub const WCN_TYPE_REGISTRAR_LIST = WCN_ATTRIBUTE_TYPE.TYPE_REGISTRAR_LIST;
pub const WCN_TYPE_REGISTRAR_MAX = WCN_ATTRIBUTE_TYPE.TYPE_REGISTRAR_MAX;
pub const WCN_TYPE_REGISTRAR_NONCE = WCN_ATTRIBUTE_TYPE.TYPE_REGISTRAR_NONCE;
pub const WCN_TYPE_REQUEST_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_REQUEST_TYPE;
pub const WCN_TYPE_RESPONSE_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_RESPONSE_TYPE;
pub const WCN_TYPE_RF_BANDS = WCN_ATTRIBUTE_TYPE.TYPE_RF_BANDS;
pub const WCN_TYPE_R_HASH1 = WCN_ATTRIBUTE_TYPE.TYPE_R_HASH1;
pub const WCN_TYPE_R_HASH2 = WCN_ATTRIBUTE_TYPE.TYPE_R_HASH2;
pub const WCN_TYPE_R_SNONCE1 = WCN_ATTRIBUTE_TYPE.TYPE_R_SNONCE1;
pub const WCN_TYPE_R_SNONCE2 = WCN_ATTRIBUTE_TYPE.TYPE_R_SNONCE2;
pub const WCN_TYPE_SELECTED_REGISTRAR = WCN_ATTRIBUTE_TYPE.TYPE_SELECTED_REGISTRAR;
pub const WCN_TYPE_SERIAL_NUMBER = WCN_ATTRIBUTE_TYPE.TYPE_SERIAL_NUMBER;
pub const WCN_TYPE_WI_FI_PROTECTED_SETUP_STATE = WCN_ATTRIBUTE_TYPE.TYPE_WI_FI_PROTECTED_SETUP_STATE;
pub const WCN_TYPE_SSID = WCN_ATTRIBUTE_TYPE.TYPE_SSID;
pub const WCN_TYPE_TOTAL_NETWORKS = WCN_ATTRIBUTE_TYPE.TYPE_TOTAL_NETWORKS;
pub const WCN_TYPE_UUID_E = WCN_ATTRIBUTE_TYPE.TYPE_UUID_E;
pub const WCN_TYPE_UUID_R = WCN_ATTRIBUTE_TYPE.TYPE_UUID_R;
pub const WCN_TYPE_VENDOR_EXTENSION = WCN_ATTRIBUTE_TYPE.TYPE_VENDOR_EXTENSION;
pub const WCN_TYPE_VERSION = WCN_ATTRIBUTE_TYPE.TYPE_VERSION;
pub const WCN_TYPE_X_509_CERTIFICATE_REQUEST = WCN_ATTRIBUTE_TYPE.TYPE_X_509_CERTIFICATE_REQUEST;
pub const WCN_TYPE_X_509_CERTIFICATE = WCN_ATTRIBUTE_TYPE.TYPE_X_509_CERTIFICATE;
pub const WCN_TYPE_EAP_IDENTITY = WCN_ATTRIBUTE_TYPE.TYPE_EAP_IDENTITY;
pub const WCN_TYPE_MESSAGE_COUNTER = WCN_ATTRIBUTE_TYPE.TYPE_MESSAGE_COUNTER;
pub const WCN_TYPE_PUBLIC_KEY_HASH = WCN_ATTRIBUTE_TYPE.TYPE_PUBLIC_KEY_HASH;
pub const WCN_TYPE_REKEY_KEY = WCN_ATTRIBUTE_TYPE.TYPE_REKEY_KEY;
pub const WCN_TYPE_KEY_LIFETIME = WCN_ATTRIBUTE_TYPE.TYPE_KEY_LIFETIME;
pub const WCN_TYPE_PERMITTED_CONFIG_METHODS = WCN_ATTRIBUTE_TYPE.TYPE_PERMITTED_CONFIG_METHODS;
pub const WCN_TYPE_SELECTED_REGISTRAR_CONFIG_METHODS = WCN_ATTRIBUTE_TYPE.TYPE_SELECTED_REGISTRAR_CONFIG_METHODS;
pub const WCN_TYPE_PRIMARY_DEVICE_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_PRIMARY_DEVICE_TYPE;
pub const WCN_TYPE_SECONDARY_DEVICE_TYPE_LIST = WCN_ATTRIBUTE_TYPE.TYPE_SECONDARY_DEVICE_TYPE_LIST;
pub const WCN_TYPE_PORTABLE_DEVICE = WCN_ATTRIBUTE_TYPE.TYPE_PORTABLE_DEVICE;
pub const WCN_TYPE_AP_SETUP_LOCKED = WCN_ATTRIBUTE_TYPE.TYPE_AP_SETUP_LOCKED;
pub const WCN_TYPE_APPLICATION_EXTENSION = WCN_ATTRIBUTE_TYPE.TYPE_APPLICATION_EXTENSION;
pub const WCN_TYPE_EAP_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_EAP_TYPE;
pub const WCN_TYPE_INITIALIZATION_VECTOR = WCN_ATTRIBUTE_TYPE.TYPE_INITIALIZATION_VECTOR;
pub const WCN_TYPE_KEY_PROVIDED_AUTOMATICALLY = WCN_ATTRIBUTE_TYPE.TYPE_KEY_PROVIDED_AUTOMATICALLY;
pub const WCN_TYPE_802_1X_ENABLED = WCN_ATTRIBUTE_TYPE.TYPE_802_1X_ENABLED;
pub const WCN_TYPE_APPSESSIONKEY = WCN_ATTRIBUTE_TYPE.TYPE_APPSESSIONKEY;
pub const WCN_TYPE_WEPTRANSMITKEY = WCN_ATTRIBUTE_TYPE.TYPE_WEPTRANSMITKEY;
pub const WCN_TYPE_UUID = WCN_ATTRIBUTE_TYPE.TYPE_UUID;
pub const WCN_TYPE_PRIMARY_DEVICE_TYPE_CATEGORY = WCN_ATTRIBUTE_TYPE.TYPE_PRIMARY_DEVICE_TYPE_CATEGORY;
pub const WCN_TYPE_PRIMARY_DEVICE_TYPE_SUBCATEGORY_OUI = WCN_ATTRIBUTE_TYPE.TYPE_PRIMARY_DEVICE_TYPE_SUBCATEGORY_OUI;
pub const WCN_TYPE_PRIMARY_DEVICE_TYPE_SUBCATEGORY = WCN_ATTRIBUTE_TYPE.TYPE_PRIMARY_DEVICE_TYPE_SUBCATEGORY;
pub const WCN_TYPE_CURRENT_SSID = WCN_ATTRIBUTE_TYPE.TYPE_CURRENT_SSID;
pub const WCN_TYPE_BSSID = WCN_ATTRIBUTE_TYPE.TYPE_BSSID;
pub const WCN_TYPE_DOT11_MAC_ADDRESS = WCN_ATTRIBUTE_TYPE.TYPE_DOT11_MAC_ADDRESS;
pub const WCN_TYPE_AUTHORIZED_MACS = WCN_ATTRIBUTE_TYPE.TYPE_AUTHORIZED_MACS;
pub const WCN_TYPE_NETWORK_KEY_SHAREABLE = WCN_ATTRIBUTE_TYPE.TYPE_NETWORK_KEY_SHAREABLE;
pub const WCN_TYPE_REQUEST_TO_ENROLL = WCN_ATTRIBUTE_TYPE.TYPE_REQUEST_TO_ENROLL;
pub const WCN_TYPE_REQUESTED_DEVICE_TYPE = WCN_ATTRIBUTE_TYPE.TYPE_REQUESTED_DEVICE_TYPE;
pub const WCN_TYPE_SETTINGS_DELAY_TIME = WCN_ATTRIBUTE_TYPE.TYPE_SETTINGS_DELAY_TIME;
pub const WCN_TYPE_VERSION2 = WCN_ATTRIBUTE_TYPE.TYPE_VERSION2;
pub const WCN_TYPE_VENDOR_EXTENSION_WFA = WCN_ATTRIBUTE_TYPE.TYPE_VENDOR_EXTENSION_WFA;
pub const WCN_NUM_ATTRIBUTE_TYPES = WCN_ATTRIBUTE_TYPE.NUM_ATTRIBUTE_TYPES;

pub const WCN_VALUE_TYPE_VERSION = enum(i32) {
    @"1_0" = 16,
    @"2_0" = 32,
};
pub const WCN_VALUE_VERSION_1_0 = WCN_VALUE_TYPE_VERSION.@"1_0";
pub const WCN_VALUE_VERSION_2_0 = WCN_VALUE_TYPE_VERSION.@"2_0";

pub const WCN_VALUE_TYPE_BOOLEAN = enum(i32) {
    FALSE = 0,
    TRUE = 1,
};
pub const WCN_VALUE_FALSE = WCN_VALUE_TYPE_BOOLEAN.FALSE;
pub const WCN_VALUE_TRUE = WCN_VALUE_TYPE_BOOLEAN.TRUE;

pub const WCN_VALUE_TYPE_ASSOCIATION_STATE = enum(i32) {
    NOT_ASSOCIATED = 0,
    CONNECTION_SUCCESS = 1,
    CONFIGURATION_FAILURE = 2,
    ASSOCIATION_FAILURE = 3,
    IP_FAILURE = 4,
};
pub const WCN_VALUE_AS_NOT_ASSOCIATED = WCN_VALUE_TYPE_ASSOCIATION_STATE.NOT_ASSOCIATED;
pub const WCN_VALUE_AS_CONNECTION_SUCCESS = WCN_VALUE_TYPE_ASSOCIATION_STATE.CONNECTION_SUCCESS;
pub const WCN_VALUE_AS_CONFIGURATION_FAILURE = WCN_VALUE_TYPE_ASSOCIATION_STATE.CONFIGURATION_FAILURE;
pub const WCN_VALUE_AS_ASSOCIATION_FAILURE = WCN_VALUE_TYPE_ASSOCIATION_STATE.ASSOCIATION_FAILURE;
pub const WCN_VALUE_AS_IP_FAILURE = WCN_VALUE_TYPE_ASSOCIATION_STATE.IP_FAILURE;

pub const WCN_VALUE_TYPE_AUTHENTICATION_TYPE = enum(i32) {
    OPEN = 1,
    WPAPSK = 2,
    SHARED = 4,
    WPA = 8,
    WPA2 = 16,
    WPA2PSK = 32,
    WPAWPA2PSK_MIXED = 34,
};
pub const WCN_VALUE_AT_OPEN = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.OPEN;
pub const WCN_VALUE_AT_WPAPSK = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.WPAPSK;
pub const WCN_VALUE_AT_SHARED = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.SHARED;
pub const WCN_VALUE_AT_WPA = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.WPA;
pub const WCN_VALUE_AT_WPA2 = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.WPA2;
pub const WCN_VALUE_AT_WPA2PSK = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.WPA2PSK;
pub const WCN_VALUE_AT_WPAWPA2PSK_MIXED = WCN_VALUE_TYPE_AUTHENTICATION_TYPE.WPAWPA2PSK_MIXED;

pub const WCN_VALUE_TYPE_CONFIG_METHODS = enum(i32) {
    USBA = 1,
    ETHERNET = 2,
    LABEL = 4,
    DISPLAY = 8,
    EXTERNAL_NFC = 16,
    INTEGRATED_NFC = 32,
    NFC_INTERFACE = 64,
    PUSHBUTTON = 128,
    KEYPAD = 256,
    VIRT_PUSHBUTTON = 640,
    PHYS_PUSHBUTTON = 1152,
    VIRT_DISPLAY = 8200,
    PHYS_DISPLAY = 16392,
};
pub const WCN_VALUE_CM_USBA = WCN_VALUE_TYPE_CONFIG_METHODS.USBA;
pub const WCN_VALUE_CM_ETHERNET = WCN_VALUE_TYPE_CONFIG_METHODS.ETHERNET;
pub const WCN_VALUE_CM_LABEL = WCN_VALUE_TYPE_CONFIG_METHODS.LABEL;
pub const WCN_VALUE_CM_DISPLAY = WCN_VALUE_TYPE_CONFIG_METHODS.DISPLAY;
pub const WCN_VALUE_CM_EXTERNAL_NFC = WCN_VALUE_TYPE_CONFIG_METHODS.EXTERNAL_NFC;
pub const WCN_VALUE_CM_INTEGRATED_NFC = WCN_VALUE_TYPE_CONFIG_METHODS.INTEGRATED_NFC;
pub const WCN_VALUE_CM_NFC_INTERFACE = WCN_VALUE_TYPE_CONFIG_METHODS.NFC_INTERFACE;
pub const WCN_VALUE_CM_PUSHBUTTON = WCN_VALUE_TYPE_CONFIG_METHODS.PUSHBUTTON;
pub const WCN_VALUE_CM_KEYPAD = WCN_VALUE_TYPE_CONFIG_METHODS.KEYPAD;
pub const WCN_VALUE_CM_VIRT_PUSHBUTTON = WCN_VALUE_TYPE_CONFIG_METHODS.VIRT_PUSHBUTTON;
pub const WCN_VALUE_CM_PHYS_PUSHBUTTON = WCN_VALUE_TYPE_CONFIG_METHODS.PHYS_PUSHBUTTON;
pub const WCN_VALUE_CM_VIRT_DISPLAY = WCN_VALUE_TYPE_CONFIG_METHODS.VIRT_DISPLAY;
pub const WCN_VALUE_CM_PHYS_DISPLAY = WCN_VALUE_TYPE_CONFIG_METHODS.PHYS_DISPLAY;

pub const WCN_VALUE_TYPE_CONFIGURATION_ERROR = enum(i32) {
    NO_ERROR = 0,
    OOB_INTERFACE_READ_ERROR = 1,
    DECRYPTION_CRC_FAILURE = 2,
    @"2_4_CHANNEL_NOT_SUPPORTED" = 3,
    @"5_0_CHANNEL_NOT_SUPPORTED" = 4,
    SIGNAL_TOO_WEAK = 5,
    NETWORK_AUTHENTICATION_FAILURE = 6,
    NETWORK_ASSOCIATION_FAILURE = 7,
    NO_DHCP_RESPONSE = 8,
    FAILED_DHCP_CONFIG = 9,
    IP_ADDRESS_CONFLICT = 10,
    COULD_NOT_CONNECT_TO_REGISTRAR = 11,
    MULTIPLE_PBC_SESSIONS_DETECTED = 12,
    ROGUE_ACTIVITY_SUSPECTED = 13,
    DEVICE_BUSY = 14,
    SETUP_LOCKED = 15,
    MESSAGE_TIMEOUT = 16,
    REGISTRATION_SESSION_TIMEOUT = 17,
    DEVICE_PASSWORD_AUTH_FAILURE = 18,
};
pub const WCN_VALUE_CE_NO_ERROR = WCN_VALUE_TYPE_CONFIGURATION_ERROR.NO_ERROR;
pub const WCN_VALUE_CE_OOB_INTERFACE_READ_ERROR = WCN_VALUE_TYPE_CONFIGURATION_ERROR.OOB_INTERFACE_READ_ERROR;
pub const WCN_VALUE_CE_DECRYPTION_CRC_FAILURE = WCN_VALUE_TYPE_CONFIGURATION_ERROR.DECRYPTION_CRC_FAILURE;
pub const WCN_VALUE_CE_2_4_CHANNEL_NOT_SUPPORTED = WCN_VALUE_TYPE_CONFIGURATION_ERROR.@"2_4_CHANNEL_NOT_SUPPORTED";
pub const WCN_VALUE_CE_5_0_CHANNEL_NOT_SUPPORTED = WCN_VALUE_TYPE_CONFIGURATION_ERROR.@"5_0_CHANNEL_NOT_SUPPORTED";
pub const WCN_VALUE_CE_SIGNAL_TOO_WEAK = WCN_VALUE_TYPE_CONFIGURATION_ERROR.SIGNAL_TOO_WEAK;
pub const WCN_VALUE_CE_NETWORK_AUTHENTICATION_FAILURE = WCN_VALUE_TYPE_CONFIGURATION_ERROR.NETWORK_AUTHENTICATION_FAILURE;
pub const WCN_VALUE_CE_NETWORK_ASSOCIATION_FAILURE = WCN_VALUE_TYPE_CONFIGURATION_ERROR.NETWORK_ASSOCIATION_FAILURE;
pub const WCN_VALUE_CE_NO_DHCP_RESPONSE = WCN_VALUE_TYPE_CONFIGURATION_ERROR.NO_DHCP_RESPONSE;
pub const WCN_VALUE_CE_FAILED_DHCP_CONFIG = WCN_VALUE_TYPE_CONFIGURATION_ERROR.FAILED_DHCP_CONFIG;
pub const WCN_VALUE_CE_IP_ADDRESS_CONFLICT = WCN_VALUE_TYPE_CONFIGURATION_ERROR.IP_ADDRESS_CONFLICT;
pub const WCN_VALUE_CE_COULD_NOT_CONNECT_TO_REGISTRAR = WCN_VALUE_TYPE_CONFIGURATION_ERROR.COULD_NOT_CONNECT_TO_REGISTRAR;
pub const WCN_VALUE_CE_MULTIPLE_PBC_SESSIONS_DETECTED = WCN_VALUE_TYPE_CONFIGURATION_ERROR.MULTIPLE_PBC_SESSIONS_DETECTED;
pub const WCN_VALUE_CE_ROGUE_ACTIVITY_SUSPECTED = WCN_VALUE_TYPE_CONFIGURATION_ERROR.ROGUE_ACTIVITY_SUSPECTED;
pub const WCN_VALUE_CE_DEVICE_BUSY = WCN_VALUE_TYPE_CONFIGURATION_ERROR.DEVICE_BUSY;
pub const WCN_VALUE_CE_SETUP_LOCKED = WCN_VALUE_TYPE_CONFIGURATION_ERROR.SETUP_LOCKED;
pub const WCN_VALUE_CE_MESSAGE_TIMEOUT = WCN_VALUE_TYPE_CONFIGURATION_ERROR.MESSAGE_TIMEOUT;
pub const WCN_VALUE_CE_REGISTRATION_SESSION_TIMEOUT = WCN_VALUE_TYPE_CONFIGURATION_ERROR.REGISTRATION_SESSION_TIMEOUT;
pub const WCN_VALUE_CE_DEVICE_PASSWORD_AUTH_FAILURE = WCN_VALUE_TYPE_CONFIGURATION_ERROR.DEVICE_PASSWORD_AUTH_FAILURE;

pub const WCN_VALUE_TYPE_CONNECTION_TYPE = enum(i32) {
    ESS = 1,
    IBSS = 2,
};
pub const WCN_VALUE_CT_ESS = WCN_VALUE_TYPE_CONNECTION_TYPE.ESS;
pub const WCN_VALUE_CT_IBSS = WCN_VALUE_TYPE_CONNECTION_TYPE.IBSS;

pub const WCN_VALUE_TYPE_DEVICE_PASSWORD_ID = enum(i32) {
    DEFAULT = 0,
    USER_SPECIFIED = 1,
    MACHINE_SPECIFIED = 2,
    REKEY = 3,
    PUSHBUTTON = 4,
    REGISTRAR_SPECIFIED = 5,
    NFC_CONNECTION_HANDOVER = 7,
    WFD_SERVICES = 8,
    OUTOFBAND_MIN = 16,
    OUTOFBAND_MAX = 65535,
};
pub const WCN_VALUE_DP_DEFAULT = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.DEFAULT;
pub const WCN_VALUE_DP_USER_SPECIFIED = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.USER_SPECIFIED;
pub const WCN_VALUE_DP_MACHINE_SPECIFIED = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.MACHINE_SPECIFIED;
pub const WCN_VALUE_DP_REKEY = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.REKEY;
pub const WCN_VALUE_DP_PUSHBUTTON = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.PUSHBUTTON;
pub const WCN_VALUE_DP_REGISTRAR_SPECIFIED = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.REGISTRAR_SPECIFIED;
pub const WCN_VALUE_DP_NFC_CONNECTION_HANDOVER = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.NFC_CONNECTION_HANDOVER;
pub const WCN_VALUE_DP_WFD_SERVICES = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.WFD_SERVICES;
pub const WCN_VALUE_DP_OUTOFBAND_MIN = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.OUTOFBAND_MIN;
pub const WCN_VALUE_DP_OUTOFBAND_MAX = WCN_VALUE_TYPE_DEVICE_PASSWORD_ID.OUTOFBAND_MAX;

pub const WCN_VALUE_TYPE_ENCRYPTION_TYPE = enum(i32) {
    NONE = 1,
    WEP = 2,
    TKIP = 4,
    AES = 8,
    TKIP_AES_MIXED = 12,
};
pub const WCN_VALUE_ET_NONE = WCN_VALUE_TYPE_ENCRYPTION_TYPE.NONE;
pub const WCN_VALUE_ET_WEP = WCN_VALUE_TYPE_ENCRYPTION_TYPE.WEP;
pub const WCN_VALUE_ET_TKIP = WCN_VALUE_TYPE_ENCRYPTION_TYPE.TKIP;
pub const WCN_VALUE_ET_AES = WCN_VALUE_TYPE_ENCRYPTION_TYPE.AES;
pub const WCN_VALUE_ET_TKIP_AES_MIXED = WCN_VALUE_TYPE_ENCRYPTION_TYPE.TKIP_AES_MIXED;

pub const WCN_VALUE_TYPE_MESSAGE_TYPE = enum(i32) {
    BEACON = 1,
    PROBE_REQUEST = 2,
    PROBE_RESPONSE = 3,
    M1 = 4,
    M2 = 5,
    M2D = 6,
    M3 = 7,
    M4 = 8,
    M5 = 9,
    M6 = 10,
    M7 = 11,
    M8 = 12,
    ACK = 13,
    NACK = 14,
    DONE = 15,
};
pub const WCN_VALUE_MT_BEACON = WCN_VALUE_TYPE_MESSAGE_TYPE.BEACON;
pub const WCN_VALUE_MT_PROBE_REQUEST = WCN_VALUE_TYPE_MESSAGE_TYPE.PROBE_REQUEST;
pub const WCN_VALUE_MT_PROBE_RESPONSE = WCN_VALUE_TYPE_MESSAGE_TYPE.PROBE_RESPONSE;
pub const WCN_VALUE_MT_M1 = WCN_VALUE_TYPE_MESSAGE_TYPE.M1;
pub const WCN_VALUE_MT_M2 = WCN_VALUE_TYPE_MESSAGE_TYPE.M2;
pub const WCN_VALUE_MT_M2D = WCN_VALUE_TYPE_MESSAGE_TYPE.M2D;
pub const WCN_VALUE_MT_M3 = WCN_VALUE_TYPE_MESSAGE_TYPE.M3;
pub const WCN_VALUE_MT_M4 = WCN_VALUE_TYPE_MESSAGE_TYPE.M4;
pub const WCN_VALUE_MT_M5 = WCN_VALUE_TYPE_MESSAGE_TYPE.M5;
pub const WCN_VALUE_MT_M6 = WCN_VALUE_TYPE_MESSAGE_TYPE.M6;
pub const WCN_VALUE_MT_M7 = WCN_VALUE_TYPE_MESSAGE_TYPE.M7;
pub const WCN_VALUE_MT_M8 = WCN_VALUE_TYPE_MESSAGE_TYPE.M8;
pub const WCN_VALUE_MT_ACK = WCN_VALUE_TYPE_MESSAGE_TYPE.ACK;
pub const WCN_VALUE_MT_NACK = WCN_VALUE_TYPE_MESSAGE_TYPE.NACK;
pub const WCN_VALUE_MT_DONE = WCN_VALUE_TYPE_MESSAGE_TYPE.DONE;

pub const WCN_VALUE_TYPE_REQUEST_TYPE = enum(i32) {
    ENROLLEE_INFO = 0,
    ENROLLEE_OPEN_1X = 1,
    REGISTRAR = 2,
    MANAGER_REGISTRAR = 3,
};
pub const WCN_VALUE_ReqT_ENROLLEE_INFO = WCN_VALUE_TYPE_REQUEST_TYPE.ENROLLEE_INFO;
pub const WCN_VALUE_ReqT_ENROLLEE_OPEN_1X = WCN_VALUE_TYPE_REQUEST_TYPE.ENROLLEE_OPEN_1X;
pub const WCN_VALUE_ReqT_REGISTRAR = WCN_VALUE_TYPE_REQUEST_TYPE.REGISTRAR;
pub const WCN_VALUE_ReqT_MANAGER_REGISTRAR = WCN_VALUE_TYPE_REQUEST_TYPE.MANAGER_REGISTRAR;

pub const WCN_VALUE_TYPE_RESPONSE_TYPE = enum(i32) {
    ENROLLEE_INFO = 0,
    ENROLLEE_OPEN_1X = 1,
    REGISTRAR = 2,
    AP = 3,
};
pub const WCN_VALUE_RspT_ENROLLEE_INFO = WCN_VALUE_TYPE_RESPONSE_TYPE.ENROLLEE_INFO;
pub const WCN_VALUE_RspT_ENROLLEE_OPEN_1X = WCN_VALUE_TYPE_RESPONSE_TYPE.ENROLLEE_OPEN_1X;
pub const WCN_VALUE_RspT_REGISTRAR = WCN_VALUE_TYPE_RESPONSE_TYPE.REGISTRAR;
pub const WCN_VALUE_RspT_AP = WCN_VALUE_TYPE_RESPONSE_TYPE.AP;

pub const WCN_VALUE_TYPE_RF_BANDS = enum(i32) {
    @"24GHZ" = 1,
    @"50GHZ" = 2,
};
pub const WCN_VALUE_RB_24GHZ = WCN_VALUE_TYPE_RF_BANDS.@"24GHZ";
pub const WCN_VALUE_RB_50GHZ = WCN_VALUE_TYPE_RF_BANDS.@"50GHZ";

pub const WCN_VALUE_TYPE_WI_FI_PROTECTED_SETUP_STATE = enum(i32) {
    RESERVED00 = 0,
    NOT_CONFIGURED = 1,
    CONFIGURED = 2,
};
pub const WCN_VALUE_SS_RESERVED00 = WCN_VALUE_TYPE_WI_FI_PROTECTED_SETUP_STATE.RESERVED00;
pub const WCN_VALUE_SS_NOT_CONFIGURED = WCN_VALUE_TYPE_WI_FI_PROTECTED_SETUP_STATE.NOT_CONFIGURED;
pub const WCN_VALUE_SS_CONFIGURED = WCN_VALUE_TYPE_WI_FI_PROTECTED_SETUP_STATE.CONFIGURED;

pub const WCN_VALUE_TYPE_PRIMARY_DEVICE_TYPE = packed struct {
    Category: u16,
    SubCategoryOUI: u32,
    SubCategory: u16,
};

const CLSID_WCNDeviceObject_Value = @import("../zig.zig").Guid.initString("c100bea7-d33a-4a4b-bf23-bbef4663d017");
pub const CLSID_WCNDeviceObject = &CLSID_WCNDeviceObject_Value;

pub const WCN_PASSWORD_TYPE = enum(i32) {
    PUSH_BUTTON = 0,
    PIN = 1,
    PIN_REGISTRAR_SPECIFIED = 2,
    OOB_SPECIFIED = 3,
    WFDS = 4,
};
pub const WCN_PASSWORD_TYPE_PUSH_BUTTON = WCN_PASSWORD_TYPE.PUSH_BUTTON;
pub const WCN_PASSWORD_TYPE_PIN = WCN_PASSWORD_TYPE.PIN;
pub const WCN_PASSWORD_TYPE_PIN_REGISTRAR_SPECIFIED = WCN_PASSWORD_TYPE.PIN_REGISTRAR_SPECIFIED;
pub const WCN_PASSWORD_TYPE_OOB_SPECIFIED = WCN_PASSWORD_TYPE.OOB_SPECIFIED;
pub const WCN_PASSWORD_TYPE_WFDS = WCN_PASSWORD_TYPE.WFDS;

pub const WCN_SESSION_STATUS = enum(i32) {
    SUCCESS = 0,
    FAILURE_GENERIC = 1,
    FAILURE_TIMEOUT = 2,
};
pub const WCN_SESSION_STATUS_SUCCESS = WCN_SESSION_STATUS.SUCCESS;
pub const WCN_SESSION_STATUS_FAILURE_GENERIC = WCN_SESSION_STATUS.FAILURE_GENERIC;
pub const WCN_SESSION_STATUS_FAILURE_TIMEOUT = WCN_SESSION_STATUS.FAILURE_TIMEOUT;

pub const WCN_VENDOR_EXTENSION_SPEC = extern struct {
    VendorId: u32,
    SubType: u32,
    Index: u32,
    Flags: u32,
};

// TODO: this type is limited to platform 'windows6.1'
const IID_IWCNDevice_Value = @import("../zig.zig").Guid.initString("c100be9c-d33a-4a4b-bf23-bbef4663d017");
pub const IID_IWCNDevice = &IID_IWCNDevice_Value;
pub const IWCNDevice = extern struct {
    pub const VTable = extern struct {
        base: IUnknown.VTable,
        SetPassword: fn(
            self: *const IWCNDevice,
            Type: WCN_PASSWORD_TYPE,
            dwPasswordLength: u32,
            pbPassword: [*:0]const u8,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        Connect: fn(
            self: *const IWCNDevice,
            pNotify: ?*IWCNConnectNotify,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetAttribute: fn(
            self: *const IWCNDevice,
            AttributeType: WCN_ATTRIBUTE_TYPE,
            dwMaxBufferSize: u32,
            pbBuffer: [*:0]u8,
            pdwBufferUsed: ?*u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetIntegerAttribute: fn(
            self: *const IWCNDevice,
            AttributeType: WCN_ATTRIBUTE_TYPE,
            puInteger: ?*u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetStringAttribute: fn(
            self: *const IWCNDevice,
            AttributeType: WCN_ATTRIBUTE_TYPE,
            cchMaxString: u32,
            wszString: [*:0]u16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetNetworkProfile: fn(
            self: *const IWCNDevice,
            cchMaxStringLength: u32,
            wszProfile: [*:0]u16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        SetNetworkProfile: fn(
            self: *const IWCNDevice,
            pszProfileXml: ?[*:0]const u16,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        GetVendorExtension: fn(
            self: *const IWCNDevice,
            pVendorExtSpec: ?*const WCN_VENDOR_EXTENSION_SPEC,
            dwMaxBufferSize: u32,
            pbBuffer: [*:0]u8,
            pdwBufferUsed: ?*u32,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        SetVendorExtension: fn(
            self: *const IWCNDevice,
            pVendorExtSpec: ?*const WCN_VENDOR_EXTENSION_SPEC,
            cbBuffer: u32,
            pbBuffer: [*:0]const u8,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        Unadvise: fn(
            self: *const IWCNDevice,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        SetNFCPasswordParams: fn(
            self: *const IWCNDevice,
            Type: WCN_PASSWORD_TYPE,
            dwOOBPasswordID: u32,
            dwPasswordLength: u32,
            pbPassword: ?[*:0]const u8,
            dwRemotePublicKeyHashLength: u32,
            pbRemotePublicKeyHash: ?[*:0]const u8,
            dwDHKeyBlobLength: u32,
            pbDHKeyBlob: ?[*:0]const u8,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IUnknown.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_SetPassword(self: *const T, Type: WCN_PASSWORD_TYPE, dwPasswordLength: u32, pbPassword: [*:0]const u8) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).SetPassword(@ptrCast(*const IWCNDevice, self), Type, dwPasswordLength, pbPassword);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_Connect(self: *const T, pNotify: ?*IWCNConnectNotify) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).Connect(@ptrCast(*const IWCNDevice, self), pNotify);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_GetAttribute(self: *const T, AttributeType: WCN_ATTRIBUTE_TYPE, dwMaxBufferSize: u32, pbBuffer: [*:0]u8, pdwBufferUsed: ?*u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).GetAttribute(@ptrCast(*const IWCNDevice, self), AttributeType, dwMaxBufferSize, pbBuffer, pdwBufferUsed);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_GetIntegerAttribute(self: *const T, AttributeType: WCN_ATTRIBUTE_TYPE, puInteger: ?*u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).GetIntegerAttribute(@ptrCast(*const IWCNDevice, self), AttributeType, puInteger);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_GetStringAttribute(self: *const T, AttributeType: WCN_ATTRIBUTE_TYPE, cchMaxString: u32, wszString: [*:0]u16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).GetStringAttribute(@ptrCast(*const IWCNDevice, self), AttributeType, cchMaxString, wszString);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_GetNetworkProfile(self: *const T, cchMaxStringLength: u32, wszProfile: [*:0]u16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).GetNetworkProfile(@ptrCast(*const IWCNDevice, self), cchMaxStringLength, wszProfile);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_SetNetworkProfile(self: *const T, pszProfileXml: ?[*:0]const u16) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).SetNetworkProfile(@ptrCast(*const IWCNDevice, self), pszProfileXml);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_GetVendorExtension(self: *const T, pVendorExtSpec: ?*const WCN_VENDOR_EXTENSION_SPEC, dwMaxBufferSize: u32, pbBuffer: [*:0]u8, pdwBufferUsed: ?*u32) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).GetVendorExtension(@ptrCast(*const IWCNDevice, self), pVendorExtSpec, dwMaxBufferSize, pbBuffer, pdwBufferUsed);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_SetVendorExtension(self: *const T, pVendorExtSpec: ?*const WCN_VENDOR_EXTENSION_SPEC, cbBuffer: u32, pbBuffer: [*:0]const u8) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).SetVendorExtension(@ptrCast(*const IWCNDevice, self), pVendorExtSpec, cbBuffer, pbBuffer);
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_Unadvise(self: *const T) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).Unadvise(@ptrCast(*const IWCNDevice, self));
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNDevice_SetNFCPasswordParams(self: *const T, Type: WCN_PASSWORD_TYPE, dwOOBPasswordID: u32, dwPasswordLength: u32, pbPassword: ?[*:0]const u8, dwRemotePublicKeyHashLength: u32, pbRemotePublicKeyHash: ?[*:0]const u8, dwDHKeyBlobLength: u32, pbDHKeyBlob: ?[*:0]const u8) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNDevice.VTable, self.vtable).SetNFCPasswordParams(@ptrCast(*const IWCNDevice, self), Type, dwOOBPasswordID, dwPasswordLength, pbPassword, dwRemotePublicKeyHashLength, pbRemotePublicKeyHash, dwDHKeyBlobLength, pbDHKeyBlob);
        }
    };}
    pub usingnamespace MethodMixin(@This());
};

// TODO: this type is limited to platform 'windows6.1'
const IID_IWCNConnectNotify_Value = @import("../zig.zig").Guid.initString("c100be9f-d33a-4a4b-bf23-bbef4663d017");
pub const IID_IWCNConnectNotify = &IID_IWCNConnectNotify_Value;
pub const IWCNConnectNotify = extern struct {
    pub const VTable = extern struct {
        base: IUnknown.VTable,
        ConnectSucceeded: fn(
            self: *const IWCNConnectNotify,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
        ConnectFailed: fn(
            self: *const IWCNConnectNotify,
            hrFailure: HRESULT,
        ) callconv(@import("std").os.windows.WINAPI) HRESULT,
    };
    vtable: *const VTable,
    pub fn MethodMixin(comptime T: type) type { return struct {
        pub usingnamespace IUnknown.MethodMixin(T);
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNConnectNotify_ConnectSucceeded(self: *const T) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNConnectNotify.VTable, self.vtable).ConnectSucceeded(@ptrCast(*const IWCNConnectNotify, self));
        }
        // NOTE: method is namespaced with interface name to avoid conflicts for now
        pub fn IWCNConnectNotify_ConnectFailed(self: *const T, hrFailure: HRESULT) callconv(.Inline) HRESULT {
            return @ptrCast(*const IWCNConnectNotify.VTable, self.vtable).ConnectFailed(@ptrCast(*const IWCNConnectNotify, self), hrFailure);
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
const Guid = @import("../zig.zig").Guid;
const HRESULT = @import("../foundation.zig").HRESULT;
const IUnknown = @import("../system/com.zig").IUnknown;
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
