(*
 * The ALCAPI, ALCAPIENTRY, and ALC_INVALID macros are deprecated, but are
 * included for applications porting code from AL 1.0
 *)

{ ALCAPI ALC_API
  ALCAPIENTRY ALC_APIENTRY
  ALC_INVALID                       = 0;


  ALC_VERSION_0_1                   = 1;}

type
  PALCdevice                        = ^ALCdevice;
  ALCdevice                         = record
  end;

  PALCcontext                       = ^ALCcontext;
  ALCcontext                        = record
  end;


(** 8-bit boolean *)
  ALCboolean                        = cbool;
  PALCboolean                       = pcbool;

(** character *)
  ALCchar                           = cchar;
  PALCchar                          = PChar; //pcchar;

(** signed 8-bit 2's complement integer *)
  ALCbyte                           = cint8;
  PALCbyte                          = pcint8;

(** unsigned 8-bit integer *)
  ALCubyte                          = cuint8;
  PALCubyte                         = pcuint8;

(** signed 16-bit 2's complement integer *)
  ALCshort                          = cint16;
  PALCshort                         = pcint16;

(** unsigned 16-bit integer *)
  ALCushort                         = cuint16;
  PALCushort                        = pcuint16;

(** signed 32-bit 2's complement integer *)
  ALCint                            = cint32;
  PALCint                           = pcint32;

(** unsigned 32-bit integer *)
  ALCuint                           = cuint32;
  PALCuint                          = pcuint32;

(** non-negative 32-bit binary integer size *)
  ALCsizei                          = cint32;
  PALCsizei                         = pcint32;

(** enumerated 32-bit value *)
  ALCenum                           = cint32;
  PALCenum                          = pcint32;

(** 32-bit IEEE754 floating-point *)
  ALCfloat                          = cfloat;
  PALCfloat                         = pcfloat;

(** 64-bit IEEE754 floating-point *)
  ALCdouble                         = cdouble;
  PALCdouble                        = pcdouble;

(** void type (for opaque pointers only) *)
  PALCvoid                          = pointer;


(* Enumerant values begin at column 50. No tabs. *)
const

(* Boolean False. *)
  ALC_FALSE                         = 0;

(* Boolean True. *)
  ALC_TRUE                          = 1;

(**
 * followed by <int> Hz
 *)
  ALC_FREQUENCY                     = $1007;

(**
 * followed by <int> Hz
 *)
  ALC_REFRESH                       = $1008;

(**
 * followed by AL_TRUE, AL_FALSE
 *)
  ALC_SYNC                          = $1009;

(**
 * followed by <int> Num of requested Mono (3D) Sources
 *)
  ALC_MONO_SOURCES                  = $1010;

(**
 * followed by <int> Num of requested Stereo Sources
 *)
  ALC_STEREO_SOURCES                = $1011;

(**
 * errors
 *)

(**
 * No error
 *)
  ALC_NO_ERROR                      = 0;

(**
 * No device
 *)
  ALC_INVALID_DEVICE                = $A001;

(**
 * invalid context ID
 *)
  ALC_INVALID_CONTEXT               = $A002;

(**
 * bad enum
 *)
  ALC_INVALID_ENUM                  = $A003;

(**
 * bad value
 *)
  ALC_INVALID_VALUE                 = $A004;

(**
 * Out of memory.
 *)
  ALC_OUT_OF_MEMORY                 = $A005;


  ALC_MAJOR_VERSION                 = $1000;
  ALC_MINOR_VERSION                 = $1001;

  ALC_ATTRIBUTES_SIZE               = $1002;
  ALC_ALL_ATTRIBUTES                = $1003;

(**
 * The Specifier string for default device
 *)
  ALC_DEFAULT_DEVICE_SPECIFIER      = $1004;
  ALC_DEVICE_SPECIFIER              = $1005;
  ALC_EXTENSIONS                    = $1006;

  ALC_ALL_DEVICES_SPECIFIER     = $1013;
  ALC_DEFAULT_ALL_DEVICES_SPECIFIER = $1012;

(**
 * Capture extension
 *)
  ALC_CAPTURE_DEVICE_SPECIFIER      = $310;
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER = $311;
  ALC_CAPTURE_SAMPLES               = $312;



var
(*
 * Context Management
 *)
  alcCreateContext: function(device: PALCdevice; const attrlist: PALCint): PALCcontext; cdecl;
  alcMakeContextCurrent: function(context: PALCcontext): ALCboolean; cdecl;
  alcProcessContext: procedure(context: PALCcontext); cdecl;
  alcSuspendContext: procedure(context: PALCcontext); cdecl;
  alcDestroyContext: procedure(context: PALCcontext); cdecl;
  alcGetCurrentContext: function(): PALCcontext; cdecl;
  alcGetContextsDevice: function(context: PALCcontext): PALCdevice; cdecl;


(*
 * Device Management
 *)
  alcOpenDevice: function(const devicename: PALCchar): PALCdevice; cdecl;
  alcCloseDevice: function(device: PALCdevice): ALCboolean; cdecl;


(*
 * Error support.
 * Obtain the most recent Context error
 *)
  alcGetError: function(device: PALCdevice): ALCenum; cdecl;


(* 
 * Extension support.
 * Query for the presence of an extension, and obtain any appropriate
 * function pointers and enum values.
 *)
  alcIsExtensionPresent: function(device: PALCdevice; const extname: PALCchar): ALCboolean; cdecl;
  alcGetProcAddress: function(device: PALCdevice; const funcname: PALCchar): pointer; cdecl;
  alcGetEnumValue: function(device: PALCdevice; const enumname: PALCchar): ALCenum; cdecl;


(*
 * Query functions
 *)
  alcGetString: function(device: PALCdevice; param: ALCenum): PALCchar; cdecl;
  alcGetIntegerv: procedure(device: PALCdevice; param: ALCenum; size: ALCsizei; data: PALCint); cdecl;


(*
 * Capture functions
 *)
  alcCaptureOpenDevice: function(const devicename: PALCchar; frequency: ALCuint; format: ALCenum; buffersize: ALCsizei): PALCdevice; cdecl;
  alcCaptureCloseDevice: function(device: PALCdevice): ALCboolean; cdecl;
  alcCaptureStart: procedure(device: PALCdevice); cdecl;
  alcCaptureStop: procedure(device: PALCdevice); cdecl;
  alcCaptureSamples: procedure(device: PALCdevice; buffer: PALCvoid; samples: ALCsizei); cdecl;

(*
 * Log Callback
 *)
type
  TALSoft_LogCallback = procedure(aUserPtr: pointer; aLevel: char; aMessage: PChar; aMessageLength: cint);
var
  alsoft_set_log_callback: procedure(aCallback: TALSoft_LogCallback; UserPtr: pointer); cdecl;

