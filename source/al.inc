(**
 * OpenAL cross platform audio library
 * Copyright (C) 1999-2000 by authors.
 * This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301, USA.
 * Or go to http://www.gnu.org/copyleft/lgpl.html
 *)


(*
 * The OPENAL, ALAPI, ALAPIENTRY, AL_INVALID, AL_ILLEGAL_ENUM, and
 * AL_ILLEGAL_COMMAND macros are deprecated, but are included for
 * applications porting code from AL 1.0
 *)

{ OPENAL
  ALAPI AL_API
  ALAPIENTRY AL_APIENTRY
  AL_INVALID                        = (-1);
  AL_ILLEGAL_ENUM                   = AL_INVALID_ENUM;
  AL_ILLEGAL_COMMAND                = AL_INVALID_OPERATION;

  AL_VERSION_1_0
  AL_VERSION_1_1}


type

(** 8-bit boolean *)
  ALboolean                         = cbool;
  PALboolean                        = pcbool;

(** character *)
  ALchar                            = cchar;
  PALchar                           = PChar; //pcchar;

(** signed 8-bit 2's complement integer *)
  ALbyte                            = cint8;
  PALbyte                           = pcint8;

(** unsigned 8-bit integer *)
  ALubyte                           = cuint8;
  PALubyte                          = pcuint8;

(** signed 16-bit 2's complement integer *)
  ALshort                           = cint16;
  PALshort                          = pcint16;

(** unsigned 16-bit integer *)
  ALushort                          = cuint16;
  PALushort                         = pcuint16;

(** signed 32-bit 2's complement integer *)
  ALint                             = cint32;
  PALint                            = pcint32;

(** unsigned 32-bit integer *)
  ALuint                            = cuint32;
  PALuint                           = pcuint32;

(** non-negative 32-bit binary integer size *)
  ALsizei                           = cint32;
  PALsizei                          = pcint32;

(** enumerated 32-bit value *)
  ALenum                            = cint32;
  PALenum                           = pcint32;

(** 32-bit IEEE754 floating-point *)
  ALfloat                           = cfloat;
  PALfloat                          = pcfloat;

(** 64-bit IEEE754 floating-point *)
  ALdouble                          = cdouble;
  PALdouble                         = pcdouble;

(** void type (for opaque pointers only) *)
  PALvoid                           = pointer;
  PPALvoid                          = ^PALvoid;


(* Enumerant values begin at column 50. No tabs. *)

const
(* "no distance model" or "no buffer" *)
  AL_NONE                           = 0;

(* Boolean False. *)
  AL_FALSE                          = 0;

(** Boolean True. *)
  AL_TRUE                           = 1;

(** Indicate Source has relative coordinates. *)
  AL_SOURCE_RELATIVE                = $202;



(**
 * Directional source, inner cone angle, in degrees.
 * Range:    [0-360] 
 * Default:  360
 *)
  AL_CONE_INNER_ANGLE               = $1001;

(**
 * Directional source, outer cone angle, in degrees.
 * Range:    [0-360] 
 * Default:  360
 *)
  AL_CONE_OUTER_ANGLE               = $1002;

(**
 * Specify the pitch to be applied, either at source,
 *  or on mixer results, at listener.
 * Range:   [0.5-2.0]
 * Default: 1.0
 *)
  AL_PITCH                          = $1003;
  
(** 
 * Specify the current location in three dimensional space.
 * OpenAL, like OpenGL, uses a right handed coordinate system,
 *  where in a frontal default view X (thumb) points right, 
 *  Y points up (index finger), and Z points towards the
 *  viewer/camera (middle finger). 
 * To switch from a left handed coordinate system, flip the
 *  sign on the Z coordinate.
 * Listener position is always in the world coordinate system.
 *) 
  AL_POSITION                       = $1004;
  
(** Specify the current direction. *)
  AL_DIRECTION                      = $1005;
  
(** Specify the current velocity in three dimensional space. *)
  AL_VELOCITY                       = $1006;

(**
 * Indicate whether source is looping.
 * Type: ALboolean?
 * Range:   [AL_TRUE, AL_FALSE]
 * Default: FALSE.
 *)
  AL_LOOPING                        = $1007;

(**
 * Indicate the buffer to provide sound samples. 
 * Type: ALuint.
 * Range: any valid Buffer id.
 *)
  AL_BUFFER                         = $1009;
  
(**
 * Indicate the gain (volume amplification) applied. 
 * Type:   ALfloat.
 * Range:  ]0.0-  ]
 * A value of 1.0 means un-attenuated/unchanged.
 * Each division by 2 equals an attenuation of -6dB.
 * Each multiplicaton with 2 equals an amplification of +6dB.
 * A value of 0.0 is meaningless with respect to a logarithmic
 *  scale; it is interpreted as zero volume - the channel
 *  is effectively disabled.
 *)
  AL_GAIN                           = $100A;

(*
 * Indicate minimum source attenuation
 * Type: ALfloat
 * Range:  [0.0 - 1.0]
 *
 * Logarthmic
 *)
  AL_MIN_GAIN                       = $100D;

(**
 * Indicate maximum source attenuation
 * Type: ALfloat
 * Range:  [0.0 - 1.0]
 *
 * Logarthmic
 *)
  AL_MAX_GAIN                       = $100E;

(**
 * Indicate listener orientation.
 *
 * at/up 
 *)
  AL_ORIENTATION                    = $100F;

(**
 * Source state information.
 *)
  AL_SOURCE_STATE                   = $1010;
  AL_INITIAL                        = $1011;
  AL_PLAYING                        = $1012;
  AL_PAUSED                         = $1013;
  AL_STOPPED                        = $1014;

(**
 * Buffer Queue params
 *)
  AL_BUFFERS_QUEUED                 = $1015;
  AL_BUFFERS_PROCESSED              = $1016;

(**
 * source specific reference distance
 * Type: ALfloat
 * Range:  0.0 - +inf
 *
 * At 0.0, no distance attenuation occurs.  Default is
 * 1.0.
 *)
  AL_REFERENCE_DISTANCE             = $1020;

(**
 * source specific rolloff factor
 * Type: ALfloat
 * Range:  0.0 - +inf
 *
 *)
  AL_ROLLOFF_FACTOR                 = $1021;

(**
 * Directional source, outer cone gain.
 *
 * Default:  0.0
 * Range:    [0.0 - 1.0]
 * Logarithmic
 *)
  AL_CONE_OUTER_GAIN                = $1022;

(**
 * Indicate distance above which sources are not
 * attenuated using the inverse clamped distance model.
 *
 * Default: +inf
 * Type: ALfloat
 * Range:  0.0 - +inf
 *)
  AL_MAX_DISTANCE                   = $1023;

    (**
 * Source buffer position information
 *)
  AL_SEC_OFFSET                     = $1024;
  AL_SAMPLE_OFFSET                  = $1025;
  AL_BYTE_OFFSET                    = $1026;

(*
 * Source type (Static, Streaming or undetermined)
 * Source is Static if a Buffer has been attached using AL_BUFFER
 * Source is Streaming if one or more Buffers have been attached using alSourceQueueBuffers
 * Source is undetermined when it has the NULL buffer attached
 *)
  AL_SOURCE_TYPE                    = $1027;
  AL_STATIC                         = $1028;
  AL_STREAMING                      = $1029;
  AL_UNDETERMINED                   = $1030;

(** Sound samples: format specifier. *)
  AL_FORMAT_MONO8                   = $1100;
  AL_FORMAT_MONO16                  = $1101;
  AL_FORMAT_STEREO8                 = $1102;
  AL_FORMAT_STEREO16                = $1103;

(**
 * Sound samples: frequency, in units of Hertz [Hz].
 * This is the number of samples per second. Half of the
 *  sample frequency marks the maximum significant
 *  frequency component.
 *)
  AL_FREQUENCY                      = $2001;
  AL_BITS                           = $2002;
  AL_CHANNELS                       = $2003;
  AL_SIZE                           = $2004;

(**
 * Buffer state.
 *
 * Not supported for public use (yet).
 *)
  AL_UNUSED                         = $2010;
  AL_PENDING                        = $2011;
  AL_PROCESSED                      = $2012;

(** Errors: No Error. *)
  AL_NO_ERROR                       = 0;

(** 
 * Invalid Name paramater passed to AL call.
 *)
  AL_INVALID_NAME                   = $A001;

(** 
 * Invalid parameter passed to AL call.
 *)
  AL_INVALID_ENUM                   = $A002;

(** 
 * Invalid enum parameter value.
 *)
  AL_INVALID_VALUE                  = $A003;

(** 
 * Illegal call.
 *)
  AL_INVALID_OPERATION              = $A004;

  
(**
 * No mojo.
 *)
  AL_OUT_OF_MEMORY                  = $A005;


(** Context strings: Vendor Name. *)
  AL_VENDOR                         = $B001;
  AL_VERSION                        = $B002;
  AL_RENDERER                       = $B003;
  AL_EXTENSIONS                     = $B004;

(** Global tweakage. *)

(**
 * Doppler scale.  Default 1.0
 *)
  AL_DOPPLER_FACTOR                 = $C000;

(**
 * Tweaks speed of propagation.
 *)
  AL_DOPPLER_VELOCITY               = $C001;

(**
 * Speed of Sound in units per second
 *)
  AL_SPEED_OF_SOUND                 = $C003;

(**
 * Distance models
 *
 * used in conjunction with DistanceModel
 *
 * implicit: NONE, which disances distance attenuation.
 *)
  AL_DISTANCE_MODEL                 = $D000;
  AL_INVERSE_DISTANCE               = $D001;
  AL_INVERSE_DISTANCE_CLAMPED       = $D002;
  AL_LINEAR_DISTANCE                = $D003;
  AL_LINEAR_DISTANCE_CLAMPED        = $D004;
  AL_EXPONENT_DISTANCE              = $D005;
  AL_EXPONENT_DISTANCE_CLAMPED      = $D006;


var
(*
 * Renderer State management
 *)
  alEnable: procedure(capability: ALenum); cdecl;
  alDisable: procedure(capability: ALenum); cdecl;
  alIsEnabled: function(capability: ALenum): ALboolean; cdecl;


(*
 * State retrieval
 *)
  alGetString: function(param: ALenum): PALchar; cdecl;
  alGetBooleanv: procedure(param: ALenum; data: PALboolean); cdecl;
  alGetIntegerv: procedure(param: ALenum; data: PALint); cdecl;
  alGetFloatv: procedure(param: ALenum; data: PALfloat); cdecl;
  alGetDoublev: procedure(param: ALenum; data: PALdouble); cdecl;
  alGetBoolean: function(param: ALenum): ALboolean; cdecl;
  alGetInteger: function(param: ALenum): ALint; cdecl;
  alGetFloat: function(param: ALenum): ALfloat; cdecl;
  alGetDouble: function(param: ALenum): ALdouble; cdecl;


(*
 * Error support.
 * Obtain the most recent error generated in the AL state machine.
 *)
  alGetError: function():ALenum; cdecl;


(* 
 * Extension support.
 * Query for the presence of an extension, and obtain any appropriate
 * function pointers and enum values.
 *)
  alIsExtensionPresent: function(const extname: PALChar): ALboolean; cdecl;
  alGetProcAddress: function(const fname: PALChar): pointer; cdecl;
  alGetEnumValue: function(const ename: PALChar): ALenum; cdecl;


(*
 * LISTENER
 * Listener represents the location and orientation of the
 * 'user' in 3D-space.
 *
 * Properties include: -
 *
 * Gain         AL_GAIN         ALfloat
 * Position     AL_POSITION     ALfloat[3]
 * Velocity     AL_VELOCITY     ALfloat[3]
 * Orientation  AL_ORIENTATION  ALfloat[6] (Forward then Up vectors)
*)

(*
 * Set Listener parameters
 *)
  alListenerf: procedure(param: ALenum; value: ALfloat); cdecl;
  alListener3f: procedure(param: ALenum; value1, value2, value3: ALfloat); cdecl;
  alListenerfv: procedure(param: ALenum; const values: PALfloat); cdecl;
  alListeneri: procedure(param: ALenum; value: ALint); cdecl;
  alListener3i: procedure(param: ALenum; value1, value2, value3: ALint); cdecl;
  alListeneriv: procedure(param: ALenum; const values: PALint); cdecl;


(*
 * Get Listener parameters
 *)
  alGetListenerf: procedure(param: ALenum; var value: ALfloat); cdecl;
  alGetListener3f: procedure(param: ALenum; var value1, value2, value3: ALfloat); cdecl;
  alGetListenerfv: procedure(param: ALenum; values: PALfloat); cdecl;
  alGetListeneri: procedure(param: ALenum; var value: ALint); cdecl;
  alGetListener3i: procedure(param: ALenum; var value1, value2, value3: ALint); cdecl;
  alGetListeneriv: procedure(param: ALenum; values: PALint); cdecl;


(**
 * SOURCE
 * Sources represent individual sound objects in 3D-space.
 * Sources take the PCM data provided in the specified Buffer,
 * apply Source-specific modifications, and then
 * submit them to be mixed according to spatial arrangement etc.
 * 
 * Properties include: -
 *
 * Gain                              AL_GAIN                 ALfloat
 * Min Gain                          AL_MIN_GAIN             ALfloat
 * Max Gain                          AL_MAX_GAIN             ALfloat
 * Position                          AL_POSITION             ALfloat[3]
 * Velocity                          AL_VELOCITY             ALfloat[3]
 * Direction                         AL_DIRECTION            ALfloat[3]
 * Head Relative Mode                AL_SOURCE_RELATIVE      ALint (AL_TRUE or AL_FALSE)
 * Reference Distance                AL_REFERENCE_DISTANCE   ALfloat
 * Max Distance                      AL_MAX_DISTANCE         ALfloat
 * RollOff Factor                    AL_ROLLOFF_FACTOR       ALfloat
 * Inner Angle                       AL_CONE_INNER_ANGLE     ALint or ALfloat
 * Outer Angle                       AL_CONE_OUTER_ANGLE     ALint or ALfloat
 * Cone Outer Gain                   AL_CONE_OUTER_GAIN      ALint or ALfloat
 * Pitch                             AL_PITCH                ALfloat
 * Looping                           AL_LOOPING              ALint (AL_TRUE or AL_FALSE)
 * MS Offset                         AL_MSEC_OFFSET          ALint or ALfloat
 * Byte Offset                       AL_BYTE_OFFSET          ALint or ALfloat
 * Sample Offset                     AL_SAMPLE_OFFSET        ALint or ALfloat
 * Attached Buffer                   AL_BUFFER               ALint
 * State (Query only)                AL_SOURCE_STATE         ALint
 * Buffers Queued (Query only)       AL_BUFFERS_QUEUED       ALint
 * Buffers Processed (Query only)    AL_BUFFERS_PROCESSED    ALint
 *)

(* Create Source objects *)
  alGenSources: procedure(n: ALsizei; sources: PALuint); cdecl;

(* Delete Source objects *)
  alDeleteSources: procedure(n: ALsizei; const sources: PALuint); cdecl;

(* Verify a handle is a valid Source *) 
  alIsSource: function(sid: ALuint): ALboolean; cdecl;


(*
 * Set Source parameters
 *)
  alSourcef: procedure(sid: ALuint; param: ALenum; value: ALfloat); cdecl;
  alSource3f: procedure(sid: ALuint; param: ALenum; value1, value2, value3: ALfloat); cdecl;
  alSourcefv: procedure(sid: ALuint; param: ALenum; const values: PALfloat); cdecl;
  alSourcei: procedure(sid: ALuint; param: ALenum; value: ALint); cdecl;
  alSource3i: procedure(sid: ALuint; param: ALenum; value1, value2, value3: ALint); cdecl;
  alSourceiv: procedure(sid: ALuint; param: ALenum; const values: PALint); cdecl;

(*
 * Get Source parameters
 *)
  alGetSourcef: procedure(sid: ALuint; param: ALenum; var value: ALfloat); cdecl;
  alGetSource3f: procedure(sid: ALuint; param: ALenum; var value1, value2, value3: ALfloat); cdecl;
  alGetSourcefv: procedure(sid: ALuint; param: ALenum; values: PALfloat); cdecl;
  alGetSourcei: procedure(sid: ALuint; param: ALenum; var value: ALint); cdecl;
  alGetSource3i: procedure(sid: ALuint; param: ALenum; var value1, value2, value3: ALint); cdecl;
  alGetSourceiv: procedure(sid: ALuint; param: ALenum; values: PALint); cdecl;


(*
 * Source vector based playback calls
 *)

(* Play, replay, or resume (if paused) a list of Sources *)
  alSourcePlayv: procedure(ns: ALsizei; const sids: PALuint); cdecl;

(* Stop a list of Sources *)
  alSourceStopv: procedure(ns: ALsizei; const sids: PALuint); cdecl;

(* Rewind a list of Sources *)
  alSourceRewindv: procedure(ns: ALsizei; const sids: PALuint); cdecl;

(* Pause a list of Sources *)
  alSourcePausev: procedure(ns: ALsizei; const sids: PALuint); cdecl;

(*
 * Source based playback calls
 *)

(* Play, replay, or resume a Source *)
  alSourcePlay: procedure(sid: ALuint); cdecl;

(* Stop a Source *)
  alSourceStop: procedure(sid: ALuint); cdecl;

(* Rewind a Source (set playback postiton to beginning) *)
  alSourceRewind: procedure(sid: ALuint); cdecl;

(* Pause a Source *)
  alSourcePause: procedure(sid: ALuint); cdecl;

(*
 * Source Queuing 
 *)
  alSourceQueueBuffers: procedure(sid: ALuint; numEntries: ALsizei; const bids: PALuint); cdecl;

  alSourceUnqueueBuffers: procedure(sid: ALuint; numEntries: ALsizei; bids: PALuint); cdecl;


(**
 * BUFFER
 * Buffer objects are storage space for sample data.
 * Buffers are referred to by Sources. One Buffer can be used
 * by multiple Sources.
 *
 * Properties include: -
 *
 * Frequency (Query only)    AL_FREQUENCY      ALint
 * Size (Query only)         AL_SIZE           ALint
 * Bits (Query only)         AL_BITS           ALint
 * Channels (Query only)     AL_CHANNELS       ALint
 *)

(* Create Buffer objects *)
  alGenBuffers: procedure(n: ALsizei; buffers: PALuint); cdecl;

(* Delete Buffer objects *)
  alDeleteBuffers: procedure(n: ALsizei; const buffers: PALuint); cdecl;

(* Verify a handle is a valid Buffer *)
  alIsBuffer: function(bid: ALuint): ALboolean; cdecl;

(* Specify the data to be copied into a buffer *)
  alBufferData: procedure(bid: ALuint; format: ALenum; data: PALvoid; size: ALsizei; freq: ALsizei); cdecl;

(*
 * Set Buffer parameters
 *)
  alBufferf: procedure(bid: ALuint; param: ALenum; value: ALfloat); cdecl;
  alBuffer3f: procedure(bid: ALuint; param: ALenum; value1, value2, value3: ALfloat); cdecl;
  alBufferfv: procedure(bid: ALuint; param: ALenum; const values: PALfloat); cdecl;
  alBufferi: procedure(bid: ALuint; param: ALenum; value: ALint); cdecl;
  alBuffer3i: procedure(bid: ALuint; param: ALenum; value1, value2, value3: ALint); cdecl;
  alBufferiv: procedure(bid: ALuint; param: ALenum; const values: ALint); cdecl;

(*
 * Get Buffer parameters
 *)
  alGetBufferf: procedure(bid: ALuint; param: ALenum; var value: ALfloat); cdecl;
  alGetBuffer3f: procedure(bid: ALuint; param: ALenum; var value1, value2, value3: ALfloat); cdecl;
  alGetBufferfv: procedure(bid: ALuint; param: ALenum; values: PALfloat); cdecl;
  alGetBufferi: procedure(bid: ALuint; param: ALenum; var value: ALint); cdecl;
  alGetBuffer3i: procedure(bid: ALuint; param: ALenum; var value1, value2, value3: ALint); cdecl;
  alGetBufferiv: procedure(bid: ALuint; param: ALenum; values: ALint); cdecl;


(*
 * Global Parameters
 *)
  alDopplerFactor: procedure(value: ALfloat); cdecl;
  alDopplerVelocity: procedure(value: ALfloat); cdecl; // deprecated
  alSpeedOfSound: procedure(value: ALfloat); cdecl;
  alDistanceModel: procedure(distanceModel: ALenum); cdecl;


