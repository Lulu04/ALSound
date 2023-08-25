{**
 * OpenAL cross platform audio library
 * Copyright (C) 2008 by authors.
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
 *  Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * Or go to http://www.gnu.org/copyleft/lgpl.html
 *}


type
// Define int64 and uint64 types */
 alsoft_int64_t = Int64;
 alsoft_uint64_t = QWord;


const

 AL_FORMAT_IMA_ADPCM_MONO16_EXT=$10000;
 AL_FORMAT_IMA_ADPCM_STEREO16_EXT=$10001;

 AL_FORMAT_WAVE_EXT=$10002;

 AL_FORMAT_VORBIS_EXT=$10003;

 AL_FORMAT_QUAD8_LOKI=$10004;
 AL_FORMAT_QUAD16_LOKI=$10005;

 AL_FORMAT_MONO_FLOAT32=$10010;
 AL_FORMAT_STEREO_FLOAT32=$10011;

 AL_FORMAT_MONO_DOUBLE_EXT=$10012;
 AL_FORMAT_STEREO_DOUBLE_EXT=$10013;

 AL_FORMAT_MONO_MULAW_EXT=$10014;
 AL_FORMAT_STEREO_MULAW_EXT=$10015;

 AL_FORMAT_MONO_ALAW_EXT=$10016;
 AL_FORMAT_STEREO_ALAW_EXT=$10017;

 ALC_CHAN_MAIN_LOKI=$500001;
 ALC_CHAN_PCM_LOKI=$500002;
 ALC_CHAN_CD_LOKI=$500003;


{* Provides support for surround sound buffer formats with 8, 16, and 32-bit
 * samples.
 *
 * QUAD8: Unsigned 8-bit, Quadraphonic (Front Left, Front Right, Rear Left,
 *        Rear Right).
 * QUAD16: Signed 16-bit, Quadraphonic.
 * QUAD32: 32-bit float, Quadraphonic.
 * REAR8: Unsigned 8-bit, Rear Stereo (Rear Left, Rear Right).
 * REAR16: Signed 16-bit, Rear Stereo.
 * REAR32: 32-bit float, Rear Stereo.
 * 51CHN8: Unsigned 8-bit, 5.1 Surround (Front Left, Front Right, Front Center,
 *         LFE, Side Left, Side Right). Note that some audio systems may label
 *         5.1's Side channels as Rear or Surround; they are equivalent for the
 *         purposes of this extension.
 * 51CHN16: Signed 16-bit, 5.1 Surround.
 * 51CHN32: 32-bit float, 5.1 Surround.
 * 61CHN8: Unsigned 8-bit, 6.1 Surround (Front Left, Front Right, Front Center,
 *         LFE, Rear Center, Side Left, Side Right).
 * 61CHN16: Signed 16-bit, 6.1 Surround.
 * 61CHN32: 32-bit float, 6.1 Surround.
 * 71CHN8: Unsigned 8-bit, 7.1 Surround (Front Left, Front Right, Front Center,
 *         LFE, Rear Left, Rear Right, Side Left, Side Right).
 * 71CHN16: Signed 16-bit, 7.1 Surround.
 * 71CHN32: 32-bit float, 7.1 Surround.
 *}
 AL_FORMAT_QUAD8=$1204;
 AL_FORMAT_QUAD16=$1205;
 AL_FORMAT_QUAD32=$1206;
 AL_FORMAT_REAR8=$1207;
 AL_FORMAT_REAR16=$1208;
 AL_FORMAT_REAR32=$1209;
 AL_FORMAT_51CHN8=$120A;
 AL_FORMAT_51CHN16=$120B;
 AL_FORMAT_51CHN32=$120C;
 AL_FORMAT_61CHN8=$120D;
 AL_FORMAT_61CHN16=$120E;
 AL_FORMAT_61CHN32=$120F;
 AL_FORMAT_71CHN8=$1210;
 AL_FORMAT_71CHN16=$1211;
 AL_FORMAT_71CHN32=$1212;

 AL_FORMAT_MONO_MULAW=$10014;
 AL_FORMAT_STEREO_MULAW=$10015;
 AL_FORMAT_QUAD_MULAW=$10021;
 AL_FORMAT_REAR_MULAW=$10022;
 AL_FORMAT_51CHN_MULAW=$10023;
 AL_FORMAT_61CHN_MULAW=$10024;
 AL_FORMAT_71CHN_MULAW=$10025;

 AL_FORMAT_MONO_IMA4=$1300;
 AL_FORMAT_STEREO_IMA4=$1301;

var
//AL_API void AL_APIENTRY alBufferDataStatic(const ALint buffer, ALenum format, ALvoid *data, ALsizei len, ALsizei freq);
  alBufferDataStatic: procedure(const buffer: ALint; format: ALEnum; data: PALvoid; len, freq: ALsizei); cdecl;

const
 ALC_CONNECTED=$313;


type
 TProc_alcSetThreadContext = function(context: PALCcontext): ALCboolean; cdecl;
 TProc_alcGetThreadContext = function(): PALCcontext; cdecl;
var
//ALC_API ALCboolean  ALC_APIENTRY alcSetThreadContext(ALCcontext *context);
//ALC_API ALCcontext* ALC_APIENTRY alcGetThreadContext(void);
  alcSetThreadContext: TProc_alcSetThreadContext; //function(context: PALCcontext): ALCboolean; cdecl;
  alcGetThreadContext: TProc_alcGetThreadContext; //function(): PALCcontext; cdecl;

const
 AL_SOURCE_DISTANCE_MODEL=$200;

 AL_BYTE_RW_OFFSETS_SOFT=$1031;
 AL_SAMPLE_RW_OFFSETS_SOFT=$1032;

var
//AL_API void AL_APIENTRY alBufferSubDataSOFT(ALuint buffer,ALenum format,const ALvoid *data,ALsizei offset,ALsizei length);
  alBufferSubDataSOFT: procedure(buffer: ALint; format: ALenum; const data: PALvoid; offset, length: ALsizei); cdecl;


const
 AL_LOOP_POINTS_SOFT=$2015;

 AL_EXT_FOLDBACK_NAME='AL_EXT_FOLDBACK';
 AL_FOLDBACK_EVENT_BLOCK=$4112;
 AL_FOLDBACK_EVENT_START=$4111;
 AL_FOLDBACK_EVENT_STOP =$4113;
 AL_FOLDBACK_MODE_MONO=$4101;
 AL_FOLDBACK_MODE_STEREO=$4102;

type
//typedef void (AL_APIENTRY*LPALFOLDBACKCALLBACK)(ALenum,ALsizei);

  LPALFOLDBACKCALLBACK=procedure(en: ALenum; si: ALsizei); cdecl;
//AL_API void AL_APIENTRY alRequestFoldbackStart(ALenum mode,ALsizei count,ALsizei length,ALfloat *mem,LPALFOLDBACKCALLBACK callback);
//AL_API void AL_APIENTRY alRequestFoldbackStop(void);
var
  alRequestFoldbackStart: procedure(mode: ALenum; count, length: ALsizei; mem: PALfloat; callback: LPALFOLDBACKCALLBACK); cdecl;
  alRequestFoldbackStop: procedure(); cdecl;

const
 AL_DEDICATED_GAIN=$0001;
 AL_EFFECT_DEDICATED_DIALOGUE=$9001;
 AL_EFFECT_DEDICATED_LOW_FREQUENCY_EFFECT=$9000;


// Channel configurations
 AL_MONO_SOFT=$1500;
 AL_STEREO_SOFT=$1501;
 AL_REAR_SOFT=$1502;
 AL_QUAD_SOFT=$1503;
 AL_5POINT1_SOFT=$1504;
 AL_6POINT1_SOFT=$1505;
 AL_7POINT1_SOFT=$1506;

// Sample types
 AL_BYTE_SOFT=$1400;
 AL_UNSIGNED_BYTE_SOFT=$1401;
 AL_SHORT_SOFT=$1402;
 AL_UNSIGNED_SHORT_SOFT=$1403;
 AL_INT_SOFT=$1404;
 AL_UNSIGNED_INT_SOFT=$1405;
 AL_FLOAT_SOFT=$1406;
 AL_DOUBLE_SOFT=$1407;
 AL_BYTE3_SOFT=$1408;
 AL_UNSIGNED_BYTE3_SOFT=$1409;

// Storage formats
 AL_MONO8_SOFT=$1100;
 AL_MONO16_SOFT=$1101;
 AL_MONO32F_SOFT=$10010;
 AL_STEREO8_SOFT=$1102;
 AL_STEREO16_SOFT=$1103;
 AL_STEREO32F_SOFT=$10011;
 AL_QUAD8_SOFT=$1204;
 AL_QUAD16_SOFT=$1205;
 AL_QUAD32F_SOFT=$1206;
 AL_REAR8_SOFT=$1207;
 AL_REAR16_SOFT=$1208;
 AL_REAR32F_SOFT=$1209;
 AL_5POINT1_8_SOFT=$120A;
 AL_5POINT1_16_SOFT=$120B;
 AL_5POINT1_32F_SOFT=$120C;
 AL_6POINT1_8_SOFT=$120D;
 AL_6POINT1_16_SOFT=$120E;
 AL_6POINT1_32F_SOFT=$120F;
 AL_7POINT1_8_SOFT=$1210;
 AL_7POINT1_16_SOFT=$1211;
 AL_7POINT1_32F_SOFT=$1212;

// Buffer attributes
 AL_INTERNAL_FORMAT_SOFT=$2008;
 AL_BYTE_LENGTH_SOFT=$2009;
 AL_SAMPLE_LENGTH_SOFT=$200A;
 AL_SEC_LENGTH_SOFT=$200B;

var
  alBufferSamplesSOFT: procedure(buffer: ALuint; samplerate: ALuint; internalformat: ALenum; samples: ALsizei; channels, _type: ALenum; const data: PALvoid); cdecl;
  alBufferSubSamplesSOFT: procedure(buffer: ALuint; offset, samples: ALsizei; channels, _type: ALenum; const data: PALvoid); cdecl;
  alGetBufferSamplesSOFT: procedure(buffer: ALuint; offset, samples: ALsizei; channels, _type: ALenum; data: PALvoid); cdecl;
  alIsBufferFormatSupportedSOFT: function(format: ALenum): ALboolean; cdecl;


const
 AL_DIRECT_CHANNELS_SOFT=$1033;

 // LOOPBACK
 ALC_FORMAT_CHANNELS_SOFT=$1990;
 ALC_FORMAT_TYPE_SOFT=$1991;

// Sample types
 ALC_BYTE_SOFT=$1400;
 ALC_UNSIGNED_BYTE_SOFT=$1401;
 ALC_SHORT_SOFT=$1402;
 ALC_UNSIGNED_SHORT_SOFT=$1403;
 ALC_INT_SOFT=$1404;
 ALC_UNSIGNED_INT_SOFT=$1405;
 ALC_FLOAT_SOFT=$1406;

// Channel configurations
 ALC_MONO_SOFT=$1500;
 ALC_STEREO_SOFT=$1501;
 ALC_QUAD_SOFT=$1503;
 ALC_5POINT1_SOFT=$1504;
 ALC_6POINT1_SOFT=$1505;
 ALC_7POINT1_SOFT=$1506;

type
  TProc_alcLoopbackOpenDeviceSOFT = function(const deviceName: PALCchar): PALCdevice; cdecl;
  TProc_alcIsRenderFormatSupportedSOFT = function(device: PALCdevice; freq: ALCsizei; channels, _type: ALCenum): ALCboolean; cdecl;
  TProc_alcRenderSamplesSOFT = procedure(device: PALCdevice; buffer: PALCvoid; samples: ALCsizei); cdecl;
var
  alcLoopbackOpenDeviceSOFT: TProc_alcLoopbackOpenDeviceSOFT;
  alcIsRenderFormatSupportedSOFT: TProc_alcIsRenderFormatSupportedSOFT;
  alcRenderSamplesSOFT: TProc_alcRenderSamplesSOFT;

const
 AL_STEREO_ANGLES=$1030;

 AL_SOURCE_RADIUS=$1031;


 AL_SAMPLE_OFFSET_LATENCY_SOFT=$1200;
 AL_SEC_OFFSET_LATENCY_SOFT=$1201;
type
  ALint64SOFT = alsoft_int64_t;
  PALint64SOFT = ^ALint64SOFT;
  ALuint64SOFT = alsoft_uint64_t;

var
  alSourcedSOFT: procedure( source: ALuint; param: ALenum; value: ALdouble); cdecl;
  alSource3dSOFT: procedure( source: ALuint; param: ALenum; value1, value2, value3: ALdouble); cdecl;
  alSourcedvSOFT: procedure( source: ALuint; param: ALenum; const values: PALdouble); cdecl;
  alGetSourcedSOFT: procedure( source: ALuint; param: ALenum; value: PALdouble); cdecl;
  alGetSource3dSOFT: procedure( source: ALuint; param: ALenum; value1, value2, value3: PALdouble); cdecl;
  alGetSourcedvSOFT: procedure( source: ALuint; param: ALenum; values: PALdouble); cdecl;
  alSourcei64SOFT: procedure( source: ALuint; param: ALenum; value: ALint64SOFT); cdecl;
  alSource3i64SOFT: procedure( source: ALuint; param: ALenum; value1, value2, value3: ALint64SOFT); cdecl;
  alSourcei64vSOFT: procedure( source: ALuint; param: ALenum; const values: PALint64SOFT); cdecl;
  alGetSourcei64SOFT: procedure( source: ALuint; param: ALenum; value: PALint64SOFT); cdecl;
  alGetSource3i64SOFT: procedure( source: ALuint; param: ALenum; value1, value2, value3: PALint64SOFT); cdecl;
  alGetSourcei64vSOFT: procedure( source: ALuint; param: ALenum; values: PALint64SOFT); cdecl;


const
 ALC_DEFAULT_FILTER_ORDER=$1100;

 AL_DEFERRED_UPDATES_SOFT=$C002;

var
  alDeferUpdatesSOFT: procedure(); cdecl;
  alProcessUpdatesSOFT: procedure(); cdecl;

const
 AL_UNPACK_BLOCK_ALIGNMENT_SOFT=$200C;
 AL_PACK_BLOCK_ALIGNMENT_SOFT=$200D;


 AL_FORMAT_MONO_MSADPCM_SOFT=$1302;
 AL_FORMAT_STEREO_MSADPCM_SOFT=$1303;


//#define AL_BYTE_LENGTH_SOFT=$2009;
//#define AL_SAMPLE_LENGTH_SOFT=$200A;
//#define AL_SEC_LENGTH_SOFT=$200B;

type
  TProc_alcDevicePauseSOFT = procedure(device: PALCdevice); cdecl;
  TProc_alcDeviceResumeSOFT = procedure(device: PALCdevice); cdecl;
var
  alcDevicePauseSOFT: TProc_alcDevicePauseSOFT;
  alcDeviceResumeSOFT: TProc_alcDeviceResumeSOFT;


const
{* Provides support for B-Format ambisonic buffers (first-order, FuMa scaling
 * and layout).
 *
 * BFORMAT2D_8: Unsigned 8-bit, 3-channel non-periphonic (WXY).
 * BFORMAT2D_16: Signed 16-bit, 3-channel non-periphonic (WXY).
 * BFORMAT2D_FLOAT32: 32-bit float, 3-channel non-periphonic (WXY).
 * BFORMAT3D_8: Unsigned 8-bit, 4-channel periphonic (WXYZ).
 * BFORMAT3D_16: Signed 16-bit, 4-channel periphonic (WXYZ).
 * BFORMAT3D_FLOAT32: 32-bit float, 4-channel periphonic (WXYZ).
 *}
 AL_FORMAT_BFORMAT2D_8=$20021;
 AL_FORMAT_BFORMAT2D_16=$20022;
 AL_FORMAT_BFORMAT2D_FLOAT32=$20023;
 AL_FORMAT_BFORMAT3D_8=$20031;
 AL_FORMAT_BFORMAT3D_16=$20032;
 AL_FORMAT_BFORMAT3D_FLOAT32=$20033;


 AL_FORMAT_BFORMAT2D_MULAW=$10031;
 AL_FORMAT_BFORMAT3D_MULAW=$10032;


 ALC_HRTF_SOFT=$1992;
 ALC_DONT_CARE_SOFT=$0002;
 ALC_HRTF_STATUS_SOFT=$1993;
 ALC_HRTF_DISABLED_SOFT=$0000;
 ALC_HRTF_ENABLED_SOFT=$0001;
 ALC_HRTF_DENIED_SOFT=$0002;
 ALC_HRTF_REQUIRED_SOFT=$0003;
 ALC_HRTF_HEADPHONES_DETECTED_SOFT=$0004;
 ALC_HRTF_UNSUPPORTED_FORMAT_SOFT=$0005;
 ALC_NUM_HRTF_SPECIFIERS_SOFT=$1994;
 ALC_HRTF_SPECIFIER_SOFT=$1995;
 ALC_HRTF_ID_SOFT=$1996;

type
  TProc_alcGetStringiSOFT = function(device: PALCdevice; paramName: ALCenum; index: ALCsizei): PALCchar; cdecl;
  TProc_alcResetDeviceSOFT = function(device: PALCdevice; const attribs: PALCint): ALCboolean; cdecl;
var
  alcGetStringiSOFT: TProc_alcGetStringiSOFT;
  alcResetDeviceSOFT: TProc_alcResetDeviceSOFT;

const
 AL_GAIN_LIMIT_SOFT=$200E;


 AL_NUM_RESAMPLERS_SOFT=$1210;
 AL_DEFAULT_RESAMPLER_SOFT=$1211;
 AL_SOURCE_RESAMPLER_SOFT=$1212;
 AL_RESAMPLER_NAME_SOFT=$1213;

var
  alGetStringiSOFT: function(pname: ALenum; index: ALsizei): PALchar; cdecl;

const
 AL_SOURCE_SPATIALIZE_SOFT=$1214;
 AL_AUTO_SOFT=$0002;


 ALC_OUTPUT_LIMITER_SOFT=$199A;

type
  ALCint64SOFT=alsoft_int64_t;
  PALCint64SOFT = ^ALCint64SOFT;
  ALCuint64SOFT=alsoft_uint64_t;
  TProc_alcGetInteger64vSOFT = procedure(device: PALCdevice; pname: ALCenum; size: ALsizei; values: PALCint64SOFT); cdecl;
const
 ALC_DEVICE_CLOCK_SOFT=$1600;
 ALC_DEVICE_LATENCY_SOFT=$1601;
 ALC_DEVICE_CLOCK_LATENCY_SOFT=$1602;
 AL_SAMPLE_OFFSET_CLOCK_SOFT=$1202;
 AL_SEC_OFFSET_CLOCK_SOFT=$1203;

var
  alcGetInteger64vSOFT: TProc_alcGetInteger64vSOFT;


const
 AL_DROP_UNMATCHED_SOFT=$0001;
 AL_REMIX_UNMATCHED_SOFT=$0002;

 AL_AMBISONIC_LAYOUT_SOFT=$1997;
 AL_AMBISONIC_SCALING_SOFT=$1998;

// Ambisonic layouts
 AL_FUMA_SOFT=$0000;
 AL_ACN_SOFT=$0001;

// Ambisonic scalings (normalization)
//*#define AL_FUMA_SOFT*/
 AL_SN3D_SOFT=$0001;
 AL_N3D_SOFT=$0002;


 ALC_AMBISONIC_LAYOUT_SOFT=$1997;
 ALC_AMBISONIC_SCALING_SOFT=$1998;
 ALC_AMBISONIC_ORDER_SOFT=$1999;
 ALC_MAX_AMBISONIC_ORDER_SOFT=$199B;

 ALC_BFORMAT3D_SOFT=$1507;

// Ambisonic layouts
 ALC_FUMA_SOFT=$0000;
 ALC_ACN_SOFT=$0001;

// Ambisonic scalings (normalization)
//#define ALC_FUMA_SOFT*/
 ALC_SN3D_SOFT=$0001;
 ALC_N3D_SOFT=$0002;

 AL_EFFECTSLOT_TARGET_SOFT=$199C;

 AL_EVENT_CALLBACK_FUNCTION_SOFT=$19A2;
 AL_EVENT_CALLBACK_USER_PARAM_SOFT=$19A3;
 AL_EVENT_TYPE_BUFFER_COMPLETED_SOFT=$19A4;
 AL_EVENT_TYPE_SOURCE_STATE_CHANGED_SOFT=$19A5;
 AL_EVENT_TYPE_DISCONNECTED_SOFT=$19A6;

type
 ALEventProcSOFT=procedure(eventType: ALenum; _object, param: ALuint;
                           length: ALsizei; message: PALchar; userParam: PALvoid); cdecl;

var
  alEventControlSOFT: procedure(count: ALsizei; const _types: PALenum; enable: ALboolean); cdecl;
  alEventCallbackSOFT: procedure(callback: ALEventProcSOFT; userParam: PALvoid); cdecl;
  alGetPointerSOFT: function(pname: ALenum): PALvoid; cdecl;
  alGetPointervSOFT: procedure(pname: ALenum; values: PPALvoid); cdecl;
  //alGetPointervSOFT: procedure(pname: ALenum; void **values); cdecl;


// ALC_SOFT_reopen_device
type
  TProc_alcReopenDeviceSOFT = function(device: PALCdevice; const deviceName: PALCchar; const attribs: PALCint): ALCboolean;
var
  alcReopenDeviceSOFT: TProc_alcReopenDeviceSOFT;


// AL_SOFT_callback_buffer
type
 ALBUFFERCALLBACKTYPESOFT = function(userptr, sampledata: PALvoid; numbytes: ALsizei): ALsizei; cdecl;
const
  AL_BUFFER_CALLBACK_FUNCTION_SOFT = $19A0;
  AL_BUFFER_CALLBACK_USER_PARAM_SOFT = $19A1;
var
  alBufferCallbackSOFT: procedure( buffer: ALuint; format: ALenum; freq: ALsizei; callback: ALBUFFERCALLBACKTYPESOFT; userptr: PALvoid); cdecl;
  alGetBufferPtrSOFT: procedure( buffer: ALuint; param: ALenum; ptr: PALvoid); cdecl;
  alGetBuffer3PtrSOFT: procedure( buffer: ALuint; param: ALenum; ptr0, ptr1, ptr2: PALvoid); cdecl;
  alGetBufferPtrvSOFT: procedure( buffer: ALuint; param: ALenum; ptr: PALvoid); cdecl;


// AL_SOFT_UHJ
const

  AL_FORMAT_UHJ2CHN8_SOFT=$19A2;
  AL_FORMAT_UHJ2CHN16_SOFT=$19A3;
  AL_FORMAT_UHJ2CHN_FLOAT32_SOFT=$19A4;
  AL_FORMAT_UHJ3CHN8_SOFT=$19A5;
  AL_FORMAT_UHJ3CHN16_SOFT=$19A6;
  AL_FORMAT_UHJ3CHN_FLOAT32_SOFT=$19A7;
  AL_FORMAT_UHJ4CHN8_SOFT=$19A8;
  AL_FORMAT_UHJ4CHN16_SOFT=$19A9;
  AL_FORMAT_UHJ4CHN_FLOAT32_SOFT=$19AA;

  AL_STEREO_MODE_SOFT=$19B0;
  AL_NORMAL_SOFT=$0000;
  AL_SUPER_STEREO_SOFT=$0001;
  AL_SUPER_STEREO_WIDTH_SOFT=$19B1;


// ALC_SOFT_output_mode
  ALC_OUTPUT_MODE_SOFT=$19AC;
  ALC_ANY_SOFT=$19AD;
  //ALC_MONO_SOFT=$1500;
  //ALC_STEREO_SOFT=$1501;
  ALC_STEREO_BASIC_SOFT=$19AE;
  ALC_STEREO_UHJ_SOFT=$19AF;
  ALC_STEREO_HRTF_SOFT=$19B2;
  //ALC_QUAD_SOFT=$1503*/
  ALC_SURROUND_5_1_SOFT=$1504;
  ALC_SURROUND_6_1_SOFT=$1505;
  ALC_SURROUND_7_1_SOFT=$1506;


