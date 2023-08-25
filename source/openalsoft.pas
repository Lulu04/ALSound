{
  **************************************************************************
 *                                                                          *
 *  This file is part of ALSound.                                           *
 *                                                                          *
 *  See the file LICENSE included in this distribution,                     *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This software is distributed in the hope of being useful                *
 *  for learning purposes about OpenAL-Soft and LibSndFile                  *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
  **************************************************************************

  ALSound offer a simple and easy way to play, capture and mix sounds
  using OpenAL-Soft and LibSndFile libraries under FreePascal/Lazarus.


 OpenAL-Soft pascal binding written by Lulu - 2022

 Checks last version at https://github.com/Lulu04/ALSound
}

unit openalsoft;

{$mode objfpc}{$H+}
{$PACKRECORDS C}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  DynLibs, ctypes, SysUtils;

const

{$IFDEF LINUX}
  {$IFDEF CPU386}
  OPENAL_LIBNAME = 'libopenal.so';
  {$ENDIF}
  {$IFDEF CPUX86_64}
  OPENAL_LIBNAME = 'libopenal.so';
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFDEF CPU386}
  OPENAL_LIBNAME = 'soft_oal.dll';
  {$ENDIF}
  {$IFDEF CPUX86_64}
  OPENAL_LIBNAME = 'soft_oal.dll';
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
  {$IFDEF CPUX86_64}
  OPENAL_LIBNAME = 'libopenal.dylib';
  {$ELSE}
   {$error This Mac OS is not supported}
  {$ENDIF}
{$ENDIF}



{$include al.inc}
{$include alc.inc}
{$include alext.inc}
{$include efx.inc}


function LoadOpenALCoreLibrary( const aFilename: string ): boolean;
procedure UnloadOpenALSoftLibrary;

// Device dependant extensions
function LoadExt_ALC_EXT_thread_local_context(aDevice: PALCDevice): boolean;
function LoadExt_ALC_SOFT_loopback(aDevice: PALCDevice): boolean;
function LoadExt_ALC_SOFT_pause_device(aDevice: PALCDevice): boolean;
function LoadExt_ALC_SOFT_HRTF(aDevice: PALCDevice): boolean;             // loaded in playback/loopback context
function LoadExt_ALC_SOFT_device_clock(aDevice: PALCDevice): boolean;
function LoadExt_ALC_SOFT_reopen_device(aDevice: PALCDevice): boolean;

// Other extensions
function LoadExt_ALC_EXT_EFX: boolean; // loaded in playback/loopback context
function LoadExt_AL_SOFT_buffer_samples: boolean;
function LoadExt_AL_SOFT_buffer_sub_data: boolean;
function LoadExt_AL_SOFT_callback_buffer: boolean;
function LoadExt_AL_SOFT_source_latency: boolean;
function LoadExt_AL_SOFT_deferred_updates: boolean;  // loaded in playback/loopback context
function LoadExt_AL_SOFT_source_resampler: boolean;  // loaded in playback/loopback context
function LoadExt_AL_SOFT_events: boolean;

// some functions to ease things
function GetDeviceNames: TStringArray;
function GetCaptureDeviceNames: TStringArray;
function GetDefaultDeviceName: string;
function GetDefaultCaptureDeviceName: string;

function ListOfContextExtension(aDevice: PALCDevice): TStringArray;

function GetALExtension: TStringArray;

function StringToNullTerminated(const s: string): PChar;

var
  FLoaded_OpenALCore_: boolean=False;
  FExtensionLoaded_ALC_EXT_EFX: boolean=False;
  FExtensionLoaded_AL_SOFT_buffer_samples: boolean=False;
  FExtensionLoaded_AL_SOFT_buffer_sub_data: boolean=False;
  FExtensionLoaded_AL_SOFT_callback_buffer: boolean=False;
  FExtensionLoaded_AL_SOFT_source_latency: boolean=False;
  FExtensionLoaded_AL_SOFT_deferred_updates: boolean=False;
  FExtensionLoaded_AL_SOFT_source_resampler: boolean=False;
  FExtensionLoaded_AL_SOFT_events: boolean=False;


function SetALSoft_LogCallback(aCallback: TALSoft_LogCallback; aUserPtr: pointer): boolean;

implementation

var
  _OpenALLib_ReferenceCounter: cardinal = 0;
  _OpenALLib_Handle: TLibHandle = dynlibs.NilHandle;


function GetCoreProc(const aName: string; var aFlag: boolean): Pointer;
begin
  Result := DynLibs.GetProcedureAddress(_OpenALLib_Handle, PChar(aName));
  aFlag := aFlag and (Result <> nil);
end;

function GetALExtProc(const aName: string; var aFlag: boolean): Pointer;
begin
  Result := alGetProcAddress(PChar(aName));
  aFlag := aFlag and (Result <> nil);
end;

function GetALCExtProc(aDevice: PALCDevice; const aName: string; var aFlag: boolean): Pointer;
begin
  Result := NIL;
  if FLoaded_OpenALCore_ then
    Result := alcGetProcAddress(aDevice, PChar(aName));
  aFlag := aFlag and (Result <> nil);
end;

function LoadOpenALCoreLibrary( const aFilename: string ): boolean;
var
  f: UnicodeString;
begin
  if _OpenALLib_Handle <> dynlibs.NilHandle then
  begin
    Inc(_OpenALLib_ReferenceCounter);
    Result := FLoaded_OpenALCore_;
  end;

  if Length(aFilename) = 0 then
    f := UnicodeString( OPENAL_LIBNAME )
  else
    f := UnicodeString( aFilename );
  _OpenALLib_Handle := DynLibs.SafeLoadLibrary( f );

  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FLoaded_OpenALCore_ := True;
    Pointer(alEnable) := GetCoreProc('alEnable', FLoaded_OpenALCore_);
    Pointer(alDisable) := GetCoreProc('alDisable', FLoaded_OpenALCore_);
    Pointer(alIsEnabled) := GetCoreProc('alIsEnabled', FLoaded_OpenALCore_);

    Pointer(alGetString) := GetCoreProc('alGetString', FLoaded_OpenALCore_);
    Pointer(alGetBooleanv) := GetCoreProc('alGetBooleanv', FLoaded_OpenALCore_);
    Pointer(alGetIntegerv) := GetCoreProc('alGetIntegerv', FLoaded_OpenALCore_);
    Pointer(alGetFloatv) := GetCoreProc('alGetFloatv', FLoaded_OpenALCore_);
    Pointer(alGetDoublev) := GetCoreProc('alGetDoublev', FLoaded_OpenALCore_);
    Pointer(alGetBoolean) := GetCoreProc('alGetBoolean', FLoaded_OpenALCore_);
    Pointer(alGetInteger) := GetCoreProc('alGetInteger', FLoaded_OpenALCore_);
    Pointer(alGetFloat) := GetCoreProc('alGetFloat', FLoaded_OpenALCore_);
    Pointer(alGetDouble) := GetCoreProc('alGetDouble', FLoaded_OpenALCore_);

    Pointer(alGetError) := GetCoreProc('alGetError', FLoaded_OpenALCore_);

    Pointer(alIsExtensionPresent) := GetCoreProc('alIsExtensionPresent', FLoaded_OpenALCore_);
    Pointer(alGetProcAddress) := GetCoreProc('alGetProcAddress', FLoaded_OpenALCore_);
    Pointer(alGetEnumValue) := GetCoreProc('alGetEnumValue', FLoaded_OpenALCore_);

    Pointer(alcIsExtensionPresent) := GetCoreProc('alcIsExtensionPresent', FLoaded_OpenALCore_);
    Pointer(alcGetProcAddress) := GetCoreProc('alcGetProcAddress', FLoaded_OpenALCore_);
    Pointer(alcGetEnumValue) := GetCoreProc('alcGetEnumValue', FLoaded_OpenALCore_);

    Pointer(alListenerf) := GetCoreProc('alListenerf', FLoaded_OpenALCore_);
    Pointer(alListener3f) := GetCoreProc('alListener3f', FLoaded_OpenALCore_);
    Pointer(alListenerfv) := GetCoreProc('alListenerfv', FLoaded_OpenALCore_);
    Pointer(alListeneri) := GetCoreProc('alListeneri', FLoaded_OpenALCore_);
    Pointer(alListener3i) := GetCoreProc('alListener3i', FLoaded_OpenALCore_);
    Pointer(alListeneriv) := GetCoreProc('alListeneriv', FLoaded_OpenALCore_);

    Pointer(alGetListenerf) := GetCoreProc('alGetListenerf', FLoaded_OpenALCore_);
    Pointer(alGetListener3f) := GetCoreProc('alGetListener3f', FLoaded_OpenALCore_);
    Pointer(alGetListenerfv) := GetCoreProc('alGetListenerfv', FLoaded_OpenALCore_);
    Pointer(alGetListeneri) := GetCoreProc('alGetListeneri', FLoaded_OpenALCore_);
    Pointer(alGetListener3i) := GetCoreProc('alGetListener3i', FLoaded_OpenALCore_);
    Pointer(alGetListeneriv) := GetCoreProc('alGetListeneriv', FLoaded_OpenALCore_);

    Pointer(alGenSources) := GetCoreProc('alGenSources', FLoaded_OpenALCore_);
    Pointer(alDeleteSources) := GetCoreProc('alDeleteSources', FLoaded_OpenALCore_);
    Pointer(alIsSource) := GetCoreProc('alIsSource', FLoaded_OpenALCore_);

    Pointer(alSourcef) := GetCoreProc('alSourcef', FLoaded_OpenALCore_);
    Pointer(alSource3f) := GetCoreProc('alSource3f', FLoaded_OpenALCore_);
    Pointer(alSourcefv) := GetCoreProc('alSourcefv', FLoaded_OpenALCore_);
    Pointer(alSourcei) := GetCoreProc('alSourcei', FLoaded_OpenALCore_);
    Pointer(alSource3i) := GetCoreProc('alSource3i', FLoaded_OpenALCore_);
    Pointer(alSourceiv) := GetCoreProc('alSourceiv', FLoaded_OpenALCore_);

    Pointer(alGetSourcef) := GetCoreProc('alGetSourcef', FLoaded_OpenALCore_);
    Pointer(alGetSource3f) := GetCoreProc('alGetSource3f', FLoaded_OpenALCore_);
    Pointer(alGetSourcefv) := GetCoreProc('alGetSourcefv', FLoaded_OpenALCore_);
    Pointer(alGetSourcei) := GetCoreProc('alGetSourcei', FLoaded_OpenALCore_);
    Pointer(alGetSource3i) := GetCoreProc('alGetSource3i', FLoaded_OpenALCore_);
    Pointer(alGetSourceiv) := GetCoreProc('alGetSourceiv', FLoaded_OpenALCore_);

    Pointer(alSourcePlayv) := GetCoreProc('alSourcePlayv', FLoaded_OpenALCore_);
    Pointer(alSourceStopv) := GetCoreProc('alSourceStopv', FLoaded_OpenALCore_);
    Pointer(alSourceRewindv) := GetCoreProc('alSourceRewindv', FLoaded_OpenALCore_);
    Pointer(alSourcePausev) := GetCoreProc('alSourcePausev', FLoaded_OpenALCore_);

    Pointer(alSourcePlay) := GetCoreProc('alSourcePlay', FLoaded_OpenALCore_);
    Pointer(alSourceStop) := GetCoreProc('alSourceStop', FLoaded_OpenALCore_);
    Pointer(alSourceRewind) := GetCoreProc('alSourceRewind', FLoaded_OpenALCore_);
    Pointer(alSourcePause) := GetCoreProc('alSourcePause', FLoaded_OpenALCore_);

    Pointer(alSourceQueueBuffers) := GetCoreProc('alSourceQueueBuffers', FLoaded_OpenALCore_);
    Pointer(alSourceUnqueueBuffers) := GetCoreProc('alSourceUnqueueBuffers', FLoaded_OpenALCore_);

    Pointer(alGenBuffers) := GetCoreProc('alGenBuffers', FLoaded_OpenALCore_);
    Pointer(alDeleteBuffers) := GetCoreProc('alDeleteBuffers', FLoaded_OpenALCore_);
    Pointer(alIsBuffer) := GetCoreProc('alIsBuffer', FLoaded_OpenALCore_);
    Pointer(alBufferData) := GetCoreProc('alBufferData', FLoaded_OpenALCore_);

    Pointer(alBufferf) := GetCoreProc('alBufferf', FLoaded_OpenALCore_);
    Pointer(alBuffer3f) := GetCoreProc('alBuffer3f', FLoaded_OpenALCore_);
    Pointer(alBufferfv) := GetCoreProc('alBufferfv', FLoaded_OpenALCore_);
    Pointer(alBufferi) := GetCoreProc('alBufferi', FLoaded_OpenALCore_);
    Pointer(alBuffer3i) := GetCoreProc('alBuffer3i', FLoaded_OpenALCore_);
    Pointer(alBufferiv) := GetCoreProc('alBufferiv', FLoaded_OpenALCore_);

    Pointer(alGetBufferf) := GetCoreProc('alGetBufferf', FLoaded_OpenALCore_);
    Pointer(alGetBuffer3f) := GetCoreProc('alGetBuffer3f', FLoaded_OpenALCore_);
    Pointer(alGetBufferfv) := GetCoreProc('alGetBufferfv', FLoaded_OpenALCore_);
    Pointer(alGetBufferi) := GetCoreProc('alGetBufferi', FLoaded_OpenALCore_);
    Pointer(alGetBuffer3i) := GetCoreProc('alGetBuffer3i', FLoaded_OpenALCore_);
    Pointer(alGetBufferiv) := GetCoreProc('alGetBufferiv', FLoaded_OpenALCore_);

    Pointer(alDopplerFactor) := GetCoreProc('alDopplerFactor', FLoaded_OpenALCore_);
    Pointer(alDopplerVelocity) := GetCoreProc('alDopplerVelocity', FLoaded_OpenALCore_);  // deprecated
    Pointer(alSpeedOfSound) := GetCoreProc('alSpeedOfSound', FLoaded_OpenALCore_);
    Pointer(alDistanceModel) := GetCoreProc('alDistanceModel', FLoaded_OpenALCore_);

    Pointer(alcCreateContext) := GetCoreProc('alcCreateContext', FLoaded_OpenALCore_);
    Pointer(alcMakeContextCurrent) := GetCoreProc('alcMakeContextCurrent', FLoaded_OpenALCore_);
    Pointer(alcProcessContext) := GetCoreProc('alcProcessContext', FLoaded_OpenALCore_);
    Pointer(alcSuspendContext) := GetCoreProc('alcSuspendContext', FLoaded_OpenALCore_);
    Pointer(alcDestroyContext) := GetCoreProc('alcDestroyContext', FLoaded_OpenALCore_);
    Pointer(alcGetCurrentContext) := GetCoreProc('alcGetCurrentContext', FLoaded_OpenALCore_);
    Pointer(alcGetContextsDevice) := GetCoreProc('alcGetContextsDevice', FLoaded_OpenALCore_);

    Pointer(alcOpenDevice) := GetCoreProc('alcOpenDevice', FLoaded_OpenALCore_);
    Pointer(alcCloseDevice) := GetCoreProc('alcCloseDevice', FLoaded_OpenALCore_);

    Pointer(alcGetError) := GetCoreProc('alcGetError', FLoaded_OpenALCore_);

    Pointer(alcIsExtensionPresent) := GetCoreProc('alcIsExtensionPresent', FLoaded_OpenALCore_);
    Pointer(alcGetProcAddress) := GetCoreProc('alcGetProcAddress', FLoaded_OpenALCore_);
    Pointer(alcGetEnumValue) := GetCoreProc('alcGetEnumValue', FLoaded_OpenALCore_);

    Pointer(alcGetString) := GetCoreProc('alcGetString', FLoaded_OpenALCore_);
    Pointer(alcGetIntegerv) := GetCoreProc('alcGetIntegerv', FLoaded_OpenALCore_);

    Pointer(alcCaptureOpenDevice) := GetCoreProc('alcCaptureOpenDevice', FLoaded_OpenALCore_);
    Pointer(alcCaptureCloseDevice) := GetCoreProc('alcCaptureCloseDevice', FLoaded_OpenALCore_);
    Pointer(alcCaptureStart) := GetCoreProc('alcCaptureStart', FLoaded_OpenALCore_);
    Pointer(alcCaptureStop) := GetCoreProc('alcCaptureStop', FLoaded_OpenALCore_);
    Pointer(alcCaptureSamples) := GetCoreProc('alcCaptureSamples', FLoaded_OpenALCore_);

    _OpenALLib_ReferenceCounter := 1;
  end
  else
    FLoaded_OpenALCore_ := False;

  Result := FLoaded_OpenALCore_;
end;

function LoadExt_ALC_EXT_EFX: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_ALC_EXT_EFX;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_ALC_EXT_EFX := True;
    Pointer(alGenEffects) := GetALExtProc('alGenEffects', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alDeleteEffects) := GetALExtProc('alDeleteEffects', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alIsEffect) := GetALExtProc('alIsEffect', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alEffecti) := GetALExtProc('alEffecti', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alEffectiv) := GetALExtProc('alEffectiv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alEffectf) := GetALExtProc('alEffectf', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alEffectfv) := GetALExtProc('alEffectfv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetEffecti) := GetALExtProc('alGetEffecti', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetEffectiv) := GetALExtProc('alGetEffectiv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetEffectf) := GetALExtProc('alGetEffectf', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetEffectfv) := GetALExtProc('alGetEffectfv', FExtensionLoaded_ALC_EXT_EFX);

    Pointer(alGenFilters) := GetALExtProc('alGenFilters', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alDeleteFilters) := GetALExtProc('alDeleteFilters', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alIsFilter) := GetALExtProc('alIsFilter', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alFilteri) := GetALExtProc('alFilteri', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alFilteriv) := GetALExtProc('alFilteriv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alFilterf) := GetALExtProc('alFilterf', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alFilterfv) := GetALExtProc('alFilterfv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetFilteri) := GetALExtProc('alGetFilteri', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetFilteriv) := GetALExtProc('alGetFilteriv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetFilterf) := GetALExtProc('alGetFilterf', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetFilterfv) := GetALExtProc('alGetFilterfv', FExtensionLoaded_ALC_EXT_EFX);

    Pointer(alGenAuxiliaryEffectSlots) := GetALExtProc('alGenAuxiliaryEffectSlots', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alDeleteAuxiliaryEffectSlots) := GetALExtProc('alDeleteAuxiliaryEffectSlots', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alIsAuxiliaryEffectSlot) := GetALExtProc('alIsAuxiliaryEffectSlot', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alAuxiliaryEffectSloti) := GetALExtProc('alAuxiliaryEffectSloti', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alAuxiliaryEffectSlotiv) := GetALExtProc('alAuxiliaryEffectSlotiv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alAuxiliaryEffectSlotf) := GetALExtProc('alAuxiliaryEffectSlotf', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alAuxiliaryEffectSlotfv) := GetALExtProc('alAuxiliaryEffectSlotfv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetAuxiliaryEffectSloti) := GetALExtProc('alGetAuxiliaryEffectSloti', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetAuxiliaryEffectSlotiv) := GetALExtProc('alGetAuxiliaryEffectSlotiv', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetAuxiliaryEffectSlotf) := GetALExtProc('alGetAuxiliaryEffectSlotf', FExtensionLoaded_ALC_EXT_EFX);
    Pointer(alGetAuxiliaryEffectSlotfv) := GetALExtProc('alGetAuxiliaryEffectSlotfv', FExtensionLoaded_ALC_EXT_EFX);
    Result := FExtensionLoaded_ALC_EXT_EFX;
  end
  else
    Result := False;
end;

function LoadExt_ALC_EXT_thread_local_context(aDevice: PALCDevice): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := alcIsExtensionPresent(NIL, PChar('ALC_EXT_thread_local_context'));
  if Result then
  begin
    Pointer(alcSetThreadContext) := GetALCExtProc(aDevice, 'alcSetThreadContext', Result);
    Pointer(alcGetThreadContext) := GetALCExtProc(aDevice, 'alcGetThreadContext', Result);
  end
  else
  begin
    alcSetThreadContext := NIL;
    alcGetThreadContext := NIL;
  end;
end;

function LoadExt_AL_SOFT_buffer_samples: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_buffer_samples;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_buffer_samples := True;
    Pointer(alBufferSamplesSOFT) := GetALExtProc('alBufferSamplesSOFT', FExtensionLoaded_AL_SOFT_buffer_samples);
    Pointer(alBufferSubSamplesSOFT) := GetALExtProc('alBufferSubSamplesSOFT', FExtensionLoaded_AL_SOFT_buffer_samples);
    Pointer(alGetBufferSamplesSOFT) := GetALExtProc('alGetBufferSamplesSOFT', FExtensionLoaded_AL_SOFT_buffer_samples);
    Pointer(alIsBufferFormatSupportedSOFT) := GetALExtProc('alIsBufferFormatSupportedSOFT', FExtensionLoaded_AL_SOFT_buffer_samples);

    Result := FExtensionLoaded_AL_SOFT_buffer_samples;
  end
  else
    Result := False;
end;

function LoadExt_AL_SOFT_buffer_sub_data: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_buffer_sub_data;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_buffer_sub_data := True;
    Pointer(alBufferSubDataSOFT) := GetALExtProc('alBufferSubDataSOFT', FExtensionLoaded_AL_SOFT_buffer_sub_data);
    Result := FExtensionLoaded_AL_SOFT_buffer_sub_data;
  end
  else
    Result := False;
end;

function LoadExt_AL_SOFT_callback_buffer: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_callback_buffer;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_callback_buffer := True;
    Pointer(alBufferCallbackSOFT) := GetALExtProc('alBufferCallbackSOFT', FExtensionLoaded_AL_SOFT_callback_buffer);
    Pointer(alGetBufferPtrSOFT) := GetALExtProc('alGetBufferPtrSOFT', FExtensionLoaded_AL_SOFT_callback_buffer);
    Pointer(alGetBuffer3PtrSOFT) := GetALExtProc('alGetBuffer3PtrSOFT', FExtensionLoaded_AL_SOFT_callback_buffer);
    Pointer(alGetBufferPtrvSOFT) := GetALExtProc('alGetBufferPtrvSOFT', FExtensionLoaded_AL_SOFT_callback_buffer);
    Result := FExtensionLoaded_AL_SOFT_callback_buffer;
  end
  else
    Result := False;
end;

function LoadExt_ALC_SOFT_loopback(aDevice: PALCDevice): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := alcIsExtensionPresent(aDevice, PChar('ALC_SOFT_loopback'));
  if Result then
  begin
    Pointer(alcLoopbackOpenDeviceSOFT) := GetALCExtProc(aDevice, 'alcLoopbackOpenDeviceSOFT', Result);
    Pointer(alcIsRenderFormatSupportedSOFT) := GetALCExtProc(aDevice, 'alcIsRenderFormatSupportedSOFT', Result);
    Pointer(alcRenderSamplesSOFT) := GetALCExtProc(aDevice, 'alcRenderSamplesSOFT', Result);
  end
  else
  begin
    alcLoopbackOpenDeviceSOFT := NIL;
    alcIsRenderFormatSupportedSOFT := NIL;
    alcRenderSamplesSOFT := NIL;
  end;
end;

function LoadExt_AL_SOFT_source_latency: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_source_latency;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_source_latency := True;
    Pointer(alSourcedSOFT) := GetALExtProc('alSourcedSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alSource3dSOFT) := GetALExtProc('alSource3dSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alSourcedvSOFT) := GetALExtProc('alSourcedvSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alGetSourcedSOFT) := GetALExtProc('alGetSourcedSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alGetSource3dSOFT) := GetALExtProc('alGetSource3dSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alGetSourcedvSOFT) := GetALExtProc('alGetSourcedvSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alSourcei64SOFT) := GetALExtProc('alSourcei64SOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alSource3i64SOFT) := GetALExtProc('alSource3i64SOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alSourcei64vSOFT) := GetALExtProc('alSourcei64vSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alGetSourcei64SOFT) := GetALExtProc('alGetSourcei64SOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alGetSource3i64SOFT) := GetALExtProc('alGetSource3i64SOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Pointer(alGetSourcei64vSOFT) := GetALExtProc('alGetSourcei64vSOFT', FExtensionLoaded_AL_SOFT_source_latency);
    Result := FExtensionLoaded_AL_SOFT_source_latency;
  end
  else
    Result := False;
end;

function LoadExt_AL_SOFT_deferred_updates: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_deferred_updates;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_deferred_updates := True;
    Pointer(alDeferUpdatesSOFT) := GetALExtProc('alDeferUpdatesSOFT', FExtensionLoaded_AL_SOFT_deferred_updates);
    Pointer(alProcessUpdatesSOFT) := GetALExtProc('alProcessUpdatesSOFT', FExtensionLoaded_AL_SOFT_deferred_updates);
    Result := FExtensionLoaded_AL_SOFT_deferred_updates;
  end
  else
    Result := False;
end;

function LoadExt_ALC_SOFT_pause_device(aDevice: PALCDevice): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := alcIsExtensionPresent(NIL, PChar('ALC_SOFT_pause_device'));
  if Result then
  begin
    Pointer(alcDevicePauseSOFT) := GetALCExtProc(aDevice, 'alcDevicePauseSOFT', Result);
    Pointer(alcDeviceResumeSOFT) := GetALCExtProc(aDevice, 'alcDeviceResumeSOFT', Result);
  end
  else
  begin
    alcDevicePauseSOFT := NIL;
    alcDeviceResumeSOFT := NIL;
  end;
end;

function LoadExt_ALC_SOFT_HRTF(aDevice: PALCDevice): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := alcIsExtensionPresent(aDevice, PChar('ALC_SOFT_HRTF'));
  if Result then
  begin
    Pointer(alcGetStringiSOFT) := GetALCExtProc(aDevice, 'alcGetStringiSOFT', Result);
    Pointer(alcResetDeviceSOFT) := GetALCExtProc(aDevice, 'alcResetDeviceSOFT', Result);
  end
  else
  begin
    alcGetStringiSOFT := NIL;
    alcResetDeviceSOFT := NIL;
  end;
end;

function LoadExt_AL_SOFT_source_resampler: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_source_resampler;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_source_resampler := true;
    Pointer(alGetStringiSOFT) := GetALExtProc('alGetStringiSOFT', FExtensionLoaded_AL_SOFT_source_resampler);
    Result := FExtensionLoaded_AL_SOFT_source_resampler;
  end
  else
    Result := False;
end;

function LoadExt_ALC_SOFT_device_clock(aDevice: PALCDevice): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := alcIsExtensionPresent(NIL, PChar('ALC_SOFT_device_clock'));
  if Result then
  begin
    Pointer(alcGetInteger64vSOFT) := GetALCExtProc(aDevice, 'alcGetInteger64vSOFT', Result);
  end
  else
  begin
    alcGetInteger64vSOFT := NIL;
  end;
end;

function LoadExt_AL_SOFT_events: boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := FExtensionLoaded_AL_SOFT_events;
  if Result then exit;
  if _OpenALLib_Handle <> DynLibs.NilHandle then
  begin
    FExtensionLoaded_AL_SOFT_events := True;
    Pointer(alEventControlSOFT) := GetALExtProc('alEventControlSOFT', FExtensionLoaded_AL_SOFT_events);
    Pointer(alEventCallbackSOFT) := GetALExtProc('alEventCallbackSOFT', FExtensionLoaded_AL_SOFT_events);
    Pointer(alGetPointerSOFT) := GetALExtProc('alGetPointerSOFT', FExtensionLoaded_AL_SOFT_events);
    Pointer(alGetPointervSOFT) := GetALExtProc('alGetPointervSOFT', FExtensionLoaded_AL_SOFT_events);
    Result := FExtensionLoaded_AL_SOFT_events;
  end
  else
    Result := False;
end;

function LoadExt_ALC_SOFT_reopen_device(aDevice: PALCDevice): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := alcIsExtensionPresent(NIL, PChar('ALC_SOFT_reopen_device'));
  if Result then
  begin
    Pointer(alcReopenDeviceSOFT) := GetALCExtProc(aDevice, 'alcReopenDeviceSOFT', Result);
  end
  else
  begin
    alcReopenDeviceSOFT := NIL;
  end;
end;




procedure UnloadOpenALSoftLibrary;
begin
  // Reference counting
  if _OpenALLib_ReferenceCounter > 0 then
    Dec(_OpenALLib_ReferenceCounter);
  if _OpenALLib_ReferenceCounter > 0 then
    exit;

  if _OpenALLib_Handle <> dynlibs.NilHandle then
  begin
    DynLibs.UnloadLibrary(_OpenALLib_Handle);
    _OpenALLib_Handle := DynLibs.NilHandle;
  end;

  FLoaded_OpenALCore_ := False;
end;


// openal-soft return a string list where each name is separated by a single
// NULL character and the list is terminated by two NULL characters
function OALMultiStringToStringArray(p: PChar): TStringArray;
var
  s: string;
begin
  Result := nil;
  SetLength(Result, 0);
  if p = nil then
    exit;
  repeat
    s := '';
    while p^ <> #0 do
    begin
      s := s + p^;
      Inc(p);
    end;
    if s <> '' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := s;
      Inc(p);
    end
    else
      exit;
  until False;
end;

function GetDeviceNames: TStringArray;
var
  s: PChar;
begin
  Result := NIL;
  if not FLoaded_OpenALCore_ then exit;

  if alcIsExtensionPresent(nil, PChar('ALC_ENUMERATE_ALL_EXT')) then
    s := alcGetString(nil, ALC_ALL_DEVICES_SPECIFIER)
  else
    s := alcGetString(nil, ALC_DEVICE_SPECIFIER);
  Result := OALMultiStringToStringArray(s);
end;

function GetCaptureDeviceNames: TStringArray;
var
  s: PChar;
begin
  Result := NIL;
  if not FLoaded_OpenALCore_ then exit;

  s := alcGetString(nil, ALC_CAPTURE_DEVICE_SPECIFIER);
  Result := OALMultiStringToStringArray(s);
end;

function GetDefaultDeviceName: string;
begin
  Result := '';
  if not FLoaded_OpenALCore_ then exit;

  if alcIsExtensionPresent(nil, PChar('ALC_ENUMERATE_ALL_EXT')) then
    Result := StrPas(alcGetString(nil, ALC_DEFAULT_ALL_DEVICES_SPECIFIER))
  else
    Result := StrPas(alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER));
end;

function GetDefaultCaptureDeviceName: string;
begin
  Result := '';
  if not FLoaded_OpenALCore_ then exit;

  Result := StrPas(alcGetString(nil, ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER));
end;

function ListOfContextExtension(aDevice: PALCDevice): TStringArray;
var
  s: string;
begin
  Result := NIL;
  if not FLoaded_OpenALCore_ then exit;

  s := StrPas(alcGetString(aDevice, ALC_EXTENSIONS));
  if Length(s) = 0 then
    SetLength(Result, 0)
  else
    Result := s.Split([' ']);
end;

function GetALExtension: TStringArray;
var
  s: string;
begin
  Result := NIL;
  if not FLoaded_OpenALCore_ then exit;

  s := StrPas(alGetString(AL_EXTENSIONS));
  if Length(s) = 0 then
    SetLength(Result, 0)
  else
    Result := s.Split([' ']);
end;

function StringToNullTerminated(const s: string): PChar;
begin
  Result := PChar(s + #0);
end;

function SetALSoft_LogCallback(aCallback: TALSoft_LogCallback; aUserPtr: pointer): boolean;
begin
  Result := False;
  if not FLoaded_OpenALCore_ then exit;

  Result := True;
  Pointer(alsoft_set_log_callback) := GetALCExtProc(NIL, 'alsoft_set_log_callback', Result);
  if Result then
    alsoft_set_log_callback(aCallback, aUserPtr);
end;

end.
