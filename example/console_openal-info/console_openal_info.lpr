program console_openal_info;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  openalsoft;


procedure PrintList( const A: TStringArray );
var i: integer;
begin
  if Length(A)=0 then
    writeln('    !! NONE !!')
  else
    for i:=0 to High(A) do
      writeln('    '+A[i]);
end;

function checkALErrors: ALenum;
begin
  Result := alGetError();
  if Result <> AL_NO_ERROR then
    writeln('OpenAL Error: '+alGetString(Result)+' '+IntToHex(Result, 8));
end;

function checkALCErrors(aDevice: PALCdevice): ALCenum;
begin
  Result:=alcGetError(aDevice);
  if Result<>ALC_NO_ERROR then
    writeln('ALC Error: '+alcGetString(aDevice, Result)+' '+IntToHex(Result, 8));
end;



procedure PrintALCInfo( aDevice: PALCdevice );
var major, minor: ALCint;
  devname: PALCchar;
begin
  if aDevice<>NIL then
  begin
    devname := NIL;
    writeln('');
    if alcIsExtensionPresent(aDevice, PChar('ALC_ENUMERATE_ALL_EXT')) then
      devname := alcGetString(aDevice, ALC_ALL_DEVICES_SPECIFIER);
    if (checkALCErrors(aDevice) <> ALC_NO_ERROR) or (devname=NIL) then
      devname := alcGetString(aDevice, ALC_DEVICE_SPECIFIER);
    writeln('** Info for device '+StrPas(devname)+' **');
  end;
  alcGetIntegerv(aDevice, ALC_MAJOR_VERSION, 1, @major);
  alcGetIntegerv(aDevice, ALC_MINOR_VERSION, 1, @minor);
  if checkALCErrors(aDevice) = ALC_NO_ERROR then
    writeln('ALC version: '+major.ToString+'.'+minor.ToString);

  if aDevice<>NIL then
  begin
    Writeln('ALC extensions:');
    PrintList(ListOfContextExtension(aDevice));
    checkALCErrors(aDevice);
  end;
end;


procedure printHRTFInfo(aDevice: PALCdevice);
var num_hrtfs: ALCint;
  i: integer;
  name: string;
begin
  if not alcIsExtensionPresent(aDevice, PChar('ALC_SOFT_HRTF')) then
  begin
    writeln('HRTF extension not available');
    exit;
  end;

  LoadExt_ALC_SOFT_HRTF(aDevice);

  alcGetIntegerv(aDevice, ALC_NUM_HRTF_SPECIFIERS_SOFT, 1, @num_hrtfs);
  if num_hrtfs = 0 then
    writeln('No HRTFs found')
  else
  begin
    writeln('Available HRTFs: ('+num_hrtfs.ToString+')');
    for i:=0 to num_hrtfs-1 do
    begin
      name := StrPas(alcGetStringiSOFT(aDevice, ALC_HRTF_SPECIFIER_SOFT, i));
      writeln('    >'+name);
    end;
  end;
  checkALCErrors(aDevice);
end;

procedure printModeInfo(aDevice: PALCdevice);
var mode: ALCenum;
  modename: string;
begin
  if alcIsExtensionPresent(aDevice, PChar('ALC_SOFT_output_mode')) then
  begin
      mode := 0;
      alcGetIntegerv(aDevice, ALC_OUTPUT_MODE_SOFT, 1, @mode);
      checkALCErrors(aDevice);
      case mode of
        ALC_ANY_SOFT: modename := 'Unknown / unspecified';
        ALC_MONO_SOFT: modename := 'Mono';
        ALC_STEREO_SOFT: modename := 'Stereo (unspecified encoding)';
        ALC_STEREO_BASIC_SOFT: modename := 'Stereo (basic)';
        ALC_STEREO_UHJ_SOFT: modename := 'Stereo (UHJ)';
        ALC_STEREO_HRTF_SOFT: modename := 'Stereo (HRTF)';
        ALC_QUAD_SOFT: modename := 'Quadraphonic';
        ALC_SURROUND_5_1_SOFT: modename := '5.1 Surround';
        ALC_SURROUND_6_1_SOFT: modename := '6.1 Surround';
        ALC_SURROUND_7_1_SOFT: modename := '7.1 Surround';
        else modename := '(error)';
      end;
        writeln('Output channel mode: '+modename);
  end
  else writeln('Output mode extension not available');
end;

procedure printALInfo;
begin
  writeln('OpenAL vendor string: '+StrPas(alGetString(AL_VENDOR)));
  writeln('OpenAL renderer string: '+StrPas(alGetString(AL_RENDERER)));
  writeln('OpenAL version string: '+StrPas(alGetString(AL_VERSION)));
  writeln('OpenAL extensions:');
  printList(GetALExtension);
  checkALErrors();
end;

procedure printResamplerInfo;
var num_resamplers, def_resampler, i: ALint;
  name: string;
begin
  if not alIsExtensionPresent(PChar('AL_SOFT_source_resampler')) then
  begin
    writeln('Resampler info not available');
    exit;
  end;

  LoadExt_AL_SOFT_source_resampler;

  num_resamplers := alGetInteger(AL_NUM_RESAMPLERS_SOFT);
  def_resampler := alGetInteger(AL_DEFAULT_RESAMPLER_SOFT);

  if num_resamplers=0 then
    writeln('!!! No resamplers found !!!')
  else
  begin
    writeln('Available resamplers:');
    for i:=0 to num_resamplers-1 do
    begin
      name := StrPas(alGetStringiSOFT(AL_RESAMPLER_NAME_SOFT, i));
      if i=def_resampler then
        writeln('    '+name+' *')
      else
        writeln('    '+name);
    end;
  end;
  checkALErrors();
end;

procedure printEFXInfo(aDevice: PALCdevice);
const filters: array[0..3] of ALint=(AL_FILTER_LOWPASS, AL_FILTER_HIGHPASS,
                                    AL_FILTER_BANDPASS, AL_FILTER_NULL);
      filterNames: array[0..2] of string=('Low-pass','High-pass','Band-pass');
      effects: array[0..13] of ALint=(AL_EFFECT_EAXREVERB, AL_EFFECT_REVERB, AL_EFFECT_CHORUS,
                                      AL_EFFECT_DISTORTION, AL_EFFECT_ECHO, AL_EFFECT_FLANGER,
                                      AL_EFFECT_FREQUENCY_SHIFTER, AL_EFFECT_VOCAL_MORPHER,
                                      AL_EFFECT_PITCH_SHIFTER, AL_EFFECT_RING_MODULATOR,
                                      AL_EFFECT_AUTOWAH, AL_EFFECT_COMPRESSOR, AL_EFFECT_EQUALIZER,
                                      AL_EFFECT_NULL);
      dedeffects: array[0..2] of ALint=(AL_EFFECT_DEDICATED_DIALOGUE,
                                        AL_EFFECT_DEDICATED_LOW_FREQUENCY_EFFECT,
                                        AL_EFFECT_NULL);

      effectNames: array[0..14] of string=('EAX Reverb','Reverb','Chorus','Distortion','Echo','Flanger',
        'Frequency Shifter','Vocal Morpher','Pitch Shifter','Ring Modulator','Autowah',
        'Compressor','Equalizer','Dedicated Dialog','Dedicated LFE');
var major, minor, sends: ALCint;
    obj: ALuint;
    i: integer;
begin
  if not alcIsExtensionPresent(aDevice, PChar('ALC_EXT_EFX')) then
  begin
    writeln('EFX not available');
    exit;
  end;

  LoadExt_ALC_EXT_EFX;

  alcGetIntegerv(aDevice, ALC_EFX_MAJOR_VERSION, 1, @major);
  alcGetIntegerv(aDevice, ALC_EFX_MINOR_VERSION, 1, @minor);
  if checkALCErrors(aDevice) = ALC_NO_ERROR then
    writeln('EFX version: '+major.ToString+'.'+minor.ToString);
  alcGetIntegerv(aDevice, ALC_MAX_AUXILIARY_SENDS, 1, @sends);
  if checkALCErrors(aDevice) = ALC_NO_ERROR then
    writeln('Max auxiliary sends: '+sends.ToString);

  alGenFilters(1, @obj);
  checkALErrors();

  writeln('Supported filters:');
  for i:=0 to High(filters)-1 do
  begin
    alFilteri(obj, AL_FILTER_TYPE, filters[i]);
    if alGetError() = AL_NO_ERROR then
      writeln('    '+filterNames[i]);
  end;

  alDeleteFilters(1, @obj);
  alGenEffects(1, @obj);
  checkALErrors();

  writeln('Supported effects:');
  for i:=0 to High(effects)-1 do
  begin
    alEffecti(obj, AL_EFFECT_TYPE, effects[i]);
    if alGetError() = AL_NO_ERROR then
      writeln('    '+effectNames[i]);
  end;

  if alcIsExtensionPresent(aDevice, PChar('ALC_EXT_DEDICATED')) then
  begin
    for i:=0 to High(dedeffects)-1 do
    begin
      alEffecti(obj, AL_EFFECT_TYPE, dedeffects[i]);
      if alGetError() = AL_NO_ERROR then
        writeln('    '+effectNames[i+13]);
    end;
  end
  else
  begin
    for i:=0 to High(dedeffects)-1 do begin
      writeln('    '+effectNames[i+13]);
    end;
  end;

  alDeleteEffects(1, @obj);
  checkALErrors();
end;


var FDevice: PALCdevice;
    FContext: PALCcontext;
    p: string;
    names: TStringArray;
    i: integer;
    FOpenALSoftLibraryLoaded: boolean;
    {$if DEFINED(Darwin)}bundleName: string;{$endif}
begin
  if (ParamCount > 1) and
     ((ParamStr(1)='--help') or (ParamStr(1)='-h')) then begin
    writeln('Usage: '+ExtractFileName(ParamStr(0))+' [playback device]');
    exit;
  end;

  {$if DEFINED(Windows)}
  SetLength(names, 3);
  names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), 'soft_oal.dll']);
  names[1] := 'soft_oal.dll';
  names[2] := 'OpenAL32.dll';
  {$endif}

  {$if DEFINED(Linux)}
  SetLength(names, 4);
  names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), 'libopenal.so']);
  names[1] := 'libopenal.so';
  names[2] := 'libopenal.so.0';
  names[3] := 'libopenal.so.1';
  {$endif}

  {$if DEFINED(Darwin)}
   // NOT TESTED !!
   {$ifdef LCL}
   f := Application.Location;
   {$else}
   f := ExtractFilePath(ParamStr(0));
   {$endif}
   SetLength(names, 3);
   bundleName := '/'+ApplicationName+'.app';
   i := Pos(bundleName, ParamStr(0));
   if i <> 0 then
     names[0] := copy(f, 1, i-1)+bundleName+'/Contents/Resources/libopenal.dylib'
   else
     names[0] := ConcatPaths([f, '/libopenal.dylib']);
   names[1] := 'libopenal.dylib';
   names[2] := '/System/Library/Frameworks/OpenAL.framework/OpenAL';
  {$endif}

  FOpenALSoftLibraryLoaded := False;
  for i:=0 to High(names) do
    if LoadOpenALCoreLibrary(names[i]) then
    begin
      FOpenALSoftLibraryLoaded := True;
      writeln('Find ' + names[i]);
      break;
    end;

  if not FOpenALSoftLibraryLoaded then begin
    writeln('Failed to load OpenAL-Soft library...');
    exit;
  end;

  writeln('Available playback device:');
  PrintList( GetDeviceNames );

  writeln('Available capture devices:');
  PrintList( GetCaptureDeviceNames );


  writeln('Default playback device:');
  writeln('    '+GetDefaultDeviceName);

  writeln('Default capture device:');
  p := GetDefaultCaptureDeviceName;
  if p='' then p := '!! NONE !!';
  writeln('    '+p);

  PrintALCInfo(NIL);

  if ParamCount>1
    then p:=ParamStr(1)
    else p:='default device';

  if ParamCount>1
    then FDevice := alcOpenDevice(PChar(ParamStr(1)))
    else FDevice := alcOpenDevice(NIL);
  if FDevice=NIL then
  begin
    writeln;
    writeln('!!! Failed to open '+p+' !!!');
    exit;
  end;

  printALCInfo(FDevice);
  printHRTFInfo(FDevice);

  FContext := alcCreateContext(FDevice, NIL);
  if (FContext=NIL) or not alcMakeContextCurrent(FContext) then
  begin
    if FContext<>NIL
      then alcDestroyContext(FContext);
    alcCloseDevice(FDevice);
    writeln;
    writeln('!!! Failed to set a context !!!');
    writeln;
    exit;
  end;

  printModeInfo(FDevice);
  printALInfo();
  printResamplerInfo();
  printEFXInfo(FDevice);

  alcMakeContextCurrent(NIL);
  alcDestroyContext(FContext);
  alcCloseDevice(FDevice);

  UnloadOpenALSoftLibrary;

end.

