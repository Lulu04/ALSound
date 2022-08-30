program console_record;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  openalsoft,
  ALSound, libsndfile;


type

TRecorder = record
  mDevice: PALCdevice;
  mFile: PSNDFILE;
  mRecTime: single;
  mDataSize: int64;

  mChannels,
  mBits,
  mSampleRate,
  mFrameSize: longword;
  mBuffer: Pointer;
  mBufferSize: longint;
 // mBuf: ArrayOfByte;
end;

const
  optlist: string = '    --channels/-c <channels>  Set channel count (1 or 2) (default: 1)'+LINEENDING+
                    '    --bits/-b <bits>          Set channel count (8, 16, or 32) (default: 16)'+LINEENDING+
                    '    --rate/-r <rate>          Set sample rate (8000 to 96000) (default: 44100)'+LINEENDING+
                    '    --time/-t <time>          Time in seconds to record (1 to 10) (default: 4)'+LINEENDING+
                    '    --outfile/-o <filename>   Output filename (default: record.wav)';
procedure ShowUsage;
begin
    writeln('Usage: '+ExtractFileName(ParamStr(0))+' [-device <name>] [options...]'+LINEENDING+LINEENDING+
            'Available options:'+LINEENDING+optlist);
end;

var
  flagShowUsage, invalidParam: boolean;
  devname: PChar;
  recorder: TRecorder;
  alformat: longint;
  err: longint;
  FFileName: string;
  duration: single;
  rate: longint;
  k: integer;
  sfinfo: TSF_INFO;

  count: ALCint;
  writtenOnFile: sf_count_t;
  data: Pointer;

procedure ShowAudioParam;
begin
  if devname=NIL
    then write('default device, ')
    else write(StrPas(devname)+', ');
  if recorder.mBits=32
    then write('Float ');
  if recorder.mBits<> 8
    then write('Signed ')
    else write('Unsigned ');
  write(recorder.mBits.ToString+'-bit ');
  if recorder.mChannels=1
    then write('Mono ')
    else write('Stereo ');
  writeln(recorder.mSampleRate.ToString+'hz (32768 samples)');
end;

begin
  writeln('Record from an audio device to a file.'+LINEENDING);

  recorder.mDevice := NIL;
  recorder.mFile := NIL;
  recorder.mRecTime := 4.0;
  recorder.mDataSize:=0;
  recorder.mChannels := 1;
  recorder.mBits := 16;
  recorder.mSampleRate := 44100;
  recorder.mFrameSize := recorder.mChannels * recorder.mBits div 8;
  recorder.mBuffer := NIL;
  recorder.mBufferSize := 0;

  FFileName := 'record.wav';
  devname:=NIL;

  k:=1;
  flagShowUsage:=FALSE;
  while k<=ParamCount do begin
    case ParamStr(k) of
      '-device': begin
        if k<ParamCount
          then devname:=PChar(ParamStr(k+1))
          else begin
             writeln('Missing argument for option '+ParamStr(k));
             flagShowUsage:=TRUE;
        end;
      end;
      '--channels','-c': begin
        if k<ParamCount then begin
          case ParamStr(k+1) of
            '1': recorder.mChannels := 1;
            '2': recorder.mChannels := 2;
            else begin
              writeln('Invalid channel count '+ParamStr(k+1));
              flagShowUsage:=TRUE;
            end;
          end;//case
          inc(k,2);
        end else begin
          writeln('Missing argument for option '+ParamStr(k));
          flagShowUsage:=TRUE;
        end;
      end;
      '--bits','-b': begin
        if k<ParamCount then begin
          case ParamStr(k+1) of
            '8': recorder.mBits := 8;
            '16': recorder.mBits := 16;
            '32': recorder.mBits := 32;
            else begin
              writeln('Invalid bit count '+ParamStr(k+1));
              flagShowUsage:=TRUE;
            end;
          end;//case
          inc(k,2);
        end else begin
          writeln('Missing argument for option '+ParamStr(k));
          flagShowUsage:=TRUE;
        end;
      end;
      '--rate','-r': begin
        if k<ParamCount then begin
          invalidParam := not TryStrToInt(ParamStr(k+1), rate);
          if not invalidParam
            then invalidParam := not ((rate>=8000) and (rate<=96000));
          if not invalidParam
            then recorder.mSampleRate:=rate
            else begin
                writeln('Invalid sample rate '+ParamStr(k+1));
                flagShowUsage:=TRUE;
              end;
          inc(k,2);
        end else begin
          writeln('Missing argument for option '+ParamStr(k));
          flagShowUsage:=TRUE;
        end;
      end;
      '--time','-t': begin
        if k<ParamCount then begin
          if TryStrToFloat(ParamStr(k+1), duration)
            then recorder.mRecTime:=duration
            else begin
              writeln('Invalid record time '+ParamStr(k+1));
              flagShowUsage:=TRUE;
            end;
          inc(k,2);
        end else begin
          writeln('Missing argument for option '+ParamStr(k));
          flagShowUsage:=TRUE;
        end;
      end;
      '--outfile','-o': begin
        if k<ParamCount then begin
          FFileName := ParamStr(k+1);
          inc(k,2);
        end else begin
          writeln('Missing argument for option '+ParamStr(k));
          flagShowUsage:=TRUE;
        end;
      end;
      '--help','-h': begin
        flagShowUsage:=TRUE;
      end;
      else begin
        writeln('Invalid option '+ParamStr(k));
        flagShowUsage:=TRUE;
      end;
    end;//case
    if flagShowUsage then begin
      ShowUsage;
      exit;
    end;
  end;

  recorder.mFrameSize := recorder.mChannels * recorder.mBits div 8;

  alformat := AL_FORMAT_MONO16;
  if recorder.mChannels = 1 then begin
    case recorder.mBits of
      8: alformat := AL_FORMAT_MONO8;
     16 :alformat := AL_FORMAT_MONO16;
     32: alformat := AL_FORMAT_MONO_FLOAT32;
    end;
  end else if recorder.mChannels = 2 then begin
     case recorder.mBits of
       8: alformat := AL_FORMAT_STEREO8;
      16: alformat := AL_FORMAT_STEREO16;
      32: alformat := AL_FORMAT_STEREO_FLOAT32;
     end;
  end;

  recorder.mDevice := alcCaptureOpenDevice(devname, recorder.mSampleRate, alformat, 32768);
  if recorder.mDevice=NIL then begin
    if devname='' then devname:='default device';
    write('Failed to open ');
    ShowAudioParam;
    exit;
  end;
  writeln('Opened '''+alcGetString(recorder.mDevice, ALC_CAPTURE_DEVICE_SPECIFIER)+'''');


  sfinfo.samplerate:=recorder.mSampleRate;
  sfinfo.channels:=recorder.mChannels;
  case recorder.mBits of
    8: sfinfo.format := SF_FORMAT_WAV or SF_FORMAT_PCM_S8;    //SF_FORMAT_OGG | SF_FORMAT_VORBIS;
   16: sfinfo.format := SF_FORMAT_WAV or SF_FORMAT_PCM_16;
   32: sfinfo.format := SF_FORMAT_WAV or SF_FORMAT_FLOAT;
  end;

  recorder.mFile := sf_open(PChar(FFileName), SFM_WRITE, @sfinfo);
  if recorder.mFile=NIL then begin
    writeln('Failed to open '+FFileName+' for writing recorded audio');
    alcCaptureCloseDevice(recorder.mDevice);
    exit;
  end;

  write('Recording from ');
  ShowAudioParam;

  err := ALC_NO_ERROR;
  alcCaptureStart(recorder.mDevice);

  while (double(recorder.mDataSize)/double(recorder.mSampleRate) < recorder.mRecTime) and
        ((err = ALC_NO_ERROR) and (sf_error(recorder.mFile)=SF_ERR_NO_ERROR)) do begin
    count:=0;
    alcGetIntegerv(recorder.mDevice, ALC_CAPTURE_SAMPLES, 1, @count);
    if count < 1 then begin
      sleep(10);
      continue;
    end else begin
      //if count > Length(recorder.mBuf) then SetLength(recorder.mBuf, count);
      if count > recorder.mBufferSize then begin
        data := GetMem(ptruint(count*recorder.mFrameSize));
        if recorder.mBufferSize>0
          then FreeMem( recorder.mBuffer );
        recorder.mBuffer := data;
        recorder.mBufferSize := count;
      end;
      alcCaptureSamples(recorder.mDevice, recorder.mBuffer, count);

      writtenOnFile:=0;
      case recorder.mBits of
        8: writtenOnFile:=sf_write_short(recorder.mFile, recorder.mBuffer, count);
       16: writtenOnFile:=sf_write_short(recorder.mFile, recorder.mBuffer, count);
       32: writtenOnFile:=sf_write_float(recorder.mFile, recorder.mBuffer, count);
      end;

      recorder.mDataSize += ALuint(writtenOnFile);
      write(#13+'Captured '+recorder.mDataSize.ToString+' samples');
    end;
    err:=alcGetError(recorder.mDevice);
  end;

  alcCaptureStop(recorder.mDevice);

  writeln('  -> '+(recorder.mDataSize*recorder.mFrameSize).ToString+' bytes');

  if err <> ALC_NO_ERROR
    then writeln('Got device error $'+IntToHex(err, 4)+': '+ StrPas(alcGetString(recorder.mDevice, err)));

  alcCaptureCloseDevice(recorder.mDevice);

  if recorder.mBuffer<>NIL
    then FreeMem(recorder.mBuffer);

  sf_write_sync( recorder.mFile ); // flush file output
  sf_close( recorder.mFile );
end.

