{$ifdef ALS_INTERFACE}
type

TBaseAudioFile = object
  Handle: PSNDFILE;
  Format,
  Channels: integer;
  Frames: int64;
  SampleRate: integer;
  // return a frame index in range [0..Frames-1]
  function TimeToFrameIndex(aTimePos: double): int64;
  // return a duration in seconds from a number of frames
  function FrameToTime(aFrameCount: int64): double;
end;


TAudioFileReader = object(TBaseAudioFile)
  // open the audio file in read mode
  function OpenRead(const aFileName: string): boolean;
  function ReadShort(p: Pointer; aCount: longword): longword;
  function ReadFloat(p: Pointer; aCount: longword): longword;
  function ReadDouble(p: PDouble; aCount: longword): longword;
  function Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
  function Close: boolean;
  function MoveToFrame(aFrameIndex: int64): boolean;
  function TotalDuration: double;
end;


TAudioFileWriter = object(TBaseAudioFile)
private
  FCopyBuffer: TALSPlaybackBuffer; // used by copy routines
  procedure InitCopyBuffer(aFramesCapacity: longword);
public
  // open the audio file in write mode
  function OpenWrite(const aFileName: string;
                     aFormat: TALSFileFormat;
                     aChannels,
                     aSampleRate: integer): boolean;
  function SampleAreFloat: boolean;
  function Close: boolean;
  function WriteShort(p: Pointer; aCount: longword): longword;
  function WriteFloat(p: Pointer; aCount: longword): longword;
  function WriteDouble(p: PDouble; aCount: longword): longword;
  function Write(const aBuf: TALSPlaybackBuffer): longword;
public
  // copy whole audio
  function CopyAll(const aSrcReader: TAudioFileReader): boolean;
  // copy part of audio
  function CopyPart(const aSrcReader: TAudioFileReader; aFromFrameIndex, aToFrameIndex: int64): boolean;

  procedure CopyMetaDataFrom(const aSrcReader: TAudioFileReader);
  procedure WriteMetaData(const aTitle, aCopyright, aSoftware, aArtist,
                          aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
end;


{ TAudioFileUtils }

TAudioFileUtils = record
  // gets the duration in seconds of the audio file.
  function GetDuration(const aFilename: string): single;
  // return the frame count of the audio file.
  function GetFramesCount(const aFilename: string): int64;
  // copy a part of aSrcFile to aDstFile.
  function CopyPart(const aSrcFile, aDstFile: string; aFrameIndexLow, aFrameIndexHigh: int64): boolean;
  // copy the audio from aSrcFile to aDstFile except the specified frame range index.
  function CutPart(const aSrcFile, aDstFile: string; aFrameIndexLow, aFrameIndexHigh: int64): boolean;
  // this function insert the content of aFilenameToInsert into aSrcFile at the specified frame index and save
  // the resulting audio into aDstFile.
  function InsertFile(const aSrcFile, aFileToInsert, aDstFile: string; aInsertAtFrameIndex: int64): boolean;
  // copy the audio from aSrcFile to aDstFile and the specified range of frame index is forced to silence.
  // aAttenuationTime controls the duration of the fadeout and fadein.
  // aAttenuationTime can be =0.0, in that case samples are directly sets to 0 (no fadeout/fadein).
  function SilencePart(const aSrcFile, aDstFile: string; aFrameIndexLow, aFrameIndexHigh: int64;
                       aAttenuationTime: single): boolean;
  // generate a audio file filled with silence.
  // aDuration is expressed in seconds.
  function GenerateSilence(const aFilename: string;
                           aChannelsCount, aSampleRate: integer;
                           aFormat: TALSFileFormat; aDuration: single): boolean;
  // copy the content of aSrcFile to aDstFile and insert a silence at the specified frame index.
  function InsertSilence(const aSrcFile, aDstFile: string; aFrameIndex: int64; aDuration: single): boolean;
  // append several audio files to the specified writer object.
  function AppendFilesToWriter(aFilenamesToAppend: TStringArray; const aTargetWriter: TAudioFileWriter): boolean;
  // this function append the content of aSrcFile and the content of the files in aFilenamesToAppend
  // and save the result into aDstFile.
  function AppendFiles(const aSrcFile: string; const aFilenamesToAppend: TStringArray; aDstFile: string): boolean;

  // this function read the content of aSrcFile and replace the frame range index by the content of aSubstitutionFile
  // and save the result into aDstFile.
  function ReplacePartByFile(const aSrcFile: string; aFrameIndexLow, aFrameIndexHigh: int64;
                             const aSubstitutionFile, aDstFile: string): boolean;
  // normalize the content of aSrcFile and save the result into aDstFile.
  // if aRemoveDC is true, the DC offset is computed and removed from the audio.
  function Normalize(const aSrcFile, aDstFile: string; aRemoveDC: boolean; aMaxdB: single): boolean;
  // Convert an audio file to another format
  function Convert(const aSrcFile, aDstFile: string; aDstFileFormat: TALSFileFormat; aForceMono: boolean): boolean;
  // amplify the content of aSrcFile by aGaindB and save the result into aDstFile.
  function ApplyGain(const aSrcFile, aDstFile: string; aGaindB: single): boolean;
end;

{$endif}
{$ifdef ALS_IMPLEMENTATION}

procedure ExceptionChannelsCount;
begin
  Raise Exception.Create('Audio buffer can not have different channel count than the file');
end;

{ TBaseAudioFile }

function TBaseAudioFile.TimeToFrameIndex(aTimePos: double): int64;
begin
  Result := Round(SampleRate*aTimePos);
end;

function TBaseAudioFile.FrameToTime(aFrameCount: int64): double;
begin
  Result := aFrameCount/SampleRate;
end;

{ TAudioFileReader }

function TAudioFileReader.OpenRead(const aFileName: string): boolean;
var sfinfo: TSF_INFO;
begin
  sfinfo.Format := 0;
  Handle := ALSOpenAudioFile(aFileName, SFM_READ, sfinfo);

  Result := Handle <> NIL;
  if Result then begin
    Format := sfinfo.Format;
    Channels := sfinfo.Channels;
    Frames := sfinfo.Frames;
    SampleRate := sfinfo.SampleRate;
  end;
end;

function TAudioFileReader.Close: boolean;
begin
  Result := True;
  if Handle <> NIL then
    Result := sf_close(Handle) = 0;
  Handle := NIL;
end;

function TAudioFileReader.MoveToFrame(aFrameIndex: int64): boolean;
begin
  Result := sf_seek(Handle, aFrameIndex, SF_SEEK_SET) <> -1;
end;

function TAudioFileReader.TotalDuration: double;
begin
  if SampleRate <> 0 then
    Result := Frames/SampleRate
  else
    Result := 0;
end;

function TAudioFileReader.ReadShort(p: Pointer; aCount: longword): longword;
begin
  Result := sf_readf_short(Handle, pcshort(p), sf_count_t(aCount));
end;

function TAudioFileReader.ReadFloat(p: Pointer; aCount: longword): longword;
begin
  Result := sf_readf_float(Handle, pcfloat(p), sf_count_t(aCount));
end;

function TAudioFileReader.ReadDouble(p: PDouble; aCount: longword): longword;
begin
  Result := sf_readf_double(Handle, pcdouble(p), sf_count_t(aCount));
end;

function TAudioFileReader.Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
begin
  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if aBuf.OutOfMemory then begin
    Result := 0;
    exit;
  end;

  if aBuf.UseFloat then
    Result := sf_readf_float(Handle, pcfloat(aBuf.Data), sf_count_t(aCount))
  else
    Result := sf_readf_short(Handle, pcshort(aBuf.Data), sf_count_t(aCount));
  aBuf.FrameCount := Result;
end;

{ TAudioFileWriter }

procedure TAudioFileWriter.InitCopyBuffer(aFramesCapacity: longword);
begin
  if SampleAreFloat
    then FCopyBuffer.Init(Channels, ALS_SAMPLE_FLOAT32)
    else FCopyBuffer.Init(Channels, ALS_SAMPLE_INT16);
  FCopyBuffer.FrameCapacity := aFramesCapacity;
end;

function TAudioFileWriter.OpenWrite(const aFileName: string;
  aFormat: TALSFileFormat; aChannels, aSampleRate: integer): boolean;
var sfinfo: TSF_INFO;
begin
  Format := aFormat;
  Channels := aChannels;
  SampleRate := aSampleRate;

  sfinfo.Format := cint(aFormat);
  sfinfo.Channels := cint(aChannels);
  sfinfo.SampleRate := cint(aSampleRate);
  Handle := ALSOpenAudioFile(aFileName, SFM_WRITE, sfinfo);
  Result := Handle <> NIL;
end;

function TAudioFileWriter.SampleAreFloat: boolean;
begin
  Result := (Format AND SF_FORMAT_SUBMASK) = libsndfile.SF_FORMAT_FLOAT;
end;

function TAudioFileWriter.Close: boolean;
begin
  Result := False;
  if Handle <> NIL then begin
    sf_write_sync(Handle);
    Result := sf_close(Handle) = 0;
  end;
  Handle := NIL;
end;

function TAudioFileWriter.WriteShort(p: Pointer; aCount: longword): longword;
begin
  if Handle <> NIL then
    Result := sf_writef_short(Handle, pcshort(p), aCount)
  else
    Result := 0;
end;

function TAudioFileWriter.WriteFloat(p: Pointer; aCount: longword): longword;
begin
  if Handle <> NIL then
    Result := sf_writef_float(Handle, pcfloat(p), aCount)
  else
    Result := 0;
end;

function TAudioFileWriter.WriteDouble(p: PDouble; aCount: longword): longword;
begin
  if Handle <> NIL then
    Result := sf_writef_double(Handle, pcdouble(p), aCount)
  else
    Result := 0;
end;

function TAudioFileWriter.Write(const aBuf: TALSPlaybackBuffer): longword;
begin
  Result := 0;

  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if (Handle = NIL) or (aBuf.FrameCount = 0) or (aBuf.OutOfMemory) then
    Result := 0
  else begin
    if aBuf.UseFloat then
      Result := sf_writef_float(Handle, pcfloat(aBuf.Data), aBuf.FrameCount)
    else
      Result := sf_writef_short(Handle, pcshort(aBuf.Data), aBuf.FrameCount);
  end;
end;

function TAudioFileWriter.CopyAll(const aSrcReader: TAudioFileReader): boolean;
var frameToRead: longword;
begin
  frameToRead := aSrcReader.Frames;
  if frameToRead = 0 then begin
    Result := True;
    exit;
  end;

  Result := aSrcReader.MoveToFrame(0);
  if not Result then exit;

  InitCopyBuffer(Min(32768, frameToRead));
  if FCopyBuffer.OutOfMemory then begin
    Result := False;
    exit;
  end;

  while (frameToRead > 0) and Result do begin
    aSrcReader.Read(FCopyBuffer, Min(FCopyBuffer.FrameCapacity, frameToRead));
    Result := Result and (Write(FCopyBuffer) = FCopyBuffer.FrameCount);
    frameToRead := frameToRead - FCopyBuffer.FrameCount;
  end;
  Result := Result and (frameToRead = 0);

  FCopyBuffer.FreeMemory;
end;

function TAudioFileWriter.CopyPart(const aSrcReader: TAudioFileReader;
  aFromFrameIndex, aToFrameIndex: int64): boolean;
var frameToRead: integer;
begin
  if aSrcReader.Frames = 0 then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFromFrameIndex < 0) or
     (aToFrameIndex < 0) or
     (aToFrameIndex < aFromFrameIndex) then exit;

  if (aFromFrameIndex <= 0) and (aToFrameIndex >= aSrcReader.Frames) then begin
    Result := CopyAll(aSrcReader);
    exit;
  end;

  Result := aSrcReader.MoveToFrame(aFromFrameIndex);
  if not Result then exit;

  frameToRead := aToFrameIndex-aFromFrameIndex+1;

  if frameToRead > 0 then begin
    InitCopyBuffer(Min(32768, frameToRead));
    if FCopyBuffer.OutOfMemory then begin
      Result := False;
      exit;
    end;

    FCopyBuffer.FrameCount := 1;
    while (frameToRead > 0) and (FCopyBuffer.FrameCount > 0) and Result do begin
      aSrcReader.Read(FCopyBuffer, Min(FCopyBuffer.FrameCapacity, frameToRead));
      Result := Result and (Write(FCopyBuffer) = FCopyBuffer.FrameCount);
      frameToRead := frameToRead - FCopyBuffer.FrameCount;
    end;
    Result := Result and (frameToRead = 0);

    FCopyBuffer.FreeMemory;
  end;
end;

procedure TAudioFileWriter.CopyMetaDataFrom(const aSrcReader: TAudioFileReader);
  procedure DoCopyStr(aStrType: cint);
  var s: PChar;
  begin
    s := sf_get_string(aSrcReader.Handle, aStrType);
    if s <> NIL then sf_set_string(Handle, aStrType, s);
  end;
begin
  DoCopyStr(SF_STR_TITLE);
  DoCopyStr(SF_STR_COPYRIGHT);
  DoCopyStr(SF_STR_SOFTWARE);
  DoCopyStr(SF_STR_ARTIST);
  DoCopyStr(SF_STR_COMMENT);
  DoCopyStr(SF_STR_DATE);
  DoCopyStr(SF_STR_ALBUM);
  DoCopyStr(SF_STR_LICENSE);
  DoCopyStr(SF_STR_TRACKNUMBER);
  DoCopyStr(SF_STR_GENRE);
end;

procedure TAudioFileWriter.WriteMetaData(const aTitle, aCopyright, aSoftware,
  aArtist, aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
  procedure DoWriteStr(aStrType: cint; const s: string);
  begin
    if s <> '' then sf_set_string(Handle, aStrType, PChar(s));
  end;
begin
  DoWriteStr(SF_STR_TITLE, aTitle);
  DoWriteStr(SF_STR_COPYRIGHT, aCopyright);
  DoWriteStr(SF_STR_SOFTWARE, aSoftware);
  DoWriteStr(SF_STR_ARTIST, aArtist);
  DoWriteStr(SF_STR_COMMENT, aComment);
  DoWriteStr(SF_STR_DATE, aDate);
  DoWriteStr(SF_STR_ALBUM, aAlbum);
  DoWriteStr(SF_STR_LICENSE, aLicense);
  DoWriteStr(SF_STR_TRACKNUMBER, aTrackNumber);
  DoWriteStr(SF_STR_GENRE, aGenre);
end;

{ TAudioFileUtils }

function TAudioFileUtils.GetDuration(const aFilename: string): single;
var reader: TAudioFileReader;
begin
  Result := 0;
  if reader.OpenRead(aFilename) then begin
    Result := reader.TotalDuration;
    reader.Close;
  end;
end;

function TAudioFileUtils.GetFramesCount(const aFilename: string): int64;
var reader: TAudioFileReader;
begin
  Result := 0;
  if reader.OpenRead(aFilename) then begin
    Result := reader.Frames;
    reader.Close;
  end;
end;

function TAudioFileUtils.CopyPart(const aSrcFile, aDstFile: string;
  aFrameIndexLow, aFrameIndexHigh: int64): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
begin
  Result := False;
  if not reader.OpenRead(aSrcFile) then exit;

  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  Result := writer.CopyPart(reader, aFrameIndexLow, aFrameIndexHigh);
  reader.Close;
  Result := Result and writer.Close;
end;

function TAudioFileUtils.CutPart(const aSrcFile, aDstFile: string; aFrameIndexLow, aFrameIndexHigh: int64): boolean;
var inputFile: TAudioFileReader;
  writer: TAudioFileWriter;
begin
  if aFrameIndexLow = aFrameIndexHigh then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFrameIndexLow < 0) or
     (aFrameIndexHigh < 0) or
     (aFrameIndexHigh < aFrameIndexLow) then exit;

  // open the file in read mode
  if not inputFile.OpenRead(aSrcFile) then exit;

  // Open the destination file in write mode
  if not writer.OpenWrite(aDstFile, inputFile.Format, inputFile.Channels, inputFile.SampleRate) then begin
    inputFile.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndexLow > 0 then
    writer.CopyPart(inputFile, 0, aFrameIndexLow-1);

  // copy the last part
  writer.CopyPart(inputFile, aFrameIndexHigh+1, inputFile.Frames-1);

  inputFile.Close;
  Result := writer.Close;
end;

function TAudioFileUtils.SilencePart(const aSrcFile, aDstFile: string;
  aFrameIndexLow, aFrameIndexHigh: int64; aAttenuationTime: single): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
  buf: TALSPlaybackBuffer;
  gain: TALSCustomBoundedFParam;
  p: PSingle;
  i, j, iRampUp: integer;
begin
  if (aFrameIndexLow = aFrameIndexHigh) then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFrameIndexLow < 0) or
     (aFrameIndexHigh < 0) or
     (aFrameIndexHigh < aFrameIndexLow) or
     (aAttenuationTime < 0) then exit;

  // open the file in read mode
  if not reader.OpenRead(aSrcFile) then exit;

  if reader.FrameToTime(aFrameIndexHigh-aFrameIndexLow+1) < aAttenuationTime*2 then
    aAttenuationTime := 0;

  // open the destination file in write mode
  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndexLow > 0 then
    if not writer.CopyPart(reader, 0, aFrameIndexLow-1) then begin
      reader.Close;
      writer.Close;
      if FileExists(aDstFile) then DeleteFile(aDstFile);
      exit;
    end;

  // read the whole selection in a buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := aFrameIndexHigh - aFrameIndexLow + 1;
  if buf.OutOfMemory then begin
    reader.Close;
    writer.Close;
    if FileExists(aDstFile) then DeleteFile(aDstFile);
    exit;
  end;

  if reader.Read(buf, buf.FrameCapacity) <> buf.FrameCapacity then begin
    reader.Close;
    writer.Close;
    if FileExists(aDstFile) then DeleteFile(aDstFile);
    buf.FreeMemory;
    exit;
  end;

  // apply silence on the buffer
  if aAttenuationTime = 0 then
    buf.FillWithSilence
  else begin
    gain := TALSCustomBoundedFParam.Create(0.0, 1.0, 1.0);
    gain.ChangeTo(0.0, aAttenuationTime, ALS_StartFastEndSlow);
    iRampUp := buf.FrameCapacity-Trunc(reader.SampleRate*aAttenuationTime);
    for i:=0 to buf.FrameCapacity-1 do begin
      p := PSingle(buf.DataOffset[i]);
      for j:=0 to buf.ChannelCount-1 do begin
        p^ := p^*gain.Value;
        inc(p);
      end;
      gain.OnElapse(1/reader.SampleRate);
      if (i >= iRampUp) and (gain.State = alspsNO_CHANGE) then
        gain.ChangeTo(1.0, aAttenuationTime, ALS_StartSlowEndFast);
    end;
    gain.Free;
  end;

  // write buffer
  if writer.Write(buf) <> buf.FrameCount then begin
    reader.Close;
    writer.Close;
    if FileExists(aDstFile) then DeleteFile(aDstFile);
    buf.FreeMemory;
    exit;
  end;

  // copy the remaining part
  buf.FrameCapacity := 32768;
  Result := True;
  while (reader.Read(buf, buf.FrameCapacity) > 0) and Result do
    Result := Result and (writer.Write(buf) = buf.FrameCount);

  buf.FreeMemory;
  reader.Close;
  Result := Result and writer.Close;

  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

function TAudioFileUtils.GenerateSilence(const aFilename: string;
  aChannelsCount, aSampleRate: integer; aFormat: TALSFileFormat;
  aDuration: single): boolean;
var writer: TAudioFileWriter;
  buf: TALSPlaybackBuffer;
  frameToWrite, frameWritten, c: int64;
begin
  Result := False;

  frameToWrite := Ceil(aSampleRate*aDuration);
  if frameToWrite <= 0 then exit;

  buf.Init(aChannelsCount, ALS_SAMPLE_INT16);
  buf.FrameCapacity := 32768;
  if buf.OutOfMemory then exit;
  buf.FillWithSilence;

  Result := writer.OpenWrite(aFilename, aFormat, aChannelsCount, aSampleRate);
  if not Result then begin
    buf.FreeMemory;
    exit;
  end;

  frameWritten := 0;
  c := frameToWrite;
  while c > 0 do begin
    buf.FrameCount := Min(buf.FrameCapacity, c);
    frameWritten := frameWritten + writer.Write(buf);
    dec(c, buf.FrameCount);
  end;

  writer.Close;
  buf.FreeMemory;
  Result := frameWritten = frameToWrite;
end;

function TAudioFileUtils.InsertSilence(const aSrcFile, aDstFile: string;
  aFrameIndex: int64; aDuration: single): boolean;
var reader: TAudioFileReader;
    writer: TAudioFileWriter;
    buf: TALSPlaybackBuffer;
    frameAdded: int64;
begin
  Result := False;

  // open the target file in read mode
  if not reader.OpenRead(aSrcFile) then exit;

  // Open the destination file in write mode with same attributes than the source
  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndex > 0 then
    if not writer.CopyPart(reader, 0, aFrameIndex) then begin
      reader.Close;
      writer.Close;
      DeleteFile(aDstFile);
      exit;
    end;

  frameAdded := Ceil(reader.SampleRate*aDuration);

  // fill a buffer with '0'
  if writer.SampleAreFloat then
    buf.Init(writer.Channels, ALS_SAMPLE_FLOAT32)
  else
    buf.Init(writer.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := frameAdded;
  if buf.OutOfMemory then begin
    reader.Close;
    writer.Close;
    DeleteFile(aDstFile);
    exit;
  end;
  buf.FillWithSilence;
  buf.FrameCount := buf.FrameCapacity;

  // insert silence
  Result := writer.Write(buf) = buf.FrameCapacity;

  // copy the last part
  if buf.FrameCapacity < 32768 then begin
    buf.FrameCapacity := 32768;
    if buf.OutOfMemory then begin
      reader.Close;
      writer.Close;
      DeleteFile(aDstFile);
      Result := False;
      exit;
    end;
  end;

  while reader.Read(buf, buf.FrameCapacity) > 0 do
    Result := Result and (writer.Write(buf) = buf.FrameCount);

  reader.Close;
  Result := Result and writer.Close;

  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
  buf.FreeMemory;
end;

function TAudioFileUtils.AppendFilesToWriter(aFilenamesToAppend: TStringArray;
  const aTargetWriter: TAudioFileWriter): boolean;
var reader: TAudioFileReader;
  i: integer;
begin
  Result := False;

  for i:=0 to High(aFilenamesToAppend) do begin
    if not reader.OpenRead(aFilenamesToAppend[i]) then exit;

    Result := aTargetWriter.CopyAll(reader);
    reader.Close;
    if not Result then exit;
  end;
end;

function TAudioFileUtils.AppendFiles(const aSrcFile: string;
  const aFilenamesToAppend: TStringArray; aDstFile: string): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
  i: integer;
  flagFicOpened: boolean;
begin
  Result := False;

  // open the target file to retrieve its info
  if not reader.OpenRead(aSrcFile) then exit;

  // open the destination file in write mode with same attributes as the source file
  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then exit;

  // Copy the content of the source file
  if not writer.CopyAll(reader) then begin
    reader.Close;
    writer.Close;
    exit;
  end;
  reader.Close;

  // Copy the content of other files
  Result := True;
  for i:=0 to High(aFilenamesToAppend) do begin
    if Result then Result := reader.OpenRead(aFilenamesToAppend[i]);
    flagFicOpened := Result;
    if Result then Result := writer.CopyAll(reader);
    if flagFicOpened then reader.Close;
  end;

  Result := Result and writer.Close;
  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

function TAudioFileUtils.InsertFile(const aSrcFile, aFileToInsert,
  aDstFile: string; aInsertAtFrameIndex: int64): boolean;
var readerSrc, readerInsert: TAudioFileReader;
    writer: TAudioFileWriter;
    buf: TALSPlaybackBuffer;
    frameToRead, totalFrame: int64;
begin
  Result := False;
  if aInsertAtFrameIndex < 0 then exit;

  // open the target file in read mode
  if not readerSrc.OpenRead(aSrcFile) then exit;

  // if index is out of bound -> append the file
  if aInsertAtFrameIndex >= readerSrc.Frames-1 then begin
    readerSrc.Close;
    Result := AppendFiles(aSrcFile, [aFileToInsert], aDstFile);
    exit;
  end;

  // open the file to insert in read mode
  if not readerInsert.OpenRead(aFileToInsert) then begin
    readerSrc.Close;
    exit;
  end;

  // check if the 2 file have same channel's count
  if readerSrc.Channels <> readerInsert.Channels then begin
    readerSrc.Close;
    readerInsert.Close;
    exit;
  end;

  // Open the destination file in write mode with same attributes as source file
  if not writer.OpenWrite(aDstFile, readerSrc.Format, readerSrc.Channels, readerSrc.SampleRate) then begin
    readerSrc.Close;
    readerInsert.Close;
    exit;
  end;

  // Init buffer
  if writer.SampleAreFloat then
    buf.Init(writer.Channels, ALS_SAMPLE_FLOAT32)
  else
    buf.Init(writer.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := 32768;
  if buf.OutOfMemory then begin
    readerSrc.Close;
    readerInsert.Close;
    exit;
  end;

  totalFrame := readerSrc.Frames + readerInsert.Frames;

  // copy the first part of the source filename
  if aInsertAtFrameIndex = 0 then
    frameToRead := 0
  else
    frameToRead := aInsertAtFrameIndex+1;

  while frameToRead > 0 do begin
    readerSrc.Read(buf, Min(buf.FrameCapacity, frameToRead));
    writer.Write(buf);
    dec(frameToRead, buf.FrameCount);
    dec(totalFrame, buf.FrameCount);
  end;

  // copy data from the file to insert
  while readerInsert.Read(buf, buf.FrameCapacity) > 0 do begin
    writer.Write(buf);
    dec(totalFrame, buf.FrameCount);
  end;

  // copy the last part of the target filename
  while readerSrc.Read(buf, buf.FrameCapacity) > 0 do begin
    writer.Write(buf);
    dec(totalFrame, buf.FrameCount);
  end;

  // close all files
  buf.FreeMemory;
  readerSrc.Close;
  readerInsert.Close;
  Result := writer.Close and (totalFrame = 0);
  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

function TAudioFileUtils.ReplacePartByFile(const aSrcFile: string;
  aFrameIndexLow, aFrameIndexHigh: int64; const aSubstitutionFile,
  aDstFile: string): boolean;
var readerSrc, readerNewData: TAudioFileReader;
    writer: TAudioFileWriter;
begin
  Result := False;

  // open the target file in read mode
  if not readerSrc.OpenRead(aSrcFile) then exit;

  // open the file to insert in read mode
  if not readerNewData.OpenRead(aSubstitutionFile) then begin
    readerSrc.Close;
    exit;
  end;

  // check if the 2 file have same channel's count
  if readerSrc.Channels <> readerNewData.Channels then begin
    readerSrc.Close;
    readerNewData.Close;
    exit;
  end;

  // Open the destination file in write mode with same attributes than source
  if not writer.OpenWrite(aDstFile, readerSrc.Format, readerSrc.Channels, readerSrc.SampleRate) then begin
    readerSrc.Close;
    readerNewData.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndexLow > 0 then
    Result := writer.CopyPart(readerSrc, 0, aFrameIndexLow-1)
  else Result := True;

  // copy the new data
  if Result then
    Result := writer.CopyAll(readerNewData);

  // copy the remaining part
  if Result and (aFrameIndexHigh < readerSrc.Frames-1) then
    Result := writer.CopyPart(readerSrc, aFrameIndexHigh+1, readerSrc.Frames-1);

  readerSrc.Close;
  readerNewData.Close;
  Result := Result and writer.Close;
  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

function TAudioFileUtils.Normalize(const aSrcFile, aDstFile: string; aRemoveDC: boolean; aMaxdB: single): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
  peak, offset, offset2, ratio: double;
  gain: single;
  processedCount, i, j: integer;
  buf: TALSPlaybackBuffer;
  A: ArrayOfSingle;
  frameToRead: int64;
  p: PSingle;
begin
  Result := False;

  if not reader.OpenRead(aSrcFile) then exit;

  if reader.Frames = 0 then exit(True);

  // init buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 32768;
  if buf.OutOfMemory then begin
    reader.Close;
    exit;
  end;

  // compute DC offset and peak levels (within buf) for the whole file
  offset := 0.0;
  processedCount := 0;
  frameToRead := reader.Frames;
  while reader.Read(buf, Min(buf.FrameCapacity, frameToRead)) > 0 do begin
    buf.ComputeChannelsLevel;

    if aRemoveDC then begin
      A := dsp_Mean_Float(PSingle(buf.Data), buf.FrameCount, buf.ChannelCount);
      offset2 := 0.0;
      for i:=0 to High(A) do
        offset2 := offset2 + A[i];
      offset := offset + offset2/Length(A);
      inc(processedCount);
    end;

    frameToRead := frameToRead-buf.FrameCount;
  end;

  if aRemoveDC then begin
    if processedCount > 0 then
      offset := offset/processedCount
    else
      offset := 0.0;
  end;

  // all samples are processed ?
  if frameToRead > 0 then begin
    reader.Close;
    buf.FreeMemory;
    exit;
  end;

  // get the maximum peak between channels
  peak :=0.0;
  for i:=0 to buf.ChannelCount-1 do
    if peak < buf.ChannelsPeak[i] then peak := buf.ChannelsPeak[i];

  // compute the gain to apply
  ratio := dBToLinear(EnsureRange(aMaxdB, ALS_DECIBEL_MIN_VALUE, ALS_DECIBEL_MAX_VALUE));
  if peak > 0 then
    gain := ratio / (peak - offset)
  else
    gain := 1.0;

  // create the destination file in write mode with same attributes than source
  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    buf.FreeMemory;
    if FileExists(aDstFile) then DeleteFile(aDstFile);
    exit;
  end;

  // place reader to its beginning
  if not reader.MoveToFrame(0) then begin
    reader.Close;
    writer.Close;
    buf.FreeMemory;
    if FileExists(aDstFile) then DeleteFile(aDstFile);
    exit;
  end;

  // read samples, apply offset and gain, write samples
  Result := True;
  frameToRead := reader.Frames;
  while reader.Read(buf, Min(buf.FrameCapacity, frameToRead)) > 0 do begin
    p := PSingle(buf.Data);
    for i:=0 to buf.FrameCount-1 do
      for j:=0 to buf.ChannelCount-1 do begin
        p^ := (p^ + offset) * gain;
        inc(p);
      end;

    Result := Result and (writer.Write(buf) = buf.FrameCount);

    frameToRead := frameToRead - buf.FrameCount;
  end;

  Result := Result and (frameToRead = 0);

  reader.Close;
  Result := Result and writer.Close;
  buf.FreeMemory;
  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

function TAudioFileUtils.Convert(const aSrcFile, aDstFile: string;
  aDstFileFormat: TALSFileFormat; aForceMono: boolean): boolean;
var reader: TAudioFileReader;
  bufRead, bufWrite: TALSPlaybackBuffer;
  writer: TAudioFileWriter;
  metadata: TALSFileMetaData;
  chanWrite, i, j: integer;
  written: longword;
  pRead, pWrite: PSingle;
  sum: single;
begin
  Result := False;

  // open file in read mode
  if not reader.OpenRead(aSrcFile) then exit;

  // read metadata
  metadata.ReadMetaDataFrom(reader.Handle);

  if aForceMono
    then chanWrite := 1
    else chanWrite := reader.Channels;

  // Open dst file in write mode
  if not writer.OpenWrite(aDstFile, aDstFileFormat, chanWrite, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // write metadata
  metadata.WriteMetaDataTo(writer.Handle);

  // prepare buffers
  bufRead.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  bufRead.FrameCapacity := 32768;
  if bufRead.OutOfMemory then begin
    reader.Close;
    writer.Close;
    DeleteFile(aDstFile);
    exit;
  end;

  if chanWrite = reader.Channels then begin
    // simple copy
    Result := True;
    while (reader.Read(bufRead, bufRead.FrameCapacity) > 0) and Result do begin
      written := writer.Write(bufRead);
      Result := written = bufRead.FrameCount;
    end;

  end else begin
    // converts multi channels to MONO
    bufWrite.Init(chanWrite, bufRead.Format);
    bufWrite.FrameCapacity := bufRead.FrameCapacity;
    if bufWrite.OutOfMemory then begin
      bufRead.FreeMemory;
      reader.Close;
      writer.Close;
      DeleteFile(aDstFile);
      exit;
    end;

    Result := True;
    while (reader.Read(bufRead, bufRead.FrameCapacity) > 0) and Result do begin
      pRead := PSingle(bufRead.Data);
      pWrite := PSingle(bufWrite.Data);
      for i:=1 to bufRead.FrameCount do begin
        sum := 0;
        for j:=1 to bufRead.ChannelCount do begin
          sum := sum+pRead^;
          inc(pRead);
        end;
        pWrite^ := sum/bufRead.ChannelCount;
        inc(pWrite);
      end;
      bufWrite.FrameCount := bufRead.FrameCount;
      written := writer.Write(bufWrite);
      Result := written = bufRead.FrameCount;
    end;
    bufWrite.FreeMemory;
  end;

  bufRead.FreeMemory;
  reader.Close;
  Result := Result and writer.Close;

  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

function TAudioFileUtils.ApplyGain(const aSrcFile, aDstFile: string; aGaindB: single): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  writer: TAudioFileWriter;
  g: single;
  written: longword;
  metadata: TALSFileMetaData;
begin
  Result := False;
  // open file in read mode
  if not reader.OpenRead(aSrcFile) then exit;

  // read metadata
  metadata.ReadMetaDataFrom(reader.Handle);

  // Open the destination file in write mode with same attributes as source
  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // write metadata
  metadata.WriteMetaDataTo(writer.Handle);

  // prepare buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 32768;

  Result := True;
  g := dBToLinear(aGaindB);
  // read audio in buffer and apply gain
  while (reader.Read(buf, buf.FrameCapacity) > 0) and Result do begin
    buf.Amplify(g);
    written := writer.Write(buf);
    Result := written = buf.FrameCount;
  end;

  reader.Close;
  buf.FreeMemory;
  Result := Result and writer.Close;
  if not Result and FileExists(aDstFile) then DeleteFile(aDstFile);
end;

{$endif}
