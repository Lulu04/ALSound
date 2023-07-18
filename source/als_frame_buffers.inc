{$ifdef ALS_INTERFACE}
//
//   Buffer definition
//


type

  { TALSFrameBufferBase }

  TALSFrameBufferBase = object
  private
    FOutOfMemory: boolean;
    FUseFloat: boolean;
    FChannelCount,
    FBytePerFrame,
    FBytePerChannel: SmallInt;
    FFrameCapacity,
    FFrameCount: longword;
    FData: Pointer;
    FChannelsLevel,               // the level (linear) of each channel [0..1]
    FChannelsPeak: ArrayOfSingle; // the peak (max) value
    procedure InitDefault;
    procedure SetFrameCapacity(AValue: longword);
    procedure SetChannelCount(AValue: SmallInt);
    function GetChannelsLevel(aIndex: integer): single;
    function GetChannelsLeveldB(aIndex: integer): single;
    function GetChannelsPeak(aIndex: integer): single;
    function GetChannelsPeakdB(aIndex: integer): single;
  private
    function GetDataOffset(frameOffset: longword): Pointer;

    procedure ResetChannelsLevelToZero;
  public
    procedure FreeMemory;
    procedure ComputeChannelsLevel;

    procedure FillWithSilence;
    procedure Amplify(aGain: single);

    property OutOfMemory: boolean read FOutOfMemory;
    property UseFloat: boolean read FUseFloat;
    property ChannelCount: SmallInt read FChannelCount;
    property ChannelsLevel[index:integer]: single read GetChannelsLevel; // for stereo buffer first is left then right
    property ChannelsLeveldB[index:integer]: single read GetChannelsLeveldB;
    property ChannelsPeak[index:integer]: single read GetChannelsPeak; // for stereo buffer first is left then right
    property ChannelsPeakdB[index:integer]: single read GetChannelsPeakdB; // for stereo buffer first is left then right
    property BytePerFrame: SmallInt read FBytePerFrame;
    property BytePerChannel: SmallInt read FBytePerChannel;
    property FrameCapacity: longword read FFrameCapacity write SetFrameCapacity; // the total capacity
    property FrameCount: longword read FFrameCount write FFrameCount; // the number of frames written to the buffer (can be < than FrameCapacity)
    property Data: Pointer read FData;
    property DataOffset[frameOffset:longword]: Pointer read GetDataOffset;// return the adress of a frame
  end;


  TALSPlaybackSampleType=
  (    ALS_SAMPLE_INT16,
       ALS_SAMPLE_FLOAT32 );

  { TALSPlaybackBuffer }

  TALSPlaybackBuffer = object(TALSFrameBufferBase)
  private
    FFormat: TALSPlaybackSampleType;
    FBufferID: ALuint;
    FQueued: boolean;
    function GenerateBufferID: ALenum;
    procedure DeleteBufferID;
  public
    procedure Init(aChannelCount: integer; aFormat: TALSPlaybackSampleType);

    property Format: TALSPlaybackSampleType read FFormat;
    property BufferID: ALuint read FBufferID;
    property Queued: boolean read FQueued write FQueued;
  end;

  function ALSPlaybackSampleTypeToBytePerChannel(aFormat: TALSPlaybackSampleType): integer;


type
  { TALSCaptureFrameBuffer }

  TALSCaptureFrameBuffer = object(TALSFrameBufferBase)
  private
    FFormat: TALSCaptureFormat;
  public
    procedure Init(aFormat: TALSCaptureFormat);
    procedure RemoveDCBias;
    property Format: TALSCaptureFormat read FFormat;
  end;
  PALSCaptureFrameBuffer = ^TALSCaptureFrameBuffer;

  function ALSCaptureFormatToChannelCount(aFormat: TALSCaptureFormat): SmallInt;
  function ALSCaptureFormatToBytePerChannel(aFormat: TALSCaptureFormat): SmallInt;
  function ALSCaptureFormatToBytePerFrame(aFormat: TALSCaptureFormat): SmallInt;


type
  { TALSLoopbackFrameBuffer }

  TALSLoopbackFrameBuffer = object(TALSFrameBufferBase)
  private
    FChannelType: TALSLoopbackChannel;
    FSampleType: TALSLoopbackSampleType;
  public
    procedure Init(aChannelType: TALSLoopbackChannel; aSampleType: TALSLoopbackSampleType);
    property ChannelType: TALSLoopbackChannel read FChannelType;
    property SampleType: TALSLoopbackSampleType read FSampleType;
  end;

  function ALSLoopbackChannelTypeToChannelCount(aChannelType: TALSLoopbackChannel): SmallInt;
  function ALSLoopbackSampleTypeToBytePerChannel(aSampleType: TALSLoopbackSampleType): SmallInt;
  function ALSLoopbackSampleTypeToAudioFileSubFormat(aSampleType: TALSLoopbackSampleType): cint;

{$endif}

{$ifdef ALS_IMPLEMENTATION}

{ TALSLoopbackFrameBuffer }
function ALSLoopbackChannelTypeToChannelCount(aChannelType: TALSLoopbackChannel): SmallInt;
begin
 case aChannelType of
   ALC_MONO_SOFT: Result := 1;
   ALC_STEREO_SOFT: Result := 2;
   ALC_QUAD_SOFT: Result := 4;
   ALC_5POINT1_SOFT: Result := 6;
   ALC_6POINT1_SOFT: Result := 7;
   ALC_7POINT1_SOFT: Result := 8;
   else Raise Exception.Create('Missing enum item in case');
 end;
end;

function ALSLoopbackSampleTypeToBytePerChannel(aSampleType: TALSLoopbackSampleType): SmallInt;
begin
 case aSampleType of
   ALC_SHORT_SOFT: Result := 2;
   ALC_INT_SOFT: Result := 4;
   ALC_FLOAT_SOFT: Result := 4;
   else Raise Exception.Create('Missing enum item in case');
 end;
end;

function ALSLoopbackSampleTypeToAudioFileSubFormat(aSampleType: TALSLoopbackSampleType): cint;
begin
 case aSampleType of
   ALC_SHORT_SOFT: Result := libsndfile.SF_FORMAT_PCM_16;
   ALC_INT_SOFT: Result := libsndfile.SF_FORMAT_PCM_32;
   ALC_FLOAT_SOFT: Result := libsndfile.SF_FORMAT_FLOAT;
   else Raise Exception.Create('Missing enum item in case');
 end;
end;

procedure TALSLoopbackFrameBuffer.Init(aChannelType: TALSLoopbackChannel;
  aSampleType: TALSLoopbackSampleType);
begin
  InitDefault;
  FChannelType := aChannelType;
  FSampleType := aSampleType;
  SetChannelCount(ALSLoopbackChannelTypeToChannelCount(FChannelType));
  FBytePerChannel := ALSLoopbackSampleTypeToBytePerChannel(aSampleType);
  FBytePerFrame := ChannelCount * BytePerChannel;
  FUseFloat := aSampleType=ALC_FLOAT_SOFT;
end;

function ALSPlaybackSampleTypeToBytePerChannel(aFormat: TALSPlaybackSampleType): integer;
begin
 case aFormat of
   ALS_SAMPLE_INT16: Result := 2;
   ALS_SAMPLE_FLOAT32: Result := 4;
   else Raise Exception.Create('Missing enum item in case');
 end;
end;

{ TALSCaptureFrameBuffer }
function ALSCaptureFormatToChannelCount(aFormat: TALSCaptureFormat): SmallInt;
begin
  case aFormat of
    ALS_CAPTUREFORMAT_MONO16,
    ALS_CAPTUREFORMAT_MONO_FLOAT32: Result := 1;

    ALS_CAPTUREFORMAT_STEREO16,
    ALS_CAPTUREFORMAT_STEREO_FLOAT32: Result := 2;
    else Raise Exception.Create('Missing enum item in case');
  end;
end;

function ALSCaptureFormatToBytePerChannel(aFormat: TALSCaptureFormat): SmallInt;
begin
  case aFormat of
    ALS_CAPTUREFORMAT_MONO16,
    ALS_CAPTUREFORMAT_STEREO16: Result := SizeOf(SmallInt);

    ALS_CAPTUREFORMAT_MONO_FLOAT32,
    ALS_CAPTUREFORMAT_STEREO_FLOAT32: Result := SizeOf(Single);
    else Raise Exception.Create('Missing enum item in case');
  end;
end;

function ALSCaptureFormatToBytePerFrame(aFormat: TALSCaptureFormat): SmallInt;
begin
  Result := ALSCaptureFormatToChannelCount(aFormat) * ALSCaptureFormatToBytePerChannel(aFormat);
end;

procedure TALSCaptureFrameBuffer.Init(aFormat: TALSCaptureFormat);
begin
  InitDefault;
  SetChannelCount(ALSCaptureFormatToChannelCount(aFormat));
  FBytePerChannel := ALSCaptureFormatToBytePerChannel(aFormat);
  FBytePerFrame := ChannelCount * BytePerChannel;
  FUseFloat := (aFormat = ALS_CAPTUREFORMAT_MONO_FLOAT32) or
               (aFormat = ALS_CAPTUREFORMAT_STEREO_FLOAT32);
end;

procedure TALSCaptureFrameBuffer.RemoveDCBias;
begin
  if FFrameCount = 0 then exit;

  case Format of
    ALS_CAPTUREFORMAT_MONO16, ALS_CAPTUREFORMAT_STEREO16:
      dsp_RemoveDCBias_Smallint(Data, FFrameCount, FChannelCount);

    ALS_CAPTUREFORMAT_MONO_FLOAT32, ALS_CAPTUREFORMAT_STEREO_FLOAT32:
      dsp_RemoveDCBias_Float(PSingle(Data), FFrameCount, FChannelCount);
  end;
end;

{ TALSPlaybackBuffer }

function TALSPlaybackBuffer.GenerateBufferID: ALenum;
begin
   alGetError();
   alGenBuffers(1, @FBufferID);
   Result := alGetError();
end;

procedure TALSPlaybackBuffer.DeleteBufferID;
begin
  if FBufferID <> 0 then
    alDeleteBuffers(1, @FBufferID);
end;

procedure TALSPlaybackBuffer.Init(aChannelCount: integer; aFormat: TALSPlaybackSampleType);
begin
  InitDefault;
  FFormat := aFormat;
  SetChannelCount( aChannelCount );
  FBytePerChannel := ALSPlaybackSampleTypeToBytePerChannel(aFormat);
  FBytePerFrame := ChannelCount * BytePerChannel;
  FUseFloat := (aFormat = ALS_SAMPLE_FLOAT32);
  FBufferID := 0;
  FQueued := False;
end;

{ TALSFrameBufferBase }

procedure TALSFrameBufferBase.InitDefault;
begin
  FData := nil;
  FFrameCapacity := 0;
  FFrameCount := 0;
  FChannelsLevel := nil;
  FChannelsPeak := nil;
  FOutOfMemory := False;
end;

procedure TALSFrameBufferBase.SetFrameCapacity(AValue: longword);
begin
  FreeMemory;
  ReturnNilIfGrowHeapFails := True; // no exception, GetMem return NIL if not enough memory
  FData := GetMem(AValue * longword(FBytePerFrame));
  if FData <> NIL then
    FFrameCapacity := AValue
  else
    FFrameCapacity := 0;
  FOutOfMemory := FData = NIL;
end;

procedure TALSFrameBufferBase.SetChannelCount(AValue: SmallInt);
begin
  FChannelCount := AValue;
  SetLength(FChannelsLevel, FChannelCount);
  SetLength(FChannelsPeak, FChannelCount);
  ResetChannelsLevelToZero;
end;

procedure TALSFrameBufferBase.FreeMemory;
begin
  if FData <> nil then
    FreeMem( FData );
  FData := nil;
  FFrameCapacity := 0;
  FFrameCount := 0;
  ResetChannelsLevelToZero;
  FOutOfMemory := False;
end;

function TALSFrameBufferBase.GetDataOffset(frameOffset: longword): Pointer;
begin
  Result := FData + BytePerFrame * frameOffset;
end;

procedure TALSFrameBufferBase.ResetChannelsLevelToZero;
var
  i: integer;
begin
  for i:=0 to High(FChannelsLevel) do
  begin
    FChannelsLevel[i] := 0.0;
    FChannelsPeak[i] := 0.0;
  end;
end;

procedure TALSFrameBufferBase.ComputeChannelsLevel;
var
  i: integer;
begin
  if FFrameCount = 0 then
    exit;

  if FUseFloat then
    dsp_ComputeLinearLevel_Float(PSingle(Data), FFrameCount, FChannelCount, @FChannelsLevel[0])
  else
    dsp_ComputeLinearLevel_Smallint(PSmallint(Data), FFrameCount, FChannelCount, @FChannelsLevel[0]);

 for i:=0 to High(FChannelsPeak) do
   if FChannelsPeak[i] < FChannelsLevel[i] then
     FChannelsPeak[i] := FChannelsLevel[i];
end;

procedure TALSFrameBufferBase.FillWithSilence;
begin
  if FUseFloat then
    FillChar(FData^, FFrameCapacity*FBytePerFrame, $00);
end;

procedure TALSFrameBufferBase.Amplify(aGain: single);
begin
  if aGain = 1.0 then
    exit;

  if FUseFloat then
    dsp_Amplify_Float(PSingle(Data), FFrameCount, FChannelCount, aGain)
  else
    dsp_Amplify_Smallint(PSmallint(Data), FFrameCount, FChannelCount, aGain);
end;

function TALSFrameBufferBase.GetChannelsLevel(aIndex: integer): single;
begin
  if (aIndex >= 0) and (aIndex < Length(FChannelsLevel)) then
    Result := Sqrt(FChannelsLevel[aIndex])
  else
    Result := 0.0;
end;

function TALSFrameBufferBase.GetChannelsLeveldB(aIndex: integer): single;
begin
  if (aIndex >= 0) and (aIndex < Length(FChannelsLevel)) then
    Result := LinearTodB(FChannelsLevel[aIndex])
  else
    Result := ALS_DECIBEL_MIN_VALUE;
end;

function TALSFrameBufferBase.GetChannelsPeak(aIndex: integer): single;
begin
  if (aIndex >= 0) and (aIndex < Length(FChannelsPeak)) then
    Result := Sqrt(FChannelsPeak[aIndex])
  else
    Result := 0.0
end;

function TALSFrameBufferBase.GetChannelsPeakdB(aIndex: integer): single;
begin
  if (aIndex >= 0) and (aIndex < Length(FChannelsPeak)) then
    Result := LinearTodB(FChannelsPeak[aIndex])
  else
    Result := ALS_DECIBEL_MIN_VALUE;
end;

{$endif}

