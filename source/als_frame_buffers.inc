{$ifdef ALS_INTERFACE}
//
//   Buffer definition
//


type

  { TALSFrameBufferBase }

  TALSFrameBufferBase = object
  private
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
    procedure FreeMemory;

    procedure ResetChannelsLevelToZero;
  public
    procedure ComputeChannelsLevel;

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
  end;


  { TALSPlaybackBuffer }

  TALSPlaybackBuffer = object(TALSFrameBufferBase)
    procedure Init(aChannelCount: integer; aUseFloat: boolean);
  end;


  { TALSCaptureFrameBuffer }

  TALSCaptureFrameBuffer = object(TALSFrameBufferBase)
  private
    FFormat: TALSCaptureFormat;
  public
    procedure Init(aFormat: TALSCaptureFormat);
    procedure RemoveDCBias(aFrameCount: longword);
    property Format: TALSCaptureFormat read FFormat;
  end;

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
 end;
end;

function ALSLoopbackSampleTypeToBytePerChannel(aSampleType: TALSLoopbackSampleType): SmallInt;
begin
 case aSampleType of
   ALC_SHORT_SOFT: Result := 2;
   ALC_INT_SOFT: Result := 4;
   ALC_FLOAT_SOFT: Result := 4;
 end;
end;

function ALSLoopbackSampleTypeToAudioFileSubFormat(aSampleType: TALSLoopbackSampleType): cint;
begin
 case aSampleType of
   ALC_SHORT_SOFT: Result := libsndfile.SF_FORMAT_PCM_16;
   ALC_INT_SOFT: Result := libsndfile.SF_FORMAT_PCM_32;
   ALC_FLOAT_SOFT: Result := libsndfile.SF_FORMAT_FLOAT;
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

{ TALSCaptureFrameBuffer }
function ALSCaptureFormatToChannelCount(aFormat: TALSCaptureFormat): SmallInt;
begin
  case aFormat of
    ALS_CAPTUREFORMAT_MONO16,
    ALS_CAPTUREFORMAT_MONO_FLOAT32: Result := 1;

    ALS_CAPTUREFORMAT_STEREO16,
    ALS_CAPTUREFORMAT_STEREO_FLOAT32: Result := 2;
  end;
end;

function ALSCaptureFormatToBytePerChannel(aFormat: TALSCaptureFormat): SmallInt;
begin
  case aFormat of
    ALS_CAPTUREFORMAT_MONO16,
    ALS_CAPTUREFORMAT_STEREO16: Result := SizeOf(SmallInt);

    ALS_CAPTUREFORMAT_MONO_FLOAT32,
    ALS_CAPTUREFORMAT_STEREO_FLOAT32: Result := SizeOf(Single);
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

procedure TALSCaptureFrameBuffer.RemoveDCBias(aFrameCount: longword);
begin
  if aFrameCount = 0 then exit;

  case Format of
    ALS_CAPTUREFORMAT_MONO16, ALS_CAPTUREFORMAT_STEREO16:
      dsp_RemoveDCBias_Smallint(Data, aFrameCount, FChannelCount);

    ALS_CAPTUREFORMAT_MONO_FLOAT32, ALS_CAPTUREFORMAT_STEREO_FLOAT32:
      dsp_RemoveDCBias_Float(PSingle(Data), aFrameCount, FChannelCount);
  end;
end;

{ TALSPlaybackBuffer }

procedure TALSPlaybackBuffer.Init(aChannelCount: integer; aUseFloat: boolean);
begin
  InitDefault;
  SetChannelCount(aChannelCount);
  if aUseFloat then
    FBytePerChannel := 4
  else
   FBytePerChannel := 2;
  FBytePerFrame := ChannelCount * BytePerChannel;
  FUseFloat := aUseFloat;
end;

{ TALSFrameBufferBase }

procedure TALSFrameBufferBase.InitDefault;
begin
  FData := nil;
  FFrameCapacity := 0;
  FFrameCount := 0;
  FChannelsLevel := nil;
  FChannelsPeak := nil;
end;

procedure TALSFrameBufferBase.SetFrameCapacity(AValue: longword);
begin
  FreeMemory;
  FData := GetMem(AValue * longword(FBytePerFrame));
  FFrameCapacity := AValue;
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
    FChannelsLevel := Copy(dsp_ComputeLinearLevel_Float(PSingle(Data), FFrameCount, FChannelCount), 0, FChannelCount)
  else
    FChannelsLevel := Copy(dsp_ComputeLinearLevel_Smallint(PSmallint(Data), FFrameCount, FChannelCount), 0, FChannelCount);

 for i:=0 to High(FChannelsPeak) do
   if FChannelsPeak[i] < FChannelsLevel[i] then
     FChannelsPeak[i] := FChannelsLevel[i];
end;

function TALSFrameBufferBase.GetChannelsLevel(aIndex: integer): single;
begin
  if (aIndex >= 0) and (aIndex < Length(FChannelsLevel)) then
    Result := FChannelsLevel[aIndex]
  else
    Result := 0.0;
end;

function TALSFrameBufferBase.GetChannelsLeveldB(aIndex: integer): single;
begin
  Result := ALSPercentToDecibel(GetChannelsLevel(aIndex));
end;

function TALSFrameBufferBase.GetChannelsPeak(aIndex: integer): single;
begin
  if (aIndex < 0) or (aIndex > High(FChannelsPeak)) then
    Result := 0.0
  else
    Result := FChannelsPeak[aIndex];
end;

function TALSFrameBufferBase.GetChannelsPeakdB(aIndex: integer): single;
begin
  Result := ALSPercentToDecibel(GetChannelsPeak(aIndex));
end;

{$endif}

