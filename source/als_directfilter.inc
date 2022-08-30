{

   To control the bass/treble (tone) and the dry gain we attach a direct filter to the
   source.

   An another filter attached on an auxiliary send controls the
   bass/treble and the gain of the audio sent to the effect.

   More explanation on https://github.com/kcat/openal-soft/issues/673

   The type of filter used depends of those available. The choice is made in
   this order:

     1 - BAND PASS FILTER  allow both bass and treble control.
     2 - LOW PASS FILTER   only treble control
     3 - HIGH PASS FILTER  only bass control.

   If filters are not available or the EFX extension is not present, setting the
   bass/treble or dry/wet gain of a source have no effect.

}


{$ifdef ALS_INTERFACE}
type

  TALSFilterCallBack = procedure of object;

  { TALSFilterBase }

  TALSFilterBase = object
  private
    FParentSound: TALSSound;
    FGlobalGain,
    FLowFreqGain,
    FBandPassGain,
    FHighFreqGain: single;
    FFilterID: ALuint;
    FFilterType: ALint;
    FOnApplyFilterOnObject,
    FOnRemoveFilterFromObject: TALSFilterCallBack;
    procedure InitDefault;
    procedure CreateFilter;
    procedure DeleteFilter;
    procedure ApplyGain;
    procedure SetGlobalGain(AValue: single);
    procedure SetLowFreqGain(AValue: single);
    procedure SetBandPassGain(AValue: single);
    procedure SetHighFreqGain(AValue: single);
  public
    // This gain affects all bands of the filter (2 or 3)
    property GlobalGain: single read FGlobalGain write SetGlobalGain;
    // Gain for Low frequency
    property LowFreqGain: single read FLowFreqGain write SetLowFreqGain;
    // Gain for middle frequency
    property BandPassGain: single read FBandPassGain write SetBandPassGain;
    // Gain for high frequency
    property HighFreqGain: single read FHighFreqGain write SetHighFreqGain;
  end;


  { TALSDirectFilter }

  TALSDirectFilter = object(TALSFilterBase)
  private
    procedure ApplyFilterOnObject;
    procedure RemoveFilterFromObject;
  public
    procedure Init(aParent: TALSSound);
  end;


  { TALSAuxiliarySend }

  TALSAuxiliarySend = object(TALSFilterBase)
  private
    FIndex,     // index of the auxiliary send. 0 based.
    FTargetEffectSlotID: ALuint;
    procedure ApplyFilterOnObject;
    procedure RemoveFilterFromObject;
  private
    FFirstChainedEffect: PALSEffect; // the adress of the first effect of a chain
  public
    procedure Init(aParent: TALSSound; aIndex: ALuint);
    procedure ConnectTo(aSlotID: ALuint);
    procedure Disconnect;
    function IsConnectedWith(aSlotID: ALuint): boolean;
    function IsConnected: boolean;

    function IsConnectedWithChain(aFirstEffect: PALSEffect): boolean;
    function IsConnectedWithAChain: boolean;
   // procedure ConnectToChain(aFirstEffect: PALSEffect);
  end;

{$endif}

{$ifdef ALS_IMPLEMENTATION}
{ TALSFilterBase }

procedure TALSFilterBase.InitDefault;
begin
  FParentSound := nil;
  FGlobalGain := 1.0;
  FLowFreqGain := 1.0;
  FBandPassGain := 1.0;
  FHighFreqGain := 1.0;
  FFilterID := AL_FILTER_NULL;
end;

procedure TALSFilterBase.CreateFilter;
begin
  // checks if extension is present
  if not FParentSound.FParentContext.HaveFilter then
  begin
    FFilterID := AL_FILTER_NULL;
    FFilterType := AL_FILTER_NULL;
    exit;
  end;

  if FFilterID = AL_FILTER_NULL then
  begin
    if FParentSound.FParentContext.FHaveBandPassFilter then
      FFilterType := AL_FILTER_BANDPASS
    else if FParentSound.FParentContext.FHaveLowPassFilter then
      FFilterType := AL_FILTER_LOWPASS
    else FFilterType := AL_FILTER_HIGHPASS;

    alGenFilters(1, @FFilterID);
    if alGetError() <> AL_NO_ERROR then
    begin
      FFilterID := AL_FILTER_NULL;
      FFilterType := AL_FILTER_NULL;
    end
    else
      alFilteri(FFilterID, AL_FILTER_TYPE, FFilterType);
      if alGetError() <> AL_NO_ERROR then
      begin
        alDeleteFilters(1, @FFilterID);
        alGetError();
        FFilterID := AL_FILTER_NULL;
        FFilterType := AL_FILTER_NULL;
      end;
  end;
end;

procedure TALSFilterBase.DeleteFilter;
begin
  // checks if extension is present
  if FParentSound.Error or
     not FParentSound.FParentContext.HaveFilter or
     (FFilterID = AL_FILTER_NULL) then
    exit;

  alDeleteFilters(1, @FFilterID);
  FFilterID := AL_FILTER_NULL;
  FFilterType := AL_FILTER_NULL;

  FOnRemoveFilterFromObject;
end;

procedure TALSFilterBase.ApplyGain;
begin
  // checks if extension is present
  if not FParentSound.FParentContext.HaveFilter then
    exit;

  CreateFilter;

  if FFilterID <> AL_FILTER_NULL then
  begin
    case FFilterType of
      AL_FILTER_LOWPASS: begin
        alFilterf(FFilterID, AL_LOWPASS_GAIN, FGlobalGain{*Sqrt(FLowFreqGain)});
        alFilterf(FFilterID, AL_LOWPASS_GAINHF, FGlobalGain*Sqrt(FHighFreqGain));
      end;

      AL_FILTER_BANDPASS: begin
        alFilterf(FFilterID, AL_BANDPASS_GAINLF, FGlobalGain*Sqrt(FLowFreqGain));
        alFilterf(FFilterID, AL_BANDPASS_GAIN, FGlobalGain{*Sqrt(FBandPassGain)});
        alFilterf(FFilterID, AL_BANDPASS_GAINHF, FGlobalGain*Sqrt(FHighFreqGain));
      end;

      AL_FILTER_HIGHPASS: begin
        alFilterf(FFilterID, AL_HIGHPASS_GAINLF, FGlobalGain*Sqrt(FLowFreqGain));
        alFilterf(FFilterID, AL_HIGHPASS_GAIN, FGlobalGain{*Sqrt(FHighFreqGain)});
      end;
    end;

    FOnApplyFilterOnObject;
  end;
end;

procedure TALSFilterBase.SetGlobalGain(AValue: single);
begin
  if FGlobalGain = AValue then Exit;
  FGlobalGain := AValue;
  ApplyGain;
end;

procedure TALSFilterBase.SetLowFreqGain(AValue: single);
begin
  if FLowFreqGain = AValue then Exit;
  FLowFreqGain := EnsureRange(AValue, 0.01, 1.0);
  ApplyGain;
end;

procedure TALSFilterBase.SetBandPassGain(AValue: single);
begin
  if FBandPassGain = AValue then Exit;
  FBandPassGain := EnsureRange(AValue, 0.01, 1.0);
  ApplyGain;
end;

procedure TALSFilterBase.SetHighFreqGain(AValue: single);
begin
  if FHighFreqGain = AValue then Exit;
  FHighFreqGain := EnsureRange(AValue, 0.01, 1.0);
  ApplyGain;
end;

{ TALSDirectFilter }

procedure TALSDirectFilter.ApplyFilterOnObject;
begin
  alSourcei(FParentSound.FSource, AL_DIRECT_FILTER, FFilterID);
end;

procedure TALSDirectFilter.RemoveFilterFromObject;
begin
  alSourcei(FParentSound.FSource, AL_DIRECT_FILTER, AL_FILTER_NULL);
end;

procedure TALSDirectFilter.Init(aParent: TALSSound);
begin
  InitDefault;
  FParentSound := aParent;
  FOnApplyFilterOnObject := @ApplyFilterOnObject;
  FOnRemoveFilterFromObject:= @RemoveFilterFromObject;
end;

{ TALSAuxiliarySend }

procedure TALSAuxiliarySend.ApplyFilterOnObject;
begin
  alSource3i(FParentSound.FSource, AL_AUXILIARY_SEND_FILTER, FTargetEffectSlotID, FIndex, FFilterID);
  alGetError();
end;

procedure TALSAuxiliarySend.RemoveFilterFromObject;
begin
  alSource3i(FParentSound.FSource, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, FIndex, AL_FILTER_NULL);
  alGetError();
  FTargetEffectSlotID := AL_EFFECTSLOT_NULL;
end;

procedure TALSAuxiliarySend.Init(aParent: TALSSound; aIndex: ALuint);
begin
  InitDefault;
  FParentSound := aParent;
  FIndex:= aIndex;
  FTargetEffectSlotID := AL_EFFECTSLOT_NULL;
  FOnApplyFilterOnObject := @ApplyFilterOnObject;
  FOnRemoveFilterFromObject:= @RemoveFilterFromObject;
end;

procedure TALSAuxiliarySend.ConnectTo(aSlotID: ALuint);
begin
  FTargetEffectSlotID := aSlotID;
  CreateFilter;
  ApplyFilterOnObject;
end;

procedure TALSAuxiliarySend.Disconnect;
begin
  DeleteFilter;
end;

function TALSAuxiliarySend.IsConnectedWith(aSlotID: ALuint): boolean;
begin
  Result := FTargetEffectSlotID = aSlotID;
end;

function TALSAuxiliarySend.IsConnected: boolean;
begin
  Result := FTargetEffectSlotID <> AL_EFFECTSLOT_NULL;
end;

function TALSAuxiliarySend.IsConnectedWithChain(aFirstEffect: PALSEffect): boolean;
begin
  Result := FFirstChainedEffect = aFirstEffect;
end;

function TALSAuxiliarySend.IsConnectedWithAChain: boolean;
begin
  Result := FFirstChainedEffect <> NIL;
end;

{procedure TALSAuxiliarySend.ConnectToChain(aFirstEffect: PALSEffect);
var
  p: PALSEffect;
begin
  // search the first active effect in the chain
  p := aFirstEffect;
  while p <> NIL do
    if p^.FMute or not p^.Ready then
      p := p^.FNext;

  if p <> NIL then
  begin // we found one
    FTargetEffectSlotID := p^.FSlotID;
    CreateFilter;
    ApplyFilterOnObject;
  end
  else
  begin // all effects in the chain are muted or not ready
    FTargetEffectSlotID := AL_EFFECTSLOT_NULL;
    CreateFilter;
    ApplyFilterOnObject;
  end;
end;    }


{$endif}

