{$ifdef ALS_INTERFACE}
type
  { TALSDeviceItem }

  TALSDeviceItem = object
  private
    FalcGetInteger64vSOFT: TProc_alcGetInteger64vSOFT;

    FalcSetThreadContext: TProc_alcSetThreadContext;
    FalcGetThreadContext: TProc_alcGetThreadContext;

    FalcDevicePauseSOFT: TProc_alcDevicePauseSOFT;
    FalcDeviceResumeSOFT: TProc_alcDeviceResumeSOFT;

    FalcGetStringiSOFT: TProc_alcGetStringiSOFT;
    FalcResetDeviceSOFT: TProc_alcResetDeviceSOFT;

    FalcReopenDeviceSOFT: TProc_alcReopenDeviceSOFT;
    procedure LoadExtension;
  private
    FalcLoopbackOpenDeviceSOFT: TProc_alcLoopbackOpenDeviceSOFT;
    FalcIsRenderFormatSupportedSOFT: TProc_alcIsRenderFormatSupportedSOFT;
    FalcRenderSamplesSOFT: TProc_alcRenderSamplesSOFT;
    procedure LoadLoopbackExtension;
    procedure FirstInitdefault;
  public
    Name: string;
    Handle: PALCDevice;
    FHaveEXT_ALC_EXT_EFX,
    FHaveExt_ALC_SOFT_output_mode,
    FHaveExt_ALC_SOFT_HRTF,
    FHaveExt_ALC_SOFT_loopback,
    FHaveExt_ALC_SOFT_output_limiter,
    FHaveExt_ALC_EXT_thread_local_context,
    FHaveExt_ALC_SOFT_reopen_device,
    FHaveExt_ALC_SOFT_device_clock,
    FHaveExt_ALC_SOFT_pause_device: boolean;
  end;
  PALSDeviceItem = ^TALSDeviceItem;

  { TALSPlaybackDeviceItem }

  TALSPlaybackDeviceItem = object(TALSDeviceItem)
  public
    OpenedCount: integer;
    procedure InitDefault;
    procedure Open;
    procedure Close;
    procedure DoCloseDevice;
  end;
  PALSPlaybackDeviceItem = ^TALSPlaybackDeviceItem;
  ArrayOfALSPlaybackDeviceItem = array of TALSPlaybackDeviceItem;

  { TALSLoopbackDeviceItem }

  TALSLoopbackDeviceItem = object(TALSDeviceItem)
    OpenedCount: integer;
    procedure InitDefault;
    procedure Open;
    procedure Close;
    procedure DoCloseDevice;
  end;
  PALSLoopbackDeviceItem = ^TALSLoopbackDeviceItem;

{$endif}

{$ifdef ALS_IMPLEMENTATION}

{ TALSLoopbackDeviceItem }

procedure TALSLoopbackDeviceItem.InitDefault;
begin
  FirstInitdefault;
  OpenedCount := 0;
end;

procedure TALSLoopbackDeviceItem.Open;
begin
  if not ALSManager.Error then
  begin
    if Handle = NIL then
    begin
      LoadLoopbackExtension;

      if FalcLoopbackOpenDeviceSOFT <> NIL then begin
        if Name = '' then
          Handle := FalcLoopbackOpenDeviceSOFT(nil)
        else
          Handle := FalcLoopbackOpenDeviceSOFT(PChar(Name));
      end;

      LoadExtension;
    end;

    if Handle <> NIL then
      inc( OpenedCount );
  end;
end;

procedure TALSLoopbackDeviceItem.Close;
begin
  if not ALSManager.Error then
  begin
    {$ifndef ALS_ENABLE_CONTEXT_SWITCHING}
      _SingleContextIsCurrent := False;
    {$endif}
    if OpenedCount = 0 then
      exit;
    dec( OpenedCount );
    if OpenedCount = 0 then
      DoCloseDevice;
  end;
end;

procedure TALSLoopbackDeviceItem.DoCloseDevice;
begin
  alcCloseDevice(Handle);
  Handle := NIL;
end;

{ TALSPlaybackDeviceItem }

procedure TALSPlaybackDeviceItem.InitDefault;
begin
  FirstInitdefault;
  OpenedCount := 0;
end;

procedure TALSPlaybackDeviceItem.Open;
begin
  if not ALSManager.Error then
  begin
    if Handle = NIL then begin
      Handle := alcOpenDevice(PChar(Name));
      LoadExtension;
      LoadLoopbackExtension;
    end;
    inc( OpenedCount );
  end;
end;

procedure TALSPlaybackDeviceItem.Close;
begin
  if not ALSManager.Error then
  begin
    {$ifndef ALS_ENABLE_CONTEXT_SWITCHING}
      _SingleContextIsCurrent := False;
    {$endif}
    if OpenedCount = 0 then
      exit;
    dec( OpenedCount );
    if OpenedCount = 0 then
      DoCloseDevice;
  end;
end;

procedure TALSPlaybackDeviceItem.DoCloseDevice;
begin
  alcCloseDevice( Handle );
  Handle := NIL;
end;

{ TALSDeviceItem }

procedure TALSDeviceItem.LoadExtension;
begin
  if Handle = NIL then begin
    FalcSetThreadContext := NIL;
    FalcGetThreadContext := NIL;
    FalcDevicePauseSOFT := NIL;
    FalcDeviceResumeSOFT := NIL;
    FalcGetStringiSOFT := NIL;
    FalcResetDeviceSOFT := NIL;
    FalcGetInteger64vSOFT := NIL;
    FalcReopenDeviceSOFT := NIL;
  end else begin
    FHaveEXT_ALC_EXT_EFX := alcIsExtensionPresent(Handle, PChar('ALC_EXT_EFX'));
    if FHaveEXT_ALC_EXT_EFX then
      FHaveEXT_ALC_EXT_EFX := LoadExt_ALC_EXT_EFX;

    FHaveExt_ALC_SOFT_output_mode := alcIsExtensionPresent(Handle, PChar('ALC_SOFT_output_mode'));

    FHaveExt_ALC_EXT_thread_local_context := LoadExt_ALC_EXT_thread_local_context(Handle);
    FalcSetThreadContext := openalsoft.alcSetThreadContext;
    FalcGetThreadContext := openalsoft.alcGetThreadContext;

    LoadLoopbackExtension;

    FHaveExt_ALC_SOFT_output_limiter := alcIsExtensionPresent(Handle, PChar('ALC_SOFT_output_limiter'));

    FHaveExt_ALC_SOFT_pause_device := LoadExt_ALC_SOFT_pause_device(Handle);
    FalcDevicePauseSOFT := openalsoft.alcDevicePauseSOFT;
    FalcDeviceResumeSOFT := openalsoft.alcDeviceResumeSOFT;

    FHaveExt_ALC_SOFT_HRTF := LoadExt_ALC_SOFT_HRTF(Handle);
    FalcGetStringiSOFT := openalsoft.alcGetStringiSOFT;
    FalcResetDeviceSOFT := openalsoft.alcResetDeviceSOFT;

    FHaveExt_ALC_SOFT_device_clock := LoadExt_ALC_SOFT_device_clock(Handle);
    FalcGetInteger64vSOFT := openalsoft.alcGetInteger64vSOFT;

    FHaveExt_ALC_SOFT_reopen_device := LoadExt_ALC_SOFT_reopen_device(Handle);
    FalcReopenDeviceSOFT := openalsoft.alcReopenDeviceSOFT;
  end;
end;

procedure TALSDeviceItem.LoadLoopbackExtension;
begin
  FHaveExt_ALC_SOFT_loopback := LoadExt_ALC_SOFT_loopback(NIL);
  FalcLoopbackOpenDeviceSOFT := openalsoft.alcLoopbackOpenDeviceSOFT;
  FalcIsRenderFormatSupportedSOFT := openalsoft.alcIsRenderFormatSupportedSOFT;
  FalcRenderSamplesSOFT := openalsoft.alcRenderSamplesSOFT;
end;

procedure TALSDeviceItem.FirstInitdefault;
begin
  Name := '';
  Handle := NIL;
  FHaveEXT_ALC_EXT_EFX := False;
  FHaveExt_ALC_SOFT_output_mode := False;
  FHaveExt_ALC_SOFT_HRTF := False;
  FHaveExt_ALC_SOFT_loopback := False;
  FHaveExt_ALC_SOFT_output_limiter := False;
  FHaveExt_ALC_EXT_thread_local_context := False;
  FHaveExt_ALC_SOFT_reopen_device := False;
  FHaveExt_ALC_SOFT_device_clock := False;
  FHaveExt_ALC_SOFT_pause_device := False;
end;

{$endif}

