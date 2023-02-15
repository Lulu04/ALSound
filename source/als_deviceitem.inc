{$ifdef ALS_INTERFACE}
type
  { TALSDeviceItem }

  TALSDeviceItem = object
  private
    alcGetInteger64vSOFT: TProc_alcGetInteger64vSOFT;

    alcSetThreadContext: TProc_alcSetThreadContext;
    alcGetThreadContext: TProc_alcGetThreadContext;

    alcDevicePauseSOFT: TProc_alcDevicePauseSOFT;
    alcDeviceResumeSOFT: TProc_alcDeviceResumeSOFT;

    alcGetStringiSOFT: TProc_alcGetStringiSOFT;
    alcResetDeviceSOFT: TProc_alcResetDeviceSOFT;

    alcReopenDeviceSOFT: TProc_alcReopenDeviceSOFT;
    procedure LoadExtension;
  private
    alcLoopbackOpenDeviceSOFT: TProc_alcLoopbackOpenDeviceSOFT;
    alcIsRenderFormatSupportedSOFT: TProc_alcIsRenderFormatSupportedSOFT;
    alcRenderSamplesSOFT: TProc_alcRenderSamplesSOFT;
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

      if alcLoopbackOpenDeviceSOFT <> NIL then begin
        if Name = '' then
          Handle := alcLoopbackOpenDeviceSOFT(nil)
        else
          Handle := alcLoopbackOpenDeviceSOFT(PChar(Name));
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
    alcSetThreadContext := NIL;
    alcGetThreadContext := NIL;
    alcDevicePauseSOFT := NIL;
    alcDeviceResumeSOFT := NIL;
    alcGetStringiSOFT := NIL;
    alcResetDeviceSOFT := NIL;
    alcGetInteger64vSOFT := NIL;
    alcReopenDeviceSOFT := NIL;
  end else begin
    FHaveEXT_ALC_EXT_EFX := alcIsExtensionPresent(Handle, PChar('ALC_EXT_EFX'));
    if FHaveEXT_ALC_EXT_EFX then
      FHaveEXT_ALC_EXT_EFX := LoadExt_ALC_EXT_EFX;

    FHaveExt_ALC_SOFT_output_mode := alcIsExtensionPresent(Handle, PChar('ALC_SOFT_output_mode'));

    FHaveExt_ALC_EXT_thread_local_context := LoadExt_ALC_EXT_thread_local_context(Handle);
    alcSetThreadContext := openalsoft._alcSetThreadContext;
    alcGetThreadContext := openalsoft._alcGetThreadContext;

    LoadLoopbackExtension;

    FHaveExt_ALC_SOFT_output_limiter := alcIsExtensionPresent(Handle, PChar('ALC_SOFT_output_limiter'));

    FHaveExt_ALC_SOFT_pause_device := LoadExt_ALC_SOFT_pause_device(Handle);
    alcDevicePauseSOFT := openalsoft._alcDevicePauseSOFT;
    alcDeviceResumeSOFT := openalsoft._alcDeviceResumeSOFT;

    FHaveExt_ALC_SOFT_HRTF := LoadExt_ALC_SOFT_HRTF(Handle);
    alcGetStringiSOFT := openalsoft._alcGetStringiSOFT;
    alcResetDeviceSOFT := openalsoft._alcResetDeviceSOFT;

    FHaveExt_ALC_SOFT_device_clock := LoadExt_ALC_SOFT_device_clock(Handle);
    alcGetInteger64vSOFT := openalsoft._alcGetInteger64vSOFT;

    FHaveExt_ALC_SOFT_reopen_device := LoadExt_ALC_SOFT_reopen_device(Handle);
    alcReopenDeviceSOFT := openalsoft._alcReopenDeviceSOFT;
  end;
end;

procedure TALSDeviceItem.LoadLoopbackExtension;
begin
  FHaveExt_ALC_SOFT_loopback := LoadExt_ALC_SOFT_loopback(NIL);
  alcLoopbackOpenDeviceSOFT := openalsoft._alcLoopbackOpenDeviceSOFT;
  alcIsRenderFormatSupportedSOFT := openalsoft._alcIsRenderFormatSupportedSOFT;
  alcRenderSamplesSOFT := openalsoft._alcRenderSamplesSOFT;
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

