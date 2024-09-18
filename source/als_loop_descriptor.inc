{$ifdef ALS_INTERFACE}
type

  { TALSLoopDescriptor }

  TALSLoopDescriptor = record
    // The time position in ms where to jump when the sound is looped. Default is 0 (beginning).
    LoopBeginFrame: sf_count_t;

    // When sound reach this time position in ms, (and Loop is True),
    // the read process jump to LoopBeginMS position.
    LoopEndFrame: sf_count_t;

    // True if the sound is looped. Default is False.
    Loop: boolean;
    procedure InitDefault;
  end;

{$endif}

{$ifdef ALS_IMPLEMENTATION}

{ TALSLoopDescriptor }

procedure TALSLoopDescriptor.InitDefault;
begin
  FillChar(Self, SizeOf(TALSLoopDescriptor), 0);
end;

{$endif}

