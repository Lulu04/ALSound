unit IT.AudioDriver.SB16;

{$MODE OBJFPC}
{$H+}
{$R-}
{$COPERATORS ON}
{$MACRO ON}

interface

uses
	Classes, SysUtils, Math,
	IT2play;

type
	{$IFDEF CPU64}
	CPUWord = UInt64;
	{$ELSE}
	CPUWord = Cardinal;
	{$ENDIF}

	TMixFunction = procedure(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal) of object;

	TITAudioDriver_SB16 = class(TITAudioDriver)
	private
		MixFunctions: array [Boolean, Boolean, Boolean] of TMixFunction;
		MixFunc: TMixFunction;

		procedure M32Mix8   (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix16  (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix8S  (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix16S (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix8I  (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix16I (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix8IS (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Mix16IS(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
	public
		procedure MixSamples; override;
		function  PostMix(AudioOut16: PInt16; SamplesToOutput: Integer): Integer; override;
		procedure Mix(NumSamples: Integer; AudioOut: PInt16); override;
		procedure FixSamples; override;

		constructor Create(AModule: TITModule; MixingFrequency: Integer); override;
	end;

implementation

// Common macros for mixing functions

{$DEFINE GetSamplePtrs :=
	base := sc.Sample.Data[False].Data;
	smp := base + sc.SamplingPosition; }

// (sc.Delta32 * numSamples) / 2^16
// = how much the sample pointer will advance per mixfunc call
{$DEFINE UpdatePos :=
	sc.Frac32 += Delta32;
	smp += SarLongInt(Int32(sc.Frac32), MIX_FRAC_BITS);
	sc.Frac32 := sc.Frac32 and MIX_FRAC_MASK; }

{$DEFINE MixLoop :=
	for i := 1 to NumSamples do begin
		DoMix;
		UpdatePos;
	end; }

{$DEFINE MixIt :=
	GetSamplePtrs;
	MixLoop;
	sc.SamplingPosition := Int32(smp - base); }

procedure TITAudioDriver_SB16.M32Mix8(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample: Int32;
	base, smp: PInt8;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample := smp^ * 256;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ -= sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix16(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample: Int32;
	base, smp: PInt16;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample := smp^;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ -= sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix8S(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample: Int32;
	base, smp: PInt8;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample := smp^ * 256;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ += sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix16S(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample: Int32;
	base, smp: PInt16;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample := smp^;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ += sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix8I(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2: Int32;
	base, smp: PInt8;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample2 := (smp[1] - smp[0]) * Int32(sc.Frac32);
		sample2 := SarLongint(sample2, MIX_FRAC_BITS-8);
		sample  := (smp[0] * 256) + sample2;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ -= sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix16I(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2: Int32;
	base, smp: PInt16;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample  := smp[0];
		sample2 := (smp[1] - smp[0]) div 2 * Int32(sc.Frac32);
		sample2 := SarLongint(sample2, MIX_FRAC_BITS-1);
		sample  += sample2;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ -= sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix8IS(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2: Int32;
	base, smp: PInt8;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample  := smp[0];
		sample2 := SarLongint(SarLongint(smp[1] - sample) * sc.Frac32, MIX_FRAC_BITS-8);
		sample  := (sample * 256) + sample2;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ += sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.M32Mix16IS(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2: Int32;
	base, smp: PInt16;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample  := smp[0];
		sample2 := SarLongint(SarLongint(smp[1] - smp[0]) * sc.Frac32, MIX_FRAC_BITS-1);
		sample  += sample2;
		MixBufPtr^ -= sample * sc.Volume[False]; Inc(MixBufPtr);
		MixBufPtr^ += sample * sc.Volume[True];  Inc(MixBufPtr);
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16.MixSamples;
var
	SamplesToMix: Cardinal;

	procedure Fix32; inline;
	begin
		{.$IFDEF CPU32}
		// 8bb: limit it so we can do a hardware 32-bit div (instead of slow software 64-bit div)
		if SamplesToMix > $FFFF then SamplesToMix := $FFFF;
		{.$ENDIF}
	end;

var
	i: Integer;
	MixBlockSize: Integer;
	NewLoopPos: Integer;
	LoopLength: Integer;
	MixBufferPtr: PInt32;
	Quotient, Remainder, Foo: Cardinal;
	sc: TSlaveChannel;

	procedure GetSamplesToMix; inline;
	begin
		// ((((uintCPUWord_t)SamplesToMix << MIX_FRAC_BITS) | ((uint16_t)sc->Frac32 ^ MIX_FRAC_MASK)) / sc->Delta32) + 1;

		// SamplesToMix and sc.Frac32 = Uint32, MIX_FRAC_ constants verified correct
		SamplesToMix := ((SamplesToMix << MIX_FRAC_BITS) or
			((sc.Frac32 and $FFFF) xor MIX_FRAC_MASK)) div sc.Delta32 + 1;
	end;

	procedure DoMix; inline;
	begin
		if SamplesToMix > MixBlockSize then
			SamplesToMix := MixBlockSize;

		MixFunc(sc, MixBufferPtr, SamplesToMix);

		MixBufferPtr += SamplesToMix * 2;
		MixBlockSize -= SamplesToMix;
	end;

	procedure ClearFlags;
	begin
		sc.Flags.SF_RECALC_PAN      := False;
		sc.Flags.SF_RECALC_VOL      := False;
		sc.Flags.SF_FREQ_CHANGE     := False;
		sc.Flags.SF_UPDATE_MIXERVOL := False;
		sc.Flags.SF_NEW_NOTE        := False;
		sc.Flags.SF_NOTE_STOP       := False;
		sc.Flags.SF_LOOP_CHANGED    := False;
		sc.Flags.SF_PAN_CHANGED     := False;
	end;

begin
	MixTransferOffset := 0;
	FillDWord(MixBuffer[0], BytesToMix*2, 0);

	for i := 0 to NumChannels-1 do
	begin
		sc := Module.SlaveChannels[i];

		if (not sc.Flags.SF_CHAN_ON) or (sc.Smp = 100) then
			Continue;

		if sc.Flags.SF_NOTE_STOP then
		begin
			sc.Flags.SF_CHAN_ON := False;
			Continue;
		end;

		if sc.Flags.SF_FREQ_CHANGE then
		begin
			{
			if ((uint32_t)sc->Frequency>>MIX_FRAC_BITS >= Driver.MixSpeed ||
				(uint32_t)sc->Frequency >= INT32_MAX/2)
			}
			//if ((sc.Frequency div $FFFF) >= MixSpeed) or
			Foo := Cardinal(sc.Frequency);
			if ((Foo >> MIX_FRAC_BITS) >= MixSpeed) or
				(Foo >= MaxInt div 2) then // 8bb: non-IT2 limit, but required for safety
			begin
				sc.Flags.WordAccess := 0;
				sc.Flags.SF_NOTE_STOP := True;
				if (sc.HostChnNum and CHN_DISOWNED = 0) then
					sc.HostChannel.Flags.HF_CHAN_ON := False; // Turn off channel
				Continue;
			end;

			// 8bb: calculate mixer delta
			{
			uint32_t Quotient  = (uint32_t)sc->Frequency / Driver.MixSpeed;
			uint32_t Remainder = (uint32_t)sc->Frequency % Driver.MixSpeed;
			sc->Delta32 = (Quotient << MIX_FRAC_BITS) | (uint16_t)((Remainder << MIX_FRAC_BITS) / Driver.MixSpeed);
			}
			Quotient  := Foo div MixSpeed;
			Remainder := Foo mod MixSpeed;

			sc.Delta32 := (Quotient << MIX_FRAC_BITS) or (((Remainder << MIX_FRAC_BITS) div MixSpeed) and $FFFF);
		end;

		if (sc.Flags.SF_UPDATE_MIXERVOL) or (sc.Flags.SF_LOOP_CHANGED) or (sc.Flags.SF_PAN_CHANGED) then
		begin
			if not sc.Flags.SF_CHN_MUTED then
			begin
				if not Module.Header.Flags.ITF_STEREO then // 8bb: mono?
				begin
					sc.Volume[False] := sc.FinalVol32768 * MixVolume div 256; // 8bb: 0..16384
					sc.Volume[True]  := sc.Volume[False];
				end
				else
				if sc.FinalPan = PAN_SURROUND then
				begin
					sc.Volume[False] := sc.FinalVol32768 * MixVolume div 512; // 8bb: 0..8192
					sc.Volume[True]  := sc.Volume[False];
				end
				else // 8bb: normal (panned)
				begin
					sc.Volume[False] := SarLongInt((64-sc.FinalPan) * MixVolume * sc.FinalVol32768, 14); // 8bb: 0..16384
					sc.Volume[True]  := SarLongInt(    sc.FinalPan  * MixVolume * sc.FinalVol32768, 14);
				end;
			end;
		end;

		if sc.Delta32 = 0 then // 8bb: added this protection just in case (shouldn't happen)
			Continue;

		MixBlockSize := BytesToMix;
		LoopLength := sc.LoopEnd - sc.LoopBegin; // 8bb: also length for non-loopers

		if (sc.Flags.SF_CHN_MUTED) or ((sc.Volume[False] = 0) and (sc.Volume[True] = 0)) then
		begin
			if LoopLength > 0 then
			begin
				if sc.LoopMode = LOOP_PINGPONG then
					UpdatePingPongLoop(sc, MixBlockSize)
				else
				if sc.LoopMode = LOOP_FORWARDS then
					UpdateForwardsLoop(sc, MixBlockSize)
				else
					UpdateNoLoop(sc, MixBlockSize);
			end;

			ClearFlags;
			Continue;
		end;

		MixFunc := MixFunctions[sc.SmpIs16Bit, MixMode = 1, sc.FinalPan = PAN_SURROUND];
		MixBufferPtr := @MixBuffer[0];

		if LoopLength > 0 then
		begin
			if sc.LoopMode = LOOP_PINGPONG then
			begin
				// pingpong loop
				while MixBlockSize > 0 do
				begin
					if sc.LoopDirection = DIR_BACKWARDS then
					begin
						if sc.SamplingPosition <= sc.LoopBegin then
						begin
							NewLoopPos := Max(0, (sc.LoopBegin - sc.SamplingPosition) mod (LoopLength * 2));
							if NewLoopPos >= LoopLength then
								sc.SamplingPosition := (sc.LoopEnd - 1) - (NewLoopPos - LoopLength)
							else
							begin
								sc.LoopDirection := DIR_FORWARDS;
								sc.SamplingPosition := sc.LoopBegin + NewLoopPos;
								sc.Frac32 := (-sc.Frac32) and $FFFF;
							end;
						end;
					end
					else // pingpong: forwards
					begin
						if sc.SamplingPosition >= sc.LoopEnd then
						begin
							NewLoopPos := Max(0, (sc.SamplingPosition - sc.LoopEnd) mod (LoopLength * 2));
							if NewLoopPos >= LoopLength then
								sc.SamplingPosition := sc.LoopBegin + (NewLoopPos - LoopLength)
							else
							begin
								sc.LoopDirection := DIR_BACKWARDS;
								sc.SamplingPosition := (sc.LoopEnd - 1) - NewLoopPos;
								sc.Frac32 := (-sc.Frac32) and $FFFF;
							end;
						end;
					end;

					if sc.LoopDirection = DIR_BACKWARDS then
					begin
						if sc.SamplingPosition = sc.LoopBegin then
						begin
							sc.LoopDirection := DIR_FORWARDS;
							sc.Frac32 := Word(0 - sc.Frac32);

							SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
							Fix32;
							GetSamplesToMix;
							Delta32 := sc.Delta32;
						end
						else
						begin
							SamplesToMix := sc.SamplingPosition - (sc.LoopBegin + 1);
							Fix32;
							Delta32 := sc.Delta32;
							Delta32 := -Delta32;
							SamplesToMix := ((SamplesToMix << MIX_FRAC_BITS) or
								(sc.Frac32 and $FFFF)) div sc.Delta32 + 1;
						end;
					end
					else // pingpong: forwards
					begin
						SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
						Fix32;
						GetSamplesToMix;
						Delta32 := sc.Delta32;
					end;

					DoMix;
				end;
			end // pingpong
			else
			if sc.LoopMode = LOOP_FORWARDS then
			begin
				// forwards loop
				while MixBlockSize > 0 do
				begin
					if sc.SamplingPosition >= sc.LoopEnd then
						sc.SamplingPosition := sc.LoopBegin + ((sc.SamplingPosition - sc.LoopEnd) mod LoopLength);
					SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
					Fix32;
					GetSamplesToMix;
					Delta32 := sc.Delta32;
					DoMix;
				end;
			end // forwards
			else
			begin
				// no loop
				while MixBlockSize > 0 do
				begin
					if sc.SamplingPosition >= sc.LoopEnd then // 8bb: LoopEnd := sample end, even for non-loopers
					begin
						sc.Flags.WordAccess := 0;
						sc.Flags.SF_NOTE_STOP := True;
						if (sc.HostChnNum and CHN_DISOWNED) = 0 then
							sc.HostChannel.Flags.HF_CHAN_ON := False; // Signify channel off
						Break;
					end;
					SamplesToMix := sc.LoopEnd - 1 - sc.SamplingPosition;
					Fix32;
					GetSamplesToMix;
					Delta32 := sc.Delta32;

					DoMix;
				end;
			end; // no loop
		end;

		ClearFlags;
	end;
end;

function TITAudioDriver_SB16.PostMix(AudioOut16: PInt16; SamplesToOutput: Integer): Integer;
var
	SampleShiftValue: Byte;
	i, SamplesLeft: Integer;
	Sample: Int32;
begin
	SampleShiftValue := 14 - BoolToInt[Module.Header.Flags.ITF_STEREO];
	SamplesLeft := IfThen(SamplesToOutput > 0, SamplesToOutput, BytesToMix);

	for i := 0 to SamplesLeft*2 - 1 do
	begin
		Sample := SarLongint(MixBuffer[MixTransferOffset], SampleShiftValue);
		Inc(MixTransferOffset);

		if Sample < -32768 then
			Sample := -32768
		else
		if Sample > +32767 then
			Sample := +32767;

		AudioOut16^ := Sample;
		Inc(AudioOut16);
	end;

	Result := SamplesLeft;
end;

procedure TITAudioDriver_SB16.Mix(NumSamples: Integer; AudioOut: PInt16);
var
	SamplesToTransfer: Integer;
begin
	MixVolume := Module.Header.MixVolume;

	while NumSamples > 0 do
	begin
		if MixTransferRemaining = 0 then
		begin
			Module.Update;
			MixSamples;
			MixTransferRemaining := BytesToMix;
		end;

		SamplesToTransfer := NumSamples;
		if SamplesToTransfer > MixTransferRemaining then
			SamplesToTransfer := MixTransferRemaining;

		PostMix(AudioOut, SamplesToTransfer);
		AudioOut += SamplesToTransfer * 2;

		MixTransferRemaining -= SamplesToTransfer;
		NumSamples -= SamplesToTransfer;
	end;
end;

// Fixes sample end bytes for interpolation (yes, we have room after the data).
// Sustain loops are always handled as non-looping during fix in IT2.
//
procedure TITAudioDriver_SB16.FixSamples;
var
	i: Integer;
	Sample: TSample;
	data8, smp8Ptr: PInt8;
	src: Int32;
	byte1, byte2: Int8;
	Sample16Bit, HasLoop: Boolean;
begin
	for i := 0 to Module.Header.SmpNum-1 do
	begin
		Sample := Module.Samples[i];
		if (Sample = nil) or (Sample.Length = 0) or (Sample.Data[False].Data = nil) then
			Continue;

		Sample16Bit := Sample.Flags.SMPF_16BIT;
		HasLoop     := Sample.Flags.SMPF_USE_LOOP;

		data8 := Sample.Data[False].Data;
		smp8Ptr := @data8[Sample.Length << BoolToInt[Sample16Bit]];

		// 8bb: added this protection for looped samples
		if (HasLoop) and ((Sample.LoopEnd - Sample.LoopBegin) < 2) then
		begin
			smp8Ptr^ := 0; Inc(smp8Ptr);
			smp8Ptr^ := 0; Inc(smp8Ptr);
			Exit;
		end;

		byte1 := 0; byte2 := 0;

		if HasLoop then
		begin
			if Sample.Flags.SMPF_LOOP_PINGPONG then
				src := Max(0, Sample.LoopEnd - 2)
			else
				src := Sample.LoopBegin; // 8bb: forward loop

			if Sample16Bit then
				src *= 2;

			byte1 := data8[src+0];
			byte2 := data8[src+1];
		end;

		smp8Ptr^ := byte1; Inc(smp8Ptr);
		smp8Ptr^ := byte2; Inc(smp8Ptr);
	end;
end;

constructor TITAudioDriver_SB16.Create(AModule: TITModule; MixingFrequency: Integer);
begin
	inherited;

	// MixFunctions[SmpIs16Bit, MixMode=1, PAN_SURROUND];
	//
	MixFunctions[False, False, False] := @M32Mix8;
	MixFunctions[False, False, True ] := @M32Mix8S;
	MixFunctions[False, True,  False] := @M32Mix8I;
	MixFunctions[False, True,  True ] := @M32Mix8IS;
	MixFunctions[True,  False, False] := @M32Mix16;
	MixFunctions[True,  False, True ] := @M32Mix16S;
	MixFunctions[True,  True,  False] := @M32Mix16I;
	MixFunctions[True,  True,  True ] := @M32Mix16IS;

	Flags.DF_SUPPORTS_MIDI := True;

	// MixMode 0 = "32 Bit Non-interpolated"
	// MixMode 1 = "32 Bit Interpolated"
	MixMode := 1;
end;


end.

