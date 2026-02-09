unit IT.AudioDriver.SB16MMX;

{$MODE OBJFPC}
{$H+}
{$R-}
{$COPERATORS ON}
{$MACRO ON}

{$DEFINE ENABLE_OLDSAMPLES_BUG}

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

	TITAudioDriver_SB16MMX = class(TITAudioDriver)
	const
		RAMPSPEED      = 7;
		RAMPCOMPENSATE = 63;
		FILTER_BITS    = 14;
	private
		MixFunctions: array of TMixFunction;
		MixFunc: TMixFunction;

		procedure M32Bit8M  (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit16M (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit8MI (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit16MI(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit8MV (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit16MV(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit8MF (sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
		procedure M32Bit16MF(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
	public
		procedure MixSamples; override;
		function  PostMix(AudioOut16: PInt16; SamplesToOutput: Integer): Integer; override;
		procedure Mix(NumSamples: Integer; AudioOut: PInt16); override;
		procedure FixSamples; override;

		constructor Create(AModule: TITModule; MixingFrequency: Integer); override;
	end;


implementation


// Common macros for mixing functions

{$DEFINE RampCurrVolume1 :=
	sc.CurrVol[False] += SarLongint(sc.DestVol[False] - sc.CurrVol[False], RAMPSPEED);
	sc.CurrVol[True]  += SarLongint(sc.DestVol[True]  - sc.CurrVol[True],  RAMPSPEED); }

{$DEFINE RampCurrVolume2 :=
	sc.CurrVol[False] += SarLongint(sc.DestVol[False] - sc.CurrVol[False], RAMPSPEED-1);
	sc.CurrVol[True]  += SarLongint(sc.DestVol[True]  - sc.CurrVol[True],  RAMPSPEED-1); }

{$DEFINE FilterSample :=
	s64 :=  (sample           * sc.filtera) +
	        (sc.OldSamples[0] * sc.filterb) +
	        (sc.OldSamples[1] * sc.filterc);
	s64 := SarInt64(s64, FILTER_BITS);
	if s64 < -32768 then s64 := -32768 else
	if s64 > +32767 then s64 := +32767;
	sample := s64;
	sc.OldSamples[1] := sc.OldSamples[0];
	sc.OldSamples[0] := sample; }

{$DEFINE GetSamplePtrs :=
	base := sc.Sample.Data[False].Data;
	smp := base + sc.SamplingPosition; }

// (sc.Delta32 * numSamples) / 2^16
// = how much the sample pointer will advance per mixfunc call
{$DEFINE UpdatePos :=
	sc.Frac32 += Delta32;
	smp += SarLongInt(Int32(sc.Frac32), MIX_FRAC_BITS);
	sc.Frac32 := sc.Frac32 and MIX_FRAC_MASK; }

// unramped mixmodes

{$DEFINE MixLoop :=
	for i := 1 to NumSamples do
		DoMix; }

{$DEFINE MixIt :=
	GetSamplePtrs;
	MixLoop;
	sc.SamplingPosition := Int32(smp - base); }

{$DEFINE PutSamples :=
	MixBufPtr^ += sample * sc.Volume[False]; Inc(MixBufPtr);
	MixBufPtr^ += sample * sc.Volume[True];  Inc(MixBufPtr);
	UpdatePos; }

{$DEFINE PutSamplesRamped :=
	MixBufPtr^ += sample * sc.CurrVol[False]; Inc(MixBufPtr);
	MixBufPtr^ += sample * sc.CurrVol[True];  Inc(MixBufPtr);
	UpdatePos; }

// ramped mixmodes

{$DEFINE MixLoop1Ramped :=
	if Odd(NumSamples) then begin
		DoMix;
		RampCurrVolume1;
		Dec(NumSamples);
	end; }

{$DEFINE MixLoop2Ramped :=
	for i := 0 to NumSamples-1 do begin
		DoMix;
		if Odd(i) then RampCurrVolume2;
	end; }

{$DEFINE MixItRamped :=
	GetSamplePtrs;
	MixLoop1Ramped;
	MixLoop2Ramped;
	sc.SamplingPosition := Int32(smp - base); }

// MixMode 0 = "MMX, Non-Interpolated"

procedure TITAudioDriver_SB16MMX.M32Bit8M(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample: Int32;
	base, smp: PInt8;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample := smp^ * 256;
		PutSamples;
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16MMX.M32Bit16M(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample: Int32;
	base, smp: PInt16;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample := smp^;
		PutSamples;
	end;
begin
	MixIt;
end;

// MixMode 1 = "MMX, Interpolated"

procedure TITAudioDriver_SB16MMX.M32Bit8MI(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2: Int32;
	base, smp: PInt8;
	i: Integer;

	procedure DoMix; inline;
	begin
		sample2 := (smp[1] - smp[0]) * Int32(sc.Frac32);
		sample2 := SarLongint(sample2, MIX_FRAC_BITS-8);
		sample  := (smp[0] * 256) + sample2;
		PutSamples;
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_SB16MMX.M32Bit16MI(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
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
		PutSamples;
	end;
begin
	MixIt;
end;

// MixMode 2 = "MMX, Volume Ramped"

// 8bb: ramped mixers need to be done slightly differently for the ramping to be sample-accurate
procedure TITAudioDriver_SB16MMX.M32Bit8MV(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2, fract: Int32;
	base, smp: PInt8;
	i: Integer;

	// M32Bit8MV_M2
	procedure DoMix; inline;
	begin
		fract := sc.Frac32 >> 1;
		sample  := smp[0];
		sample2 := smp[1];
		sample  := (sample  * 256) or Byte(sample  and $FF);
		sample2 := (sample2 * 256) or Byte(sample2 and $FF);
		sample2 *= fract;
		fract   := fract xor (MIX_FRAC_MASK >> 1);
		sample  *= fract;
		sample  := SarLongint(sample + sample2, MIX_FRAC_BITS-1);

		PutSamplesRamped;
	end;

begin
	MixItRamped;
end;

procedure TITAudioDriver_SB16MMX.M32Bit16MV(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2, fract: Int32;
	base, smp: PInt16;
	i: Integer;

	// M32Bit16MV_M2
	procedure DoMix; inline;
	begin
		fract := sc.Frac32 >> 1;
		sample2 := smp[1] * fract;
		fract := fract xor (MIX_FRAC_MASK >> 1);
		sample := smp[0] * fract;
		sample := SarLongint(sample + sample2, MIX_FRAC_BITS-1);

		PutSamplesRamped;
	end;

begin
	MixItRamped;
end;

// MixMode 3 = "MMX, Filtered"

procedure TITAudioDriver_SB16MMX.M32Bit8MF(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2, fract: Int32;
	base, smp: PInt8;
	i: Integer;

	// M32Bit8MF_M2
	procedure DoMix; inline;
	var
		s64: Int64;
	begin
		fract   := sc.Frac32 >> 1;
		sample  := smp[0];
		sample2 := smp[1];
		sample  := (sample  * 256) or Byte(sample  and $FF);
		sample2 := (sample2 * 256) or Byte(sample2 and $FF);
		sample2 *= fract;
		fract   := fract xor (MIX_FRAC_MASK >> 1);
		sample  *= fract;
		sample  := SarLongint(sample + sample2, MIX_FRAC_BITS);

		FilterSample;

		PutSamplesRamped;
	end;

begin
	MixItRamped;
end;

procedure TITAudioDriver_SB16MMX.M32Bit16MF(sc: TSlaveChannel; MixBufPtr: PInt32; NumSamples: Cardinal);
var
	sample, sample2, fract: Int32;
	base, smp: PInt16;
	i: Integer;

	// M32Bit16MF_M2
	procedure DoMix; inline;
	var
		s64: Int64;
	begin
		fract := sc.Frac32 >> 1;
		sample2 := smp[1] * fract;
		fract := fract xor (MIX_FRAC_MASK >> 1);
		sample := smp[0] * fract;
		sample := SarLongint(sample + sample2, MIX_FRAC_BITS);

		FilterSample;

		PutSamplesRamped;
	end;

begin
	MixItRamped;
end;


procedure TITAudioDriver_SB16MMX.MixSamples;
var
	SamplesToMix: Cardinal;

	function NearbyInt(F: Float): Integer; inline;
	begin
		Result := Trunc(F);
		if Abs(Frac(F)) >= 0.5 then
			Result += Trunc(2 * Frac(F));
	end;

	procedure Fix32; inline;
	begin
		{$IFDEF CPU32}
		// 8bb: limit it so we can do a hardware 32-bit div (instead of slow software 64-bit div)
		if SamplesToMix > $FFFF then SamplesToMix := $FFFF;
		{$ENDIF}
	end;

var
	i: Integer;
	MixBlockSize, OldMixOffset: Cardinal;
	LoopLength, NewLoopPos: Cardinal;
	iLoopLength: Int32 absolute LoopLength;
	MixBufferPtr: PInt32;
	Quotient, Remainder: Cardinal;

	UseOldMixOffset: Boolean;
	OldSamplesBug: Int16;
	filterCutOff, filterQ: Byte;

	sc: TSlaveChannel;

	FilterFreqValue: Word;
	r, p, psub1, d, e, a, fa, fb, fc, dea: Float;

	Chan: Boolean;
	DestVol, OldVolume: array[Boolean] of Int32;


	procedure GetSamplesToMix; inline;
	begin
		// ((((uintCPUWord_t)SamplesToMix << MIX_FRAC_BITS) | ((uint16_t)sc->Frac32 ^ MIX_FRAC_MASK)) / sc->Delta32) + 1;
		SamplesToMix := ((SamplesToMix << MIX_FRAC_BITS) or
			((sc.Frac32 xor MIX_FRAC_MASK) and $FFFF)) div sc.Delta32 + 1;
	end;

	procedure DoMix; inline;
	begin
		if SamplesToMix > MixBlockSize then
			SamplesToMix := MixBlockSize;

		if SamplesToMix > 0 then
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

	procedure SetSamplingPosition(NewPos: Cardinal); inline;
	begin
		sc.SamplingPosition := Int32(NewPos);
	end;

begin
	MixTransferOffset := 0;
	FillByte(MixBuffer[0], BytesToMix * 2 * 4, 0);

	for i := 0 to NumChannels-1 do
	begin
		sc := Module.SlaveChannels[i];
		if (not sc.Flags.SF_CHAN_ON) or (sc.Smp = 100) then
			Continue;

		UseOldMixOffset := False;

		if sc.Flags.SF_NOTE_STOP then
		begin
			sc.Flags.SF_CHAN_ON := False;
			if MixMode < 2 then
				Continue; // 8bb: if no ramp enabled in driver, don't ramp ending voices
			sc.Volume[False] := 0;
			sc.Volume[True]  := 0;
		end
		else
		begin
			OldSamplesBug := 0;

			if sc.Flags.SF_FREQ_CHANGE then
			begin
				if (SarLongInt(sc.Frequency, MIX_FRAC_BITS) >= MixSpeed) or
					(sc.Frequency >= (MaxInt div 2)) then
				begin
					// 8bb: non-IT2 limit, but required for safety
					sc.Flags.WordAccess := 0;
					sc.Flags.SF_NOTE_STOP := True;
					if (sc.HostChnNum and CHN_DISOWNED = 0) then
						sc.HostChannel.Flags.HF_CHAN_ON := False; // Turn off channel
					Continue;
				end;

				// 8bb: calculate mixer delta (could be faster, but slow method needed for OldSamplesBug)
				//
				Quotient  := Cardinal(sc.Frequency) div MixSpeed;
				Remainder := Cardinal(sc.Frequency) mod MixSpeed;
				sc.Delta32 := Quotient << MIX_FRAC_BITS;

				Remainder := Remainder << MIX_FRAC_BITS;
				Quotient  := (Remainder div MixSpeed) and $FFFF;
				Remainder := Remainder mod MixSpeed;
				sc.Delta32 := sc.Delta32 or Quotient;

				OldSamplesBug := Remainder; // 8bb: fun
			end;

			if sc.Flags.SF_NEW_NOTE then
			begin
				// 8bb: reset filter state
				sc.filtera := 0;
				sc.filterb := 0;
				sc.filterc := 0;
				sc.OldSamples[0] := 0;

				// 8bb: This one was supposed to be cleared, but Jeffrey Lim accidentally used
				// the DX register instead of AX. That means that the content relies on what was
				// in DX at the time. Thankfully, whenever the SF_NEW_NOTE is set, SF_FREQ_CHANGE
				// is also set, hence we only need to simulate a DX value change from the mixer
				// delta calculation (see above).
				//
				// This bug is important to simulate, as it can actually change the shape of the waveform.
				//
				{$IFDEF ENABLE_OLDSAMPLES_BUG}
				sc.OldSamples[1] := OldSamplesBug;
				{$ELSE}
				sc.OldSamples[1] := 0;
				{$ENDIF}
				// -----------------------
				// Zero current volume for volume sliding
				sc.OldVolume[False] := 0;
				sc.OldVolume[True]  := 0;
			end;

			if (sc.Flags.SF_UPDATE_MIXERVOL) or (sc.Flags.SF_NEW_NOTE) or (sc.Flags.SF_LOOP_CHANGED) or (sc.Flags.SF_PAN_CHANGED) then
			begin
				if sc.Flags.SF_CHN_MUTED then
				begin
					sc.MixOffset := 4; // 8bb: use zero-vol mixer
					UseOldMixOffset := True;

					// 8bb: reset filter state
					sc.filtera := 0;
					sc.filterb := 0;
					sc.filterc := 0;
					sc.OldSamples[0] := 0;
					sc.OldSamples[1] := 0;

					for Chan in Boolean do
					begin
						sc.Volume[Chan]    := 0;
						sc.OldVolume[Chan] := 0;
					end;

					if (sc.HostChnNum and CHN_DISOWNED) = 0 then
					begin
						filterCutOff := FilterParameters[sc.HostChnNum];
						filterQ      := FilterParameters[sc.HostChnNum+64];

						sc.VolEnvState.CurNode := (filterCutOff * 256) + (sc.VolEnvState.CurNode and $00FF);
						sc.MIDIBank := (filterQ * 256) + (sc.MIDIBank and $00FF);
					end;
				end
				else
				begin
					if MixMode = 3 then // 8bb: filters enabled in driver?
					begin
						if (sc.HostChnNum and CHN_DISOWNED) <> 0 then
							filterQ := sc.MIDIBank >> 8 // Disowned? Then use channel filters.
						else
						begin
							filterCutOff := FilterParameters[sc.HostChnNum];
							filterQ      := FilterParameters[sc.HostChnNum+64];
							// If the values are different, then force recalculate volume. (and hence mixmode)
							if (filterCutOff <> (sc.VolEnvState.CurNode div 256)) and (filterQ <> (sc.MIDIBank div 256)) then
							begin
								sc.Volume[False] := 0;
								sc.Volume[True]  := 0;
							end;

							sc.VolEnvState.CurNode := (filterCutOff * 256) + (sc.VolEnvState.CurNode and $00FF);
							sc.MIDIBank := (filterQ << 8) or (sc.MIDIBank and $00FF);
						end;

						// 8bb: FilterEnvVal (0..255) * CutOff (0..127)
						FilterFreqValue := (sc.MIDIBank and $00FF) * (sc.VolEnvState.CurNode div 256); // !!!
						if (FilterFreqValue <> 127*255) or (filterQ <> 0) then
						begin
							Assert((FilterFreqValue <= 127*255) and (filterQ <= 127));

							r := Power(2.0, FilterFreqValue * FreqParameterMultiplier) * FreqMultiplier;
							p := QualityFactorTable[filterQ];

							// 8bb:
							// The code is done like this to be more accurate on x86/x86_64, even if the FPU is used instead of SIMD.
							// The order (and amount) of operations really matter. The FPU precision is set to 24-bit (32-bit) in IT2,
							// so we want to lose a bit of precision per calculation.
							//
							psub1 := p - 1;
							d := p * r;
							d += psub1;
							e := r * r;
							a := 1 + d;
							a += e;
							a := 16384 / a;
							fa := a;
							dea := d + e;
							dea += e;
							dea *= a;
							fb := dea;
							fc := e * a;
							fc := -fc;

							// 8bb: For all possible filter parameters ((127*255+1)*(127+1) := 4145408), there's about
							// 0.06% off-by-one errors in it2play vs. real IT2. This is fairly accurate.
							//
							// Use NearbyInt instead of Round, to get even less rounding errors vs. IT2 for
							// special numbers. The default rounding mode is FE_TONEAREST, which is what we want.
							//
							sc.filtera := NearbyInt(fa);
							sc.filterb := NearbyInt(fb);
							sc.filterc := NearbyInt(fc);

							sc.Volume[False] := 0;
							sc.Volume[True]  := 0;
						end;
					end;

					OldVolume[False] := sc.Volume[False];
					OldVolume[True]  := sc.Volume[True];

					if not Module.Header.Flags.ITF_STEREO then // 8bb: mono?
					begin
						sc.Volume[False] := (sc.FinalVol32768 * MixVolume) >> 9; // 8bb: 0..8192
						sc.Volume[True]  := sc.Volume[False];
					end
					else
					if sc.FinalPan = PAN_SURROUND then
					begin
						sc.Volume[False] := (sc.FinalVol32768 * MixVolume) >> 10; // 8bb: 0..4096
						sc.Volume[True]  := -sc.Volume[False];
					end
					else // 8bb: normal (panned)
					begin
						sc.Volume[False] := ((64-sc.FinalPan) * MixVolume * sc.FinalVol32768) >> 15; // 8bb: 0..8192
						sc.Volume[True]  := (    sc.FinalPan  * MixVolume * sc.FinalVol32768) >> 15;
					end;

					if (not sc.Flags.SF_NEW_NOTE) and (not sc.Flags.SF_NOTE_STOP) and (not sc.Flags.SF_LOOP_CHANGED) and
						(sc.Volume[False] = OldVolume[False]) and (sc.Volume[True] = OldVolume[True]) then
							UseOldMixOffset := True;
				end;
			end
			else
			begin
				// 8bb: No vol/pan update needed, use old mix offset (IT2 BUG: fast zero-vol mixer is rarely used because of this)
				UseOldMixOffset := True;
			end;
		end;

		if not UseOldMixOffset then
		begin
			sc.MixOffset := MixMode;

			if (sc.Volume[False] = 0) and (sc.Volume[True] = 0) then
			begin
				// 8bb: ramp disabled or ramp volumes zero?
				if (MixMode < 2) or ((sc.OldVolume[False] = 0) and (sc.OldVolume[True] = 0)) then
					sc.MixOffset := 4; // 8bb: use position update routine (zero volume)
			end;

			if (sc.MixOffset <> 4) and (MixMode = 3) and (sc.filterb = 0) and (sc.filterc = 0) then
				sc.MixOffset -= 1; // 8bb: Filter driver selected, but no filter active. Use non-filter mixer.
		end;

		if sc.Delta32 = 0 then // 8bb: added this protection just in case (shouldn't happen)
			Continue;

		MixBlockSize := BytesToMix;
		OldMixOffset := sc.MixOffset;

		if sc.MixOffset = 4 then // 8bb: use position update routine (zero volume)
		begin
			LoopLength := sc.LoopEnd - sc.LoopBegin; // 8bb: also length for non-loopers
			if iLoopLength > 0 then
				case sc.LoopMode of
					LOOP_PINGPONG: UpdatePingPongLoop(sc, MixBlockSize);
					LOOP_FORWARDS: UpdateForwardsLoop(sc, MixBlockSize);
					else           UpdateNoLoop      (sc, MixBlockSize);
				end;
		end
		else // 8bb: regular mixing
		begin
			//
			// MixOffset 0 = No interpolation, no volume ramp, no filtering
			// MixOffset 1 = Interpolation, no volume ramp, no filtering
			// MixOffset 2 = Interpolation, volume ramp, no filtering
			// MixOffset 3 = Interpolation, volume ramp, filtering
			// MixOffset 4 = Use position update routine (zero volume)
			//
			MixFunc := MixFunctions[sc.MixOffset << 1 + BoolToInt[sc.SmpIs16Bit]];
			Assert(MixFunc <> nil);

			// 8bb: pre-mix routine
			if sc.MixOffset >= 2 then // 8bb: volramp used?
			begin
				// 8bb: prepare volume ramp
				DestVol[False] := sc.Volume[False];
				DestVol[True]  := sc.Volume[True];

				if sc.MixOffset = 3 then // 8bb: filters (and volramp)
				begin
					// 8bb: filters use double volume range (because of 15-bit sample input)
					if (sc.Flags.SF_UPDATE_MIXERVOL) or (sc.Flags.SF_NEW_NOTE) or (sc.Flags.SF_LOOP_CHANGED) or (sc.Flags.SF_PAN_CHANGED) then
					begin
						for Chan in Boolean do
							DestVol[Chan] += DestVol[Chan];
					end;
					if (DestVol[False] = 0) and (DestVol[True] = 0) then
						sc.MixOffset += 1; // Zero-vol mixing (update positions only) (8bb: next round, under some circumstances)
				end
				else
				if sc.MixOffset = 2 then // 8bb: volramp + no filters
					sc.MixOffset -= 1; // 8bb: disable volume ramp next round (under some circumstances)

				// 8bb: compensate for upwards ramp
				for Chan in Boolean do
				begin
					if DestVol[Chan] >= sc.CurrVol[Chan] then DestVol[Chan] += RAMPCOMPENSATE;
					sc.DestVol[Chan] := DestVol[Chan];
					sc.CurrVol[Chan] := sc.OldVolume[Chan];
				end;
			end;

			if (sc.MixOffset = 3) and (sc.filtera = 0) then // 8bb: filters?
				sc.filtera := 1;

			LoopLength := sc.LoopEnd - sc.LoopBegin; // 8bb: also length for non-loopers

			if iLoopLength > 0 then
			begin
				MixBufferPtr := @MixBuffer[0];

				if sc.LoopMode = LOOP_PINGPONG then
				begin
					// pingpong loop
					while MixBlockSize > 0 do
					begin
						if sc.LoopDirection = DIR_BACKWARDS then
						begin
							if sc.SamplingPosition <= sc.LoopBegin then
							begin
								NewLoopPos := (sc.LoopBegin - sc.SamplingPosition) mod (LoopLength << 1);
								if NewLoopPos >= LoopLength then
								begin
									SetSamplingPosition((sc.LoopEnd - 1) - (NewLoopPos - LoopLength));
									if sc.SamplingPosition = sc.LoopBegin then
									begin
										sc.LoopDirection := DIR_FORWARDS;
										sc.Frac32 := (-sc.Frac32) and $FFFF;
									end;
								end
								else
								begin
									sc.LoopDirection := DIR_FORWARDS;
									SetSamplingPosition(sc.LoopBegin + NewLoopPos);
									sc.Frac32 := (-sc.Frac32) and $FFFF;
								end;
							end;
						end
						else // pingpong: forwards
						begin
							if sc.SamplingPosition >= sc.LoopEnd then
							begin
								NewLoopPos := (sc.SamplingPosition - sc.LoopEnd) mod (LoopLength << 1);
								if NewLoopPos >= LoopLength then
								begin
									SetSamplingPosition(sc.LoopBegin + (NewLoopPos - LoopLength));
								end
								else
								begin
									SetSamplingPosition((sc.LoopEnd - 1) - NewLoopPos);
									if sc.SamplingPosition <> sc.LoopBegin then
									begin
										sc.LoopDirection := DIR_BACKWARDS;
										sc.Frac32 := (-sc.Frac32) and $FFFF;
									end;
								end;
							end;
						end;

						if sc.LoopDirection = DIR_BACKWARDS then
						begin
							SamplesToMix := sc.SamplingPosition - (sc.LoopBegin + 1);
							Fix32;
							//GetSamplesToMix;
							SamplesToMix := ((SamplesToMix << MIX_FRAC_BITS) or
								(sc.Frac32 and $FFFF)) div sc.Delta32 + 1;
							Delta32 := sc.Delta32;
							Delta32 := -Delta32;
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
							SetSamplingPosition(sc.LoopBegin + ((sc.SamplingPosition - sc.LoopEnd) mod LoopLength));

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

						SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
						Fix32;
						GetSamplesToMix;
						Delta32 := sc.Delta32;

						DoMix;
					end;
				end; // no loop
			end;

			if OldMixOffset >= 2 then // 8bb: if volramp was used, reset volumes
			begin
				for Chan in Boolean do
				begin
					sc.Volume[Chan]    := sc.CurrVol[Chan];
					sc.OldVolume[Chan] := sc.CurrVol[Chan];
				end;
			end;

		end;

		ClearFlags;
	end;
end;

function TITAudioDriver_SB16MMX.PostMix(AudioOut16: PInt16; SamplesToOutput: Integer): Integer;
var
	SampleShiftValue: Byte;
	i, SamplesLeft: Integer;
	Sample: Int32;
begin
	SampleShiftValue := 13 - BoolToInt[Module.Header.Flags.ITF_STEREO];
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

procedure TITAudioDriver_SB16MMX.Mix(NumSamples: Integer; AudioOut: PInt16);
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
procedure TITAudioDriver_SB16MMX.FixSamples;
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

constructor TITAudioDriver_SB16MMX.Create(AModule: TITModule; MixingFrequency: Integer);
var
	i: Integer;
begin
	inherited;

	MixFunctions := [
		@M32Bit8M,
		@M32Bit16M,
		@M32Bit8MI,
		@M32Bit16MI,
		@M32Bit8MV,
		@M32Bit16MV,
		@M32Bit8MF,
		@M32Bit16MF
	];

	NumChannels := 128;

	Flags.DF_SUPPORTS_MIDI := True;
	Flags.DF_USES_VOLRAMP  := True;
	Flags.DF_HAS_RESONANCE_FILTER := True;

	// -1/(24*256) (8bb: w/ small rounding error!)
	FreqParameterMultiplier := -0.000162760407;

	// 1/(2*PI*110.0*2^0.25) * MixingFrequency
	FreqMultiplier := 0.00121666200 * MixingFrequency;

	// pre-calc filter coeff tables (bit-accurate)
	for i := 0 to 127 do
		QualityFactorTable[i] := Power(10, (-i * 24) / (128 * 20));

	// MixMode 0 = "MMX, Non-Interpolated"
	// MixMode 1 = "MMX, Interpolated"
	// MixMode 2 = "MMX, Volume Ramped"
	// MixMode 3 = "MMX, Filtered"
	//
	MixMode := 3;
end;


end.

