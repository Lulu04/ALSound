unit IT2play;

{$MODE OBJFPC}
{$H+}
{$R-}
{$MACRO ON}
{$COPERATORS ON}

{$DEFINE USEFPUCODE}
{$UNDEF DEBUGLOG}

interface

uses
	Types, Classes, SysUtils, Math,
	IT.Tables;

const
	BoolToInt: array[Boolean] of Byte = (0, 1);

	LOWEST_BPM_POSSIBLE = 31;

	NNA_NOTE_CUT   = 0;
	NNA_CONTINUE   = 1;
	NNA_NOTE_OFF   = 2;
	NNA_NOTE_FADE  = 3;

	DCT_DISABLED   = 0;
	DCT_NOTE       = 1;
	DCT_SAMPLE     = 2;
	DCT_INSTRUMENT = 3;

	DCA_NOTE_CUT   = 0;

	CHN_DISOWNED   = 128;
	DIR_FORWARDS   = 0;
	DIR_BACKWARDS  = 1;
	PAN_SURROUND   = 100;
	LOOP_PINGPONG  = 24;
	LOOP_FORWARDS  = 8;

	// 8bb: do NOT change these, it will only mess things up!
	MAX_PATTERNS       = 200;
	MAX_ORDERS         = 256;
	MAX_ROWS           = 200;
	MAX_SAMPLES        = 100;
	MAX_INSTRUMENTS    = 100;
	MAX_HOST_CHANNELS  = 64;
	MAX_SLAVE_CHANNELS = 256;
	MAX_SONGMSG_LENGTH = 8000;

	SMPF_ASSOCIATED_WITH_HEADER = 1;
	SMPF_16BIT = 2;
	SMPF_STEREO = 4;
	SMPF_COMPRESSED = 8;
	SMPF_USE_LOOP = 16;
	SMPF_USE_SUSTAINLOOP = 32;
	SMPF_LOOP_PINGPONG = 64;
	SMPF_SUSTAINLOOP_PINGPONG = 128;

	MIDICOMMAND_START         = $0000;
	MIDICOMMAND_STOP          = $0020;
	MIDICOMMAND_TICK          = $0040;
	MIDICOMMAND_PLAYNOTE      = $0060;
	MIDICOMMAND_STOPNOTE      = $0080;
	MIDICOMMAND_CHANGEVOLUME  = $00A0;
	MIDICOMMAND_CHANGEPAN     = $00C0;
	MIDICOMMAND_BANKSELECT    = $00E0;
	MIDICOMMAND_PROGRAMSELECT = $0100;
	MIDICOMMAND_CHANGEPITCH   = $FFFF;

	// 8bb:
	// Amount of extra bytes to allocate for every instrument sample,
	// this is used for storing interpolation tap samples before
	// entering the mixer.
	//
	SMP_MAX_INTRP_TAPS = 16;
	SMP_DAT_OFFSET = SMP_MAX_INTRP_TAPS div 2;
	SAMPLE_PAD_LENGTH = SMP_DAT_OFFSET * 2;

type
	TAudioDriverType = (
		DRIVER_NONE,
		DRIVER_DEFAULT,
		DRIVER_WAVWRITER,
		DRIVER_SB16,
		DRIVER_SB16MMX,
		DRIVER_HQ
	);

	TModuleType = (
		FORMAT_UNKNOWN = 0,
		FORMAT_IT      = 1,
		FORMAT_S3M     = 2,
		FORMAT_MOD     = 3
	);

	{$DEFINE TBitFlags8:=bitpacked record case Boolean of False: (ByteAccess: Byte); True: }
	{$DEFINE TBitFlags16:=bitpacked record case Boolean of False: (WordAccess: Word); True: }

	TITModule = class;

	TPattern = class
	public
		Rows:       Word;
		PackedData: array of Byte;
	end;

	TEnvNode = record
		Magnitude: Int8;
		Tick:      Word;
	end;
	PEnvNode = ^TEnvNode;

	TEnvState = record
		Value, Delta: Int32;
		Tick, CurNode, NextTick: Int16;
	end;

	TEnv = record
		Flags: TBitFlags8(
			ENVF_ENABLED,
			ENVF_LOOP,
			ENVF_SUSTAINLOOP,
			ENVF_CARRY,
			ENVF_UNUSED1,
			ENVF_UNUSED2,
			ENVF_UNUSED3,
			ENVF_TYPE_FILTER:    Boolean; // 8bb: for pitch envelope only
		);
		end;

		Num, LoopBegin, LoopEnd,
		SustainLoopBegin, SustainLoopEnd: Byte;
		NodePoints: array [0..24] of TEnvNode;
	end;

	TInstrument = class
	public
		DOSFilename: String[12+1];
		NNA, DCT, DCA: Byte;
		FadeOut: Word;
		PitchPanSep, PitchPanCenter, GlobVol, DefPan, RandVol, RandPan: Byte;
		InstrumentName: String[25+1];
		FilterCutoff, FilterResonance: Byte;
		MIDIChn, MIDIProg: Byte;
		MIDIBank: Word;
		SmpNoteTable: array [0..119] of Word;
		VolEnv, PanEnv, PitchEnv: TEnv;
	end;

	TSample = class
	type
		TSampleData = record
			OrigData: array of Byte;
			Data:     Pointer;
		end;

		procedure Decompress16BitData(Dst: PInt16; Src: PByte;   BlockLength: Cardinal);
		procedure Decompress8BitData (Dst: PInt8;  Src: Pointer; BlockLength: Cardinal);
	private
		Module: TITModule;
	public
		Flags: TBitFlags8(
			SMPF_ASSOCIATED_WITH_HEADER,
			SMPF_16BIT,
			SMPF_STEREO,
			SMPF_COMPRESSED,
			SMPF_USE_LOOP,
			SMPF_USE_SUSTAINLOOP,
			SMPF_LOOP_PINGPONG,
			SMPF_SUSTAINLOOP_PINGPONG: Boolean;
		); end;

		DOSFilename: String[13];
		GlobVol, Vol: Byte;
		SampleName: String[26];
		Cvt, DefPan: Byte;
		Length, LoopBegin, LoopEnd, C5Speed, SustainLoopBegin, SustainLoopEnd, OffsetInFile: Cardinal;
		AutoVibratoSpeed, AutoVibratoDepth, AutoVibratoRate, AutoVibratoWaveform: Byte;

		Data: array[Boolean] of TSampleData;

		procedure AllocateSample(NewLength: Cardinal; Is16Bit, IsStereo: Boolean);
		procedure ReleaseSample;

		function  LoadCompressedSample(Stream: TStream; Is16Bit, IsStereo, IsDeltaEncoded: Boolean): Boolean;

		constructor Create(AModule: TITModule);
	end;

	TSlaveChannel = class;

	THostChannelFlags = TBitFlags16(
		HF_UPDATE_EFX_IF_CHAN_ON,
		HF_ALWAYS_UPDATE_EFX,
		HF_CHAN_ON,
		HF_CHAN_CUT,         // No longer implemented
		HF_PITCH_SLIDE_ONGOING,
		HF_FREEPLAY_NOTE,    // 8bb: Only needed for tracker. Logic removed.
		HF_ROW_UPDATED,
		HF_APPLY_RANDOM_VOL,
		HF_UPDATE_VOLEFX_IF_CHAN_ON,
		HF_ALWAYS_VOLEFX:    Boolean;
	);
	end;

	THostChannel = class
	private
		PattLoopStartRow, PattLoopCount: Byte;

		HighSmpOffs: Byte;
		VolSlideDelta: Int8;
		PortaFreq: Int32;
		LastVibratoData, LastTremoloData: Int8;
		VibratoWaveform, VibratoPos, VibratoDepth, VibratoSpeed: Byte;
		TremorCount, TremorOnOff, RetrigCount: Byte;
		TremoloWaveform, TremoloPos, TremoloDepth, TremoloSpeed: Byte;
		PanbrelloWaveform, PanbrelloPos, PanbrelloDepth, PanbrelloSpeed, LastPanbrelloData: Byte;

		EfxMem_DKL, EfxMem_EFG, 
		EfxMem_O, EfxMem_I, EfxMem_J, EfxMem_N, EfxMem_P,
		EfxMem_Q, EfxMem_T, EfxMem_S, EfxMem_W, EfxMem_G_Compat, 
		EfxMem_SFx: Byte;
	public
		SlaveChannel: TSlaveChannel;

		{
		THostChannelFlags = ( // 8bb: host channel flags
			HF_UPDATE_EFX_IF_CHAN_ON = 1,
			HF_ALWAYS_UPDATE_EFX = 2,
			HF_CHAN_ON = 4,
			HF_CHAN_CUT = 8, // No longer implemented
			HF_PITCH_SLIDE_ONGOING = 16,
			HF_FREEPLAY_NOTE = 32, // 8bb: Only needed for tracker. Logic removed.
			HF_ROW_UPDATED = 64,
			HF_APPLY_RANDOM_VOL = 128,
			HF_UPDATE_VOLEFX_IF_CHAN_ON = 256,
			HF_ALWAYS_VOLEFX = 512
		}
		Flags: THostChannelFlags;

		MiscEfxData: array [0..15] of Byte;
		Smp, Ins, NotePackMask, RawNote, RawVolColumn,
		Cmd, CmdVal, OldCmd, OldCmdVal, VolCmd, VolCmdVal,
		HostChnNum, VolSet,
		MIDIChn, MIDIProg, TranslatedNote,
		ChnPan, ChnVol: Byte;
	end;

	TSlaveChannel = class
	public
		Flags: TBitFlags16(
			SF_CHAN_ON,
			SF_RECALC_PAN,
			SF_NOTE_OFF,
			SF_FADEOUT,
			SF_RECALC_VOL,
			SF_FREQ_CHANGE,
			SF_UPDATE_MIXERVOL,
			SF_CENTRAL_PAN,
			SF_NEW_NOTE,
			SF_NOTE_STOP,
			SF_LOOP_CHANGED,
			SF_CHN_MUTED,
			SF_VOLENV_ON,
			SF_PANENV_ON,
			SF_PITCHENV_ON,
			SF_PAN_CHANGED: Boolean;
		);
		end;

		Instrument:  TInstrument;
		Sample:      TSample;
		HostChannel: THostChannel;

		LoopBegin, LoopEnd: Cardinal;

		// should be private
		SamplingPosition: Int32;
		Delta32, Frac32: Cardinal;
		FinalVol32768: Word;
		SmpIs16Bit: Boolean;
		MixOffset: Cardinal; // 8bb: which sample mix function to use
		VolEnvState, PanEnvState, PitchEnvState: TEnvState;

		// 8bb: added these
		OldSamples:  array [0..1] of Int32;
		fOldSamples: array [0..3] of Float;
		DestVol, CurrVol: array [Boolean] of Int32; // 8bb: ramp
		filterA, filterB, filterC: Int32;
		fFilterA, fFilterB, fFilterC: Float;

		// 8bb: for custom HQ mixer
		HasLooped: Boolean;
		fVolume, fOldVolume,
		fDestVol, fCurrVol: array [Boolean] of Float;
		Frac64, Delta64: UInt64;

		// 8bb: for interpolation taps
		leftTmpSamples16, rightTmpSamples16: array [Boolean, 0..3] of Int16;
		leftTmpSamples8,  rightTmpSamples8:  array [Boolean, 0..3] of Int8;

		LoopMode, LoopDirection: Byte;
		Volume, OldVolume: array [Boolean] of Int32;
		Frequency, FrequencySet: Int32;
		AutoVibratoPos: Byte;
		AutoVibratoDepth: Word;
		FinalVol128, Vol, VolSet, ChnVol, SmpVol, FinalPan: Byte;
		FadeOut: Word;
		MIDIBank: Word;
		DCT, DCA, Pan, PanSet: Byte;

		Note, Ins, Smp: Byte;
		HostChnNum, NNA, MIDIChn, MIDIProg: Byte;

		procedure CopyFrom(Src: TSlaveChannel);
	end;

	TITAudioDriver = class // 8bb: custom struct
	const
		// delta/pos resolution for non-HQ drivers
		MIX_FRAC_BITS = 16;
		MIX_FRAC_MASK = $FFFF; //(1 << MIX_FRAC_BITS)-1;
	type
		TITAudioDriverFlags = record
			DF_SUPPORTS_MIDI,
			DF_USES_VOLRAMP,        // 8bb: aka. "hiqual"
			DF_WAVEFORM,            // Output waveform data available
			DF_HAS_RESONANCE_FILTER // 8bb: added this
			: Boolean;
		end;
	protected
		Delta32: Int32;
		Delta64: Int64;
		QualityFactorTable: array [0..127] of Float;
		FreqParameterMultiplier, FreqMultiplier: Float;

	{
		// 8bb: for "WAV writer" driver
		StartNoRamp: Boolean;
		LastLeftValue, LastRightValue: Int32;

		// 8bb: for HQ driver
		fSincLUT: PFloat;
		fLastLeftValue, fLastRightValue: Float;
	}
		// These are used when the final volume is zero, and they'll only update
		// the sampling position instead of doing actual mixing. They are the same
		// for SB16/"SB16 MMX"/"WAV writer".
		//
		procedure UpdateNoLoop(sc: TSlaveChannel; NumSamples: Cardinal); virtual;
		procedure UpdateForwardsLoop(sc: TSlaveChannel; NumSamples: Cardinal); virtual;
		procedure UpdatePingPongLoop(sc: TSlaveChannel; NumSamples: Cardinal); virtual;
	public
		Module:  TITModule;

		Flags: TITAudioDriverFlags;

		NumChannels: Cardinal;
		FilterParameters: array [0..127] of Byte;
		MixMode, MixSpeed: Cardinal;

		MixVolume: Word;
		BytesToMix, MixTransferRemaining, MixTransferOffset: Integer;
		MixBuffer: array of Int32;

		procedure MixSamples; virtual; abstract;
		procedure SetTempo(Tempo: Byte); virtual;
		procedure SetMixVolume(Volume: Byte); virtual;
		procedure ResetMixer; virtual;
		function  PostMix(AudioOut16: PInt16; SamplesToOutput: Integer): Integer; virtual; abstract;
		procedure Mix(NumSamples: Integer; AudioOut: PInt16); virtual; abstract;
		procedure FixSamples; virtual; abstract;

		constructor Create(AModule: TITModule; MixingFrequency: Integer); virtual;
	end;

	TITHeader = record
		Flags: TBitFlags16(
			ITF_STEREO,
			ITF_VOL0_OPTIMIZATION, // 8bb: not used in IT1.04 and later
			ITF_INSTR_MODE,
			ITF_LINEAR_FRQ,
			ITF_OLD_EFFECTS,
			ITF_COMPAT_GXX,
			ITF_USE_MIDI_PITCH_CNTRL,
			ITF_REQ_MIDI_CFG:   Boolean;
		);
		end;
		SongName: String[25];
		OrdNum, InsNum, SmpNum, PatNum, Cwtv, Cmwt, Special,
		MessageLength: Word;
		GlobalVol, MixVolume, InitialSpeed, InitialTempo, PanSep: Byte;
		MessageOffset: Cardinal;
		ChnlPan, ChnlVol: array [0..MAX_HOST_CHANNELS-1] of Byte;
	end;

	TMixerDefaultEvent = procedure(Module: TITModule) of Object;
	TMixerBoolEvent    = procedure(Module: TITModule; Value: Boolean) of Object;
	TMixerOpenEvent    = function(Module: TITModule; MixingFrequency, MixingBufferSize: Cardinal): Boolean of Object;

	TITModule = class
	type
		TCommandProc = procedure(hc: THostChannel) of Object;
	const
		SlideTable: array[0..8] of Byte = ( 1, 4, 8, 16, 32, 64, 96, 128, 255 );
	private
		RandSeed1, RandSeed2: Word;

		ChannelCountTable,
		ChannelVolumeTable:   array[0..99] of Byte;
		ChannelLocationTable: array[0..99] of TSlaveChannel;

		AllocateNumChannels: Cardinal;
		AllocateSlaveOffset: Word;
		LastSlaveChannel: TSlaveChannel;

		InitCommandTable,
		CommandTable,
		VolumeEffectTable: array of TCommandProc;

		ProcessOrder, ProcessRow, BreakRow: Word;
		DecodeExpectedPattern, DecodeExpectedRow: Word;

		EmptyPattern: TPattern;

		LastMIDIByte, MIDIInterpretState, MIDIInterpretType: Byte;
		MIDIDataArea: array [0..(9+16+128)*32-1] of Byte;

		procedure NoCommand({%H-}hc: THostChannel);

		procedure VolumeCommandC(hc: THostChannel);
		procedure VolumeCommandD(hc: THostChannel);
		procedure VolumeCommandE(hc: THostChannel);
		procedure VolumeCommandF(hc: THostChannel);
		procedure VolumeCommandG(hc: THostChannel);

		procedure InitCommandA(hc: THostChannel);
		procedure InitCommandB(hc: THostChannel);
		procedure InitCommandC(hc: THostChannel);
		procedure InitCommandD(hc: THostChannel);
		procedure InitCommandE(hc: THostChannel);
		procedure InitCommandF(hc: THostChannel);
		procedure InitCommandG(hc: THostChannel);
		procedure InitCommandH(hc: THostChannel);
		procedure InitCommandI(hc: THostChannel);
		procedure InitCommandJ(hc: THostChannel);
		procedure InitCommandK(hc: THostChannel);
		procedure InitCommandL(hc: THostChannel);
		procedure InitCommandM(hc: THostChannel);
		procedure InitCommandN(hc: THostChannel);
		procedure InitCommandO(hc: THostChannel);
		procedure InitCommandP(hc: THostChannel);
		procedure InitCommandQ(hc: THostChannel);
		procedure InitCommandR(hc: THostChannel);
		procedure InitCommandS(hc: THostChannel);
		procedure InitCommandT(hc: THostChannel);
		procedure InitCommandU(hc: THostChannel);
		procedure InitCommandV(hc: THostChannel);
		procedure InitCommandW(hc: THostChannel);
		procedure InitCommandX(hc: THostChannel);
		procedure InitCommandY(hc: THostChannel);
		procedure InitCommandZ(hc: THostChannel);

		procedure InitNoCommand(hc: THostChannel);
		procedure InitNoCommand3(hc: THostChannel; constref hcFlags: THostChannelFlags);
		procedure InitNoCommand11(hc: THostChannel; sc: TSlaveChannel; constref hcFlags: THostChannelFlags);
		procedure InitCommandD7(hc: THostChannel; sc: TSlaveChannel);
		procedure InitCommandG11(hc: THostChannel);
		procedure InitCommandM2(hc: THostChannel; vol: Byte);
		procedure InitCommandX2(hc: THostChannel; pan: Byte);
		procedure InitCommandEorF(hc: THostChannel; SlideUp: Boolean = False);

		procedure PreInitCommand(hc: THostChannel);
		procedure InitVibrato(hc: THostChannel);
		procedure InitTremolo(hc: THostChannel);
		procedure InitVolumeEffect(hc: THostChannel);

		procedure CommandD2(hc: THostChannel; sc: TSlaveChannel; vol: Byte);
		procedure CommandH5(hc: THostChannel; sc: TSlaveChannel; VibratoData: Int8);
		procedure CommandFChain(hc: THostChannel; SlideValue: Word);
		procedure CommandEChain(hc: THostChannel; SlideValue: Word);

		procedure CommandD(hc: THostChannel);
		procedure CommandE(hc: THostChannel);
		procedure CommandF(hc: THostChannel);
		procedure CommandG(hc: THostChannel);
		procedure CommandH(hc: THostChannel);
		procedure CommandI(hc: THostChannel);
		procedure CommandJ(hc: THostChannel);
		procedure CommandK(hc: THostChannel);
		procedure CommandL(hc: THostChannel);
		procedure CommandN(hc: THostChannel);
		procedure CommandP(hc: THostChannel);
		procedure CommandQ(hc: THostChannel);
		procedure CommandR(hc: THostChannel);
		procedure CommandR2(hc: THostChannel; sc: TSlaveChannel; TremoloData: Int8);
		procedure CommandS(hc: THostChannel);
		procedure CommandT(hc: THostChannel);
		procedure CommandW(hc: THostChannel);
		procedure CommandY(hc: THostChannel);

		procedure NoOldEffect(hc: THostChannel; constref hcFlags: THostChannelFlags);

		procedure PitchSlideLinear(hc: THostChannel; sc: TSlaveChannel; SlideValue: Int16);
		procedure PitchSlide  (hc: THostChannel; sc: TSlaveChannel; SlideValue: Int16);
		function  Gxx_ChangeSample(hc: THostChannel; sc: TSlaveChannel; sample: Byte): Boolean;

		function  DuplicateCheck(out scOut: TSlaveChannel; hc: THostChannel; hostChnNum: Byte; ins: TInstrument; DCT, DCVal: Byte): Boolean;
		function  GetPattern(Index: Word): TPattern;
		procedure UpdateGOTONote; // Get offset
		procedure UpdateNoteData;
		procedure UpdateData;
		procedure UpdateAutoVibrato(sc: TSlaveChannel); // 8bb: renamed from UpdateVibrato() to UpdateAutoVibrato() for clarity
		procedure UpdateSamples; // 8bb: for songs without instruments

		procedure ApplyRandomValues(hc: THostChannel);
		procedure GetLoopInformation(sc: TSlaveChannel);

		function  AllocateChannelSample(hc: THostChannel; var hcFlags: THostChannelFlags): TSlaveChannel;
		function  AllocateChannel(hc: THostChannel; var hcFlags: THostChannelFlags): TSlaveChannel;

		// MIDI
		procedure SetDefaultMIDIDataArea;
		//function GetMIDIDataArea: char ;
		procedure MIDITranslate(hc: THostChannel; sc: TSlaveChannel; Input: Word);
		procedure MIDISendFilter(hc: THostChannel; sc: TSlaveChannel; Data: Byte);

		// Filter
		procedure SetFilterCutoff(hc: THostChannel; sc: TSlaveChannel; Value: Byte);    // Assumes that channel is non-disowned
		procedure SetFilterResonance(hc: THostChannel; sc: TSlaveChannel; Value: Byte); // Assumes that channel is non-disowned

		// Instruments
		function  UpdateEnvelope(var env: TEnv; var envState: TEnvState; SustainReleased: Boolean): Boolean;
		procedure UpdateInstruments;
		function  AllocateChannelInstrument(hc: THostChannel; sc: TSlaveChannel; ins: TInstrument; var hcFlags: THostChannelFlags): TSlaveChannel;
		procedure InitPlayInstrument(hc: THostChannel; sc: TSlaveChannel; ins: TInstrument);

		//procedure PrepareWAVRender; // 8bb: added this
		//procedure WAV_WriteHeader(FILE *f, int32_t frq)
		//procedure void WAV_WriteEnd(FILE *f, uint32_t size)

		function  RandomNumber: Byte;

		procedure RecalculateAllVolumes;
		procedure InitTempo;
		function  FindFreeSlaveChannel: TSlaveChannel;

		function  AllocatePattern(Index, NewLength: Word): TPattern;
		procedure ReleasePattern(Index: Word);

		function  TranslateS3MPattern(Src: PByte; Pattern: Word): Boolean;

		function  LoadIT (Stream: TStream): Boolean;
		function  LoadS3M(Stream: TStream): Boolean;

		function  OpenMixer(MixingFrequency, MixingBufferSize: Word): Boolean; // 16000..64000, 256..8192
		procedure LockMixer;
		procedure UnlockMixer;
		procedure CloseMixer;
	public
		Header: TITHeader;

		Orders:        array [0..MAX_ORDERS-1] of Byte;
		Instruments:   array [0..MAX_INSTRUMENTS-1] of TInstrument;
		Samples:       array [0..MAX_SAMPLES-1]  of TSample;
		Patterns:      array [0..MAX_PATTERNS-1] of TPattern;
		HostChannels:  array [0..MAX_HOST_CHANNELS-1]  of THostChannel;
		SlaveChannels: array [0..MAX_SLAVE_CHANNELS-1] of TSlaveChannel;

		{Patterns: TObjectList<TPattern>;
		Instruments: TObjectList<TInstrument>;
		Samples: TObjectList<TSample>;}

		SongMessage: TStringList;
		//SongMessage: array [0..MAX_SONGMSG_LENGTH] of AnsiChar; // 8bb: +1 to fit protection-NUL

		Loaded, Playing: Boolean;
		CurrentOrder, CurrentPattern, CurrentRow: Word;
		PatternOffset: PByte;
		RowDelay: Byte;
		RowDelayOn, StopSong, PatternLooping: Boolean;
		NumberOfRows, CurrentTick, CurrentSpeed, ProcessTick: Word;
		Tempo, GlobalVolume: Word;

		Driver: TITAudioDriver;

		// callbacks
		OnLockMixer:  TMixerBoolEvent;
		OnOpenMixer:  TMixerOpenEvent; // 16000..64000, 256..8192
		OnCloseMixer: TMixerDefaultEvent;
		OnPlayback:   TMixerBoolEvent;

		function  GetModuleType(Stream: TStream): TModuleType;
		function  LoadFromStream(Stream: TStream): Boolean;
		function  LoadFromFile(const Filename: String): Boolean;

		procedure Update;
		procedure FillAudioBuffer(Buffer: PInt16; NumSamples: Cardinal);

		//procedure PreviousOrder;
		//procedure NextOrder;
		function  Play(Order: Word = 0): Boolean;
		procedure Stop;

		procedure FreeSong;

		//function  GetActiveVoices: Integer;
		//function  RenderToWAV(Filename: String): Boolean;

		function  Init(DriverType: TAudioDriverType;
		          MixingFrequency: Word = 44100;
		          MixingBufferSize: Cardinal = 0): Boolean;

		constructor Create;
		destructor  Destroy; override;
	end;


	function NearbyInt(F: Float): Integer; inline;


implementation

uses
	IT.AudioDriver.SB16,
	IT.AudioDriver.SB16MMX;


// ================================================================================================
// Debug
// ================================================================================================


procedure Debug(const {%H-}Msg: String);
begin
	{$IFDEF DEBUGLOG}
	WriteLn(Msg);
	{$ENDIF}
end;

procedure DebugInfo(const {%H-}Msg: String);
begin
	{$IFDEF DEBUGLOG}
	Debug(Msg);
	{$ENDIF}
end;


// ================================================================================================
// Utility
// ================================================================================================


function NearbyInt(F: Float): Integer; inline;
begin
	Result := Trunc(F);
	if Abs(Frac(F)) >= 0.5 then
		Result += Trunc(2 * Frac(F));
end;


// ================================================================================================
// TSlaveChannel
// ================================================================================================


procedure TSlaveChannel.CopyFrom(Src: TSlaveChannel);
var
	B: Boolean;
begin
	Flags := Src.Flags;

	VolEnvState := Src.VolEnvState;
	PanEnvState := Src.PanEnvState;
	PitchEnvState := Src.PitchEnvState;

	SamplingPosition := Src.SamplingPosition;
	Delta32 := Src.Delta32;
	Frac32 := Src.Frac32;
	FinalVol32768 := Src.FinalVol32768;
	SmpIs16Bit := Src.SmpIs16Bit;

	LoopMode := Src.LoopMode;
	LoopDirection := Src.LoopDirection;
	for B in Boolean do
	begin
		Volume[B] := Src.Volume[B];
		OldVolume[B] := Src.OldVolume[B];
	end;
	Frequency := Src.Frequency;
	FrequencySet := Src.FrequencySet;
	AutoVibratoPos := Src.AutoVibratoPos;
	AutoVibratoDepth := Src.AutoVibratoDepth;
	FinalVol128 := Src.FinalVol128;
	Vol := Src.Vol;
	VolSet := Src.VolSet;
	ChnVol := Src.ChnVol;
	SmpVol := Src.SmpVol;
	FinalPan := Src.FinalPan;
	FadeOut := Src.FadeOut;
	MIDIBank := Src.MIDIBank;
	DCT := Src.DCT;
	DCA := Src.DCA;
	Pan := Src.Pan;
	PanSet := Src.PanSet;

	Note := Src.Note;
	Ins := Src.Ins;
	Smp := Src.Smp;
	HostChnNum := Src.HostChnNum;
	NNA := Src.NNA;
	MIDIChn := Src.MIDIChn;
	MIDIProg := Src.MIDIProg;
	LoopBegin := Src.LoopBegin;
	LoopEnd := Src.LoopEnd;

	Instrument := Src.Instrument;
	Sample := Src.Sample;
	HostChannel := Src.HostChannel;
end;


// ================================================================================================
// TSample
// ================================================================================================

procedure TSample.AllocateSample(NewLength: Cardinal; Is16Bit, IsStereo: Boolean);
var
	IsRightSample: Boolean;
begin
	// NewLength is in samples, not bytes!
	if Is16Bit then NewLength *= 2;

	DebugInfo(Format('AllocateSample(%d, 16bit=%d, Stereo=%d)',
		[NewLength, BoolToInt[is16bit], BoolToInt[isstereo]]));

	for IsRightSample := False to IsStereo do
	begin
		// 8bb: extra bytes for interpolation taps, filled later
		SetLength(Data[IsRightSample].OrigData, NewLength + SAMPLE_PAD_LENGTH);
		FillByte(Data[IsRightSample].OrigData[0], SMP_DAT_OFFSET, 0);
		FillByte(Data[IsRightSample].OrigData[NewLength], SMP_MAX_INTRP_TAPS div 2, 0);
		// 8bb: offset sample so that we can fix negative interpolation taps
		Data[IsRightSample].Data := @Data[IsRightSample].OrigData[SMP_DAT_OFFSET];
	end;

	Length := NewLength;
	Flags.SMPF_ASSOCIATED_WITH_HEADER := True;
end;

procedure TSample.ReleaseSample;
var
	B: Boolean;
begin
	if Module <> nil then
		Module.LockMixer;

	for B in Boolean do
	begin
		SetLength(Data[B].OrigData, 0);
		Data[B].Data := nil;
	end;

	if Module <> nil then
		Module.UnlockMixer;
end;

procedure TSample.Decompress16BitData(Dst: PInt16; Src: PByte; BlockLength: Cardinal);
var
	Byte8, BitDepth, BitDepthInv, BitsRead: Byte;
	Bytes16, LastVal, DX: Word;
	Bytes32: Cardinal;
	foo: SmallInt;
	pca: PCardinal;
begin
	DebugInfo('Decompress16BitData');

	LastVal := 0;
	BitDepth := 17;
	BitDepthInv := 0;
	BitsRead := 0;
	BlockLength := BlockLength >> 1;

	while BlockLength <> 0 do
	begin
		pca := PCardinal(Src);
		Bytes32 := pca^ >> BitsRead;

		BitsRead += BitDepth;
		Src += (BitsRead >> 3);
		BitsRead := BitsRead and 7;

		if BitDepth <= 6 then
		begin
			Bytes32 := Bytes32 << (BitDepthInv and $1F);
			Bytes16 := Bytes32 and $FFFF;

			if Bytes16 <> $8000 then
			begin
				foo := SarSmallint(Int16(Bytes16), BitDepthInv and $1F); // arithmetic shift
				LastVal += foo;
				Dst^ := LastVal;
				Inc(Dst);
				Dec(BlockLength);
			end
			else
			begin
				Byte8 := ((Bytes32 >> 16) and $F) + 1;
				if Byte8 >= BitDepth then Inc(Byte8);
				BitDepth := Byte8;

				BitDepthInv := 16;
				if BitDepthInv < BitDepth then Inc(BitDepthInv);
				BitDepthInv -= BitDepth;

				BitsRead += 4;
			end;

			Continue;
		end;

		Bytes16 := Bytes32 and $FFFF;

		if BitDepth <= 16 then
		begin
			DX := $FFFF >> (BitDepthInv and $1F);
			Bytes16 := Bytes16 and DX;
			DX := (DX >> 1) - 8;

			if (Bytes16 > DX+16) or (Bytes16 <= DX) then
			begin
				Bytes16 := Bytes16 << (BitDepthInv and $1F);
				foo := SarSmallInt(Int16(Bytes16), BitDepthInv and $1F); // arithmetic shift
				LastVal += foo;
				Bytes16 := Word(foo);
				Dst^ := LastVal;
				Inc(Dst);
				Dec(BlockLength);

				Continue;
			end;

			Byte8 := (Bytes16 - DX) and $FF;
			if Byte8 >= BitDepth then Inc(Byte8);
			BitDepth := Byte8;

			BitDepthInv := 16;
			if BitDepthInv < BitDepth then Inc(BitDepthInv);
			BitDepthInv -= BitDepth;

			Continue;
		end;

		if (Bytes32 and $10000) > 0 then
		begin
			BitDepth := (Bytes16 + 1) and $FF;
			BitDepthInv := 16 - BitDepth;
		end
		else
		begin
			LastVal += Bytes16;
			Dst^ := LastVal;
			Inc(Dst);
			Dec(BlockLength);
		end;
	end;
end;

procedure TSample.Decompress8BitData(Dst: PInt8; Src: Pointer; BlockLength: Cardinal);
var
	LastVal, Byte8, BitDepth, BitDepthInv, BitsRead: Byte;
	Bytes16: Word;
begin
	DebugInfo('Decompress8BitData');

	LastVal := 0;
	BitDepth := 9;
	BitDepthInv := 0;
	BitsRead := 0;

	while BlockLength <> 0 do
	begin
		Bytes16 := PWord(Src)^ shr BitsRead;

		BitsRead += BitDepth;
		Src += (BitsRead >> 3);
		BitsRead := BitsRead and 7;

		Byte8 := Bytes16 and $FF;

		if BitDepth <= 6 then
		begin
			Bytes16 := Bytes16 << (BitDepthInv and $1F);
			Byte8 := Bytes16 and $FF;

			if Byte8 <> $80 then
			begin
				LastVal += Int8(Byte8) >> (BitDepthInv and $1F); // arithmetic shift
				Dst^ := LastVal;
				Inc(Dst);
				Dec(BlockLength);
				Continue;
			end;

			Byte8 := (Bytes16 >> 8) and 7;
			BitsRead += 3;
			Src += (BitsRead >> 3);
			BitsRead := BitsRead and 7;
		end
		else
		begin
			if BitDepth = 8 then
			begin
				if (Byte8 < $7C) or (Byte8 > $83) then
				begin
					LastVal += Byte8;
					Dst^ := LastVal;
					Inc(Dst);
					Dec(BlockLength);
					Continue;
				end;
				Byte8 -= $7C;
			end
			else
			if BitDepth < 8 then
			begin
				Byte8 := Byte8 << 1;
				if (Byte8 < $78) or (Byte8 > $86) then
				begin
					LastVal += Int8(Byte8) >> (BitDepthInv and $1F); // arithmetic shift
					Dst^ := LastVal;
					Inc(Dst);
					Dec(BlockLength);
					Continue;
				end;
				Byte8 := (Byte8 >> 1) - $3C;
			end
			else
			begin
				Bytes16 := Bytes16 and $1FF;
				if (Bytes16 and $100) = 0 then
				begin
					LastVal += Byte8;
					Dst^ := LastVal;
					Inc(Dst);
					Dec(BlockLength);
					Continue;
				end;
			end;
		end;

		Inc(Byte8);
		if Byte8 >= BitDepth then Inc(Byte8);
		BitDepth := Byte8;

		BitDepthInv := 8;
		if BitDepthInv < BitDepth then Inc(BitDepthInv);
		BitDepthInv -= BitDepth;
	end;
end;

function TSample.LoadCompressedSample(Stream: TStream;
	Is16Bit, IsStereo, IsDeltaEncoded: Boolean): Boolean;
var
	Chan: Boolean;
	DstPtr: PInt8;
	Ptr16:  PInt16;
	LastSmp8: Int8;
	LastSmp16: Int16;
	i, j, BytesToUnpack: Cardinal;
	PackedLen: Word;
	DecompBuffer: array [0..64*1024] of Byte;
begin
	Result := False;
	if Stream = nil then Exit;

	DebugInfo(Format('LoadCompressedSample(16bit=%d, Stereo=%d, Delta=%d)',
		[BoolToInt[is16bit], BoolToInt[isstereo], BoolToInt[IsDeltaEncoded]]));

	for Chan := False to IsStereo do
	begin
		DstPtr := Data[Chan].Data;
		i := Length;
		while i > 0 do
		begin
			BytesToUnpack := Min(i, 32768);
			PackedLen := Stream.ReadWord;
			DebugInfo('  PackedLen    =' + PackedLen.ToString);
			DebugInfo('  BytesToUnpack=' + BytesToUnpack.ToString);
			Stream.ReadBuffer({%H-}DecompBuffer[0], PackedLen);

			if Is16bit then
			begin
				Decompress16BitData(PInt16(DstPtr), @DecompBuffer[0], BytesToUnpack);

				if IsDeltaEncoded then // convert from delta values to PCM
				begin
					Ptr16 := Data[Chan].Data;
					LastSmp16 := 0; // yes, reset this every block!
					for j := 0 to BytesToUnpack div 2 - 1 do
					begin
						LastSmp16 += Ptr16^;
						Ptr16^ := LastSmp16;
						Inc(Ptr16);
					end;
				end;
			end
			else
			begin
				Decompress8BitData(DstPtr, @DecompBuffer[0], BytesToUnpack);

				if IsDeltaEncoded then // convert from delta values to PCM
				begin
					LastSmp8 := 0; // yes, reset this every block!
					for j := 0 to BytesToUnpack-1 do
					begin
						LastSmp8 += DstPtr^;
						DstPtr^ := LastSmp8;
						Inc(DstPtr);
					end;
				end;
			end;

			DstPtr += BytesToUnpack;
			i -= BytesToUnpack;
		end;
	end;

	Result := True;
end;

constructor TSample.Create(AModule: TITModule);
begin
	inherited Create;

	Module := AModule;
end;


// ================================================================================================
// TITAudioDriver
// ================================================================================================


// TODO remove unnecessary code duplication

procedure TITAudioDriver.UpdateNoLoop(sc: TSlaveChannel; NumSamples: Cardinal);
var
	SamplingPosition, IntSamples: Cardinal;
	FracSamples: Word;
	Delta: UInt64;
begin
	SamplingPosition := sc.SamplingPosition;

	Delta := sc.Delta32 * NumSamples;
	IntSamples  := Delta >> MIX_FRAC_BITS;
	FracSamples := Delta and MIX_FRAC_MASK;

	sc.Frac32 += FracSamples;
	sc.SamplingPosition += SarLongInt(Int32(sc.Frac32), MIX_FRAC_BITS);
	sc.Frac32 := sc.Frac32 and MIX_FRAC_MASK;
	SamplingPosition += IntSamples;

	if SamplingPosition >= sc.LoopEnd then
	begin
		sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
		if (sc.HostChnNum and CHN_DISOWNED) = 0 then
		begin
			sc.HostChannel.Flags.HF_CHAN_ON := False; // Signify channel off
			Exit;
		end;
	end;

	sc.SamplingPosition := SamplingPosition;
end;

procedure TITAudioDriver.UpdateForwardsLoop(sc: TSlaveChannel; NumSamples: Cardinal);
var
	IntSamples, LoopLength: Cardinal;
	FracSamples: Word;
	Delta: UInt64;
begin
	Delta := sc.Delta32 * NumSamples;
	IntSamples  := Delta >> MIX_FRAC_BITS;
	FracSamples := Delta and MIX_FRAC_MASK;

	sc.Frac32 += FracSamples;
	sc.SamplingPosition += sc.Frac32 >> MIX_FRAC_BITS;
	sc.Frac32 := sc.Frac32 and MIX_FRAC_MASK;
	sc.SamplingPosition += IntSamples;

	if sc.SamplingPosition >= sc.LoopEnd then // Reset position...
	begin
		LoopLength := sc.LoopEnd - sc.LoopBegin;
		if LoopLength = 0 then
			sc.SamplingPosition := 0
		else
			sc.SamplingPosition := sc.LoopBegin + ((sc.SamplingPosition - sc.LoopEnd) div LoopLength);
	end;
end;

procedure TITAudioDriver.UpdatePingPongLoop(sc: TSlaveChannel; NumSamples: Cardinal);
var
	IntSamples, LoopLength, NewLoopPos: Cardinal;
	FracSamples: Word;
	Delta: UInt64;
begin
	Assert(NumSamples <= $FFFF);

	Delta := sc.Delta32 * NumSamples;
	IntSamples  := Delta >> MIX_FRAC_BITS;
	FracSamples := Delta and MIX_FRAC_MASK;
	LoopLength := sc.LoopEnd - sc.LoopBegin;

	if sc.LoopDirection = DIR_BACKWARDS then
	begin
		sc.Frac32 -= FracSamples;
		sc.SamplingPosition += SarLongInt(Int32(sc.Frac32), MIX_FRAC_BITS);
		sc.SamplingPosition -= IntSamples;
		sc.Frac32 := sc.Frac32 and MIX_FRAC_MASK;

		if sc.SamplingPosition <= sc.LoopBegin then
		begin
			NewLoopPos := Cardinal(sc.LoopBegin - sc.SamplingPosition) mod (LoopLength * 2);
			if NewLoopPos >= LoopLength then
				sc.SamplingPosition := (sc.LoopEnd - 1) - (NewLoopPos - LoopLength)
			else
			begin
				sc.LoopDirection := DIR_FORWARDS;
				sc.SamplingPosition := sc.LoopBegin + NewLoopPos;
				sc.Frac32 := Word(0 - sc.Frac32);
			end;
		end;
	end
	else // 8bb: forwards
	begin
		sc.Frac32 += FracSamples;
		sc.SamplingPosition += SarLongInt(Int32(sc.Frac32), MIX_FRAC_BITS);
		sc.SamplingPosition += IntSamples;
		sc.Frac32 := sc.Frac32 and MIX_FRAC_MASK;

		if Cardinal(sc.SamplingPosition) >= Cardinal(sc.LoopEnd) then
		begin
			NewLoopPos := Cardinal(sc.SamplingPosition - sc.LoopEnd) mod (LoopLength * 2);
			if NewLoopPos >= LoopLength then
				sc.SamplingPosition := sc.LoopBegin + (NewLoopPos - LoopLength)
			else
			begin
				sc.SamplingPosition := (sc.LoopEnd - 1) - NewLoopPos;
				sc.LoopDirection := DIR_BACKWARDS;
				sc.Frac32 := Word(0 - sc.Frac32);
			end;
		end;
	end;
end;

procedure TITAudioDriver.SetTempo(Tempo: Byte);
begin
	if Tempo < LOWEST_BPM_POSSIBLE then
		Tempo := LOWEST_BPM_POSSIBLE;
	BytesToMix := ((MixSpeed * 2) + (MixSpeed div 2)) div Tempo;
end;

procedure TITAudioDriver.SetMixVolume(Volume: Byte);
begin
	MixVolume := Volume;
	Module.RecalculateAllVolumes;
end;

procedure TITAudioDriver.ResetMixer;
begin
	MixTransferRemaining := 0;
	MixTransferOffset := 0;
	SetMixVolume(Module.Header.MixVolume);
end;

constructor TITAudioDriver.Create(AModule: TITModule; MixingFrequency: Integer);
var
	MaxSamplesToMix: Integer;
begin
	inherited Create;

	Flags := Default(TITAudioDriverFlags);
	Module := AModule;

	MixingFrequency := Max(MixingFrequency, 8000);
	MixingFrequency := Min(MixingFrequency, 64000);
	MixSpeed := MixingFrequency;

	MaxSamplesToMix := ((MixingFrequency * 2) + (MixingFrequency div 2)) div LOWEST_BPM_POSSIBLE + 1;
	SetLength(MixBuffer, MaxSamplesToMix * 2);

	NumChannels := 64;
end;


// ================================================================================================
// TITModule
// ================================================================================================

// fill default MIDI configuration values (important for filters)
procedure TITModule.SetDefaultMIDIDataArea; // 8bb: added this

	procedure PutData(Index: Word; const Data: AnsiString);
	var
		i: Integer;
	begin
		for i := 0 to Length(Data)-1 do
			MIDIDataArea[Index+i] := Ord(Data[i+1]);
	end;

begin
	FillByte(MIDIDataArea[0], Length(MIDIDataArea), 0); // data is padded with zeroes, not spaces

	// MIDI commands
	PutData(0*32, 'FF');
	PutData(1*32, 'FC');
	PutData(3*32, '9c n v');
	PutData(4*32, '9c n 0');
	PutData(7*32, 'Bc 0 a 20 b');
	PutData(8*32, 'Cc p');

	// macro setup (SF0)
	PutData(9*32, 'F0F000z');

	// macro setup (Z80..Z8F)
	PutData(25*32, 'F0F00100');
	PutData(26*32, 'F0F00108');
	PutData(27*32, 'F0F00110');
	PutData(28*32, 'F0F00118');
	PutData(29*32, 'F0F00120');
	PutData(30*32, 'F0F00128');
	PutData(31*32, 'F0F00130');
	PutData(32*32, 'F0F00138');
	PutData(33*32, 'F0F00140');
	PutData(34*32, 'F0F00148');
	PutData(35*32, 'F0F00150');
	PutData(36*32, 'F0F00158');
	PutData(37*32, 'F0F00160');
	PutData(38*32, 'F0F00168');
	PutData(39*32, 'F0F00170');
	PutData(40*32, 'F0F00178');
end;

procedure TITModule.MIDITranslate(hc: THostChannel; sc: TSlaveChannel; Input: Word);
var
	B: Int16;
	volume: Word;
	cA, value, MIDIData, CharsParsed: Byte;
begin
	if (Driver = nil) or (not Driver.Flags.DF_SUPPORTS_MIDI) then Exit;
	if Input >= $F000 then Exit; // 8bb: we don't support (nor need) MIDI commands
	if (Input div 32) >= (9+16+128) then Exit; // 8bb: added protection, just in case

	MIDIData := 0;
	CharsParsed := 0;
	cA := Ord('a');

	while True do
	begin
		B := MIDIDataArea[Input];
		Input += 1;

		if B = 0 then
		begin
			if CharsParsed > 0 then
				MIDISendFilter(hc, sc, MIDIData);
			Break; // 8bb: and we're done!
		end;

		if B = Ord(' ') then
		begin
			if CharsParsed > 0 then
				MIDISendFilter(hc, sc, MIDIData);
			Continue;
		end;

		// Interpretation time.

		B -= Ord('0');
		if B < 0 then Continue;

		if B <= 9 then
		begin
			MIDIData := (MIDIData << 4) or B; // !!!
			CharsParsed += 1;

			if CharsParsed >= 2 then
			begin
				MIDISendFilter(hc, sc, MIDIData);
				CharsParsed := 0;
				MIDIData := 0;
			end;

			Continue;
		end;

		B -= Ord('A') - Ord('0');
		if B < 0 then Continue;

		if B <= (Ord('F') - Ord('A')) then
		begin
			MIDIData := (MIDIData << 4) or (B + 10);
			CharsParsed += 1;

			if CharsParsed >= 2 then
			begin
				MIDISendFilter(hc, sc, MIDIData);
				CharsParsed := 0;
				MIDIData := 0;
			end;

			Continue;
		end;

		B -= cA - Ord('A');
		if (B < 0) or (B > (Ord('z') - cA)) then Continue;

		if B = (Ord('c') - cA) then
		begin
			if sc = nil then Continue;

			MIDIData := (MIDIData << 4) or (sc.MIDIChn-1);
			CharsParsed += 1;

			if CharsParsed >= 2 then
			begin
				MIDISendFilter(hc, sc, MIDIData);
				CharsParsed := 0;
				MIDIData := 0;
			end;

			Continue;
		end;

		if CharsParsed > 0 then
		begin
			MIDISendFilter(hc, sc, MIDIData);
			MIDIData := 0;
		end;

		if B = (Ord('z') - cA) then // Zxx?
			MIDISendFilter(hc, sc, hc.CmdVal)
		else
		if B = (Ord('o') - cA) then // 8bb: sample offset?
			MIDISendFilter(hc, sc, hc.EfxMem_O)
		else
		if sc <> nil then
		begin
			if B = (Ord('n') - cA) then // Note?
				MIDISendFilter(hc, sc, sc.Note)
			else
			if B = (Ord('m') - cA) then // 8bb: MIDI note (sample loop direction on sample channels)
				MIDISendFilter(hc, sc, sc.LoopDirection)
			else
			if B = (Ord('v') - cA) then // Velocity?
			begin
				if sc.Flags.SF_CHN_MUTED then
					MIDISendFilter(hc, sc, 0)
				else
				begin
					volume := (sc.VolSet * GlobalVolume * sc.ChnVol) >> 4;
					value := (volume * sc.SmpVol) >> 15;

					if value = 0    then value := 1 else
					if value >= 128 then value -= 1;

					MIDISendFilter(hc, sc, value);
				end;
			end
			else
			if B = (Ord('u') - cA) then // Volume?
			begin
				if sc.Flags.SF_CHN_MUTED then
					MIDISendFilter(hc, sc, 0)
				else
				begin
					value := sc.FinalVol128;
					if value = 0    then value := 1 else
					if value >= 128 then value -= 1;

					MIDISendFilter(hc, sc, value);
				end;
			end
			else
			if B = (Ord('h') - cA) then // HCN (8bb: host channel number)
				MIDISendFilter(hc, sc, sc.HostChnNum and $7F)
			else
			if B = (Ord('x') - cA) then // Pan set
			begin
				value := Byte(sc.Pan * 2); // 8bb: yes, sc.Pan (not sc.PanSet)
				if value >= 128 then value -= 1;
				if value >= 128 then value := 64;
				MIDISendFilter(hc, sc, value);
			end
			else
			if B = (Ord('p') - cA) then // Program?
				MIDISendFilter(hc, sc, sc.MIDIProg)
			else
			if B = (Ord('b') - cA) then // 8bb: MIDI bank low
				MIDISendFilter(hc, sc, sc.MIDIBank and $FF)
			else
			if B = (Ord('a') - cA) then // 8bb: MIDI bank high
				MIDISendFilter(hc, sc, sc.MIDIBank >> 8);
		end;

		MIDIData := 0;
		CharsParsed := 0;
	end;
end;

procedure TITModule.MIDISendFilter(hc: THostChannel; sc: TSlaveChannel; Data: Byte);
var
	i: Integer;
	IsFilterQ: Boolean;
begin
	if not Driver.Flags.DF_SUPPORTS_MIDI then Exit;

	if (Data >= $80) and (Data < $F0) then
	begin
		if Data = LastMIDIByte then Exit;
		LastMIDIByte := Data;
	end;

	// 8bb: We implement the SendUARTOut() code found in the
	// SB16 MMX driver and WAV writer driver and use it here
	// instead of doing real MIDI data handling.
	//
	// It will only interpret filter commands (set and clear).
	//
	if MIDIInterpretState < 2 then
	begin
		if Data = $F0 then
			MIDIInterpretState += 1
		else
		begin
			if (Data = $FA) or (Data = $FC) or (Data = $FF) then
			begin
				// 8bb: reset filters
				for i := 0 to MAX_HOST_CHANNELS-1 do
				begin
					Driver.FilterParameters[00+i] := 127; // 8bb: Cutoff
					Driver.FilterParameters[64+i] := 0;   // 8bb: Q
				end;
			end;
			MIDIInterpretState := 0;
		end;
	end
	else
	if MIDIInterpretState = 2 then
	begin
		if Data < 2 then // 8bb: must be 0..1 (Cutoff or Q)
		begin
			MIDIInterpretType := Data;
			MIDIInterpretState += 1;
		end
		else
			MIDIInterpretState := 0;
	end
	else
	if MIDIInterpretState = 3 then
	begin
		// Have InterpretType, now get parameter, then return to normal
		if Data <= $7F then
		begin
			IsFilterQ := (MIDIInterpretType = 1);
			if IsFilterQ then
				Driver.FilterParameters[(64 + hc.HostChnNum) and 127] := Data
			else
				Driver.FilterParameters[hc.HostChnNum and 127] := Data;
			if sc <> nil then
				sc.Flags.SF_UPDATE_MIXERVOL := True;
		end;
		MIDIInterpretState := 0;
	end;
end;

procedure TITModule.SetFilterCutoff(hc: THostChannel; sc: TSlaveChannel; Value: Byte);
begin
	MIDISendFilter(hc, sc, $F0);
	MIDISendFilter(hc, sc, $F0);
	MIDISendFilter(hc, sc, $00);
	MIDISendFilter(hc, sc, value);
end;

procedure TITModule.SetFilterResonance(hc: THostChannel; sc: TSlaveChannel; Value: Byte);
begin
	MIDISendFilter(hc, sc, $F0);
	MIDISendFilter(hc, sc, $F0);
	MIDISendFilter(hc, sc, $01);
	MIDISendFilter(hc, sc, value);
end;

function TITModule.DuplicateCheck(out scOut: TSlaveChannel; hc: THostChannel; hostChnNum: Byte;
	ins: TInstrument; DCT, DCVal: Byte): Boolean;
var
	i: Integer;
	sc: TSlaveChannel;
begin
	for i := 0 to AllocateNumChannels-1 do
	begin
		sc := SlaveChannels[AllocateSlaveOffset + i];
		scOut := sc; // 8bb: copy current slave channel pointer to scOut

		if (not sc.Flags.SF_CHAN_ON) or ((hc.Smp <> 101) and (sc.HostChnNum <> hostChnNum)) or (sc.Ins <> hc.Ins) then
			Continue;

		// 8bb: the actual duplicate test
		if (DCT = DCT_NOTE)       and (sc.Note <> DCVal) then Continue;
		if (DCT = DCT_SAMPLE)     and (sc.Smp  <> DCVal) then Continue;
		if (DCT = DCT_INSTRUMENT) and (sc.Ins  <> DCVal) then Continue;

		if hc.Smp = 101 then // New note is a MIDI?
		begin
			if (sc.Smp = 100) and (sc.MIDIChn = hostChnNum) then // is current channel a MIDI chan?
			begin
				sc.Flags.SF_NOTE_STOP := True;
				if (sc.HostChnNum and CHN_DISOWNED) = 0 then
				begin
					sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED;
					sc.HostChannel.Flags.HF_CHAN_ON := False;
				end;
			end;
		end
		else
		if sc.DCA = ins.DCA then
			Exit(True); // 8bb: dupe found
	end;

	Result := False; // 8bb: dupe not found
end;

function TITModule.GetPattern(Index: Word): TPattern;
begin
	Assert(Index < MAX_PATTERNS);
	Result := Patterns[Index];
	if Result.PackedData = nil then
		Result := EmptyPattern;
end;

procedure TITModule.PreInitCommand(hc: THostChannel);
var
	Ins: TInstrument;
begin
	if hc.NotePackMask and $33 <> 0 then
	begin
		if (not Header.Flags.ITF_INSTR_MODE) or (hc.RawNote >= 120) or (hc.Ins = 0) then
		begin
			hc.TranslatedNote := hc.RawNote;
			hc.Smp := hc.Ins;
		end
		else
		begin
			Ins := Instruments[hc.Ins-1];
			if Ins = nil then Exit;

			hc.TranslatedNote := Ins.SmpNoteTable[hc.RawNote] and $FF;

			// 8bb:
			// Added >128 check to prevent instruments with ModPlug/OpenMPT plugins
			// from being handled as MIDI (would result in silence, and crash IT2).
			//
			if (Ins.MIDIChn = 0) or (Ins.MIDIChn > 128) then
			begin
				hc.Smp := Ins.SmpNoteTable[hc.RawNote] >> 8;
			end
			else // 8bb: MIDI
			begin
				hc.MIDIChn := IfThen(Ins.MIDIChn = 17, hc.HostChnNum and $0F + 1, ins.MIDIChn);
				hc.MIDIProg := Ins.MIDIProg;
				hc.Smp := 101;
			end;

			if hc.Smp = 0 then Exit; // No sample
		end;
	end;

	InitCommandTable[hc.Cmd and 31](hc); // Init note

	hc.Flags.HF_ROW_UPDATED := True;

	if (Header.ChnlPan[hc.HostChnNum] and 128 <> 0) and // channel muted
		(not hc.Flags.HF_FREEPLAY_NOTE) and (hc.Flags.HF_CHAN_ON) then
		hc.SlaveChannel.Flags.SF_CHN_MUTED := True;
end;

procedure TITModule.UpdateGOTONote;
var
	hc: THostChannel;
	Pattern: TPattern;
	p: PByte;
	rowsTodo: Word;
	chnNum: Byte;
begin
	DecodeExpectedPattern := CurrentPattern;

	Pattern := GetPattern(DecodeExpectedPattern);
	p := @Pattern.PackedData[0];
	NumberOfRows := Pattern.Rows;
	if ProcessRow >= NumberOfRows then
		ProcessRow := 0;

	CurrentRow := ProcessRow;
	DecodeExpectedRow := ProcessRow;
	rowsTodo := ProcessRow;

	if rowsTodo > 0 then
	while True do
	begin
		chnNum := p^;
		Inc(p);

		if chnNum = 0  then
		begin
			Dec(rowsTodo);
			if rowsTodo = 0 then Break;
			Continue;
		end;

		hc := HostChannels[(chnNum and $7F) - 1];

		if (chnNum and $80) <> 0 then
		begin
			hc.NotePackMask := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 1) <> 0 then
		begin
			hc.RawNote := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 2) <> 0 then
		begin
			hc.Ins := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 4) <> 0 then
		begin
			hc.RawVolColumn := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 8) <> 0 then
		begin
			hc.OldCmd    := p^; Inc(p);
			hc.OldCmdVal := p^; Inc(p);
		end;
	end;

	PatternOffset := p;
end;

procedure TITModule.UpdateNoteData;
var
	hc: THostChannel;
	i: Integer;
	B: Boolean;
	p: PByte;
	chnNum: Byte;
begin
	PatternLooping := False;

	B := CurrentPattern <> DecodeExpectedPattern;
	if not B then
	begin
		Inc(DecodeExpectedRow);
		B := DecodeExpectedRow <> CurrentRow;
	end;
	if B then
		UpdateGOTONote;

	// First clear all old command & value.
	for i := 0 to MAX_HOST_CHANNELS-1 do
	with HostChannels[i].Flags do
	begin
		HF_UPDATE_EFX_IF_CHAN_ON := False;
		HF_ALWAYS_UPDATE_EFX := False;
		HF_ROW_UPDATED := False;
		HF_UPDATE_VOLEFX_IF_CHAN_ON := False;
	end;

	p := PatternOffset;

	while True do
	begin
		chnNum := p^;
		Inc(p);
		if chnNum = 0 then Break; // No more!

		hc := HostChannels[(chnNum and $7F)-1];

		if (chnNum and $80) <> 0 then
		begin
			hc.NotePackMask := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 1) <> 0 then
		begin
			hc.RawNote := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 2) <> 0 then
		begin
			hc.Ins := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 4) <> 0 then
		begin
			hc.RawVolColumn := p^;
			Inc(p);
		end;

		if (hc.NotePackMask and 8) <> 0 then
		begin
			hc.Cmd    := p^;
			hc.OldCmd := p^;
			Inc(p);
			hc.CmdVal    := p^;
			hc.OldCmdVal := p^;
			Inc(p);
		end
		else
		if (hc.NotePackMask and 128) <> 0 then
		begin
			hc.Cmd    := hc.OldCmd;
			hc.CmdVal := hc.OldCmdVal;
		end
		else
		begin
			hc.Cmd    := 0;
			hc.CmdVal := 0;
		end;

		PreInitCommand(hc);
	end;

	PatternOffset := p;
end;

// 8bb: I only ported the logic for "Play Song" mode (mode=2)
//
procedure TITModule.UpdateData;
var
	hc: THostChannel;
	i: Integer;
	NewRow, NewOrder: Word;
	NewPattern, OldNotePackMask: Byte;
begin
	Dec(ProcessTick);
	Dec(CurrentTick);

	if CurrentTick = 0 then
	begin
		CurrentTick := CurrentSpeed;
		ProcessTick := CurrentSpeed;
		Dec(RowDelay);

		if RowDelay = 0 then
		begin
			RowDelay := 1;
			RowDelayOn := false;
			NewRow := ProcessRow + 1;

			if NewRow >= NumberOfRows then
			begin
				NewOrder := ProcessOrder + 1;

				while True do
				begin
					if NewOrder >= 256 then
					begin
						NewOrder := 0;
						Continue;
					end;

					NewPattern := Orders[NewOrder]; // next pattern
					if NewPattern >= 200 then
					begin
						if NewPattern = $FE then // 8bb: skip pattern separator
							Inc(NewOrder)
						else
						begin
							NewOrder := 0;
							StopSong := True; // 8bb: for WAV rendering
						end;
					end
					else
					begin
						CurrentPattern := NewPattern;
						Break;
					end;
				end;

				CurrentOrder := NewOrder;
				ProcessOrder := NewOrder;
				NewRow := BreakRow;
				BreakRow := 0;
			end;

			CurrentRow := NewRow;
			ProcessRow := NewRow;
			UpdateNoteData;
		end
		else
		begin
			for i := 0 to MAX_HOST_CHANNELS-1 do
			begin
				hc := HostChannels[i];
				if (not hc.Flags.HF_ROW_UPDATED) or (hc.NotePackMask and $88 = 0) then
					Continue;

				OldNotePackMask := hc.NotePackMask;
				hc.NotePackMask := hc.NotePackMask and $88;
				InitCommandTable[hc.Cmd and 31](hc);
				hc.NotePackMask := OldNotePackMask;
			end;
		end;
	end
	else
	begin
		// OK. call update command.
		for i := 0 to MAX_HOST_CHANNELS-1 do
		begin
			hc := HostChannels[i];
			if (hc.Flags.HF_CHAN_ON) and (hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON) then
				VolumeEffectTable[hc.VolCmd and 7](hc);
			if (hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON or hc.Flags.HF_ALWAYS_UPDATE_EFX) and
				(hc.Flags.HF_ALWAYS_UPDATE_EFX or hc.Flags.HF_CHAN_ON) then
				CommandTable[hc.Cmd and 31](hc);
		end;
	end;
end;

function TITModule.FindFreeSlaveChannel: TSlaveChannel;
var
	i: Integer;
	sc: TSlaveChannel;
begin
	// find a free slave channel
	for i := 0 to MAX_SLAVE_CHANNELS-1 do
	begin
		sc := SlaveChannels[i];
		if (sc <> nil) and (not sc.Flags.SF_CHAN_ON) then
			Exit(sc);
	end;

	Result := nil;
end;

procedure TITModule.UpdateAutoVibrato(sc: TSlaveChannel);
var
	VibratoData: Int16;
	smp: TSample;
begin
	smp := sc.Sample;
	Assert(smp <> nil);

	if smp.AutoVibratoDepth = 0 then
		Exit;

	sc.AutoVibratoDepth += smp.AutoVibratoRate;
	if (sc.AutoVibratoDepth >> 8) > smp.AutoVibratoDepth then
		sc.AutoVibratoDepth := (smp.AutoVibratoDepth << 8) or (sc.AutoVibratoDepth and $FF);

	if smp.AutoVibratoSpeed = 0 then
		Exit;

	if smp.AutoVibratoWaveform = 3 then
		VibratoData := (RandomNumber and 127) - 64
	else
	begin
		sc.AutoVibratoPos += smp.AutoVibratoSpeed; // Update pointer
		Assert(smp.AutoVibratoWaveform < 3);
		VibratoData := FineSineData[(smp.AutoVibratoWaveform << 8) + sc.AutoVibratoPos];
	end;

	VibratoData := SarSmallInt(VibratoData * (sc.AutoVibratoDepth div 256), 6);
	if VibratoData <> 0 then
		PitchSlideLinear(sc.HostChannel, sc, VibratoData);
end;

procedure TITModule.UpdateSamples;
var
	i: Integer;
	p: Integer;
	volume: Word;
	sc: TSlaveChannel;
begin
	for i := 0 to Driver.NumChannels-1 do
	begin
		sc := SlaveChannels[i];
		if (sc = nil) or (not sc.Flags.SF_CHAN_ON) then
			Continue;

		if sc.Flags.SF_RECALC_VOL then // 8bb: recalculate volume
		begin
			sc.Flags.SF_RECALC_VOL      := False;
			sc.Flags.SF_UPDATE_MIXERVOL := True;

			volume := (((sc.Vol * sc.ChnVol * sc.SmpVol) >> 4) * GlobalVolume) >> 7;
			Assert(volume <= 32768);

			sc.FinalVol32768 := volume;      // 8bb: 0..32768
			sc.FinalVol128   := volume >> 8; // 8bb: 0..128
		end;

		if sc.Flags.SF_RECALC_PAN then // 8bb: recalculate panning
		begin
			sc.Flags.SF_RECALC_PAN  := False;
			sc.Flags.SF_PAN_CHANGED := True;

			if sc.Pan = PAN_SURROUND  then
				sc.FinalPan := sc.Pan
			else
			begin
				p := Int8(sc.Pan) - 32;
				p *= (Header.PanSep div 2);
				sc.FinalPan := p >> 6 + 32; // 8bb: 0..64
				Assert(sc.FinalPan <= 64);
			end;
		end;

		UpdateAutoVibrato(sc);
	end;
end;

function TITModule.RandomNumber: Byte;
var
	r1, r2, r3, r4: Word;
begin
	r1 := RandSeed1; r2 := RandSeed2;
	r3 := RandSeed2; r4 := RandSeed2;

	r1 += r2;
	r1 := (r1 << (r3 and 15)) or (r1 >> ((16-r3) and 15));
	r1 := r1 xor r4;
	r3 := (r3 >> 8) or (r3 << 8);
	r2 += r3;
	r4 += r2;
	r3 += r1;
	r1 -= r4 + (r2 and 1);
	r2 := (r2 << 15) or (r2 >> 1);

	RandSeed2 := r4;
	RandSeed1 := r1;

	Result := r1 and $FF; // ?
end;

procedure TITModule.GetLoopInformation(sc: TSlaveChannel);
var
	LoopMode: Byte;
	LoopBegin, LoopEnd: Cardinal;
	LoopEnabled, SustainLoopOnlyAndNoteOff: Boolean;
	S: TSample;
begin
	S := sc.Sample;
	Assert(S <> nil);

	LoopEnabled := (S.Flags.SMPF_USE_LOOP) or (S.Flags.SMPF_USE_SUSTAINLOOP);
	SustainLoopOnlyAndNoteOff := (S.Flags.SMPF_USE_SUSTAINLOOP) and
		(sc.Flags.SF_NOTE_OFF) and (not S.Flags.SMPF_USE_LOOP);

	if (not LoopEnabled) or (SustainLoopOnlyAndNoteOff) then
	begin
		LoopBegin := 0;
		LoopEnd   := S.Length;
		LoopMode  := 0;
	end
	else
	begin
		LoopBegin := S.LoopBegin;
		LoopEnd   := S.LoopEnd;
		LoopMode  := S.Flags.ByteAccess;

		if S.Flags.SMPF_USE_SUSTAINLOOP then
		begin
			if not sc.Flags.SF_NOTE_OFF then // 8bb: sustain on (note not released)?
			begin
				LoopBegin := S.SustainLoopBegin;
				LoopEnd   := S.SustainLoopEnd;
				LoopMode  := LoopMode >> 1; // 8bb: loop mode := sustain loop mode
			end;
		end;

		LoopMode := IfThen(LoopMode and SMPF_LOOP_PINGPONG <> 0,
			LOOP_PINGPONG, LOOP_FORWARDS); // 8bb: set loop type (Ping-Pong or Forwards)
	end;

	// 8bb: if any parameter changed, update all
	if (sc.LoopMode <> LoopMode) or (sc.LoopBegin <> LoopBegin) or (sc.LoopEnd <> LoopEnd) then
	begin
		sc.LoopMode  := LoopMode;
		sc.LoopBegin := LoopBegin;
		sc.LoopEnd   := LoopEnd;
		sc.Flags.SF_LOOP_CHANGED := True;
		// 8bb: for my high quality mixer
		if sc.SamplingPosition < sc.LoopBegin then
			sc.HasLooped := False;
	end;
end;

procedure TITModule.RecalculateAllVolumes;
var
	i: Integer;
begin
	for i := 0 to Driver.NumChannels-1 do
	with SlaveChannels[i].Flags do
	begin
		SF_RECALC_PAN := True;
		SF_RECALC_VOL := True;
	end;
end;

procedure TITModule.ApplyRandomValues(hc: THostChannel);
var
	value: Int8;
	vol, pan: Int16;
	sc: TSlaveChannel;
	ins: TInstrument;
begin
	sc := hc.SlaveChannel;
	ins := sc.Instrument;
	value := Int8(RandomNumber); // -128.+127
	hc.Flags.HF_APPLY_RANDOM_VOL := False;

	if ins.RandVol <> 0 then // Random volume, 0.100
	begin
		vol := SarSmallint(ins.RandVol * value, 6) + 1;
		vol := sc.SmpVol + ((vol * sc.SmpVol) div 199);
		if vol < 0   then vol := 0 else
		if vol > 128 then vol := 128;
		sc.SmpVol := vol;
	end;

	value := Int8(RandomNumber); // -128.+127
	if (ins.RandPan <> 0) and (sc.Pan <> PAN_SURROUND) then // Random pan, 0.64
	begin
		pan := sc.Pan + SarSmallint(ins.RandPan * value, 7);
		if pan < 0  then pan := 0 else
		if pan > 64 then pan := 64;
		sc.Pan    := pan;
		sc.PanSet := pan;
	end;
end;

function TITModule.AllocateChannelSample(hc: THostChannel; var hcFlags: THostChannelFlags): TSlaveChannel;
var
	sc: TSlaveChannel;
	Sam: TSample;
begin
	Result := nil;
	sc := SlaveChannels[hc.HostChnNum];

	// copy out channel
	if (Driver.Flags.DF_USES_VOLRAMP) and (sc.Flags.SF_CHAN_ON) then
	begin
		sc.Flags.SF_NOTE_STOP := True;
		sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED;
		SlaveChannels[MAX_HOST_CHANNELS-1].CopyFrom(sc);
	end;

	hc.SlaveChannel := sc;
	sc.HostChannel  := hc;
	sc.HostChnNum   := hc.HostChnNum;

	sc.ChnVol := hc.ChnVol;
	sc.Pan    := hc.ChnPan;
	sc.PanSet := hc.ChnPan;
	sc.FadeOut := 1024;
	sc.VolEnvState.Value := (64 << 16) or (sc.VolEnvState.Value and $FFFF); // !!! 8bb: keeps frac
	sc.MIDIBank := $00FF; // Filter cutoff
	sc.Note := hc.RawNote;
	sc.Ins := hc.Ins;

	sc.Flags.WordAccess := 0;
	sc.Flags.SF_CHAN_ON := True;
	sc.Flags.SF_RECALC_PAN := True;
	sc.Flags.SF_RECALC_VOL := True;
	sc.Flags.SF_FREQ_CHANGE := True;
	sc.Flags.SF_NEW_NOTE := True;

	if hc.Smp > 0 then
	begin
		sc.Smp := hc.Smp - 1;
		Sam := Samples[sc.Smp];
		sc.Sample := Sam;

		sc.AutoVibratoDepth := 0; sc.AutoVibratoPos := 0;
		sc.PanEnvState.Value   := sc.PanEnvState.Value   and $FFFF; // !!! No pan deviation (8bb: keeps frac)
		sc.PitchEnvState.Value := sc.PitchEnvState.Value and $FFFF; // !!! No pitch deviation (8bb: keeps frac)
		sc.LoopDirection := DIR_FORWARDS; // Reset loop dir

		if (Sam = nil) or (Sam.Length = 0) or (not Sam.Flags.SMPF_ASSOCIATED_WITH_HEADER) then
		begin
			sc.SmpIs16Bit := False;
			sc.Flags.WordAccess := 0;
			sc.Flags.SF_NOTE_STOP := True;
			hcFlags.HF_CHAN_ON := False;
			Exit;
		end;

		sc.SmpIs16Bit := Sam.Flags.SMPF_16BIT;
		sc.SmpVol := Sam.GlobVol * 2;
		Result := sc;
	end
	else // No sample!
	begin
		sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
		hcFlags.HF_CHAN_ON := False;
	end;
end;

function TITModule.AllocateChannel(hc: THostChannel; var hcFlags: THostChannelFlags): TSlaveChannel;
var
	i, j: Integer;
	hostChnNum, NNA, DCT, DCVal, count, lowestVol, targetSmp, scSmp: Byte;
	scInitialized, skipMIDITest, doDupeCheck: Boolean;
	sc, scTmp, scTmp2: TSlaveChannel;
	ins: TInstrument;
begin
	LastSlaveChannel := nil;

	if (not Header.Flags.ITF_INSTR_MODE) or (hc.Ins = 255) then
		Exit(AllocateChannelSample(hc, hcFlags));

	// Instrument handler!

	if hc.Ins = 0 then
		Exit(nil);

	if (hc.Smp = 101) and (Driver.NumChannels < MAX_SLAVE_CHANNELS) then // 8bb: MIDI and below 256 virtual channels in driver?
	begin
		AllocateNumChannels := MAX_SLAVE_CHANNELS - Driver.NumChannels;
		AllocateSlaveOffset := Driver.NumChannels;
	end
	else
	begin
		AllocateNumChannels := Driver.NumChannels;
		AllocateSlaveOffset := 0; // 8bb: points to first virtual channel
	end;

	// 8bb: some of these are initialized only to prevent compiler warnings
	sc := nil;
	NNA := 0;
	ins := Instruments[hc.Ins-1];

	scInitialized := False;

	if hcFlags.HF_CHAN_ON then // 8bb: host channel on?
	begin
		sc := hc.SlaveChannel;
		if sc.Instrument = ins then // 8bb: slave channel has same inst. as host channel?
			LastSlaveChannel := sc;

		NNA := sc.NNA;
		if NNA <> NNA_NOTE_CUT then // 8bb: not note-cut
			sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED; // Disown channel

		scInitialized := True;
	end;

	while True do // New note action handling...
	begin
		skipMIDITest := False;
		if scInitialized then
		begin
			if (NNA <> NNA_NOTE_CUT) and (sc.VolSet > 0) and (sc.ChnVol > 0) and (sc.SmpVol > 0) then
			begin
				if NNA = NNA_NOTE_OFF then
				begin
					sc.Flags.SF_NOTE_OFF := True;
					GetLoopInformation(sc); // 8bb: update sample loop (sustain released)
				end
				else
				if NNA >= NNA_NOTE_FADE then
					sc.Flags.SF_FADEOUT := True;
				// 8bb: else: NNA_CONTINUE
			end
			else
			begin
				// 8bb: NNA=Note Cut (or volumes are zero)
				if sc.Smp = 100 then // MIDI?
				begin
					sc.Flags.SF_NOTE_STOP := True;
					sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED; // Disown channel
					if hc.Smp <> 101 then
						Break; // Sample.. (8bb: find available voice now)
				end
				else
				begin
					if Driver.Flags.DF_USES_VOLRAMP then
					begin
						sc.Flags.SF_NOTE_STOP := True;
						sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED; // Disown channel
						Break; // 8bb: find available voice now
					end;

					sc.Flags.WordAccess := 0;
					sc.Flags.SF_NOTE_STOP := True;
					if ins.DCT = DCT_DISABLED then
						Exit(AllocateChannelInstrument(hc, sc, ins, hcFlags));

					skipMIDITest := True;
				end;
			end;
		end;

		hostChnNum := 0;
		DCT   := 0;
		DCVal := 0;
		doDupeCheck := False;

		if (not skipMIDITest) and (hc.Smp = 101) then
		begin
			// 8bb: MIDI note, do a "duplicate note" check regardless of instrument's DCT setting
			hostChnNum := hc.MIDIChn;
			DCT := DCT_NOTE;
			DCVal := hc.TranslatedNote;
			doDupeCheck := True;
		end
		else
		if ins.DCT <> DCT_DISABLED then
		begin
			hostChnNum := hc.HostChnNum or CHN_DISOWNED; // 8bb: only search disowned host channels
			DCT := ins.DCT;

			if ins.DCT = DCT_NOTE then
				DCVal := hc.RawNote // 8bb: yes, raw note, not the translated notenot
			else
			if ins.DCT = DCT_INSTRUMENT then
				DCVal := hc.Ins
			else // 8bb: DCT_SAMPLE (or any other number, like DCT=4 from OpenMPT, which is unsupported)
			begin
				DCVal := hc.Smp - 1;
				if Int8(DCVal) < 0 then
					Break; // 8bb: illegal (or no) sample, ignore dupe test and find available voice now
			end;
			doDupeCheck := True;
		end;

		if doDupeCheck then // 8bb: NNA Duplicate Check
		begin
			sc := SlaveChannels[AllocateSlaveOffset];
			if DuplicateCheck(sc, hc, hostChnNum, ins, DCT, DCVal) then
			begin
				// 8bb: dupe found!
				scInitialized := True; // 8bb: we have an sc pointer now (we could come from a shutdown host channel)
				if ins.DCA = DCA_NOTE_CUT then
					NNA := NNA_NOTE_CUT
				else
				begin
					sc.DCT := DCT_DISABLED; // 8bb: turn off dupe check to prevent further NNA testing
					sc.DCA := DCA_NOTE_CUT;
					NNA := ins.DCA + 1;
				end;
				Continue; // 8bb: do another NNA test with the new NNA type
			end;
		end;

		Break; // NNA handling done, find available voice now
	end;

	// 8bb: search for inactive channels

	if hc.Smp <> 101 then
	begin
		// 8bb: no MIDI
		for i := 0 to AllocateNumChannels-1 do
		begin
			sc := SlaveChannels[AllocateSlaveOffset+i];
			if not sc.Flags.SF_CHAN_ON then
				Exit(AllocateChannelInstrument(hc, sc, ins, hcFlags));
		end;
	end
	else
	begin
		// MIDI 'slave channels' have to be maintained if still referenced
		for i := 0 to AllocateNumChannels-1 do
		begin
			sc := SlaveChannels[AllocateSlaveOffset+i];
			if not sc.Flags.SF_CHAN_ON then
			begin
				// Have a channel.. check that it's host's slave isn't SI (8bb: SI := sc)
				if (sc.HostChannel = nil) or (sc.HostChannel.SlaveChannel <> sc) then
					Exit(AllocateChannelInstrument(hc, sc, ins, hcFlags));
			end;
		end;
	end;

	// Common sample search

	FillByte(ChannelCountTable[0],    Length(ChannelCountTable),    0);
	FillByte(ChannelVolumeTable[0],   Length(ChannelVolumeTable),   255);
	FillByte(ChannelLocationTable[0], Length(ChannelLocationTable), 0);

	for i := 0 to AllocateNumChannels-1 do
	begin
		sc := SlaveChannels[AllocateSlaveOffset+i];
		if sc.Smp > 99 then Continue; // Just for safety

		ChannelCountTable[sc.Smp] += 1;
		if (sc.HostChnNum and CHN_DISOWNED <> 0) and (sc.FinalVol128 < ChannelVolumeTable[sc.Smp]) then
		begin
			ChannelLocationTable[sc.Smp] := sc;
			ChannelVolumeTable  [sc.Smp] := sc.FinalVol128;
		end;
	end;

	// OK.. now search table for maximum occurrence of sample...

	sc := nil;
	count := 2; // Find maximum count, has to be greater than 2 channels
	for i := 0 to 99 do
	begin
		if ChannelCountTable[i] > count then
		begin
			count := ChannelCountTable[i];
			sc := ChannelLocationTable[i];
		end;
	end;
	if sc <> nil then
		Exit(AllocateChannelInstrument(hc, sc, ins, hcFlags));

	// Find out which host channel has the most (disowned) slave channels.
	// Then find the softest non-single sample in that channel.
	//
	FillByte(ChannelCountTable[0], MAX_HOST_CHANNELS, 0);

	for i := 0 to AllocateNumChannels-1 do
	begin
		sc := SlaveChannels[AllocateSlaveOffset+i];
		ChannelCountTable[sc.HostChnNum and 63] += 1;
	end;

	// OK.. search through and find the most heavily used channel

	while True do
	begin
		hostChnNum := 0;
		count := 1;

		for i := 0 to MAX_HOST_CHANNELS-1 do
		begin
			if ChannelCountTable[i] > count then
			begin
				count := ChannelCountTable[i];
				hostChnNum := i;
			end;
		end;

		if count <= 1 then
		begin
			// Now search for softest disowned sample (not non-single)
			sc := nil;
			lowestVol := 255;

			for i := 0 to AllocateNumChannels-1 do
			begin
				scTmp := SlaveChannels[AllocateSlaveOffset+i];
				if ((scTmp.HostChnNum and CHN_DISOWNED) <> 0) and (scTmp.FinalVol128 <= lowestVol) then
				begin
					sc := scTmp;
					lowestVol := scTmp.FinalVol128;
				end;
			end;

			if sc = nil then
			begin
				hcFlags.HF_CHAN_ON := False;
				Exit(nil);
			end;

			Exit(AllocateChannelInstrument(hc, sc, ins, hcFlags));
		end;

		hostChnNum := hostChnNum or CHN_DISOWNED; // Search for disowned only
		sc := nil; // Offset
		lowestVol := 255;
		targetSmp := hc.Smp-1;

		for i := 0 to AllocateNumChannels-1 do
		begin
			scTmp := SlaveChannels[AllocateSlaveOffset+i];
			if (scTmp.HostChnNum <> hostChnNum) or (scTmp.FinalVol128 >= lowestVol) then
				Continue;

			// Now check if any other channel contains this sample

			if scTmp.Smp = targetSmp then
			begin
				sc := scTmp;
				lowestVol := scTmp.FinalVol128;
				Continue;
			end;


			scSmp := scTmp.Smp;
			scTmp.Smp := 255;

			for j := 0 to AllocateNumChannels-1 do
			begin
				scTmp2 := SlaveChannels[AllocateSlaveOffset+j];
				if (scTmp2.Smp = targetSmp) or (scTmp2.Smp = scSmp) then
				begin
					// OK found a second sample.
					sc := scTmp;
					lowestVol := scTmp.FinalVol128;
					Break;
				end;
			end;
			scTmp.Smp := scSmp;
		end;

		if sc <> nil then Break; // done!

		ChannelCountTable[hostChnNum and 63] := 0; // Next cycle...
	end;

	// 8bb: we have a target slave channel in sc at this point

	lowestVol := 255;

	for i := 0 to AllocateNumChannels-1 do
	begin
		scTmp := SlaveChannels[AllocateSlaveOffset+i];
		if (scTmp.Smp = sc.Smp) and (scTmp.HostChnNum and CHN_DISOWNED <> 0) and (scTmp.FinalVol128 < lowestVol) then
		begin
			sc := scTmp;
			lowestVol := scTmp.FinalVol128;
		end;
	end;

	Result := AllocateChannelInstrument(hc, sc, ins, hcFlags);
end;

procedure TITModule.InitTempo;
begin
	if Driver <> nil then
		Driver.SetTempo(Tempo);
end;

procedure TITModule.NoCommand(hc: THostChannel);
begin
	// do nothing!
end;

procedure TITModule.VolumeCommandC(hc: THostChannel);
var
	vol: Int8;
	sc: TSlaveChannel;
begin
	sc := hc.SlaveChannel;

	vol := sc.VolSet + hc.VolCmdVal;
	if vol > 64 then
	begin
		hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON := False; // Turn off effect calling
		vol := 64;
	end;

	CommandD2(hc, sc, vol);
end;

procedure TITModule.CommandD2(hc: THostChannel; sc: TSlaveChannel; vol: Byte);
begin
	sc.Vol    := vol;
	sc.VolSet := vol;
	hc.VolSet := vol;
	sc.Flags.SF_RECALC_VOL := True;
end;

procedure TITModule.VolumeCommandD(hc: THostChannel);
var
	sc: TSlaveChannel;
	vol: Int8;
begin
	sc := hc.SlaveChannel;

	vol := sc.VolSet - hc.VolCmdVal;
	if vol < 0 then
	begin
		hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON := False; // Turn off effect calling
		vol := 0;
	end;

	CommandD2(hc, sc, vol);
end;

procedure TITModule.VolumeCommandE(hc: THostChannel);
begin
	CommandEChain(hc, hc.EfxMem_EFG << 2);
end;

procedure TITModule.VolumeCommandF(hc: THostChannel);
begin
	CommandFChain(hc, hc.EfxMem_EFG << 2);
end;

procedure TITModule.VolumeCommandG(hc: THostChannel);
var
	SlideValue: Int16;
	sc: TSlaveChannel;

	procedure ClearFlags;
	begin
		hc.Flags.HF_PITCH_SLIDE_ONGOING      := False;
		hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON := False;
	end;

begin
	if not hc.Flags.HF_PITCH_SLIDE_ONGOING then Exit;

	SlideValue := IfThen(Header.Flags.ITF_COMPAT_GXX,
		hc.EfxMem_G_Compat, hc.EfxMem_EFG) * 4; //<< 2;
	if SlideValue = 0 then Exit;

	sc := hc.SlaveChannel;

	if hc.MiscEfxData[2] = 1 then // 8bb: slide up?
	begin
		PitchSlide(hc, sc, SlideValue);
		sc.FrequencySet := sc.Frequency;

		if (sc.Flags.SF_NOTE_STOP) or (sc.Frequency >= hc.PortaFreq) then
		begin
			sc.Flags.SF_NOTE_STOP := False;
			hc.Flags.HF_CHAN_ON   := True; // Turn on

			sc.Frequency    := hc.PortaFreq;
			sc.FrequencySet := hc.PortaFreq;
			ClearFlags;
		end;
	end
	else // 8bb: slide down
	begin
		PitchSlide(hc, sc, -SlideValue);

		if sc.Frequency <= hc.PortaFreq then
		begin
			sc.Frequency := hc.PortaFreq;
			ClearFlags;
		end;

		sc.FrequencySet := sc.Frequency;
	end;
end;

procedure TITModule.InitVolumeEffect(hc: THostChannel);
var
	cmd, val: Byte;
	volCmd, vol: Int8;
	sc: TSlaveChannel;
begin
	if (hc.NotePackMask and $44) = 0 then Exit;

	volCmd := (hc.RawVolColumn and $7F) - 65;
	if volCmd < 0 then Exit;

	if (hc.RawVolColumn and $80) <> 0 then
		volCmd += 60;

	cmd := volCmd div 10;
	val := volCmd mod 10;

	hc.VolCmd := cmd; // Store effect number

	// Memory for effects A.D, (EFG)/H don't share.
	//
	// Effects Ax and Bx (fine volume slide up and down) require immediate
	// handling. No flags required. (effect 0 and 1)
	//
	// Effects Cx, Dx, Ex, Fx (volume/pitch slides) require flag to be set (effects 2, 5)
	//
	// Effects Gx and Hx need init (handling) code + flags. (effects 6, 7).
	//
	if val > 0 then
	begin
		if cmd < 4 then
			hc.VolCmdVal := val
		else
		if cmd < 6 then
			hc.EfxMem_EFG := val << 2
		else
		if cmd = 6 then
		begin
			if Header.Flags.ITF_COMPAT_GXX then
				hc.EfxMem_G_Compat := SlideTable[val-1]
			else
				hc.EfxMem_EFG := SlideTable[val-1];
		end;
	end;

	if hc.Flags.HF_CHAN_ON then
	begin
		sc := hc.SlaveChannel;

		if cmd > 1 then
		begin
			hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON := True;

			if cmd > 6 then
			begin
				if val <> 0 then
					hc.VibratoDepth := val << 2;
				if hc.Flags.HF_CHAN_ON then
					InitVibrato(hc);
			end
			else
			if cmd = 6 then
				InitCommandG11(hc);
		end
		else
		if cmd = 1 then
		begin
			// Fine volume slide down
			vol := Max(0, sc.VolSet - hc.VolCmdVal);
			CommandD2(hc, sc, vol);
		end
		else
		begin
			// Fine volume slide up
			vol := Min(64, sc.VolSet + hc.VolCmdVal);
			CommandD2(hc, sc, vol);
		end;
	end
	else
	begin
		// Channel not on
		if cmd = 7 then // Vibrato?
		begin
			if val <> 0 then
				hc.VibratoDepth := val << 2;
			if hc.Flags.HF_CHAN_ON then
				InitVibrato(hc);
		end;
	end;
end;

procedure TITModule.NoOldEffect(hc: THostChannel; constref hcFlags: THostChannelFlags);
var
	vol: Byte;
	sc: TSlaveChannel;
begin
	vol := hc.RawVolColumn;

	if ((hc.NotePackMask and $44) = 0) or (vol > 64) then
	begin
		if (hc.NotePackMask and $44 <> 0) and (vol and $7F < 65) then
		begin
			// Panning set!
			hc.Flags.WordAccess := (hc.Flags.WordAccess and $FF00) or (hcFlags.WordAccess and $00FF);
			InitCommandX2(hc, vol - 128);
		end;
		if (hc.NotePackMask and $22 = 0) or (hc.Smp = 0) then // Instrument present?
		begin
			InitNoCommand3(hc, hcFlags);
			Exit;
		end;

		vol := Samples[hc.Smp-1].Vol; // Default volume
	end;

	hc.VolSet := vol;

	if hcFlags.HF_CHAN_ON then
	begin
		sc := hc.SlaveChannel;
		sc.Vol    := vol;
		sc.VolSet := vol;
		sc.Flags.SF_RECALC_VOL := True;
	end;

	InitNoCommand3(hc, hcFlags);
end;

procedure TITModule.InitNoCommand11(hc: THostChannel; sc: TSlaveChannel; constref hcFlags: THostChannelFlags);
begin
	GetLoopInformation(sc);

	if (hc.NotePackMask and ($22+$44)) = 0 then
	begin
		InitNoCommand3(hc, hcFlags);
		Exit;
	end;

	if (Header.Flags.ITF_INSTR_MODE) and (Header.Flags.ITF_OLD_EFFECTS) then
	begin
		if (hc.Ins <> 255) and (hc.NotePackMask and $22 <> 0) then
		begin
			sc.FadeOut := 1024;
			InitPlayInstrument(hc, sc, Instruments[hc.Ins-1]);
		end;
	end;

	NoOldEffect(hc, hcFlags);
end;

procedure TITModule.InitNoCommand3(hc: THostChannel; constref hcFlags: THostChannelFlags);
var
	ApplyRandomVolume: Boolean;
begin
	// Randomise volume if required.
	ApplyRandomVolume := hc.Flags.HF_APPLY_RANDOM_VOL;

	hc.Flags.WordAccess := (hc.Flags.WordAccess and $FF00) or (hcFlags.WordAccess and $00FF);

	if ApplyRandomVolume then
		ApplyRandomValues(hc);

	InitVolumeEffect(hc);
end;

procedure TITModule.InitNoCommand(hc: THostChannel);
var
	hcFlags: THostChannelFlags;
	sc: TSlaveChannel;
	S: TSample;
	volColumnPortamento: Boolean;
begin
	hcFlags.WordAccess := hc.Flags.WordAccess and $00FF;

	if (hc.NotePackMask and $33) = 0 then
	begin
		NoOldEffect(hc, hcFlags);
		Exit;
	end;

	// Note here! Check for noteoff.
	if hc.TranslatedNote >= 120 then
	begin
		if hcFlags.HF_CHAN_ON then
		begin
			sc := hc.SlaveChannel;

			if hc.TranslatedNote = 255 then // 8bb: note off
			begin
				sc.Flags.SF_NOTE_OFF := True;
				InitNoCommand11(hc, sc, hcFlags);
				Exit;
			end
			else
			if hc.TranslatedNote = 254 then // 8bb: note cut
			begin
				hcFlags.HF_CHAN_ON := False;
				if (sc.Smp = 100) or (Driver.Flags.DF_USES_VOLRAMP) then
				else
					sc.Flags.WordAccess := 0;
				sc.Flags.SF_NOTE_STOP := True;
			end
			else // 8bb: note fade (?)
				sc.Flags.SF_FADEOUT := True;
		end;

		NoOldEffect(hc, hcFlags);
		Exit;
	end;

	if hcFlags.HF_CHAN_ON then
	begin
		sc := hc.SlaveChannel;
		if (hc.NotePackMask and $11 = 0) and (sc.Note = hc.RawNote) and (sc.Ins = hc.Ins) then
		begin
			NoOldEffect(hc, hcFlags);
			Exit;
		end;
	end;

	volColumnPortamento := (hc.RawVolColumn >= 193) and (hc.RawVolColumn <= 202);
	if (hc.NotePackMask and $44 <> 0) and (volColumnPortamento) and (hc.Flags.HF_CHAN_ON) then
	begin
		InitVolumeEffect(hc);
		Exit;
	end;

	sc := AllocateChannel(hc, hcFlags);
	if sc = nil then
	begin
		NoOldEffect(hc, hcFlags);
		Exit;
	end;

	// Channel allocated.

	S := sc.Sample;

	sc.Vol    := hc.VolSet;
	sc.VolSet := hc.VolSet;

	if not Header.Flags.ITF_INSTR_MODE then
	begin
		if (s.DefPan and $80) <> 0 then
		begin
			sc.Pan := S.DefPan and 127;
			hc.ChnPan := sc.Pan;
		end;
	end;

	sc.SamplingPosition := 0;
	sc.Frac32 := 0; // 8bb: clear fractional sampling position
	sc.Frac64 := 0; // 8bb: also clear frac for my high-quality driver/mixer
	sc.HasLooped := False; // 8bb: for my high-quality driver/mixer
	sc.Frequency := Int32(s.C5Speed * PitchTable[hc.TranslatedNote] >> 16);
	sc.FrequencySet := sc.Frequency;

	hcFlags.HF_CHAN_ON := True;
	hcFlags.HF_PITCH_SLIDE_ONGOING := False;

	InitNoCommand11(hc, sc, hcFlags);
end;

procedure TITModule.InitCommandEorF(hc: THostChannel; SlideUp: Boolean = False);
var
	CmdVal: Byte;
	SlideVal: Word;
	ptr: PWord;
	sc: TSlaveChannel;
begin
	InitNoCommand(hc);

	CmdVal := hc.CmdVal;
	if CmdVal = 0 then
		CmdVal := hc.EfxMem_EFG;

	hc.EfxMem_EFG := CmdVal;

	if (not hc.Flags.HF_CHAN_ON) or (hc.EfxMem_EFG = 0) then
		Exit;

	if (hc.EfxMem_EFG and $F0) < $E0 then
	begin
		ptr := @hc.MiscEfxData[0];
		ptr^ := hc.EfxMem_EFG << 2;
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True; // call update only if necess.
		Exit;
	end;

	if (hc.EfxMem_EFG and $0F) = 0 then
		Exit;

	SlideVal := hc.EfxMem_EFG and $0F;
	if (hc.EfxMem_EFG and $F0) <> $E0 then
		SlideVal := SlideVal << 2;

	sc := hc.SlaveChannel;
	if SlideUp then
		PitchSlide(hc, sc, SlideVal)
	else
		PitchSlide(hc, sc, -SlideVal);
	sc.FrequencySet := sc.Frequency;
end;

procedure TITModule.CommandEChain(hc: THostChannel; SlideValue: Word);
var
	sc: TSlaveChannel;
begin
	sc := hc.SlaveChannel;
	PitchSlide(hc, sc, -SlideValue);
	sc.FrequencySet := sc.Frequency;
end;

procedure TITModule.CommandFChain(hc: THostChannel; SlideValue: Word);
var
	sc: TSlaveChannel;
begin
	sc := hc.SlaveChannel;
	PitchSlide(hc, sc, +SlideValue);
	sc.FrequencySet := sc.Frequency;
end;

procedure TITModule.CommandH5(hc: THostChannel; sc: TSlaveChannel; VibratoData: Int8);
begin
	VibratoData := ((VibratoData * hc.VibratoDepth * 4) + 128) div 256;
	if Header.Flags.ITF_OLD_EFFECTS then
		VibratoData := -VibratoData;
	PitchSlide(hc, sc, VibratoData);
end;

procedure TITModule.InitVibrato(hc: THostChannel);
begin
	if Header.Flags.ITF_OLD_EFFECTS then
	begin
		hc.SlaveChannel.Flags.SF_FREQ_CHANGE := True;
		CommandH5(hc, hc.SlaveChannel, hc.LastVibratoData);
	end
	else
		CommandH(hc);
end;

procedure TITModule.InitTremolo(hc: THostChannel);
var
	sc: TSlaveChannel;
begin
	if Header.Flags.ITF_OLD_EFFECTS then
	begin
		sc := hc.SlaveChannel;
		sc.Flags.SF_UPDATE_MIXERVOL := True; // Volume change...
		CommandR2(hc, sc, hc.LastTremoloData);
	end
	else
		CommandR(hc);
end;

function TITModule.Gxx_ChangeSample(hc: THostChannel; sc: TSlaveChannel; sample: Byte): Boolean;
var
	S: TSample;
begin
	sc.Flags.SF_NOTE_STOP := False;
	sc.Flags.SF_LOOP_CHANGED := False;
	sc.Flags.SF_CHN_MUTED := False;
	sc.Flags.SF_VOLENV_ON := False;
	sc.Flags.SF_PANENV_ON := False;
	sc.Flags.SF_PITCHENV_ON := False;
	sc.Flags.SF_PAN_CHANGED := False;
	sc.Flags.SF_NEW_NOTE := True;

	// Now to update sample info.
	s := Samples[sample];
	sc.Sample := s;
	sc.AutoVibratoDepth := 0;
	sc.LoopDirection := 0;
	sc.Frac32 := 0; // 8bb: reset sampling position fraction
	sc.Frac64 := 0; // 8bb: also clear frac for my high-quality driver/mixer
	sc.HasLooped := false; // 8bb: for my high-quality driver/mixer
	sc.SamplingPosition := 0;
	sc.SmpVol := s.GlobVol * 2;

	if not S.Flags.SMPF_ASSOCIATED_WITH_HEADER then
	begin
		// 8bb: turn off channel
		sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
		hc.Flags.HF_CHAN_ON := False;
		Exit(False);
	end;

	sc.SmpIs16Bit := s.Flags.SMPF_16BIT;
	GetLoopInformation(sc);

	Result := True;
end;

procedure TITModule.InitCommandX2(hc: THostChannel; pan: Byte); // 8bb: pan := 0..63
var
	sc: TSlaveChannel;
begin
	if hc.Flags.HF_CHAN_ON then
	begin
		sc := hc.SlaveChannel;
		sc.Pan    := pan;
		sc.PanSet := pan;
		sc.Flags.SF_RECALC_PAN      := True;
		sc.Flags.SF_UPDATE_MIXERVOL := True;
	end;

	hc.ChnPan := pan;
end;

// Jumped to from Lxx (8bb: and normal tone portamento)

procedure TITModule.InitCommandG11(hc: THostChannel);
var
	sc: TSlaveChannel;
	ins: TInstrument;
	s: TSample;
	ChangeInstrument, volFromVolColumn: Boolean;
	vol, hcSmp, oldSlaveIns: Byte;
	oldSCFlags, SlideSpeed: Word;
	ptr: PWord;
begin
	sc := hc.SlaveChannel;

	if (hc.NotePackMask and $22 <> 0) and (hc.Smp > 0) then
	begin
		// Checking for change of sample or instrument
		ChangeInstrument := False;

		if Header.Flags.ITF_COMPAT_GXX then
		begin
			hc.Smp := sc.Smp+1;
			sc.SmpVol := Samples[sc.Smp].GlobVol * 2;
			ChangeInstrument := True;
		end
		else
		if hc.Smp <> 101 then // Don't overwrite note if MIDI!
		begin
			hcSmp := hc.Smp-1;
			oldSlaveIns := sc.Ins;
			sc.Note := hc.RawNote;
			sc.Ins := hc.Ins;

			if sc.Ins <> oldSlaveIns then // Ins the same?
			begin
				if sc.Smp <> hcSmp then // Sample the same?
				begin
					if not Gxx_ChangeSample(hc, sc, hcSmp) then
						Exit; // 8bb: sample was not assciated with sample header
				end;
				ChangeInstrument := True;
			end
			else
			if sc.Smp <> hcSmp then
			begin
				if not Gxx_ChangeSample(hc, sc, hcSmp) then
					Exit; // 8bb: sample was not assciated with sample header
				ChangeInstrument := True;
			end;
		end;

		if (Header.Flags.ITF_INSTR_MODE) and (ChangeInstrument) then
		begin
			// Now for instruments
			ins := Instruments[hc.Ins-1];
			sc.FadeOut := 1024;
			oldSCFlags := sc.Flags.WordAccess;
			InitPlayInstrument(hc, sc, ins);

			if (oldSCFlags and 1{SF_CHAN_ON}) = 0 then
				sc.Flags.SF_NEW_NOTE := False;

			sc.SmpVol := (ins.GlobVol * sc.SmpVol) >> 7;
		end;
	end;

	if (Header.Flags.ITF_INSTR_MODE) or (hc.NotePackMask and $11 <> 0) then
	begin
		// OK. Time to calc freq.

		if hc.TranslatedNote < 120 then
		begin
			// Don't overwrite note if MIDI!
			if hc.Smp <> 101 then
				sc.Note := hc.TranslatedNote;

			s := sc.Sample;

			hc.PortaFreq := Int32(s.C5Speed * PitchTable[hc.TranslatedNote]) >> 16; // !!! cast?
			hc.Flags.HF_PITCH_SLIDE_ONGOING := True;
		end
		else
		if hc.Flags.HF_CHAN_ON then
		begin
			if hc.TranslatedNote = 255 then
			begin
				sc.Flags.SF_NOTE_OFF := True;
				GetLoopInformation(sc);
			end
			else
			if hc.TranslatedNote = 254 then
			begin
				hc.Flags.HF_CHAN_ON := False;
				sc.Flags.WordAccess := 0;
				sc.Flags.SF_NOTE_STOP := True;
			end
			else
				sc.Flags.SF_FADEOUT := True;
		end;
	end;

	volFromVolColumn := False;
	vol := 0;

	if hc.NotePackMask and $44 <> 0 then
	begin
		if hc.RawVolColumn <= 64 then
		begin
			vol := hc.RawVolColumn;
			volFromVolColumn := True;
		end
		else
		if (hc.RawVolColumn and $7F) < 65 then
			InitCommandX2(hc, hc.RawVolColumn - 128);
	end;

	if (volFromVolColumn) or (hc.NotePackMask and $22 <> 0) then
	begin
		if not volFromVolColumn then
			vol := sc.Sample.Vol;
		sc.Flags.SF_RECALC_VOL := True;
		sc.Vol    := vol;
		sc.VolSet := vol;
		hc.VolSet := vol;
	end;

	if hc.Flags.HF_PITCH_SLIDE_ONGOING then // Slide on?
	begin
		// Work out magnitude + dir
		if Header.Flags.ITF_COMPAT_GXX then
			SlideSpeed := hc.EfxMem_G_Compat * 4
		else
			SlideSpeed := hc.EfxMem_EFG * 4;

		if SlideSpeed > 0 then
		begin
			ptr := @hc.MiscEfxData[0];
			ptr^ := SlideSpeed;

			if sc.FrequencySet <> hc.PortaFreq then
			begin
				// slide direction (0=down, 1=up)
				hc.MiscEfxData[2] := 1-BoolToInt[sc.FrequencySet > hc.PortaFreq];
				if not hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON then
					hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True; // Update effect if necessary
			end;
		end;
	end;

	// Don't call volume effects if it has a Gxx!
	if not hc.Flags.HF_UPDATE_VOLEFX_IF_CHAN_ON then
		InitVolumeEffect(hc);
end;

procedure TITModule.InitCommandA(hc: THostChannel);
begin
	if hc.CmdVal <> 0 then
	begin
		CurrentTick := (CurrentTick - CurrentSpeed) + hc.CmdVal;
		CurrentSpeed := hc.CmdVal;
	end;

	InitNoCommand(hc);
end;

procedure TITModule.InitCommandB(hc: THostChannel);
begin
	ProcessOrder := hc.CmdVal - 1;
	ProcessRow := $FFFE;

	InitNoCommand(hc);
end;

procedure TITModule.InitCommandC(hc: THostChannel);
begin
	if not PatternLooping then
	begin
		BreakRow := hc.CmdVal;
		ProcessRow := $FFFE;
	end;

	InitNoCommand(hc);
end;

procedure TITModule.InitCommandD(hc: THostChannel);
var
	CmdVal: Byte;
begin
	InitNoCommand(hc);

	CmdVal := hc.CmdVal;
	if CmdVal = 0 then
		CmdVal := hc.EfxMem_DKL;

	hc.EfxMem_DKL := CmdVal;

	if hc.Flags.HF_CHAN_ON then
		InitCommandD7(hc, hc.SlaveChannel);
end;

procedure TITModule.InitCommandE(hc: THostChannel);
begin
	InitCommandEorF(hc, False);
end;

procedure TITModule.InitCommandF(hc: THostChannel);
begin
	InitCommandEorF(hc, True);
end;

procedure TITModule.InitCommandG(hc: THostChannel);
begin
	if hc.CmdVal <> 0 then
	begin
		if Header.Flags.ITF_COMPAT_GXX then // Compatibility Gxx?
			hc.EfxMem_G_Compat := hc.CmdVal
		else
			hc.EfxMem_EFG := hc.CmdVal;
	end;

	if hc.Flags.HF_CHAN_ON then
		InitCommandG11(hc)
	else
		InitNoCommand(hc);
end;

procedure TITModule.InitCommandH(hc: THostChannel);
var
	speed, depth: Byte;
begin
	if (hc.NotePackMask and $11 <> 0) and (hc.RawNote < 120) then
	begin
		hc.VibratoPos := 0;
		hc.LastVibratoData := 0;
	end;

	speed := (hc.CmdVal >> 4) << 2;
	depth := (hc.CmdVal and $0F) << 2;

	if speed > 0 then
		hc.VibratoSpeed := speed;

	if depth > 0 then
	begin
		if Header.Flags.ITF_OLD_EFFECTS then
			depth *= 2;
		hc.VibratoDepth := depth;
	end;

	InitNoCommand(hc);

	if hc.Flags.HF_CHAN_ON then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True; // Update mode.
		InitVibrato(hc);
	end;
end;

procedure TITModule.InitCommandI(hc: THostChannel);
var
	CmdVal, OffTime, OnTime: Byte;
begin
	InitNoCommand(hc);

	CmdVal := hc.CmdVal;
	if CmdVal > 0 then
		hc.EfxMem_I := CmdVal;

	if hc.Flags.HF_CHAN_ON then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;

		OffTime := hc.EfxMem_I and $0F;
		OnTime  := hc.EfxMem_I >> 4;

		if Header.Flags.ITF_OLD_EFFECTS then
		begin
			OffTime += 1;
			OnTime  += 1;
		end;

		hc.MiscEfxData[0] := OffTime;
		hc.MiscEfxData[1] := OnTime;

		CommandI(hc);
	end;
end;

procedure TITModule.InitCommandJ(hc: THostChannel);
var
	ptr: PWord;
	CmdVal: Byte;
begin
	InitNoCommand(hc);

	ptr := @hc.MiscEfxData[0];
	ptr^ := 0; // 8bb: clear arp tick counter

	CmdVal := hc.CmdVal;
	if CmdVal = 0 then
		CmdVal := hc.EfxMem_J
	else
		hc.EfxMem_J := CmdVal;

	if not hc.Flags.HF_CHAN_ON then Exit;

	hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True; // Update when channel on

	// 8bb: Original code stores 16-bit PitchTable memory addresses here,
	// but we store notes instead because we work with bigger pointer sizes.
	// The outcome is the same.

	ptr := @hc.MiscEfxData[2];
	ptr^ := 60 + (hc.EfxMem_J >> 4);   // 8bb: Tick 1 note
	Inc(ptr);
	ptr^ := 60 + (hc.EfxMem_J and $0F); // 8bb: Tick 2 note
end;

// Jmp point for Lxx (8bb: and Dxx/Kxx)
procedure TITModule.InitCommandD7(hc: THostChannel; sc: TSlaveChannel);
var
	hi, lo: Byte;
	vol: Int8;
begin
	sc.Flags.SF_RECALC_VOL := True;

	hi := hc.EfxMem_DKL and $F0;
	lo := hc.EfxMem_DKL and $0F;

	if lo = 0 then
	begin
		// Slide up
		hc.VolSlideDelta := hi >> 4;
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;
		if hc.VolSlideDelta = 15 then
			CommandD(hc);
	end
	else
	if hi = 0 then
	begin
		// Slide down
		hc.VolSlideDelta := -lo;
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;
		if hc.VolSlideDelta = -15 then
			CommandD(hc);
	end
	else
	if lo = $0F then
	begin
		// Slide up (fine)
		hc.VolSlideDelta := 0;
		vol := sc.VolSet + (hi >> 4);
		if vol > 64 then vol := 64;
		sc.Vol    := vol;
		sc.VolSet := vol;
		hc.VolSet := vol;
	end
	else
	if hi = $F0 then
	begin
		// Slide down (fine)
		hc.VolSlideDelta := 0;
		vol := sc.VolSet - lo;
		if vol < 0 then vol := 0;
		sc.Vol    := vol;
		sc.VolSet := vol;
		hc.VolSet := vol;
	end;
end;

procedure TITModule.InitCommandK(hc: THostChannel);
begin
	if hc.CmdVal > 0 then
		hc.EfxMem_DKL := hc.CmdVal;

	InitNoCommand(hc);

	if not hc.Flags.HF_CHAN_ON then Exit;

	InitVibrato(hc);
	InitCommandD7(hc, hc.SlaveChannel);

	hc.Flags.HF_ALWAYS_UPDATE_EFX := True; // Always update.
end;

procedure TITModule.InitCommandL(hc: THostChannel);
var
	CmdVal: Byte;
begin
	CmdVal := hc.CmdVal;
	if CmdVal > 0 then
		hc.EfxMem_DKL := CmdVal;
	if hc.Flags.HF_CHAN_ON then
	begin
		InitCommandG11(hc);
		InitCommandD7(hc, hc.SlaveChannel);
	end;
end;

procedure TITModule.InitCommandM2(hc: THostChannel; vol: Byte);
var
	sc: TSlaveChannel;
begin
	if hc.Flags.HF_CHAN_ON then
	begin
		sc := hc.SlaveChannel;
		sc.ChnVol := vol;
		sc.Flags.SF_RECALC_VOL := True;
	end;
	hc.ChnVol := vol;
end;

procedure TITModule.InitCommandM(hc: THostChannel);
begin
	InitNoCommand(hc);
	if hc.CmdVal <= $40 then
		InitCommandM2(hc, hc.CmdVal);
end;

procedure TITModule.InitCommandN(hc: THostChannel);
var
	hi, lo, vol, CmdVal: Byte;
begin
	CmdVal := hc.CmdVal;
	if CmdVal > 0 then
		hc.EfxMem_N := CmdVal;

	InitNoCommand(hc);

	hi := hc.EfxMem_N and $F0;
	lo := hc.EfxMem_N and $0F;

	if lo = 0 then
	begin
		hc.MiscEfxData[0] := hi >> 4;
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
	end
	else
	if hi = 0 then
	begin
		hc.MiscEfxData[0] := -lo;
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
	end
	else
	if lo = $0F then
	begin
		vol := hc.ChnVol + (hi >> 4);
		if vol > 64 then vol := 64;
		InitCommandM2(hc, vol);
	end
	else
	if hi = $F0 then
	begin
		vol := hc.ChnVol - lo;
		if Int8(vol) < 0 then vol := 0;
		InitCommandM2(hc, vol);
	end;
end;

procedure TITModule.InitCommandO(hc: THostChannel);
var
	CmdVal: Byte;
	offset: Integer;
	sc: TSlaveChannel;
begin
	CmdVal := hc.CmdVal;
	if CmdVal = 0 then
		CmdVal := hc.EfxMem_O;

	hc.EfxMem_O := CmdVal;

	InitNoCommand(hc);

	if ((hc.NotePackMask and $33) <> 0) and (hc.TranslatedNote < 120) and (hc.Flags.HF_CHAN_ON) then
	begin
		sc := hc.SlaveChannel;

		offset := ((hc.HighSmpOffs * 256) or hc.EfxMem_O) * 256;
		if offset >= sc.LoopEnd then
		begin
			if not Header.Flags.ITF_OLD_EFFECTS then
				Exit;
			offset := sc.LoopEnd - 1;
		end;

		sc.SamplingPosition := offset;
		sc.Frac32 := 0; // 8bb: clear fractional sampling position
		sc.Frac64 := 0; // 8bb: also clear frac for my high-quality driver/mixer
	end;
end;

procedure TITModule.InitCommandP(hc: THostChannel);
var
	CmdVal, hi, lo, pan: Byte;
begin
	CmdVal := hc.CmdVal;
	if CmdVal > 0 then
		hc.EfxMem_P := CmdVal;

	InitNoCommand(hc);

	pan := hc.ChnPan;
	if hc.Flags.HF_CHAN_ON then
		pan := hc.SlaveChannel.PanSet;

	if pan = PAN_SURROUND then
		Exit;

	hi := hc.EfxMem_P and $F0;
	lo := hc.EfxMem_P and $0F;

	if lo = 0 then
	begin
		hc.MiscEfxData[0] := -(hi >> 4);
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
	end
	else
	if hi = 0 then
	begin
		hc.MiscEfxData[0] := lo;
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
	end
	else
	if lo = $0F then
	begin
		pan -= hi >> 4;
		if Int8(pan) < 0 then pan := 0;

		InitCommandX2(hc, pan);
	end
	else
	if hi = $F0 then
	begin
		pan += lo;
		if pan > 64 then pan := 64;

		InitCommandX2(hc, pan);
	end;
end;

procedure TITModule.InitCommandQ(hc: THostChannel);
begin
	InitNoCommand(hc);

	if hc.CmdVal > 0 then
		hc.EfxMem_Q := hc.CmdVal;

	if not hc.Flags.HF_CHAN_ON then
		Exit;

	hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;

	if (hc.NotePackMask and $11) <> 0 then
		hc.RetrigCount := hc.EfxMem_Q and $0F
	else
		CommandQ(hc);
end;

procedure TITModule.InitCommandR(hc: THostChannel);
var
	speed, depth: Byte;
begin
	speed := hc.CmdVal >> 4;
	depth := hc.CmdVal and $0F;

	if speed > 0 then
		hc.TremoloSpeed := speed << 2;
	if depth > 0 then
		hc.TremoloDepth := depth << 1;

	InitNoCommand(hc);

	if hc.Flags.HF_CHAN_ON then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;
		InitTremolo(hc);
	end;
end;

procedure TITModule.InitCommandS(hc: THostChannel);
var
	cmd, val, CmdVal,
	targetHostChnNum: Byte;
	i: Integer;
	sc: TSlaveChannel;
begin
	CmdVal := hc.CmdVal;
	if CmdVal = 0 then
		CmdVal := hc.EfxMem_S;

	hc.EfxMem_S := CmdVal;

	cmd := CmdVal and $F0;
	val := CmdVal and $0F;

	hc.MiscEfxData[0] := cmd;
	hc.MiscEfxData[1] := val;

	case cmd of

		$30: // set vibrato waveform
		begin
			if val <= 3 then
				hc.VibratoWaveform := val;
			InitNoCommand(hc);
		end;

		$40: // set tremolo waveform
		begin
			if val <= 3 then
				hc.TremoloWaveform := val;
			InitNoCommand(hc);
		end;

		$50: // set panbrello waveform
		begin
			if val <= 3 then
			begin
				hc.PanbrelloWaveform := val;
				hc.PanbrelloPos := 0;
			end;
			InitNoCommand(hc);
		end;

		$60: // extra delay of x frames
		begin
			CurrentTick += val;
			ProcessTick += val;
			InitNoCommand(hc);
		end;

		$70: // instrument functions
		begin
			targetHostChnNum := hc.HostChnNum or CHN_DISOWNED;
			case val of

				$0: // Past note cut
				begin
					InitNoCommand(hc);
					for i := 0 to MAX_SLAVE_CHANNELS-1 do
					begin
						sc := SlaveChannels[i];
						if sc.HostChnNum = targetHostChnNum then
						begin
							if not Driver.Flags.DF_USES_VOLRAMP then
								sc.Flags.WordAccess := 0;
							sc.Flags.SF_NOTE_STOP := True;
						end;
					end;
				end;

				$1: // Past note off
				begin
					InitNoCommand(hc);
					for i := 0 to MAX_SLAVE_CHANNELS-1 do
					begin
						sc := SlaveChannels[i];
						if sc.HostChnNum = targetHostChnNum then
							sc.Flags.SF_NOTE_OFF := True;
					end;
				end;

				$2: // Past note fade
				begin
					InitNoCommand(hc);
					for i := 0 to MAX_SLAVE_CHANNELS-1 do
					begin
						sc := SlaveChannels[i];
						if sc.HostChnNum = targetHostChnNum then
							sc.Flags.SF_FADEOUT := True;
					end;
				end;

				$3..$6: // Set NNA to cut/continue/off/fade
				begin
					InitNoCommand(hc);
					if hc.Flags.HF_CHAN_ON then
						hc.SlaveChannel.NNA := val - 3;
				end;

				$7: // Set volume envelope off/on
				begin
					InitNoCommand(hc);
					if hc.Flags.HF_CHAN_ON then
						hc.SlaveChannel.Flags.SF_VOLENV_ON := (val = $8);
				end;

				$9, $A: // Set panning envelope off/on
				begin
					InitNoCommand(hc);
					if hc.Flags.HF_CHAN_ON then
						hc.SlaveChannel.Flags.SF_PANENV_ON := (val = $A);
				end;

				$B, $C: // Set pitch envelope off/on
				begin
					InitNoCommand(hc);
					if hc.Flags.HF_CHAN_ON then
						hc.SlaveChannel.Flags.SF_PITCHENV_ON := (val = $C);
				end;

				else // $D, $E, $F
					InitNoCommand(hc);
			end;
		end;

		$80: // set pan
		begin
			InitNoCommand(hc);
			InitCommandX2(hc, (((val << 4) or val) + 2) >> 2);
		end;

		$90: // set surround
		begin
			InitNoCommand(hc);
			if val = 1 then
				InitCommandX2(hc, PAN_SURROUND);
		end;

		$A0: // Set high order offset
		begin
			hc.HighSmpOffs := val;
			InitNoCommand(hc);
		end;

		$B0: // loop control (8bb: pattern loop)
		begin
			InitNoCommand(hc);
			if val = 0 then
				hc.PattLoopStartRow := CurrentRow
			else
			if hc.PattLoopCount = 0 then
			begin
				hc.PattLoopCount := val;
				ProcessRow := hc.PattLoopStartRow - 1;
				PatternLooping := True;
			end
			else
			begin
				Dec(hc.PattLoopCount);
				if hc.PattLoopCount <> 0 then
				begin
					ProcessRow     := hc.PattLoopStartRow - 1;
					PatternLooping := True;
				end
				else
					hc.PattLoopStartRow := CurrentRow + 1;
			end;
		end;

		$C0: // note cut
		begin
			hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;
			InitNoCommand(hc);
		end;

		$D0: // note delay
		begin
			hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
		end;

		$E0: // pattern delay
		begin
			if not RowDelayOn then
			begin
				RowDelay   := val + 1;
				RowDelayOn := True;
			end;
			InitNoCommand(hc);
		end;

		$F0: // MIDI Macro select
		begin
			hc.EfxMem_SFx := val;
			InitNoCommand(hc);
		end;

		else
			InitNoCommand(hc);
	end;
end;

procedure TITModule.InitCommandT(hc: THostChannel);
var
	CmdVal: Byte;
begin
	CmdVal := hc.CmdVal;
	if CmdVal = 0 then
		CmdVal := hc.EfxMem_T;

	hc.EfxMem_T := CmdVal;

	if CmdVal >= $20 then
	begin
		Tempo := CmdVal;
		InitTempo;
		InitNoCommand(hc);
	end
	else
	begin
		InitNoCommand(hc);
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True; // Update mode
	end;
end;

procedure TITModule.InitCommandU(hc: THostChannel);
var
	speed, depth: Byte;
begin
	if (hc.NotePackMask and $11) <> 0 then
	begin
		hc.VibratoPos := 0;
		hc.LastVibratoData := 0;
	end;

	speed := (hc.CmdVal >> 4) << 2;
	depth := hc.CmdVal and $0F;

	if speed > 0 then
		hc.VibratoSpeed := speed;

	if depth > 0 then
	begin
		if Header.Flags.ITF_OLD_EFFECTS then
			depth *= 2;
		hc.VibratoDepth := depth;
	end;

	InitNoCommand(hc);

	if hc.Flags.HF_CHAN_ON then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True; // Update mode
		InitVibrato(hc);
	end;
end;

procedure TITModule.InitCommandV(hc: THostChannel);
begin
	if hc.CmdVal <= $80 then
	begin
		GlobalVolume := hc.CmdVal;
		RecalculateAllVolumes;
	end;

	InitNoCommand(hc);
end;

procedure TITModule.InitCommandW(hc: THostChannel);
var
	hi, lo: Byte;
	vol: Word;
begin
	InitNoCommand(hc);

	if hc.CmdVal > 0 then
		hc.EfxMem_W := hc.CmdVal;

	if hc.EfxMem_W = 0 then
		Exit;

	hi := hc.EfxMem_W and $F0;
	lo := hc.EfxMem_W and $0F;

	if lo = 0 then
	begin
		hc.MiscEfxData[0] := hi >> 4;
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
	end
	else
	if hi = 0 then
	begin
		hc.MiscEfxData[0] := -lo;
		hc.Flags.HF_ALWAYS_UPDATE_EFX := True;
	end
	else
	if lo = $0F then
	begin
		vol := GlobalVolume + (hi >> 4);
		if vol > 128 then vol := 128;
		GlobalVolume := vol;
		RecalculateAllVolumes;
	end
	else
	if hi = $F0 then
	begin
		vol := GlobalVolume - lo;
		if Int16(vol) < 0 then vol := 0;
		GlobalVolume := vol;
		RecalculateAllVolumes;
	end;
end;

procedure TITModule.InitCommandX(hc: THostChannel);
begin
	InitNoCommand(hc);
	InitCommandX2(hc, (hc.CmdVal + 2) >> 2); // 8bb: 0..255 . 0..63 (rounded)
end;

procedure TITModule.InitCommandY(hc: THostChannel);
var
	speed, depth: Byte;
begin
	speed := hc.CmdVal >> 4;
	depth := hc.CmdVal and $0F;

	if speed > 0 then
		hc.PanbrelloSpeed := speed;

	if depth > 0 then
		hc.PanbrelloDepth := depth << 1;

	InitNoCommand(hc);

	if hc.Flags.HF_CHAN_ON then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True; // Update mode
		CommandY(hc);
	end;
end;

procedure TITModule.InitCommandZ(hc: THostChannel);
var
	W: Word;
begin
	InitNoCommand(hc);

	// Macros start at 120h, 320h
	if hc.CmdVal >= $80 then // Macros
	begin
		W := Word(hc.CmdVal and $7F) << 5;
		W += $320;
	end
	else
	begin
		W := Word(hc.EfxMem_SFx and $0F) << 5;
		W += $120;
	end;

	MIDITranslate(hc, hc.SlaveChannel, W);
end;

procedure TITModule.CommandD(hc: THostChannel);
var
	sc: TSlaveChannel;
	vol: Int8;
begin
	sc := hc.SlaveChannel;

	vol := sc.VolSet + hc.VolSlideDelta;

	if vol < 0 then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := False;
		vol := 0;
	end
	else
	if vol > 64 then
	begin
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := False;
		vol := 64;
	end;

	CommandD2(hc, sc, vol);
end;

procedure TITModule.CommandE(hc: THostChannel);
begin
	CommandEChain(hc, hc.MiscEfxData[1] << 8 or hc.MiscEfxData[0]);
end;

procedure TITModule.CommandF(hc: THostChannel);
begin
	CommandFChain(hc, hc.MiscEfxData[1] << 8 or hc.MiscEfxData[0]);
end;

// G - pitch slide
procedure TITModule.CommandG(hc: THostChannel);
var
	sc: TSlaveChannel;
	SlideValue: Word;
begin
	if not hc.Flags.HF_PITCH_SLIDE_ONGOING then Exit;

	sc := hc.SlaveChannel;

	SlideValue := hc.MiscEfxData[1] << 8 or hc.MiscEfxData[0];

	if hc.MiscEfxData[2] = 1 then // 8bb: slide direction
	begin
		// Slide up!
		PitchSlide(hc, sc, SlideValue);

		// Check that:
		//  1) Channel is on
		//  2) Frequency (set) is below porta to frequency

		if (not sc.Flags.SF_NOTE_STOP) and (sc.Frequency < hc.PortaFreq) then
			sc.FrequencySet := sc.Frequency
		else
		begin
			sc.Flags.SF_NOTE_STOP := False;
			hc.Flags.HF_CHAN_ON   := True; // Turn on.
			sc.Frequency    := hc.PortaFreq;
			sc.FrequencySet := hc.PortaFreq;
			hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := False;
			hc.Flags.HF_ALWAYS_UPDATE_EFX     := False;
			hc.Flags.HF_PITCH_SLIDE_ONGOING   := False; // Turn off calling
		end;
	end
	else
	begin
		// Slide down
		PitchSlide(hc, sc, -SlideValue);

		// Check that frequency is above porta to frequency.
		if sc.Frequency > hc.PortaFreq then
			sc.FrequencySet := sc.Frequency
		else
		begin
			sc.Frequency    := hc.PortaFreq;
			sc.FrequencySet := hc.PortaFreq;
			hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := False;
			hc.Flags.HF_ALWAYS_UPDATE_EFX     := False;
			hc.Flags.HF_PITCH_SLIDE_ONGOING   := False; // Turn off calling
		end;
	end;
end;

// H - vibrato
procedure TITModule.CommandH(hc: THostChannel);
var
	VibratoData: Int8;
	sc: TSlaveChannel;
begin
	sc := hc.SlaveChannel;
	sc.Flags.SF_FREQ_CHANGE := True;

	hc.VibratoPos += hc.VibratoSpeed;

	if hc.VibratoWaveform = 3 then
		VibratoData := (RandomNumber and 127) - 64
	else
		VibratoData := FineSineData[(hc.VibratoWaveform << 8) + hc.VibratoPos];

	hc.LastVibratoData := VibratoData; // Save last vibrato
	CommandH5(hc, sc, VibratoData);
end;

// I - tremor
procedure TITModule.CommandI(hc: THostChannel);
var
	sc: TSlaveChannel;
begin
	sc := hc.SlaveChannel;
	sc.Flags.SF_RECALC_VOL := True;
	Dec(hc.TremorCount);
	if Int8(hc.TremorCount) <= 0 then
	begin
		hc.TremorOnOff := hc.TremorOnOff xor 1;
		hc.TremorCount := hc.MiscEfxData[hc.TremorOnOff];
	end;
	if hc.TremorOnOff <> 1 then
		sc.Vol := 0;
end;

// J - arpeggio
procedure TITModule.CommandJ(hc: THostChannel);
var
	tick, arpNote: Word;
	tp: PWord;
	sc: TSlaveChannel;
	freq: UInt64;
begin
	sc := hc.SlaveChannel;
	tp := @hc.MiscEfxData[0];
	tick := tp^;
	sc.Flags.SF_FREQ_CHANGE := True;

	// 8bb: used as an index to a 16-bit LUT (hence increments of 2)
	tick += 2;
	if tick >= 6 then
	begin
		tp^ := 0;
		Exit;
	end;
	tp^ := tick;

	tp := @hc.MiscEfxData[tick];
	arpNote := tp^;

	freq := sc.Frequency * PitchTable[arpNote];
	if (freq and $FFFF000000000000) <> 0 then // 8bb: arp freq overflow
		sc.Frequency := 0
	else
		sc.Frequency := freq >> 16;
end;

// K - vibrato+volume slide (H+D)
procedure TITModule.CommandK(hc: THostChannel);
begin
	CommandH(hc);
	CommandD(hc);
end;

// L - pitch slide+volume slide (G+D)
procedure TITModule.CommandL(hc: THostChannel);
begin
	if hc.Flags.HF_PITCH_SLIDE_ONGOING then
	begin
		CommandG(hc);
		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := True;
	end;

	CommandD(hc);
end;

// N - channel volume slide
procedure TITModule.CommandN(hc: THostChannel);
var
	vol: Byte;
begin
	vol := hc.ChnVol + hc.MiscEfxData[0];

	if Int8(vol) < 0 then
		vol := 0
	else
	if vol > 64 then
		vol := 64;

	InitCommandM2(hc, vol);
end;

// P - panning slide
procedure TITModule.CommandP(hc: THostChannel);
var
	pan: Byte;
begin
	pan := hc.ChnPan;
	if hc.Flags.HF_CHAN_ON then
		pan := hc.SlaveChannel.PanSet;

	pan += hc.MiscEfxData[0];

	if Int8(pan) < 0 then pan := 0
	else
	if pan > 64 then pan := 64;

	InitCommandX2(hc, pan);
end;

// Q - retrig
procedure TITModule.CommandQ(hc: THostChannel);
var
	vol: Byte;
	sc, scTmp: TSlaveChannel;
begin
	Dec(hc.RetrigCount);
	if Int8(hc.RetrigCount) > 0 then
		Exit;

	// OK... reset counter
	hc.RetrigCount := hc.EfxMem_Q and $0F;

	sc := hc.SlaveChannel;

	// retrig count done
	if Driver.Flags.DF_USES_VOLRAMP then
	begin
		if Header.Flags.ITF_INSTR_MODE then
		begin
			scTmp := FindFreeSlaveChannel;
			if scTmp <> nil then
			begin
				scTmp.CopyFrom(sc);
				sc.Flags.SF_NOTE_STOP := True; // Cut
				sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED;
				sc := scTmp;
				hc.SlaveChannel := sc;
			end;
		end
		else // 8bb: samples-only mode
		begin
			scTmp := SlaveChannels[MAX_HOST_CHANNELS-1];
			scTmp.CopyFrom(sc);
			scTmp.Flags.SF_NOTE_STOP := True; // Cut
			scTmp.HostChnNum := scTmp.HostChnNum or CHN_DISOWNED;
		end;
	end;

	sc.Frac32 := 0; // 8bb: clear sampling position fraction
	sc.Frac64 := 0; // 8bb: also clear frac for my high-quality driver/mixer
	sc.HasLooped := False; // 8bb: for my high-quality driver/mixer
	sc.SamplingPosition := 0;

	sc.Flags.SF_UPDATE_MIXERVOL := True;
	sc.Flags.SF_NEW_NOTE := True;
	sc.Flags.SF_LOOP_CHANGED := True;

	vol := sc.VolSet;
	case (hc.EfxMem_Q >> 4) of
		$1: vol -=  1;
		$2: vol -=  2;
		$3: vol -=  4;
		$4: vol -=  8;
		$5: vol -= 16;
		$6: vol := (vol << 1) div 3;
		$7: vol := (vol >> 1);
		$8: Exit;
		$9: vol +=  1;
		$A: vol +=  2;
		$B: vol +=  4;
		$C: vol +=  8;
		$D: vol += 16;
		$E: vol := (vol * 3) >> 1;
		$F: vol := (vol << 1);
		else
			Exit;
	end;

	if Int8(vol) < 0 then vol := 0 else
	if vol > 64 then vol := 64;

	sc.Vol    := vol;
	sc.VolSet := vol;
	hc.VolSet := vol;
	sc.Flags.SF_RECALC_VOL := True;

	if hc.Smp = 101 then // MIDI sample
		MIDITranslate(hc, sc, MIDICOMMAND_STOPNOTE);
end;

procedure TITModule.CommandR2(hc: THostChannel; sc: TSlaveChannel; TremoloData: Int8);
var
	vol: Int16;
begin
	TremoloData := ((TremoloData * hc.TremoloDepth * 4) + 128) div 256;

	vol := sc.Vol + TremoloData;
	if vol < 0  then vol := 0 else
	if vol > 64 then vol := 64;
	sc.Vol := vol;
end;

// R - tremolo
procedure TITModule.CommandR(hc: THostChannel);
var
	TremoloData: Int8;
	sc: TSlaveChannel;
begin
	sc := hc.SlaveChannel;
	sc.Flags.SF_RECALC_VOL := True;

	hc.TremoloPos += hc.TremoloSpeed;

	if hc.TremoloWaveform = 3 then
		TremoloData := (RandomNumber and 127) - 64
	else
		TremoloData := FineSineData[(hc.TremoloWaveform << 8) + hc.TremoloPos];

	hc.LastTremoloData := TremoloData; // Save last tremolo
	CommandR2(hc, sc, TremoloData);
end;

// S - miscellanous
procedure TITModule.CommandS(hc: THostChannel);
var
	SCmd: Byte;
	ChannelMuted: Boolean;
	sc: TSlaveChannel;
begin
	// Have to handle SDx, SCx
	SCmd := hc.MiscEfxData[0];

	sc := hc.SlaveChannel;

	if SCmd = $D0 then // 8bb: Note delay
	begin
		hc.MiscEfxData[1] -= 1;
		if Int8(hc.MiscEfxData[1]) > 0 then
			Exit;

		hc.Flags.HF_UPDATE_EFX_IF_CHAN_ON := False;
		hc.Flags.HF_ALWAYS_UPDATE_EFX     := False;
		InitNoCommand(hc);
		hc.Flags.HF_ROW_UPDATED := True;

		ChannelMuted := (Header.ChnlPan[hc.HostChnNum] and 128) <> 0;
		if (ChannelMuted) and (not hc.Flags.HF_FREEPLAY_NOTE) and (hc.Flags.HF_CHAN_ON) then
			sc.Flags.SF_CHN_MUTED := True;
	end
	else
	if SCmd = $C0 then // Note cut.
	begin
		if not hc.Flags.HF_CHAN_ON then
			Exit;

		hc.MiscEfxData[1] -= 1;
		if Int8(hc.MiscEfxData[1]) > 0 then
			Exit;

		hc.Flags.HF_CHAN_ON := False;

		if (sc.Smp = 100) or (Driver.Flags.DF_USES_VOLRAMP) then
		else
			sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
	end;
end;

// T - tempo
procedure TITModule.CommandT(hc: THostChannel);
var
	T: Int16;
begin
	T := Tempo;

	if (hc.EfxMem_T and $F0) <> 0 then
	begin
		// Slide Up
		T += hc.EfxMem_T - 16;
		if T > 255 then Tempo := 255;
	end
	else
	begin
		// Slide Down
		Tempo -= hc.EfxMem_T;
		if T < 32 then Tempo := 32;
	end;

	Tempo := T;
	Driver.SetTempo(Tempo);
end;

// W - global volume
procedure TITModule.CommandW(hc: THostChannel);
var
	vol: Word;
begin
	vol := GlobalVolume + hc.MiscEfxData[0];

	if Int16(vol) < 0 then vol := 0
	else
	if vol > 128 then vol := 128;

	GlobalVolume := vol;
	RecalculateAllVolumes;
end;

// Y - panbrello
procedure TITModule.CommandY(hc: THostChannel);
var
	panData: Int8;
	sc: TSlaveChannel;
begin
	if not hc.Flags.HF_CHAN_ON then Exit;

	sc := hc.SlaveChannel;

	if hc.PanbrelloWaveform >= 3 then // 8bb: panbrello waveform
	begin
		// Random panning make speed the delay time.

		hc.PanbrelloPos -= 1;
		if Int8(hc.PanbrelloPos) <= 0 then
		begin
			hc.PanbrelloPos := hc.PanbrelloSpeed; // reset countdown.
			panData := (RandomNumber and 127) - 64;
			hc.LastPanbrelloData := panData;
		end
		else
			panData := hc.LastPanbrelloData;
	end
	else
	begin
		hc.PanbrelloPos += hc.PanbrelloSpeed;
		panData := FineSineData[hc.PanbrelloWaveform << 8 + hc.PanbrelloPos];
	end;

	if sc.PanSet <> PAN_SURROUND then
	begin
		panData := ((panData * Int8(hc.PanbrelloDepth) * 4) + 128) div 256;
		panData += sc.PanSet;

		if panData < 0  then panData := 0
		else
		if panData > 64 then panData := 64;

		sc.Flags.SF_RECALC_PAN := True;
		sc.Pan := panData;
	end;
end;

procedure TITModule.PitchSlideLinear(hc: THostChannel; sc: TSlaveChannel; SlideValue: Int16);
var
	fMultiplier, dNewFreq: Float;
begin
	Assert(sc <> nil);
	Assert(hc <> nil);
	Assert((SlideValue >= -1024) and (SlideValue <= 1024));

	sc.Flags.SF_FREQ_CHANGE := True; // recalculate pitch!

	{$IFDEF USEFPUCODE} // 8bb: IT2.15 (registered)

	// 8bb: yes, IT2 really uses 24-bit (float) precision here
	fMultiplier := Power(2, SlideValue * (1/768));

	dNewFreq := sc.Frequency * fMultiplier;
	if dNewFreq >= High(Int32) then
	begin
		sc.Flags.SF_NOTE_STOP := True; // Turn off channel
		hc.Flags.HF_CHAN_ON := False;
		Exit;
	end;

	sc.Frequency := NearbyInt(dNewFreq); // 8bb: rounded (nearbyint() is needed over round()!)

	{$ELSE}
	// 8bb: IT2.14 and older (what the vast majority of IT2 users had)

	if SlideValue < 0 then
	begin
		// 8bb: slide down
		SlideValue := -SlideValue;

		const uint16_t *SlideTable;
		if (SlideValue <= 15)
		begin
			SlideTable := FineLinearSlideDownTable;
		end;
		else
		begin
			SlideTable := LinearSlideDownTable;
			SlideValue >>= 2;
		end;

		sc.Frequency := ((uint64_t)sc.Frequency * SlideTable[SlideValue]) >> 16;
	end;
	else
	begin
		// 8bb: slide up

		const uint32_t *SlideTable;
		if (SlideValue <= 15)
		begin
			SlideTable := FineLinearSlideUpTable;
		end;
		else
		begin
			SlideTable := LinearSlideUpTable;
			SlideValue >>= 2;
		end;

		uint64_t Frequency := ((uint64_t)sc.Frequency * SlideTable[SlideValue]) >> 16;
		if (Frequency and $FFFF000000000000)
		begin
			sc.Flags |= SF_NOTE_STOP; // Turn off channel
			hc.Flags &= ~HF_CHAN_ON;
		end;
		else
		begin
			sc.Frequency := (uint32_t)Frequency;
		end;
	end;
	{$ENDIF}
end;

procedure TITModule.PitchSlide(hc: THostChannel; sc: TSlaveChannel; SlideValue: Int16);
const
	PeriodBase = 8363 * 1712;
var
	InitFreq, dFreqDiv, dNewFreq: Float;
begin
	Assert(sc <> nil);
	Assert(hc <> nil);

	if Header.Flags.ITF_LINEAR_FRQ then
	begin
		// 8bb: linear frequencies
		PitchSlideLinear(hc, sc, SlideValue);
	end
	else
	begin
		// 8bb: Amiga frequencies

		{$IFDEF USEFPUCODE}
		// 8bb: IT2.15

		InitFreq := sc.Frequency;
		dFreqDiv := PeriodBase - (InitFreq * SlideValue);
		// 8bb: added this, needed to make it work in extreme edge cases
		if dFreqDiv <= 0.0 then
			dFreqDiv := 1e-9; // 8bb: any tiny number (positive)

		sc.Flags.SF_FREQ_CHANGE := True; // recalculate pitch!

		dNewFreq := PeriodBase * InitFreq / dFreqDiv;
		if dNewFreq >= High(Int32) then
		begin
			sc.Flags.SF_NOTE_STOP := True; // Turn off channel
			hc.Flags.HF_CHAN_ON   := False;
			Exit;
		end;

		sc.Frequency := Trunc(dNewFreq); // 8bb: Do not round here! Truncate.
		{$ELSE}
		// 8bb: IT2.14 and older

		if (SlideValue < 0)
		begin
			SlideValue := -SlideValue;

			// 8bb: slide down

			uint64_t FreqSlide64 := (uint64_t)sc.Frequency * (uint32_t)SlideValue;
			if (FreqSlide64 > UINT32_MAX)
			begin
				sc.Flags |= SF_NOTE_STOP; // Turn off channel
				hc.Flags &= ~HF_CHAN_ON;
				return;
			end;

			FreqSlide64 += PeriodBase;

			uint32_t ShiftValue := 0;
			while (FreqSlide64 > UINT32_MAX)
			begin
				FreqSlide64 >>= 1;
				ShiftValue++;
			end;

			uint32_t Temp32 := (uint32_t)FreqSlide64;
			uint64_t Temp64 := (uint64_t)sc.Frequency * (uint32_t)PeriodBase;

			if (ShiftValue > 0)
				Temp64 >>= ShiftValue;

			if (Temp32 <= Temp64>>32)
			begin
				sc.Flags |= SF_NOTE_STOP;
				hc.Flags &= ~HF_CHAN_ON;
				return;
			end;

			sc.Frequency := (uint32_t)(Temp64 / Temp32);
		end;
		else
		begin
			// 8bb: slide up

			uint64_t FreqSlide64 := (uint64_t)sc.Frequency * (uint32_t)SlideValue;
			if (FreqSlide64 > UINT32_MAX)
			begin
				sc.Flags |= SF_NOTE_STOP; // Turn off channel
				hc.Flags &= ~HF_CHAN_ON;
				return;
			end;

			uint32_t FreqSlide32 := (uint32_t)FreqSlide64;

			uint32_t Temp32 := PeriodBase - FreqSlide32;
			if ((int32_t)Temp32 <= 0)
			begin
				sc.Flags |= SF_NOTE_STOP;
				hc.Flags &= ~HF_CHAN_ON;
				return;
			end;

			uint64_t Temp64 := (uint64_t)sc.Frequency * (uint32_t)PeriodBase;
			if (Temp32 <= Temp64>>32)
			begin
				sc.Flags |= SF_NOTE_STOP;
				hc.Flags &= ~HF_CHAN_ON;
				return;
			end;

			sc.Frequency := (uint32_t)(Temp64 / Temp32);
		end;
		{$ENDIF}
	end;
end;

function TITModule.AllocatePattern(Index, NewLength: Word): TPattern;
begin
	if Index >= MAX_PATTERNS then Exit(nil);

	if (Patterns[Index] = nil) or (Length(Patterns[Index].PackedData) = 0) then
	begin
		ReleasePattern(Index);
		Patterns[Index] := TPattern.Create;
		SetLength(Patterns[Index].PackedData, NewLength);
	end;

	Result := Patterns[Index];
end;

procedure TITModule.ReleasePattern(Index: Word);
begin
	if Index >= MAX_PATTERNS then Exit;

	if Patterns[Index] <> nil then
		FreeAndNil(Patterns[Index]);
end;

const
	S3M_ROWS = 64;

var
	PatternDataArea: array of Byte;
	EncodingInfo: array [0..MAX_HOST_CHANNELS*6-1] of Byte;

procedure ClearEncodingInfo;
var
	i: Integer;
	Enc: PByte;
begin
	Enc := @EncodingInfo[0];
	for i := 0 to MAX_HOST_CHANNELS-1 do
	begin
		Enc[0] := 0;   // mask
		Enc[1] := 253; // note
		Enc[2] := 0;   // ins
		Enc[3] := 255; // vol
		Enc[4] := 0;   // cmd
		Enc[5] := 0;   // value
		Enc += 6;
	end;
end;

function GetPatternLength(Rows: Word; out LengthOut: Word): Boolean;
var
	i, j: Integer;
	PW: PWord;
	P: Pointer;
	Bytes: Cardinal;
	Mask, Note, Instr, Vol: Byte;
	EfxAndParam: Word;
	Src, Enc: Cardinal;
label
	Next;
begin
	Result := False;
	LengthOut := 0;

	ClearEncodingInfo;

	Src := 0;
	Bytes := Rows; // End of row bytes added

	for i := 0 to Rows-1 do
	begin
		Enc := 0;

		for j := 0 to MAX_HOST_CHANNELS-1 do
		begin
			if (PatternDataArea[Src] = 253) and (PatternDataArea[Src+1] = 0) and
				(PatternDataArea[Src+2] = 255) and (PatternDataArea[Src+3] = 0) and
					(PatternDataArea[Src+4] = 0) then
						goto Next;

			Inc(Bytes); // 1 byte for channel indication

			Mask := 0;

			Note := PatternDataArea[Src+0];
			if Note <> 253 then
			begin
				if EncodingInfo[Enc+1] <> Note then
				begin
					EncodingInfo[Enc+1] := Note;
					Inc(Bytes);
					Mask := Mask or 1;
				end
				else
					Mask := Mask or 16;
			end;

			Instr := PatternDataArea[Src+1];
			if Instr <> 0 then
			begin
				if EncodingInfo[Enc+2] <> Instr then
				begin
					EncodingInfo[Enc+2] := Instr;
					Inc(Bytes);
					Mask := Mask or 2;
				end
				else
					Mask := Mask or 32;
			end;

			Vol := PatternDataArea[Src+2];
			if Vol <> 255 then
			begin
				if EncodingInfo[Enc+3] <> Vol then
				begin
					EncodingInfo[Enc+3] := Vol;
					Inc(Bytes);
					Mask := Mask or 4;
				end
				else
					Mask := Mask or 64;
			end;

			P := @PatternDataArea[Src+3];
			PW := P;
			EfxAndParam := PW^;

			if EfxAndParam <> 0 then
			begin
				P := @EncodingInfo[Enc+4];
				PW := P;
				if PW^ <> EfxAndParam then
				begin
					PW^ := EfxAndParam;
					Bytes += 2;
					Mask := Mask or 8;
				end
				else
					Mask := Mask or 128;
			end;

			if Mask <> EncodingInfo[Enc] then
			begin
				EncodingInfo[Enc] := Mask;
				Inc(Bytes);
			end;
Next:
			Src += 5;
			Enc += 6;
		end;
	end;

	if Bytes > 65535 then Exit;

	LengthOut := Bytes;
	Result := True;
end;

procedure EncodePattern(var Patt: TPattern; Rows: Byte);
var
	i: Integer;
	//Src, Dst, Enc: PByte;
	Dst: PByte;
	PW: PWord;
	P: Pointer;
	ch, Mask, Note, Ins, Vol: Byte;
	Src, Enc: Cardinal;
	EfxAndParam: Word;
label
	Next;
begin
	ClearEncodingInfo;

	Patt.Rows := Rows;

	Src := 0; //@PatternDataArea[0];
	Dst := @Patt.PackedData[0];

	for i := 0 to Rows-1 do
	begin
		Enc := 0; //@EncodingInfo[0];

		for ch := 0 to MAX_HOST_CHANNELS-1 do
		begin
			if (PatternDataArea[Src] = 253) and (PatternDataArea[Src+1] = 0) and
				(PatternDataArea[Src+2] = 255) and (PatternDataArea[Src+3] = 0) and
					(PatternDataArea[Src+4] = 0) then
						goto Next;

			Mask := 0;

			Note := PatternDataArea[Src+0];
			if Note <> 253 then
			begin
				if EncodingInfo[Enc+1] <> Note then
				begin
					EncodingInfo[Enc+1] := Note;
					Mask := Mask or 1;
				end
				else
					Mask := Mask or 16;
			end;

			Ins := PatternDataArea[Src+1];
			if PatternDataArea[Src+1] <> 0 then
			begin
				if EncodingInfo[Enc+2] <> Ins then
				begin
					EncodingInfo[Enc+2] := Ins;
					Mask := Mask or 2;
				end
				else
					Mask := Mask or 32;
			end;

			Vol := PatternDataArea[Src+2];
			if Vol <> 255 then
			begin
				if EncodingInfo[Enc+3] <> Vol then
				begin
					EncodingInfo[Enc+3] := Vol;
					Mask := Mask or 4;
				end
				else
					Mask := Mask or 64;
			end;

			P := @PatternDataArea[Src+3];
			PW := P;
			EfxAndParam := PW^;

			if EfxAndParam <> 0 then
			begin
				P := @EncodingInfo[Enc+4];
				PW := P;
				if PW^ <> EfxAndParam then
				begin
					PW^ := EfxAndParam;
					Mask := Mask or 8;
				end
				else
					Mask := Mask or 128;
			end;

			if EncodingInfo[Enc+0] <> Mask then
			begin
				EncodingInfo[Enc+0] := Mask;
				Dst^ := (ch + 1) or 128; // read another mask...
				Inc(Dst);
				Dst^ := Mask;
				Inc(Dst);
			end
			else
			begin
				Dst^ := ch + 1;
				Inc(Dst);
			end;

			if (Mask and 1) <> 0 then
			begin
				Dst^ := Note;
				Inc(Dst);
			end;

			if (Mask and 2) <> 0 then
			begin
				Dst^ := Ins;
				Inc(Dst);
			end;

			if (Mask and 4) <> 0 then
			begin
				Dst^ := Vol;
				Inc(Dst);
			end;

			if (Mask and 8) <> 0 then
			begin
				PWord(Dst)^ := EfxAndParam;
				Dst += 2;
			end;
Next:
			Src += 5;
			Enc += 6;
		end;

		Dst^ := 0;
		Inc(Dst);
	end;
end;

function TITModule.TranslateS3MPattern(Src: PByte; Pattern: Word): Boolean;
var
	i, j: Integer;
	Ptr8, OrigDst, Dst: PByte;
	B, Mask, Lo, Hi: Byte;
	PackedLength: Word;
begin
	Result := False;

	SetLength(PatternDataArea, MAX_HOST_CHANNELS * MAX_ROWS * 5);

	// 8bb: clear destination pattern
	Ptr8 := @PatternDataArea[0];
	for i := 0 to 199 do
	begin
		for j := 0 to MAX_HOST_CHANNELS-1 do
		begin
			Ptr8[0] := 253; // note
			Ptr8[1] := 0;   // ins
			Ptr8[2] := 255; // vol
			Ptr8[3] := 0;   // cmd
			Ptr8[4] := 0;   // value
			Ptr8 += 5;
		end;
	end;

	OrigDst := @PatternDataArea[0];

	for i := 0 to S3M_ROWS-1 do
	begin
		while True do
		begin
			B    := Src^;
			Mask := B;
			Inc(Src);

			if Mask = 0 then
			begin
				OrigDst += MAX_HOST_CHANNELS * 5; // 8bb: end of channels, go to next row
				Break;
			end;

			Dst := OrigDst + ((Mask and 31) * 5); // 8bb: aligned to current channel to write into

			// 8bb: Note and sample
			if (Mask and 32) <> 0 then
			begin
				B := Src^;
				Inc(Src);
				if B = 254 then
					Dst[0] := 254
				else
				if B <= 127 then
					Dst[0] := 12 + (((B >> 4) * 12) + (B and $0F)); // C5 is now central octave

				// Instrument
				B := Src^;
				Inc(Src);
				if B <= 99 then
					Dst[1] := B
				else
					Dst[1] := 0;
			end;

			// Volume
			if (Mask and 64) <> 0 then
			begin
				B := Src^;
				Inc(Src);
				if B <> 255 then
				begin
					if B <= 64 then
						Dst[2] := B
					else
						Dst[2] := 64;
				end;
			end;

			// 8bb: Effect + parameter
			if (Mask and 128) <> 0 then
			begin
				Dst[3] := Src^; Inc(Src);
				Dst[4] := Src^; Inc(Src);

				if Dst[3] = (Ord('C') - Ord('@')) then
				begin
					// 8bb: IT2's broken (?) way of converting between decimal/hex
					Dst[4] := (Dst[4] and $0F) + ((Dst[4] and $F0) >> 1) + ((Dst[4] and $F0) >> 3);
				end
				else
				if Dst[3] = (Ord('V') - Ord('@')) then
				begin
					if Dst[4] < 128 then
						Dst[4] := Dst[4] << 1
					else
						Dst[4] := 255;
				end
				else
				if Dst[3] = (Ord('X') - Ord('@')) then
				begin
					if Dst[4] = $A4 then // 8bb: surround
					begin
						Dst[3] := Ord('S') - Ord('@');
						Dst[4] := $91;
					end
					else
					begin
						if Dst[4] < 128 then
							Dst[4] := Dst[4] << 1
						else
							Dst[4] := 255;
					end;
				end
				else
				if Dst[3] = (Ord('D') - Ord('@')) then
				begin
					lo := Dst[4] and $0F;
					hi := Dst[4] and $F0;
					if (lo <> 0) and (hi <> 0) then
					begin
						if (lo <> $0F) and (hi <> $F0) then
							Dst[4] := Dst[4] and $0F;
					end;
				end;
			end;
		end;
	end;

	if not GetPatternLength(S3M_ROWS, PackedLength) then Exit;
	if AllocatePattern(Pattern, PackedLength) = nil then Exit;
	EncodePattern(Patterns[Pattern], S3M_ROWS);

	Result := True;
end;

function TITModule.LoadS3M(Stream: TStream): Boolean;

	function ReadString(L: Word): AnsiString;
	var
		i: Integer;
	begin
		SetLength(Result{%H-}, L);
		Stream.ReadBuffer(Result[1], L);

		for i := 1 to L do
			if Result[i] = #0 then
				Result[i] := ' ';

		//Result := TrimRight(Result);
	end;

var
	SP: Int64;
	i, j: Integer;
	Flags, MemSegL, PackedPatLength: Word;
	SmpPtrs, PatPtrs: array [0..99] of Word;
	Offset: Cardinal;
	Pan, DefPan, Kind, MemSegH, SmpFlags, ChannelOffFlag: Byte;
	Chan, IsStereo, Is16Bit: Boolean;
	PackedData: array of Byte;
	Sam: TSample;
	Ptr8: PInt8;
	Ptr16: PInt16;
begin
	Result := False;

	SP := Stream.Position;

	Header.SongName := ReadString(25);

	Stream.Seek(SP+$20, soBeginning);

	Header.OrdNum := Stream.ReadWord;
	Header.SmpNum := Stream.ReadWord;
	Header.PatNum := Stream.ReadWord;
	Flags := Stream.ReadWord;

	Stream.Seek(SP+$30, soBeginning);

	Header.GlobalVol    := Stream.ReadByte;
	Header.InitialSpeed := Stream.ReadByte;
	Header.InitialTempo := Stream.ReadByte;
	Header.MixVolume    := Stream.ReadByte;
	Stream.ReadByte;    // discard
	DefPan              := Stream.ReadByte;

	Header.SmpNum := Min(Header.SmpNum, 100);
	Header.PatNum := Min(Header.PatNum, 100);

	Header.Flags.WordAccess := 0;
	Header.Flags.ITF_OLD_EFFECTS := True;
	if (Flags and 8) <> 0 then
	begin
		Header.Flags.WordAccess := 0;
		Header.Flags.ITF_VOL0_OPTIMIZATION := True;
	end;

	Header.PanSep := 128;
	Header.GlobalVol *= 2;

	if (Header.MixVolume and 128) <> 0 then
	begin
		Header.Flags.ITF_STEREO := True;
		Header.MixVolume := Header.MixVolume and 127;
	end;

	// OK, panning now...

	Stream.Seek(SP+64, soBeginning);

	for i := 0 to 31 do
	begin
		Pan := Stream.ReadByte;

		if Pan >= 128 then
			Header.ChnlPan[i] := 32 or 128 // 8bb: center + channel off
		else
		begin
			Pan := Pan and 127;
			if Pan <= 7 then
				Header.ChnlPan[i] := 0
			else
			if Pan <= 15 then
				Header.ChnlPan[i] := 64
			else
				Header.ChnlPan[i] := 32;
		end;
	end;

	// 8bb: set rest of channels to "off"
	for i := 32 to MAX_HOST_CHANNELS-1 do
		Header.ChnlPan[i] := 32 or 128;

	for i := 0 to MAX_HOST_CHANNELS-1 do
		Header.ChnlVol[i] := 64;

	FillByte(Orders[0], MAX_ORDERS, 255);
	Stream.ReadBuffer(Orders[0],  Header.OrdNum);  // Order list loaded
	Stream.ReadBuffer({%H-}SmpPtrs[0], Header.SmpNum * 2);
	Stream.ReadBuffer({%H-}PatPtrs[0], Header.PatNum * 2);

	if DefPan = 252 then // 8bb: load custom channel pans, if present
	begin
		for i := 0 to 31 do
		begin
			Pan := Stream.ReadByte;
			if (Pan and 32) <> 0 then
			begin
				ChannelOffFlag := Header.ChnlPan[i] and 128;
				Header.ChnlPan[i] := (((Pan and 15) << 2) + 2) or ChannelOffFlag;
			end;
		end;
	end;

	// Load instruments (8bb: and data)

	for i := 0 to Header.SmpNum-1 do
	begin
		Offset := SmpPtrs[i] << 4;
		if Offset = 0 then Continue;

		Sam := TSample.Create(Self);
		Samples[i].Free; // precaution
		Samples[i] := Sam;

		Stream.Seek(SP+Offset, soBeginning);

		Kind := Stream.ReadByte;

		Sam.DOSFilename := ReadString(12);

		MemSegH := Stream.ReadByte;
		MemSegL := Stream.ReadWord;

		Sam.Length    := Stream.ReadDWord;
		Sam.LoopBegin := Stream.ReadDWord;
		Sam.LoopEnd   := Stream.ReadDWord;
		Sam.Vol       := Stream.ReadByte;

		Stream.ReadWord; // discard

		SmpFlags := Stream.ReadByte;
		Sam.C5Speed := Stream.ReadDWord;

		Stream.Seek(12, soCurrent);

		Sam.SampleName := ReadString(25);

		if Kind = 1 then
		begin
			Sam.Flags.SMPF_STEREO := SmpFlags and 2 <> 0;
			Sam.Flags.SMPF_ASSOCIATED_WITH_HEADER := (Sam.Length and $FFFF) > 0;

			Sam.OffsetInFile := ((MemSegH << 16) or MemSegL) << 4;
		end;

		Sam.Flags.SMPF_USE_LOOP := SmpFlags and 1 <> 0;
		Sam.Flags.SMPF_16BIT := SmpFlags and 4 <> 0;

		Sam.GlobVol := 64;
		Sam.DefPan  := 32;

		if Sam.Flags.SMPF_ASSOCIATED_WITH_HEADER then
		begin
			if Sam.OffsetInFile > 0 then // 8bb: added this check
			begin
				IsStereo := Sam.Flags.SMPF_STEREO; // 8bb: added stereo support for custom HQ driver
				Is16Bit  := Sam.Flags.SMPF_16BIT;

				Sam.AllocateSample(Sam.Length, Is16Bit, IsStereo);

				Stream.Seek(SP + Sam.OffsetInFile, soBeginning);
				for Chan := False to IsStereo do
					Stream.ReadBuffer(Sam.Data[Chan].Data^, Sam.Length);

				if not Is16Bit then
				begin
					// 8bb: convert from unsigned to signed
					for Chan := False to IsStereo do
					begin
						Ptr8 := Sam.Data[Chan].Data;
						for j := 0 to Sam.Length-1 do
						begin
							Ptr8^ := Ptr8^ xor $80;
							Inc(Ptr8);
						end;
					end;
				end
				else
				begin
					// 8bb: AllocateSample() also set Sam.Length, divide by two if 16-bit
					Sam.Length := Sam.Length div 2;
					for Chan := False to IsStereo do
					begin
						Ptr16 := Sam.Data[Chan].Data;
						for j := 0 to Sam.Length-1 do
						begin
							Ptr16^ := Ptr16^ xor $8000;
							Inc(Ptr16);
						end;
					end;
				end;
			end;
		end;
	end;

	// Load patterns....

	try
		for i := 0 to Header.PatNum-1 do
		begin
			Offset := PatPtrs[i] << 4;
			if Offset = 0 then Continue;

			Stream.Seek(SP + Offset, soBeginning);
			PackedPatLength := Stream.ReadWord;
			SetLength(PackedData{%H-}, PackedPatLength);
			Stream.ReadBuffer(PackedData[0], PackedPatLength);

			if not TranslateS3MPattern(@PackedData[0], i) then
				Exit;
		end;
	finally
		SetLength(PatternDataArea, 0);
	end;

	Result := True;
end;

function TITModule.LoadIT(Stream: TStream): Boolean;

	function ReadString(L: Word): AnsiString;
	var
		i: Integer;
	begin
		SetLength(Result{%H-}, L);
		Stream.ReadBuffer(Result[1], L);

		for i := 1 to L do
			if Result[i] = #0 then
				Result[i] := ' ';

		Result := TrimRight(Result);
	end;

	function ReadStrings(List: TStrings; L: Cardinal): Boolean;
	var
		i: Integer;
		B: Byte;
		S: AnsiString = '';
	begin
		if (List = nil) or (L < 1) then
			Exit(False);

		for i := 1 to L do
		begin
			B := Stream.ReadByte;
			case B of
				$0D:
				begin
					List.Add(S);
					S := '';
				end;
				$0:
					Break;
				else
					S := S + Chr(B);
			end;
		end;

		if S <> '' then
			List.Add(S);
		Result := True;
	end;

var
	SP: Int64;
	i, j, k: Integer;
	offset, PtrListOffset, PatPtrOffset,
	SmpPtrOffset, SmpOffset,
	InsPtrOffset, InsOffset: Cardinal;
	B, IsStereo, IsCompressed, Is16Bit, IsSignedSamples, IsDeltaEncoded: Boolean;
	PatLength, PatRows, W: Word;
	Pattern: TPattern;
	Ptr16: PInt16;
	Ptr8:  PInt8;
	Sam: TSample;
	Ins: TInstrument;
	env: ^TEnv;
begin
	Result := False;
	SP := Stream.Position;
	try
		Stream.Seek(SP+4, soBeginning);

		Header.SongName := ReadString(High(Header.SongName));

		DebugInfo('Song name: "' + Header.SongName + '"');

		Stream.Seek(1+2, soCurrent);

		Header.OrdNum := Stream.ReadWord;
		Header.InsNum := Stream.ReadWord;
		Header.SmpNum := Stream.ReadWord;
		Header.PatNum := Stream.ReadWord;

		DebugInfo('Orders: '      + Header.OrdNum.ToString);
		DebugInfo('Instruments: ' + Header.InsNum.ToString);
		DebugInfo('Samples: '     + Header.SmpNum.ToString);
		DebugInfo('Patterns: '    + Header.PatNum.ToString);

		Header.Cwtv    := Stream.ReadWord;
		Header.Cmwt    := Stream.ReadWord;
		Header.Flags.WordAccess := Stream.ReadWord;
		Header.Special := Stream.ReadWord;
		Header.GlobalVol := Stream.ReadByte;
		Header.MixVolume := Stream.ReadByte;
		Header.InitialSpeed := Stream.ReadByte;
		Header.InitialTempo := Max(Stream.ReadByte, LOWEST_BPM_POSSIBLE);
		Header.PanSep := Stream.ReadByte;

		Stream.ReadByte;

		Header.MessageLength := Min(Stream.ReadWord, MAX_SONGMSG_LENGTH);
		Header.MessageOffset := Stream.ReadDWord;

		Stream.Seek(4, soCurrent); // skip unwanted stuff
		Stream.ReadBuffer(Header.ChnlPan[0], MAX_HOST_CHANNELS);
		Stream.ReadBuffer(Header.ChnlVol[0], MAX_HOST_CHANNELS);

		// IT2 doesn't do this test, but I do it for safety.
		if (Header.OrdNum > MAX_ORDERS+1) or (Header.InsNum > MAX_INSTRUMENTS) or
		   (Header.SmpNum > MAX_SAMPLES)  or (Header.PatNum > MAX_PATTERNS)    then
				Exit;

		PtrListOffset := 192 + Header.OrdNum;

		FillByte(Orders[0], MAX_ORDERS, 255);
		i := Header.OrdNum - 1; // IT2 does this (removes the count for the last 255 terminator)
		if i > 0 then
			Stream.ReadBuffer(Orders[0], i);

		Stream.Seek(SP + 192 + Header.OrdNum +
			((Header.InsNum + Header.SmpNum + Header.PatNum) * 4), soBeginning);

		// skip time data, if present
		if (Header.Special and 2) <> 0 then
		begin
			offset := Stream.ReadWord;
			if offset > 0 then
			begin
				DebugInfo('Skipping time data, ' + offset.ToSTring + ' bytes');
				Stream.Seek(offset * 8, soCurrent);
			end;
		end;

		// read embedded MIDI configuration, if preset (needed for Zxx macros)
		if (Header.Special and 8) > 0 then
			Stream.ReadBuffer(MIDIDataArea[0], Length(MIDIDataArea));

		// load song message, if present
		SongMessage.Clear;
		if ((Header.Special and 1) <> 0) and (Header.MessageLength > 0) and (Header.MessageOffset > 0) then
		begin
			DebugInfo('Song message (' + Header.MessageLength.ToString +
				' bytes at ' + Header.MessageOffset.ToString + '):');
			Stream.Seek(SP+Header.MessageOffset, soBeginning);
			ReadStrings(SongMessage, Header.MessageLength);
			DebugInfo(SongMessage.Text);
		end;

		// =====================================
		// ========  LOAD INSTRUMENTS  =========
		// =====================================

		InsPtrOffset := PtrListOffset;

		for i := 0 to Header.InsNum-1 do
		begin
			Ins := TInstrument.Create;
                        Instruments[i].Free;
			Instruments[i] := Ins;

			Stream.Seek(SP + InsPtrOffset + (i * 4), soBeginning);

			InsOffset := Stream.ReadDWord;
			if InsOffset = 0 then Continue;

			Stream.Seek(SP + InsOffset, soBeginning);

			if Header.Cmwt >= $200 then
			begin
				Stream.Seek(4, soCurrent); // skip unwanted stuff
				ins.DOSFilename := ReadString(High(Ins.DOSFilename));
				ins.NNA := Stream.ReadByte;
				ins.DCT := Stream.ReadByte;
				ins.DCA := Stream.ReadByte;
				ins.FadeOut := Stream.ReadWord;
				ins.PitchPanSep := Stream.ReadByte;
				ins.PitchPanCenter := Stream.ReadByte;
				ins.GlobVol := Stream.ReadByte;
				ins.DefPan  := Stream.ReadByte;
				ins.RandVol := Stream.ReadByte;
				ins.RandPan := Stream.ReadByte;
				Stream.Seek(4, soCurrent); // skip unwanted stuff
				ins.InstrumentName := ReadString(High(Ins.InstrumentName));
				ins.FilterCutoff := Stream.ReadByte;
				ins.FilterResonance := Stream.ReadByte;
				ins.MIDIChn := Stream.ReadByte;
				ins.MIDIProg := Stream.ReadByte;
				ins.MIDIBank := Stream.ReadWord;
				Stream.ReadBuffer(Ins.SmpNoteTable[0], Length(Ins.SmpNoteTable)*2);

				// read envelopes
				for j := 0 to 2 do
				begin
					case j of
						0: env := @Ins.VolEnv;
						1: env := @Ins.PanEnv;
						2: env := @Ins.PitchEnv;
					end;

					env^.Flags.ByteAccess := Stream.ReadByte;
					env^.Num := Stream.ReadByte;
					env^.LoopBegin := Stream.ReadByte;
					env^.LoopEnd := Stream.ReadByte;
					env^.SustainLoopBegin := Stream.ReadByte;
					env^.SustainLoopEnd := Stream.ReadByte;

					for k := 0 to 24 do
					begin
						env^.NodePoints[k].Magnitude := Stream.ReadByte;
						env^.NodePoints[k].Tick := Stream.ReadWord;
					end;

					Stream.ReadByte; // skip
				end;
			end
			else // old instruments (v1.xx)
			begin
				Stream.Seek(4, soCurrent); // skip unwanted stuff
				ins.DOSFilename := ReadString(High(ins.DOSFilename));
				ins.VolEnv.Flags.ByteAccess := Stream.ReadByte;
				ins.VolEnv.LoopBegin := Stream.ReadByte;
				ins.VolEnv.LoopEnd   := Stream.ReadByte;
				ins.VolEnv.SustainLoopBegin := Stream.ReadByte;
				ins.VolEnv.SustainLoopEnd := Stream.ReadByte;
				Stream.ReadWord;  // skip unwanted stuff
				ins.FadeOut := Stream.ReadWord * 2;
				ins.NNA := Stream.ReadByte;
				ins.DCT := Stream.ReadByte;
				Stream.Seek(4, soCurrent); // skip unwanted stuff
				ins.InstrumentName := ReadString(High(ins.InstrumentName));
				Stream.Seek(6, soCurrent); // skip unwanted stuff
				Stream.ReadBuffer(ins.SmpNoteTable[0], Length(ins.SmpNoteTable)*2);

				// set default values not present in old instrument
				ins.PitchPanCenter := 60;
				ins.GlobVol := 128;
				ins.DefPan := 32 + 128; // center + pan disabled

				Stream.Seek(200, soCurrent);

				// read volume envelope
				for j := 0 to 24 do
				begin
					ins.VolEnv.Num := j;
					W := Stream.ReadWord;
					if W = $FFFF then Break; // end of envelope

					with ins.VolEnv.NodePoints[j] do
					begin
						Magnitude := W >> 8;
						Tick := W and $FF;
					end;
				end;

				ins.PanEnv.Num := 2;
				ins.PanEnv.NodePoints[1].Tick := 99;
				ins.PitchEnv.Num := 2;
				ins.PitchEnv.NodePoints[1].Tick := 99;
			end;

			DebugInfo('[INSTRUMENT ' + (i+1).ToString + ']');

			DebugInfo('File offset: ' + (InsOffset+4).ToHexString);
				DebugInfo('Name: "' + Ins.InstrumentName + '"');
				DebugInfo('DOS Filename: "' + Ins.DOSFilename + '"');
		end;

		// =====================================
		// ======== LOAD SAMPLE HEADERS ========
		// =====================================

		SmpPtrOffset := Header.InsNum * 4 + PtrListOffset;
		Stream.Seek(SP + SmpPtrOffset, soBeginning);

		for i := 0 to Header.SmpNum-1 do
		begin
			Stream.Seek(SP + SmpPtrOffset + (i * 4), soBeginning);

			SmpOffset := Stream.ReadDWord;
			if SmpOffset = 0 then
				Continue;

			Stream.Seek(SP + SmpOffset + 4, soBeginning); // skip unwanted stuff

			Sam := TSample.Create(Self);
			Samples[i].Free; // precaution
			Samples[i] := Sam;

			Sam.DOSFilename := ReadString(High(Sam.DOSFilename));
			Sam.GlobVol := Stream.ReadByte;
			Sam.Flags.ByteAccess := Stream.ReadByte;
			Sam.Vol := Stream.ReadByte;
			Sam.SampleName := ReadString(High(Sam.SampleName));
			Sam.Cvt := Stream.ReadByte;
			Sam.DefPan := Stream.ReadByte;
			Sam.Length := Stream.ReadDWord;
			Sam.LoopBegin := Stream.ReadDWord;
			Sam.LoopEnd := Stream.ReadDWord;
			Sam.C5Speed := Stream.ReadDWord;
			Sam.SustainLoopBegin := Stream.ReadDWord;
			Sam.SustainLoopEnd := Stream.ReadDWord;
			Sam.OffsetInFile := Stream.ReadDWord;
			Sam.AutoVibratoSpeed := Stream.ReadByte;
			Sam.AutoVibratoDepth := Stream.ReadByte;
			Sam.AutoVibratoRate := Stream.ReadByte;
			Sam.AutoVibratoWaveform := Stream.ReadByte;

			if (Sam.Length > 0) and (not Header.Flags.ITF_INSTR_MODE) then
			begin
				DebugInfo('');
				DebugInfo('[SAMPLE ' + (i+1).ToString + ']');

				DebugInfo('File offset: ' + (SmpOffset+4).ToHexString);
				DebugInfo('Size: ' + Sam.Length.ToString);
//				if Sam.SampleName <> '' then
					DebugInfo('Sample name: "' + Sam.SampleName + '"');
//				if Sam.DOSFilename <> '' then
					DebugInfo('DOS Filename: "' + Sam.DOSFilename + '"');
				if Sam.Flags.SMPF_STEREO     then DebugInfo('[Stereo] ');
				if Sam.Flags.SMPF_COMPRESSED then DebugInfo('[IsCompressed] ');
				if Sam.Flags.SMPF_16BIT      then DebugInfo('[Is16Bit] ');
				if (Sam.Cvt and 1) <> 0      then DebugInfo('[IsSignedSamples] ');
				if (Sam.Cvt and 4) <> 0      then DebugInfo('[IsDeltaEncoded] ');
				DebugInfo('');
			end;
		//end;

		// =====================================
		// ========  LOAD SAMPLE DATA  =========
		// =====================================

		{for i := 0 to Header.SmpNum-1 do
		begin
			Sam := Samples[i];}
			if (Sam = nil) or (Sam.OffsetInFile = 0) or (not Sam.Flags.SMPF_ASSOCIATED_WITH_HEADER) then
				Continue;

			Stream.Seek(SP + Sam.OffsetInFile, soBeginning);

			IsStereo        := Sam.Flags.SMPF_STEREO; // added stereo support for custom HQ driver
			IsCompressed    := Sam.Flags.SMPF_COMPRESSED;
			Is16Bit         := Sam.Flags.SMPF_16BIT;
			IsSignedSamples := (Sam.Cvt and 1) <> 0;
			IsDeltaEncoded  := (Sam.Cvt and 4) <> 0;

			if (IsDeltaEncoded) and (not IsCompressed) then
			begin
				DebugInfo('Skip 1');
				Continue;
			end;

			if (Sam.Length = 0) or (not Sam.Flags.SMPF_ASSOCIATED_WITH_HEADER) then
			begin
				DebugInfo('Skip 2');
				Continue; // safely skip this sample
			end;

			if (Sam.Cvt and %11111010) <> 0 then
			begin
				DebugInfo('Skip 3');
				Continue; // not supported
			end;

			Sam.AllocateSample(Sam.Length, Is16Bit, IsStereo);

			if IsCompressed then
			begin
				if not Sam.LoadCompressedSample(Stream, Is16Bit, IsStereo, IsDeltaEncoded) then
				begin
					Debug('ERROR: LoadCompressedSample failed!');
					Exit;
				end;
			end
			else
			begin
				for B := False to IsStereo do
					Stream.ReadBuffer(Sam.Data[B].Data^, Sam.Length);
			end;

			// convert unsigned sample to signed
			if not IsSignedSamples then
			begin
				if Is16Bit then
				begin
					Ptr16 := Sam.Data[False].Data;
					for offset := 0 to Sam.Length-1 do
					begin
						Ptr16^ := Ptr16^ xor $8000;
						Inc(Ptr16);
					end;
				end
				else
				begin
					Ptr8 := Sam.Data[False].Data;
					for offset := 0 to Sam.Length-1 do
					begin
						Ptr8^ := Ptr8^ xor $80;
						Inc(Ptr8);
					end;
				end;
			end;

			// AllocateSample() also set Sam.Length, divide by two if 16-bit
			if Is16Bit then
				Sam.Length := Sam.Length div 2;
		end;

		// =====================================
		// ==========  LOAD PATTERNS  ==========
		// =====================================

		PatPtrOffset := PtrListOffset + (Header.InsNum * 4) + (Header.SmpNum * 4);
		Stream.Seek(SP + PatPtrOffset, soBeginning);

		for i := 0 to Header.PatNum-1 do
		begin
			Stream.Seek(SP + PatPtrOffset + (i * 4), soBeginning);

			offset := Stream.ReadDWord;
			if offset = 0 then
				Continue;

			Stream.Seek(SP + offset, soBeginning);

			PatLength := Stream.ReadWord;
			PatRows   := Stream.ReadWord; // Pattern.Rows

			if (PatLength = 0) or (PatRows = 0) then
				Continue;

			Stream.Seek(4, soCurrent);

			Pattern := AllocatePattern(i, PatLength);
			Pattern.Rows := PatRows;

			Stream.ReadBuffer(Pattern.PackedData[0], PatLength);
		end;

	except
		on E:Exception do
		begin
			Debug('Error: ' + E.Message);
			Exit(False);
		end;
	end;

	Debug('Loaded.');
	Result := True;
end;

function TITModule.GetModuleType(Stream: TStream): TModuleType;
const
	STR_IT:  AnsiString = 'IMPM';
	STR_S3M: AnsiString = 'SCRM';
var
	Hdr: array[0..44+4-1] of Byte;
	OldOffset, DataLen: Int64;
begin
	Result := FORMAT_UNKNOWN;

	OldOffset := Stream.Position;
	DataLen   := Stream.Size;

	Stream.ReadBuffer({%H-}Hdr[0], Length(Hdr));

	if (DataLen >= 4) and (CompareChar(Hdr[0], STR_IT[1], 4) = 0) then
		Result := FORMAT_IT
	else
	if (DataLen >= 44+4) and ((CompareChar(Hdr[44], STR_S3M[1], 4) = 0)) then
		Result := FORMAT_S3M;

	Stream.Seek(OldOffset, soBeginning);
end;

function TITModule.LoadFromStream(Stream: TStream): Boolean;
const
	mmcmpstr = 'ziRCONia';
var
	WasCompressed: Boolean = False;
	S: AnsiString;
begin
	Result := False;

	if Stream.Size >= 4+4 then
	begin
		// find out if module is MMCMP compressed
		S := mmcmpstr;
		Stream.Read(S[1], 8);
		if S = mmcmpStr then
		begin
			Debug('Compression not supported');
			{if UnpackMMCMP(Stream) then
				Result := True;
			else}
				Exit;
		end;

		SetDefaultMIDIDataArea;

		Stream.Seek(0, soBeginning);
		case GetModuleType(Stream) of
			FORMAT_IT:  Result := LoadIT(Stream);
			FORMAT_S3M: Result := LoadS3M(Stream);
		end;

		if Result then
		begin
			Stop;
			Driver.SetMixVolume(Header.MixVolume);
			Driver.FixSamples;
		end;
	end;

	Loaded := Result;
end;

function TITModule.LoadFromFile(const Filename: String): Boolean;
var
	Stream: TFileStream;
begin
	if Loaded then
		FreeSong;

	Result := False;
	try
		Stream := TFileStream.Create(Filename, fmOpenRead);
		if Stream.Size > 0 then
			Result := LoadFromStream(Stream);
	finally
		Stream.Free;
	end;
end;

procedure TITModule.Update;
var
	i: Integer;
	sc: TSlaveChannel;
begin
	for i := 0 to MAX_SLAVE_CHANNELS-1 do
	begin
		sc := SlaveChannels[i];
		if (sc = nil) or (not sc.Flags.SF_CHAN_ON) then
			Continue;

		if sc.Vol <> sc.VolSet then
		begin
			sc.Vol := sc.VolSet;
			sc.Flags.SF_RECALC_VOL := True;
		end;

		if sc.Frequency <> sc.FrequencySet then
		begin
			sc.Frequency := sc.FrequencySet;
			sc.Flags.SF_FREQ_CHANGE := True;
		end;
	end;

	UpdateData;

	if Header.Flags.ITF_INSTR_MODE then
		UpdateInstruments
	else
		UpdateSamples;
end;

procedure TITModule.FillAudioBuffer(Buffer: PInt16; NumSamples: Cardinal);
begin
	if Buffer = nil then Exit;

	if (not Self.Playing) or (Driver = nil) then {or (WAVRender_Flag)}
		FillDWord(Buffer, NumSamples, 0)
	else
		Driver.Mix(NumSamples, Buffer);
end;

procedure TITModule.Stop;
var
	i: Integer;
	hc: THostChannel;
	sc: TSlaveChannel;
begin
	Playing := False;

	LockMixer;

	MIDITranslate(nil, SlaveChannels[0], MIDICOMMAND_STOP);

	DecodeExpectedPattern := $FFFE;
	DecodeExpectedRow     := $FFFE;
	RowDelay     := 1;
	RowDelayOn   := False;
	CurrentRow   := 0;
	CurrentOrder := 0;
	CurrentTick  := 1;
	BreakRow     := 0;

	// clear host/slave channels

	for i := 0 to MAX_HOST_CHANNELS-1 do
	begin
		HostChannels[i].Free;
		hc := THostChannel.Create;
		hc.HostChnNum := i;
		// 8bb: set initial channel pan and channel vol
		hc.ChnPan := Header.ChnlPan[i] and $7F;
		hc.ChnVol := Header.ChnlVol[i];
		HostChannels[i] := hc;
	end;

	for i := 0 to MAX_SLAVE_CHANNELS-1 do
	begin
		SlaveChannels[i].Free;
		sc := TSlaveChannel.Create;
		sc.Flags.SF_NOTE_STOP := True;
		SlaveChannels[i] := sc;
	end;

	if Loaded then
	begin
		GlobalVolume := Header.GlobalVol;
		ProcessTick  := Header.InitialSpeed;
		CurrentSpeed := Header.InitialSpeed;
		Tempo        := Header.InitialTempo;
		InitTempo;
	end;

	UnlockMixer;

	if Assigned(OnPlayback) then
		OnPlayback(Self, False);
end;

function TITModule.Play(Order: Word): Boolean;
begin
	if Loaded then
	begin
		Stop;

		MIDITranslate(nil, SlaveChannels[0], MIDICOMMAND_START); // 8bb: this will reset channel filters

		CurrentOrder := Order;
		ProcessOrder := Order - 1;
		ProcessRow := $FFFE;

		// 8bb: reset seed (IT2 only does this at tracker startup, but let's do it here)
		RandSeed1 := $1234;
		RandSeed2 := $5678;

		// 8bb: clear MIDI filter interpretor state
		MIDIInterpretState := 0;
		MIDIInterpretType  := 0;

		Driver.ResetMixer;

		// don't try playing empty orderlist
		Playing := Orders[0] <= Header.PatNum;
	end
	else
		Playing := False;

	Result := Playing;

	if (Result) and (Assigned(OnPlayback)) then
		OnPlayback(Self, True);
end;

procedure TITModule.FreeSong;
var
	i: Integer;
begin
	Stop;
	Loaded := False;

	SongMessage.Clear;

	for i := 0 to MAX_HOST_CHANNELS-1 do
		FreeAndNil(HostChannels[i]);

	for i := 0 to MAX_SLAVE_CHANNELS-1 do
		FreeAndNil(SlaveChannels[i]);

	for i := 0 to MAX_PATTERNS-1 do
		ReleasePattern(i);

	for i := 0 to MAX_SAMPLES-1 do
		if Samples[i] <> nil then
			FreeAndNil(Samples[i]);

	for i := 0 to MAX_INSTRUMENTS-1 do
		if Instruments[i] <> nil then
			FreeAndNil(Instruments[i]);
end;

constructor TITModule.Create;
var
	i: Integer;
begin
	inherited Create;

	InitCommandTable := [
		@InitNoCommand, @InitCommandA,  @InitCommandB,  @InitCommandC,
		@InitCommandD,  @InitCommandE,  @InitCommandF,  @InitCommandG,
		@InitCommandH,  @InitCommandI,  @InitCommandJ,  @InitCommandK,
		@InitCommandL,  @InitCommandM,  @InitCommandN,  @InitCommandO,
		@InitCommandP,  @InitCommandQ,  @InitCommandR,  @InitCommandS,
		@InitCommandT,  @InitCommandU,  @InitCommandV,  @InitCommandW,
		@InitCommandX,  @InitCommandY,  @InitCommandZ,  @InitNoCommand,
		@InitNoCommand, @InitNoCommand, @InitNoCommand, @InitNoCommand
	];
	CommandTable := [
		@NoCommand, @NoCommand, @NoCommand, @NoCommand, @CommandD,  @CommandE,
		@CommandF,  @CommandG,  @CommandH,  @CommandI,  @CommandJ,  @CommandK,
		@CommandL,  @NoCommand, @CommandN,  @NoCommand, @CommandP,  @CommandQ,
		@CommandR,  @CommandS,  @CommandT,  @CommandH,  @NoCommand, @CommandW,
		@NoCommand, @CommandY,  @NoCommand, @NoCommand, @NoCommand, @NoCommand
	];
	VolumeEffectTable := [
		@NoCommand,      @NoCommand,      @VolumeCommandC, @VolumeCommandD,
		@VolumeCommandE, @VolumeCommandF, @VolumeCommandG, @CommandH
	];

	SongMessage := TStringList.Create;
	EmptyPattern := TPattern.Create;

	for i := 0 to MAX_SAMPLES-1 do
		Samples[i] := TSample.Create(Self);

	for i := 0 to MAX_INSTRUMENTS-1 do
		Instruments[i] := TInstrument.Create;

	Driver := nil;
end;

function TITModule.Init(DriverType: TAudioDriverType;
	MixingFrequency: Word; MixingBufferSize: Cardinal = 0): Boolean;
begin
	if Driver <> nil then
	begin
		CloseMixer;
		FreeAndNil(Driver);
	end;

	if DriverType = DRIVER_DEFAULT then
		DriverType := DRIVER_SB16MMX; // !!! update when adding better driver

	case DriverType of
		DRIVER_WAVWRITER: ;
		DRIVER_SB16:      Driver := TITAudioDriver_SB16.Create(Self, MixingFrequency);
		DRIVER_SB16MMX:   Driver := TITAudioDriver_SB16MMX.Create(Self, MixingFrequency);
		DRIVER_HQ:        ;
		else              ;
	end;

	Stop;

	Result := (Driver <> nil);
	if Result then
		OpenMixer(MixingFrequency, MixingBufferSize);
end;

destructor TITModule.Destroy;
begin
	FreeSong;

	SongMessage.Free;
	EmptyPattern.Free;

	Driver.Free;
	CloseMixer;

	inherited Destroy;
end;


// ================================================================================================
// instrument support
// ================================================================================================


function TITModule.UpdateEnvelope(var env: TEnv; var envState: TEnvState; SustainReleased: Boolean): Boolean;
var
	LoopBegin, LoopEnd: Byte;
	Looping, HasLoop, HasSustainLoop: Boolean;
	NextNode, Delta, TickDelta: Int16;
begin
	Result := False;

	if envState.Tick < envState.NextTick then
	begin
		envState.Tick  += 1;
		envState.Value += envState.Delta;
		Exit; // 8bb: last node not reached
	end;

	envState.Value := env.NodePoints[envState.CurNode and $00FF].Magnitude << 16;
	NextNode := (envState.CurNode and $00FF) + 1;

	if (env.Flags.ENVF_LOOP) or (env.Flags.ENVF_SUSTAINLOOP) then // 8bb: any loop at all?
	begin
		LoopBegin      := env.LoopBegin;
		LoopEnd        := env.LoopEnd;
		HasLoop        := env.Flags.ENVF_LOOP;
		HasSustainLoop := env.Flags.ENVF_SUSTAINLOOP;
		Looping := True;

		if HasSustainLoop then
		begin
			if not SustainReleased then
			begin
				LoopBegin := env.SustainLoopBegin;
				LoopEnd   := env.SustainLoopEnd;
			end
			else
			if not HasLoop then
				Looping := False;
		end;

		if (Looping) and (NextNode > LoopEnd) then
		begin
			envState.CurNode  := (envState.CurNode and $FF00) or LoopBegin;
			envState.Tick     := env.NodePoints[envState.CurNode and $00FF].Tick;
			envState.NextTick := envState.Tick;
			Exit; // 8bb: last node not reached
		end;
	end;

	if NextNode >= env.Num then
		Exit(True); // 8bb: last node reached

	// 8bb: new node

	envState.NextTick := env.NodePoints[NextNode].Tick;
	envState.Tick     := env.NodePoints[envState.CurNode and $00FF].Tick + 1;

	TickDelta := envState.NextTick - env.NodePoints[envState.CurNode and $00FF].Tick;
	if TickDelta = 0 then TickDelta := 1;

	Delta := env.NodePoints[NextNode].Magnitude - env.NodePoints[envState.CurNode and $00FF].Magnitude;
	envState.Delta   := (Delta << 16) div TickDelta;
	envState.CurNode := (envState.CurNode and $FF00) or (Byte(NextNode) and $FF);
end;

procedure TITModule.UpdateInstruments;
var
	i, p: Integer;
	EnvVal: Int16;
	SustainReleased, HandleNoteFade, TurnOffCh: Boolean;
	volume: Word;
	PanVal, PanEnvVal: Int8;
	ins: TInstrument;
	sc: TSlaveChannel;
begin
	for i := 0 to MAX_SLAVE_CHANNELS-1 do
	begin
		sc := SlaveChannels[i];
		if not sc.Flags.SF_CHAN_ON then Continue;

		if sc.Ins <> $FF then // 8bb: got an instrument?
		begin
			ins := sc.Instrument;
			SustainReleased := sc.Flags.SF_NOTE_OFF;

			// 8bb: handle pitch/filter envelope

			if sc.Flags.SF_PITCHENV_ON then
			begin
				if UpdateEnvelope(ins.PitchEnv, sc.PitchEnvState, SustainReleased) then // 8bb: last node reached?
					sc.Flags.SF_PITCHENV_ON := False;
			end;

			if not ins.PitchEnv.Flags.ENVF_TYPE_FILTER then // 8bb: pitch envelope
			begin
				//EnvVal := SarLongint(sc.PitchEnvState.Value, 8);
				//EnvVal := (Int16)((Cardinal)sc.PitchEnvState.Value >> 8);
				EnvVal := sc.PitchEnvState.Value div 256;
				EnvVal := SarSmallint(EnvVal, 3); // 8bb: arithmetic shift

				if EnvVal <> 0 then
				begin
					PitchSlideLinear(sc.HostChannel, sc, EnvVal);
					sc.Flags.SF_FREQ_CHANGE := True;
				end;
			end
			else
			if sc.Smp <> 100 then // 8bb: filter envelope
			begin
				//EnvVal := SarLongint(sc.PitchEnvState.Value, 8);
				//EnvVal := (Int16)((Cardinal)sc.PitchEnvState.Value >> 8);
				EnvVal := sc.PitchEnvState.Value div 256;
				EnvVal := SarSmallint(EnvVal, 6); // 8bb: arithmetic shift, -128..128 (though -512..511 is in theory possible)

				// 8bb: Annoying upper-clamp logic.
				//
				// Original asm code:
				//  add bx,128
				//  cmp bh,1
				//  adc bl,-1
				//
				// The code below is confirmed to be correct
				// for the whole -512..511 range.
				//
				// However, EnvVal should only be -128..128
				// (0..256 after +128 add) unless something
				// nasty is going on. Let's still implement
				// this behavior, just in case.
				//
				EnvVal += 128;
				if (EnvVal and $FF00) <> 0 then
					EnvVal -= 1;

				sc.MIDIBank := (sc.MIDIBank and $FF00) or (EnvVal and $FF); // 8bb: don't mess with upper byte!
				sc.Flags.SF_UPDATE_MIXERVOL := True;
			end;

			if sc.Flags.SF_PANENV_ON then
			begin
				sc.Flags.SF_RECALC_PAN := True;
				if UpdateEnvelope(ins.PanEnv, sc.PanEnvState, SustainReleased) then // 8bb: last node reached?
					sc.Flags.SF_PANENV_ON := False;
			end;

			HandleNoteFade := False;
			TurnOffCh      := False;

			if sc.Flags.SF_VOLENV_ON then // Volume envelope on?
			begin
				sc.Flags.SF_RECALC_VOL := True;

				if UpdateEnvelope(ins.VolEnv, sc.VolEnvState, SustainReleased) then // 8bb: last node reached?
				begin
					// Envelope turned off...
					sc.Flags.SF_VOLENV_ON := False;

					if (sc.VolEnvState.Value and $00FF0000) = 0 then // Turn off if end of loop is reached (8bb: last env. point is zero?)
						TurnOffCh := True
					else
					begin
						sc.Flags.SF_FADEOUT := True;
						HandleNoteFade := True;
					end;
				end
				else
				begin
					if not sc.Flags.SF_FADEOUT then // Note fade on?
					begin
						// Now, check if loop + sustain off
						if (SustainReleased) and (ins.VolEnv.Flags.ENVF_LOOP) then // Normal vol env loop?
						begin
							sc.Flags.SF_FADEOUT := True;
							HandleNoteFade := True;
						end;
					end
					else
						HandleNoteFade := True;
				end;
			end
			else
			if sc.Flags.SF_FADEOUT then // Note fade??
			begin
				HandleNoteFade := True;
			end
			else
			if sc.Flags.SF_NOTE_OFF then // Note off issued?
			begin
				sc.Flags.SF_FADEOUT := True;
				HandleNoteFade := True;
			end;

			if HandleNoteFade then
			begin
				sc.FadeOut -= ins.FadeOut;
				if Int16(sc.FadeOut) <= 0 then
				begin
					sc.FadeOut := 0;
					TurnOffCh := True;
				end;
				sc.Flags.SF_RECALC_VOL := True;
			end;

			if TurnOffCh then
			begin
				if (sc.HostChnNum and CHN_DISOWNED) = 0 then
				begin
					sc.HostChnNum := sc.HostChnNum or CHN_DISOWNED; // Host channel exists
					sc.HostChannel.Flags.HF_CHAN_ON := False;
				end;
				sc.Flags.SF_RECALC_VOL := True;
				sc.Flags.SF_NOTE_STOP  := True;
			end;
		end;

		if sc.Flags.SF_RECALC_VOL then // Calculate volume
		begin
			sc.Flags.SF_RECALC_VOL := False;
			sc.Flags.SF_UPDATE_MIXERVOL := True;

			volume := (sc.Vol * sc.ChnVol * sc.FadeOut) >> 7;
			volume := (volume * sc.SmpVol) >> 7;
			volume := (volume * (sc.VolEnvState.Value div 256)) >> 14;
			volume := (volume * GlobalVolume) >> 7;
			Assert(volume <= 32768);

			sc.FinalVol32768 := volume;      // 8bb: 0..32768
			sc.FinalVol128   := volume >> 8; // 8bb: 0..128
		end;

		if sc.Flags.SF_RECALC_PAN then // Change in panning?
		begin
			sc.Flags.SF_RECALC_PAN := False;
			sc.Flags.SF_PAN_CHANGED := True;

			if sc.Pan = PAN_SURROUND then
				sc.FinalPan := sc.Pan
			else
			begin
				PanVal := 32 - sc.Pan;
				if PanVal < 0 then
				begin
					PanVal := PanVal xor 255;
					PanVal -= 255;
				end;
				PanVal := -PanVal;
				PanVal += 32;

				PanEnvVal := SarLongint(sc.PanEnvState.Value, 16);
				PanVal := sc.Pan + SarLongInt(Int32(PanVal * PanEnvVal), 5);
				PanVal -= 32;

//				sc.FinalPan := ((PanVal * (Header.PanSep >> 1)) >> 6) + 32; // 8bb: 0..64 !!!
				p := PanVal;
				p *= (Header.PanSep div 2);
				sc.FinalPan := p >> 6 + 32; // 8bb: 0..64
				Assert(sc.FinalPan <= 64);
			end;
		end;

		UpdateAutoVibrato(sc);
	end;
end;

procedure TITModule.InitPlayInstrument(hc: THostChannel; sc: TSlaveChannel; ins: TInstrument);
var
	pan, filterQ: Byte;
	newPan: Int16;
	S: TSample;
	lastSC: TSlaveChannel;
begin
	Assert((hc <> nil) and (sc <> nil) and (ins <> nil));

	sc.Instrument := ins;

	sc.NNA := ins.NNA;
	sc.DCT := ins.DCT;
	sc.DCA := ins.DCA;

	if hc.MIDIChn <> 0 then // 8bb: MIDI?
	begin
		sc.MIDIChn  := ins.MIDIChn;
		sc.MIDIProg := ins.MIDIProg;
		sc.MIDIBank := ins.MIDIBank;
		sc.LoopDirection := hc.RawNote; // 8bb: during MIDI, LpD = MIDI note
	end;

	sc.ChnVol := hc.ChnVol;

	pan := IfThen(ins.DefPan and $80 <> 0, hc.ChnPan, ins.DefPan);
	if hc.Smp <> 0 then
	begin
		S := Samples[hc.Smp-1];
		if s.DefPan and $80 <> 0 then
			pan := s.DefPan and 127;
	end;

	if pan <> PAN_SURROUND then
	begin
		//newPan := pan + (((int8_t)(hc.RawNote - ins.PitchPanCenter) * (int8_t)ins.PitchPanSep) >> 3);
		newPan := pan + (((hc.RawNote - ins.PitchPanCenter) * ins.PitchPanSep)>> 3);
		if newPan < 0  then newPan := 0 else
		if newPan > 64 then newPan := 64;
		pan := newPan;
	end;

	sc.Pan    := pan;
	sc.PanSet := pan;

	// Envelope init
	sc.VolEnvState.Value := 64 << 16; // 8bb: clears fractional part
	sc.VolEnvState.Tick     := 0;
	sc.VolEnvState.NextTick := 0;
	sc.VolEnvState.CurNode  := 0;

	sc.PanEnvState.Value := 0; // 8bb: clears fractional part
	sc.PanEnvState.Tick     := 0;
	sc.PanEnvState.NextTick := 0;
	sc.PanEnvState.CurNode  := 0;

	sc.PitchEnvState.Value := 0; // 8bb: clears fractional part
	sc.PitchEnvState.Tick     := 0;
	sc.PitchEnvState.NextTick := 0;
	sc.PitchEnvState.CurNode  := 0;

	sc.Flags.WordAccess := 0;
	sc.Flags.SF_CHAN_ON     := True;
	sc.Flags.SF_RECALC_PAN  := True;
	sc.Flags.SF_RECALC_VOL  := True;
	sc.Flags.SF_FREQ_CHANGE := True;
	sc.Flags.SF_NEW_NOTE    := True;

	if ins.VolEnv.Flags.ENVF_ENABLED   then sc.Flags.SF_VOLENV_ON   := True;
	if ins.PanEnv.Flags.ENVF_ENABLED   then sc.Flags.SF_PANENV_ON   := True;
	if ins.PitchEnv.Flags.ENVF_ENABLED then sc.Flags.SF_PITCHENV_ON := True;

	if LastSlaveChannel <> nil then
	begin
		lastSC := LastSlaveChannel;

		if (ins.VolEnv.Flags.ENVF_ENABLED) and (ins.VolEnv.Flags.ENVF_CARRY) then // Transfer volume data
		begin
			sc.VolEnvState.Value := lastSC.VolEnvState.Value;
			sc.VolEnvState.Delta := lastSC.VolEnvState.Delta;
			sc.VolEnvState.Tick  := lastSC.VolEnvState.Tick;
			sc.VolEnvState.CurNode  := lastSC.VolEnvState.CurNode;
			sc.VolEnvState.NextTick := lastSC.VolEnvState.NextTick;
		end;

		if (ins.PanEnv.Flags.ENVF_ENABLED) and (ins.PanEnv.Flags.ENVF_CARRY) then // Transfer pan data
		begin
			sc.PanEnvState.Value := lastSC.PanEnvState.Value;
			sc.PanEnvState.Delta := lastSC.PanEnvState.Delta;
			sc.PanEnvState.Tick  := lastSC.PanEnvState.Tick;
			sc.PanEnvState.CurNode  := lastSC.PanEnvState.CurNode;
			sc.PanEnvState.NextTick := lastSC.PanEnvState.NextTick;
		end;

		if (ins.PitchEnv.Flags.ENVF_ENABLED) and (ins.PitchEnv.Flags.ENVF_CARRY) then // Transfer pitch data
		begin
			sc.PitchEnvState.Value := lastSC.PitchEnvState.Value;
			sc.PitchEnvState.Delta := lastSC.PitchEnvState.Delta;
			sc.PitchEnvState.Tick  := lastSC.PitchEnvState.Tick;
			sc.PitchEnvState.CurNode  := lastSC.PitchEnvState.CurNode;
			sc.PitchEnvState.NextTick := lastSC.PitchEnvState.NextTick;
		end;
	end;

	hc.Flags.HF_APPLY_RANDOM_VOL := True; // Apply random volume/pan

	if hc.MIDIChn = 0 then
	begin
		sc.MIDIBank := $00FF; // 8bb: reset filter resonance (Q) and cutoff

		if (ins.FilterCutoff and $80) <> 0 then // If IFC bit 7 = 1, then set filter cutoff
		begin
			SetFilterCutoff(hc, sc, ins.FilterCutoff and $7F);
		end;

		if (ins.FilterResonance and $80) <> 0 then // If IFR bit 7 = 1, then set filter resonance
		begin
			filterQ := ins.FilterResonance and $7F;
			sc.MIDIBank := (filterQ << 8) or (sc.MIDIBank and $00FF);
			SetFilterResonance(hc, sc, filterQ);
		end;
	end;
end;

function TITModule.AllocateChannelInstrument(hc: THostChannel; sc: TSlaveChannel; ins: TInstrument;
	var hcFlags: THostChannelFlags): TSlaveChannel;
var
	S: TSample;
begin
	Result := nil;
	Assert((hc <> nil) and (sc <> nil) and (ins <> nil));

	hc.SlaveChannel := sc;
	sc.HostChannel  := hc;
	sc.HostChnNum   := hc.HostChnNum;

	sc.SmpIs16Bit := False;
	sc.AutoVibratoDepth := 0;
	sc.AutoVibratoPos   := 0;
	sc.LoopDirection := DIR_FORWARDS; // reset loop dir

	InitPlayInstrument(hc, sc, ins);

	sc.SmpVol  := ins.GlobVol;
	sc.FadeOut := 1024;
	sc.Ins  := hc.Ins;
	sc.Note := IfThen(hc.Smp = 101, hc.TranslatedNote, hc.RawNote);

	if hc.Smp = 0 then // shut down channel
	begin
		sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
		hcFlags.HF_CHAN_ON := False;
		Exit;
	end;

	sc.Smp := hc.Smp-1;
	S := Samples[sc.Smp];
	sc.Sample := S;

	if (S.Length = 0) or (not s.Flags.SMPF_ASSOCIATED_WITH_HEADER) then // shut down channel
	begin
		sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
		hcFlags.HF_CHAN_ON := False;
		Exit;
	end;

	sc.SmpIs16Bit := s.Flags.SMPF_16BIT;
	sc.SmpVol := (s.GlobVol * sc.SmpVol) >> 6; // 0.128

	Result := sc;
end;


// ================================================================================================
// mixer callbacks
// ================================================================================================


function TITModule.OpenMixer(MixingFrequency, MixingBufferSize: Word): Boolean;
begin
	if Assigned(OnOpenMixer) then
		Result := OnOpenMixer(Self, MixingFrequency, MixingBufferSize)
	else
		Result := False;
end;

// disable or enable mixing while buffer is being processed
procedure TITModule.LockMixer;
begin
	if Assigned(OnLockMixer) then
		OnLockMixer(Self, True);
end;

// enables mixing again
procedure TITModule.UnlockMixer;
begin
	if Assigned(OnLockMixer) then
		OnLockMixer(Self, False);
end;

procedure TITModule.CloseMixer;
begin
	if Assigned(OnCloseMixer) then
		OnCloseMixer(Self);
end;

end.
