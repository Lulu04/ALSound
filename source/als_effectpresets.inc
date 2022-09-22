{$ifdef ALS_INTERFACE}
//
// sound effects properties and presets
//

{
   You'll found here some effect's presets.
   Made by the author of ALSound with low quality headphones and
   and low quality ears... They can surely be improved.
}

type

  { TALSAutoWahProperties }

  TALSAutoWahProperties = record
    AttackTime,       // Min 0.0001  Max 1.0  Default 0.06
    ReleaseTime,      // Min 0.0001  Max 1.0  Default 0.06
    Resonance,        // Min 2.0  Max 1000.0  Default 1000.0
    PeakGain: cfloat; // Min 0.00003  Max 31621.0  Default 11.22
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  TALSChorusWaveform = ( cwSinusoid=0, cwTriangle );

  { TALSChorusProperties }

  TALSChorusProperties = record
    Waveform: TALSChorusWaveform; // Default cwTriangle
    Phase: ALint;     // Min -180  Max 180  Default 90
    Rate,            // Min 0.0  Max 10.0  Default 1.0
    Depth,           // Min 0.0  Max 1.0  Default 0.1
    Feedback,        // Min -1.0  Max 1.0  Default 0.25
    Delay: cfloat;   // Min 0.0  Max 0.016  Default 0.016
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  TALSFlangerWaveform = ( fwSinusoid=0, fwTriangle );

  { TALSFlangerProperties }

  TALSFlangerProperties = record
    Waveform: TALSFlangerWaveform;
    Phase: ALint;      // Min -180  Max 180  Default 0
    Rate,             // Min 0.0  Max 10.0  Default 0.27
    Depth,            // Min 0.0  Max 1.0  Default 1.0
    Feedback,         // Min -1.0  Max 1.0  Default -0.5
    Delay: cfloat;    // Min 0.0  Max 0.004  Default 0.002
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  { TALSCompressorProperties }

  TALSCompressorProperties = record
    OnOff: ALint;        // 0 or 1
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  { TALSDistortionProperties }

  TALSDistortionProperties = record
    Edge,                // Min 0.0  Max 1.0  Default 0.2
    Gain,                // Min 0.1  Max 1.0  Default 0.05
    LowpassCutoff,       // Min 80.0  Max 24000.0  Default 8000.0
    EQCenter,            // Min 80.0  Max 24000.0  Default 3600.0
    EQBandwidth: cfloat; // Min 80.0  Max 24000.0  Default 3600.0
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  { TALSEchoProperties }

  TALSEchoProperties = record
    Delay,          // Min 0.0  Max 0.207  Default 0.1
    LRDelay,        // Min 0.0  Max 0.404  Default 0.1
    Damping,        // Min 0.0  Max 0.99  Default 0.5
    Feedback,       // Min 0.0  Max 1.0  Default 0.5
    Spread: cfloat; // Min -1.0  Max 1.0  Default -1.0
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  { TALSEqualizerProperties }

  TALSEqualizerProperties = record
    LowGain,            // Min 0.126  Max 7.943  Default 1.0
    LowCutoff,          // Min 50.0  Max 800.0  Default 200.0
    Mid1Gain,           // Min 0.126  Max 7.943  Default 1.0
    Mid1Center,         // Min 200.0  Max 3000.0  Default 500.0
    Mid1Width,          // Min 0.01  Max 1.0  Default 1.0
    Mid2Gain,           // Min 0.126  Max 7.943  Default 1.0
    Mid2Center,         // Min 1000.0  Max 8000.0  Default 3000.0
    Mid2Width,          // Min 0.01  Max 1.0  Default 1.0
    HighGain,           // Min 0.126  Max 7.943  Default 1.0
    HighCutoff: cfloat; // Min 4000.0  Max 16000.0  Default 6000.0
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  TALSFreqShifterDirection = ( sdDown=0, sdUp, sdOff );

  { TALSFreqShifterProperties }

  TALSFreqShifterProperties = record
    Frequency: cfloat;   // Min 0.0  Max 24000.0  Default 0.0
    LeftDirection,
    RightDirection: TALSFreqShifterDirection;
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;


  { TALSPitchShifterProperties }

  TALSPitchShifterProperties = record
    CoarseTune,      // Min -12  Max 12  Default 12
    FineTune: ALint; // Min -50  Max 50  Default 0
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;

  TALSRingModulatorWaveform = ( rmwSinusoid=0, rmwSawtooth, rmwSquare );

  { TALSRingModulatorProperties }

  TALSRingModulatorProperties = record
    Frequency,              // Min 0.0  Max 8000.0  Default 440.0
    HighPassCutoff: cfloat; // Min 0.0  Max 24000.0  Default 800.0
    Waveform: TALSRingModulatorWaveform;
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;

  TALSVMorpherPhoneme = ( phoA=0, phoE, phoI, phoO, phoU,
                          phoAA, phoAE, phoAH, phoAO, phoEH, phoER, phoIH,
                          phoIY, phoUH, phoUW, phoB, phoD, phoF, phoG, phoJ,
                          phoK, phoL, phoM, phoN, phoP, phoR, phoS, phoT,
                          phoV, phoZ );
  TALSVMorpherWaveform  = ( vmwSinusoid=0, vmwTriangle, vmwSawtooth );

  { TALSVocalMorpherProperties }

  TALSVocalMorpherProperties = record
    PhonemeA,
    PhonemeB: TALSVMorpherPhoneme;
    PhonemeACoarseTuning, // Min -24  Max 24  Default 0
    PhonemeBCoarseTuning: ALint; // Min -24  Max 24  Default 0
    Waveform: TALSVMorpherWaveform;
    Rate: cfloat;  // Min 0.0  Max 10.0  Default 1.41
    procedure InitDefault;
    function PhonemeList: TStringArray;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
  end;

  TALSDedicatedProperties = record
    Gain: cfloat;
  end;

  { TALSReverbProperties }

  TALSReverbProperties = record
    Density,    // Min 0.0  Max 1.0  Default 1.0
    Diffusion,  // Min 0.0  Max 1.0  Default 1.0
    Gain,       // Min 0.0  Max 1.0  Default 0.32
    GainHF,     // Min 0.0  Max 1.0  Default 0.89
    DecayTime,  // Min 0.1  Max 20.0  Default 1.49
    DecayHFRatio, // Min 0.1  Max 2.0  Default 0.83
    ReflectionsGain, // Min 0.0  Max 3.16  Default 0.05
    ReflectionsDelay, // Min 0.0  Max 0.3  Default 0.007
    LateReverbGain, // Min 0.0  Max 10.0  Default 1.26
    LateReverbDelay, // Min 0.0  Max 0.1  Default 0.011
    AirAbsorptionGainHF,// Min 0.892  Max 1.0  Default 0.994
    RoomRolloffFactor: cfloat; // Min 0.0  Max 10.0  Default 0.0
    DecayHFLimit: ALint; // 0 or 1
    procedure InitDefault;
    function PresetList: TStringArray;
    procedure InitWithPreset(aIndex: integer);
    procedure InitWithEAXPreset(aIndex: integer);
  end;



const
// Preset AutoWah
  AUTOWAH_PRESET_DEFAULT: TALSAutoWahProperties=
    ( AttackTime: 0.06;
      ReleaseTime: 0.06;
      Resonance: 1000.0;
      PeakGain: 11.22 );

  AUTOWAH_PRESET_OLD_RADIO: TALSAutoWahProperties=
    ( AttackTime: 0.4131;
      ReleaseTime: 0.1631;
      Resonance: 249.5040;
      PeakGain: 885.38800 );

  AUTOWAH_PRESET_WIND_IMPACT: TALSAutoWahProperties=
    ( AttackTime: 1.0000;
      ReleaseTime: 0.0061;
      Resonance: 968.0640;
      PeakGain: 18213.69531 );

  AUTOWAH_PRESET_VOCAL_DELIRIUM: TALSAutoWahProperties=
    ( AttackTime: 0.0301;
      ReleaseTime: 0.0131;
      Resonance: 1000.0000;
      PeakGain: 15019.97461 );

  AUTOWAH_PRESET_ROCKY1: TALSAutoWahProperties=
    ( AttackTime: 0.6000;
      ReleaseTime: 0.1651;
      Resonance: 465.0720;
      PeakGain: 27668.37500 );

  AUTOWAH_PRESET_ROCKY2: TALSAutoWahProperties=
    ( AttackTime: 0.06;
      ReleaseTime: 0.05;
      Resonance: 500;
      PeakGain: 30000 );

  AUTOWAH_PRESET_VOCAL_SUPPORT: TALSAutoWahProperties=
    ( AttackTime: 0.0551;
      ReleaseTime: 0.5570;
      Resonance: 608.7840;
      PeakGain: 15620.77441 );

// Preset Chorus
  CHORUS_PRESET_DEFAULT: TALSChorusProperties=
    ( Waveform: cwTriangle;
      Phase: 90;
      Rate: 1.0;
      Depth: 0.1;
      Feedback: 0.25;
      Delay: 0.016 );

  CHORUS_PRESET_ACOUSTIC_DUET: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: 75;
      Rate: 3.3500;
      Depth: 0.2160;
      Feedback: -0.10600;
      Delay: 0.00182 );

  CHORUS_PRESET_AMATEUR: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: 3;
      Rate: 1.5700;
      Depth: 0.5700;
      Feedback: 0.77200;
      Delay: 0.00467 );

  CHORUS_PRESET_FLYING_SAUCER: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: -90;
      Rate: 6.0400;
      Depth: 0.3070;
      Feedback: 0.20400;
      Delay: 0.01248 );

  CHORUS_PRESET_DUO: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: 8;
      Rate: 1.0400;
      Depth: 0.2670;
      Feedback: 0.20400;
      Delay: 0.01434 );

  CHORUS_PRESET_ELECTRO_VOICE: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: 8;
      Rate: 9.3200;
      Depth: 0.0530;
      Feedback: 0.19400;
      Delay: 0.01502 );

  CHORUS_PRESET_60SPSYCHEDELIC: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: 8;
      Rate: 9.5800;
      Depth: 0.4490;
      Feedback: -0.75000;
      Delay: 0.00182 );

  CHORUS_PRESET_TRIPPYVOX: TALSChorusProperties=
    ( Waveform: cwTriangle;
      Phase: 52;
      Rate: 1.3600;
      Depth: 0.2200;
      Feedback: -0.00400;
      Delay: 0.00730 );

  CHORUS_PRESET_RICH: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: -87;
      Rate: 5.8500;
      Depth: 0.2610;
      Feedback: 0.36800;
      Delay: 0.00682 );

  CHORUS_PRESET_WIDE: TALSChorusProperties=
    ( Waveform: cwSinusoid;
      Phase: 12;
      Rate: 6.1700;
      Depth: 0.0610;
      Feedback: 0.67000;
      Delay: 0.01482 );

// Preset Flanger
  FLANGER_PRESET_DEFAULT: TALSFlangerProperties=
    ( Waveform: fwTriangle;
      Phase: 0;
      Rate: 0.27;
      Depth: 1.0;
      Feedback: -0.5;
      Delay: 0.002 );

  FLANGER_PRESET_AURAL: TALSFlangerProperties=
    ( Waveform: fwTriangle;
      Phase: 48;
      Rate: 5.6600;
      Depth: 0.9030;
      Feedback: 0.64400;
      Delay: 0.00230 );

  FLANGER_PRESET_RICH_VOICE: TALSFlangerProperties=
    ( Waveform: fwTriangle;
      Phase: 123;
      Rate: 6.0000;
      Depth: 0.6820;
      Feedback: 0.00000;
      Delay: 0.00288 );

  FLANGER_PRESET_MULTI_VOICE: TALSFlangerProperties=
    ( Waveform: fwSinusoid;
      Phase: 90;
      Rate: 0.8700;
      Depth: 0.8790;
      Feedback: 0.46600;
      Delay: 0.00120 );

  FLANGER_PRESET_METALLIC: TALSFlangerProperties=
    ( Waveform: fwSinusoid;
      Phase: 55;
      Rate: 0.0600;
      Depth: 0.0020;
      Feedback: 0.90600;
      Delay: 0.004 );

  FLANGER_PRESET_NIGHTMARE: TALSFlangerProperties=
    ( Waveform: fwTriangle;
      Phase: -115;
      Rate: 1.3600;
      Depth: 0.4750;
      Feedback: 0.54600;
      Delay: 0.00144 );

  FLANGER_PRESET_ENHANCED_STEREO: TALSFlangerProperties=
    ( Waveform: fwTriangle;
      Phase: -115;
      Rate: 1.3600;
      Depth: 0.4110;
      Feedback: 0.22400;
      Delay: 0.00379 );

  FLANGER_PRESET_SUSTAIN: TALSFlangerProperties=
    ( Waveform: fwTriangle;
      Phase: 0;
      Rate: 8.8300;
      Depth: 0.1040;
      Feedback: 0.68200;
      Delay: 0.004 );

  FLANGER_PRESET_MEDIUM_FLANGER: TALSFlangerProperties=
    ( Waveform: fwSinusoid;
      Phase: 83;
      Rate: 0.1300;
      Depth: 0.4560;
      Feedback: 0.77200;
      Delay: 0.001536 );

  FLANGER_PRESET_STRONG_FLANGER: TALSFlangerProperties=
    ( Waveform: fwSinusoid;
      Phase: 83;
      Rate: 1.6100;
      Depth: 0.8260;
      Feedback: 0.77200;
      Delay: 0.001536 );

// Preset Distortion
  DISTORTION_PRESET_DEFAULT: TALSDistortionProperties=
    ( Edge: 0.2;
      Gain: 0.05;
      LowpassCutoff: 8000;
      EQCenter: 3600;
      EQBandwidth: 3600 );

  DISTORTION_PRESET_70S_FUZZGUITAR: TALSDistortionProperties=
    ( Edge: 0.25200;
      Gain: 0.26641;
      LowpassCutoff: 1515.20;
      EQCenter: 247.44;
      EQBandwidth: 1874.00 );

  DISTORTION_PRESET_HEAVY: TALSDistortionProperties=
    ( Edge: 0.29200;
      Gain: 1.00000;
      LowpassCutoff: 343.12;
      EQCenter: 3070.00;
      EQBandwidth: 6012.16 );

  DISTORTION_PRESET_DESTROY_BASS: TALSDistortionProperties=
    ( Edge: 0.33500;
      Gain: 1.00000;
      LowpassCutoff: 749.76;
      EQCenter: 223.52;
      EQBandwidth: 127.84 );

  DISTORTION_PRESET_SOFT: TALSDistortionProperties=
    ( Edge: 0.03000;
      Gain: 0.55153;
      LowpassCutoff: 3022.16;
      EQCenter: 1299.92;
      EQBandwidth: 1539.12 );

  DISTORTION_PRESET_MEDIUM: TALSDistortionProperties=
    ( Edge: 0.19300;
      Gain: 0.55153;
      LowpassCutoff: 3022.16;
      EQCenter: 1299.92;
      EQBandwidth: 1539.12 );

  DISTORTION_PRESET_HARD: TALSDistortionProperties=
    ( Edge: 0.59700;
      Gain: 0.81091;
      LowpassCutoff: 3380.96;
      EQCenter: 1395.60;
      EQBandwidth: 1539.12 );

// Preset Echo
  ECHO_PRESET_DEFAULT: TALSEchoProperties=
    ( Delay: 0.1;
      LRDelay: 0.1;
      Damping: 0.5;
      Feedback: 0.5;
      Spread: -1.0 );

  ECHO_PRESET_1950s_STYLE: TALSEchoProperties=
    ( Delay: 0.0079;
      LRDelay: 0.1325;
      Damping: 0.0000;
      Feedback: 0.1990;
      Spread: 0.0000 );

  ECHO_PRESET_OLD_SCHOOL: TALSEchoProperties=
    ( Delay: 0.0058;
      LRDelay: 0.0412;
      Damping: 0.8722;
      Feedback: 0.1330;
      Spread: 0.0000 );

  ECHO_PRESET_AUDITORIUM: TALSEchoProperties=
    ( Delay: 0.0470;
      LRDelay: 0.0077;
      Damping: 0.8603;
      Feedback: 0.1630;
      Spread: 0.1280 );

  ECHO_PRESET_CANYON: TALSEchoProperties=
    ( Delay: 0.2070;
      LRDelay: 0.4040;
      Damping: 0.9900;
      Feedback: 0.5490;
      Spread: 0.5600 );

  ECHO_PRESET_CHEAP_SPRINGS: TALSEchoProperties=
    ( Delay: 0.0544;
      LRDelay: 0.0529;
      Damping: 0.4703;
      Feedback: 0.7610;
      Spread: -0.6940 );

  ECHO_PRESET_DRAIN_PIPE: TALSEchoProperties=
    ( Delay: 0.1285;
      LRDelay: 0.0113;
      Damping: 0.9900;
      Feedback: 0.7220;
      Spread: 1.0000 );

  ECHO_PRESET_LIGHT_ECHO_HALL: TALSEchoProperties=
    ( Delay: 0.1246;
      LRDelay: 0.1248;
      Damping: 0.9900;
      Feedback: 0.6140;
      Spread: 1.0000 );

  ECHO_PRESET_DELAYED: TALSEchoProperties=
    ( Delay: 0.2070;
      LRDelay: 0.4040;
      Damping: 0.8603;
      Feedback: 0.0000;
      Spread: 1.0000 );

  ECHO_PRESET_MONO_VOCAL_1: TALSEchoProperties=
    ( Delay: 0.0211;
      LRDelay: 0.1248;
      Damping: 0.9900;
      Feedback: 0.0110;
      Spread: -0.0720 );

  ECHO_PRESET_MONO_VOCAL_2: TALSEchoProperties=
    ( Delay: 0.0211;
      LRDelay: 0.1248;
      Damping: 0.9900;
      Feedback: 0.3260;
      Spread: 0.0000 );

  ECHO_PRESET_ROBUST_SHORT: TALSEchoProperties=
    ( Delay: 0.1952;
      LRDelay: 0.0032;
      Damping: 0.2247;
      Feedback: 0.1440;
      Spread: 0.0000 );

  ECHO_PRESET_SMALL_ROOM: TALSEchoProperties=
    ( Delay: 0.0298;
      LRDelay: 0.0315;
      Damping: 0.7148;
      Feedback: 0.1530;
      Spread: 0.4660 );

  ECHO_PRESET_TAJ_MAHAL: TALSEchoProperties=
    ( Delay: 0.2070;
      LRDelay: 0.1850;
      Damping: 0.1653;
      Feedback: 0.3310;
      Spread: -1.0000 );

  ECHO_PRESET_DELIRIUM: TALSEchoProperties=
    ( Delay: 0.0097;
      LRDelay: 0.4024;
      Damping: 0.4703;
      Feedback: 0.7480;
      Spread: 0.9440 );

// Preset Equalizer
  EQUALIZER_PRESET_DEFAULT: TALSEqualizerProperties=
    ( LowGain: 1.0;
      LowCutoff: 200.0;
      Mid1Gain: 1.0;
      Mid1Center: 500.0;
      Mid1Width: 1.0;
      Mid2Gain: 1.0;
      Mid2Center: 3000.0;
      Mid2Width: 1.0;
      HighGain: 1.0;
      HighCutoff: 6000.0 );

  EQUALIZER_PRESET_BASSBOOST: TALSEqualizerProperties=
    ( LowGain: 2.5;
      LowCutoff: 200.0;
      Mid1Gain: 1.0;
      Mid1Center: 500.0;
      Mid1Width: 1.0;
      Mid2Gain: 1.0;
      Mid2Center: 3000.0;
      Mid2Width: 1.0;
      HighGain: 1.0;
      HighCutoff: 6000.0 );

  EQUALIZER_PRESET_HIGHBOOST: TALSEqualizerProperties=
    ( LowGain: 1.0;
      LowCutoff: 200.0;
      Mid1Gain: 1.0;
      Mid1Center: 500.0;
      Mid1Width: 1.0;
      Mid2Gain: 1.0;
      Mid2Center: 3000.0;
      Mid2Width: 1.0;
      HighGain: 2.0;
      HighCutoff: 6000.0 );

  EQUALIZER_PRESET_LOW_FIDELITY: TALSEqualizerProperties=
    ( LowGain: 0.126;
      LowCutoff: 800.0;
      Mid1Gain: 7.943;
      Mid1Center: 1149.2;
      Mid1Width: 0.6554;
      Mid2Gain: 7.943;
      Mid2Center: 2120.0;
      Mid2Width: 1.0;
      HighGain: 0.126;
      HighCutoff: 4000.0 );

// Preset Frequency Shifter
  FREQSHIFTER_PRESET_DEFAULT: TALSFreqShifterProperties=
    ( Frequency: 0.0;
      LeftDirection: sdDown;
      RightDirection: sdDown );

  FREQSHIFTER_PRESET_HOARSE_VOICE: TALSFreqShifterProperties=
    ( Frequency: 75.5;
      LeftDirection: sdDown;
      RightDirection: sdDown );

  FREQSHIFTER_PRESET_DUAL_VOICE: TALSFreqShifterProperties=
    ( Frequency: 17.0;
      LeftDirection: sdUp;
      RightDirection: sdUp );

  FREQSHIFTER_PRESET_MELODY_DESTRUCTOR: TALSFreqShifterProperties=
    ( Frequency: 302.5;
      LeftDirection: sdDown;
      RightDirection: sdDown );

  FREQSHIFTER_PRESET_RAP_3_VOICES: TALSFreqShifterProperties=
    ( Frequency: 30.5;
      LeftDirection: sdDown;
      RightDirection: sdUp );

// Preset pitch Shifter
  PITCHSHIFTER_PRESET_DEFAULT: TALSPitchShifterProperties=
    ( CoarseTune: 12;
      FineTune: 0 );

  PITCHSHIFTER_PRESET_VOICEDUO_BASS: TALSPitchShifterProperties=
    ( CoarseTune: -5;
      FineTune: 0 );

// Preset Ring Modulator
  RINGMODULATOR_PRESET_DEFAULT: TALSRingModulatorProperties=
    ( Frequency: 440.0;
      HighPassCutoff: 800.0;
      Waveform: rmwSinusoid );

  RINGMODULATOR_PRESET_GARGLE: TALSRingModulatorProperties=
    ( Frequency: 30.0;
      HighPassCutoff: 0.0;
      Waveform: rmwSinusoid );

  RINGMODULATOR_PRESET_CAT_IN_THROAT: TALSRingModulatorProperties=
    ( Frequency: 93.4;
      HighPassCutoff: 0.0;
      Waveform: rmwSinusoid );

  RINGMODULATOR_PRESET_HIGH_FREQ: TALSRingModulatorProperties=
    ( Frequency: 12.4;
      HighPassCutoff: 5640.0;
      Waveform: rmwSinusoid );

  RINGMODULATOR_PRESET_SSSS: TALSRingModulatorProperties=
    ( Frequency: 6.8;
      HighPassCutoff: 3192.0;
      Waveform: rmwSinusoid );

// Preset Vocal Morpher
  VOCALMORPHER_PRESET_DEFAULT: TALSVocalMorpherProperties=
    ( PhonemeA: phoA;
      PhonemeB: phoER;
      PhonemeACoarseTuning: 0;
      PhonemeBCoarseTuning: 0;
      Waveform: vmwSinusoid;
      Rate: 1.41 );

  VOCALMORPHER_PRESET_NIGHTCLUB_TOILET: TALSVocalMorpherProperties=
    ( PhonemeA: phoE;
      PhonemeB: phoU;
      PhonemeACoarseTuning: -19;
      PhonemeBCoarseTuning: -6;
      Waveform: vmwTriangle;
      Rate: 0.000 );

// Preset Reverb
  REVERB_PRESET_DEFAULT: TALSReverbProperties=
    ( Density: 1.0;
      Diffusion: 1.0;
      Gain: 0.32;
      GainHF: 0.89;
      DecayTime: 1.49;
      DecayHFRatio: 0.83;
      ReflectionsGain: 0.05;
      ReflectionsDelay: 0.007;
      LateReverbGain: 1.26;
      LateReverbDelay: 0.011;
      AirAbsorptionGainHF: 0.994;
      RoomRolloffFactor: 0.0;
      DecayHFLimit: 1 );

{$endif}

{$ifdef ALS_IMPLEMENTATION}


procedure TALSAutoWahProperties.InitDefault;
begin
  InitWithPreset(0);
{  AttackTime := 0.06;
  ReleaseTime := 0.06;
  Resonance := 1000.0;
  PeakGain := 11.22;  }
end;

function TALSAutoWahProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 7);
  Result[0] := 'DEFAULT';
  Result[1] := 'OLD_RADIO';
  Result[2] := 'WIND_IMPACT';
  Result[3] := 'VOCAL_DELIRIUM';
  Result[4] := 'ROCKY1';
  Result[5] := 'ROCKY2';
  Result[6] := 'VOCAL_SUPPORT';
end;

procedure TALSAutoWahProperties.InitWithPreset(aIndex: integer);
var p: ^TALSAutoWahProperties;
begin
  case aIndex of
   1: p := @AUTOWAH_PRESET_OLD_RADIO;
   2: p := @AUTOWAH_PRESET_WIND_IMPACT;
   3: p := @AUTOWAH_PRESET_VOCAL_DELIRIUM;
   4: p := @AUTOWAH_PRESET_ROCKY1;
   5: p := @AUTOWAH_PRESET_ROCKY2;
   6: p := @AUTOWAH_PRESET_VOCAL_SUPPORT;
   else p := @AUTOWAH_PRESET_DEFAULT;
  end;

  AttackTime := p^.AttackTime;
  ReleaseTime := p^.ReleaseTime;
  Resonance := p^.Resonance;
  PeakGain := p^.PeakGain;
end;

procedure TALSChorusProperties.InitDefault;
begin
  InitWithPreset(0);
{Waveform := cwTriangle;
Phase := 90;
Rate := 1.1;
Depth := 0.1;
Feedback := 0.25;
Delay := 0.016;  }
end;

function TALSChorusProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 10);
  Result[0] := 'DEFAULT';
  Result[1] := 'ACOUSTIC_DUET';
  Result[2] := 'AMATEUR';
  Result[3] := 'FLYING_SAUCER';
  Result[4] := 'DUO';
  Result[5] := 'ELECTRO_VOICE';
  Result[6] := '60SPSYCHEDELIC';
  Result[7] := 'TRIPPYVOX';
  Result[8] := 'RICH';
  Result[9] := 'WIDE';
end;

procedure TALSChorusProperties.InitWithPreset(aIndex: integer);
var
  p: ^TALSChorusProperties;
begin
  case aIndex of
    1: p := @CHORUS_PRESET_ACOUSTIC_DUET;
    2: p := @CHORUS_PRESET_AMATEUR;
    3: p := @CHORUS_PRESET_FLYING_SAUCER;
    4: p := @CHORUS_PRESET_DUO;
    5: p := @CHORUS_PRESET_ELECTRO_VOICE;
    6: p := @CHORUS_PRESET_60SPSYCHEDELIC;
    7: p := @CHORUS_PRESET_TRIPPYVOX;
    8: p := @CHORUS_PRESET_RICH;
    9: p := @CHORUS_PRESET_WIDE;
    else p := @CHORUS_PRESET_DEFAULT;
  end;

  Waveform := p^.Waveform;
  Phase := p^.Phase;
  Rate := p^.Rate;
  Depth := p^.Depth;
  Feedback := p^.Feedback;
  Delay := p^.Delay;
end;

procedure TALSFlangerProperties.InitDefault;
begin
  InitWithPreset(0);
{  Waveform := fwTriangle;
  Phase := 0;
  Rate := 0.27;
  Depth := 1.0;
  Feedback := -0.5;
  Delay := 0.002;  }
end;

function TALSFlangerProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 10);
  Result[0] := 'DEFAULT';
  Result[1] := 'AURAL';
  Result[2] := 'RICH_VOICE';
  Result[3] := 'MULTI_VOICE';
  Result[4] := 'METALLIC';
  Result[5] := 'NIGHTMARE';
  Result[6] := 'ENHANCED_STEREO';
  Result[7] := 'SUSTAIN';
  Result[8] := 'MEDIUM_FLANGER';
  Result[9] := 'STRONG_FLANGER';
end;

procedure TALSFlangerProperties.InitWithPreset(aIndex: integer);
var
  p: ^TALSFlangerProperties;
begin
  case aIndex of
    1: p := @FLANGER_PRESET_AURAL;
    2: p := @FLANGER_PRESET_RICH_VOICE;
    3: p := @FLANGER_PRESET_MULTI_VOICE;
    4: p := @FLANGER_PRESET_METALLIC;
    5: p := @FLANGER_PRESET_NIGHTMARE;
    6: p := @FLANGER_PRESET_ENHANCED_STEREO;
    7: p := @FLANGER_PRESET_SUSTAIN;
    8: p := @FLANGER_PRESET_MEDIUM_FLANGER;
    9: p := @FLANGER_PRESET_STRONG_FLANGER;
    else
      p := @FLANGER_PRESET_DEFAULT;
  end;

  Waveform := p^.Waveform;
  Phase := p^.Phase;
  Rate := p^.Rate;
  Depth := p^.Depth;
  Feedback := p^.Feedback;
  Delay := p^.Delay;
end;

procedure TALSCompressorProperties.InitDefault;
begin
  OnOff := AL_TRUE;
end;

function TALSCompressorProperties.PresetList: TStringArray;
begin
  Result := NIL;
end;

procedure TALSCompressorProperties.InitWithPreset(aIndex: integer);
begin
  if aIndex=0 then; // avoid hint
end;

procedure TALSDistortionProperties.InitDefault;
begin
  InitWithPreset(0);
{  Edge := 0.2;
  Gain := 0.05;
  LowpassCutoff := 8000.0;
  EQCenter := 3600.0;
  EQBandwidth := 3600.0;  }
end;

function TALSDistortionProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 7);
  Result[0] := 'DEFAULT';
  Result[1] := '70S_FUZZGUITAR';
  Result[2] := 'HEAVY';
  Result[3] := 'DESTROY_BASS';
  Result[4] := 'SOFT';
  Result[5] := 'MEDIUM';
  Result[6] := 'HARD';
end;

procedure TALSDistortionProperties.InitWithPreset(aIndex: integer);
var
  p: ^TALSDistortionProperties;
begin
  case aIndex of
    1: p := @DISTORTION_PRESET_70S_FUZZGUITAR;
    2: p := @DISTORTION_PRESET_HEAVY;
    3: p := @DISTORTION_PRESET_DESTROY_BASS;
    4: p := @DISTORTION_PRESET_SOFT;
    5: p := @DISTORTION_PRESET_MEDIUM;
    6: p := @DISTORTION_PRESET_HARD;
    else
      p := @DISTORTION_PRESET_DEFAULT;
  end;

  Edge := p^.Edge;
  Gain := p^.Gain;
  LowpassCutoff := p^.LowpassCutoff;
  EQCenter := p^.EQCenter;
  EQBandwidth := p^.EQBandwidth;
end;

procedure TALSEchoProperties.InitDefault;
begin
  InitWithPreset(0);
{  Delay := 0.1;
  LRDelay := 0.1;
  Damping := 0.5;
  Feedback := 0.5;
  Spread := -1.0;   }
end;

function TALSEchoProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 15);
  Result[0] := 'DEFAULT';
  Result[1] := '1950s_STYLE';
  Result[2] := 'OLD_SCHOOL';
  Result[3] := 'AUDITORIUM';
  Result[4] := 'CANYON';
  Result[5] := 'CHEAP_SPRINGS';
  Result[6] := 'DRAIN_PIPE';
  Result[7] := 'LIGHT_ECHO_HALL';
  Result[8] := 'DELAYED';
  Result[9] := 'MONO_VOCAL_1';
  Result[10] := 'MONO_VOCAL_2';
  Result[11] := 'ROBUST_SHORT';
  Result[12] := 'SMALL_ROOM';
  Result[13] := 'TAJ_MAHAL';
  Result[14] := 'DELIRIUM';
end;

procedure TALSEchoProperties.InitWithPreset(aIndex: integer);
var p: ^TALSEchoProperties;
begin
  case aIndex of
    1: p := @ECHO_PRESET_1950s_STYLE;
    2: p := @ECHO_PRESET_OLD_SCHOOL;
    3: p := @ECHO_PRESET_AUDITORIUM;
    4: p := @ECHO_PRESET_CANYON;
    5: p := @ECHO_PRESET_CHEAP_SPRINGS;
    6: p := @ECHO_PRESET_DRAIN_PIPE;
    7: p := @ECHO_PRESET_LIGHT_ECHO_HALL;
    8: p := @ECHO_PRESET_DELAYED;
    9: p := @ECHO_PRESET_MONO_VOCAL_1;
    10: p := @ECHO_PRESET_MONO_VOCAL_2;
    11: p := @ECHO_PRESET_ROBUST_SHORT;
    12: p := @ECHO_PRESET_SMALL_ROOM;
    13: p := @ECHO_PRESET_TAJ_MAHAL;
    14: p := @ECHO_PRESET_DELIRIUM;
    else
      p := @ECHO_PRESET_DEFAULT
  end;

  Delay := p^.Delay;
  LRDelay := p^.LRDelay;
  Damping := p^.Damping;
  Feedback := p^.Feedback;
  Spread := p^.Spread;
end;

procedure TALSEqualizerProperties.InitDefault;
begin
  InitWithPreset(0);
{  LowGain := 1.0;
  LowCutoff := 200.0;
  Mid1Gain := 1.0;
  Mid1Center := 500.0;
  Mid1Width := 1.0;
  Mid2Gain := 1.0;
  Mid2Center := 3000.0;
  Mid2Width := 1.0;
  HighGain := 1.0;
  HighCutoff := 6000.0;  }
end;

function TALSEqualizerProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 4);
  Result[0] := 'DEFAULT';
  Result[1] := 'BASSBOOST';
  Result[2] := 'HIGHBOOST';
  Result[3] := 'LOW_FIDELITY';
end;

procedure TALSEqualizerProperties.InitWithPreset(aIndex: integer);
var
  p: ^TALSEqualizerProperties;
begin
  case aIndex of
    1: p := @EQUALIZER_PRESET_BASSBOOST;
    2: p := @EQUALIZER_PRESET_HIGHBOOST;
    3: p := @EQUALIZER_PRESET_LOW_FIDELITY;
    else
      p := @EQUALIZER_PRESET_DEFAULT;
  end;

  LowCutoff := p^.LowCutoff;
  LowGain := p^.LowGain;
  Mid1Center := p^.Mid1Center;
  Mid1Gain := p^.Mid1Gain;
  Mid1Width := p^.Mid1Width;
  Mid2Center := p^.Mid2Center;
  Mid2Gain := p^.Mid2Gain;
  Mid2Width := p^.Mid2Width;
  HighCutoff := p^.HighCutoff;
  HighGain := p^.HighGain;
end;

procedure TALSFreqShifterProperties.InitDefault;
begin
  InitWithPreset(0);
 { Frequency := 0.0;
  LeftDirection := sdDown;
  RightDirection := sdDown;   }
end;

function TALSFreqShifterProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 5);
  Result[0] := 'DEFAULT';
  Result[1] := 'HOARSE_VOICE';
  Result[2] := 'DUAL_VOICE';
  Result[3] := 'MELODY_DESTRUCTOR';
  Result[4] := 'RAP_3_VOICES';
end;

procedure TALSFreqShifterProperties.InitWithPreset(aIndex: integer);
var p: ^TALSFreqShifterProperties;
begin
  case aIndex of
    1: p := @FREQSHIFTER_PRESET_HOARSE_VOICE;
    2: p := @FREQSHIFTER_PRESET_DUAL_VOICE;
    3: p := @FREQSHIFTER_PRESET_MELODY_DESTRUCTOR;
    4: p := @FREQSHIFTER_PRESET_RAP_3_VOICES;
    else
      p := @FREQSHIFTER_PRESET_DEFAULT;
  end;

  Frequency := p^.Frequency;
  LeftDirection := p^.LeftDirection;
  RightDirection := p^.RightDirection;
end;

procedure TALSRingModulatorProperties.InitDefault;
begin
  InitWithPreset(0);
{  Frequency := 440.0;
  HighPassCutoff := 800.0;
  Waveform := rmwSinusoid;     }
end;

function TALSRingModulatorProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 5);
  Result[0] := 'DEFAULT';
  Result[1] := 'GARGLE';
  Result[2] := 'CAT_IN_THROAT';
  Result[3] := 'HIGH_FREQ';
  Result[4] := 'SSSS';
end;

procedure TALSRingModulatorProperties.InitWithPreset(aIndex: integer);
var p: ^TALSRingModulatorProperties;
begin
  case aIndex of
    1: p := @RINGMODULATOR_PRESET_GARGLE;
    2: p := @RINGMODULATOR_PRESET_CAT_IN_THROAT;
    3: p := @RINGMODULATOR_PRESET_HIGH_FREQ;
    4: p := @RINGMODULATOR_PRESET_SSSS;
    else
      p := @RINGMODULATOR_PRESET_DEFAULT;
  end;

  Frequency := p^.Frequency;
  HighPassCutoff := p^.HighPassCutoff;
  Waveform := p^.Waveform;
end;

procedure TALSPitchShifterProperties.InitDefault;
begin
  InitWithPreset(0);
{  CoarseTune := 12;
  FineTune := 0;     }
end;

function TALSPitchShifterProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 2);
  Result[0] := 'DEFAULT';
  Result[1] := 'VOICEDUO_BASS';
end;

procedure TALSPitchShifterProperties.InitWithPreset(aIndex: integer);
var p: ^TALSPitchShifterProperties;
begin
  case aIndex of
    1: p := @PITCHSHIFTER_PRESET_VOICEDUO_BASS;
    else
      p := @PITCHSHIFTER_PRESET_DEFAULT;
  end;

  CoarseTune := p^.CoarseTune;
  FineTune := p^.FineTune;
end;

procedure TALSVocalMorpherProperties.InitDefault;
begin
  InitWithPreset(0);
{  PhonemeA := phoA;
  PhonemeB := phoER;
  PhonemeACoarseTuning := 0;
  PhonemeBCoarseTuning := 0;
  Waveform := vmwSinusoid;
  Rate := 1.41;    }
end;

function TALSVocalMorpherProperties.PhonemeList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 30);
  Result[0] := 'A';   Result[1] := 'E';   Result[2] := 'I';   Result[3] := 'O';
  Result[4] := 'U';   Result[5] := 'AA';  Result[6] := 'AE';  Result[7] := 'AH';
  Result[8] := 'AO';  Result[9] := 'EH';  Result[10] := 'ER'; Result[11] := 'IH';
  Result[12] := 'IY'; Result[13] := 'UH'; Result[14] := 'UW'; Result[15] := 'B';
  Result[16] := 'D';  Result[17] := 'F';  Result[18] := 'G';  Result[19] := 'J';
  Result[20] := 'K';  Result[21] := 'L';  Result[22] := 'M';  Result[23] := 'N';
  Result[24] := 'P';  Result[25] := 'R';  Result[26] := 'S';  Result[27] := 'T';
  Result[28] := 'V';  Result[29] := 'Z';
end;

function TALSVocalMorpherProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 2);
  Result[0] := 'DEFAULT';
  Result[1] := 'NIGHTCLUB_TOILET';
end;

procedure TALSVocalMorpherProperties.InitWithPreset(aIndex: integer);
var
  p: ^TALSVocalMorpherProperties;
begin
  case aIndex of
    1: p := @VOCALMORPHER_PRESET_NIGHTCLUB_TOILET;
    else
      p := @VOCALMORPHER_PRESET_DEFAULT;
  end;

  PhonemeA := p^.PhonemeA;
  PhonemeB := p^.PhonemeB;
  PhonemeACoarseTuning := p^.PhonemeACoarseTuning;
  PhonemeBCoarseTuning := p^.PhonemeBCoarseTuning;
  Waveform := p^.Waveform;
  Rate := p^.Rate;
end;

procedure TALSReverbProperties.InitDefault;
begin
  InitWithPreset(0);
{  Density := 1.0;
  Diffusion := 1.0;
  Gain := 0.32;
  GainHF := 0.89;
  DecayTime := 1.49;
  DecayHFRatio := 0.83;
  ReflectionsGain := 0.05;
  ReflectionsDelay := 0.007;
  LateReverbGain := 1.26;
  LateReverbDelay := 0.011;
  AirAbsorptionGainHF := 0.994;
  RoomRolloffFactor := 0.0;
  DecayHFLimit := AL_TRUE;  }
end;

function TALSReverbProperties.PresetList: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 1);
  Result[0] := 'DEFAULT';
end;

procedure TALSReverbProperties.InitWithPreset(aIndex: integer);
var
  p: ^TALSReverbProperties;
begin
  case aIndex of
    1: p := @REVERB_PRESET_DEFAULT;
    else
      p := @REVERB_PRESET_DEFAULT;
  end;

  Density := p^.Density;
  Diffusion := p^.Diffusion;
  Gain := p^.Gain;
  GainHF := p^.GainHF;
  DecayTime := p^.DecayTime;
  DecayHFRatio := p^.DecayHFRatio;
  ReflectionsGain := p^.ReflectionsGain;
  ReflectionsDelay := p^.ReflectionsDelay;
  LateReverbGain := p^.LateReverbGain;
  LateReverbDelay := p^.LateReverbDelay;
  AirAbsorptionGainHF := p^.AirAbsorptionGainHF;
  RoomRolloffFactor := p^.RoomRolloffFactor;
  DecayHFLimit := p^.DecayHFLimit;
end;

procedure TALSReverbProperties.InitWithEAXPreset(aIndex: integer);
var
  eaxReverbProp: TEAXReverbProperties;
begin
  eaxReverbProp.InitWithPreset(aIndex);

  Density := eaxReverbProp.flDensity;
  Diffusion := eaxReverbProp.flDiffusion;
  Gain := eaxReverbProp.flGain;
  GainHF := eaxReverbProp.flGainHF;
  DecayTime := eaxReverbProp.flDecayTime;
  DecayHFRatio := eaxReverbProp.flDecayHFRatio;
  ReflectionsGain := eaxReverbProp.flReflectionsGain;
  ReflectionsDelay := eaxReverbProp.flReflectionsDelay;
  LateReverbGain := eaxReverbProp.flLateReverbGain;
  LateReverbDelay := eaxReverbProp.flLateReverbDelay;
  AirAbsorptionGainHF := eaxReverbProp.flAirAbsorptionGainHF;
  RoomRolloffFactor := eaxReverbProp.flRoomRolloffFactor;
  DecayHFLimit := eaxReverbProp.iDecayHFLimit;
end;




{$endif}
