{
  **************************************************************************
 *                                                                          *
 *  This file is part of ALSound.                                           *
 *                                                                          *
 *  See the file LICENSE included in this distribution,                     *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This software is distributed in the hope of being useful                *
 *  for learning purposes about OpenAL-Soft and LibSndFile                  *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
  **************************************************************************

  ALSound offer a simple and easy way to play, capture and mix sounds
  using OpenAL-Soft and LibSndFile libraries under FreePascal/Lazarus.

 written by Lulu - 2022 - https://github.com/Lulu04/ALSound
}

unit ALSound;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$pointerMath on}

interface

uses
  Classes, SysUtils, ctypes, Types,
  {$ifdef LCL}ExtCtrls, Graphics, Forms,{$endif}  // needed to access Application.Location
                                                  // and to render velocity curve on TImage
  openalsoft,
  libsndfile,
  als_dsp_utils;

const
  ALS_VERSION = '3.0.0';


type
  TALSSound = class;

const
  // Volume range. 0.0=silence  1.0=original volume  >1.0=amplification
  ALS_VOLUME_MIN = 0.0;
  ALS_VOLUME_MAX = 1.0;
  ALS_VOLUME_MAXAMP = 8.0; // The maximum amplification value. This is
                           // equivalent to +18dB

  // Pan range
  ALS_PAN_LEFT = -1.0;
  ALS_PAN_CENTER = 0.0;
  ALS_PAN_RIGHT = 1.0;

  // Pitch range
  ALS_PITCH_MIN = 0.1;
  ALS_PITCH_MAX = 4.0;
  ALS_PITCH_NORMAL = 1.0;

  // Tone range
  ALS_TONE_ONLYLOWFREQ = 0.0;
  ALS_TONE_ONLYHIGHFREQ = 1.0;
  ALS_TONE_NORMAL = 0.5;

  // Decibel range is -60dB to 0dB
  ALS_DECIBEL_MIN_VALUE = -60;
  ALS_DECIBEL_MAX_VALUE = 0;

type

  // Possible state for sounds, playlist and context
  TALSState = ( ALS_STOPPED,
                ALS_PLAYING,
                ALS_PAUSED,
                ALS_RECORDING,
                ALS_MIXING );

  // Playback context output mode.
  TALSPlaybackContextOutputMode = (
              ALC_SURROUND_5_1 = openalsoft.ALC_SURROUND_5_1_SOFT,
              ALC_SURROUND_6_1 = openalsoft.ALC_SURROUND_6_1_SOFT,
              ALC_SURROUND_7_1 = openalsoft.ALC_SURROUND_7_1_SOFT,
              ALC_ANY_SOFT     = openalsoft.ALC_ANY_SOFT,
              ALC_STEREO_BASIC = openalsoft.ALC_STEREO_BASIC_SOFT,
              ALC_STEREO_UHJ   = openalsoft.ALC_STEREO_UHJ_SOFT,
              ALC_STEREO_HRTF  = openalsoft.ALC_STEREO_HRTF_SOFT );

  // Capture context sample format
  TALSCaptureFormat = ( ALS_CAPTUREFORMAT_MONO16=$1101,
                        ALS_CAPTUREFORMAT_STEREO16=$1103,
                        ALS_CAPTUREFORMAT_MONO_FLOAT32=$10010,
                        ALS_CAPTUREFORMAT_STEREO_FLOAT32=$10011 );


  TALSLoopbackChannel = ( ALC_MONO_SOFT = $1500,
                          ALC_STEREO_SOFT = $1501,
                          ALC_QUAD_SOFT = $1503,
                          ALC_5POINT1_SOFT = $1504,
                          ALC_6POINT1_SOFT = $1505,
                          ALC_7POINT1_SOFT = $1506 );

  TALSLoopbackSampleType = ( //ALC_BYTE_SOFT = $1400,
                             //ALC_UNSIGNED_BYTE_SOFT = $1401,
                             ALC_SHORT_SOFT = $1402, // 16b signed integer
                             //ALC_UNSIGNED_SHORT_SOFT = $1403,
                             ALC_INT_SOFT = $1404, // 32bit signed integer
                             //ALC_UNSIGNED_INT_SOFT = $1405,
                             ALC_FLOAT_SOFT = $1406); // 32bits float

  TALSFileMajorFormat = (
       SF_FORMAT_WAV = $010000,  // Microsoft WAV format (little endian default)
       SF_FORMAT_AIFF = $020000,  // Apple/SGI AIFF format (big endian)
       SF_FORMAT_AU = $030000,  // Sun/NeXT AU format (big endian)
       SF_FORMAT_RAW = $040000,  // RAW PCM data
       SF_FORMAT_PAF = $050000,  // Ensoniq PARIS file format
       SF_FORMAT_SVX = $060000,  // Amiga IFF / SVX8 / SV16 format
       SF_FORMAT_NIST = $070000,  // Sphere NIST format
       SF_FORMAT_VOC = $080000,  // VOC files
       SF_FORMAT_IRCAM = $0A0000,  // Berkeley/IRCAM/CARL
       SF_FORMAT_W64 = $0B0000,  // Sonic Foundry's 64 bit RIFF/WAV
       SF_FORMAT_MAT4 = $0C0000,  // Matlab (tm) V4.2 / GNU Octave 2.0
       SF_FORMAT_MAT5 = $0D0000,  // Matlab (tm) V5.0 / GNU Octave 2.1
       SF_FORMAT_PVF = $0E0000,  // Portable Voice Format
       SF_FORMAT_XI = $0F0000,  // Fasttracker 2 Extended Instrument
       SF_FORMAT_HTK = $100000,  // HMM Tool Kit format
       SF_FORMAT_SDS = $110000,  // Midi Sample Dump Standard
       SF_FORMAT_AVR = $120000,  // Audio Visual Research
       SF_FORMAT_WAVEX = $130000,  // MS WAVE with WAVEFORMATEX
       SF_FORMAT_SD2 = $160000,  // Sound Designer 2
       SF_FORMAT_FLAC = $170000,  // FLAC lossless file format
       SF_FORMAT_CAF = $180000,  // Core Audio File format
       SF_FORMAT_WVE = $190000,  // Psion WVE format
       SF_FORMAT_OGG = $200000,  // Xiph OGG container
       SF_FORMAT_MPC2K = $210000,  // Akai MPC 2000 sampler
       SF_FORMAT_RF64 = $220000,  // RF64 WAV file
       SF_FORMAT_MPEG = $230000 ); // MPEG-1/2 audio stream

  TALSFileSubFormat = (
       SF_FORMAT_PCM_S8 = $0001,  // Signed 8 bit data */
       SF_FORMAT_PCM_16 = $0002,  // Signed 16 bit data */
       SF_FORMAT_PCM_24 = $0003,  // Signed 24 bit data */
       SF_FORMAT_PCM_32 = $0004,  // Signed 32 bit data */

       SF_FORMAT_PCM_U8 = $0005,  // Unsigned 8 bit data (WAV and RAW only) */

       SF_FORMAT_FLOAT = $0006,  // 32 bit float data */
       SF_FORMAT_DOUBLE = $0007, // 64 bit float data */

       SF_FORMAT_ULAW = $0010,  // U-Law encoded. */
       SF_FORMAT_ALAW = $0011,  // A-Law encoded. */
       SF_FORMAT_IMA_ADPCM = $0012,  // IMA ADPCM. */
       SF_FORMAT_MS_ADPCM = $0013,   // Microsoft ADPCM. */

       SF_FORMAT_GSM610 = $0020,   // GSM 6.10 encoding. */
       SF_FORMAT_VOX_ADPCM = $0021,// OKI / Dialogix ADPCM */

       SF_FORMAT_NMS_ADPCM_16 = $0022,  // 16kbs NMS G721-variant encoding. */
       SF_FORMAT_NMS_ADPCM_24 = $0023,  // 24kbs NMS G721-variant encoding. */
       SF_FORMAT_NMS_ADPCM_32 = $0024,  // 32kbs NMS G721-variant encoding. */

       SF_FORMAT_G721_32 = $0030,  // 32kbs G721 ADPCM encoding. */
       SF_FORMAT_G723_24 = $0031,  // 24kbs G723 ADPCM encoding. */
       SF_FORMAT_G723_40 = $0032,  // 40kbs G723 ADPCM encoding. */

       SF_FORMAT_DWVW_12 = $0040, // 12 bit Delta Width Variable Word encoding. */
       SF_FORMAT_DWVW_16 = $0041, // 16 bit Delta Width Variable Word encoding. */
       SF_FORMAT_DWVW_24 = $0042, // 24 bit Delta Width Variable Word encoding. */
       SF_FORMAT_DWVW_N = $0043,  // N bit Delta Width Variable Word encoding. */

       SF_FORMAT_DPCM_8 = $0050,  // 8 bit differential PCM (XI only) */
       SF_FORMAT_DPCM_16 = $0051, // 16 bit differential PCM (XI only) */

       SF_FORMAT_VORBIS = $0060,// Xiph Vorbis encoding. */
       SF_FORMAT_OPUS = $0064,  // Xiph/Skype Opus encoding. */

       SF_FORMAT_ALAC_16 = $0070,  // Apple Lossless Audio Codec (16 bit). */
       SF_FORMAT_ALAC_20 = $0071,  // Apple Lossless Audio Codec (20 bit). */
       SF_FORMAT_ALAC_24 = $0072,  // Apple Lossless Audio Codec (24 bit). */
       SF_FORMAT_ALAC_32 = $0073,  // Apple Lossless Audio Codec (32 bit). */

       SF_FORMAT_MPEG_LAYER_I = $0080,    // MPEG-1 Audio Layer I. */
       SF_FORMAT_MPEG_LAYER_II = $0081,   //MPEG-1 Audio Layer II. */
       SF_FORMAT_MPEG_LAYER_III = $0082 );//MPEG-2 Audio Layer III. */

  TALSFileEndian = (
       SF_ENDIAN_FILE = $00000000,  // Default file endian-ness. */
       SF_ENDIAN_LITTLE = $10000000,// Force little endian-ness. */
       SF_ENDIAN_BIG = $20000000,   // Force big endian-ness. */
       SF_ENDIAN_CPU = $30000000 ); // Force CPU endian-ness. */

  TALSFileFormat = $00000000..$FFFFFFFF;

  function ALSMakeFileFormat(aFileMajorFormat: TALSFileMajorFormat;
                             aFileSubformat: TALSFileSubFormat;
                             aFileEndian: TALSFileEndian = SF_ENDIAN_FILE): TALSFileFormat;

type
  // Distance model
  // For more explanation see https://indiegamedev.net/2020/04/12/the-complete-guide-to-openal-with-c-part-3-positioning-sounds/
  TALSDistanceModel = (
              AL_NONE = openalsoft.AL_NONE, // no distance attenuation
              AL_INVERSE_DISTANCE = openalsoft.AL_INVERSE_DISTANCE,
              AL_INVERSE_DISTANCE_CLAMPED = openalsoft.AL_INVERSE_DISTANCE_CLAMPED,
              AL_LINEAR_DISTANCE = openalsoft.AL_LINEAR_DISTANCE,
              AL_LINEAR_DISTANCE_CLAMPED = openalsoft.AL_LINEAR_DISTANCE_CLAMPED,
              AL_EXPONENT_DISTANCE = openalsoft.AL_EXPONENT_DISTANCE,
              AL_EXPONENT_DISTANCE_CLAMPED = openalsoft.AL_EXPONENT_DISTANCE_CLAMPED );

  // Sound effects
  TALSEffectType = ( AL_EFFECT_NONE = $0000,
                     AL_EFFECT_REVERB = $0001,
                     AL_EFFECT_CHORUS = $0002,
                     AL_EFFECT_DISTORTION = $0003,
                     AL_EFFECT_ECHO = $0004,
                     AL_EFFECT_FLANGER = $0005,
                     AL_EFFECT_FREQUENCYSHIFTER = $0006,
                     AL_EFFECT_VOCALMORPHER = $0007,
                     AL_EFFECT_PITCHSHIFTER = $0008,
                     AL_EFFECT_RINGMODULATOR = $0009,
                     AL_EFFECT_AUTOWAH = $000A,
                     AL_EFFECT_COMPRESSOR = $000B,
                     AL_EFFECT_EQUALIZER = $000C,
                     AL_EFFECT_EAXREVERB = $8000 );

  ArrayOfByte = array of byte;
  ArrayOfALint = array of ALint;



  TALSPlaybackContext = class;

  PALSEffect = ^TALSEffect;
  { TALSEffect }

  TALSEffect = record
  private
    FParentContext: TALSPlaybackContext;
    FReady,
    FSlotAssigned,
    FEffectAssigned,
    FMute: boolean;
    procedure DealocateALObjects;
    procedure InitDefault(aParentContext: TALSPlaybackContext);
    procedure InternalSetOutputGain;
  private
    FSlotID, FEffectID: ALuint;
    FApplyDistanceAttenuation: boolean;
    FOutputGain,
    FMuteCoef: single;
    FEffectType: TALSEffectType;
    procedure SetApplyDistanceAttenuation(AValue: boolean);
    procedure SetOutputGain(AValue: single);
    procedure InitEffect(aEffectType: TALSEffectType; const aParameters);
    procedure DoUpdateParam(const P);
  private
    FPrevious,
    FNext: PALSEffect;
    function IsInChain: boolean;
    function AllOtherChainedEffectAreMuted: boolean;
    procedure ForceAllMuteCoeffTo1;
    function FirstEffectInChain: PALSEffect;
    function GetPreviousActiveEffectSlotID(aEffect: PALSEffect; out aSlotID: ALuint): boolean;
    function GetNextActiveEffectSlotID(aEffect: PALSEffect): ALuint;
    // backward scan the effect's chain to retrieve an active effect
    function GetPreviousActiveEffect(aEffect: PALSEffect): PALSEffect;
    // forward scan the effect's chain to retrieve an active effect
    function GetNextActiveEffect(aEffect: PALSEffect): PALSEffect;

    procedure SetMute(AValue: boolean);
  public
    procedure UpdateParameters(const P);
    // Redirects the effect output to the input of another.
    // This allow you to chains multiple effects on a single auxiliary send.
    // Returns True if success.
    function ChainWith(var aTargetEffect: TALSEffect): boolean;

    // True if the effect is ready to use.
    // False if there is an error while initializing it.
    property Ready: boolean read FReady;

    // You can mute an effect setting this property to True. Also work on effect
    // that are a part of effect's chain.
    property Mute: boolean read FMute write SetMute;

    // the output gain of the effect. Range is [0..1]. Default value is 1.0.
    property OutputGain: single read FOutputGain write SetOutputGain;
    // Enable or disable automatic send adjustments based on the physical
    // positions of the sound and the listener
    property ApplyDistanceAttenuation: boolean read FApplyDistanceAttenuation write SetApplyDistanceAttenuation;
    property EffectType: TALSEffectType read FEffectType;
  end;

  // The available curve you can use to change volume, pan, pitch,... not linearly
  TALSCurveID = ( ALS_Linear = 0,
                  ALS_StartFastEndSlow,
                  ALS_StartSlowEndFast,
                  ALS_Sinusoid,
                  ALS_Sinusoid2 );

  {$define ALS_INTERFACE}
  {$include als_error.inc}
  {$include als_effectpresets.inc}
  {$include eaxreverbpresets.inc}
  {$include als_directfilter.inc}
  {$include als_frame_buffers.inc}
  {$include als_velocity_curve.inc}
  {$include als_deviceitem.inc}

type
  { TALSErrorHandling }
  // In ALSound, everything that handle errors inherits from TALSErrorHandling class
  TALSErrorHandling = class
  private
    function GetErrorState: boolean;
  protected
    FErrorCode: TALSError;
    FALErrorCode,
    FALCErrorCode: ALenum;
    function GetStrError: string;
    procedure InitializeErrorStatus; virtual;
    // return True if OpenAL returned an error
    function CheckALError(aErrorCode: TALSError): boolean;
    function CheckALCError(aDevice: PALCDevice; aErrorCode: TALSError): boolean;
    procedure SetError(aErrorCode: TALSError);
    procedure SetALError(aErrorCode: TALSError; aALError: ALenum);
    procedure SetALCError(aErrorCode: TALSError; aALCError: ALenum);
  public
    property Error: boolean read GetErrorState;
    property StrError: string read GetStrError;
  end;


  TALSNotifyEvent = procedure(Sender: TALSSound) of object;
  TALSOnCustomDSP = procedure(Sender: TALSSound;
                              const aBuffer: TALSPlaybackBuffer;
                              aUserData: Pointer) of object;

  { TALSSound }

  TALSSound = class( TALSErrorHandling )
  private
    FCriticalSection: TRTLCriticalSection;
    procedure EnterCS;
    procedure LeaveCS;
    procedure Update(const aElapsedTime: single); virtual;
  protected
    procedure InitializeErrorStatus; override;
  private
    FParentContext: TALSPlaybackContext;
    FSampleRate,
    FChannelCount: integer;
    FFormatForAL: DWord;
    FFrameSize: integer;
    FFrameCount,
    FByteCount: QWord;
    FStrFormat,
    FStrSubFormat,
    FFilename: string;
    FTag: integer;
    function GetFormatForAL( aChannelCount: integer; aContextUseFloat, aWantBFormatAmbisonic: boolean ): DWord;
    procedure DecodeFileInfo( aFileHandle: PSNDFILE; aSFInfo: TSF_INFO );
  protected
    FSource: longword;
    FLoop, FPaused: boolean;
    FBuffers: array of TALSPlaybackBuffer;
    FMonitoringEnabled: boolean;
    procedure GenerateALSource;
    procedure GenerateALBuffers(aCount: integer);
    procedure SetBuffersFrameCapacity(aFrameCapacity: longword);
    procedure FreeBuffers;
  protected
    FFadeOutEnabled, FKillAfterFadeOut, FKillAfterPlay, FKill: boolean;
  private
    procedure SetLoop(AValue: boolean);
    procedure SetALVolume;
    procedure SetALPan;
    procedure SetALPitch;
  private
    FAuxiliarySend: array of TALSAuxiliarySend;
    FDirectFilter: TALSDirectFilter;
    function RetrieveAuxSend(const aEffect: TALSEffect; out AuxSendIndex: ALint): boolean;
    procedure RemoveAllALEffects;
   // procedure RemoveALEffects
    function AllAuxiliarySendAreEmpty: boolean;
  private
    FApplyToneOnAuxSend: boolean;
    procedure SetALTone;
    procedure SetApplyToneOnAuxSend(AValue: boolean);
  private
    FGlobalVolume,
    FMuteMultiplicator: single;
    FPositionRelativeToListener: boolean;
    procedure SetGlobalVolume(AValue: single);
    function GetChannelLevel(index: integer): single; virtual;
    function GetChannelLeveldB(index: integer): single;
    function GetDistanceModel: TALSDistanceModel;
    function GetMute: boolean;
    function GetState: TALSState;
    procedure SetDistanceModel(AValue: TALSDistanceModel);
    procedure SetMute(AValue: boolean);
    procedure SetPositionRelativeToListener(AValue: boolean);
    function GetResamplerIndex: integer;
    procedure SetResamplerIndex(AValue: integer);
    procedure InternalRewind; virtual;
    procedure CreateParameters;
    procedure FreeParameters;
  private
    FOnCustomDSP: TALSOnCustomDSP;
    FOnCustomDSPUserData: pointer;
  private
    FPreviousState: TALSState;
    FOnStopped: TALSNotifyEvent;
    procedure SetOnStopped(AValue: TALSNotifyEvent);
  private
    function GetTimePosition: single; virtual;
    procedure SetTimePosition(AValue: single); virtual;
  public
    // Plays the sound or resume it if it is paused.
     procedure Play(aFromBegin: boolean = True);
    // Stops the sound.
    procedure Stop;
    // Pause/Resume the sound.
    procedure Pause;

    // Play a sound with its volume initialy sets to 0, then gradually increase it to ALS_VOLUME_MAX.
    procedure FadeIn(aDuration: single; ACurveID: TALSCurveID = ALS_Linear);

    // Play a sound with its volume initially sets to 0, then gradually increase it to the specified value.
    procedure FadeIn(AVolume: single; aDuration: single; ACurveID: TALSCurveID = ALS_Linear);

    // Decrease the volume of the sound and when it reach 0.0 stops the sound.
    procedure FadeOut(aDuration: single; ACurveID: TALSCurveID = ALS_Linear);

    // Play the sound and kill it when it reach the end.
    // If Loop property is sets to TRUE, the sound will never be killed.
    procedure PlayThenKill(FromBeginning: boolean = True);

    // Decrease the volume of the sound and when its volume reach 0 kill it.
    procedure FadeOutThenKill(aDuration: single; aCurveID: TALSCurveID = ALS_Linear);

    // kill the sound (stop it and free all ressources for this sound).
    procedure Kill;

    // The time length in seconds of the sound.
    function TotalDuration: single;

    // The playback position expressed in seconds.
    property TimePosition: single read GetTimePosition write SetTimePosition;
  public
    function Byte2Seconds(aPosition: QWord): single;
    function Seconds2Byte(aTimePosition: single): QWord;

    // Apply an effect (or a chain of effects) to the sound and return the index
    // of the connected auxiliary send (0 based). An effect occupies one
    // auxiliary send. Do nothing if all auxiliary send are already occupied or
    // if the effect is already connected.
    // The available number of auxiliary send is playback/loopback context
    // dependant and can be retrieved with their property AuxiliarySendCount.
    function ApplyEffect(const aEffect: TALSEffect): integer;

    // After this call, the specified effect will no longer affects the sound
    // The effect still remains available
    procedure RemoveEffect(const aEffect: TALSEffect);

    // Remove all effects previously applied
    procedure RemoveAllEffects;

    // set the ratio Dry - Wet volume   range is  [0..1]
    // AValue      0.0     0.5     1.0
    // Dry         100%----100%     0%        dry is the original sound
    // Wet          0%     100%----100%       wet is the output of the effect
    procedure SetEffectDryWetVolume(const aEffect: TALSEffect; AValue: single);

    procedure SetAuxSendGain(const aEffect: TALSEffect; AValue: single);
    procedure SetDryGain(AValue: single);

    // aReferenceDistance determines when the attenuation actually starts.
    // aMaxDistance is the distance from where the gain can get no lower
    // aRollOffFactor reflects the attenuation curve between the two previous distance.
    // aConeOuterGainHF: high frequency attenuation in the rear position.
    // More explanation on https://indiegamedev.net/2020/04/12/the-complete-guide-to-openal-with-c-part-3-positioning-sounds/
    // and on EffectsExtensionGuide.pdf
    procedure Attenuation3D( aReferenceDistance, aMaxDistance, aRollOffFactor:
        single; aConeOuterGainHF: single=1.0 );
    // Sets the position of the sound in the 3D world
    procedure Position3D( aX, aY, aZ: single );

  public
    // Use this method to define a callback to apply your custom DSP effects.
    // This callback is called when a buffer is filled with new raw audio data
    // and before send it to OpenAL-Soft pipeline.
    // Only once for a static sound, when data are loaded or generated in memory.
    // Each time a new buffer is read from a streamed sound.
    // Your callback must be fast and mustn't update any GUI items.
    procedure SetOnCustomDSP(aProc: TALSOnCustomDSP; aUserData: Pointer);

  public
    // The volume of the sound. Range is [0.0 to 8.0]
    // 0.0=silence  1.0=original volume  >1.0=amplification
    Volume: TALSBoundedFParam;
    // Panning control. Range is -1.0 to 1.0. Works only on MONO and STEREO sounds.
    // value           -1.0<------->0<---------->1.0
    //                 LEFT       CENTER        RIGHT
    Pan: TALSBoundedFParam;
    // Pitch control. Range is [0.1 to 4.0]
    Pitch: TALSBoundedFParam; // pitch range: [0.1 to 4.0]   1.0 = no pitch

    // Control the tone of the sound. Default value is 0.5.
    // value           0.0<-------->0.5<-------->1.0
    //       only low frequency    normal      only high frequency
    Tone: TALSBoundedFParam;
  //  property Tone: single read FTone write SetTone;
    // Sets to True (default) apply tone on auxiliary send.
    property ApplyToneOnAuxSend: boolean read FApplyToneOnAuxSend write SetApplyToneOnAuxSend;

    // Mute the sound.
    property Mute: boolean read GetMute write SetMute;
    // The original sample rate.
    property SampleRate: integer read FSampleRate;
    // Sets to true to loop the sound.
    property Loop: boolean read FLoop write SetLoop;
    // The channel count of the sound.
    property ChannelCount: integer read FChannelCount;
    // The level for each channel expressed in percent: 0=silence 1=full
    property ChannelsLevel[index:integer]: single read GetChannelLevel;
    // The level for each channel expressed in decibel. Range is -60dB to 0dB.
    property ChannelsLeveldB[index:integer]: single read GetChannelLeveldB;

    // The sound's filename. Empty string if the sound is not loaded from a file.
    property Filename: string read FFilename;
    // The format of the sound file e.g: WAV (Microsoft), AIFF(Apple/SGI), ...
    // Empty string if the sound is not loaded from a file.
    property Format: string read FStrFormat;
    // The sub-format of the sound file e.g: Signed 16 bit PCM, IMA ADPCM, ...
    // Empty string if the sound is not loaded from a file.
    property SubFormat: string read FStrSubFormat;
    // The total number of frames.
    property FrameCount: QWord read FFrameCount;
    // The sound length expressed in byte.
    property ByteCount: QWord read FByteCount;

    // The distance model OpenAL must use to compute the sound attenuation in 3D
    // word. Default value AL_NONE
    property DistanceModel: TALSDistanceModel read GetDistanceModel write SetDistanceModel;
    property PositionRelativeToListener: boolean read FPositionRelativeToListener write SetPositionRelativeToListener;

    // When the sound and device sample rate don't match, OpenAL performs
    // resampling. You can choose the used resampler with this property.
    // ResamplerIndex is an index in the list of resampler provided by
    // TALSPlaybackContext.ResamplerList property
    // For more info see  https://openal-soft.org/openal-extensions/SOFT_source_resampler.txt
    property ResamplerIndex: integer read GetResamplerIndex write SetResamplerIndex;

    property OnStopped: TALSNotifyEvent read FOnStopped write SetOnStopped;

    // State of the sound. Possible value are ALS_STOPPED, ALS_PLAYING, ALS_PAUSED
    property State: TALSState read GetState;
    // general purpose
    property Tag: integer read FTag write FTag;

    // This is an additional volume control. To explain this, let's take an example:
    // in the preferences of a game, the user adjusts the volume independently
    // for musics to 50%, sounds effects to 80% and voices to 100%.
    // This is the purpose of the GlobalVolume property, to have a volume control
    // for the same type of sounds.
    // (internaly, the final volume is equal to Volume.Value*GlobalVolume)
    // Range is [0.0 to 1.0], default value is 1.0
    property GlobalVolume: single read FGlobalVolume write SetGlobalVolume;
  end;



  { TALSSingleStaticBufferSound }

  TALSSingleStaticBufferSound = class(TALSSound)
  private
  const
    LEVEL_TIME_SLICE = 0.050;
  var
    // stuff for channels level
    FLevels: array of ArrayOfSingle;
    procedure InitLevelsFromBuffer(const aBuf: TALSPlaybackBuffer);
    function GetChannelLevel(index: integer): single; override;
  public
    constructor CreateFromFile(aParent: TALSPlaybackContext;
                               const aFilename: string;
                               aEnableMonitor: boolean;
                               aOnCustomDSP: TALSOnCustomDSP;
                               aCustomDSPUserData: Pointer);
    constructor CreateWhiteNoise(aParent: TALSPlaybackContext;
                                 aDuration: single;
                                 aChannelCount: integer;
                                 aEnableMonitor: boolean;
                                 aOnCustomDSP: TALSOnCustomDSP;
                                 aCustomDSPUserData: Pointer);
    destructor Destroy; override;
  end;


  { TALSStreamBufferSound }

  TALSStreamBufferSound = class(TALSSound)
  private
  const
    NUM_BUFFERS = 8;
    BUFFER_TIME_LENGTH = 0.050;
  type
    TALSFunctionReadFromStream = function(aDest: Pointer; aFrameCount: longword): int64 of object;
  private
    FPlayedBufferIndex: integer;
    function GetChannelLevel(index: integer): single; override;
  private
    FDoReadFromStream: TALSFunctionReadFromStream;
    function DoReadStreamFromFile(aDest: Pointer; aFrameCount: longword): int64;
    //function DoReadStreamFromUrl(aDest: Pointer; aFrameCount: longword): int64;
  private
    Fsndfile: PSNDFILE;
    Fsfinfo: TSF_INFO;
    FBufferFrameCount: integer;
    procedure PreBuffAudio;
  private
    FFrameReadAccu: sf_count_t;
    FUsedBuffer: ALsizei;
    procedure Update(const aElapsedTime: single); override;
    procedure InternalRewind; override;
    function GetTimePosition: single; override;
    procedure SetTimePosition(AValue: single); override;
  public
    constructor CreateFromFile(aParent: TALSPlaybackContext;
                               const aFilename: string;
                               aEnableMonitor: boolean;
                               aOnCustomDSP: TALSOnCustomDSP;
                               aCustomDSPUserData: Pointer);
    {constructor CreateFromUrl(aParent: TALSPlaybackContext;
                              const aUrl: string;
                               aEnableMonitor: boolean;
                               aOnCustomDSP: TALSOnCustomDSP;
                               aCustomDSPUserData: Pointer); }
    destructor Destroy; override;
  end;


  { TALSPlaybackCapturedSound }

  TALSPlaybackCapturedSound = class(TALSSound)
  private
  const
    NUM_BUFFERS = 32;
  private
    FTempBufID: array[0..NUM_BUFFERS-1] of ALuint;
    procedure SetTimePosition(AValue: single); override;
  public
    constructor CreateFromCapture(aParent: TALSPlaybackContext;
                                  aSampleRate: integer;
                                  aBuffer: PALSCaptureFrameBuffer);
    destructor Destroy; override;
    procedure QueueBuffer(aBuffer: PALSCaptureFrameBuffer);

    // Because this sound's instance is not associated with a file, use this
    // method to give it a name.
    // Usefull when this sound's instance is shown in a list
    procedure SetFileName(const aName: string);
  end;

  { TALSPlaylist }

  TALSPlaylist = class
  private
    FParentContext: TALSPlaybackContext;
    FList: TStringList;
    FKillList: TFPList;
    FMusic: TALSSound;
    FMusicIndex: integer;
    FState: TALSState;
    FFadeTime, FVolume, FTimeAccu: single;
    function GetCount: integer;
    function GetCurrentFile: string;
    function GetIsPlaying: boolean;
    procedure SetVolume(AValue: single);
    procedure Update(const aElapsedTime: single);
    procedure IncrementMusicIndex;
    procedure FreeCurrentMusic;
    procedure LoadCurrentMusic;
    procedure MoveCurrentMusicToKillList;
  public
    constructor Create(aParent: TALSPlaybackContext);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const aFilename: string);
    procedure Delete(Index: integer);

    procedure Play(aFadeInTime: single = 0);
    procedure Pause(aFadeoutTime: single = 0);
    procedure Stop(aFadeoutTime: single = 0);

    procedure Rewind(aFadeTime: single = 0);
    procedure Next(aFadeTime: single = 0);
    procedure Previous(aFadeTime: single = 0);

    property Volume: single read FVolume write SetVolume;
    property Count: integer read GetCount;
    property State: TALSState read FState;
    property IsPlaying: boolean read GetIsPlaying;
    property CurrentFile: string read GetCurrentFile;
    property CurrentIndex: integer read FMusicIndex;
  end;


  { TALSContextAttributes }
  // Allow to customize a playback or loopback context. First initialize all
  // fields with InitDefault, then apply your settings.
  TALSContextAttributes = record
  public
    // Context output sample rate.
    // Range is 8000hz to 192000hz - default 44100 Hz.
    SampleRate,

    // Number of mono sound the context can play - default 128.
    MonoCount,

    // Number of stereo sound the context can play - default 128.
    StereoCount: integer;

    // TRUE asks the context to use buffers with float samples.
    // FALSE asks the context to use buffers with 16bits signed int samples.
    ContextUseFloat: boolean;

    // Number of auxiliary SEND per sound - default 2 (max 6).
    MaxAuxSend: integer;

    // This is the output mode for a playback context.
    // Default value is ALC_STEREO_BASIC
    OutputMode: TALSPlaybackContextOutputMode;

    // The index of the HRTF to use for the playback context.
    // This index points to an HRTF name in the list provided by TALSPlaybackContext.HRTFList.
    // Default value is -1, that means default HRTF will be used.
    // Please, see HRTFDemo for an typical usage.
    HRTFIndex: integer;

    // Toogle ON/OFF the output limiter on the context. Default value is TRUE.
    // For now, OpenAL-Soft don't offer a control on the limiter's parameters.
    // See https://openal-soft.org/openal-extensions/SOFT_output_limiter.txt
    EnableOutputLimiter: boolean;

    // Initialize all fields to their default value. Don't forgot to call first !
    procedure InitDefault;
    // Initialize parameters for a loopback context.
    procedure SetLoopbackMode(aSampleRate: integer; aChannels: TALSLoopbackChannel; aSampleType: TALSLoopbackSampleType);
  private
    FLoopbackModeEnabled: boolean;
    FLoopbackChannelType: TALSLoopbackChannel;
    FLoopbackSampleType: TALSLoopbackSampleType;
    procedure InitFrom(aAttribs: TALSContextAttributes);
    function ToArray(aEFXPresent, aOutputModePresent,
                     aHRTFPresent, aLoopbackPresent,
                     aOutputLimiterPresent: boolean): ArrayOfALint;
  end;


type
  TALSDoUpdate = procedure(const aElapsedTime: single) of object;

  { TALSThread }

  TALSThread = class(TThread)
  private
    FPeriodUpdate: cardinal;
    FDoUpdate: TALSDoUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(aCallBackDoUpdate: TALSDoUpdate; aUpdatePeriod: cardinal;
      aStart: boolean);
    property DoUpdate: TALSDoUpdate read FDoUpdate write FDoUpdate;
  end;


  { TALSPlaybackContext }

  TALSPlaybackContext = class(TALSErrorHandling)
  protected
    FCriticalSection: TRTLCriticalSection;
    FThread: TALSThread;
    FThreadIsStarted: boolean;
    FSoundToProcess: TALSSound;
    procedure DoUpdate(const aElapsedTime: single);
    procedure DoSoundOnStopped;
    procedure EnterCS;
    procedure LeaveCS;
  protected
    FExecutingConstructor: boolean;
  private
    FHaveExt_ALC_SOFT_HRTF: boolean;
    FList: TFPList;
    FPlaylist: TALSPlaylist;
    FParentDeviceItem: PALSDeviceItem;
    FParentDevice: PALCdevice;
    FContext: PALCcontext;
    FDistanceModel: TALSDistanceModel;
    FUseBufferOfFloat: boolean;
    FSampleRate,
    FObtainedSampleRate: integer;
    FAuxiliarySendAvailable: ALCInt;
    FDefaultResamplerIndex: integer;

    FHaveLowPassFilter,
    FHaveBandPassFilter,
    FHaveHighPassFilter,
    FHaveExt_AL_SOFT_effect_target,
    FHaveExt_AL_EXT_STEREO_ANGLES,
    FHaveExt_AL_EXT_BFORMAT,
    FHaveExt_AL_SOFT_deferred_updates,
    FHaveExt_AL_SOFT_source_resampler,
    FHaveExt_AL_SOFT_source_spatialize,
    FHaveExt_AL_SOFT_gain_clamp_ex,
    FHaveExt_AL_EXT_source_distance_model,
    FHaveExt_AL_SOFT_buffer_samples,
    FHaveExt_AL_SOFT_buffer_sub_data: boolean;

    FMasterGain: TALSBoundedFParam;

    FInternalSampleType: TALSPlaybackSampleType;

    function GetHaveEXT_ALC_EXT_EFX: boolean;
    function GetHaveExt_ALC_SOFT_HRTF: boolean;
    function GetHaveFilter: boolean;
    function GetHRTFEnabled: boolean;
    function GetHRTFList: TStringArray;
    function GetResamplerList: TStringArray;
    function GetSoundCount: integer;
    function GetObtainedMonoCount: integer;
    function GetObtainedStereoCount: integer;
    function GetSoundByIndex(index: integer): TALSSound;
    function GetObtainedAuxiliarySendCount: integer;
    procedure InternalDeleteSound(AIndex: integer);
    procedure SetDistanceModel(AValue: TALSDistanceModel);
    procedure SetListenerGain;
    procedure InitializeALContext(const aAttribs: TALSContextAttributes);
    procedure CreateParameters;
    procedure FreeParameters;
    procedure InternalCloseDevice; virtual;
    function AddCapturePlayback(aSampleRate: integer; aCaptureBuffer: PALSCaptureFrameBuffer): TALSPlaybackCapturedSound;
  public
    // Don't create playback context directly.
    // Use ALSManager.CreateDefaultPlaybackContext
    // or  ALSManager.CreatePlaybackContext(...) method for this
    constructor Create(aDevice: PALSPlaybackDeviceItem; const aAttribs: TALSContextAttributes);
    destructor Destroy; override;

    // Loads a sound file into memory and return its instance.
    // Set aEnableMonitoring to True if you need the channel's level of the
    // sound (it take some ram and cpu resources).
    function AddSound(const aFilename: string;
                      aEnableMonitoring: boolean=False;
                      aOnCustomDSP: TALSOnCustomDSP=NIL;
                      aCustomDSPUserData: Pointer=NIL): TALSSound;
    // Opens the sound file as stream and return its instance.
    // Set aEnableMonitoring to True if you need the channel's level of the
    // sound (it take some ram and cpu resources).
    function AddStream(const aFilename: string;
                       aEnableMonitoring: boolean=False;
                       aOnCustomDSP: TALSOnCustomDSP=NIL;
                       aCustomDSPUserData: Pointer=NIL): TALSSound;

  { TODO : AddWebStream( const aUrl: string ): TOALSound; to play audio from url }

    // Creates a memory sound filled with white noise.
    // Set aEnableMonitoring to True if you need the channel's level of the
    // sound (it take some ram and cpu resources).
    function CreateWhiteNoise(aDuration: single;
                              aChannelCount: integer;
                              aEnableMonitoring: boolean=False;
                              aOnCustomDSP: TALSOnCustomDSP=NIL;
                              aCustomDSPUserData: Pointer=NIL): TALSSound;

    // stops the sound and free it
    procedure Delete(ASound: TALSSound);
    // Deletes all sounds currently in the context
    procedure DeleteAll;
    // Stops all sounds
    procedure StopAllSound;

    // Loads the sound file into memory and play it with the specified volume.
    // Sound is freed when the end is reached, or the sound is stopped.
    procedure PlaySoundThenKill(const aFilename: string; aVolume: single);
    // Opens the sound file as stream and play it with the specified volume.
    // Sound is freed when the end is reached, or the sound is stopped.
    procedure PlayStreamThenKill(const aFilename: string; aVolume: single);

    // Creates an effect that can be applyed on sound instance (TALSSound)
    // All effects created must be deleted before destroying the context.
    // The pair aEffectType, aParameters describes the type of effect you want
    // and its parameters. Can be :
    //   AL_EFFECT_AUTOWAH, TALSAutoWahProperties for AutoWah
    //   AL_EFFECT_CHORUS, TALSChorusProperties for Chorus
    //   AL_EFFECT_FLANGER, TALSFlangerProperties for Flanger
    //   AL_EFFECT_COMPRESSOR, TALSCompressorProperties for Compressor
    //   AL_EFFECT_DISTORTION, TALSDistortionProperties for Distortion
    //   AL_EFFECT_ECHO, TALSEchoProperties for Echo
    //   AL_EFFECT_EQUALIZER, TALSEqualizerProperties for Equalizer
    //   AL_EFFECT_FREQUENCYSHIFTER, TALSFreqShifterProperties for Frequency Shifter
    //   AL_EFFECT_PITCHSHIFTER, TALSPitchShifterProperties for Pitch Shifter
    //   AL_EFFECT_RINGMODULATOR, TALSRingModulatorProperties for Ring Modulator
    //   AL_EFFECT_VOCALMORPHER, TALSVocalMorpherProperties for Vocal Morpher
    //   AL_EFFECT_REVERB, TALSReverbProperties for Reverb
    //   AL_EFFECT_EAXREVERB, TEAXReverbProperties for EAXReverb
    function CreateEffect(aEffectType: TALSEffectType; const aParameters): TALSEffect;
    // Disconnects the effect from all sounds auxiliary send and free it.
    procedure DeleteEffect(var aEffect: TALSEffect);

    // You can try to change the attributes of the playback context on the fly,
    // e.g. to sets the new HRTF selected by the user
    // but the device may not accept...
    function ChangeAttributes(const aAttribs: TALSContextAttributes): boolean;

    procedure SetListenerPosition(aX, aY, aZ: single);
    procedure SetListenerVelocity(aX, aY, aZ: single);
    procedure SetListenerOrientation(aATX, aATY, aATZ, aUPX, aUPY, aUPZ: single);


    // This is the master gain. It affects all sounds in the context and the playlist
    property MasterGain: TALSBoundedFParam read FMasterGain;

    property ObtainedSampleRate: integer read FObtainedSampleRate;

    property Playlist: TALSPlaylist read FPlaylist;

    property SoundCount: integer read GetSoundCount;
    property Sounds[index:integer]: TALSSound read GetSoundByIndex;

    property ObtainedMonoCount: integer read GetObtainedMonoCount;
    property ObtainedStereoCount: integer read GetObtainedStereoCount;
    property ObtainedAuxiliarySendCount: integer read GetObtainedAuxiliarySendCount;

    property HaveStereoAngle: boolean read FHaveExt_AL_EXT_STEREO_ANGLES;
    property HaveEFX: boolean read GetHaveEXT_ALC_EXT_EFX;
    property HaveFilter: boolean read GetHaveFilter;

    // The list of available resampler.
    // For more info see  https://openal-soft.org/openal-extensions/SOFT_source_resampler.txt
    property ResamplerList: TStringArray read GetResamplerList;
    property DefaultResamplerIndex: integer read FDefaultResamplerIndex;
    property HaveExt_AL_SOFT_source_resampler: boolean read FHaveExt_AL_SOFT_source_resampler;

    // True if the context have the HRTF capability.
    property HaveHRTF: boolean read GetHaveExt_ALC_SOFT_HRTF;
    // Gives the list of available HRTF.
    property HRTFList: TStringArray read GetHRTFList;
    // Return the success (True) or failure (False) of a HRTF change after a
    // call of ChangeAttributes method.
    property HRTFEnabled: boolean read GetHRTFEnabled;
    // The distance model applyed for processing sound in 3D space.
    // Default value is AL_NONE.
    property DistanceModel: TALSDistanceModel read FDistanceModel write SetDistanceModel;
  end;




  TALSLoopbackContext = class;
  TALSProgressEvent = procedure(Sender: TALSLoopbackContext;
                                aTimePos: double;
                                const aFrameBuffer: TALSLoopbackFrameBuffer;
                                var SaveBufferToFile: boolean;
                                var Cancel: boolean) of object;

  { TALSLoopbackContext }

  TALSLoopbackContext = class(TALSPlaybackContext)
  private
    FLoopbackError: TALSError;
    procedure ResetMixingError;
    procedure SetMixingError(aError: TALSError);
    function GetMixingError: boolean;
    function GetMixingStrError: string;
  private
    FOnProgress: TALSProgressEvent;
    FTimeSlice,
    FMixTime: double;
    FFrameBuffer: TALSLoopbackFrameBuffer;
    FIsMixing: boolean;
    procedure SetTimeSlice(AValue: double);
    procedure Update(const aElapsedTime: double);
    procedure InternalCloseDevice; override;
  private
    FFilename: string;
    FFileInfo: TSF_INFO;
    FFile: PSNDFILE;
  private
    procedure RenderAudioToBuffer;
    procedure SaveBufferToFile;
    procedure CloseFile;
    procedure DoExceptionNoCallback;
  public
    // Don't call directly this constructor. Instead, use method
    // ALSManager.CreateDefaultLoopbackContext.
    constructor Create(aDevice: PALSLoopbackDeviceItem);

    // Checks if the specified attributes are supported by a loopback context.
    function IsAttributesSupported( aSampleRate: integer;
                                    aChannels:TALSLoopbackChannel;
                                    aSampleType: TALSLoopbackSampleType ): boolean;

    // Finalize the context creation with the specified attributes.
    procedure InitContext(aAttribs: TALSContextAttributes);

    // Ask to save the mix to the specified audio file.
    // Use function ALSMakeFileFormat(...) to generate the expected file format.
    // e.g: ALSMakeFileFormat( SF_FORMAT_WAV, SF_FORMAT_PCM_16)
    // You can also retrieve the available file formats from
    // ALSManager.ListOfSimpleAudioFileFormat[].Format
    function PrepareSavingToFile(const aFilename: string; aFileFormat: TALSFileFormat): boolean;

    // Call this method before start your mixing process, before any calls to
    // Mix(...)
    procedure BeginOfMix;

    // Call this method to render audio. "OnProgress" event is triggered every
    // time a buffer is filled.
    // The buffer length can be adjusted with property "TimeSlice"
    // Don't forget to put some sounds in playing state otherwise the result
    // will be nothing but silence !
    procedure Mix(aDuration: single);

    // Call this method at the end of your mixing process. It close the output
    // file (if any), free buffer memory, etc...
    procedure EndOfMix;

    // This callback is triggered each time the buffer is filled with audio.
    // It allows your application to:
    //     - control if the current audio buffer content must be saved to the
    //         output file. Usefull to save only a portion of the mix.
    //     - control if the mixing must be stopped.
    //     - retrieve the channels levels and peaks.
    //     - do what you want with the audio data
    // NOTE: as this callback controls all the mixing process, it must be
    //       defined by your application.
    property OnProgress: TALSProgressEvent read FOnProgress write FOnProgress;

    // By default the mixing process use a buffer capacity of 10ms.
    // This means that callback OnProgress is fired every 10ms of audio.
    // With this property, you can adjust this setting according to your need.
    property TimeSlice: double read FTimeSlice write SetTimeSlice;

    property SampleRate: integer read FSampleRate;

    property MixingError: boolean read GetMixingError;
    property MixingStrError: string read GetMixingStrError;

    // The handle returned by libsndfile, after a call to PrepareSavingToFile.
    // Usefull to do some tasks directly with the LibSndFile API, like writing
    // metadata, setting the bitrate mode and/or compression level...
    // Do this tasks before starting the mixing
    property LibSndFileHandle: PSNDFILE read FFile;
  end;


  TALSCaptureContext = class;
  TALSOnCaptureBuffer = procedure(Sender: TALSCaptureContext;
                                  const aBuffer: TALSCaptureFrameBuffer) of object;

  { TALSCaptureContext }

  TALSCaptureContext = class(TALSErrorHandling)
  private
    FDevice: PALCdevice;
    FSampleRate: longword;
    FRemoveDCBiasWhileRecording: boolean;
    FCaptureError: TALSError;
    procedure ResetCaptureError;
    procedure SetCaptureError(aError: TALSError);
    function GetCaptureError: boolean;
    function GetStrCaptureError: string;
  protected
    FCriticalSection: TRTLCriticalSection;
    FThread: TALSThread;
    FThreadRefCount: integer;
    FOnCaptureBuffer: TALSOnCaptureBuffer;
    procedure StartThread;
    procedure StopThread;
    procedure DoUpdate(const {%H-}aElapsedTime: single);
    procedure DoOnCaptureBufferEvent;
  private
    FMonitoringEnabled: boolean;
    FPreAmp: single;
    FState: TALSState;
    FCaptureToFileIsReady: boolean;
    FUserFileName: string;
    FUserFileInfo: TSF_INFO;
    FUserFile: PSNDFILE;

    FPlaybackSound: TALSPlaybackCapturedSound;

    FCapturedFrames: TALSCaptureFrameBuffer;

    FFileWriteErrorWhileCapturing, FALErrorWhileCapturing: boolean;
    function GetChannelsLevel(Index: integer): single;
    function GetChannelsLeveldB(Index: integer): single;
    function GetChannelsPeak(Index: integer): single;
    function GetChannelsPeakdB(Index: integer): single;
    procedure SetMonitoringEnabled(AValue: boolean);
    procedure SetOnCaptureBuffer(AValue: TALSOnCaptureBuffer);
    procedure SetPreAmp(AValue: single);
  public
    // Don't call this contructor directly, instead use
    // ALSManager.CreateDefaultCaptureContext or
    // ALSManager.CreateCaptureContext(...)
    constructor Create(const aCaptureDeviceName: string; aFrequency: longword;
      aFormat: TALSCaptureFormat; aBufferTimeSize: double);

    destructor Destroy; override;

    // Ask to save the captured audio to the specified file.
    // Use function ALSMakeFileFormat(...) to generate the expected file format.
    // e.g: ALSMakeFileFormat( SF_FORMAT_WAV, SF_FORMAT_PCM_16)
    // You can also retrieve the available file formats from
    // ALSManager.ListOfSimpleAudioFileFormat[].Format
    function PrepareSavingToFile(const aFileName: string; aFormat: TALSFileFormat): boolean;

    // Ask to playback the captured audio.
    // This method creates a streamed sound in the specified playback context
    // that will receive the captured audio to playback. You can Add any affects
    // to the returned sound's instance before starting the capture but do not use
    // methods that affect its playing state (Play, Pause, Stop, FadeIn, FadeOut)
    function PrepareToPlayback(aTargetContext: TALSPlaybackContext): TALSPlaybackCapturedSound;

    // Start capture
    procedure StartCapture;

    // Pause capture
    procedure PauseCapture;

    // Stop capture.
    procedure StopCapture;

    // Start(True) or Stop(False) monitoring of the channel's level and peak.
    property MonitoringEnabled: boolean read FMonitoringEnabled write SetMonitoringEnabled;
    // The channel's level expressed in percent. Range is from 0.0 to 1.0
    property ChannelsLevel[Index:integer]: single read GetChannelsLevel;
    // The peak's level expressed in percent. Range is from 0.0 to 1.0
    property ChannelsPeak[Index:integer]: single read GetChannelsPeak;
    // The channel's level expressed in decibel. Range is -60 to 0
    property ChannelsLeveldB[Index:integer]: single read GetChannelsLeveldB;
    // The channel's peak values expressed in decibel. Range is -60 to 0
    property ChannelsPeakdB[Index:integer]: single read GetChannelsPeakdB;

    // The pre-amplification applyed on the captured audio.
    // Range is 0 to 8    0=silence   1=normal    >1=amplified
    // Default value is 1.
    property PreAmp: single read FPreAmp write SetPreAmp;

    // This event is fired when the capture context have a new buffer with audio
    // data. This event is called by the context's thread throught Queue method.
    property OnCaptureBuffer: TALSOnCaptureBuffer read FOnCaptureBuffer write SetOnCaptureBuffer;

    property Frequency: longword read FSampleRate;
    // Sets this property to True to remove the DC bias signal while recording.
    property RemoveDCBias: boolean read FRemoveDCBiasWhileRecording write FRemoveDCBiasWhileRecording;
    // return the state of this recording context: ALS_STOPPED, ALS_RECORDING or
    // ALS_PAUSED
    property State: TALSState read FState;

    // Return True if an error occured while capturing audio.
    property CaptureError: boolean read GetCaptureError;
    // Give an human readable error message of the error.
    property StrCaptureError: string read GetStrCaptureError;

    // The handle returned by libsndfile, after a call to PrepareSavingToFile.
    // Usefull to do some tasks directly with the LibSndFile API, like writing
    // metadata, setting the bitrate mode and/or compression level...
    // Do this tasks after a call to PrepareSavingToFile and before StartCapture
    property LibSndFileHandle: PSNDFILE read FUserFile;
  end;

  TALSAudioFileSubFormat = record
    Name: string;
    Format: longint; //cint;
  end;
  ArrayOfALSAudioFileSubFormat = array of TALSAudioFileSubFormat;

  { TALSAudioFileFormat }

  TALSAudioFileFormat = record
    Name,
    FileExt: string;
    Format: longint; //cint;
    SubFormat: ArrayOfALSAudioFileSubFormat;
    function SubFormatCount: integer;
  end;
  ArrayOfALSAudioFileFormat = array of TALSAudioFileFormat;

  TALSSimplifiedAudioFileFormat = record
    Name,
    FileExt: string;
    Format: TALSFileFormat; //longint;
  end;
  ArrayOfALSSimplifiedAudioFileFormat = array of TALSSimplifiedAudioFileFormat;

  { TALSManager }

  TALSManager = class( TALSErrorHandling )
  private
    FOpenALSoftLibraryLoaded,
    FLibSndFileLibraryLoaded: boolean;
    function GetLibSndFileVersion: string;
    function GetOpenAlSoftVersion: string;
  protected
    procedure InitializeErrorStatus; override;
  private
    FLibrariesSubFolder,
    FOpenALSoftLibraryFilename,
    FLibSNDFileLibraryFilename: string;
    FCompleteFileFormats: ArrayOfALSAudioFileFormat;
    FSimplifiedFileFormats: ArrayOfALSSimplifiedAudioFileFormat;
    procedure DoLoadLibrary;
    procedure DoUnloadLibrary;
  private
    FPlaybackDevices: ArrayOfALSPlaybackDeviceItem;
    FDefaultPlaybackDeviceIndex: integer;
    procedure RetrievePlaybackDevices;
    procedure ClosePlaybackDevice( aDeviceHandle: PALCDevice );
    procedure CloseAllPlaybackDevice;
  private
    FDefaultLoopbackDevice: TALSLoopbackDeviceItem;
    procedure CloseLoopbackDevice( aDeviceHandle: PALCDevice );
  private
    FALSoftLogCallback: TALSoft_LogCallback;
    FALSoftLogCallback_UserPtr: pointer;
    FALSoftLogCallbackIsActive: boolean;
  public
    // Don't use ! Only one instance is allowed and it is created at startup.
    constructor Create;
    destructor Destroy; override;

  public // LIBRARIES

    // OpenAL-Soft provide a callback for its log messages. Use this method to
    // define this callback to be able to save OpenAL-Soft log messages with
    // those from your application.
    // NOTE: this callback must be defined BEFORE the call to LoadLibraries.
    procedure SetOpenALSoftLogCallback(aCallback: TALSoft_LogCallback; aUserPtr: pointer);

    // If the librarie's binaries are in a sub-folder of your application
    // executable, you can inform ALSManager before a call to LoadLibraries.
    // NOTE FOR Mac:(not tested) if you use a bundle, the sub-folder must be in
    // the Resources folder.
    procedure SetLibrariesSubFolder(const aAppLibrariesSubFolder: string);

    // Call this method at the begining of your application to load OpenAL-Soft
    // and LibSndFile library. Librarie's binaries must be located in the
    // application executable folder or sub-folder. In case of sub-folder, use
    // property LibrariesSubFolder to inform ALSManager before calling
    // LoadLibraries.
    procedure LoadLibraries;

    // If the librarie's binaries are in a sub-folder of your application
    // executable, you can inform ALSManager before a call to LoadLibraries.
    // NOTE FOR Mac:(not tested) if you use a bundle, the sub-folder must be in
    // the Resources folder.
    property LibrariesSubFolder: string write FLibrariesSubFolder; deprecated 'will be removed on future update. Use method SetLibrariesSubFolder instead';

    property OpenALSoftLibraryLoaded: boolean read FOpenALSoftLibraryLoaded;
    property LibSndFileLibraryLoaded: boolean read FLibSndFileLibraryLoaded;

    // Return the version of OpenAL-Soft library.
    // You need to load the libraries and create a playback context before
    // querying it.
    property OpenAlSoftVersion: string read GetOpenAlSoftVersion;
    // Return the version of LibSndFile library.
    property LibSndFileVersion: string read GetLibSndFileVersion;

    // Return True if the log callback for OpenALSoft succed.
    property ALSoftLogCallbackIsActive: boolean read FALSoftLogCallbackIsActive;

  public // PLAYBACK DEVICE AND CONTEXT

    // Gives the list of playback device names found by OpenAL-Soft
    function ListOfPlaybackDeviceName: TStringArray;
    // Gives the default playback device name. '' if none.
    function DefaultPlaybackDeviceName: string;

    // Opens the default playback device and creates a context on it.
    // As a device support multiple contexts, you can call this method several
    // times in the same application.
    function CreateDefaultPlaybackContext: TALSPlaybackContext;

    // Opens the specified playback device and creates a context on it with
    // custom attributes.
    // aNameIndex is an index in the list provided by ListOfPlaybackDeviceName.
    // You can set aNameIndex to -1 to refer to the default playback device.
    function CreatePlaybackContext(aNameIndex: integer;
      const aAttribs: TALSContextAttributes): TALSPlaybackContext;

    // Gives the list of the available mixing mode expressed in string.
    // The mixing mode is the way OpenAL-Soft will make the final mix of the
    // sounds on a playback context ( stereo, surround 5.1, HRTF,...) See TALSPlaybackContextOutputMode for the available value.
    function ListOfMixMode: TStringArray;
    // Converts an index in the list of mixing mode to its corresponding enum value.
    function MixModeIndexToEnum( aIndex: integer ): TALSPlaybackContextOutputMode;


  public // LOOPBACK DEVICE AND CONTEXT
    function CreateDefaultLoopbackContext: TALSLoopbackContext;


  public // CAPTURE DEVICE AND CONTEXT

    // Gives the list of capture device names found by OpenAL-Soft
    function ListOfCaptureDeviceName: TStringArray;
    // Gives the name of the default capture device. '' if none.
    function DefaultCaptureDeviceName: string;

    // Opens the default capture device and creates a capture context on it.
    function CreateDefaultCaptureContext: TALSCaptureContext;

    // Opens the specified capture device and creates a capture context on it
    // with custom attributes.
    // aNameIndex is an index in the list provided by ListOfCaptureDeviceName.
    // You can set aNameIndex to -1 to refer to the default capture device.
    // aFrequency: the frequency of the capture.
    // aFormat: the sample format
    // aBufferSize: the length in seconds of the internal buffer used by OpenAL-Soft
    function CreateCaptureContext(aNameIndex: integer; aFrequency: longword;
      aFormat: TALSCaptureFormat; aBufferSize: double): TALSCaptureContext;

  public  // UTILS

    // Gives the complete list of audio file format and sub-format supported by
    // LibSndFile
    function ListOfAudioFileFormat_Complete: ArrayOfALSAudioFileFormat;

    // Gives a simplified list of audio file format supported by libsndfile
    function ListOfAudioFileFormat_Simplified: ArrayOfALSSimplifiedAudioFileFormat;

    // Gives the supported list of audio file extension formatted to be used
    // directly with TOpenDialog.Filter and TSaveDialog.Filter property.
    // StringForAudioFile will be shown in the dialog near audio file filters.
    // StringForAllFile will be shown in the dialog near '*.*' filter.
    // If you don't want the all file filter, keep StringForAllFile empty.
    function DialogFileFilters(const StringForAudioFile, StringForAllFile: string): string;

    // Gives the list of available output mode expressed in string. Output mode
    // can be applyed only on playback context. this is the way OpenAL-Soft will
    // render the final mix. Value can be stereo, surround 5.1, HRTF,...
    // See TOALSPlaybackMixMode for the available mode.
    function ListOfPlaybackOutputMode: TStringArray;
    // convert an index to enum TALSPlaybackContextOutputMode
    function PlaybackOutputModeIndexToEnum(aIndex: integer): TALSPlaybackContextOutputMode;
  end;


var
  // ALSManager is the tools box  for ALSound.
  // It is automaticaly created and freed on Initialization and Finalization section.
  // Use this unique instance to retrieve the names of the audio devices available on your system,
  // or to create your playback, loopback or capture context,
  // or to retrieve some usefull informations.
  ALSManager: TALSManager;

implementation

uses Math;

var
{$ifndef ALS_ENABLE_CONTEXT_SWITCHING}
  _SingleContextIsCurrent: boolean=FALSE;
{$else}
  _CSLockContext: TRTLCriticalSection;
{$endif}

procedure LockContext( aContext: PALCcontext );
begin
  if ALSManager.Error then
    exit;
{$ifndef ALS_ENABLE_CONTEXT_SWITCHING}
  if not _SingleContextIsCurrent then
  begin
    _SingleContextIsCurrent := True;
    alcMakeContextCurrent( aContext );
  end;
{$else}
  EnterCriticalSection( _CSLockContext );
  alcMakeContextCurrent( aContext );
{$endif}
end;

procedure UnlockContext;
begin
{$ifdef ALS_ENABLE_CONTEXT_SWITCHING}
  if ALSManager.Error then
    exit;
  LeaveCriticalSection( _CSLockContext );
{$endif}
end;

{$undef ALS_INTERFACE}
{$define ALS_IMPLEMENTATION}
{$include als_error.inc}
{$include als_effectpresets.inc}
{$include eaxreverbpresets.inc}
{$include als_directfilter.inc}
{$include als_frame_buffers.inc}
{$include als_velocity_curve.inc}
{$include als_deviceitem.inc}
{$undef ALS_IMPLEMENTATION}

function ALSMakeFileFormat(aFileMajorFormat: TALSFileMajorFormat;
  aFileSubformat: TALSFileSubFormat; aFileEndian: TALSFileEndian): TALSFileFormat;
begin
  Result := TALSFileFormat(Ord(aFileMajorFormat) or Ord(aFileSubformat) or
                Ord(aFileEndian));
end;


{ TALSLoopbackContext }

procedure TALSLoopbackContext.ResetMixingError;
begin
  FLoopbackError := als_NoError;
end;

procedure TALSLoopbackContext.SetMixingError(aError: TALSError);
begin
  if FLoopbackError = als_NoError then
    FLoopbackError := aError;
end;

procedure TALSLoopbackContext.Update(const aElapsedTime: double);
var
  i: integer;
  s: TALSSound;
begin
  if not Error then
    LockContext( FContext );

  try
    if FMasterGain.State <> alspsNo_CHANGE then
    begin
      FMasterGain.OnElapse( aElapsedTime );
      if not Error then
        alListenerf( AL_GAIN, FMasterGain.Value );
    end;
    // Kill or update sounds
    for i := FList.Count - 1 downto 0 do
    begin
      s := TALSSound(FList.Items[i]);
      if s.FKill then
        InternalDeleteSound(i)
      else
        s.Update(aElapsedTime);
    end;
    // update playlist (if exists)
    if FPlaylist <> nil then
      FPlaylist.Update(aElapsedTime);
  finally
    if not Error then
      UnlockContext;
  end;
end;

procedure TALSLoopbackContext.InternalCloseDevice;
begin
  ALSManager.CloseLoopbackDevice(FParentDevice);
end;

procedure TALSLoopbackContext.RenderAudioToBuffer;
begin
  LockContext(FContext);
  try
    FParentDeviceItem^.FalcRenderSamplesSOFT(FParentDevice,
       FFrameBuffer.Data, ALCsizei(FFrameBuffer.FrameCapacity));
    FFrameBuffer.FrameCount := FFrameBuffer.FrameCapacity;
  finally
    UnlockContext;
  end;
end;

procedure TALSLoopbackContext.SaveBufferToFile;
var written: sf_count_t;
begin
  if (FFile <> NIL) then
  begin
    written := 0; // avoid compilation hint
    case FFrameBuffer.SampleType of
      ALC_SHORT_SOFT: written := sf_writef_short(FFile, FFrameBuffer.Data, FFrameBuffer.FrameCount);
      ALC_INT_SOFT: written := sf_writef_int(FFile, FFrameBuffer.Data, FFrameBuffer.FrameCount);
      ALC_FLOAT_SOFT: written := sf_writef_float(FFile, FFrameBuffer.Data, FFrameBuffer.FrameCount);
    end;

    // check write file error
    if written <> sf_count_t(FFrameBuffer.FrameCount) then
      SetMixingError(als_FileWriteErrorWhileMixing);
  end;
end;

procedure TALSLoopbackContext.CloseFile;
begin
  if FFile <> NIL then
  begin
    // close file
    sf_write_sync(FFile);
    sf_close(FFile);

    FFile := NIL;
  end;
end;

procedure TALSLoopbackContext.DoExceptionNoCallback;
begin
  Raise Exception.Create('TALSLoopbackContext.StartMixing - Callback OnProgress must be defined');
end;

procedure TALSLoopbackContext.SetTimeSlice(AValue: double);
begin
  if FIsMixing then exit;

  AValue := EnsureRange(AValue, 0.001, 0.100);
  if FTimeSlice = AValue then Exit;
  FTimeSlice := AValue;
end;

function TALSLoopbackContext.GetMixingError: boolean;
begin
  Result := FLoopbackError <> als_NoError;
end;

function TALSLoopbackContext.GetMixingStrError: string;
begin
  Result := ALSound.GetStrError(FLoopbackError);
end;

constructor TALSLoopbackContext.Create(aDevice: PALSLoopbackDeviceItem);
begin
  FExecutingConstructor := True;
  InitializeErrorStatus;
  ResetMixingError;
  FParentDevice := aDevice^.Handle;
  FParentDeviceItem := PALSDeviceItem(aDevice);

  if aDevice^.Handle = NIL then
    SetError(als_ALCanNotOpenLoopbackDevice)
  else if not aDevice^.FHaveExt_ALC_SOFT_loopback then
         SetError(als_ALContextCanNotLoopback);

  InitCriticalSection( FCriticalSection );

  CreateParameters;
  FTimeSlice := 0.010;

  FExecutingConstructor := False;
end;

function TALSLoopbackContext.IsAttributesSupported(aSampleRate: integer;
  aChannels: TALSLoopbackChannel; aSampleType: TALSLoopbackSampleType): boolean;
begin
  if Error then
    Result := False
  else
  begin
    Result := FParentDeviceItem^.FalcIsRenderFormatSupportedSOFT(FParentDevice,
        ALCsizei(aSampleRate), ALCsizei(Ord(aChannels)), ALCenum(Ord(aSampleType)));
    alcGetError(FPArentDevice);
  end;
end;

procedure TALSLoopbackContext.InitContext(aAttribs: TALSContextAttributes);
begin
  ResetMixingError;

  if not aAttribs.FLoopbackModeEnabled then
  begin
    Raise Exception.Create('TALSLoopbackContext.InitContext: context attributes not configured as loopback');
    SetError(als_ALAttributesNotConfiguredForLoopback);
    exit;
  end;

  FExecutingConstructor := True;
  if not Error then
    InitializeALContext(aAttribs);

  if not Error then
  begin
    FFrameBuffer.InitDefault;
    FFrameBuffer.Init(aAttribs.FLoopbackChannelType, aAttribs.FLoopbackSampleType);
  end;

  FExecutingConstructor := False;
end;

function TALSLoopbackContext.PrepareSavingToFile(const aFilename: string;
  aFileFormat: TALSFileFormat): boolean;
begin
  Result := False;

  if Error or
     FIsMixing then
    exit;

  FFilename := aFilename;
  // Create output audio file
  FFileInfo.SampleRate := FSampleRate;
  FFileInfo. Format := aFileFormat;
  FFileInfo.Channels := FFrameBuffer.ChannelCount;
  FFile := ALSOpenAudioFile(aFilename, SFM_WRITE, FFileInfo);

  Result := FFile <> NIL;
end;

procedure TALSLoopbackContext.BeginOfMix;
begin
  if FOnProgress = NIL then
    DoExceptionNoCallback
  else begin
    FIsMixing := True;
    FMixTime := 0.0;
  end;
end;

procedure TALSLoopbackContext.Mix(aDuration: single);
var
  deltaTime: double;
  flagSaveToFile, flagCancel: boolean;
begin
  if Error or MixingError then
    exit;

  flagSaveToFile := (FFile <> NIL);
  flagCancel := False;

  while (aDuration > 0.0005) and not MixingError and not flagCancel do
  begin
    // we need to mix slice of time defined by user
    deltaTime := Min(aDuration, FTimeSlice);
    aDuration := aDuration - deltaTime;

    // reserves buffer memory
    if FFrameBuffer.FrameCapacity <> Round(FSampleRate*deltaTime) then
      FFrameBuffer.FrameCapacity := Round(FSampleRate*deltaTime);

    // Render audio
    RenderAudioToBuffer;

    // update the context
    deltaTime := FFrameBuffer.FrameCount/FSampleRate;
    Update(deltaTime);
    FMixTime := FMixTime + deltaTime;

    // Callback
    FOnProgress(Self, FMixTime, FFrameBuffer, flagSaveToFile, flagCancel);

    if flagSaveToFile then
      SaveBufferToFile;
  end;
end;

procedure TALSLoopbackContext.EndOfMix;
begin
  if FIsMixing then begin
    CloseFile;
    FFrameBuffer.FreeMemory;
    FIsMixing := False;
  end;
end;

{ TALSAudioFileFormat }

function TALSAudioFileFormat.SubFormatCount: integer;
begin
  Result := Length(SubFormat);
end;

{ TALSErrorHandling }

procedure TALSErrorHandling.InitializeErrorStatus;
begin
  FErrorCode := ALSManager.FErrorCode;
end;

function TALSErrorHandling.CheckALError(aErrorCode: TALSError): boolean;
var
  err: ALenum;
begin
  err := openalsoft.alGetError();
  if (err <> openalsoft.AL_NO_ERROR) then
  begin
    Result := True;
    SetALError(aErrorCode, err);
  end
  else
    Result := False;
end;

function TALSErrorHandling.CheckALCError(aDevice: PALCDevice; aErrorCode: TALSError): boolean;
var
  err: ALenum;
begin
  err := openalsoft.alcGetError(aDevice);
  if (err <> openalsoft.ALC_NO_ERROR) then
  begin
    Result := True;
    if FErrorCode = als_NoError then
    begin
      FErrorCode := aErrorCode;
      FALCErrorCode := err;
    end;
  end
  else
    Result := False;
end;

procedure TALSErrorHandling.SetError(aErrorCode: TALSError);
begin
  if FErrorCode = als_NoError then
    FErrorCode := aErrorCode;
end;

procedure TALSErrorHandling.SetALError(aErrorCode: TALSError; aALError: ALenum);
begin
  if FErrorCode = als_NoError then
  begin
    FErrorCode := aErrorCode;
    FALErrorCode := aALError;
  end;
end;

procedure TALSErrorHandling.SetALCError(aErrorCode: TALSError; aALCError: ALenum);
begin
  if FErrorCode = als_NoError then
  begin
    FErrorCode := aErrorCode;
    FALCErrorCode := aALCError;
  end;
end;

function TALSErrorHandling.GetStrError: string;
begin
  Result := ALSound.GetStrError(FErrorCode);
  if FALErrorCode <> AL_NO_ERROR then
    Result := Result + ' (ALerror: $'+IntToHex(FALErrorCode, 4)+')';
  if FALCErrorCode <> ALC_NO_ERROR then
    Result := Result + ' (ALCerror: $'+IntToHex(FALCErrorCode, 4)+')';
end;

function TALSErrorHandling.GetErrorState: boolean;
begin
  Result := FErrorCode <> als_NoError;
end;

{ TALSCaptureContext }

procedure TALSCaptureContext.ResetCaptureError;
begin
  FCaptureError := als_NoError;
end;

procedure TALSCaptureContext.SetCaptureError(aError: TALSError);
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FCaptureError = als_NoError then
      FCaptureError := aError;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TALSCaptureContext.GetStrCaptureError: string;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := ALSound.GetStrError(FCaptureError);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TALSCaptureContext.StartThread;
begin
  if FThreadRefCount = 0 then
  begin
    alcCaptureStart( FDevice );
    FThread := TALSThread.Create(@DoUpdate, 2, True);
    FThread.Priority := tpHighest;
  end;
  inc(FThreadRefCount);
end;

procedure TALSCaptureContext.StopThread;
begin
  if FThreadRefCount > 0 then
  begin
    dec(FThreadRefCount);
    if FThreadRefCount = 0 then
    begin
      alcCaptureStop( FDevice );
      FThread.Terminate;
      FThread.WaitFor;
      FThread.Free;
      FThread := nil;
    end;
  end;
end;

procedure TALSCaptureContext.DoUpdate(const aElapsedTime: single);
var
  framesRead: cint;
  writtenOnFile: sf_count_t;
begin
  framesRead := 0;
  // gets available frames (not byte !)
  alcGetIntegerv(FDevice, ALC_CAPTURE_SAMPLES, 1, @framesRead);

  if framesRead > 0 then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      // adjust buffer size - not necessary, just in case...
      if FCapturedFrames.FrameCapacity < longword(framesRead) then
        FCapturedFrames.FrameCapacity := framesRead;

      // Fill buffer with captured frames
      alcCaptureSamples(FDevice, FCapturedFrames.Data, framesRead);
      FCapturedFrames.FrameCount := framesRead;

      if alcGetError(FDevice) <> AL_NO_ERROR then
      begin
        SetCaptureError(als_ALErrorWhileCapturing);
        FALErrorWhileCapturing := True;
        SetCaptureError(als_ALErrorWhileCapturing);
      end;

      // Remove DC bias from the signal
      if FRemoveDCBiasWhileRecording then
        FCapturedFrames.RemoveDCBias;

      // Apply pre-amplification
      if FPreAmp <> 1.0 then
        FCapturedFrames.Amplify(FPreAmp);

      // Compute channels level/peak
      if FMonitoringEnabled then
        FCapturedFrames.ComputeChannelsLevel;

      if FState = ALS_RECORDING then
      begin
        // Send data to playback
        if FPlaybackSound <> NIL then
          FPlaybackSound.QueueBuffer( @FCapturedFrames );

        // Save data to audio file
        if FCaptureToFileIsReady and
           not FFileWriteErrorWhileCapturing and
           not FALErrorWhileCapturing then
        begin
          writtenOnFile := 0;
          case FCapturedFrames.UseFloat of
            False: writtenOnFile := sf_writef_short(FUserFile, FCapturedFrames.Data, FCapturedFrames.FrameCount);
            True: writtenOnFile := sf_writef_float(FUserFile, FCapturedFrames.Data, FCapturedFrames.FrameCount);
          end;
          FFileWriteErrorWhileCapturing := writtenOnFile <> sf_count_t(FCapturedFrames.FrameCount);
          if FFileWriteErrorWhileCapturing then
          SetCaptureError(als_FileWriteErrorWhileCapturing);
        end;

      end;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
    // Send event to main thread
    if FOnCaptureBuffer <> NIL then
      FThread.Queue(FThread, @DoOnCaptureBufferEvent);
  end;
end;

procedure TALSCaptureContext.DoOnCaptureBufferEvent;
begin
  if FOnCaptureBuffer <> NIL then
    FOnCaptureBuffer(Self, FCapturedFrames);
end;

function TALSCaptureContext.GetChannelsLevel(Index: integer): single;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := FCapturedFrames.ChannelsLevel[Index];
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TALSCaptureContext.GetCaptureError: boolean;
begin
  Result := FCaptureError <> als_NoError;
end;

function TALSCaptureContext.GetChannelsLeveldB(Index: integer): single;
begin
  Result := LinearTodB(GetChannelsLevel(Index));
end;

function TALSCaptureContext.GetChannelsPeak(Index: integer): single;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := FCapturedFrames.ChannelsPeak[Index];
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TALSCaptureContext.GetChannelsPeakdB(Index: integer): single;
begin
  Result := LinearTodB(GetChannelsPeak(Index));
end;

procedure TALSCaptureContext.SetMonitoringEnabled(AValue: boolean);
begin
  if FMonitoringEnabled = AValue then Exit;

  FMonitoringEnabled := AValue;
  if Error then
    exit;

  if not AValue then
  begin
    StopThread;
    FCapturedFrames.ResetChannelsLevelToZero;
  end
  else StartThread;
end;

procedure TALSCaptureContext.SetOnCaptureBuffer(AValue: TALSOnCaptureBuffer);
begin
  if FOnCaptureBuffer=AValue then Exit;

  EnterCriticalSection(FCriticalSection);
  try
    FOnCaptureBuffer := AValue;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TALSCaptureContext.SetPreAmp(AValue: single);
begin
  AValue := EnsureRange(AValue, ALS_VOLUME_MIN, ALS_VOLUME_MAXAMP);

  if FPreAmp = AValue then Exit;

  EnterCriticalSection(FCriticalSection);
  try
    FPreAmp := AValue;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

constructor TALSCaptureContext.Create(const aCaptureDeviceName: string;
  aFrequency: longword; aFormat: TALSCaptureFormat; aBufferTimeSize: double);
begin
  ResetCaptureError;
  InitializeErrorStatus;
  FState := ALS_STOPPED;
  FSampleRate := aFrequency;

  FCapturedFrames.Init( aFormat );
  FCapturedFrames.FrameCapacity := Round(aFrequency * aBufferTimeSize);

  // opens the capture device
  if not Error then
  begin
    if aCaptureDeviceName = '' then
      FDevice := alcCaptureOpenDevice(nil, aFrequency, ALCenum(Ord(aFormat)),
                    FCapturedFrames.FrameCapacity)
    else
      FDevice := alcCaptureOpenDevice(PChar(aCaptureDeviceName), aFrequency,
                    ALCenum(Ord(aFormat)), FCapturedFrames.FrameCapacity);

    if FDevice = nil then
      SetError(als_ALCanNotOpenCaptureDevice);
  end;

  InitCriticalSection(FCriticalSection);
  FPreAmp := 1.0;
end;

destructor TALSCaptureContext.Destroy;
begin
  StopCapture;
  if FThread <> NIL then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;

  FCapturedFrames.FreeMemory;
  DoneCriticalSection(FCriticalSection);

  if not Error then
  begin
    alcCaptureCloseDevice(FDevice);
  end;
  inherited Destroy;
end;

function TALSCaptureContext.PrepareSavingToFile(const aFileName: string; aFormat: TALSFileFormat): boolean;
begin
  ResetCaptureError;
  if Error then
  begin
    Result := False;
    exit;
  end;

  FUserFileName := aFilename;
  FUserFileInfo.SampleRate := FSampleRate;
  FUserFileInfo.Format := aFormat;
  FUserFileInfo.Channels := FCapturedFrames.ChannelCount;
  FUserFile := ALSOpenAudioFile(aFilename, SFM_WRITE, FUserFileInfo);

  FCaptureToFileIsReady := FUserFile <> nil;
  Result := FCaptureToFileIsReady;
end;

function TALSCaptureContext.PrepareToPlayback(aTargetContext: TALSPlaybackContext): TALSPlaybackCapturedSound;
begin
  if FPlaybackSound <> NIL then
    FPlaybackSound.Kill;

  FPlaybackSound := aTargetContext.AddCapturePlayback(FSampleRate, @FCapturedFrames);

  Result := FPlaybackSound;
end;

procedure TALSCaptureContext.StartCapture;
begin
  if Error then
    exit;
  if FState = ALS_RECORDING then
    exit;
  if FState = ALS_PAUSED then
  begin
    EnterCriticalSection(FCriticalSection);
    try
     FState := ALS_RECORDING;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
    exit;
  end;

  FFileWriteErrorWhileCapturing := False;
  FALErrorWhileCapturing := False;

  StartThread;
  FState := ALS_RECORDING;
end;

procedure TALSCaptureContext.PauseCapture;
begin
  if Error or (FState = ALS_STOPPED) then exit;

  EnterCriticalSection(FCriticalSection);
  try
    if FState = ALS_PAUSED then
      FState := ALS_RECORDING
    else if FState = ALS_RECORDING then
      FState := ALS_PAUSED;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TALSCaptureContext.StopCapture;
begin
  if Error or
     (FState = ALS_STOPPED) then
    exit;

  // stop capture
  StopThread;
  FState := ALS_STOPPED;

  // Release the sound instance for playback
  if FPlaybackSound <> NIL then
  begin
    FPlaybackSound.Kill;
    FPlaybackSound := NIL;
  end;

  if FCaptureToFileIsReady then
  begin
    // close the capture file
    sf_write_sync(FUserFile);
    if sf_close(FUserFile) <> 0 then
      SetCaptureError(als_CanNotCloseCaptureFile);
    FCaptureToFileIsReady := False;
  end;
end;





{ TALSManager }

procedure TALSManager.DoLoadLibrary;
var
  names: TStringArray;
  i: integer;
  {$if DEFINED(Darwin)}f, bundleName: string;{$endif}
begin
  if not FOpenALSoftLibraryLoaded then
  begin
    names := nil;

    {$if DEFINED(Windows)}
    SetLength(names, 3);
    if Length(FLibrariesSubFolder) = 0 then
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), 'soft_oal.dll'])
    else
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), FLibrariesSubFolder, 'soft_oal.dll']);
    names[1] := 'soft_oal.dll';
    names[2] := 'OpenAL32.dll';
    {$endif}

    {$if DEFINED(Linux)}
    SetLength(names, 4);
    if Length(FLibrariesSubFolder) = 0 then
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), 'libopenal.so'])
    else
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), FLibrariesSubFolder, 'libopenal.so']);
    names[1] := 'libopenal.so';
    names[2] := 'libopenal.so.0';
    names[3] := 'libopenal.so.1';
    {$endif}

    {$if DEFINED(Darwin)}
     {$ifdef LCL}
     f := Application.Location;
     {$else}
     f := ExtractFilePath(ParamStr(0));
     {$endif}
     SetLength(names, 1);
     bundleName := '/'+ApplicationName+'.app';
     i := Pos(bundleName, ParamStr(0));
     if i <> 0 then
     begin
       // executable is included in a bundle
       if Length(FLibrariesSubFolder) = 0 then
         names[0] := ConcatPaths([copy(f, 1, i-1), bundleName, 'Contents/Resources/libopenal.dylib'])
       else
         names[0] := ConcatPaths([copy(f, 1, i-1), bundleName, 'Contents/Resources', FLibrariesSubFolder, 'libopenal.dylib']);
     end
     else
     begin
       // no bundle
       if Length(FLibrariesSubFolder) = 0 then
         names[0] := ConcatPaths([f, 'libopenal.dylib'])
       else
         names[0] := ConcatPaths([f, FLibrariesSubFolder, 'libopenal.dylib']);
     end;
    {$endif}

    FOpenALSoftLibraryLoaded := False;
    FOpenALSoftLibraryFilename := '';
    for i:=0 to High(names) do
      if LoadOpenALCoreLibrary(names[i]) then
      begin
        FOpenALSoftLibraryLoaded := True;
        FOpenALSoftLibraryFilename := names[i];
        break;
      end;
  end;

  if not FLibSndFileLibraryLoaded then
  begin
    {$if DEFINED(Windows)}
    SetLength(names, 2);
    if Length(FLibrariesSubFolder) = 0 then
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), 'sndfile.dll'])
    else
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), FLibrariesSubFolder, 'sndfile.dll']);
    names[1] := 'sndfile.dll';
    {$endif}

    {$if DEFINED(Linux)}
    SetLength(names, 2);
    if Length(FLibrariesSubFolder) = 0 then
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), 'libsndfile.so'])
    else
      names[0] := ConcatPaths([ExtractFilePath(ParamStr(0)), FLibrariesSubFolder, 'libsndfile.so']);
    names[1] := 'libsndfile.so';
    {$endif}

    {$if defined(Darwin)}
    // NOT TESTED !!
    {$ifdef LCL}
    f := Application.Location;
    {$else}
    f := ExtractFilePath(ParamStr(0));
    {$endif}
    SetLength(names, 1);
    bundleName := '/'+ApplicationName+'.app';
    i := Pos(bundleName, ParamStr(0));
    if i <> 0 then
    begin
      if Length(FLibrariesSubFolder) = 0 then
        names[0] := ConcatPaths([copy(f, 1, i-1), bundleName, 'Contents/Resources/libsndfile.dylib'])
      else
        names[0] := ConcatPaths([copy(f, 1, i-1), bundleName, 'Contents/Resources', FLibrariesSubFolder, 'libsndfile.dylib']);
    end
    else
    begin
      if Length(FLibrariesSubFolder) = 0 then
        names[0] := ConcatPaths([f, 'libsndfile.dylib'])
      else
        names[0] := ConcatPaths([f, FLibrariesSubFolder, 'libsndfile.dylib']);
    end;
    {$endif}

    FLibSndFileLibraryLoaded := False;
    FLibSndFileLibraryFilename := '';
    for i:=0 to High(names) do
      if LoadSndFileLibrary(names[i]) then
      begin
        FLibSndFileLibraryLoaded := True;
        FLibSndFileLibraryFilename := names[i];
        break;
      end;
  end;

  InitializeErrorStatus;
end;

function TALSManager.GetLibSndFileVersion: string;
begin
  if FLibSndFileLibraryLoaded then
    Result := StrPas(sf_version_string())
  else
    Result := StrALS_LibSndFileNotLoaded;
end;

function TALSManager.GetOpenAlSoftVersion: string;
begin
  if FOpenALSoftLibraryLoaded and (alGetString <> NIL) then
    Result := StrPas(alGetString(AL_VERSION))
  else
    Result := StrALS_ALLibraryNotLoaded;
end;

procedure TALSManager.InitializeErrorStatus;
begin
  if not FOpenALSoftLibraryLoaded then
    SetError(als_ALLibraryNotLoaded)
  else if not FLibSndFileLibraryLoaded then
    SetError(als_LibSndFileNotLoaded)
  else FErrorCode := als_NoError;
end;

procedure TALSManager.RetrievePlaybackDevices;
var
  A: TStringArray;
  i: integer;
  _defaultDeviceName: string;
begin
  FPlaybackDevices := NIL;
  FDefaultPlaybackDeviceIndex := -1;

  if not Error then
  begin
    A := openalsoft.GetDeviceNames;
    if Length(A) = 0 then exit;

    _defaultDeviceName := openalsoft.GetDefaultDeviceName;
    SetLength(FPlaybackDevices, Length(A));
    for i:=0 to High(A) do
    begin
      FPlaybackDevices[i].InitDefault;
      FPlaybackDevices[i].Name := A[i];
      FPlaybackDevices[i].Handle := NIL;
      FPlaybackDevices[i].OpenedCount := 0;
      if A[i]=_defaultDeviceName then
        FDefaultPlaybackDeviceIndex := i;
    end;
  end;
end;

procedure TALSManager.ClosePlaybackDevice(aDeviceHandle: PALCDevice);
var
  i: integer;
begin
  if not Error and (aDeviceHandle <> NIL) then
    for i:=0 to High(FPlaybackDevices) do
      if FPlaybackDevices[i].Handle = aDeviceHandle then
      begin
        FPlaybackDevices[i].Close;
        exit;
      end;
end;

procedure TALSManager.CloseAllPlaybackDevice;
var
  i: integer;
begin
  if not Error then
    for i:=0 to High(FPlaybackDevices) do
      if FPlaybackDevices[i].OpenedCount > 0 then
        FPlaybackDevices[i].DoCloseDevice;
end;

procedure TALSManager.CloseLoopbackDevice(aDeviceHandle: PALCDevice);
begin
  if not Error and (aDeviceHandle <> NIL) then
  begin
    if aDeviceHandle = FDefaultLoopbackDevice.Handle then
      FDefaultLoopbackDevice.Close;
  end;
end;

constructor TALSManager.Create;
begin
  if ALSManager <> NIL then
  begin
    Exception.Create('Don''t create TALSManager instance yourself !'+lineending+
           'Only one instance is allowed and it is created at application initialization');
  end;

  FDefaultLoopbackDevice.InitDefault;
end;

destructor TALSManager.Destroy;
begin
  CloseAllPlaybackDevice;

  if FDefaultLoopbackDevice.OpenedCount > 0 then
    FDefaultLoopbackDevice.DoCloseDevice;

  DoUnloadLibrary;
  inherited Destroy;
end;

procedure TALSManager.SetOpenALSoftLogCallback(aCallback: TALSoft_LogCallback;
  aUserPtr: pointer);
begin
  FALSoftLogCallback := aCallback;
  FALSoftLogCallback_UserPtr := aUserPtr;
end;

procedure TALSManager.SetLibrariesSubFolder(const aAppLibrariesSubFolder: string);
begin
  FLibrariesSubFolder := aAppLibrariesSubFolder;
end;

procedure TALSManager.LoadLibraries;
begin
  DoUnloadLibrary;
  DoLoadLibrary;

  FALSoftLogCallbackIsActive := False;
  if FALSoftLogCallback <> NIL then begin
    FALSoftLogCallbackIsActive := SetALSoft_LogCallback(FALSoftLogCallback, FALSoftLogCallback_UserPtr);
  end;

  RetrievePlaybackDevices;
end;

procedure TALSManager.DoUnloadLibrary;
begin
  if FOpenALSoftLibraryLoaded then
    UnloadOpenALSoftLibrary;

  if FLibSndFileLibraryLoaded then
    UnloadSndFileLibrary;

  FOpenALSoftLibraryLoaded := False;
  FLibSndFileLibraryLoaded := False;
end;

function TALSManager.ListOfPlaybackDeviceName: TStringArray;
begin
  if not Error then
    Result := openalsoft.GetDeviceNames
  else
    Result := NIL;
end;

function TALSManager.DefaultPlaybackDeviceName: string;
begin
  Result := openalsoft.GetDefaultDeviceName;
end;

function TALSManager.ListOfCaptureDeviceName: TStringArray;
begin
  if not Error then
    Result := openalsoft.GetCaptureDeviceNames
  else
    Result := NIL;
end;

function TALSManager.DefaultCaptureDeviceName: string;
begin
  if not Error then
    Result := openalsoft.GetDefaultCaptureDeviceName
  else
    Result := '';
end;


function TALSManager.ListOfAudioFileFormat_Complete: ArrayOfALSAudioFileFormat;
var
  info: TSF_FORMAT_INFO;
  sfinfo: TSF_INFO;
  format, major_count, subtype_count, m, s, k: cint;
begin
  if not FLibSndFileLibraryLoaded then
    Result := NIL
  else
  begin
    if Length(FCompleteFileFormats) = 0 then
    begin
      sf_command(nil, SFC_GET_FORMAT_MAJOR_COUNT, @major_count, sizeof(cint));
      sf_command(nil, SFC_GET_FORMAT_SUBTYPE_COUNT, @subtype_count, sizeof(cint));

      SetLength(FCompleteFileFormats, major_count);
      sfinfo.channels := 1;
      for m := 0 to major_count - 1 do
      begin
        info.format := m;
        sf_command(nil, SFC_GET_FORMAT_MAJOR, @info, SizeOf(TSF_FORMAT_INFO));
        FCompleteFileFormats[m].Name := StrPas(info.Name);
        FCompleteFileFormats[m].FileExt := StrPas(info.extension);
        FCompleteFileFormats[m].Format := info.format;

        SetLength(FCompleteFileFormats[m].SubFormat, 0);
        format := info.format;
        for s := 0 to subtype_count - 1 do
        begin
          info.format := s;
          sf_command(nil, SFC_GET_FORMAT_SUBTYPE, @info, SizeOf(TSF_FORMAT_INFO));

          format := (format and SF_FORMAT_TYPEMASK) or info.format;
          sfinfo.format := format;
          if sf_format_check(@sfinfo) <> 0 then
          begin  // sub-format is valid
            k := Length(FCompleteFileFormats[m].SubFormat);
            SetLength(FCompleteFileFormats[m].SubFormat, k + 1);
            FCompleteFileFormats[m].SubFormat[k].Format := info.format;
            FCompleteFileFormats[m].SubFormat[k].Name := info.Name;
          end;
        end;
      end;
    end;
    Result := FCompleteFileFormats;
  end;
end;

function TALSManager.ListOfAudioFileFormat_Simplified: ArrayOfALSSimplifiedAudioFileFormat;
var
  format_info: TSF_FORMAT_INFO;
  k, count: cint;
begin
  if not FLibSndFileLibraryLoaded then
    Result := NIL
  else
  begin
    if Length(FSimplifiedFileFormats) = 0 then
    begin
      sf_command(NIL, SFC_GET_SIMPLE_FORMAT_COUNT, @count, SizeOf(cint));
      SetLength(FSimplifiedFileFormats, count);

      for k := 0 to count-1 do
      begin
        format_info.format := k;
        sf_command(NIL, SFC_GET_SIMPLE_FORMAT, @format_info, SizeOf(format_info));
        FSimplifiedFileFormats[k].Name := format_info.name;
        FSimplifiedFileFormats[k].FileExt := format_info.extension;
        FSimplifiedFileFormats[k].Format := format_info.format;
      end;
    end;
    Result := FSimplifiedFileFormats;
  end;
end;

function TALSManager.DialogFileFilters(const StringForAudioFile, StringForAllFile: string): string;
var
  A: ArrayOfALSSimplifiedAudioFileFormat;
  i: integer;
  flag: boolean;
begin
  Result := '';

  if not Error then
  begin
    A := ListOfAudioFileFormat_Simplified;
    Result := StringForAudioFile+'|';
    flag := False;
    for i:=0 to High(A) do
      if Pos(A[i].FileExt, Result) = 0 then
      begin
        if flag then
          Result := Result + ';';
        Result := Result + '*.' + A[i].FileExt;
        flag := True;
      end;
  end;

  if (Length(StringForAllFile) > 0) or Error then
  begin
    if Result<>'' then
      Result := Result + '|';
    Result := Result + StringForAllFile+'|*.*';
  end;
end;

function TALSManager.ListOfPlaybackOutputMode: TStringArray;
begin
  Result := Nil;
  SetLength(Result, 7);
  Result[0] := 'ALC_SURROUND_5_1';
  Result[1] := 'ALC_SURROUND_6_1';
  Result[2] := 'ALC_SURROUND_7_1';
  Result[3] := 'ALC_ANY_SOFT';
  Result[4] := 'ALC_STEREO_BASIC';
  Result[5] := 'ALC_STEREO_UHJ';
  Result[6] := 'ALC_STEREO_HRTF';
end;

function TALSManager.PlaybackOutputModeIndexToEnum(aIndex: integer): TALSPlaybackContextOutputMode;
begin
  case aIndex of
    0: Result := ALC_SURROUND_5_1;
    1: Result := ALC_SURROUND_6_1;
    2: Result := ALC_SURROUND_7_1;
    3: Result := ALC_ANY_SOFT;
    5: Result := ALC_STEREO_UHJ;
    6: Result := ALC_STEREO_HRTF;
    else Result := ALC_STEREO_BASIC;
  end;
end;

function TALSManager.ListOfMixMode: TStringArray;
begin
  Result := NIL;
  SetLength(Result, 7);
  Result[0] := 'SURROUND 5.1';
  Result[1] := 'SURROUND 6.1';
  Result[2] := 'SURROUND 7.1';
  Result[3] := 'ANY_STEREO';     // necessary to keep this ?
  Result[4] := 'BASIC STEREO';
  Result[5] := 'UHJ STEREO';
  Result[6] := 'HRTF STEREO';
end;

function TALSManager.MixModeIndexToEnum(aIndex: integer): TALSPlaybackContextOutputMode;
begin
  case aIndex of
    0: Result := ALC_SURROUND_5_1;
    1: Result := ALC_SURROUND_6_1;
    2: Result := ALC_SURROUND_7_1;
    3: Result := ALC_ANY_SOFT;
    4: Result := ALC_STEREO_BASIC;
    5: Result := ALC_STEREO_UHJ;
    6: Result := ALC_STEREO_HRTF;
    else Result := ALC_STEREO_BASIC;
  end;
end;

function TALSManager.CreateDefaultLoopbackContext: TALSLoopbackContext;
begin
  FDefaultLoopbackDevice.Open;
  Result := TALSLoopbackContext.Create(@FDefaultLoopbackDevice);
end;

function TALSManager.CreateDefaultPlaybackContext: TALSPlaybackContext;
var
  attribs: TALSContextAttributes;
begin
  attribs.InitDefault;
  Result := CreatePlaybackContext(-1, attribs);
end;

function TALSManager.CreatePlaybackContext(aNameIndex: integer;
  const aAttribs: TALSContextAttributes): TALSPlaybackContext;
begin
  if aNameIndex = -1 then
    aNameIndex := FDefaultPlaybackDeviceIndex;

  if Error or
     (Length(FPlaybackDevices) = 0) or
     (aNameIndex < 0) or
     (aNameIndex >= Length(ListOfPlaybackDeviceName)) then
    Result := TALSPlaybackContext.Create(NIL, aAttribs)
  else begin
    FPlaybackDevices[aNameIndex].Open;
    Result := TALSPlaybackContext.Create(@FPlaybackDevices[aNameIndex], aAttribs);
  end;
end;

function TALSManager.CreateDefaultCaptureContext: TALSCaptureContext;
begin
  Result := CreateCaptureContext(-1, 44100, ALS_CAPTUREFORMAT_STEREO16, 0.100);
end;

function TALSManager.CreateCaptureContext(aNameIndex: integer; aFrequency: longword;
  aFormat: TALSCaptureFormat; aBufferSize: double): TALSCaptureContext;
var
  A: TStringArray;
begin
  A := ListOfCaptureDeviceName;
  if (Length(A) = 0) or (aNameIndex < 0) or (aNameIndex > High(A)) then
    Result := TALSCaptureContext.Create('', aFrequency, aFormat, aBufferSize)
  else
    Result := TALSCaptureContext.Create(ListOfCaptureDeviceName[aNameIndex],
      aFrequency, aFormat, aBufferSize);
end;

{ TALSContextAttributes }

procedure TALSContextAttributes.InitDefault;
begin
  SampleRate := 44100;
  MaxAuxSend := 2;
  MonoCount := 128;
  StereoCount := 128;
  OutputMode := ALC_STEREO_BASIC;
  ContextUseFloat := False;
  HRTFIndex := -1;
  FLoopbackModeEnabled := False;
  EnableOutputLimiter := True;
end;

procedure TALSContextAttributes.InitFrom(aAttribs: TALSContextAttributes);
begin
  SampleRate := aAttribs.SampleRate;
  MaxAuxSend := aAttribs.MaxAuxSend;
  MonoCount := aAttribs.MonoCount;
  StereoCount := aAttribs.StereoCount;
  OutputMode := aAttribs.OutputMode;
  HRTFIndex := aAttribs.HRTFIndex;
  ContextUseFloat := aAttribs.ContextUseFloat;
  FLoopbackModeEnabled := aAttribs.FLoopbackModeEnabled;
  FLoopbackChannelType := aAttribs.FLoopbackChannelType;
  FLoopbackSampleType := aAttribs.FLoopbackSampleType;
end;

procedure TALSContextAttributes.SetLoopbackMode(aSampleRate: integer;
  aChannels: TALSLoopbackChannel; aSampleType: TALSLoopbackSampleType);
begin
  FLoopbackModeEnabled := True;
  SampleRate := ALint(aSampleRate);
  FLoopbackChannelType := aChannels;
  FLoopbackSampleType := aSampleType;
  ContextUseFloat := aSampleType = ALC_FLOAT_SOFT;
end;

function TALSContextAttributes.ToArray( aEFXPresent, aOutputModePresent,
  aHRTFPresent, aLoopbackPresent, aOutputLimiterPresent: boolean): ArrayOfALint;
var A: ArrayOfALint;
  i: integer;
  procedure AddAttrib( aAttribLabel, aAttribValue: ALint );
  begin
    i := Length(A);
    SetLength(A, i+2);
    A[i] := aAttribLabel;
    A[i+1] := aAttribValue;
  end;

begin
  A := nil;
  // Ask for mono sources (legacy)
  AddAttrib( ALC_MONO_SOURCES, ALint(MonoCount) );

  // Ask for stereo sources (legacy)
  AddAttrib( ALC_STEREO_SOURCES, ALint(StereoCount) );

  // Ask for output frequency (legacy)
  AddAttrib( ALC_FREQUENCY, ALint(SampleRate) );

  // Ask for requested auxiliary send per source (extension)
  if aEFXPresent then
    AddAttrib( ALC_MAX_AUXILIARY_SENDS, ALint(MaxAuxSend) );

  // Output mode (extension). Ignored if loopback mode is enabled.
  if aOutputModePresent and not FLoopbackModeEnabled then
    AddAttrib( ALC_OUTPUT_MODE_SOFT, ALint(Ord(OutputMode)) );

  // HRTF selection (extension)
  if (HRTFIndex<>-1) and
      aHRTFPresent then
  begin
    AddAttrib( ALC_HRTF_SOFT, ALC_TRUE );
    AddAttrib( ALC_HRTF_ID_SOFT, ALint(HRTFIndex) );
  end;

  // Loopback parameters (extension)
  if FLoopbackModeEnabled and
     aLoopbackPresent then
  begin
    AddAttrib( ALC_FORMAT_CHANNELS_SOFT, ALint(Ord(FLoopbackChannelType)) );
    AddAttrib( ALC_FORMAT_TYPE_SOFT, ALint(Ord(FLoopbackSampleType)) );
  end;

  // Output limiter ON/OFF (extension)
  if aOutputLimiterPresent then
  begin
    if EnableOutputLimiter then
      AddAttrib(ALC_OUTPUT_LIMITER_SOFT, ALC_TRUE)
    else
      AddAttrib(ALC_OUTPUT_LIMITER_SOFT, ALC_FALSE);
  end;

  // end of context attributes
  AddAttrib( 0, 0 );
  Result := A;
end;

{ TALSPlaylist }

function TALSPlaylist.GetCount: integer;
begin
  Result := FList.Count;
end;

function TALSPlaylist.GetCurrentFile: string;
begin
  Result := '';
  if FList.Count = 0 then
    exit;
  if (FMusicIndex >= 0) and (FMusicIndex < FList.Count) then
    Result := FList.Strings[FMusicIndex];
end;

function TALSPlaylist.GetIsPlaying: boolean;
begin
  if FMusic <> nil then
    Result := FMusic.State = ALS_PLAYING
  else
    Result := False;
end;

procedure TALSPlaylist.SetVolume(AValue: single);
begin
  if FVolume = AValue then
    Exit;
  FVolume := AValue;
  if FMusic <> nil then
    FMusic.Volume.Value := AValue;
end;

procedure TALSPlaylist.Update(const aElapsedTime: single);
var
  i: integer;
  snd: TALSSound;
begin
  // process kill list
  for i := FKillList.Count - 1 downto 0 do
  begin
    snd := TALSSound(FKillList.Items[i]);
    snd.Update(aElapsedTime);
    if snd.FKill then
    begin
      snd.Free;
      FKillList.Delete(i);
    end;
  end;

  if FMusic <> nil then
    FMusic.Update(aElapsedTime);

  case FState of
    ALS_PAUSED:
    begin
      if (FMusic.Volume.State = alspsNO_CHANGE) and (FMusic.State <> ALS_PAUSED)
      then
        FMusic.Pause;
    end;
    ALS_PLAYING:
    begin
      if FMusic = nil then
      begin
        FTimeAccu := FTimeAccu + aElapsedTime;
        if FTimeAccu >= 0.330 then
        begin
          FTimeAccu := 0.0;
          IncrementMusicIndex;
          LoadCurrentMusic;
          if FMusic <> nil then
          begin
            FMusic.Volume.Value := FVolume;
            FMusic.Play(True);
          end;
        end;
      end
      else if FMusic.State = ALS_STOPPED then
      begin
        IncrementMusicIndex;
        LoadCurrentMusic;
        if FMusic <> nil then
        begin
          FMusic.Volume.Value := FVolume;
          FMusic.Play(True);
        end;
      end;
    end;
  end;//case
end;

procedure TALSPlaylist.IncrementMusicIndex;
begin
  Inc(FMusicIndex);
  if FMusicIndex >= FList.Count then
    FMusicIndex := 0;
end;

procedure TALSPlaylist.FreeCurrentMusic;
begin
  if FMusic <> nil then
  begin
    FMusic.Free;
    FMusic := nil;
  end;
end;

procedure TALSPlaylist.LoadCurrentMusic;
begin
  FreeCurrentMusic;
  try
    FMusic := TALSStreamBufferSound.CreateFromFile(FParentContext, FList.Strings[FMusicIndex], False, NIL, NIL);
  except
    FMusic.Free;
    FMusic := nil;
  end;
end;

procedure TALSPlaylist.MoveCurrentMusicToKillList;
begin
  if FMusic <> nil then
  begin
    try
      EnterCriticalSection(FParentContext.FCriticalSection);
      FKillList.Add(FMusic);
      FMusic := nil;
    finally
      LeaveCriticalSection(FParentContext.FCriticalSection);
    end;
  end;
end;

constructor TALSPlaylist.Create(aParent: TALSPlaybackContext);
begin
  FParentContext := aParent;
  FKillList := TFPList.Create;
  FList := TStringList.Create;
  FMusicIndex := 0;
  FState := ALS_STOPPED;
  FVolume := 1.0;
end;

destructor TALSPlaylist.Destroy;
begin
  Clear;
  FList.Free;
  FKillList.Free;
  inherited Destroy;
end;

procedure TALSPlaylist.Clear;
begin
  EnterCriticalSection(FParentContext.FCriticalSection);
  try
    while FKillList.Count > 0 do
    begin
      TALSSound(FKillList.Items[0]).Free;
      FKillList.Delete(0);
    end;
    FreeCurrentMusic;
    FList.Clear;
    FState := ALS_STOPPED;
    FMusicIndex := 0;
  finally
    LeaveCriticalSection(FParentContext.FCriticalSection);
  end;
end;

procedure TALSPlaylist.Add(const aFilename: string);
begin
  EnterCriticalSection(FParentContext.FCriticalSection);
  try
    FList.Add(aFilename);
  finally
    LeaveCriticalSection(FParentContext.FCriticalSection);
  end;
end;

procedure TALSPlaylist.Delete(Index: integer);
begin
  EnterCriticalSection(FParentContext.FCriticalSection);
  try
    FList.Delete(Index);
  finally
    LeaveCriticalSection(FParentContext.FCriticalSection);
  end;
end;

procedure TALSPlaylist.Rewind(aFadeTime: single);
begin
  if FState = ALS_PLAYING then
  begin
    Stop(aFadeTime);
    FMusicIndex := 0;
    Play(aFadeTime);
  end
  else
  begin
    FMusicIndex := 0;
  end;
end;

procedure TALSPlaylist.Next(aFadeTime: single);
begin
  if FList.Count = 0 then
    exit;

  if State = ALS_STOPPED then
  begin
    Play(aFadeTime);
    exit;
  end;

  if FMusic <> nil then
    FMusic.FadeOutThenKill(aFadeTime, ALS_StartFastEndSlow);
  MoveCurrentMusicToKillList;

  IncrementMusicIndex;
  LoadCurrentMusic;
  if FMusic <> nil then
    FMusic.FadeIn(FVolume, aFadeTime, ALS_StartSlowEndFast);
  FState := ALS_PLAYING;
end;

procedure TALSPlaylist.Previous(aFadeTime: single);
begin
  if FList.Count = 0 then
    exit;

  if State = ALS_STOPPED then
  begin
    Play(aFadeTime);
    exit;
  end;

  if FMusic <> nil then
    FMusic.FadeOutThenKill(aFadeTime, ALS_StartFastEndSlow);
  MoveCurrentMusicToKillList;

  Dec(FMusicIndex);
  if FMusicIndex < 0 then
    FMusicIndex := FList.Count - 1;
  LoadCurrentMusic;
  if FMusic <> nil then
    FMusic.FadeIn(FVolume, aFadeTime, ALS_StartSlowEndFast);
  FState := ALS_PLAYING;
end;

procedure TALSPlaylist.Play(aFadeInTime: single);
begin
  if FList.Count = 0 then
    exit;

  case FState of
    ALS_PAUSED:
    begin
      FMusic.FadeIn(FVolume, aFadeInTime, ALS_StartSlowEndFast);
      FState := ALS_PLAYING;
    end;
    ALS_STOPPED:
    begin
      if (FMusicIndex >= 0) and (FMusicIndex < FList.Count) then
      begin
        LoadCurrentMusic;
        if FMusic <> nil then
          FMusic.FadeIn(FVolume, aFadeInTime, ALS_StartSlowEndFast);
        FState := ALS_PLAYING;
      end;
    end;
  end;
end;

procedure TALSPlaylist.Pause(aFadeoutTime: single);
begin
  if FList.Count = 0 then
    exit;
  if FState <> ALS_PLAYING then
    exit;

  FMusic.Volume.ChangeTo(0, aFadeoutTime, ALS_StartFastEndSlow);
  FFadeTime := aFadeoutTime;
  FState := ALS_PAUSED;
end;

procedure TALSPlaylist.Stop(aFadeoutTime: single);
begin
  if FMusic <> nil then
  begin
    FMusic.FadeOutThenKill(aFadeoutTime, ALS_StartFastEndSlow);
    MoveCurrentMusicToKillList;
  end;
  FState := ALS_STOPPED;
  FMusicIndex := 0;
end;


{ TALSEffect }

procedure TALSEffect.DealocateALObjects;
begin
  if not Ready then Exit;

  if FEffectAssigned then
    alDeleteEffects(1, @FEffectID);

  if FSlotAssigned then
    alDeleteAuxiliaryEffectSlots(1, @FSlotID);

  FEffectAssigned := False;
  FSlotAssigned := False;
  FReady := False;
end;

procedure TALSEffect.InitDefault(aParentContext: TALSPlaybackContext);
begin
  FParentContext := aParentContext;
  FEffectAssigned := False;
  FSlotAssigned := False;
  FReady := False;
  FOutputGain := 1.0;
  FMuteCoef := 1.0;
  FMute := False;
  FPrevious := NIL;
  FNext := NIL;
end;

procedure TALSEffect.InternalSetOutputGain;
begin
  alAuxiliaryEffectSlotf(FSlotID, AL_EFFECTSLOT_GAIN, FOutputGain*FMuteCoef);
  alGetError(); // reset error
end;

procedure TALSEffect.DoUpdateParam(const P);
var
  eaxrev: TEAXReverbProperties;
  ReverbParam: TALSReverbProperties;
  ChorusParam: TALSChorusProperties;
  FlangerParam: TALSFlangerProperties;
  DistortionParam: TALSDistortionProperties;
  EchoParam: TALSEchoProperties;
  FShifterParam: TALSFreqShifterProperties;
  VMorpherParam: TALSVocalmorpherProperties;
  PShifterParam: TALSPitchShifterProperties;
  ModulatorParam: TALSRingModulatorProperties;
  CompressorParam: TALSCompressorProperties;
  EQParam: TALSEqualizerProperties;
  AutoWahParam: TALSAutoWahProperties;
begin
  if not FSlotAssigned or
     not FEffectAssigned then
    exit;

  // initialize effect parameters
  case FEffectType of
    AL_EFFECT_EAXREVERB:
    begin
      eaxrev := TEAXReverbProperties(P);
      alEffectfv(FEffectID, AL_EAXREVERB_DENSITY, @eaxrev.flDensity);
      alEffectfv(FEffectID, AL_EAXREVERB_DIFFUSION, @eaxrev.flDiffusion);
      alEffectfv(FEffectID, AL_EAXREVERB_GAIN, @eaxrev.flGain);
      alEffectfv(FEffectID, AL_EAXREVERB_GAINHF, @eaxrev.flGainHF);
      alEffectfv(FEffectID, AL_EAXREVERB_GAINLF, @eaxrev.flGainLF);
      alEffectfv(FEffectID, AL_EAXREVERB_DECAY_TIME, @eaxrev.flDecayTime);
      alEffectfv(FEffectID, AL_EAXREVERB_DECAY_HFRATIO, @eaxrev.flDecayHFRatio);
      alEffectfv(FEffectID, AL_EAXREVERB_DECAY_LFRATIO, @eaxrev.flDecayLFRatio);
      alEffectfv(FEffectID, AL_EAXREVERB_REFLECTIONS_GAIN, @eaxrev.flReflectionsGain);
      alEffectfv(FEffectID, AL_EAXREVERB_REFLECTIONS_DELAY, @eaxrev.flReflectionsDelay);
      alEffectfv(FEffectID, AL_EAXREVERB_REFLECTIONS_PAN, @eaxrev.flReflectionsPan);
      alEffectfv(FEffectID, AL_EAXREVERB_LATE_REVERB_GAIN, @eaxrev.flLateReverbGain);
      alEffectfv(FEffectID, AL_EAXREVERB_LATE_REVERB_DELAY, @eaxrev.flLateReverbDelay);
      alEffectfv(FEffectID, AL_EAXREVERB_LATE_REVERB_PAN, @eaxrev.flLateReverbPan);
      alEffectfv(FEffectID, AL_EAXREVERB_ECHO_TIME, @eaxrev.flEchoTime);
      alEffectfv(FEffectID, AL_EAXREVERB_ECHO_DEPTH, @eaxrev.flEchoDepth);
      alEffectfv(FEffectID, AL_EAXREVERB_MODULATION_TIME, @eaxrev.flModulationTime);
      alEffectfv(FEffectID, AL_EAXREVERB_MODULATION_DEPTH, @eaxrev.flModulationDepth);
      alEffectfv(FEffectID, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, @eaxrev.flAirAbsorptionGainHF);
      alEffectfv(FEffectID, AL_EAXREVERB_HFREFERENCE, @eaxrev.flHFReference);
      alEffectfv(FEffectID, AL_EAXREVERB_LFREFERENCE, @eaxrev.flLFReference);
      alEffectfv(FEffectID, AL_EAXREVERB_ROOM_ROLLOFF_FACTOR, @eaxrev.flRoomRolloffFactor);
      alEffecti(FEffectID, AL_EAXREVERB_DECAY_HFLIMIT, eaxrev.iDecayHFLimit);
    end;

    AL_EFFECT_REVERB:
    begin
      ReverbParam := TALSReverbProperties(P);
      alEffectfv(FEffectID, AL_REVERB_DENSITY, @ReverbParam.Density);
      alEffectfv(FEffectID, AL_REVERB_DIFFUSION, @ReverbParam.Diffusion);
      alEffectfv(FEffectID, AL_REVERB_GAIN, @ReverbParam.Gain);
      alEffectfv(FEffectID, AL_REVERB_GAINHF, @ReverbParam.GainHF);
      alEffectfv(FEffectID, AL_REVERB_DECAY_TIME, @ReverbParam.DecayTime);
      alEffectfv(FEffectID, AL_REVERB_DECAY_HFRATIO, @ReverbParam.DecayHFRatio);
      alEffectfv(FEffectID, AL_REVERB_REFLECTIONS_GAIN, @ReverbParam.ReflectionsGain);
      alEffectfv(FEffectID, AL_REVERB_REFLECTIONS_DELAY, @ReverbParam.ReflectionsDelay);
      alEffectfv(FEffectID, AL_REVERB_LATE_REVERB_GAIN, @ReverbParam.LateReverbGain);
      alEffectfv(FEffectID, AL_REVERB_LATE_REVERB_DELAY, @ReverbParam.LateReverbDelay);
      alEffectfv(FEffectID, AL_REVERB_AIR_ABSORPTION_GAINHF, @ReverbParam.AirAbsorptionGainHF);
      alEffectfv(FEffectID, AL_REVERB_ROOM_ROLLOFF_FACTOR, @ReverbParam.RoomRolloffFactor);
      alEffecti(FEffectID, AL_REVERB_DECAY_HFLIMIT, ReverbParam.DecayHFLimit);
    end;

    AL_EFFECT_CHORUS:
    begin
      ChorusParam := TALSChorusProperties(P);
      alEffecti(FEffectID, AL_CHORUS_WAVEFORM, ALint(ChorusParam.Waveform));
      alEffecti(FEffectID, AL_CHORUS_PHASE, ChorusParam.Phase);
      alEffectfv(FEffectID, AL_CHORUS_RATE, @ChorusParam.Rate);
      alEffectfv(FEffectID, AL_CHORUS_DEPTH, @ChorusParam.Depth);
      alEffectfv(FEffectID, AL_CHORUS_FEEDBACK, @ChorusParam.Feedback);
      alEffectfv(FEffectID, AL_CHORUS_DELAY, @ChorusParam.Delay);
    end;

    AL_EFFECT_FLANGER:
    begin
      FlangerParam := TALSFlangerProperties(P);
      alEffecti(FEffectID, AL_FLANGER_WAVEFORM, ALint(FlangerParam.Waveform));
      alEffecti(FEffectID, AL_FLANGER_PHASE, FlangerParam.Phase);
      alEffectfv(FEffectID, AL_FLANGER_RATE, @FlangerParam.Rate);
      alEffectfv(FEffectID, AL_FLANGER_DEPTH, @FlangerParam.Depth);
      alEffectfv(FEffectID, AL_FLANGER_FEEDBACK, @FlangerParam.Feedback);
      alEffectfv(FEffectID, AL_FLANGER_DELAY, @FlangerParam.Delay);
    end;

    AL_EFFECT_DISTORTION:
    begin
      DistortionParam := TALSDistortionProperties(P);
      alEffectfv(FEffectID, AL_DISTORTION_EDGE, @DistortionParam.Edge);
      alEffectfv(FEffectID, AL_DISTORTION_GAIN, @DistortionParam.Gain);
      alEffectfv(FEffectID, AL_DISTORTION_LOWPASS_CUTOFF, @DistortionParam.LowpassCutoff);
      alEffectfv(FEffectID, AL_DISTORTION_EQCENTER, @DistortionParam.EQCenter);
      alEffectfv(FEffectID, AL_DISTORTION_EQBANDWIDTH, @DistortionParam.EQBandwidth);
    end;

    AL_EFFECT_ECHO:
    begin
      EchoParam := TALSEchoProperties(P);
      alEffectfv(FEffectID, AL_ECHO_DELAY, @EchoParam.Delay);
      alEffectfv(FEffectID, AL_ECHO_LRDELAY, @EchoParam.LRDelay);
      alEffectfv(FEffectID, AL_ECHO_DAMPING, @EchoParam.Damping);
      alEffectfv(FEffectID, AL_ECHO_FEEDBACK, @EchoParam.Feedback);
      alEffectfv(FEffectID, AL_ECHO_SPREAD, @EchoParam.Spread);
    end;

    AL_EFFECT_FREQUENCYSHIFTER:
    begin
      FShifterParam := TALSFreqShifterProperties(P);
      alEffectfv(FEffectID, AL_FREQUENCY_SHIFTER_FREQUENCY, @FShifterParam.Frequency);
      alEffecti(FEffectID, AL_FREQUENCY_SHIFTER_LEFT_DIRECTION, ALint(FShifterParam.LeftDirection));
      alEffecti(FEffectID, AL_FREQUENCY_SHIFTER_RIGHT_DIRECTION, ALint(FShifterParam.RightDirection));
    end;

    AL_EFFECT_VOCALMORPHER:
    begin
      VMorpherParam := TALSVocalmorpherProperties(P);
      alEffectfv(FEffectID, AL_VOCAL_MORPHER_RATE, @VMorpherParam.Rate);
      alEffecti(FEffectID, AL_VOCAL_MORPHER_PHONEMEA, ALint(VMorpherParam.PhonemeA));
      alEffecti(FEffectID, AL_VOCAL_MORPHER_PHONEMEB, ALint(VMorpherParam.PhonemeB));
      alEffecti(FEffectID, AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING, VMorpherParam.PhonemeACoarseTuning);
      alEffecti(FEffectID, AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING, VMorpherParam.PhonemeBCoarseTuning);
      alEffecti(FEffectID, AL_VOCAL_MORPHER_WAVEFORM, ALint(VMorpherParam.Waveform));
    end;

    AL_EFFECT_PITCHSHIFTER:
    begin
      PShifterParam := TALSPitchShifterProperties(P);
      alEffecti(FEffectID, AL_PITCH_SHIFTER_COARSE_TUNE, PShifterParam.CoarseTune);
      alEffecti(FEffectID, AL_PITCH_SHIFTER_FINE_TUNE, PShifterParam.FineTune);
    end;

    AL_EFFECT_RINGMODULATOR:
    begin
      ModulatorParam := TALSRingModulatorProperties(P);
      alEffectfv(FEffectID, AL_RING_MODULATOR_FREQUENCY, @ModulatorParam.Frequency);
      alEffectfv(FEffectID, AL_RING_MODULATOR_HIGHPASS_CUTOFF, @ModulatorParam.HighPassCutoff);
      alEffecti(FEffectID, AL_RING_MODULATOR_WAVEFORM, ALint(ModulatorParam.Waveform));
    end;

    AL_EFFECT_COMPRESSOR:
    begin
      CompressorParam := TALSCompressorProperties(P);
      alEffecti(FEffectID, AL_COMPRESSOR_ONOFF, CompressorParam.OnOff);
    end;

    AL_EFFECT_EQUALIZER:
    begin
      EQParam := TALSEqualizerProperties(P);
      alEffectfv(FEffectID, AL_EQUALIZER_LOW_CUTOFF, @EQParam.LowCutoff);
      alEffectfv(FEffectID, AL_EQUALIZER_LOW_GAIN, @EQParam.LowGain);
      alEffectfv(FEffectID, AL_EQUALIZER_MID1_CENTER, @EQParam.Mid1Center);
      alEffectfv(FEffectID, AL_EQUALIZER_MID1_GAIN, @EQParam.Mid1Gain);
      alEffectfv(FEffectID, AL_EQUALIZER_MID1_WIDTH, @EQParam.Mid1Width);
      alEffectfv(FEffectID, AL_EQUALIZER_MID2_CENTER, @EQParam.Mid2Center);
      alEffectfv(FEffectID, AL_EQUALIZER_MID2_GAIN, @EQParam.Mid2Gain);
      alEffectfv(FEffectID, AL_EQUALIZER_MID2_WIDTH, @EQParam.Mid2Width);
      alEffectfv(FEffectID, AL_EQUALIZER_HIGH_CUTOFF, @EQParam.HighCutoff);
      alEffectfv(FEffectID, AL_EQUALIZER_HIGH_GAIN, @EQParam.HighGain);
    end;

    AL_EFFECT_AUTOWAH:
    begin
      AutoWahParam := TALSAutoWahProperties(P);
      alEffectfv(FEffectID, AL_AUTOWAH_ATTACK_TIME, @AutoWahParam.AttackTime);
      alEffectfv(FEffectID, AL_AUTOWAH_RELEASE_TIME, @AutoWahParam.ReleaseTime);
      alEffectfv(FEffectID, AL_AUTOWAH_RESONANCE, @AutoWahParam.Resonance);
      alEffectfv(FEffectID, AL_AUTOWAH_PEAK_GAIN, @AutoWahParam.PeakGain);
    end;
  end;//case

{ Changing a parameter value in the Effect Object after it has been attached to the Auxiliary Effect
  Slot will not affect the effect in the effect slot. To update the parameters of the effect in the effect
  slot, an application must update the parameters of an Effect object and then re-attach it to the
  Auxiliary Effect Slot. }
  alAuxiliaryEffectSloti( FSlotID, AL_EFFECTSLOT_EFFECT, FEffectID);

  // we don't care about wrong parameters error
  alGetError(); // reset error
end;

function TALSEffect.IsInChain: boolean;
begin
  Result := (FPrevious <> NIL) or (FNext <> NIL);
end;

function TALSEffect.AllOtherChainedEffectAreMuted: boolean;
var
  p: PALSEffect;
begin
  Result := True;

  p := FPrevious;
  while p <> NIL do
  begin
    Result := Result and p^.FMute;
    p := p^.FPrevious;
  end;

  p := FNext;
  while p <> NIL do
  begin
    Result := Result and p^.FMute;
    p := p^.FNext;
  end;
end;

procedure TALSEffect.ForceAllMuteCoeffTo1;
var
  p: PALSEffect;
begin
  p := @Self;
  while p <> NIL do
  begin
    p^.FMuteCoef := 1.0;
    p^.InternalSetOutputGain;
    p := p^.FPrevious;
  end;

  p := FNext;
  while p <> NIL do
  begin
    p^.FMuteCoef := 1.0;
    p^.InternalSetOutputGain;
    p := p^.FNext;
  end;
end;

function TALSEffect.FirstEffectInChain: PALSEffect;
begin
  Result := @Self;
  while Result^.FPrevious <> NIL do
    Result := Result^.FPrevious;
end;

function TALSEffect.GetPreviousActiveEffectSlotID(aEffect: PALSEffect; out aSlotID: ALuint): boolean;
var
  p: PALSEffect;
begin
  p := GetPreviousActiveEffect(aEffect);
  if p <> NIL then
  begin
    aSlotID := p^.FSlotID;
    Result := True;
  end
  else
    Result := False;
end;

function TALSEffect.GetNextActiveEffectSlotID(aEffect: PALSEffect): ALuint;
var
  p: PALSEffect;
begin
  p := GetNextActiveEffect(aEffect);
  if p <> NIL then
    Result := p^.FSlotID
  else
    Result := AL_EFFECTSLOT_NULL;
end;

function TALSEffect.GetPreviousActiveEffect(aEffect: PALSEffect): PALSEffect;
begin
  Result := aEffect;
  while Result <> NIL do
  begin
    if Result^.Ready and not Result^.Mute then
      exit
    else
      Result := Result^.FPrevious;
  end;
end;

function TALSEffect.GetNextActiveEffect(aEffect: PALSEffect): PALSEffect;
begin
  Result := aEffect;
  while Result <> NIL do
  begin
    if Result^.Ready and not Result^.Mute then
      exit
    else
      Result := Result^.FNext;
  end;
end;

procedure TALSEffect.SetMute(AValue: boolean);
var
  previousSlotID, targetSlotID: ALuint;
  i, j: integer;
begin
  if FMute=AValue then Exit;
  FMute:=AValue;

  if (FParentContext = NIL) or
     not FReady then
    exit;
  if FParentContext.Error or
     not FParentContext.FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX then
    exit;

  LockContext( FParentContext.FContext );
  try
    if not IsInChain then
    begin
      // The effect is single one -> we multiply the output gain by a coeff. 0 or 1.
      if AValue then
        FMuteCoef := 0.0
      else
        FMuteCoef := 1.0;
      InternalSetOutputGain;
    end
    else
    begin // The effect is chained with others.

      targetSlotID := GetNextActiveEffectSlotID(FNext);
      if AValue then
      begin // Muting = connect the previous effect slot to the next effect slot
            // If there isn't a previous effect, we have to disconnect all sound's
            // auxiliary send that use the current slot and connect them to the
            // next effect slot, and connect current to AL_EFFECTSLOT_NULL

        if AllOtherChainedEffectAreMuted then
        begin  // we mute the last unmuted effect of the chain
               //   -> we sets its FMuteCoef to 0
          FMuteCoef := 0.0;
          InternalSetOutputGain;
        end
        else

        if GetPreviousActiveEffectSlotID(FPrevious, previousSlotID) then
        begin // we mute one effect at the middle of the chain.
              //  -> connects the previous effect slotID to the next effect slotID
          alAuxiliaryEffectSloti(previousSlotID, AL_EFFECTSLOT_TARGET_SOFT, targetSlotID);
          alAuxiliaryEffectSloti(FSlotID, AL_EFFECTSLOT_TARGET_SOFT, AL_EFFECTSLOT_NULL);
          alGetError();
        end
        else
        begin // we mute the first effect of the chain
          for i:=0 to FParentContext.GetSoundCount-1 do // scans all sound of the context
            with FParentContext.GetSoundByIndex(i) do
            begin
              EnterCS;
              try
                for j:=0 to High(FAuxiliarySend) do  // connects the AuxSend to the new effect slot
                  if FAuxiliarySend[j].IsConnectedWithChain(FirstEffectInChain) then
                    FAuxiliarySend[j].ConnectTo(targetSlotID);
              finally
                LeaveCS;
              end;
            end;
          alAuxiliaryEffectSloti(FSlotID, AL_EFFECTSLOT_TARGET_SOFT, AL_EFFECTSLOT_NULL);
          alGetError();
        end;
      end
      else
      begin // Unmuting = connect the previous effect slot to the current one
            // and connect the current one to the next one.
            // if there isn't a previous effect, we have to disconnect all sound's
            // auxiliary send that use the next slot and connect them to the
            // current one, and connect current to the next.

        if FMuteCoef = 0.0 then
        begin
          FMuteCoef := 1.0;
          InternalSetOutputGain;
        end
        else
        begin
          ForceAllMuteCoeffTo1;

          if GetPreviousActiveEffectSlotID(FPrevious, previousSlotID) then
          begin  // link previous to current and current to next.
            alAuxiliaryEffectSloti(previousSlotID, AL_EFFECTSLOT_TARGET_SOFT, FSlotID);
            alAuxiliaryEffectSloti(FSlotID, AL_EFFECTSLOT_TARGET_SOFT, targetSlotID);
            alGetError();
          end
          else
          begin // re-connects the first effect of the chain to the auxiliary sends.
            for i:=0 to FParentContext.GetSoundCount-1 do
              with FParentContext.GetSoundByIndex(i) do
              begin
                EnterCS;
                try
                  for j:=0 to High(FAuxiliarySend) do
                    if FAuxiliarySend[j].IsConnectedWithChain(FirstEffectInChain) then
                      FAuxiliarySend[j].ConnectTo(Self.FSlotID);
                finally
                  LeaveCS;
                end;
              end;
            alAuxiliaryEffectSloti(FSlotID, AL_EFFECTSLOT_TARGET_SOFT, targetSlotID);
            alGetError();
          end;
        end;
      end;
    end;
  finally
    UnlockContext;
  end;
end;

procedure TALSEffect.UpdateParameters(const P);
begin
  if not Ready then
    exit;

  LockContext( FParentContext.FContext );
  try
    DoUpdateParam( P );
  finally
    UnlockContext;
  end;
end;

function TALSEffect.ChainWith(var aTargetEffect: TALSEffect): boolean;
var
  outputSlotID, targetSlotID: ALuint;
begin
  // Link the effect
  FNext := @aTargetEffect;
  aTargetEffect.FPrevious := @Self;

  Result := False;
  if FParentContext = NIL then
    exit;
  if not FParentContext.FHaveExt_AL_SOFT_effect_target or
     FParentContext.Error then
    exit;

  LockContext( FParentContext.FContext );
  try
    // connects the first previous valid slot ID in the chain with the target slot.
    if GetPreviousActiveEffectSlotID(@Self, outputSlotID) then
    begin
      // search the first next valid slot ID to connect to. May be AL_EFFECTSLOT_NULL if none.
      targetSlotID := GetNextActiveEffectSlotID(@aTargetEffect);
      alAuxiliaryEffectSloti(outputSlotID, AL_EFFECTSLOT_TARGET_SOFT, targetSlotID);
      Result := alGetError() = AL_NO_ERROR;
    end
    else
      Result := False;
  finally
    UnlockContext;
  end;
end;

procedure TALSEffect.SetOutputGain(AValue: single);
begin
  if not Ready then exit;
  FOutputGain := AValue;
  LockContext( FParentContext.FContext );
  try
    InternalSetOutputGain;
  finally
    UnlockContext;
  end;
end;

procedure TALSEffect.SetApplyDistanceAttenuation(AValue: boolean);
begin
  if FApplyDistanceAttenuation=AValue then Exit;
  FApplyDistanceAttenuation:=AValue;

  if FParentContext.Error or
     not FParentContext.FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX or
     not Ready then
    Exit;

  LockContext( FParentContext.FContext );
  try
    case AValue of
      True: alAuxiliaryEffectSloti(FSlotID, AL_EFFECTSLOT_AUXILIARY_SEND_AUTO, AL_TRUE);
      False: alAuxiliaryEffectSloti(FSlotID, AL_EFFECTSLOT_AUXILIARY_SEND_AUTO, AL_FALSE);
    end;
    alGetError(); // reset error
  finally
    UnlockContext;
  end;
end;

procedure TALSEffect.InitEffect(aEffectType: TALSEffectType; const aParameters);
begin
  FEffectType := aEffectType;

  // Create the effect object
  if not FEffectAssigned then
  begin
    alGenEffects(1, @FEffectID);
    if alGetError() <> AL_NO_ERROR then
      exit;
    FEffectAssigned := True;
  end;

  // set the type of effect
  alEffecti(FEffectID, AL_EFFECT_TYPE, ALint(aEffectType));
  if alGetError() <> AL_NO_ERROR then
  begin
    DealocateALObjects;
    exit;
  end;

  // Create an effect slot object
  if not FSlotAssigned then
  begin
    alGenAuxiliaryEffectSlots(1, @FSlotID);
    if alGetError() <> AL_NO_ERROR then
    begin
      DealocateALObjects;
      exit;
    end;
    FSlotAssigned := True;
  end;

  // set effect parameters and attach the effect to the slot
  DoUpdateParam(aParameters);

  FReady := True;
end;

{ TALSPlaybackCapturedSound }

procedure TALSPlaybackCapturedSound.QueueBuffer(aBuffer: PALSCaptureFrameBuffer);
var
  processed, stat: ALint;
   i, Index: integer;
begin
  if Error or
     (aBuffer^.FrameCount = 0) then
    exit;

  LockContext(FParentContext.FContext);
  EnterCS;
  try
    processed := 0;
    // Get the number of processed buffer
    alGetSourceiv(FSource, AL_BUFFERS_PROCESSED, @processed);
    // and unqueue them from source
    if processed > 0 then
    begin
      alSourceUnqueueBuffers(FSource, processed, @FTempBufID[0]);
      alGetError();
      // marks buffer as unqueued
      while processed > 0 do
      begin
        dec(processed);
        for i:=0 to High(FBuffers) do
          if FBuffers[i].BufferID = FTempBufID[processed] then
          begin
            FBuffers[i].FQueued := False;
            break;
          end;
      end;
    end;

    // search the index of the first unqueued buffer -> only its ID will be used
    Index := -1;
    for i:=0 to High(FBuffers) do
      if not FBuffers[i].Queued then
      begin
        Index := i;
        break;
      end;

    if Index <> -1 then
    begin
      // refill the openAL buffer with passed audio data
      alBufferData(FBuffers[Index].BufferID,  // unqueued buffer ID
                   ALenum(FFormatForAL),
                   aBuffer^.Data,
                   ALsizei(aBuffer^.FrameCount*aBuffer^.BytePerFrame),
                   ALsizei(FSampleRate));
      alGetError(); //CheckALError(als_ErrorWhileBufferingData);

      // and queue it back on the source
      alSourceQueueBuffers(FSource, 1, @FBuffers[Index].BufferID);
      if alGetError() = AL_NO_ERROR then //CheckALError(als_ErrorWhileQueuingBuffer);
      begin
        FBuffers[Index].Queued := True;
        stat := 0;
        alGetSourcei(FSource, AL_SOURCE_STATE, stat);
        case stat of
          AL_INITIAL, AL_STOPPED, AL_PAUSED:
          begin
            alSourcePlay(FSource);
            alGetError();
          end;
        end;
      end
      else alSourceStop(FSource); // an error occurs while queuing a buffer:
                                  // -> we reset the playback until an another buffer is queued
    end;
  finally
    UnlockContext;
    LeaveCS;
  end;
end;

procedure TALSPlaybackCapturedSound.SetFileName(const aName: string);
begin
  FFilename := aName;
end;

procedure TALSPlaybackCapturedSound.SetTimePosition(AValue: single);
begin
  // Do nothing here
  AValue := AValue; // avoid hint
end;

constructor TALSPlaybackCapturedSound.CreateFromCapture(aParent: TALSPlaybackContext;
  aSampleRate: integer; aBuffer: PALSCaptureFrameBuffer);
begin
  FParentContext := aParent;
  InitializeErrorStatus;
  FChannelCount := aBuffer^.ChannelCount;
  FMonitoringEnabled := False;
  FSampleRate := aSampleRate;

  if not Error then
  begin
    FFormatForAL := GetFormatForAL(aBuffer^.ChannelCount, aBuffer^.UseFloat, False);
    if not Error then
    begin
      LockContext( FParentContext.FContext );
      try
        if not Error then
        begin
          // generates the buffers and source
          GenerateALBuffers(NUM_BUFFERS);
          GenerateALSource;
        end;
      finally
        UnlockContext;
      end;
    end;
  end;

  CreateParameters;
end;

destructor TALSPlaybackCapturedSound.Destroy;
begin
  if not FParentContext.Error then
    LockContext( FParentContext.FContext );
  try
    if not Error then
    begin
      alSourceStop( FSource );
      RemoveAllALEffects;
      FDirectFilter.DeleteFilter;
      alSourcei(FSource, AL_BUFFER, 0);
      FreeBuffers;
      alDeleteSources(1, @FSource);
    end;
    FreeParameters;
  finally
    if not FParentContext.Error then
      UnlockContext;
  end;
  inherited Destroy;
end;


{ TALSStreamBufferSound }

function TALSStreamBufferSound.GetChannelLevel(index: integer): single;
begin
  if Error or
     not FMonitoringEnabled or
     (State <> ALS_PLAYING) then
    Result := 0
  else
  begin
    LockContext( FParentContext.FContext );
    EnterCS;
    try
      Result := FBuffers[FPlayedBufferIndex].ChannelsLevel[index] * Volume.Value * FMuteMultiplicator;
    finally
      LeaveCS;
      UnlockContext;
    end;
  end;
end;

function TALSStreamBufferSound.DoReadStreamFromFile(aDest: Pointer; aFrameCount: longword): int64;
begin
  if FParentContext.FUseBufferOfFloat then
    Result := sf_readf_float(Fsndfile, aDest, aFrameCount)
  else
    Result := sf_readf_short(Fsndfile, aDest, aFrameCount);
end;


procedure TALSStreamBufferSound.PreBuffAudio;
var
  i: integer;
  readCount, todo: sf_count_t;
begin
  alGetError();

  FUsedBuffer := 0;
  FPlayedBufferIndex := 0;
  for i := 0 to NUM_BUFFERS - 1 do
  begin
    FBuffers[i].FrameCount := 0;
    todo := FBuffers[i].FrameCapacity;
    repeat
      readCount := FDoReadFromStream(FBuffers[i].DataOffset[FBuffers[i].FrameCount], todo);

      if readCount > 0 then
      begin
        todo := todo - readCount;
        FBuffers[i].FrameCount := FBuffers[i].FrameCount + readCount;
        if FLoop and (todo <> 0) then
          sf_seek(Fsndfile, 0, SF_SEEK_SET);
      end;
    until (readCount < 1) or not FLoop or (todo = 0);
    if readCount < 1 then
      break;

    if FOnCustomDSP <> NIL then
      FOnCustomDSP(Self, FBuffers[i], FOnCustomDSPUserData);

    // refill AL buffer with audio
    alBufferData(FBuffers[i].BufferID, FFormatForAL, FBuffers[i].Data,
      FBuffers[i].FrameCount*FBuffers[i].BytePerFrame, ALsizei(Fsfinfo.SampleRate));

    // retrieve the channels level
    if FMonitoringEnabled then
      FBuffers[i].ComputeChannelsLevel;

    Inc(FUsedBuffer);
  end;

  CheckALError(als_ErrorWhileBufferingData);

  if not Error then
  begin
    // Now queue the used buffers
    for i:=0 to FUsedBuffer - 1 do
    begin
      alSourceQueueBuffers(FSource, 1, @FBuffers[i].BufferID);
      CheckALError(als_ErrorWhileQueuingBuffer);
      FBuffers[i].Queued := True;
    end;
  end;
end;

procedure TALSStreamBufferSound.Update(const aElapsedTime: single);
var
  processed: ALint;
  bufid: ALuint;
  readCount: sf_count_t;
  bufferIndex: integer;
  res: ALenum;
begin
  inherited Update(aElapsedTime);

  if Error then
    exit;

  EnterCS;
  try
    // Get the number of processed buffer
    alGetSourceiv(FSource, AL_BUFFERS_PROCESSED, @processed);
    if processed < 1 then
      exit;

    FFrameReadAccu := FFrameReadAccu + int64(processed * FBufferFrameCount);

    // Unqueue and fill each processed buffer
    while (processed > 0) do
    begin
      alSourceUnqueueBuffers(FSource, 1, @bufid);
      Dec(processed);
      res := alGetError();
      if res <> AL_NO_ERROR then continue;

      // increment the index of the played buffer. we use this index to retrieve
      // the channel's level.
      inc(FPlayedBufferIndex);
      if FPlayedBufferIndex >= FUsedBuffer then
        FPlayedBufferIndex := 0;

      // retrieves the index of the buffer to refill with audio
      bufferIndex := 0;
      while FBuffers[bufferIndex].BufferID <> bufid do
       inc(bufferIndex);

      // Read data from opened file
      readCount := FDoReadFromStream(FBuffers[bufferIndex].Data,
                                     FBuffers[bufferIndex].FrameCapacity);
      FBuffers[bufferIndex].FrameCount := readCount;

      if readCount > 0 then
      begin
        // callback custom DSP
        if FOnCustomDSP <> NIL then
          FOnCustomDSP(Self, FBuffers[bufferIndex], FOnCustomDSPUserData);
        // refill the openAL buffer with...
        alBufferData(bufid, ALenum(FFormatForAL), FBuffers[bufferIndex].Data,
          ALsizei(readCount * FFrameSize), ALsizei(Fsfinfo.SampleRate));
        // and queue it back on the source
        alSourceQueueBuffers(FSource, 1, @bufid);
        // Set the opened file read cursor to the beginning if LOOP mode is enabled,
        // and the buffer was not completely filled
        if FLoop and (readCount < FBuffers[bufferIndex].FrameCapacity) then
        begin
          sf_seek(Fsndfile, 0, SF_SEEK_SET);
          FFrameReadAccu := 0;
        end;

        // retrieve the channels level
        if FMonitoringEnabled then
          FBuffers[bufferIndex].ComputeChannelsLevel;
      end;
    end;
  finally
    LeaveCS;
  end;
end;

procedure TALSStreamBufferSound.InternalRewind;
begin
  if Error then
    exit;

  alSourceRewind(FSource);
  alSourcei(FSource, AL_BUFFER, 0);
  sf_seek(Fsndfile, 0, SF_SEEK_SET);

  FFrameReadAccu := 0;
  PreBuffAudio;
end;

constructor TALSStreamBufferSound.CreateFromFile(aParent: TALSPlaybackContext;
  const aFilename: string; aEnableMonitor: boolean;
  aOnCustomDSP: TALSOnCustomDSP; aCustomDSPUserData: Pointer);
var
  fileopened: boolean;
begin
  FParentContext := aParent;
  InitializeErrorStatus;
  FChannelCount := 1;
  fileopened := False;
  FFilename := aFilename;
  FMonitoringEnabled := aEnableMonitor;
  FOnCustomDSP := aOnCustomDSP;
  FOnCustomDSPUserData := aCustomDSPUserData;

  FDoReadFromStream := @DoReadStreamFromFile;

  if not Error then
  begin
    Fsndfile := ALSOpenAudioFile(aFilename, SFM_READ, Fsfinfo);
    if Fsndfile = nil then
      SetError(als_FileNotOpened)
    else
      fileopened := True;

    if not Error then
    begin
      DecodeFileInfo( Fsndfile, Fsfinfo );

      LockContext( FParentContext.FContext );
      try
        if not Error then
        begin
          // generates the buffers and source
          GenerateALBuffers(NUM_BUFFERS);
          GenerateALSource;
        end;

        if not Error then
        begin
          FBufferFrameCount := Round(BUFFER_TIME_LENGTH*SampleRate);
          SetBuffersFrameCapacity( FBufferFrameCount );

          // prebuf some data
          if not Error then
            PreBuffAudio;
        end;
      finally
        UnlockContext;
      end;
    end;
  end;

  if Error and fileopened then
  begin
    sf_close(Fsndfile);
    Fsndfile := nil;
  end;

  CreateParameters;
end;

destructor TALSStreamBufferSound.Destroy;
begin
  if not FParentContext.Error then
    LockContext( FParentContext.FContext );
  try
    if not Error then
    begin
      alSourceStop( FSource );
      RemoveAllALEffects;
      FDirectFilter.DeleteFilter;
      alSourcei(FSource, AL_BUFFER, 0);
      FreeBuffers;
      alDeleteSources(1, @FSource);
      sf_close(Fsndfile);
    end;
    FreeParameters;
  finally
    if not FParentContext.Error then
      UnlockContext;
  end;
  inherited Destroy;
end;

function TALSStreamBufferSound.GetTimePosition: single;
begin
  if Error then
    Result := 0
  else
  begin
    Result := FFrameReadAccu / FSampleRate;
    if Result > TotalDuration then
      Result := TotalDuration;
  end;
end;

procedure TALSStreamBufferSound.SetTimePosition(AValue: single);
var
  sta: TALSState;
  i: integer;
begin
  if Error then exit;
  if TotalDuration = 0 then exit;
  if (AValue < 0) or (AValue > TotalDuration) then exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    sta := State;
    if sta <> ALS_STOPPED then
      alSourceStop(FSource);

    alSourcei(FSource, AL_BUFFER, 0); // unqueue all buffer from source
    for i:=0 to High(FBuffers) do
      FBuffers[i].Queued := False;

    FFrameReadAccu := Round(AValue/TotalDuration*FFrameCount);
    sf_seek(Fsndfile, sf_count_t(FFrameReadAccu), SF_SEEK_SET);
    PreBuffAudio;

    if sta = ALS_PLAYING then
      alSourcePlay(FSource);
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

{ TALSSingleStaticBufferSound }

procedure TALSSingleStaticBufferSound.InitLevelsFromBuffer(const aBuf: TALSPlaybackBuffer);
var
  i, levelCount: integer;
  frameToRead, c, frameIndex: longword;
begin
  FLevels := NIL;

  levelCount := Round(TotalDuration/LEVEL_TIME_SLICE)+1;
  SetLength(FLevels, levelCount, aBuf.ChannelCount);

  frameToRead := Round(LEVEL_TIME_SLICE * FSampleRate);

  frameIndex := 0;
  i := 0;
  while frameIndex < aBuf.FrameCount do
  begin
    // compute number of frame to read
    if frameIndex + frameToRead >= aBuf.FrameCount then
      c := aBuf.FrameCount - frameIndex
    else
      c := frameToRead;

    if aBuf.UseFloat then
      dsp_ComputeLinearLevel_Float(PSingle(aBuf.DataOffset[frameIndex]),
                      c, aBuf.ChannelCount, @FLevels[i][0])
    else
      dsp_ComputeLinearLevel_Smallint(PSmallint(aBuf.DataOffset[frameIndex]),
                      c, aBuf.ChannelCount, @FLevels[i][0]);

    frameIndex := frameIndex+frameToRead;
    inc(i);
  end;
end;

function TALSSingleStaticBufferSound.GetChannelLevel(index: integer): single;
var
  i: integer;
begin
  if Error or
     not FMonitoringEnabled or
     (State <> ALS_PLAYING) or
     (index < 0) or
     (index >= FChannelCount) then
    Result := 0.0
  else begin
    LockContext( FParentContext.FContext );
    EnterCS;
    try
      i := Trunc(GetTimePosition/LEVEL_TIME_SLICE);
      Result:= FLevels[i][index] * Volume.Value * FMuteMultiplicator;
    finally
      LeaveCS;
      UnlockContext;
    end;
  end;
end;

constructor TALSSingleStaticBufferSound.CreateFromFile(aParent: TALSPlaybackContext;
  const aFilename: string; aEnableMonitor: boolean; aOnCustomDSP: TALSOnCustomDSP;
  aCustomDSPUserData: Pointer);
var
  sndfile: PSNDFILE;
  sfinfo: TSF_INFO;
  frameRead: sf_count_t;
  fileopened: boolean;
begin
  FParentContext := aParent;
  InitializeErrorStatus;
  FChannelCount := 1;
  fileopened := False;
  FFilename := aFilename;
  FMonitoringEnabled := aEnableMonitor;
  FOnCustomDSP := aOnCustomDSP;
  FOnCustomDSPUserData := aCustomDSPUserData;

  if not Error then
  begin
    sndfile := ALSOpenAudioFile(aFilename, SFM_READ, sfinfo{%H-});
    if sndfile = nil then
      SetError(als_FileNotOpened)
    else
      fileopened := True;

    if not Error then
      DecodeFileInfo( sndfile, sfinfo );

    // prepare buffer and load the sound in
    if not Error then
    begin
      LockContext( FParentContext.FContext );
      try
        GenerateALBuffers( 1 );
        if not Error then
        begin
          // reserves memory for buffer
          SetBuffersFrameCapacity( sfinfo.frames );

          if not Error then
          begin
            // load sound data in buffer
            if FParentContext.FUseBufferOfFloat then
              frameRead := sf_readf_float(sndfile, FBuffers[0].Data, sfinfo.frames)
            else
              frameRead := sf_readf_short(sndfile, FBuffers[0].Data, sfinfo.frames);
            FBuffers[0].FrameCount := frameRead;

            if frameRead < 1 then
              SetError(als_FailToReadSample)
            else
            begin
              if frameRead < sfinfo.frames then
              begin
                FFrameCount := frameRead;
                FByteCount := frameRead*FFrameSize;
              end;
              if FOnCustomDSP <> NIL then
                FOnCustomDSP(Self, FBuffers[0], FOnCustomDSPUserData);
            end;
          end;
        end;

        if not Error then
        begin
          alBufferData(FBuffers[0].BufferID, FFormatForAL, FBuffers[0].Data,
            FByteCount, FSampleRate);
          CheckALError(als_ALCanNotFillBuffer);
        end;

        GenerateALSource;
        if not Error then
        begin
          alSourcei(FSource, AL_BUFFER, FBuffers[0].BufferID);
          CheckALError(als_ALCanNotAttachBufferToSource);
        end;

        // generates channel's level by slices of time
        if FMonitoringEnabled then
          InitLevelsFromBuffer(FBuffers[0]);

        FBuffers[0].FreeMemory;
      finally
        UnlockContext;
      end;
    end;

    // close the file
    if fileopened then
      sf_close(sndfile);
  end;

  CreateParameters;
end;

// White noise generation from OpenAL example "altonegen.c"
constructor TALSSingleStaticBufferSound.CreateWhiteNoise(aParent: TALSPlaybackContext;
  aDuration: single; aChannelCount: integer; aEnableMonitor: boolean;
  aOnCustomDSP: TALSOnCustomDSP; aCustomDSPUserData: Pointer);
begin
  FParentContext := aParent;
  InitializeErrorStatus;
  FFilename := '';
  FMonitoringEnabled := aEnableMonitor;
  FOnCustomDSP := aOnCustomDSP;
  FOnCustomDSPUserData := aCustomDSPUserData;

  if not Error then
  begin
    FChannelCount := aChannelCount;
    FSampleRate := FParentContext.FObtainedSampleRate;
    FFrameCount := Trunc( FSampleRate * aDuration );
    FStrFormat := '';
    if FParentContext.FUseBufferOfFloat then
    begin
      FStrSubFormat := '32 bit FLOAT';
      FFrameSize := SizeOf(Single) * FChannelCount;
    end
    else
    begin
      FStrSubFormat := '16 bit PCM';
      FFrameSize := SizeOf(Word) * FChannelCount;
    end;
    FByteCount := FFrameCount * FFrameSize;

    FFormatForAL := GetFormatForAL(FChannelCount, FParentContext.FUseBufferOfFloat, FALSE);

    if not Error then
    begin
      // generate buffer and source
      LockContext( FParentContext.FContext );
      try
        GenerateALBuffers( 1 );
        if not Error then
        begin
          // fill buffer data in memory
          SetBuffersFrameCapacity( FFrameCount );
          if not Error then
          begin
            if FParentContext.FUseBufferOfFloat then
              dsp_FillWithWhiteNoise_Single(FBuffers[0].Data, FFrameCount, FBuffers[0].ChannelCount)
            else
              dsp_FillWithWhiteNoise_Smallint(FBuffers[0].Data, FFrameCount, FBuffers[0].ChannelCount);

            if FOnCustomDSP <> NIL then
              FOnCustomDSP(Self, FBuffers[0], FOnCustomDSPUserData);

            alBufferData(FBuffers[0].BufferID, FFormatForAL, FBuffers[0].Data, FByteCount, FSampleRate);
            CheckALError(als_ALCanNotFillBuffer);

            if not Error and FMonitoringEnabled then
              InitLevelsFromBuffer(FBuffers[0]); // generates channel's level by slices of time

            FBuffers[0].FreeMemory;
          end;
        end;

        GenerateALSource;
        if not Error then
        begin
          alSourcei(FSource, AL_BUFFER, FBuffers[0].BufferID);
          CheckALError(als_ALCanNotAttachBufferToSource);
        end;
      finally
        UnlockContext;
      end;

    end;
  end;

  CreateParameters;
end;

destructor TALSSingleStaticBufferSound.Destroy;
begin
  if not FParentContext.Error then
    LockContext(FParentContext.FContext);
  try
    if not Error then
    begin
      alSourceStop(FSource);
      RemoveAllALEffects;
      FDirectFilter.DeleteFilter;
      alSourcei(FSource, AL_BUFFER, 0);
      FreeBuffers;
      alDeleteSources(1, @FSource);
    end;
    FreeParameters;
  finally
    if not FParentContext.Error then
      UnlockContext;
  end;
  inherited Destroy;
end;


{ TALSThread }

procedure TALSThread.Execute;
var
  T1, T2, DeltaT: QWord;
begin
  T1 := GetTickCount64;
  while not Terminated do
  begin
    T2 := GetTickCount64;
    DeltaT := T2 - T1;
    if (DeltaT >= 1) and
       ((FPeriodUpdate = 0) or (DeltaT >= FPeriodUpdate)) then
    begin
      FDoUpdate(single(DeltaT) * 0.001);
      T1 := T2;
    end;
    if FPeriodUpdate > 1 then
      sleep(FPeriodUpdate - 1);
  end;
end;

constructor TALSThread.Create(aCallBackDoUpdate: TALSDoUpdate;
  aUpdatePeriod: cardinal; aStart: boolean);
begin
  inherited Create(True);
  FPeriodUpdate := aUpdatePeriod;
  FDoUpdate := aCallBackDoUpdate;
  if aStart then
    Start;
end;

{ TALSPlaybackContext }

destructor TALSPlaybackContext.Destroy;
begin
  if FThread <> NIL then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  DeleteAll;
  FreeParameters;

  DoneCriticalSection( FCriticalSection );

  if not Error then
  begin
    if FContext <> nil then
    begin
      LockContext(nil);
      try
        alcDestroyContext(FContext);
        FContext := nil;
      finally
        UnlockContext;
      end;
    end;
    InternalCloseDevice;
  end;
  inherited Destroy;
end;

function TALSPlaybackContext.AddStream(const aFilename: string; aEnableMonitoring: boolean;
  aOnCustomDSP: TALSOnCustomDSP; aCustomDSPUserData: Pointer): TALSSound;
begin
  EnterCriticalSection(FCriticalSection);
  LockContext( FContext );
  try
    Result := TALSStreamBufferSound.CreateFromFile(Self, aFilename,
                  aEnableMonitoring, aOnCustomDSP, aCustomDSPUserData);
    FList.Add(Result);
  finally
    LeaveCriticalSection(FCriticalSection);
    UnlockContext;
  end;
end;

function TALSPlaybackContext.CreateWhiteNoise(aDuration: single;
  aChannelCount: integer; aEnableMonitoring: boolean;
  aOnCustomDSP: TALSOnCustomDSP; aCustomDSPUserData: Pointer): TALSSound;
begin
  EnterCriticalSection(FCriticalSection);
  LockContext( FContext );
  try
    Result := TALSSingleStaticBufferSound.CreateWhiteNoise(Self, aDuration,
       aChannelCount, aEnableMonitoring, aOnCustomDSP, aCustomDSPUserData);
    FList.Add(Result);
  finally
    LeaveCriticalSection(FCriticalSection);
    UnlockContext;
  end;
end;

procedure TALSPlaybackContext.DoUpdate(const aElapsedTime: single);
var
  i: integer;
begin
  EnterCriticalSection(FCriticalSection);
  FThreadIsStarted:=True;
  try
    if not Error then
    begin
      LockContext( FContext );
      if FHaveExt_AL_SOFT_deferred_updates then
        alDeferUpdatesSOFT()
      else
        alcSuspendContext(FContext);
    end;

    if FMasterGain.State <> alspsNo_CHANGE then
    begin
      FMasterGain.OnElapse( aElapsedTime );
      if not Error then
        alListenerf( AL_GAIN, FMasterGain.Value );
    end;
    // Kill or update sounds
    for i := FList.Count - 1 downto 0 do
    begin
      FSoundToProcess := TALSSound(FList.Items[i]);
      if FSoundToProcess.FKill then
        InternalDeleteSound(i)
      else
        FSoundToProcess.Update(aElapsedTime);
    end;
    // update playlist (if exists)
    if FPlaylist <> nil then
      FPlaylist.Update(aElapsedTime);
  finally
    if not Error then
    begin
      if FHaveExt_AL_SOFT_deferred_updates then
        alProcessUpdatesSOFT()
      else
        alcProcessContext(FContext);
      UnlockContext;
    end;
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TALSPlaybackContext.DoSoundOnStopped;
begin
  if FSoundToProcess.FOnStopped <> NIL then
    FSoundToProcess.FOnStopped(FSoundToProcess);
end;

procedure TALSPlaybackContext.EnterCS;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TALSPlaybackContext.LeaveCS;
begin
  LeaveCriticalSection(FCriticalSection);
end;

procedure TALSPlaybackContext.DeleteAll;
var
  i: integer;
begin
  LockContext( FContext );
  try
    EnterCriticalSection(FCriticalSection);
    try
      for i := 0 to GetSoundCount - 1 do
        GetSoundByIndex(i).Free;
      FList.Clear;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  finally
    UnlockContext;
  end;
end;

procedure TALSPlaybackContext.StopAllSound;
var
  i: integer;
begin
  for i := 0 to GetSoundCount - 1 do
    GetSoundByIndex(i).Stop;
end;

function TALSPlaybackContext.CreateEffect(aEffectType: TALSEffectType;
  const aParameters): TALSEffect;
var
  temp: TALSReverbProperties;
  preset: TEAXReverbProperties;
begin
  Result.InitDefault(Self);

  if not Error and FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX then
  begin
    LockContext( FContext );
    try
      if (aEffectType = AL_EFFECT_EAXREVERB) and
        (alGetEnumValue(PChar('AL_EFFECT_EAXREVERB')) = 0) then
      begin
        // EAX is not available  ->  use a standard reverb
        preset := TEAXReverbProperties(aParameters);
        temp.Density := preset.flDensity;
        temp.Diffusion := preset.flDiffusion;
        temp.Gain := preset.flGain;
        temp.GainHF := preset.flGainHF;
        temp.DecayTime := preset.flDecayTime;
        temp.DecayHFRatio := preset.flDecayHFRatio;
        temp.ReflectionsGain := preset.flReflectionsGain;
        temp.ReflectionsDelay := preset.flReflectionsDelay;
        temp.LateReverbGain := preset.flLateReverbGain;
        temp.LateReverbDelay := preset.flLateReverbDelay;
        temp.AirAbsorptionGainHF := preset.flAirAbsorptionGainHF;
        temp.RoomRolloffFactor := preset.flRoomRolloffFactor;
        temp.DecayHFLimit := preset.iDecayHFLimit;
        Result.InitEffect(AL_EFFECT_REVERB, temp);
      end
      else
        Result.InitEffect(aEffectType, aParameters);
    finally
      UnlockContext;
    end;
  end
  else
    Result.FReady := False;
end;

procedure TALSPlaybackContext.DeleteEffect(var aEffect: TALSEffect);
var
  i: Integer;
begin
  if Error then exit;

  // disconnect effect from auxiliary send
  for i:=0 to SoundCount-1 do
    GetSoundByIndex(i).RemoveEffect(aEffect);

  LockContext( FContext );
  try
    aEffect.DealocateALObjects;

    // Extract from effect's chain
    if aEffect.FPrevious <> NIL then
      aEffect.FPrevious^.FNext := aEffect.FNext;

    if aEffect.FNext <> NIL then
      aEffect.FNext^.FPrevious := aEffect.FPrevious;

    aEffect.FPrevious := NIL;
    aEffect.FNext := NIL;

  finally
    UnlockContext;
  end;
end;

function TALSPlaybackContext.ChangeAttributes(const aAttribs: TALSContextAttributes): boolean;
begin
  if Error or not FHaveExt_ALC_SOFT_HRTF then
    Result := FALSE
  else
  begin
    LockContext( FContext );
    try
      Result := FParentDeviceItem^.FalcResetDeviceSOFT( FParentDevice, @aAttribs );
    finally
      UnlockContext;
    end;
  end;
end;

procedure TALSPlaybackContext.SetListenerPosition(aX, aY, aZ: single);
begin
  if not Error then
  begin
    LockContext( FContext );
    try
      alListener3f( AL_POSITION, aX, aY, aZ );
    finally
      UnlockContext;
    end;
  end;
end;

procedure TALSPlaybackContext.SetListenerVelocity(aX, aY, aZ: single);
begin
  if not Error then
  begin
    LockContext( FContext );
    try
      alListener3f( AL_VELOCITY, aX, aY, aZ );
    finally
      UnlockContext;
    end;
  end;
end;

procedure TALSPlaybackContext.SetListenerOrientation(aATX, aATY, aATZ, aUPX, aUPY, aUPZ: single);
var
  A: array[0..5] of ALfloat;
begin
  if not Error then
  begin
    A[0] := aATX;
    A[1] := aATY;
    A[2] := aATZ;
    A[3] := aUPX;
    A[4] := aUPY;
    A[5] := aUPZ;
    LockContext( FContext );
    try
      alListenerfv( AL_ORIENTATION, @A[0] );
    finally
      UnlockContext;
    end;
  end;
end;

function TALSPlaybackContext.GetSoundCount: integer;
begin
  try
    EnterCriticalSection(FCriticalSection);
    Result := FList.Count;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TALSPlaybackContext.GetObtainedMonoCount: integer;
var
  p: ALCint;
begin
  if Error then
    Result := 0
  else
  begin
    LockContext( FContext );
    try
      alcGetIntegerv(FParentDevice, ALC_MONO_SOURCES, sizeof(p), @p);
    finally
      UnlockContext;
    end;
    Result := p;
  end;
end;

function TALSPlaybackContext.GetObtainedStereoCount: integer;
var
  p: ALCint;
begin
  if Error then
    Result := 0
  else
  begin
    LockContext( FContext );
    try
      alcGetIntegerv(FParentDevice, ALC_STEREO_SOURCES, sizeof(p), @p);
    finally
      UnlockContext;
    end;
    Result := p;
  end;
end;

function TALSPlaybackContext.GetHRTFEnabled: boolean;
var
  hrtf_state: ALCint;
begin
  if not Error then
  begin
    LockContext( FContext );
    try
      alcGetIntegerv(FParentDevice, ALC_HRTF_SOFT, 1, @hrtf_state);
    finally
      UnlockContext;
    end;
    Result := hrtf_state<>0;
  end
  else
    Result := False;
end;

function TALSPlaybackContext.GetHaveFilter: boolean;
begin
  Result := FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX and
           (FHaveLowPassFilter or FHaveBandPassFilter or FHaveHighPassFilter);
end;

function TALSPlaybackContext.GetHaveEXT_ALC_EXT_EFX: boolean;
begin
  Result := FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX;
end;

function TALSPlaybackContext.GetHaveExt_ALC_SOFT_HRTF: boolean;
begin
  Result := FParentDeviceItem^.FHaveExt_ALC_SOFT_HRTF;
end;

function TALSPlaybackContext.GetHRTFList: TStringArray;
var
  num_hrtf: ALCint;
  i: integer;
begin
  Result := NIL;
  LockContext( FContext );
  try
    if Error or not FHaveExt_ALC_SOFT_HRTF then
      SetLength(Result, 0)
    else
    begin
      alcGetIntegerv(FParentDevice, ALC_NUM_HRTF_SPECIFIERS_SOFT, 1, @num_hrtf);
      SetLength(Result, num_hrtf);
      for i:=0 to num_hrtf-1 do
        Result[i] := FParentDeviceItem^.FalcGetStringiSOFT(FParentDevice, ALC_HRTF_SPECIFIER_SOFT, ALCSizei(i));
    end;
  finally
    UnlockContext;
  end;
end;

function TALSPlaybackContext.GetResamplerList: TStringArray;
var num_resamplers, i: ALint;
begin
  Result := NIL;
  LockContext( FContext );
  try
    if FHaveExt_AL_SOFT_source_resampler then
    begin
        num_resamplers := alGetInteger(AL_NUM_RESAMPLERS_SOFT);

        if num_resamplers <> 0 then
        begin
          SetLength(Result, num_resamplers);
          for i:=0 to num_resamplers-1 do
            Result[i] := StrPas(alGetStringiSOFT(AL_RESAMPLER_NAME_SOFT, i));
        end;
        alGetError(); // reset error
    end;
  finally
    UnlockContext;
  end;
end;

function TALSPlaybackContext.GetSoundByIndex(index: integer): TALSSound;
begin
  try
    EnterCriticalSection(FCriticalSection);
    Result := TALSSound(FList.Items[index]);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TALSPlaybackContext.GetObtainedAuxiliarySendCount: integer;
begin
  Result := FAuxiliarySendAvailable;
end;

procedure TALSPlaybackContext.InternalDeleteSound(AIndex: integer);
begin
  if (AIndex < 0) and (AIndex >= GetSoundCount) then
    exit;

  TALSSound(FList.Items[AIndex]).Free;
  FList.Delete(AIndex);
end;

procedure TALSPlaybackContext.SetDistanceModel(AValue: TALSDistanceModel);
begin
  if AValue = FDistanceModel then exit;
  FDistanceModel := AValue;
  if not Error then
  begin
    LockContext( FContext );
    try
      alDistanceModel( Ord(AValue) );
    finally
      UnlockContext;
    end;
  end;
end;

procedure TALSPlaybackContext.SetListenerGain;
begin
  if Error or FExecutingConstructor then
    exit;

  LockContext( FContext );
  try
    alListenerf( AL_GAIN, FMasterGain.Value );
  finally
    UnlockContext;
  end;
end;

procedure TALSPlaybackContext.InitializeALContext(
  const aAttribs: TALSContextAttributes);
var
  A: array[0..5] of single;
  obj: ALuint;
begin
  FExecutingConstructor := True;
  FDistanceModel := AL_NONE;
  FUseBufferOfFloat := aAttribs.ContextUseFloat;
  FSampleRate := aAttribs.SampleRate;
  FInternalSampleType := ALS_SAMPLE_INT16;

  // Look for some extensions before the creation of the context because
  // some attributes use them.
{  FHaveEXT_ALC_EXT_EFX := alcIsExtensionPresent(FParentDevice, PChar('ALC_EXT_EFX'));
  if FHaveEXT_ALC_EXT_EFX then
    FHaveEXT_ALC_EXT_EFX := LoadExt_ALC_EXT_EFX;

  FHaveExt_ALC_SOFT_HRTF := alcIsExtensionPresent(FParentDevice, PChar('ALC_SOFT_HRTF'));
  if FHaveExt_ALC_SOFT_HRTF then
    FHaveExt_ALC_SOFT_HRTF := LoadExt_ALC_SOFT_HRTF(FParentDevice);  }

  FHaveExt_AL_SOFT_source_resampler := alcIsExtensionPresent(FParentDevice, PChar('AL_SOFT_source_resampler'));
  if FHaveExt_AL_SOFT_source_resampler then
    FHaveExt_AL_SOFT_source_resampler := LoadExt_AL_SOFT_source_resampler;

//  FHaveExt_ALC_SOFT_output_mode := alcIsExtensionPresent(FParentDevice, PChar('ALC_SOFT_output_mode'));

//  FHaveExt_ALC_SOFT_loopback := alcIsExtensionPresent(FParentDevice, PChar('ALC_SOFT_loopback'));
  // extension is loaded in TALSLoopbackDeviceItem.Open

//  FHaveExt_ALC_SOFT_output_limiter := alcIsExtensionPresent(FParentDevice, PChar('ALC_SOFT_output_limiter'));

  FContext := alcCreateContext(FParentDevice,
    @aAttribs.ToArray(FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX,
                      FParentDeviceItem^.FHaveExt_ALC_SOFT_output_mode,
                      FParentDeviceItem^.FHaveExt_ALC_SOFT_HRTF,
                      FParentDeviceItem^.FHaveExt_ALC_SOFT_loopback,
                      FParentDeviceItem^.FHaveExt_ALC_SOFT_output_limiter)[0]);
  CheckALCError(FParentDevice, als_ALContextNotCreated);

  if FContext <> nil then
  begin
    LockContext( FContext );
    try
      // set default distance model
      alDistanceModel(ALenum(Ord(FDistanceModel)));
      // enable source to have their own distance model
      // https://openal-soft.org/openal-extensions/EXT_source_distance_model.txt
      alEnable( AL_SOURCE_DISTANCE_MODEL );
      // default listener position and orientation
      A[0] := 0.0;
      A[1] := 0.0;
      A[2] := 0.0;
      alListenerfv(AL_POSITION, @A[0]);
      alListenerfv(AL_VELOCITY, @A[0]);
      A[0] := 0.0;
      A[1] := 0.0;
      A[2] := -1.0;
      A[3] := 0.0;
      A[4] := 1.0;
      A[5] := 0.0;
      alListenerfv(AL_ORIENTATION, @A[0]);

      //check for available filter
      if FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX then
      begin
        alGenFilters(1, @obj);
        alGetError();
        alFilteri(obj, AL_FILTER_TYPE, AL_FILTER_LOWPASS);
        FHaveLowPassFilter := alGetError() = AL_NO_ERROR;
        alFilteri(obj, AL_FILTER_TYPE, AL_FILTER_HIGHPASS);
        FHaveHighPassFilter := alGetError() = AL_NO_ERROR;
        alFilteri(obj, AL_FILTER_TYPE, AL_FILTER_BANDPASS);
        FHaveBandPassFilter := alGetError() = AL_NO_ERROR;
        alDeleteFilters(1, @obj);
      end;


      FHaveExt_AL_SOFT_gain_clamp_ex := alIsExtensionPresent(PChar('AL_SOFT_gain_clamp_ex'));
      if FHaveExt_AL_SOFT_gain_clamp_ex then
      begin  // sets max amplification gain to 8.0
       // alListenerf(AL_GAIN_LIMIT_SOFT, ALfloat(ALS_VOLUME_MAXAMP));
        alGetError();
      end;

      FHaveExt_AL_EXT_source_distance_model := alIsExtensionPresent(PChar('AL_EXT_source_distance_model'));
      if FHaveExt_AL_EXT_source_distance_model then
        alEnable( AL_SOURCE_DISTANCE_MODEL );// enable source to have their own distance model
                                             // https://openal-soft.org/openal-extensions/EXT_source_distance_model.txt

      FHaveExt_AL_SOFT_deferred_updates := alIsExtensionPresent(PChar('AL_SOFT_deferred_updates'));
      if FHaveExt_AL_SOFT_deferred_updates then
        FHaveExt_AL_SOFT_deferred_updates := LoadExt_AL_SOFT_deferred_updates;

      FHaveExt_AL_EXT_STEREO_ANGLES := alIsExtensionPresent(PChar('AL_EXT_STEREO_ANGLES'));

      FHaveExt_AL_SOFT_effect_target := alIsExtensionPresent(PChar('AL_SOFT_effect_target'));

      FHaveExt_AL_SOFT_source_spatialize := alIsExtensionPresent(PChar('AL_SOFT_source_spatialize'));

      FHaveExt_AL_SOFT_buffer_samples := alIsExtensionPresent(PChar('AL_SOFT_buffer_samples'));
      if FHaveExt_AL_SOFT_buffer_samples then
        FHaveExt_AL_SOFT_buffer_samples := LoadExt_AL_SOFT_buffer_samples;

      FHaveExt_AL_SOFT_buffer_sub_data := alIsExtensionPresent(PChar('AL_SOFT_buffer_sub_data'));
      if FHaveExt_AL_SOFT_buffer_sub_data then
        FHaveExt_AL_SOFT_buffer_sub_data := LoadExt_AL_SOFT_buffer_sub_data;

      FUseBufferOfFloat := ((alGetEnumValue(PChar('AL_FORMAT_MONO_FLOAT32')) <> 0) or
                            FHaveExt_AL_SOFT_buffer_samples) and aAttribs.ContextUseFloat;

      if aAttribs.ContextUseFloat and FUseBufferOfFloat then
         FInternalSampleType := ALS_SAMPLE_FLOAT32;

      FHaveExt_AL_EXT_BFORMAT := alIsExtensionPresent(PChar('AL_EXT_BFORMAT'));

      if FHaveExt_AL_SOFT_source_resampler then
        FDefaultResamplerIndex := integer(alGetInteger(AL_DEFAULT_RESAMPLER_SOFT))
      else
        FDefaultResamplerIndex := -1;

      alcGetIntegerv(FParentDevice, ALC_MAX_AUXILIARY_SENDS, 1, @FAuxiliarySendAvailable);
      alcGetIntegerv(FParentDevice, ALC_FREQUENCY, 1, @FObtainedSampleRate);
    finally
      UnlockContext;
    end;
  end;
end;

procedure TALSPlaybackContext.CreateParameters;
begin
  FList := TFPList.Create;
  FPlaylist := TALSPlaylist.Create(Self);
  FMasterGain := TALSBoundedFParam.Create(ALS_VOLUME_MIN, ALS_VOLUME_MAX, ALS_VOLUME_MAX);
  FMasterGain.FProc := @SetListenerGain;
  FMasterGain.FOnLockParam := @EnterCS;
  FMasterGain.FOnUnlockParam := @LeaveCS;
end;

procedure TALSPlaybackContext.FreeParameters;
begin
  FreeAndNil(FList);
  FreeAndNil(FPlaylist);
  FreeAndNil(FMasterGain);
end;

procedure TALSPlaybackContext.InternalCloseDevice;
begin
  ALSManager.ClosePlaybackDevice(FParentDevice);
end;

function TALSPlaybackContext.AddCapturePlayback(aSampleRate: integer;
  aCaptureBuffer: PALSCaptureFrameBuffer): TALSPlaybackCapturedSound;
begin
  LockContext( FContext );
  EnterCriticalSection(FCriticalSection);
  try
    Result := TALSPlaybackCapturedSound.CreateFromCapture(Self, aSampleRate, aCaptureBuffer);
    FList.Add(Result);
  finally
    LeaveCriticalSection(FCriticalSection);
    UnlockContext;
  end;
end;

constructor TALSPlaybackContext.Create(aDevice: PALSPlaybackDeviceItem;
  const aAttribs: TALSContextAttributes);
begin
  FExecutingConstructor := True;
  InitializeErrorStatus;
  FParentDeviceItem := PALSDeviceItem(aDevice);
  FObtainedSampleRate := aAttribs.SampleRate;

  if aDevice = NIL then begin
    FParentDevice := NIL;
    SetError(als_ALTryToCreateContextOnNonExistentDevice)
  end else begin
    FParentDevice := aDevice^.Handle;
    if aDevice^.Handle = NIL then
      SetError(als_ALCanNotOpenPlaybackDevice)
    else
      InitializeALContext(aAttribs);
  end;


  CreateParameters;

  InitCriticalSection( FCriticalSection );
  FThreadIsStarted:=False;
  FThread := TALSThread.Create(@DoUpdate, 10, True);
  FThread.Priority := tpHighest;
  // waits for the thread to be started to prevent any problems
  while not FThreadIsStarted do
    Sleep(1);
  FExecutingConstructor := False;
end;

function TALSPlaybackContext.AddSound(const aFilename: string; aEnableMonitoring: boolean;
  aOnCustomDSP: TALSOnCustomDSP; aCustomDSPUserData: Pointer): TALSSound;
begin
  LockContext( FContext );
  try

    EnterCriticalSection(FCriticalSection);
    try
      Result := TALSSingleStaticBufferSound.CreateFromFile(Self, aFilename,
              aEnableMonitoring, aOnCustomDSP, aCustomDSPUserData);
      FList.Add(Result);
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

  finally
    UnlockContext;
  end;
end;

procedure TALSPlaybackContext.PlaySoundThenKill(const aFilename: string;
  aVolume: single);
var
  snd: TALSSound;
begin
  snd := AddSound(aFilename, False);
  if snd = nil then
    exit;
  snd.Volume.Value := aVolume;
  snd.PlayThenKill(True);
end;

procedure TALSPlaybackContext.PlayStreamThenKill(const aFilename: string;
  aVolume: single);
var
  snd: TALSSound;
begin
  snd := AddStream(aFilename, False);
  if snd = nil then
    exit;
  snd.Volume.Value := aVolume;
  snd.PlayThenKill(True);
end;

procedure TALSPlaybackContext.Delete(ASound: TALSSound);
begin
  try
    EnterCriticalSection(FCriticalSection);
    InternalDeleteSound(FList.IndexOf(ASound));
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

{ TALSSound }
procedure TALSSound.CreateParameters;
var
  i: integer;
  v: single;
begin
  InitCriticalSection(FCriticalSection);

  FMuteMultiplicator := 1.0;
  FGlobalVolume := 1.0;

  if FParentContext.FHaveExt_AL_SOFT_gain_clamp_ex then
    v := ALS_VOLUME_MAXAMP
  else
    v := ALS_VOLUME_MAX;
  Volume := TALSBoundedFParam.Create(ALS_VOLUME_MIN, v, ALS_VOLUME_MAX);
  Volume.FProc := @SetALVolume;
  Volume.FOnLockParam := @EnterCS;
  Volume.FOnUnlockParam := @LeaveCS;

  Pan := TALSBoundedFParam.Create(ALS_PAN_LEFT, ALS_PAN_RIGHT, ALS_PAN_CENTER);
  Pan.FProc := @SetALPan;
  Pan.FOnLockParam := @EnterCS;
  Pan.FOnUnlockParam := @LeaveCS;

  Pitch := TALSBoundedFParam.Create(ALS_PITCH_MIN, ALS_PITCH_MAX, ALS_PITCH_NORMAL);
  Pitch.FProc := @SetALPitch;
  Pitch.FOnLockParam := @EnterCS;
  Pitch.FOnUnlockParam := @LeaveCS;

  Tone := TALSBoundedFParam.Create(ALS_TONE_ONLYLOWFREQ, ALS_TONE_ONLYHIGHFREQ, ALS_TONE_NORMAL);
  Tone.FProc := @SetALTone;
  Tone.FOnLockParam := @EnterCS;
  Tone.FOnUnlockParam := @LeaveCS;

  if not Error and FParentContext.FParentDeviceItem^.FHaveEXT_ALC_EXT_EFX then
  begin
    SetLength(FAuxiliarySend, FParentContext.ObtainedAuxiliarySendCount);
    for i := 0 to High(FAuxiliarySend) do
      FAuxiliarySend[i].Init(Self, i);
  end
  else
    FAuxiliarySend := nil;

  FDirectFilter.Init(Self);
  FApplyToneOnAuxSend := True;
end;

procedure TALSSound.FreeParameters;
var
  i: integer;
begin
  Volume.Free;
  Pan.Free;
  Pitch.Free;
  Tone.Free;
  DoneCriticalSection(FCriticalSection);
  if not Error then
    for i:=0 to High(FAuxiliarySend) do
      FAuxiliarySend[i].Disconnect;
end;

procedure TALSSound.SetOnCustomDSP(aProc: TALSOnCustomDSP; aUserData: Pointer);
begin
  EnterCS;
  try
    FOnCustomDSP := aProc;
    FOnCustomDSPUserData := aUserData;
  finally
    LeaveCS;
  end;
end;

procedure TALSSound.SetOnStopped(AValue: TALSNotifyEvent);
begin
  EnterCS;
  try
    FOnStopped := AValue;
  finally
    LeaveCS;
  end;
end;

function TALSSound.GetFormatForAL(aChannelCount: integer; aContextUseFloat,
  aWantBFormatAmbisonic: boolean): DWord;
begin
  Result := 0;
  if aWantBFormatAmbisonic and not FParentContext.FHaveExt_AL_EXT_BFORMAT then
  begin
    SetError(als_ALCanNotManageBFormat);
    exit;
  end;

  case aChannelCount of
    // 1 channel
    1: if aContextUseFloat then
       begin
         if alGetEnumValue(PChar('AL_FORMAT_MONO_FLOAT32')) <> 0 then
           Result := AL_FORMAT_MONO_FLOAT32
         else
           SetError(als_FloatSampleNotSupported);
       end
       else
         Result := AL_FORMAT_MONO16;

    // 2 channels
    2: if aContextUseFloat then
       begin
         if alGetEnumValue(PChar('AL_FORMAT_STEREO_FLOAT32')) <> 0 then
           Result := AL_FORMAT_STEREO_FLOAT32
         else
           SetError(als_FloatSampleNotSupported);
       end
       else
         Result := AL_FORMAT_STEREO16;

    3: if aWantBFormatAmbisonic then
       begin
         if aContextUseFloat then
         begin
           if alGetEnumValue(PChar('AL_FORMAT_BFORMAT2D_FLOAT32')) <> 0 then
             Result := AL_FORMAT_BFORMAT2D_FLOAT32
           else
             SetError(als_FloatSampleNotSupported);
         end
         else
           if (alGetEnumValue(PChar('AL_FORMAT_BFORMAT2D_16')) <> 0) then
             Result := AL_FORMAT_BFORMAT2D_16
           else
             SetError(als_UnsupportedFormat);
       end
       else SetError(als_UnsupportedFormat);

    4: if aWantBFormatAmbisonic then
       begin
        if aContextUseFloat then
        begin
          if alGetEnumValue(PChar('AL_FORMAT_BFORMAT3D_FLOAT32')) <> 0 then
            Result := AL_FORMAT_BFORMAT3D_FLOAT32
          else
            SetError(als_FloatSampleNotSupported);
        end
        else
          if (alGetEnumValue(PChar('AL_FORMAT_BFORMAT3D_16')) <> 0) then
            Result := AL_FORMAT_BFORMAT3D_16
          else
            SetError(als_UnsupportedFormat);
       end
       else
       begin
         if (alGetEnumValue(PChar('AL_FORMAT_QUAD16')) <> 0) and
            not aContextUseFloat then
           Result := AL_FORMAT_QUAD16
         else
           SetError(als_UnsupportedFormat);
       end;

    6: if aContextUseFloat then
       begin
        if alGetEnumValue(PChar('AL_FORMAT_51CHN32')) <> 0 then
          Result := AL_FORMAT_51CHN32
        else
          SetError(als_FloatSampleNotSupported);
       end
       else
         if (alGetEnumValue(PChar('AL_FORMAT_51CHN16')) <> 0) then
           Result := AL_FORMAT_51CHN16
         else
           SetError(als_UnsupportedFormat);

    7: if aContextUseFloat then
       begin
        if alGetEnumValue(PChar('AL_FORMAT_61CHN32')) <> 0 then
          Result := AL_FORMAT_61CHN32
        else
          SetError(als_FloatSampleNotSupported);
       end
       else
         if (alGetEnumValue(PChar('AL_FORMAT_61CHN16')) <> 0) then
           Result := AL_FORMAT_61CHN16
         else
           SetError(als_UnsupportedFormat);

    8: if aContextUseFloat then
       begin
         if alGetEnumValue(PChar('AL_FORMAT_71CHN32')) <> 0 then
           Result := AL_FORMAT_71CHN32
         else
           SetError(als_FloatSampleNotSupported);
       end
       else
         if (alGetEnumValue(PChar('AL_FORMAT_71CHN16')) <> 0) then
           Result := AL_FORMAT_71CHN16
         else
           SetError(als_UnsupportedFormat);
    else
      SetError(als_UnsupportedChannelCount);
  end;//case
end;

procedure TALSSound.DecodeFileInfo(aFileHandle: PSNDFILE; aSFInfo: TSF_INFO);
var
  formatInfo: TSF_FORMAT_INFO;
  haveBFormatAmbisonic: boolean;
begin
  FSampleRate := aSFInfo.Samplerate;
  FChannelCount := aSFInfo.Channels;
  FFrameCount := aSFInfo.frames;

  // retrieve the format and sub-format of the audio file and
  // try to translate them to an OpenAL format
  formatInfo.format := aSFInfo.Format and SF_FORMAT_TYPEMASK;
  sf_command( aFileHandle, SFC_GET_FORMAT_INFO, @formatInfo, SizeOf(TSF_FORMAT_INFO) );
  FStrFormat := StrPas(formatInfo.Name);

  formatInfo.format := aSFInfo.Format and SF_FORMAT_SUBMASK;
  sf_command( aFileHandle, SFC_GET_FORMAT_INFO, @formatInfo, SizeOf(TSF_FORMAT_INFO) );
  FStrSubFormat := StrPas(formatInfo.Name);

  // OpenAL BFormat support see:
  //    https://icculus.org/alextreg/wiki/action=printer&id=AL_EXT_BFORMAT
  //    http://dream.cs.bath.ac.uk/researchdev/wave-ex/bformat.html
  haveBFormatAmbisonic := sf_command(aFileHandle, SFC_WAVEX_GET_AMBISONIC, nil, 0) = SF_AMBISONIC_B_FORMAT;

  FFormatForAL := GetFormatForAL( FChannelCount, FParentContext.FUseBufferOfFloat, haveBFormatAmbisonic );

  if FParentContext.FUseBufferOfFloat then
    FFrameSize := SizeOf(Single) * FChannelCount
  else
    FFrameSize := SizeOf(Word) * FChannelCount;

  FByteCount := aSFInfo.frames * FFrameSize;
end;

procedure TALSSound.InitializeErrorStatus;
begin
  if FParentContext <> NIL then
    FErrorCode := FParentContext.FErrorCode
  else
    FErrorCode := als_ALContextNotCreated;
end;

procedure TALSSound.GenerateALSource;
begin
  if Error then
    exit;

  alGetError();
  alGenSources(1, @FSource);
  if not CheckALError(als_ALCanNotGenerateSource) then
  begin
    SetPositionRelativeToListener( True );
    alSource3f(FSource, AL_POSITION, 0.0, 0.0, -1.0);
    alSourcef(FSource, AL_ROLLOFF_FACTOR, 0.0);

    if FParentContext.FHaveExt_AL_EXT_source_distance_model then
    begin
      alSourcei(FSource, AL_DISTANCE_MODEL, ALint(FParentContext.FDistanceModel) );
      alGetError();
    end;

    if FParentContext.FHaveExt_AL_SOFT_gain_clamp_ex then
    begin
      alSourcef(FSource, AL_MAX_GAIN, ALS_VOLUME_MAXAMP);
      alGetError();
    end;
  end;
end;

procedure TALSSound.GenerateALBuffers(aCount: integer);
var
  err: ALenum;
  i: Integer;
begin
  FBuffers := NIL;
  SetLength(FBuffers, aCount);
  for i:=0 to High(FBuffers) do
  begin
    FBuffers[i].Init(FChannelCount, FParentContext.FInternalSampleType);
    err := FBuffers[i].GenerateBufferID;
    if err <> AL_NO_ERROR then
      SetALError(als_ALCanNotGenerateBuffer, err);
  end;
end;

procedure TALSSound.SetBuffersFrameCapacity(aFrameCapacity: longword);
var
  i, j: Integer;
begin
  for i:=0 to High(FBuffers) do
  begin
    FBuffers[i].FrameCapacity := aFrameCapacity;
    if FBuffers[i].OutOfMemory then
    begin
      for j:=0 to i do
       FBuffers[j].FreeMemory;
      SetError(als_OutOfMemory);
    end;
  end;
end;

procedure TALSSound.FreeBuffers;
var
  i: Integer;
begin
  for i:=0 to High(FBuffers) do
  begin
    FBuffers[i].FreeMemory;
    FBuffers[i].DeleteBufferID;
  end;
end;

procedure TALSSound.RemoveAllALEffects;
var
  i: integer;
begin
  for i := 0 to High(FAuxiliarySend) do
    FAuxiliarySend[i].Disconnect;
end;

function TALSSound.AllAuxiliarySendAreEmpty: boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(FAuxiliarySend) do
    Result := Result and
      not(FAuxiliarySend[i].IsConnected or FAuxiliarySend[i].IsConnectedWithAChain);
end;

procedure TALSSound.SetLoop(AValue: boolean);
var
  v: integer;
  stat: ALint;
begin
  FLoop := AValue;
  if Error then
    exit;
  LockContext( FParentContext.FContext );
  EnterCS;
  try
    alGetSourcei(FSource, AL_SOURCE_TYPE, stat{%H-});
    if stat = AL_STATIC then
    begin
      case AValue of
       False: v := 0;
      else
        v := 1;
      end;
      alSourcei(FSource, AL_LOOPING, v);
    end;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetALVolume;
begin
  if Error or FParentContext.FExecutingConstructor then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    alSourcef(FSource, AL_GAIN, Volume.Value * FMuteMultiplicator * FGlobalVolume);
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetALPan;
var
  pa: single;
  p: array[0..1] of single;
const
  PIdiv6 = 0.5235987755982988;  // PI/6
  PIx035 = 1.0995574287564276;  // PI*0.35
begin
  if Error or FParentContext.FExecutingConstructor then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    case ChannelCount of
      1:
      begin
        // https://github.com/kcat/openal-soft/issues/194
        pa := pan.Value{*0.5};
        alSource3f(FSource, AL_POSITION, pa, 0, -Sqrt(1.0 - pa * pa));
      end;
      2: if FParentContext.FHaveExt_AL_EXT_STEREO_ANGLES then
        begin
          p[0] := PIdiv6 - PIx035 * pan.Value;  //left
          p[1] := -PIdiv6 - PIx035 * pan.Value; // right
          alSourcefv(FSource, AL_STEREO_ANGLES, @p[0]);
        end;
    end;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetALPitch;
begin
  if Error or FParentContext.FExecutingConstructor then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    alSourcef(FSource, AL_PITCH, Pitch.Value);
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

function TALSSound.RetrieveAuxSend(const aEffect: TALSEffect;
  out AuxSendIndex: ALint): boolean;
var
  i: integer;
begin
  if Error then
  begin
    Result := False;
    exit;
  end;

  for i := 0 to High(FAuxiliarySend) do
    if FAuxiliarySend[i].IsConnectedWith(aEffect.FSlotID) or
       FAuxiliarySend[i].IsConnectedWithChain(aEffect.FirstEffectInChain) then
    begin
      AuxSendIndex := ALint( i );
      Result := True;
      exit;
    end;
  Result := False;
end;

function TALSSound.GetMute: boolean;
begin
  Result := FMuteMultiplicator = 0.0;
end;

function TALSSound.GetDistanceModel: TALSDistanceModel;
var v: ALint;
begin
  if Error then
    Result := AL_NONE
  else
  begin
    LockContext( FParentContext.FContext );
    try
      alGetSourceiv(FSource, AL_DISTANCE_MODEL, @v);
    finally
      UnlockContext;
    end;
    Result := TALSDistanceModel(v);
  end;
end;

function TALSSound.GetChannelLevel(index: integer): single;
begin
  // overriden in descendant classes
  index := index;
  Result := 0;
end;

function TALSSound.GetChannelLeveldB(index: integer): single;
begin
  Result := LinearTodB( GetChannelLevel(index) );
end;

procedure TALSSound.SetMute(AValue: boolean);
var
  v: single;
begin
  case AValue of
    True: v := 0.0;
    False: v := 1.0;
  end;
  if FMuteMultiplicator <> v then
  begin
    FMuteMultiplicator := v;
    SetALVolume;
  end;
end;

procedure TALSSound.SetPositionRelativeToListener(AValue: boolean);
begin
  if FPositionRelativeToListener=AValue then Exit;
  FPositionRelativeToListener:=AValue;
  if Error then
    exit;

  LockContext( FParentContext.FContext );
  try
    alSourcei( FSource, AL_SOURCE_RELATIVE, ALint(AValue.ToInteger) );
  finally
    UnlockContext;
  end;
end;

function TALSSound.GetResamplerIndex: integer;
var
  v: ALint;
begin
  if Error then
    exit;
  LockContext( FParentContext.FContext );
  try
    alGetSourceiv( FSource, AL_SOURCE_RESAMPLER_SOFT, @v );
    Result := integer(v);
  finally
    UnlockContext;
  end;
end;

procedure TALSSound.SetResamplerIndex(AValue: integer);
begin
  if Error then Exit;

  LockContext( FParentContext.FContext );
  try
    alSourcei( FSource, AL_SOURCE_RESAMPLER_SOFT, ALint(AValue) );
  finally
    UnlockContext;
  end;
end;

procedure TALSSound.SetALTone;
var
  i: Integer;
  bassGain1, trebleGain1: single;
begin
  if Error then Exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    if Tone.Value <= 0.5 then
      bassGain1 := 1.0
    else
      bassGain1 := (1 - (Tone.Value - 0.5) * 2);

    if Tone.Value >= 0.5 then
      trebleGain1 := 1.0
    else
      trebleGain1 := Tone.Value * 2;

    FDirectFilter.LowFreqGain := bassGain1;
    FDirectFilter.HighFreqGain := trebleGain1;

    if FApplyToneOnAuxSend then
      for i:=0 to High(FAuxiliarySend) do
      begin
        FAuxiliarySend[i].LowFreqGain := bassGain1;
        FAuxiliarySend[i].HighFreqGain := trebleGain1;
      end;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetApplyToneOnAuxSend(AValue: boolean);
var
  i: Integer;
begin
  if FApplyToneOnAuxSend=AValue then Exit;
  FApplyToneOnAuxSend := AValue;
  if Error then Exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    for i:=0 to High(FAuxiliarySend) do
      if not AValue then
      begin
        FAuxiliarySend[i].LowFreqGain := 1.0;
        FAuxiliarySend[i].HighFreqGain := 1.0;
      end
      else
      begin
        FAuxiliarySend[i].LowFreqGain := FDirectFilter.LowFreqGain;
        FAuxiliarySend[i].HighFreqGain := FDirectFilter.HighFreqGain;
      end;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetGlobalVolume(AValue: single);
begin
  FGlobalVolume := EnsureRange(AValue, 0.0, 1.0);
  SetALVolume;
end;

procedure TALSSound.EnterCS;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TALSSound.LeaveCS;
begin
  LeaveCriticalSection(FCriticalSection);
end;

procedure TALSSound.Update(const aElapsedTime: single);
var
  v: single;
  flagDoOnStopped: boolean;
begin
  flagDoOnStopped := False;

  EnterCriticalSection(FCriticalSection);
  try
    v := Volume.Value;
    Volume.OnElapse(aElapsedTime);
    if v <> Volume.Value then
      SetALVolume;

    v := Pan.Value;
    Pan.OnElapse(aElapsedTime);
    if v <> Pan.Value then
      SetALPan;

    v := Pitch.Value;
    Pitch.OnElapse(aElapsedTime);
    if v <> Pitch.Value then
      SetALPitch;

    if (Volume.State = alspsNO_CHANGE) and FFadeOutEnabled then
    begin
      FFadeOutEnabled := False;
      Stop;
      FKill := FKill or FKillAfterFadeOut;
    end;

    flagDoOnStopped := (State = ALS_STOPPED) and
                       (FPreviousState <> ALS_STOPPED);
    FPreviousState := State;
  finally
    LeaveCriticalSection(FCriticalSection);;
  end;

  if flagDoOnStopped and (FOnStopped <> NIL) then
      FParentContext.FThread.Synchronize(FParentContext.FThread,
                                         @FParentContext.DoSoundOnStopped);
end;

procedure TALSSound.InternalRewind;
begin
  if not Error then
    alSourceRewind(FSource);
end;

function TALSSound.GetTimePosition: single;
begin
  if Error then
    Result := 0
  else
  begin
    LockContext( FParentContext.FContext );
    EnterCS;
    try
      alGetSourcef(FSource, AL_SEC_OFFSET, Result{%H-});
    finally
      LeaveCS;
      UnlockContext;
    end;
  end;
end;

procedure TALSSound.SetTimePosition(AValue: single);
begin
  if Error then exit;
  LockContext( FParentContext.FContext );
  EnterCS;
  try
    alSourcef(FSource, AL_SEC_OFFSET, AValue);
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

function TALSSound.Byte2Seconds(aPosition: QWord): single;
begin
  if Error then
    Result := 0
  else
    Result := aPosition / FSampleRate;
end;

function TALSSound.Seconds2Byte(aTimePosition: single): QWord;
begin
  if Error then
    Result := 0
  else
    Result := round(aTimePosition * FSampleRate);
end;

function TALSSound.ApplyEffect(const aEffect: TALSEffect): integer;
var
  i: integer;
begin
  Result := -1;
  if Error or not aEffect.Ready then
    exit;

  // if the effect is already assigned on this sound, returns its Aux send index
  if RetrieveAuxSend(aEffect, i) then
  begin
    Result := i;
    exit;
  end;

  // look for an empty send auxiliary channel
  for i := 0 to High(FAuxiliarySend) do
    if not FAuxiliarySend[i].IsConnected and
       not FAuxiliarySend[i].IsConnectedWithAChain then
    begin
      Result := i;
      break;
    end;
  if Result = -1 then
    exit;  // all occupied

  // connect the auxiliary send with the effect slot
  LockContext( FParentContext.FContext );
  EnterCS;
  try
    FAuxiliarySend[Result].ConnectTo(aEffect.FSlotID);

    if aEffect.IsInChain then
      FAuxiliarySend[Result].FFirstChainedEffect := @aEffect
    else
      FAuxiliarySend[Result].FFirstChainedEffect := NIL;

  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.RemoveEffect(const aEffect: TALSEffect);
var
  i: integer;
begin
  if Error or not RetrieveAuxSend(aEffect, i) then
    exit;

  // disable send
  LockContext( FParentContext.FContext );
  EnterCS;
  try
    FAuxiliarySend[i].Disconnect;
    FAuxiliarySend[i].FFirstChainedEffect := NIL;
    if AllAuxiliarySendAreEmpty then
      FDirectFilter.GlobalGain := 1.0;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.RemoveAllEffects;
begin
  if Error then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    RemoveAllALEffects;
    FDirectFilter.GlobalGain := 1.0;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetEffectDryWetVolume(const aEffect: TALSEffect; AValue: single);
var
  dryGain, wetGain: single;
  index: ALint;
begin
  if Error or not aEffect.Ready or not RetrieveAuxSend(aEffect, index) then
    exit;

  AValue := EnsureRange(AValue, ALS_VOLUME_MIN, ALS_VOLUME_MAX);
  if AValue <= 0.5 then
    dryGain := 1.0
  else
    dryGain := (1 - (AValue - 0.5) * 2);
  if AValue >= 0.5 then
    wetGain := 1.0
  else
    wetGain := AValue * 2;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    FAuxiliarySend[index].GlobalGain := wetGain;
    FDirectFilter.GlobalGain := dryGain;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetAuxSendGain(const aEffect: TALSEffect; AValue: single);
var
  sendIndex: ALint;
begin
  if Error or not aEffect.Ready or not RetrieveAuxSend(aEffect, sendIndex) then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    FAuxiliarySend[sendIndex].GlobalGain := AValue;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.SetDryGain(AValue: single);
begin
  if Error or AllAuxiliarySendAreEmpty then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    FDirectFilter.GlobalGain := AValue;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.Attenuation3D(aReferenceDistance, aMaxDistance, aRollOffFactor:
  single; aConeOuterGainHF: single);
begin
  if Error then
    exit;
  LockContext( FParentContext.FContext );
  try
    alSourcef(FSource, AL_MAX_DISTANCE, aMaxDistance);
    alSourcef(FSource, AL_REFERENCE_DISTANCE, aReferenceDistance);
    alSourcef(FSource, AL_ROLLOFF_FACTOR, aRollOffFactor);

    alSourcei(FSource, AL_DIRECT_FILTER_GAINHF_AUTO, AL_TRUE);
    alSourcef(FSource, AL_CONE_OUTER_GAINHF, aConeOuterGainHF);

    // if the sound is not MONO, we force the 'spatialization'
    // https://openal-soft.org/openal-extensions/SOFT_source_spatialize.txt
    if (FChannelCount>1) and FParentContext.FHaveExt_AL_SOFT_source_spatialize then
     alSourcei( FSource, AL_SOURCE_SPATIALIZE_SOFT, AL_TRUE );
  finally
    UnlockContext;
  end;
end;

procedure TALSSound.Position3D(aX, aY, aZ: single);
begin
  if not Error then
  begin
    LockContext( FParentContext.FContext );
    try
      alSource3f( FSource, AL_POSITION, aX, aY, aZ );
    finally
      UnlockContext;
    end;
  end;
end;

function TALSSound.TotalDuration: single;
begin
  if Error then
    Result := 0
  else
    Result := FFrameCount / FSampleRate;
end;

procedure TALSSound.Play(aFromBegin: boolean);
begin
  if Error then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    case GetState of
      ALS_STOPPED:
      begin
        if aFromBegin then
          InternalRewind;
        alSourcePlay(FSource);
        SetALVolume;
      end;
      ALS_PLAYING:
      begin
        if aFromBegin then
        begin
          alSourceStop(FSource);
          InternalRewind;
          alSourcePlay(FSource);
        end;
      end;
      ALS_PAUSED:
      begin
        alSourcePlay(FSource);
      end;
    end;
    FFadeOutEnabled := False;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.Stop;
begin
  if Error then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    alSourceStop(FSource);
    InternalRewind;
    FFadeOutEnabled := False;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.Pause;
begin
  if Error then
    exit;

  case GetState of
    ALS_PAUSED: Play(False);
    ALS_PLAYING: begin
      LockContext( FParentContext.FContext );
      EnterCS;
      try
        alSourcePause(FSource);
      finally
        LeaveCS;
        UnlockContext;
      end;
    end;
  end;
  FFadeOutEnabled := False;
end;

procedure TALSSound.FadeIn(aDuration: single; ACurveID: TALSCurveID);
begin
  FadeIn(ALS_VOLUME_MAX, aDuration, ACurveID);
end;

procedure TALSSound.FadeIn(AVolume: single; aDuration: single; ACurveID: TALSCurveID);
begin
  if Error then
    exit;

  LockContext( FParentContext.FContext );
  EnterCS;
  try
    case GetState of
      ALS_STOPPED:
      begin
        Volume.Value := 0.0;
        Volume.ChangeTo(AVolume, aDuration, ACurveID);
        alSourcePlay(FSource);
      end;
      ALS_PAUSED:
      begin
        Volume.Value := 0.0;
        Volume.ChangeTo(AVolume, aDuration, ACurveID);
        alSourcePlay(FSource);
      end;
      ALS_PLAYING: Volume.ChangeTo(aVolume, aDuration, ACurveID);
    end;
    FFadeOutEnabled := False;
  finally
    LeaveCS;
    UnlockContext;
  end;
end;

procedure TALSSound.FadeOut(aDuration: single; ACurveID: TALSCurveID);
begin
  if Error then
    exit;

  case GetState of
    ALS_STOPPED: ;
    ALS_PLAYING:
    begin
      Volume.ChangeTo(0, aDuration, ACurveID);
      FFadeOutEnabled := True;
    end;
    ALS_PAUSED:
    begin
      Volume.ChangeTo(0, aDuration, ACurveID);
      FFadeOutEnabled := True;
      LockContext( FParentContext.FContext );
      try
        alSourcePlay(FSource);
      finally
        UnlockContext;
      end;
    end;
  end;//case
end;

procedure TALSSound.PlayThenKill(FromBeginning: boolean);
begin
  if not Error then
  begin
    Loop := False;
    FKillAfterPlay := True;
    Play(FromBeginning);
  end
  else
    Kill;
end;

procedure TALSSound.FadeOutThenKill(aDuration: single; aCurveID: TALSCurveID);
begin
  if not Error then
  begin
    if (State = ALS_STOPPED) or (State = ALS_PAUSED) then
      Kill
    else
    begin
      FKillAfterFadeOut := True;
      FadeOut(aDuration, aCurveID);
    end;
  end
  else
    Kill;
end;

procedure TALSSound.Kill;
begin
  FKill := True;
end;

function TALSSound.GetState: TALSState;
var
  v: ALint;
{%H-}begin
  if Error then
    Result := ALS_STOPPED
  else
  begin
    LockContext( FParentContext.FContext );
    try
      alGetSourcei(FSource, AL_SOURCE_STATE, v{%H-});
    finally
      UnlockContext;
    end;
    case v of
      AL_INITIAL, AL_STOPPED: Result := ALS_STOPPED;
      AL_PLAYING: Result := ALS_PLAYING;
      AL_PAUSED: Result := ALS_PAUSED;
    end;//case
  end;
end;

procedure TALSSound.SetDistanceModel(AValue: TALSDistanceModel);
begin
  if not Error and FParentContext.FHaveExt_AL_EXT_source_distance_model then
  begin
    LockContext( FParentContext.FContext );
    try
      alSourcei( FSource, AL_DISTANCE_MODEL, Ord(AValue) );
      alGetError();
    finally
      UnlockContext;
    end;
  end;
end;

initialization
  {$define ALS_INITIALIZATION}
  {$include als_velocity_curve.inc}
  {$undef ALS_INITIALIZATION}

  {$ifdef ALS_ENABLE_CONTEXT_SWITCHING}
  InitCriticalSection( _CSLockContext{%H-} );
  {$endif}
  ALSManager := NIL;
  ALSManager := TALSManager.Create;

finalization
  {$define ALS_FINALIZATION}
  {$include als_velocity_curve.inc}
  {$undef ALS_FINALIZATION}

  ALSManager.Free;
  {$ifdef ALS_ENABLE_CONTEXT_SWITCHING}
  DoneCriticalSection( _CSLockContext );
  {$endif}

end.
