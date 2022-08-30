unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    ComboBox17: TComboBox;
    ComboBox18: TComboBox;
    ComboBox19: TComboBox;
    ComboBox2: TComboBox;
    ComboBox20: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label109: TLabel;
    Label11: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label12: TLabel;
    Label120: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label8: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label9: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageAutoWah: TTabSheet;
    Panel1: TPanel;
    PageChorus: TTabSheet;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    PageFlanger: TTabSheet;
    PageDistortion: TTabSheet;
    PageEcho: TTabSheet;
    PageEqualizer: TTabSheet;
    PageFrequencyShifter: TTabSheet;
    PagePitchShifter: TTabSheet;
    PageRingModulator: TTabSheet;
    PageVocalMorpher: TTabSheet;
    PageReverb: TTabSheet;
    PageCompressor: TTabSheet;
    TB1: TTrackBar;
    TB10: TTrackBar;
    TB11: TTrackBar;
    TB12: TTrackBar;
    TB13: TTrackBar;
    TB14: TTrackBar;
    TB15: TTrackBar;
    TB16: TTrackBar;
    TB17: TTrackBar;
    TB18: TTrackBar;
    TB19: TTrackBar;
    TB2: TTrackBar;
    TB20: TTrackBar;
    TB21: TTrackBar;
    TB22: TTrackBar;
    TB23: TTrackBar;
    TB24: TTrackBar;
    TB25: TTrackBar;
    TB26: TTrackBar;
    TB27: TTrackBar;
    TB28: TTrackBar;
    TB29: TTrackBar;
    TB3: TTrackBar;
    TB30: TTrackBar;
    TB31: TTrackBar;
    TB32: TTrackBar;
    TB33: TTrackBar;
    TB34: TTrackBar;
    TB35: TTrackBar;
    TB36: TTrackBar;
    TB37: TTrackBar;
    TB38: TTrackBar;
    TB39: TTrackBar;
    TB4: TTrackBar;
    TB40: TTrackBar;
    TB41: TTrackBar;
    TB42: TTrackBar;
    TB43: TTrackBar;
    TB44: TTrackBar;
    TB45: TTrackBar;
    TB46: TTrackBar;
    TB47: TTrackBar;
    TB48: TTrackBar;
    TB49: TTrackBar;
    TB5: TTrackBar;
    TB50: TTrackBar;
    TB51: TTrackBar;
    TB52: TTrackBar;
    TB53: TTrackBar;
    TB54: TTrackBar;
    TB55: TTrackBar;
    TB56: TTrackBar;
    TB6: TTrackBar;
    TB7: TTrackBar;
    TB8: TTrackBar;
    TB9: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox10Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TB12Change(Sender: TObject);
    procedure TB17Change(Sender: TObject);
    procedure TB1Change(Sender: TObject);
    procedure TB22Change(Sender: TObject);
    procedure TB27Change(Sender: TObject);
    procedure TB37Change(Sender: TObject);
    procedure TB38Change(Sender: TObject);
    procedure TB40Change(Sender: TObject);
    procedure TB42Change(Sender: TObject);
    procedure TB45Change(Sender: TObject);
    procedure TB5Change(Sender: TObject);
    procedure TB7Change(Sender: TObject);
  private
    FPlaybackContext: TALSPlaybackContext;
    FSound: TALSSound;
  public
    // effect instances
    FAutoWah,
    FChorus,
    FFlanger,
    FDistortion,
    FEcho,
    FEqualizer,
    FFreqShifter,
    FRingModulator,
    FPitchShifter,
    FVocalMorpher,
    FReverb,
    FCompressor: TALSEffect;
    // effect properties
    FAutoWahProp: TALSAutoWahProperties;
    FChorusProp: TALSChorusProperties;
    FFlangerProp: TALSFlangerProperties;
    FDistortionProp: TALSDistortionProperties;
    FEchoProp: TALSEchoProperties;
    FEqualizerProp: TALSEqualizerProperties;
    FFreqShifterProp: TALSFreqShifterProperties;
    FRingModulatorProp: TALSRingModulatorProperties;
    FPitchShifterProp: TALSPitchShifterProperties;
    FVocalMorpherProp: TALSVocalmorpherProperties;
    FReverbProp: TALSReverbProperties;
    FEAXReverbProp: TEAXReverbProperties;
    FCompressorProp: TALSCompressorProperties;

    FSettingAPreset: boolean;
    procedure ApplyEffectOnSound;
    procedure SetTBPosition(aTB: TTrackBar; aMin, aMax, aValue: single);
    procedure UpdateCaptionAutoWah;
    procedure UpdateCaptionChorus;
    procedure UpdateCaptionFlanger;
    procedure UpdateCaptionDistortion;
    procedure UpdateCaptionEcho;
    procedure UpdateCaptionEqualizer;
    procedure UpdateCaptionFreqShifter;
    procedure UpdateCaptionPitchShifter;
    procedure UpdateCaptionRingModulator;
    procedure UpdateCaptionVocalMorpher;
    procedure UpdateCaptionReverb;
  end;

var
  Form1: TForm1;

implementation
uses Clipbrd;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  DefaultFormatSettings.decimalSeparator := '.';

  // Updates the file open dialog filter property with supported audio file types.
  OpenDialog1.Filter := ALSManager.DialogFileFilters(True);

  // Create a playback context on the default playback device
  FPlaybackContext := ALSManager.CreateDefaultPlaybackContext;

  // Creates all effects and initialize values to default
  FAutoWahProp.InitDefault;
  FAutoWah := FPlaybackContext.CreateEffect( AL_EFFECT_AUTOWAH, FAutoWahProp);

  FChorusProp.InitDefault;
  FChorus := FPlaybackContext.CreateEffect( AL_EFFECT_CHORUS, FChorusProp);

  FFlangerProp.InitDefault;
  FFlanger := FPlaybackContext.CreateEffect( AL_EFFECT_FLANGER, FFlangerProp);

  FDistortionProp.InitDefault;
  FDistortion := FPlaybackContext.CreateEffect( AL_EFFECT_DISTORTION, FDistortionProp);

  FEchoProp.InitDefault;
  FEcho := FPlaybackContext.CreateEffect( AL_EFFECT_ECHO, FEchoProp);

  FFreqShifterProp.InitDefault;
  FFreqShifter := FPlaybackContext.CreateEffect( AL_EFFECT_FREQUENCYSHIFTER, FFreqShifterProp);

  FRingModulatorProp.InitDefault;
  FRingModulator := FPlaybackContext.CreateEffect( AL_EFFECT_RINGMODULATOR, FRingModulatorProp);

  FPitchShifterProp.InitDefault;
  FPitchShifter := FPlaybackContext.CreateEffect( AL_EFFECT_PITCHSHIFTER, FPitchShifterProp);

  FVocalMorpherProp.InitDefault;
  FVocalMorpher := FPlaybackContext.CreateEffect( AL_EFFECT_VOCALMORPHER, FVocalMorpherProp);

  FEqualizerProp.InitDefault;
  FEqualizer := FPlaybackContext.CreateEffect( AL_EFFECT_EQUALIZER, FEqualizerProp);

  FReverbProp.InitDefault;
  FReverb := FPlaybackContext.CreateEffect( AL_EFFECT_REVERB, FReverbProp);

  FCompressorProp.InitDefault;
  FCompressor := FPlaybackContext.CreateEffect( AL_EFFECT_COMPRESSOR, FCompressorProp);

  // Because Linux fonts are higher than Windows
  {$ifdef LINUX}
  ComboBox10.Font.Height:=9;
  ComboBox11.Font.Height:=9;
  ComboBox12.Font.Height:=9;
  ComboBox13.Font.Height:=9;
  ComboBox14.Font.Height:=9;
  ComboBox15.Font.Height:=9;
  ComboBox16.Font.Height:=9;
  ComboBox17.Font.Height:=9;
  ComboBox18.Font.Height:=9;
  ComboBox19.Font.Height:=9;
  ComboBox20.Font.Height:=9;
  {$endif}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free all effects
  FPlaybackContext.DeleteEffect( FAutoWah );
  FPlaybackContext.DeleteEffect( FChorus );
  FPlaybackContext.DeleteEffect( FFlanger );
  FPlaybackContext.DeleteEffect( FDistortion );
  FPlaybackContext.DeleteEffect( FEcho );
  FPlaybackContext.DeleteEffect( FEqualizer );
  FPlaybackContext.DeleteEffect( FFreqShifter );
  FPlaybackContext.DeleteEffect( FRingModulator );
  FPlaybackContext.DeleteEffect( FPitchShifter );
  FPlaybackContext.DeleteEffect( FVocalMorpher );
  FPlaybackContext.DeleteEffect( FReverb );
  FPlaybackContext.DeleteEffect( FCompressor );
  // Free our playback context
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if FPlaybackContext.Error then
    ShowMessage(FPlaybackContext.StrError);

  // fill combobox Vocal Morpher with phoneme list
  ComboBox6.Items.AddStrings(FVocalMorpherProp.PhonemeList, True);
  ComboBox6.ItemIndex := 0;
  ComboBox7.Items.AddStrings(FVocalMorpherProp.PhonemeList, True);
  ComboBox7.ItemIndex := 10;

  ComboBox10.Items.AddStrings(FAutoWahProp.PresetList);
  ComboBox10.ItemIndex := 0;
  ComboBox11.Items.AddStrings(FChorusProp.PresetList);
  ComboBox11.ItemIndex := 0;
  ComboBox12.Items.AddStrings(FFlangerProp.PresetList);
  ComboBox12.ItemIndex := 0;
  ComboBox13.Items.AddStrings(FDistortionProp.PresetList);
  ComboBox13.ItemIndex := 0;
  ComboBox14.Items.AddStrings(FEchoProp.PresetList);
  ComboBox14.ItemIndex := 0;
  ComboBox15.Items.AddStrings(FEqualizerProp.PresetList);
  ComboBox15.ItemIndex := 0;
  ComboBox16.Items.AddStrings(FFreqShifterProp.PresetList);
  ComboBox16.ItemIndex := 0;
  ComboBox17.Items.AddStrings(FPitchShifterProp.PresetList);
  ComboBox17.ItemIndex := 0;
  ComboBox18.Items.AddStrings(FRingModulatorProp.PresetList);
  ComboBox18.ItemIndex := 0;
  ComboBox19.Items.AddStrings(FVocalMorpherProp.PresetList);
  ComboBox19.ItemIndex := 0;
  ComboBox20.Items.AddStrings(FEAXReverbProp.PresetList);//(FReverbProp.PresetList);
  ComboBox20.ItemIndex := 0;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if FSound<>NIL then
    FSound.RemoveAllEffects;
  ApplyEffectOnSound;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if FSound<>NIL then
    FSound.Play;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if FSound<>NIL then
    FSound.Pause;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;

  Label1.Caption := ExtractFileName(OpenDialog1.FileName);

  // kill the previous sound
  if FSound<>NIL then
    FSound.FadeOutThenKill( 2.0 );

  // create the new one, enable loop mode and playback with smooth fadein
  FSound := FPlaybackContext.AddStream( OpenDialog1.FileName );
  FSound.Loop := True;
  FSound.FadeIn( 1.0 );

  ApplyEffectOnSound;
end;

procedure TForm1.TB1Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FAutoWahProp.AttackTime := 0.0001+(1.0-0.0001)*TB1.Position/TB1.Max;
  FAutoWahProp.ReleaseTime := 0.0001+(1.0-0.0001)*TB2.Position/TB2.Max;
  FAutoWahProp.Resonance := 2.0+(1000.0-2.0)*TB3.Position/TB3.Max;
  FAutoWahProp.PeakGain := 0.00003+(31621-0.00003)*TB4.Position/TB4.Max;

  UpdateCaptionAutoWah;

  FAutoWah.UpdateParameters( FAutoWahProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB7Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FChorusProp.Waveform := TALSChorusWaveform(ComboBox1.ItemIndex);
  FChorusProp.Phase := Round(-180+(180--180)*TB7.Position/TB7.Max);
  FChorusProp.Rate := 10.0*TB8.Position/TB8.Max;
  FChorusProp.Depth := TB9.Position/TB9.Max;
  FChorusProp.Feedback := -1.0+2*TB10.Position/TB10.Max;
  FChorusProp.Delay := 0.016*TB11.Position/TB11.Max;

  UpdateCaptionChorus;

  FChorus.UpdateParameters( FChorusProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB12Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FFlangerProp.Waveform := TALSFlangerWaveform(ComboBox2.ItemIndex);
  FFlangerProp.Phase := Round(-180+(180--180)*TB12.Position/TB12.Max);
  FFlangerProp.Rate := 10.0*TB13.Position/TB13.Max;
  FFlangerProp.Depth := TB14.Position/TB14.Max;
  FFlangerProp.Feedback := -1.0+2*TB15.Position/TB15.Max;
  FFlangerProp.Delay := 0.016*TB16.Position/TB16.Max;

  UpdateCaptionFlanger;

  FFlanger.UpdateParameters( FFlangerProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB17Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FDistortionProp.Edge := TB17.Position/TB17.Max;
  FDistortionProp.Gain := 0.01+(1.0-0.01)*TB18.Position/TB18.Max;
  FDistortionProp.LowpassCutoff := 80.0+(24000.0-80.0)*TB19.Position/TB19.Max;
  FDistortionProp.EQCenter := 80.0+(24000.0-80.0)*TB20.Position/TB20.Max;
  FDistortionProp.EQBandwidth := 80.0+(24000.0-80.0)*TB21.Position/TB21.Max;

  UpdateCaptionDistortion;

  FDistortion.UpdateParameters( FDistortionProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB22Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FEchoProp.Delay := 0.207*TB22.Position/TB22.Max;
  FEchoProp.LRDelay := 0.404*TB23.Position/TB23.Max;
  FEchoProp.Damping := 0.99*TB24.Position/TB24.Max;
  FEchoProp.Feedback := TB25.Position/TB25.Max;
  FEchoProp.Spread := -1.0+(1.0--1.0)*TB26.Position/TB26.Max;

  UpdateCaptionEcho;

  FEcho.UpdateParameters( FEchoProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB27Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FEqualizerProp.LowGain := 0.126+(7.943-0.126)*TB27.Position/TB27.Max;
  FEqualizerProp.LowCutoff := 50.0+(800.0-50.0)*TB28.Position/TB28.Max;
  FEqualizerProp.Mid1Gain := 0.126+(7.943-0.126)*TB29.Position/TB29.Max;
  FEqualizerProp.Mid1Center := 200.0+(3000.0-200.0)*TB30.Position/TB30.Max;
  FEqualizerProp.Mid1Width := 0.01+(1.0--0.01)*TB31.Position/TB31.Max;
  FEqualizerProp.Mid2Gain := 0.126+(7.943-0.126)*TB32.Position/TB32.Max;
  FEqualizerProp.Mid2Center := 1000.0+(8000.0-1000.0)*TB33.Position/TB33.Max;
  FEqualizerProp.Mid2Width := 0.01+(1.0--0.01)*TB34.Position/TB34.Max;
  FEqualizerProp.HighGain := 0.126+(7.943-0.126)*TB35.Position/TB35.Max;
  FEqualizerProp.HighCutoff := 4000.0+(16000.0-4000.0)*TB36.Position/TB36.Max;

  UpdateCaptionEqualizer;

  FEqualizer.UpdateParameters( FEqualizerProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB37Change(Sender: TObject);
var
  f: single;
begin
  if FSettingAPreset then exit;
  if RadioButton1.Checked then
    f := 500.0
  else if RadioButton2.Checked then
    f := 1000.0
  else f := 24000.0;
  FFreqShifterProp.Frequency := f*TB37.Position/TB37.Max;
  FFreqShifterProp.LeftDirection := TALSFreqShifterDirection(ComboBox3.ItemIndex);
  FFreqShifterProp.RightDirection := TALSFreqShifterDirection(ComboBox4.ItemIndex);

  UpdateCaptionFreqShifter;

  FFreqShifter.UpdateParameters( FFreqShifterProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB38Change(Sender: TObject);
begin
  FPitchShifterProp.CoarseTune := Round(-12 + 24*TB38.Position/TB38.Max);
  FPitchShifterProp.FineTune := Round(-50 + 100*TB39.Position/TB39.Max);

  UpdateCaptionPitchShifter;

  FPitchShifter.UpdateParameters( FPitchShifterProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB40Change(Sender: TObject);
var
  f: single;
begin
  if FSettingAPreset then exit;
  if RadioButton4.Checked then
    f := 100.0
  else if RadioButton5.Checked then
    f := 1000.0
  else f := 8000.0;
  FRingModulatorProp.Frequency := f*TB40.Position/TB40.Max;
  FRingModulatorProp.HighPassCutoff := 24000.0*TB41.Position/TB41.Max;
  FRingModulatorProp.Waveform := TALSRingModulatorWaveform( ComboBox5.ItemIndex );

  UpdateCaptionRingModulator;

  FRingModulator.UpdateParameters( FRingModulatorProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB42Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FVocalMorpherProp.PhonemeA := TALSVMorpherPhoneme(ComboBox6.ItemIndex);
  FVocalMorpherProp.PhonemeB := TALSVMorpherPhoneme(ComboBox7.ItemIndex);
  FVocalMorpherProp.PhonemeACoarseTuning := TB42.Position;
  FVocalMorpherProp.PhonemeBCoarseTuning := TB43.Position;
  FVocalMorpherProp.Waveform := TALSVMorpherWaveform(ComboBox8.ItemIndex);
  FVocalMorpherProp.Rate := 10.0*TB44.Position/TB44.Max;

  UpdateCaptionVocalMorpher;

  FVocalMorpher.UpdateParameters( FVocalMorpherProp );
  ApplyEffectOnSound;
end;

procedure TForm1.TB45Change(Sender: TObject);
begin
  if FSettingAPreset then exit;
  FReverbProp.Density := TB45.Position/TB45.Max;
  FReverbProp.Diffusion := TB46.Position/TB46.Max;
  FReverbProp.Gain := TB47.Position/TB47.Max;
  FReverbProp.GainHF := TB48.Position/TB48.Max;
  FReverbProp.DecayTime := 0.1+(20.0-0.1)*TB49.Position/TB49.Max;
  FReverbProp.DecayHFRatio := 0.1+(2.0-0.1)*TB50.Position/TB50.Max;
  FReverbProp.ReflectionsGain := 3.16*TB51.Position/TB51.Max;
  FReverbProp.ReflectionsDelay := 0.3*TB52.Position/TB52.Max;
  FReverbProp.LateReverbGain := 10.0*TB53.Position/TB53.Max;
  FReverbProp.LateReverbDelay := 0.1*TB54.Position/TB54.Max;
  FReverbProp.RoomRolloffFactor := 10.0*TB55.Position/TB55.Max;
  FReverbProp.AirAbsorptionGainHF := 0.892+(1.0-0.892)*TB56.Position/TB56.Max;
  FReverbProp.DecayHFLimit := ComboBox9.ItemIndex;

  UpdateCaptionReverb;

  FReverb.UpdateParameters( FReverbProp );
  ApplyEffectOnSound;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  Label11.Enabled:=CheckBox1.Checked;
  Label10.Enabled:=CheckBox1.Checked;
  TB5.Enabled:=CheckBox1.Checked;
  ApplyEffectOnSound;
end;

procedure TForm1.ComboBox10Select(Sender: TObject);
var
  f: integer;
begin
  FSettingAPreset := True;
  if Sender = ComboBox10 then
  begin
    FAutoWahProp.InitWithPreset(ComboBox10.ItemIndex);
    FAutoWah.UpdateParameters(FAutoWahProp);
    SetTBPosition(TB1, 0.0001, 1.0, FAutoWahProp.AttackTime);
    SetTBPosition(TB2, 0.0001, 1.0, FAutoWahProp.ReleaseTime);
    SetTBPosition(TB3, 2.0, 1000.0, FAutoWahProp.Resonance);
    SetTBPosition(TB4, 0.00003, 31621.0, FAutoWahProp.PeakGain);
    UpdateCaptionAutoWah;
  end
  else if Sender = ComboBox11 then
  begin
    FChorusProp.InitWithPreset(ComboBox11.ItemIndex);
    FChorus.UpdateParameters(FChorusProp);
    ComboBox1.ItemIndex := Ord(FChorusProp.Waveform);
    SetTBPosition(TB7, -180, 180, FChorusProp.Phase);
    SetTBPosition(TB8, 0.0, 10.0, FChorusProp.Rate);
    SetTBPosition(TB9, 0.0, 1.0, FChorusProp.Depth);
    SetTBPosition(TB10, -1.0, 1.0, FChorusProp.Feedback);
    SetTBPosition(TB11, 0.0, 0.016, FChorusProp.Delay);
    UpdateCaptionChorus;
  end
  else if Sender = ComboBox12 then
  begin
    FFlangerProp.InitWithPreset(ComboBox12.ItemIndex);
    FFlanger.UpdateParameters(FFlangerProp);
    ComboBox2.ItemIndex := Ord(FFlangerProp.Waveform);
    SetTBPosition(TB12, -180, 180, FFlangerProp.Phase);
    SetTBPosition(TB13, 0.0, 10.0, FFlangerProp.Rate);
    SetTBPosition(TB14, 0.0, 1.0, FFlangerProp.Depth);
    SetTBPosition(TB15, -1.0, 1.0, FFlangerProp.Feedback);
    SetTBPosition(TB16, 0.0, 0.004, FFlangerProp.Delay);
    UpdateCaptionFlanger;
  end
  else if Sender = ComboBox13 then
  begin
    FDistortionProp.InitWithPreset(ComboBox13.ItemIndex);
    FDistortion.UpdateParameters(FDistortionProp);
    SetTBPosition(TB17, 0.0, 1.0, FDistortionProp.Edge);
    SetTBPosition(TB18, 0.01, 1.0, FDistortionProp.Gain);
    SetTBPosition(TB19, 80.0, 24000.0, FDistortionProp.LowpassCutoff);
    SetTBPosition(TB20, 80.0, 24000.0, FDistortionProp.EQCenter);
    SetTBPosition(TB21, 80.0, 24000.0, FDistortionProp.EQBandwidth);
    UpdateCaptionDistortion;
  end
  else if Sender = ComboBox14 then
  begin
    FEchoProp.InitWithPreset(ComboBox14.ItemIndex);
    FEcho.UpdateParameters(FEchoProp);
    SetTBPosition(TB22, 0.0, 0.207, FEchoProp.Delay);
    SetTBPosition(TB23, 0.0, 0.404, FEchoProp.LRDelay);
    SetTBPosition(TB24, 0.0, 0.99, FEchoProp.Damping);
    SetTBPosition(TB25, 0.0, 1.0, FEchoProp.Feedback);
    SetTBPosition(TB26, -1.0, 1.0, FEchoProp.Spread);
    UpdateCaptionEcho;
  end
  else if Sender = ComboBox15 then
  begin
    FEqualizerProp.InitWithPreset(ComboBox15.ItemIndex);
    FEqualizer.UpdateParameters(FEqualizerProp);
    SetTBPosition(TB27, 0.126, 7.943, FEqualizerProp.LowGain);
    SetTBPosition(TB28, 50.0, 800.0, FEqualizerProp.LowCutoff);
    SetTBPosition(TB29, 0.126, 7.943, FEqualizerProp.Mid1Gain);
    SetTBPosition(TB30, 200.0, 3000.0, FEqualizerProp.Mid1Center);
    SetTBPosition(TB31, 0.01, 1.0, FEqualizerProp.Mid1Width);
    SetTBPosition(TB32, 0.126, 7.943, FEqualizerProp.Mid2Gain);
    SetTBPosition(TB33, 1000.0, 8000.0, FEqualizerProp.Mid2Center);
    SetTBPosition(TB34, 0.01, 1.0, FEqualizerProp.Mid2Width);
    SetTBPosition(TB35, 0.126, 7.943, FEqualizerProp.HighGain);
    SetTBPosition(TB36, 4000.0, 16000.0, FEqualizerProp.HighCutoff);
    UpdateCaptionEqualizer;
  end
  //Freq shifter
  else if Sender = ComboBox16 then
  begin
    FFreqShifterProp.InitWithPreset(ComboBox16.ItemIndex);
    FFreqShifter.UpdateParameters(FFreqShifterProp);
    if FFreqShifterProp.Frequency <= 500 then
    begin
      RadioButton1.Checked := True;
      f := 500;
    end
    else if FFreqShifterProp.Frequency <= 1000 then
    begin
      RadioButton2.Checked := True;
      f := 1000;
    end
    else
    begin
      RadioButton3.Checked := True;
      f := 24000;
    end;
    SetTBPosition(TB37, 0, f, FFreqShifterProp.Frequency);
    ComboBox3.ItemIndex := Ord(FFreqShifterProp.LeftDirection);
    ComboBox4.ItemIndex := Ord(FFreqShifterProp.RightDirection);
    UpdateCaptionFreqShifter;
  end
  // Pitch shifter
  else if Sender = ComboBox17 then
  begin
    FPitchShifterProp.InitWithPreset(ComboBox17.ItemIndex);
    FPitchShifter.UpdateParameters(FPitchShifterProp);
    SetTBPosition(TB38, -12, 12, FPitchShifterProp.CoarseTune);
    SetTBPosition(TB39, -50, 50, FPitchShifterProp.FineTune);
    UpdateCaptionPitchShifter;
  end
  else if Sender = ComboBox18 then
  begin
    FRingModulatorProp.InitWithPreset(ComboBox18.ItemIndex);
    FRingModulator.UpdateParameters(FRingModulatorProp);
    ComboBox5.ItemIndex := Ord(FRingModulatorProp.Waveform);
    if FRingModulatorProp.Frequency <= 100 then
    begin
      RadioButton4.Checked := True;
      f := 100;
    end
    else if FRingModulatorProp.Frequency <= 1000 then
    begin
      RadioButton5.Checked := True;
      f := 1000;
    end
    else
    begin
      RadioButton6.Checked := True;
      f := 8000;
    end;
    SetTBPosition(TB40, 0.0, f, FRingModulatorProp.Frequency);
    SetTBPosition(TB41, 0.0, 24000.0, FRingModulatorProp.HighPassCutoff);
    UpdateCaptionRingModulator;
  end
  else if Sender = ComboBox19 then
  begin
    FVocalMorpherProp.InitWithPreset(ComboBox19.ItemIndex);
    FVocalMorpher.UpdateParameters(FVocalMorpherProp);
    ComboBox6.ItemIndex := Ord(FVocalMorpherProp.PhonemeA);
    ComboBox7.ItemIndex := Ord(FVocalMorpherProp.PhonemeB);
    ComboBox8.ItemIndex := Ord(FVocalMorpherProp.Waveform);
    SetTBPosition(TB42, -24, 24, FVocalMorpherProp.PhonemeACoarseTuning);
    SetTBPosition(TB43, -24, 24, FVocalMorpherProp.PhonemeBCoarseTuning);
    SetTBPosition(TB44, 0.0, 10.0, FVocalMorpherProp.Rate);
    UpdateCaptionVocalMorpher;
  end
  else if Sender = ComboBox20 then
  begin
    FReverbProp.InitWithEAXPreset(ComboBox20.ItemIndex);
    //FReverbProp.InitWithPreset(ComboBox20.ItemIndex);
    FReverb.UpdateParameters(FReverbProp);
    SetTBPosition(TB45, 0.0, 1.0, FReverbProp.Density);
    SetTBPosition(TB46, 0.0, 1.0, FReverbProp.Diffusion);
    SetTBPosition(TB47, 0.0, 1.0, FReverbProp.Gain);
    SetTBPosition(TB48, 0.0, 1.0, FReverbProp.GainHF);
    SetTBPosition(TB49, 0.1, 20.0, FReverbProp.DecayTime);
    SetTBPosition(TB50, 0.1, 2.0, FReverbProp.DecayHFRatio);
    SetTBPosition(TB51, 0.0, 3.16, FReverbProp.ReflectionsGain);
    SetTBPosition(TB52, 0.0, 0.3, FReverbProp.ReflectionsDelay);
    SetTBPosition(TB53, 0.0, 10.0, FReverbProp.LateReverbGain);
    SetTBPosition(TB54, 0.0, 0.1, FReverbProp.LateReverbDelay);
    SetTBPosition(TB55, 0.892, 1.0, FReverbProp.AirAbsorptionGainHF);
    SetTBPosition(TB56, 0.0, 10.0, FReverbProp.RoomRolloffFactor);
    ComboBox9.ItemIndex := FReverbProp.DecayHFLimit;
    UpdateCaptionReverb;
  end;
  FSettingAPreset := False;
end;

procedure TForm1.TB5Change(Sender: TObject);
begin
  if FSound = NIL then exit;

  if PageControl1.ActivePage = PageAutoWah then
    FSound.SetAuxSendGain(FAutoWah, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageChorus then
    FSound.SetAuxSendGain(FChorus, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageFlanger then
    FSound.SetAuxSendGain(FFlanger, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageDistortion then
    FSound.SetAuxSendGain(FDistortion, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageEcho then
    FSound.SetAuxSendGain(FEcho, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageEqualizer then
    FSound.SetAuxSendGain(FEqualizer, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageFrequencyShifter then
    FSound.SetAuxSendGain(FFreqShifter, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PagePitchShifter then
    FSound.SetAuxSendGain(FPitchShifter, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageRingModulator then
    FSound.SetAuxSendGain(FRingModulator, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageVocalMorpher then
    FSound.SetAuxSendGain(FVocalMorpher, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageReverb then
    FSound.SetAuxSendGain(FReverb, TB5.Position/TB5.Max);

  if PageControl1.ActivePage = PageCompressor then
    FSound.SetAuxSendGain(FCompressor, TB5.Position/TB5.Max);

  FSound.SetDryGain( TB6.Position/TB6.Max );
end;

procedure TForm1.ApplyEffectOnSound;
begin
  if FSound=NIL then exit;

  if not CheckBox1.Checked then
  begin
    FSound.RemoveAllEffects;
    exit;
  end;

  if PageControl1.ActivePage = PageAutoWah then
    FSound.ApplyEffect( FAutoWah );

  if PageControl1.ActivePage = PageChorus then
    FSound.ApplyEffect( FChorus );

  if PageControl1.ActivePage = PageFlanger then
    FSound.ApplyEffect( FFlanger );

  if PageControl1.ActivePage = PageDistortion then
    FSound.ApplyEffect( FDistortion );

  if PageControl1.ActivePage = PageEcho then
    FSound.ApplyEffect( FEcho );

  if PageControl1.ActivePage = PageEqualizer then
    FSound.ApplyEffect( FEqualizer );

  if PageControl1.ActivePage = PageFrequencyShifter then
    FSound.ApplyEffect( FFreqShifter );

  if PageControl1.ActivePage = PagePitchShifter then
    FSound.ApplyEffect( FPitchShifter );

  if PageControl1.ActivePage = PageRingModulator then
    FSound.ApplyEffect( FRingModulator );

  if PageControl1.ActivePage = PageVocalMorpher then
    FSound.ApplyEffect( FVocalMorpher );

  if PageControl1.ActivePage = PageReverb then
    FSound.ApplyEffect( FReverb );

  if PageControl1.ActivePage = PageCompressor then
    FSound.ApplyEffect( FCompressor );
end;

procedure TForm1.SetTBPosition(aTB: TTrackBar; aMin, aMax, aValue: single);
begin
  {  prop := min+(max-min)*tbpos/tbmax
    =>prop-min := (max-min)*tbpos/tbmax
    =>(prop-min)*tbmax := (max-min)*tbpos
    =>(prop-min)*tbmax/(max-min) = tbpos  }
  aTB.Position := Round((aValue-aMin)*aTB.Max/(aMax-aMin));
end;

procedure TForm1.UpdateCaptionAutoWah;
begin
  Label3.Caption := FormatFloat('0.0000', FAutoWahProp.AttackTime);
  Label5.Caption := FormatFloat('0.0000', FAutoWahProp.ReleaseTime);
  Label7.Caption := FormatFloat('0.0000', FAutoWahProp.Resonance);
  Label9.Caption := FormatFloat('0.00000', FAutoWahProp.PeakGain);
end;

procedure TForm1.UpdateCaptionChorus;
begin
  Label15.Caption := FChorusProp.Phase.ToString;
  Label17.Caption := FormatFloat('0.0000', FChorusProp.Rate);
  Label19.Caption := FormatFloat('0.0000', FChorusProp.Depth);
  Label21.Caption := FormatFloat('0.00000', FChorusProp.Feedback);
  Label23.Caption := FormatFloat('0.00000', FChorusProp.Delay);
end;

procedure TForm1.UpdateCaptionFlanger;
begin
  Label25.Caption := FFlangerProp.Phase.ToString;
  Label27.Caption := FormatFloat('0.0000', FFlangerProp.Rate);
  Label29.Caption := FormatFloat('0.0000', FFlangerProp.Depth);
  Label31.Caption := FormatFloat('0.00000', FFlangerProp.Feedback);
  Label33.Caption := FormatFloat('0.00000', FFlangerProp.Delay);
end;

procedure TForm1.UpdateCaptionDistortion;
begin
  Label35.Caption := FormatFloat('0.00000', FDistortionProp.Edge);
  Label37.Caption := FormatFloat('0.00000', FDistortionProp.Gain);
  Label39.Caption := FormatFloat('0.00', FDistortionProp.LowpassCutoff);
  Label41.Caption := FormatFloat('0.00', FDistortionProp.EQCenter);
  Label43.Caption := FormatFloat('0.00', FDistortionProp.EQBandwidth);
end;

procedure TForm1.UpdateCaptionEcho;
begin
  Label45.Caption := FormatFloat('0.0000', FEchoProp.Delay);
  Label47.Caption := FormatFloat('0.0000', FEchoProp.LRDelay);
  Label49.Caption := FormatFloat('0.0000', FEchoProp.Damping);
  Label51.Caption := FormatFloat('0.0000', FEchoProp.Feedback);
  Label53.Caption := FormatFloat('0.0000', FEchoProp.Spread);
end;

procedure TForm1.UpdateCaptionEqualizer;
begin
  Label55.Caption := FormatFloat('0.0000', FEqualizerProp.LowGain);
  Label57.Caption := FormatFloat('0.0', FEqualizerProp.LowCutoff);
  Label59.Caption := FormatFloat('0.0000', FEqualizerProp.Mid1Gain);
  Label61.Caption := FormatFloat('0.0', FEqualizerProp.Mid1Center);
  Label63.Caption := FormatFloat('0.0000', FEqualizerProp.Mid1Width);
  Label65.Caption := FormatFloat('0.0000', FEqualizerProp.Mid2Gain);
  Label67.Caption := FormatFloat('0.0', FEqualizerProp.Mid2Center);
  Label69.Caption := FormatFloat('0.0000', FEqualizerProp.Mid2Width);
  Label71.Caption := FormatFloat('0.0000', FEqualizerProp.HighGain);
  Label73.Caption := FormatFloat('0.0', FEqualizerProp.HighCutoff);
end;

procedure TForm1.UpdateCaptionFreqShifter;
begin
  Label75.Caption := FormatFloat('0.0', FFreqShifterProp.Frequency);
end;

procedure TForm1.UpdateCaptionPitchShifter;
begin
  Label79.Caption := FPitchShifterProp.CoarseTune.ToString;
  Label81.Caption := FPitchShifterProp.FineTune.ToString;
end;

procedure TForm1.UpdateCaptionRingModulator;
begin
  Label83.Caption := FormatFloat('0.0', FRingModulatorProp.Frequency);
  Label85.Caption := FormatFloat('0.0', FRingModulatorProp.HighPassCutoff);
end;

procedure TForm1.UpdateCaptionVocalMorpher;
begin
  Label93.Caption := FVocalMorpherProp.PhonemeACoarseTuning.ToString;
  Label95.Caption := FVocalMorpherProp.PhonemeBCoarseTuning.ToString;
  Label88.Caption := FormatFloat('0.000', FVocalMorpherProp.Rate);
end;

procedure TForm1.UpdateCaptionReverb;
begin
  Label97.Caption := FormatFloat('0.0000', FReverbProp.Density);
  Label99.Caption := FormatFloat('0.0000', FReverbProp.Diffusion);
  Label101.Caption := FormatFloat('0.0000', FReverbProp.Gain);
  Label103.Caption := FormatFloat('0.0000', FReverbProp.GainHF);
  Label105.Caption := FormatFloat('0.0000', FReverbProp.DecayTime);
  Label109.Caption := FormatFloat('0.0000', FReverbProp.DecayHFRatio);
  Label111.Caption := FormatFloat('0.0000', FReverbProp.ReflectionsGain);
  Label113.Caption := FormatFloat('0.0000', FReverbProp.ReflectionsDelay);
  Label116.Caption := FormatFloat('0.0000', FReverbProp.LateReverbGain);
  Label118.Caption := FormatFloat('0.0000', FReverbProp.LateReverbDelay);
  Label120.Caption := FormatFloat('0.0000', FReverbProp.RoomRolloffFactor);
  Label122.Caption := FormatFloat('0.0000', FReverbProp.AirAbsorptionGainHF);
end;

procedure TForm1.Button2Click(Sender: TObject);
var txt, n, t: string;
  function IndexToFreqShifterDir( index: integer ): string;
  begin
    case index of
      0: Result := 'sdDown';
      1: Result := 'sdUp';
      3: Result := 'sdOff';
    end;
  end;

begin
  n := 'PRESET_NAME';
  if not InputQuery('New preset', 'Enter the name of the preset:', n) then
    exit;
  txt := '';

  if PageControl1.ActivePage = PageAutoWah then
  begin
    txt := '  AUTOWAH_PRESET_'+n.ToUpper+': TALSAutoWahProperties='+LINEENDING+
           '    ( AttackTime: '+Label3.Caption+';'+LINEENDING+
           '      ReleaseTime: '+Label5.Caption+';'+LINEENDING+
           '      Resonance: '+Label7.Caption+';'+LINEENDING+
           '      PeakGain: '+Label9.Caption+' );';
  end;

  if PageControl1.ActivePage = PageChorus then
  begin
    case ComboBox1.ItemIndex of
      0: t := 'cwSinusoid';
      1: t := 'cwTriangle';
    end;
    txt := '  CHORUS_PRESET_'+n.ToUpper+': TALSChorusProperties='+LINEENDING+
           '    ( Waveform: '+t+';'+LINEENDING+
           '      Phase: '+Label15.Caption+';'+LINEENDING+
           '      Rate: '+Label17.Caption+';'+LINEENDING+
           '      Depth: '+Label19.Caption+';'+LINEENDING+
           '      Feedback: '+Label21.Caption+';'+LINEENDING+
           '      Delay: '+Label23.Caption+' );';
  end;

  if PageControl1.ActivePage = PageFlanger then
  begin
    case ComboBox2.ItemIndex of
      0: t := 'fwSinusoid';
      1: t := 'fwTriangle';
    end;
    txt := '  FLANGER_PRESET_'+n.ToUpper+': TALSFlangerProperties='+LINEENDING+
           '    ( Waveform: '+t+';'+LINEENDING+
           '      Phase: '+Label25.Caption+';'+LINEENDING+
           '      Rate: '+Label27.Caption+';'+LINEENDING+
           '      Depth: '+Label29.Caption+';'+LINEENDING+
           '      Feedback: '+Label31.Caption+';'+LINEENDING+
           '      Delay: '+Label33.Caption+' );';
  end;

  if PageControl1.ActivePage = PageDistortion then
  begin
    txt := '  DISTORTION_PRESET_'+n.ToUpper+': TALSDistortionProperties='+LINEENDING+
           '    ( Edge: '+Label35.Caption+';'+LINEENDING+
           '      Gain: '+Label37.Caption+';'+LINEENDING+
           '      LowpassCutoff: '+Label39.Caption+';'+LINEENDING+
           '      EQCenter: '+Label41.Caption+';'+LINEENDING+
           '      EQBandwidth: '+Label43.Caption+' );';
  end;

  if PageControl1.ActivePage = PageEcho then
  begin
    txt := '  ECHO_PRESET_'+n.ToUpper+': TALSEchoProperties='+LINEENDING+
           '    ( Delay: '+Label45.Caption+';'+LINEENDING+
           '      LRDelay: '+Label47.Caption+';'+LINEENDING+
           '      Damping: '+Label49.Caption+';'+LINEENDING+
           '      Feedback: '+Label51.Caption+';'+LINEENDING+
           '      Spread: '+Label53.Caption+' );';
  end;

  if PageControl1.ActivePage = PageEqualizer then
  begin
    txt := '  EQUALIZER_PRESET_'+n.ToUpper+': TALSEqualizerProperties='+LINEENDING+
           '    ( LowGain: '+Label55.Caption+';'+LINEENDING+
           '      LowCutoff: '+Label57.Caption+';'+LINEENDING+
           '      Mid1Gain: '+Label59.Caption+';'+LINEENDING+
           '      Mid1Center: '+Label61.Caption+';'+LINEENDING+
           '      Mid1Width: '+Label63.Caption+';'+LINEENDING+
           '      Mid2Gain: '+Label65.Caption+';'+LINEENDING+
           '      Mid2Center: '+Label67.Caption+';'+LINEENDING+
           '      Mid2Width: '+Label69.Caption+';'+LINEENDING+
           '      HighGain: '+Label71.Caption+';'+LINEENDING+
           '      HighCutoff: '+Label73.Caption+' );';
  end;

  if PageControl1.ActivePage = PageFrequencyShifter then
  begin
    txt := '  FREQSHIFTER_PRESET_'+n.ToUpper+': TALSFreqShifterProperties='+LINEENDING+
           '    ( Frequency: '+Label75.Caption+';'+LINEENDING+
           '      LeftDirection: '+IndexToFreqShifterDir( ComboBox3.ItemIndex )+';'+LINEENDING+
           '      RightDirection: '+IndexToFreqShifterDir( ComboBox4.ItemIndex )+' );';
  end;

  if PageControl1.ActivePage = PagePitchShifter then
  begin
    txt := '  PITCHSHIFTER_PRESET_'+n.ToUpper+': TALSPitchShifterProperties='+LINEENDING+
           '    ( CoarseTune: '+Label79.Caption+';'+LINEENDING+
           '      FineTune: '+Label81.Caption+' );';
  end;

  if PageControl1.ActivePage = PageRingModulator then
  begin
    case ComboBox5.ItemIndex of
      0: t := 'rmwSinusoid';
      1: t := 'rmwSawtooth';
      2: t := 'rmwSquare';
    end;
    txt := '  RINGMODULATOR_PRESET_'+n.ToUpper+': TALSRingModulatorProperties='+LINEENDING+
           '    ( Frequency: '+Label83.Caption+';'+LINEENDING+
           '      HighPassCutoff: '+Label85.Caption+';'+LINEENDING+
           '      Waveform: '+t+' );';
  end;

  if PageControl1.ActivePage = PageVocalMorpher then
  begin
    case ComboBox8.ItemIndex of
      0: t := 'vmwSinusoid';
      1: t := 'vmwTriangle';
      2: t := 'vmwSawtooth';
    end;
    txt := '  VOCALMORPHER_PRESET_'+n.ToUpper+': TALSVocalMorpherProperties='+LINEENDING+
           '    ( PhonemeA: pho'+FVocalMorpherProp.PhonemeList[ComboBox6.ItemIndex]+';'+LINEENDING+
           '      PhonemeB: pho'+FVocalMorpherProp.PhonemeList[ComboBox7.ItemIndex]+';'+LINEENDING+
           '      PhonemeACoarseTuning: '+Label93.Caption+';'+LINEENDING+
           '      PhonemeBCoarseTuning: '+Label95.Caption+';'+LINEENDING+
           '      Waveform: '+t+';'+LINEENDING+
           '      Rate: '+Label88.Caption+' );';
  end;

  if PageControl1.ActivePage = PageReverb then
  begin
    txt := '  REVERB_PRESET_'+n.ToUpper+': TALSReverbProperties='+LINEENDING+
           '    ( Density: '+Label97.Caption+';'+LINEENDING+
           '      Diffusion: '+Label99.Caption+';'+LINEENDING+
           '      Gain: '+Label101.Caption+';'+LINEENDING+
           '      GainHF: '+Label103.Caption+';'+LINEENDING+
           '      DecayTime: '+Label105.Caption+';'+LINEENDING+
           '      DecayHFRatio: '+Label109.Caption+';'+LINEENDING+
           '      ReflectionsGain: '+Label111.Caption+';'+LINEENDING+
           '      ReflectionsDelay: '+Label113.Caption+';'+LINEENDING+
           '      LateReverbGain: '+Label116.Caption+';'+LINEENDING+
           '      LateReverbDelay: '+Label118.Caption+';'+LINEENDING+
           '      AirAbsorptionGainHF: '+Label122.Caption+';'+LINEENDING+
           '      RoomRolloffFactor: '+Label120.Caption+';'+LINEENDING+
           '      DecayHFLimit: '+ComboBox9.ItemIndex.ToString+' );';
  end;

  if Length(txt)>0 then
  begin
    Clipboard.AsText := txt;
    ShowMessage('Preset '+n+' have been copied to the clipboard'+LINEENDING+
                'Simply paste it in the library');
  end;
end;


end.

