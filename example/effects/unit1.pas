unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Spin, Buttons,
  ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    BLoad: TBitBtn;
    BPlay: TSpeedButton;
    BPause: TSpeedButton;
    BStop: TSpeedButton;
    BFadeIn: TButton;
    BFadeOut: TButton;
    BResetPitch: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    FSE1: TFloatSpinEdit;
    FSE2: TFloatSpinEdit;
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    ProgressBar1: TProgressBar;
    BPanCenter: TSpeedButton;
    BMute: TSpeedButton;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Shape19: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Shape24: TShape;
    Shape25: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    TB5: TTrackBar;
    TB6: TTrackBar;
    TB2: TTrackBar;
    TB3: TTrackBar;
    TB4: TTrackBar;
    Timer1: TTimer;
    TBVolume: TTrackBar;
    TBPitch: TTrackBar;
    TBPan: TTrackBar;
    TB1: TTrackBar;
    TBTone: TTrackBar;
    TrackBar1: TTrackBar;
    procedure BLoadClick(Sender: TObject);
    procedure BPlayClick(Sender: TObject);
    procedure BPauseClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure BFadeInClick(Sender: TObject);
    procedure BFadeOutClick(Sender: TObject);
    procedure BResetPitchClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BPanCenterClick(Sender: TObject);
    procedure BMuteClick(Sender: TObject);
    procedure TB1Change(Sender: TObject);
    procedure TBToneChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
    procedure TBPitchChange(Sender: TObject);
    procedure TBPanChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    // our playback context
    FPlaybackContext: TALSPlaybackContext;
    // sound instance
    FSound: TALSSound;
    // effects instance
    FEAXReverb,
    FVocalMorpher,
    FDistorsion,
    FEcho,
    FFlanger,
    FAutoWah: TALSEffect;
    // effect properties
    FVocalMorpherParam: TALSVocalmorpherProperties;
    procedure UncheckAllEffects;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BPlayClick(Sender: TObject);
begin
 if FSound = NIL then exit;
 FSound.Play;
 FSound.Volume.Value := ALS_VOLUME_MAX;
end;

procedure TForm1.BLoadClick(Sender: TObject);
var
  s: string;
begin
 if not OD1.Execute then exit;

 UncheckAllEffects;

 Label11.Caption := ExtractFileName( OD1.FileName );

 // Free the old one
 if FSound <> NIL then
   FSound.Kill;    // we can also do FPlaybackContext.Delete( FSound );

 // Creates the new one as stream
 FSound := FPlaybackContext.AddStream( OD1.FileName );

 // check if an error occurs
 if FSound.Error then
   ShowMessage(FSound.StrError);

 // Show sound info
 Label15.Caption := FSound.Format+' - '+FSound.SubFormat;
 s := FSound.ChannelCount.ToString;
 if FSound.ChannelCount > 1 then
   s := s+' channels'
 else
   s := s+' channel';
 s := s+'  -  '+ FSound.SampleRate.ToString+'Hz  -  duration '+
      FormatFloat('0.00', FSound.TotalDuration) + 's';
 Label17.Caption := s;

 // Set loop mode
 FSound.Loop := CheckBox1.Checked;
end;

procedure TForm1.BPauseClick(Sender: TObject);
begin
 if FSound = NIL then exit;
 if FSound <> nil then FSound.Pause;
end;

procedure TForm1.BStopClick(Sender: TObject);
begin
 if FSound = NIL then exit;
 if FSound <> nil then FSound.Stop;
end;

procedure TForm1.BFadeInClick(Sender: TObject);
begin
 if FSound = NIL then exit;
 FSound.FadeIn( ALS_VOLUME_MAX, FSE1.Value, TALSCurveID(ComboBox1.ItemIndex) );
end;

procedure TForm1.BFadeOutClick(Sender: TObject);
begin
  if FSound = NIL then exit;
  FSound.FadeOut( FSE2.Value, TALSCurveID(ComboBox2.ItemIndex) );
end;

procedure TForm1.BResetPitchClick(Sender: TObject);
begin
  TBPitch.Position := 100;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if FSound = NIL then exit;
  FSound.Loop := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    FSound.ApplyEffect( FEAXReverb );
    FSound.SetAuxSendGain( FEAXReverb, TB1.Position/TB1.Max );
  end
  else FSound.RemoveEffect( FEAXReverb );
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.Checked then
  begin
    FSound.ApplyEffect( FVocalMorpher );
    FSound.SetAuxSendGain( FVocalMorpher, TB2.Position/TB2.Max );
  end
  else FSound.RemoveEffect( FVocalMorpher );
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  if CheckBox4.Checked then
  begin
    FSound.ApplyEffect( FDistorsion );
    FSound.SetAuxSendGain( FDistorsion, TB3.Position/TB3.Max );
  end
  else FSound.RemoveEffect( FDistorsion );
end;

procedure TForm1.CheckBox5Change(Sender: TObject);
begin
  if CheckBox5.Checked then
  begin
    FSound.ApplyEffect( FEcho );
    FSound.SetAuxSendGain( FEcho, TB4.Position/TB4.Max );
  end
  else FSound.RemoveEffect( FEcho );
end;

procedure TForm1.CheckBox6Change(Sender: TObject);
begin
  if CheckBox6.Checked then
  begin
    FSound.ApplyEffect( FFlanger );
    FSound.SetAuxSendGain( FFlanger, TB5.Position/TB5.Max );
  end
  else FSound.RemoveEffect( FFlanger );
end;

procedure TForm1.CheckBox7Change(Sender: TObject);
begin
  if CheckBox7.Checked then
  begin
    FSound.ApplyEffect( FAutoWah );
    FSound.SetAuxSendGain( FAutoWah, TB6.Position/TB6.Max );
  end
  else FSound.RemoveEffect( FAutoWah );
end;

procedure TForm1.CheckBox8Change(Sender: TObject);
begin
  if FSound <> NIL then
    FSound.ApplyToneOnAuxSend := CheckBox8.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var i: integer;
begin
  i := ComboBox1.ItemIndex;
  if i = -1 then exit;
  ALSVelocityCurveList.GetCurveByIndex( i ).DrawOn( Image1 );
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
var
  i: integer;
begin
  i := ComboBox2.ItemIndex;
  if i = -1 then exit;
  ALSVelocityCurveList.GetCurveByIndex( i ).DrawOn( Image2 );
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FAttribs: TALSContextAttributes;
begin
  // Fill the file open dialog filter property with supported audio file types.
  OD1.Filter := ALSManager.DialogFileFilters(True);

  // In this demo we need 6 auxiliary sends. By default, OpenAL-Soft provide
  // only 2. So we have to pass a customized attributes to the context creation.
  // For that, we use a TALSContextAttributes to ask 6 auxiliary send
  // It's not guaranted that the selected device can manage 6 auxiliary sends.
  // Check the real number of auxiliary send available after the creation of the
  // context with TALSPlaybackContext.AuxiliarySendCount property.

  FAttribs.InitDefault;  // fill our FAttribs with default value
  FAttribs.MaxAuxSend := 6;  //  need 6 auxiliary send

  // Create a playback context with the default playback device and our custom attributes
  FPlaybackContext := ALSManager.CreatePlaybackContext(-1, FAttribs);

  // show the real available auxiliary send
  Label16.Caption := FPlaybackContext.AuxiliarySendCount.ToString+' auxiliary send available';

  // create an EAX reverb with the preset 'EFX_REVERB_PRESET_UNDERWATER'
  FEAXReverb := FPlaybackContext.CreateEffect( AL_EFFECT_EAXREVERB, EFX_REVERB_PRESET_UNDERWATER);

  // This is an example how to sets effect's parameters 'manualy'
  FVocalMorpherParam.PhonemeA:=phoA;
  FVocalMorpherParam.PhonemeACoarseTuning:=-20;
  FVocalMorpherParam.PhonemeB:=phoER;
  FVocalMorpherParam.PhonemeBCoarseTuning:=+20;
  FVocalMorpherParam.Rate:=4.0;
  FVocalMorpherParam.Waveform:=vmwSinusoid;
  FVocalMorpher := FPlaybackContext.CreateEffect( AL_EFFECT_VOCALMORPHER, FVocalMorpherParam); // create the effect

  // For the other effects, we use presets
  FDistorsion := FPlaybackContext.CreateEffect( AL_EFFECT_DISTORTION, DISTORTION_PRESET_70S_FUZZGUITAR);
  FEcho := FPlaybackContext.CreateEffect( AL_EFFECT_ECHO, ECHO_PRESET_MONO_VOCAL_1);
  FFlanger := FPlaybackContext.CreateEffect( AL_EFFECT_FLANGER, FLANGER_PRESET_DEFAULT);
  FAutoWah := FPlaybackContext.CreateEffect(AL_EFFECT_AUTOWAH, AUTOWAH_PRESET_ROCKY2);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPlaybackContext.DeleteEffect( FEAXReverb );
  FPlaybackContext.DeleteEffect( FVocalMorpher );
  FPlaybackContext.DeleteEffect( FDistorsion );
  FPlaybackContext.DeleteEffect( FEcho );
  FPlaybackContext.DeleteEffect( FFlanger );
  FPlaybackContext.DeleteEffect( FAutoWah );
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var i: integer;
  txt: string;
begin
  // display some error message or hints
  if FPlaybackContext.Error then begin
    ShowMessage(FPlaybackContext.StrError);
    Label16.Caption := '';
  end else begin
      txt:='';
      if not FPlaybackContext.HaveStereoAngle then txt+='Can not apply panning on stereo sound...'+LINEENDING;
      if not FPlaybackContext.HaveEFX then txt+='Can not EFX...'+LINEENDING;
      if not FEAXReverb.Ready then txt+='EAXReverb Not Ready'+LINEENDING;
      if not FVocalMorpher.Ready then txt+='Vocal Morpher Not Ready'+LINEENDING;
      if not FDistorsion.Ready then txt+='Distorsion Not Ready'+LINEENDING;
      if not FEcho.Ready then txt+='Echo Not Ready'+LINEENDING;
      if not FFlanger.Ready then txt+='Flanger Not Ready'+LINEENDING;
      if not FAutoWah.Ready then txt+='AutoWah Not Ready'+LINEENDING;
      if txt<>'' then ShowMessage(txt);
    end;

  // fill the combo box with available velocity curve list
  ComboBox1.Clear;
  ComboBox2.Clear;
  for i:=0 to ALSVelocityCurveList.Count-1 do
   begin
    ComboBox1.Items.Add( ALSVelocityCurveList.GetCurveByIndex( i ).Name );
    ComboBox2.Items.Add( ALSVelocityCurveList.GetCurveByIndex( i ).Name );
   end;
  ComboBox1.ItemIndex := 2;
  ComboBox2.ItemIndex := 1;
  ComboBox1Change( self );
  ComboBox2Change( self );
end;

procedure TForm1.BPanCenterClick(Sender: TObject);
begin
  TBPan.Position:=0;
end;

procedure TForm1.BMuteClick(Sender: TObject);
begin
  case BMute.Tag of
    0: begin
      BMute.Tag:=1;
      BMute.Color:=$0080FFFF;
      if FSound <> NIl then FSound.Mute:=TRUE;
    end;
    1: begin
      BMute.Tag:=0;
      BMute.Color:=$007D7D7D;
      if FSound <> NIl then FSound.Mute:=FALSE;
    end;
  end;
end;

procedure TForm1.TB1Change(Sender: TObject);
begin
  if FSound = NIl then exit;

  if Sender=TB1 then // FSound.SetEffectDryWetVolume( FEAXReverb, TB1.Position/TB1.Max );
    FSound.SetAuxSendGain( FEAXReverb, TB1.Position/TB1.Max );

  if Sender=TB2 then //FSound.SetEffectDryWetVolume( FVocalMorpher, TB2.Position/TB2.Max );
    FSound.SetAuxSendGain( FVocalMorpher, TB2.Position/TB2.Max );

  if Sender=TB3 then //FSound.SetEffectDryWetVolume( FDistorsion, TB3.Position/TB3.Max );
    FSound.SetAuxSendGain( FDistorsion, TB3.Position/TB3.Max );

  if Sender=TB4 then //FSound.SetEffectDryWetVolume( FEcho, TB4.Position/TB4.Max );
    FSound.SetAuxSendGain( FEcho, TB4.Position/TB4.Max );

  if Sender=TB5 then //FSound.SetEffectDryWetVolume( FFlanger, TB5.Position/TB5.Max );
    FSound.SetAuxSendGain( FFlanger, TB5.Position/TB5.Max );

  if Sender=TB6 then //FSound.SetEffectDryWetVolume( FAutoWah, TB6.Position/TB6.Max );
    FSound.SetAuxSendGain( FAutoWah, TB6.Position/TB6.Max );
end;

procedure TForm1.TBToneChange(Sender: TObject);
begin
  if FSound <> NIL then
    FSound.Tone.Value := TBTone.Position/TBTone.Max;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  Timer1.Enabled:=FALSE;
  if FSound = NIL then begin
    Label1.Caption := ' ';
    Label4.Caption := ' ';
    Label15.Caption := ' ';
    Label17.Caption := ' ';
    Timer1.Enabled:=TRUE;
    exit;
  end;

  if FSound.Error then
    Label4.Caption := FSound.StrError
  else
  begin
    Label1.Caption := 'time: ' +
      FormatFloat('0.00', FSound.GetTimePosition) + ' / ' +
      FormatFloat('0.00', FSound.TotalDuration) + 's';

    ProgressBar1.Max := FSound.Seconds2Byte( FSound.TotalDuration ); // FSound.SampleCount div FSound.ChannelCount;
    ProgressBar1.Position := FSound.Seconds2Byte( FSound.GetTimePosition );

    case FSound.State of
      ALS_STOPPED: s := 'STOPPED';
      ALS_PLAYING: s := 'PLAYING';
      ALS_PAUSED: s := 'PAUSED';
    end;
    Label4.Caption := 'State : ' + s;
  end;
  Timer1.Enabled:=TRUE;
end;

procedure TForm1.TBVolumeChange(Sender: TObject);
begin
 if FSound = NIL then exit;
 FSound.Volume.Value := TBVolume.Position/TBVolume.Max;
end;

procedure TForm1.TBPitchChange(Sender: TObject);
begin
 if FSound = NIL then exit;
 FSound.Pitch.Value := TBPitch.Position / 100;
 Label3.Caption := 'Pitch : ' + formatfloat('0.00', FSound.Pitch.Value);
end;

procedure TForm1.TBPanChange(Sender: TObject);
var v: single;
begin
  if FSound = NIL then exit;
  v:=TBPan.Position/TBPan.Max;
  FSound.Pan.Value:=v;
  Label12.Caption := 'Pan: ' + formatfloat('0.0', v);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if FSound <> NIL then
    FSound.SetDryGain(TrackBar1.Position/TrackBar1.Max);
end;

procedure TForm1.UncheckAllEffects;
begin
  CheckBox2.Checked:=FALSE;
  CheckBox3.Checked:=FALSE;
  CheckBox4.Checked:=FALSE;
  CheckBox5.Checked:=FALSE;
  CheckBox6.Checked:=FALSE;
  CheckBox7.Checked:=FALSE;
end;


end.
