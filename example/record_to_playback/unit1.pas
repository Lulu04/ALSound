unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls,
  ALSound,
  frame_channel_level;

type

  TChannelLevel=record
    Left,
    Right: single;
  end;
  PChannelLevel=^TChannelLevel;


  { TForm1 }

  TForm1 = class(TForm)
    BStart: TSpeedButton;
    BStop: TSpeedButton;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    ImageList1: TImageList;
    Label12: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel3: TPanel;
    Label3: TLabel;
    Panel2: TPanel;
    Label10: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    Shape1: TShape;
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
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    procedure CheckBox3Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure ComboBox5Select(Sender: TObject);
    procedure ComboBox6Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BStartClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    FCaptureContext: TALSCaptureContext; // context for capture
    FPlaybackContext: TALSPlaybackContext; // context for playback

    FSound: TALSSound;  // sound for the playback

    FEffects: array[0..2] of TALSEffect; // 3 effects applyed on the playback sound

    FFlag_LockPresetSelectionChange: boolean;
    FPresetIndex: array[0..2] of integer;

    FAutowahProp: TALSAutoWahProperties;
    FChorusProp: TALSChorusProperties;
    FFlangerProp: TALSFlangerProperties;
    FCompressorProp: TALSCompressorProperties;
    FDistortionProp: TALSDistortionProperties;
    FEchoProp: TALSEchoProperties;
    FEqualizerProp: TALSEqualizerProperties;
    FFreqShifterProp: TALSFreqShifterProperties;
    FPitchShifterProp: TALSPitchShifterProperties;
    FRingModulatorProp: TALSRingModulatorProperties;
    FVocalMorpherProp: TALSVocalMorpherProperties;
    FEAXReverbProp: TEAXReverbProperties;

    FrameChannelsLevel1: TFrameChannelsLevel;

    procedure FillPresetList(aCB: TComboBox; aEffectIndex: integer);
    procedure ReconstructEffectChain;
    procedure CreateEffect(Index: integer);

    function CaptureContextIsReady: boolean;
    procedure UpdateWidgets;
    procedure ProcessOnCaptureBuffer(Sender: TALSCaptureContext;
                                     const aBuf: TALSCaptureFrameBuffer);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  attribs: TALSContextAttributes;
begin
  // load OpenAL-Soft and LibSndFile libraries
  ALSManager.LoadLibraries;

  // Creates a playback context with float buffer to listen the captured samples
  attribs.InitDefault;
  attribs.ContextUseFloat := True;
  attribs.EnableOutputLimiter := False;
  FPlaybackContext := ALSManager.CreatePlaybackContext(-1, attribs);

  FEffects[0] := FPlaybackContext.CreateEffect( AL_EFFECT_NONE, FAutowahProp );
  FEffects[1] := FPlaybackContext.CreateEffect( AL_EFFECT_NONE, FAutowahProp );
  FEffects[2] := FPlaybackContext.CreateEffect( AL_EFFECT_NONE, FAutowahProp );

  FrameChannelsLevel1 := TFrameChannelsLevel.Create(Self);
  FrameChannelsLevel1.Parent := Panel9;
  FrameChannelsLevel1.Align := alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Destroy the capture context
  if FCaptureContext <> nil then
    FCaptureContext.Free;

  // Destroy Effects
  if FEffects[0].Ready then
    FPlaybackContext.DeleteEffect( FEffects[0] );
  if FEffects[1].Ready then
    FPlaybackContext.DeleteEffect( FEffects[1] );
  if FEffects[2].Ready then
    FPlaybackContext.DeleteEffect( FEffects[2] );

  // Destroy the playback context
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  A: TStringArray;
  i: integer;
begin
  // Check if ALSManager encounter an error while loading OpenAL-Soft and LibSndFile
  if ALSManager.Error then
    ShowMessage(ALSManager.StrError);

  // Fill combobox with available capture device names
  ComboBox1.Clear;
  A := ALSManager.ListOfCaptureDeviceName;
  for i := 0 to High(A) do
    ComboBox1.Items.Add(A[i]);

  // Fill preset's combobox
  FFlag_LockPresetSelectionChange := True;
  FillPresetList( ComboBox6, ComboBox5.ItemIndex );
  FillPresetList( ComboBox8, ComboBox7.ItemIndex );
  FillPresetList( ComboBox10, ComboBox9.ItemIndex );

  ComboBox6.ItemIndex := 0;
  ComboBox8.ItemIndex := 0;
  ComboBox10.ItemIndex := 0;
  FFlag_LockPresetSelectionChange := False;

  UpdateWidgets;
end;

procedure TForm1.BStartClick(Sender: TObject);
begin
  // checks if user have choosen all parameters
  if not CaptureContextIsReady then
    exit;

  // Asks to our capture context to play the captured audio in real time.
  FSound := FCaptureContext.PrepareToPlayback( FPlaybackContext );

  // In this demo we choose not to apply tone on auxiliary send slots.
  FSound.ApplyToneOnAuxSend := False;

  // Show a message in case of error
  if FSound.Error then
    ShowMessage( FSound.StrError );

  // We can start the capture
  FCaptureContext.StartCapture;

  // Create and apply the effect chain to the sound
  ReconstructEffectChain;

  Label3.Tag := 0;
  Label3.Caption := 'Recording';
  Timer1.Enabled := True;
  BStart.Enabled := False;
  BStop.Enabled := True;
  Panel1.Enabled := FALSE;
  Panel2.Enabled := True;
end;

procedure TForm1.BStopClick(Sender: TObject);
begin
  // Stop the capture
  FCaptureContext.StopCapture;

  // If any show the capture message error
  if FCaptureContext.CaptureError then
    ShowMessage(FCaptureContext.StrCaptureError);

  FSound := nil;
  Timer1.Enabled := False;
  Label3.Visible := False;
  BStart.Enabled := True;
  BStop.Enabled := False;
  Panel1.Enabled := True;
  Panel2.Enabled := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // the timer is used to blink a label
  Timer1.Enabled := False;

  case Label3.Tag of
    0: begin
      Label3.Tag := 1;
      Label3.Caption := 'Playback';
    end;
    1: begin
      Label3.Tag := 0;
      Label3.Caption := '';
    end;
  end;

  Timer1.Enabled := True;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  FSound.SetEffectDryWetVolume( FEffects[0], TrackBar1.Position/TrackBar1.Max);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  FCaptureContext.PreAmp := TrackBar2.Position*0.01;
  Label23.Caption := 'x'+FormatFloat('0.0', TrackBar2.Position*0.01);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  FSound.Tone.Value := TrackBar3.Position/TrackBar3.Max;
end;

procedure TForm1.FillPresetList(aCB: TComboBox; aEffectIndex: integer);
begin
  aCB.Clear;
  case aEffectIndex of
    0: aCB.Items.AddStrings( FAutowahProp.PresetList );
    1: aCB.Items.AddStrings( FChorusProp.PresetList );
    2: aCB.Items.AddStrings( FFlangerProp.PresetList );
    3: aCB.Items.AddStrings( FCompressorProp.PresetList );
    4: aCB.Items.AddStrings( FDistortionProp.PresetList );
    5: aCB.Items.AddStrings( FEchoProp.PresetList );
    6: aCB.Items.AddStrings( FEqualizerProp.PresetList );
    7: aCB.Items.AddStrings( FFreqShifterProp.PresetList );
    8: aCB.Items.AddStrings( FPitchShifterProp.PresetList );
    9: aCB.Items.AddStrings( FRingModulatorProp.PresetList );
   10: aCB.Items.AddStrings( FVocalMorpherProp.PresetList );
   11: aCB.Items.AddStrings( FEAXReverbProp.PresetList );
  end;
end;

procedure TForm1.ReconstructEffectChain;
var
  i: Integer;
begin
  // Remove all effects previously applyed on the sound.
  FSound.RemoveAllEffects;

  // Delete all previous effects.
  FPlaybackContext.DeleteEffect(FEffects[0]);
  FPlaybackContext.DeleteEffect(FEffects[1]);
  FPlaybackContext.DeleteEffect(FEffects[2]);

  for i:=0 to High(FEffects) do
  begin
   // FPlaybackContext.DeleteEffect(FEffects[i]);
    CreateEffect(i);
  end;

  // Reconstruct the effect's chain.
  FEffects[0].ChainWith( FEffects[1] );
  FEffects[1].ChainWith( FEffects[2] );

  // Apply the effects to the sound.
  FSound.ApplyEffect( FEffects[0] );

  // Apply mute.
  FEffects[0].Mute := CheckBox3.Checked;
  FEffects[1].Mute := CheckBox4.Checked;
  FEffects[2].Mute := CheckBox5.Checked;

  // Apply Dry/Wet gain.
  FSound.SetEffectDryWetVolume( FEffects[0], TrackBar1.Position/TrackBar1.Max);
end;

procedure TForm1.CreateEffect(Index: integer);
var
  effectType, presetIndex: integer;
begin
  case Index of
    0: effectType := ComboBox5.ItemIndex;
    1: effectType := ComboBox7.ItemIndex;
    2: effectType := ComboBox9.ItemIndex;
  end;

  case Index of
    0: presetIndex := ComboBox6.ItemIndex;
    1: presetIndex := ComboBox8.ItemIndex;
    2: presetIndex := ComboBox10.ItemIndex;
  end;
  if presetIndex = -1 then
    presetIndex := 0;

  // Create the new one
  case effectType of
    0: begin
      FAutowahProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_AUTOWAH, FAutowahProp );
    end;
    1: begin
      FChorusProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_CHORUS, FChorusProp );
    end;
    2: begin
      FFlangerProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_FLANGER, FFlangerProp );
    end;
    3: begin
      FCompressorProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_COMPRESSOR, FCompressorProp );
    end;
    4: begin
      FDistortionProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_DISTORTION, FDistortionProp );
    end;
    5: begin
      FEchoProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_ECHO, FEchoProp );
    end;
    6: begin
      FEqualizerProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_EQUALIZER, FEqualizerProp );
    end;
    7: begin
      FFreqShifterProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_FREQUENCYSHIFTER, FFreqShifterProp );
    end;
    8: begin
      FPitchShifterProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_PITCHSHIFTER, FPitchShifterProp);
    end;
    9: begin
      FRingModulatorProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_RINGMODULATOR, FRingModulatorProp );
    end;
   10: begin
      FVocalMorpherProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_VOCALMORPHER, FVocalMorpherProp );
    end;
   11: begin
      FEAXReverbProp.InitWithPreset( presetIndex );
      FEffects[Index] := FPlaybackContext.CreateEffect( AL_EFFECT_EAXREVERB, FEAXReverbProp );
    end;
  end;
end;

function TForm1.CaptureContextIsReady: boolean;
begin
  Result := FCaptureContext <> NIL;
  if Result
    then Result := not FCaptureContext.Error;
end;

procedure TForm1.UpdateWidgets;
begin
  if FCaptureContext <> nil then
  begin
    if FCaptureContext.Error then
    begin
      Label2.Caption := FCaptureContext.StrError;
      Label2.Color := clRed;
    end
    else
    begin
      Label2.Caption := 'Status: Ready';
      Label2.Color := clLime;
    end;
    Label2.Visible := True;
  end
  else
  begin
    Label2.Visible := FALSE;
    Label3.Caption := '';
  end;

  Panel3.Enabled := CaptureContextIsReady;

  if not FEffects[0].Ready then
    Label18.Caption := 'NOT READY'
  else
    Label18.Caption := 'READY';

  if not FEffects[1].Ready then
    Label19.Caption := 'NOT READY'
  else
    Label19.Caption := 'READY';

  if not FEffects[2].Ready then
    Label20.Caption := 'NOT READY'
  else
    Label20.Caption := 'READY';

end;

procedure TForm1.ProcessOnCaptureBuffer(Sender: TALSCaptureContext;
  const aBuf: TALSCaptureFrameBuffer);
begin
  FrameChannelsLevel1.UpdateProgressBar( aBuf );
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
var
  captureFormat: TALSCaptureFormat;
  captureSampleRate: longword;
begin
  // User have selected a capture device.
  if ComboBox1.ItemIndex = -1 then
    exit;

  // Destroy the previous capture context.
  if FCaptureContext <> nil then
    FCaptureContext.Free;

  // Retrieve capture format from combobox.
  case ComboBox3.ItemIndex of
    0: case ComboBox2.ItemIndex of
        0: captureFormat := ALS_CAPTUREFORMAT_MONO16;
        1: captureFormat := ALS_CAPTUREFORMAT_MONO_FLOAT32;
      end;
    1: case ComboBox2.ItemIndex of
        0: captureFormat := ALS_CAPTUREFORMAT_STEREO16;
        1: captureFormat := ALS_CAPTUREFORMAT_STEREO_FLOAT32;
      end;
  end;

  // Retrieve the sample rate from combobox.
  case ComboBox4.ItemIndex of
    0: captureSampleRate := 44100;
    1: captureSampleRate := 48000;
    2: captureSampleRate := 96000;
  end;

  // Creates the new capture context.
  FCaptureContext := ALSManager.CreateCaptureContext(ComboBox1.ItemIndex,
    captureSampleRate, captureFormat, 0.1);

  // Ask the capture context to compute channel's level.
  FCaptureContext.MonitoringEnabled := True;

  // Ask the capture context to call a callback each time it have a new buffer
  // filled with captured samples.
  FCaptureContext.OnCaptureBuffer := @ProcessOnCaptureBuffer;

  // Check error and show the error message.
  if FCaptureContext.Error then
    ShowMessage(FCaptureContext.StrError);

  UpdateWidgets;
end;

procedure TForm1.ComboBox5Select(Sender: TObject);
var
  cb, presetCB: TComboBox;
begin
  // User have selected an effect.
  cb := Sender as TComboBox;

  // Fill the presets list according to the selected effect.
  FFlag_LockPresetSelectionChange := True;

  // Retrieve the corresponding preset's combobox.
  case cb.Tag of
    0: presetCB := ComboBox6;
    1: presetCB := ComboBox8;
    2: presetCB := ComboBox10;
  end;
  // And fill it with the effect's preset.
  FillPresetList( presetCB, cb.ItemIndex );
   // Select the previous preset used by the user for this effect.
  presetCB.ItemIndex := FPresetIndex[cb.Tag];

  FFlag_LockPresetSelectionChange := False;

  ReconstructEffectChain;

  UpdateWidgets;
end;

procedure TForm1.ComboBox6Select(Sender: TObject);
var
  cb: TComboBox;
begin
  // User have selected a preset.
  if FFlag_LockPresetSelectionChange then
    exit;

  cb := Sender as TComboBox;

  // Save the preset index selected by the user
  FPresetIndex[cb.Tag] := cb.ItemIndex;

  ReconstructEffectChain;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
var
  cb: TCheckBox;
begin
  cb := Sender as TCheckBox;

  // Mute/Unmute the corresponding effect.
  FEffects[cb.Tag].Mute := cb.Checked;
end;


end.
