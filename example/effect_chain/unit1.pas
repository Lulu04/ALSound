unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, ComCtrls,
  ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    BLoad: TBitBtn;
    BPause: TSpeedButton;
    BPlay: TSpeedButton;
    BStop: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Shape1: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape9: TShape;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure BLoadClick(Sender: TObject);
    procedure BPlayClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    // our playback context
    FPlaybackContext: TALSPlaybackContext;
    // our sound object
    FSound: TALSSound;
    // the 3 effects
    FEffect1,
    FEffect2,
    FEffect3: TALSEffect;

    FChorusProp: TALSChorusProperties;
    FEAXReverbProp: TEAXReverbProperties;
    FEquProp: TALSEqualizerProperties;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  FAttribs: TALSContextAttributes;
begin
  // load OpenAL-Soft and LibSndFile libraries
  ALSManager.LoadLibraries;

  // configure custom attributes to ask the playback context to use float
  FAttribs.InitDefault;
//  FAttribs.ContextUseFloat := True;

  // Create a playback context on default playback device, with our custom attributes
  FPlaybackContext := ALSManager.CreatePlaybackContext(-1, FAttribs);

  // Creates 3 effects
  FEffect1 := FPlaybackContext.CreateEffect(AL_EFFECT_CHORUS, CHORUS_PRESET_DEFAULT);
  FEffect2 := FPlaybackContext.CreateEffect(AL_EFFECT_EAXREVERB, EFX_REVERB_PRESET_CONCERTHALL);
  FEffect3 := FPlaybackContext.CreateEffect(AL_EFFECT_EQUALIZER, EQUALIZER_PRESET_DEFAULT);

  // Chains our 3 effects
  FEffect1.ChainWith( FEffect2 );  // Redirect Effect1 output to Effect2 input
  FEffect2.ChainWith( FEffect3 );  // Redirect Effect2 output to Effect3 input
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free the 3 effects
  FPlaybackContext.DeleteEffect(FEffect1);
  FPlaybackContext.DeleteEffect(FEffect2);
  FPlaybackContext.DeleteEffect(FEffect3);
  // Free our playback context
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Display some error message or hints
  if FPlaybackContext.Error then
    ShowMessage(FPlaybackContext.StrError);

  if not Feffect1.Ready then
    Label1.Caption:='NOT READY !';
  if not Feffect2.Ready then
    Label1.Caption:='NOT READY !';
  if not Feffect3.Ready then
    Label1.Caption:='NOT READY !';

  ComboBox1.Items.AddStrings(FChorusProp.PresetList, True);
  ComboBox1.ItemIndex := 0;
  ComboBox2.Items.AddStrings(FEAXReverbProp.PresetList, True);
  ComboBox2.ItemIndex := 0;
  ComboBox3.Items.AddStrings(FEquProp.PresetList, True);
  ComboBox3.ItemIndex := 0;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if FSound = NIL then Exit;

  // User moves trackbar WET or DRY gain

  if Sender = TrackBar1 then
    FSound.SetDryGain(TrackBar1.Position/TrackBar1.Max);

  if Sender = TrackBar2 then
    FSound.SetAuxSendGain(FEffect1, TrackBar2.Position/TrackBar2.Max);
end;

procedure TForm1.BLoadClick(Sender: TObject);
var
  s: string;
begin
  // User selects a sound filename to load
  if not OD1.Execute then exit;

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
  FSound.Loop := True;

  // Apply the first effect to the first free auxiliary send
  FSound.ApplyEffect( FEffect1 );
end;

procedure TForm1.BPlayClick(Sender: TObject);
begin
  if FSound = NIL then Exit;

  // User have clicked on Play/Pause/Stop button

  if Sender = BPlay then
    FSound.Play(True);

  if Sender = BPause then
    FSound.Pause;

  if Sender = BStop then
    FSound.Stop;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if FSound = NIL then Exit;

  // Mute the appropriate effect

  if Sender = CheckBox1 then
    FEffect1.Mute := CheckBox1.Checked;

  if Sender = CheckBox2 then
    FEffect2.Mute := CheckBox2.Checked;

  if Sender = CheckBox3 then
    FEffect3.Mute := CheckBox3.Checked;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
var
  cb: TComboBox;
begin
  cb := Sender as TComboBox;

  if Sender = ComboBox1 then
  begin // User have selected a chorus preset
    FChorusProp.InitWithPreset( cb.ItemIndex ); // Updates the chorus property record
    FEffect1.UpdateParameters( FChorusProp ); // Updates Effect1 with new properties
  end;

  if Sender = ComboBox2 then
  begin // User have selected a reverb preset
    FEAXReverbProp.InitWithPreset( cb.ItemIndex ); // Updates the reverb property record
    FEffect2.UpdateParameters( FEAXReverbProp ); // Updates Effect2 with new properties
  end;

  if Sender = ComboBox3 then
  begin // User have selected a equalizer preset
    FEquProp.InitWithPreset( cb.ItemIndex ); // Updates the equalizer property record
    FEffect3.UpdateParameters( FEquProp ); // Updates Effect3 with new properties
  end;
end;

end.

