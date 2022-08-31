unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls,
  ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Image1: TImage;
    ImageList1: TImageList;
    LabelHelicopter: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelTruck1: TLabel;
    Label5: TLabel;
    LabelBar: TLabel;
    Timer1: TTimer;
    procedure ComboBox1Select(Sender: TObject);
    procedure ComboBox2Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FAttribs: TALSContextAttributes;
    FHelicopter,
    FMotorTruck1,
    FBarMusic: TALSSound;
    procedure SetSoundsAttenuation;
    procedure SetSoundsPosition;
  public
    FPlaybackContext: TALSPlaybackContext;

  end;

var
  Form1: TForm1;

const
  TexturedAndAntialiased_Helicopter : string=
               '_______      ' + LINEENDING +
               ' __|___      ' + LINEENDING +
               '/      \____/' + LINEENDING +
               '\______/    \';

  ObjectTruck: string=
               '    ___  ' + LINEENDING +
               '___|___\_' + LINEENDING +
               ' O     O ';

  ObjectBar: string=
               '__________' + LINEENDING +
               '|   BAR  |' + LINEENDING +
               '|        |';

implementation

uses Math;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  f: string;
begin
  // For this demo, we need to position sound in space using HRTF -> we have to customize our playback context
  // For that, we use an TALSContextAttributes and set the output mode
  // to ALC_STEREO_HRTF.
  // https://en.wikipedia.org/wiki/Head-related_transfer_function
  FAttribs.InitDefault;  // First, fill attributes with default value
  FAttribs.OutputMode := ALC_STEREO_HRTF;

  // Create a playback context with the default playback device and our custom attributes
  FPlaybackContext := ALSManager.CreatePlaybackContext(-1, FAttribs);

  f := ConcatPaths([Application.Location, '..', 'data']);

  // add a sound to our context. AddSound method load the whole sound data in memory
  FHelicopter := FPlaybackContext.AddSound( ConcatPaths([f, 'helicopterloop.ogg']));
  FHelicopter.PositionRelativeToListener := False; // helicopter position is not relative to the listener
  FHelicopter.Loop := True; // loop the sound
  LabelHelicopter.Caption := TexturedAndAntialiased_Helicopter;

  FMotorTruck1 := FPlaybackContext.AddSound( ConcatPaths([f, 'carengineloop.ogg']));
  FMotorTruck1.PositionRelativeToListener := False;
  FMotorTruck1.Loop := True;
  FMotorTruck1.Pitch.Value := 0.7;
  LabelTruck1.Caption := ObjectTruck;

  FBarMusic := FPlaybackContext.AddStream( ConcatPaths([f, 'MusicEndStage.ogg']));
  FBarMusic.PositionRelativeToListener := False;
  FBarMusic.Loop := True;
  LabelBar.Caption := ObjectBar;

  Randomize;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  // update the HRTF index
  FAttribs.HRTFIndex := ComboBox1.ItemIndex;
  // and try to re-start the playback context with the new attributes
  if not FPlaybackContext.ChangeAttributes( FAttribs ) then
    ShowMessage('Can not apply the new context attributes');
end;

procedure TForm1.ComboBox2Select(Sender: TObject);
begin
  // update the mixing mode in our context attributes
  FAttribs.OutputMode := ALSManager.PlaybackOutputModeIndexToEnum(ComboBox1.ItemIndex);
  // and try to re-start the playback context with the new attributes
  if not FPlaybackContext.ChangeAttributes( FAttribs ) then
    ShowMessage('Can not apply the new context attributes');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // don't forget to free your playback context.
  // All sounds will be automatically freed.
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  txt: string;
begin
  // display some error message or hints
  if FPlaybackContext.Error then
    showMessage( FPlaybackContext.StrError );

  txt:='';
  if not FPlaybackContext.HaveHRTF then txt+='Device can not apply HRTF to position audio in 3D space...'+LINEENDING;
  if not FPlaybackContext.HRTFEnabled then txt+='HRTF is not enabled...';
  if txt<>'' then ShowMessage(txt);

  //fill combobox with available HRTF ( HRTF is relative to a playback context )
  Combobox1.Clear;
  Combobox1.Items.AddStrings( FPlaybackContext.HRTFList );

  if Combobox1.Items.Count = 1 then
    Combobox1.ItemIndex := 0;

  // Fill combobox2 with available output mode
  Combobox2.Clear;
  Combobox2.Items.AddStrings( ALSManager.ListOfPlaybackOutputMode );
  if Combobox2.Items.Count = 7 then
    Combobox2.ItemIndex := 6; // show ALC_STEREO_HRTF by default

  if FHelicopter.Error then
    ShowMessage( FHelicopter.StrError );
  FHelicopter.FadeIn(1.0, 2.0);  // start the sound smoothly

  if FMotorTruck1.Error then
    ShowMessage( FMotorTruck1.StrError );
  FMotorTruck1.FadeIn(1.0, 2.0);

  if FBarMusic.Error then
    ShowMessage( FBarMusic.StrError );
  FBarMusic.FadeIn(1.0, 2.0);

  SetSoundsAttenuation;
  SetSoundsPosition;
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  p: TPoint;
begin
  Timer1.Enabled := False;
  // retrieve mouse coordinates on the window
  p := ScreenToClient( Mouse.CursorPos );
  // sets the position of the listener
  FPlaybackContext.SetListenerPosition( p.x, p.y, 0.0 );
  // sets the position of the headphone on the windows
  Image1.SetBounds( p.x-Image1.Width shr 1, p.y-Image1.Height shr 1,
                    Image1.Width, Image1.Height );
  Timer1.Enabled := True;
end;

procedure TForm1.SetSoundsAttenuation;
var
  m: integer;
begin
  m := Max( Width, Height );

  FHelicopter.DistanceModel := AL_INVERSE_DISTANCE_CLAMPED;
  FHelicopter.Attenuation3D( 80, m, 5.0, 0.0);

  FMotorTruck1.DistanceModel := AL_EXPONENT_DISTANCE;
  FMotorTruck1.Attenuation3D( 20, m, 2.0);

  FBarMusic.DistanceModel := AL_INVERSE_DISTANCE_CLAMPED;
  FBarMusic.Attenuation3D( 20, m, 10.0, 0.0);
end;

procedure TForm1.SetSoundsPosition;
begin
  FHelicopter.Position3D( LabelHelicopter.Left+LabelHelicopter.Width shr 1,
                        LabelHelicopter.Top+LabelHelicopter.Height shr 1, 0.0 );

  FMotorTruck1.Position3D( LabelTruck1.Left+LabelTruck1.Width shr 1,
                             LabelTruck1.Top+LabelTruck1.Height shr 1, 0.0 );
  FBarMusic.Position3D( LabelBar.Left+LabelBar.Width shr 1,
                           LabelBar.Top+LabelBar.Height shr 1, 0.0 );
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  SetSoundsAttenuation;
end;

end.

