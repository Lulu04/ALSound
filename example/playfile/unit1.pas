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
    ImageList1: TImageList;
    Label1: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    OD1: TOpenDialog;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure BLoadClick(Sender: TObject);
    procedure BPlayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private
    // our playback context
    FPlaybackContext: TALSPlaybackContext;
    // our sound object
    FSound: TALSSound;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create a playback context with the default playback device and default attributes.
  FPlaybackContext := ALSManager.CreateDefaultPlaybackContext;

  // Fill the file open dialog filter property with supported audio file types.
  OD1.Filter := ALSManager.DialogFileFilters(True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free our playback context
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Display some error message or hints.
  if FPlaybackContext.Error then
    ShowMessage(FPlaybackContext.StrError);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  // Reposition tone cursor to middle (normal).
  if FSound <> NIL then
    TrackBar2.Position := TrackBar1.Max div 2;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  // User change sound's volume.
  if FSound <> NIL then Exit;
    FSound.Volume.Value := TrackBar1.Position/TrackBar1.Max;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  // User change sound's tone.
  if FSound <> NIL then
    FSound.Tone.Value := TrackBar2.Position/TrackBar2.Max;
end;

procedure TForm1.BLoadClick(Sender: TObject);
var
  s: string;
begin
  if not OD1.Execute then exit;

  Label11.Caption := ExtractFileName( OD1.FileName );

  // Free the previous (if any)
  if FSound <> NIL then
    FSound.Kill;    // we can also do FPlaybackContext.Delete( FSound );

  // Creates the new one as stream
  FSound := FPlaybackContext.AddStream( OD1.FileName );

  // Checks if an error occured
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

  // Sets loop mode
  FSound.Loop := True;
end;

procedure TForm1.BPlayClick(Sender: TObject);
begin
  if FSound = NIL then Exit;

  if Sender = BPlay then
    FSound.Play(True);

  if Sender = BPause then
    FSound.Pause;

  if Sender = BStop then
    FSound.Stop;
end;

end.

