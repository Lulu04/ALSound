unit unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons,
  ALSound;

type

  { TForm2 }

  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FPlaybackContext: TALSPlaybackContext;
    FMusic: TALSSound;
  public
    procedure Play( const aF: string );
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  // update progress bar position
  if FMusic.TotalDuration <> 0 then
    with FMusic do
      ProgressBar1.Position := Round( GetTimePosition / TotalDuration * ProgressBar1.Max );

  Timer1.Enabled := True;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  Timer1.Enabled := False;

  // Stops the sound
  FMusic.Stop;
  // Free the playback context
  FPlaybackContext.Free;
  // Close the playback window
  Close;
end;

procedure TForm2.Play(const aF: string);
var
  attribs: TALSContextAttributes;
begin
  // Creates a playback context
  attribs.InitDefault;
  attribs.ContextUseFloat := True;
  FPlaybackContext := ALSManager.CreatePlaybackContext(-1, attribs);//.CreateDefaultPlaybackContext;

  // Creates an audio stream for playback
  FMusic := FPlaybackContext.AddStream( aF );

  // Show some infos for the user and error message in case of error
  Label1.Caption := aF;
  if not FMusic.Error then
    Label2.Caption := FMusic.SampleRate.ToString+'Hz  -  '+
                      FMusic.ChannelCount.ToString+' channel(s) - '+
                      FMusic.Format+' '+FMusic.SubFormat
  else
    Label2.Caption := FMusic.StrError;

  // Set loop mode
  FMusic.Loop := True;

  // initialize the progress bar and start timer
  ProgressBar1.Position := 0;
  Timer1.Enabled := True;

  // play the sound from beginning
  FMusic.Play(True);

  ShowModal;
end;

end.

