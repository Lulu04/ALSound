unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  Spin, ExtCtrls, ComCtrls,
  ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FPlaybackContext: TALSPlaybackContext;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Creates a default playback context with default attributes
  FPlaybackContext := ALSManager.CreateDefaultPlaybackContext;

  // Updates the file open dialog filter property with supported audio file types.
  OpenDialog1.Filter := ALSManager.DialogFileFilters(True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free our playback context
  FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if FPlaybackContext.Error
    then ShowMessage(FPlaybackContext.StrError);

  Timer1.Enabled:=TRUE;
  Label3.Caption:='';
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  FPlaybackContext.Playlist.Play(FloatSpinEdit1.Value);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  FPlaybackContext.Playlist.Pause(FloatSpinEdit1.Value);
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  FPlaybackContext.Playlist.Stop(FloatSpinEdit1.Value);
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
var i: integer;
begin
  if not OpenDialog1.Execute then exit;
  for i:=0 to OpenDialog1.Files.Count-1 do begin
    FPlaybackContext.Playlist.Add(OpenDialog1.Files.Strings[i]);
    ListBox1.Items.Add(ExtractFileName(OpenDialog1.Files.Strings[i]));
  end;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  FPlaybackContext.Playlist.Clear;
  ListBox1.Clear;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  FPlaybackContext.Playlist.Previous(FloatSpinEdit1.Value);
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  FPlaybackContext.Playlist.Next(FloatSpinEdit1.Value);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=FALSE;

  if FPlaybackContext.Error then
  begin
    Label3.Caption := FPlaybackContext.StrError;
    exit;
  end;

  case FPlaybackContext.Playlist.State of

    ALS_PLAYING: begin
      Label3.Caption := (FPlaybackContext.Playlist.CurrentIndex+1).ToString+'/'+
                        FPlaybackContext.Playlist.Count.ToString+' - '+
                        ExtractFileName(FPlaybackContext.Playlist.CurrentFile);
      Label3.Tag:=0;
      Label3.Font.Color:=clDefault;
      ListBox1.ItemIndex:=FPlaybackContext.Playlist.CurrentIndex;
    end;

   ALS_PAUSED: begin
    Label3.Tag := Label3.Tag+1;
    case Label3.Tag of
      0..1: begin
       Label3.Font.Color:=clGray;
      end;
      2: begin
       Label3.Font.Color:=clDefault;
      end;
      3: Label3.Tag := 0;
    end;
    ListBox1.ItemIndex:=FPlaybackContext.Playlist.CurrentIndex;
   end;

   ALS_STOPPED: begin
     Label3.Caption := 'Stopped';
     Label3.Tag:=0;
     Label3.Font.Color:=clDefault;
     ListBox1.ItemIndex:=-1;
   end;

  end;
  Timer1.Enabled:=TRUE;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Label5.Caption:=TrackBar1.Position.ToString+'%';
  FPlaybackContext.Playlist.Volume := TrackBar1.Position/TrackBar1.Max;
end;


end.

