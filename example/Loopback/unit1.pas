unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, EditBtn, Spin,
  ALSound;

type

  TTrack = record
    Filename: string;
    Sound: TALSSound;
    LabelFilename: TLabel;
    Volume: single;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
    Edit1: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    BPlay: TSpeedButton;
    BStop: TSpeedButton;
    BMixToFile: TSpeedButton;
    Panel6: TPanel;
    Panel7: TPanel;
    ProgressBar1: TProgressBar;
    BCancel: TSpeedButton;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    StaticText1: TStaticText;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    procedure BCancelClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox2Select(Sender: TObject);
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BPlayClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure BMixToFileClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    // our playback context to listen the 3 sounds
    FPlaybackContext: TALSPlaybackContext;
    // our loopback context to mixdown to a file
    FLoopbackContext: TALSLoopbackContext;
    // tracks instance
    FTracks: array[0..2] of TTrack;
    FMixingTime: double;
    procedure InitTracks;
    function GetSampleRate: integer;
    function GetChannel: TALSLoopbackChannel;
    function GetSampleType: TALSLoopbackSampleType;
    procedure EnableMixGUI(aState: boolean);
  private
    procedure ProcessLoopbackContextOnProgress(Sender: TALSLoopbackContext;
      aTimePos: double; const aFrameBuffer: TALSLoopbackFrameBuffer;
      var SaveBufferToFile: boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  i: integer;
begin
  if FPlaybackContext <> NIL then
    exit;

  if not OpenDialog1.Execute then exit;

  i := TBitBtn(Sender).Tag;

  FTracks[i].Filename := OpenDialog1.FileName;
  FTracks[i].LabelFilename.Caption := ExtractFileName( OpenDialog1.FileName );
end;

procedure TForm1.BCancelClick(Sender: TObject);
begin
  // User want to cancel the mix
  BCancel.Tag := 1;
end;

procedure TForm1.ComboBox2Select(Sender: TObject);
begin
  FLoopbackContext := ALSManager.CreateDefaultLoopbackContext;
  if FLoopbackContext.Error then
    ShowMessage(FLoopbackContext.StrError)
  else
  begin
    if FLoopbackContext.IsAttributesSupported(GetSampleRate, GetChannel, GetSampleType) then
      Label12.Caption := ' '
    else
      Label12.Caption := 'NOT SUPPORTED';
  end;

  FreeAndNil(FLoopbackContext);
end;

procedure TForm1.FloatSpinEdit1Change(Sender: TObject);
begin
  // check if Start time is lower than End time
  if FloatSpinEdit2.Value <= FloatSpinEdit1.Value then
    FloatSpinEdit2.Value := FloatSpinEdit1.Value + 0.1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // load OpenAL-Soft and LibSndFile libraries
  ALSManager.LoadLibraries;

  // Updates the file open dialog filter property with supported audio file types.
  OpenDialog1.Filter := ALSManager.DialogFileFilters('Sound files', 'All files');

  // We configure the progress bar that show the mixing channel levels, to fit
  // decibel range from -60dB to 0dB
  ProgressBar2.Min := ALS_DECIBEL_MIN_VALUE;
  ProgressBar2.Max := 0;
  ProgressBar2.Position := ALS_DECIBEL_MIN_VALUE;

  ProgressBar3.Min := ALS_DECIBEL_MIN_VALUE;
  ProgressBar3.Max := 0;
  ProgressBar3.Position := ALS_DECIBEL_MIN_VALUE;

  InitTracks;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FPlaybackContext <> NIL then
    FPlaybackContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Display error message
  if ALSManager.Error then
    ShowMessage(ALSManager.StrError);

  DirectoryEdit1.Text := ExtractFilePath(Application.Location);
  Edit1.Text := '';
  Label12.Caption := ' ';
end;

procedure TForm1.BPlayClick(Sender: TObject);
var
  i: integer;
begin
  BStopClick(NIL);

  // first, (re-)creates a playback context
  FPlaybackContext := ALSManager.CreateDefaultPlaybackContext;
  // then adds the user's sounds
  for i:=0 to High(FTracks) do
  begin
    if FTracks[i].Filename <> '' then
    begin
      FTracks[i].Sound := FPlaybackContext.AddStream(FTracks[i].Filename);
      FTracks[i].Sound.Volume.Value := FTracks[i].Volume;
      FTracks[i].Sound.Play(True);
    end;
  end;
end;

procedure TForm1.BStopClick(Sender: TObject);
var
  i: integer;
begin
  if FPlaybackContext = NIL then
    exit;

  FreeAndNil(FPlaybackContext);

  for i:=0 to High(FTracks) do
    FTracks[i].Sound := NIL;
end;

procedure TForm1.BMixToFileClick(Sender: TObject);
var
  FAttribs: TALSContextAttributes;
  i: integer;
  fileFormat: TALSFileFormat;
  outputFilename: string;
begin
  // Stops the playback.
  BStopClick(NIL);

  // Some check.
  if (Edit1.Text = '') or
     (DirectoryEdit1.Text = '') then
    exit;

  // Creates a loopback context and checks error.
  FLoopbackContext := ALSManager.CreateDefaultLoopbackContext;
  if FLoopbackContext.Error then
  begin
    ShowMessage(FLoopbackContext.StrError);
    FreeAndNil(FLoopbackContext);
    exit;
  end;

  // Checks if the mixing format is supported.
  if not FLoopbackContext.IsAttributesSupported(GetSampleRate, GetChannel, GetSampleType) then
  begin
    ShowMessage('Current mixing format not supported'+lineending+
                'Please, try different setting');
    FreeAndNil(FLoopbackContext);
    exit;
  end;


  EnableMixGUI(False); // Avoid any user's interaction.

  // Customize our context attributes for a loopback context
  FAttribs.InitDefault;  // don't forget this first !
  FAttribs.SetLoopbackMode(GetSampleRate, GetChannel, GetSampleType);

  // Finalize the creation of the loopback context.
  FLoopbackContext.InitContext(FAttribs);
  if FLoopbackContext.Error then
    ShowMessage(FLoopbackContext.StrError);

  // Adds the sounds to our loopback context and play them.
  // Only sounds in playing state will be mixed.
  for i:=0 to High(FTracks) do
    if FTracks[i].Filename <> '' then
    begin
      FTracks[i].Sound := FLoopbackContext.AddStream(FTracks[i].Filename);
      with FTracks[i].Sound do
      begin
        if Error then
          ShowMessage(FTracks[i].Filename+lineending+StrError);
        Volume.Value := FTracks[i].Volume;
        Play(True);
      end;
    end;

  // Prepare output file name with path and '.wav' extension.
  outputFilename := ChangeFileExt(Edit1.Text, '.wav');
  outputFilename := ConcatPaths([DirectoryEdit1.Text, outputFilename]);

  // In this demo, the file output major format is 'wav' and the bit width is
  // the same as the loopback context.
  case ComboBox3.ItemIndex of
    0: fileFormat := ALSMakeFileFormat( SF_FORMAT_WAV, SF_FORMAT_PCM_16);
    1: fileFormat := ALSMakeFileFormat( SF_FORMAT_WAV, SF_FORMAT_PCM_32);
    2: fileFormat := ALSMakeFileFormat( SF_FORMAT_WAV, SF_FORMAT_FLOAT);
  end;

  // Asks the context to save audio to the output file. Shows an error message
  // in case of failure and cancel operation.
  if not FLoopbackContext.PrepareSavingToFile(outputFilename, fileFormat) then
  begin
    ShowMessage('Can not create output file' + LineEnding + outputFilename);
    FreeAndNil(FLoopbackContext);
    EnableMixGUI(True);
    Exit;
  end;

  // Define a callback to update our progress bar, vu-meters and controls the
  // mixing process. This callback will be fired each time a buffer is
  // filled with audio
  FLoopbackContext.OnProgress := @ProcessLoopbackContextOnProgress;

  // We have to call this method before render audio.
  FLoopbackContext.BeginOfMix;

     repeat
       // Ask the context to render 10Ms of audio.
       FLoopbackContext.Mix(0.010);
      until (FMixingTime >= FloatSpinEdit2.Value) or // mixing time reach the end of the interval
            (BCancel.Tag <> 0);                      // user click cancel button

  // We have to call this method at the end, to finalize the mixing process.
  FLoopbackContext.EndOfMix;

  // Checks error only if the mix was not canceled
  if BCancel.Tag = 0 then // if user clicks cancel button, its Tag is sets to 1.
  begin
    // Check mixing error
    if FLoopbackContext.MixingError then
      Showmessage(FLoopbackContext.MixingStrError)
    else
      ShowMessage('Mixdown saved to' + lineending +
                  outputFilename + lineending + 'WITH SUCCESS');
  end
  else BCancel.Tag := 0;

  // Free loopback context (and loopback device)
  FreeAndNil(FLoopbackContext);

  // Prepare for another mixing
  for i:=0 to High(FTracks) do
    FTracks[i].Sound := NIL;
  ProgressBar1.Position := 0;
  ProgressBar2.Position := ALS_DECIBEL_MIN_VALUE;
  ProgressBar3.Position := ALS_DECIBEL_MIN_VALUE;
  EnableMixGUI(True);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  tb: TTrackBar;
  i: integer;
begin
  // user change volume
  tb := Sender as TTrackBar;
  i := tb.Tag;
  FTracks[i].Volume := tb.Position/tb.Max;
  if FTracks[i].Sound <> NIL then
    FTracks[i].Sound.Volume.Value := FTracks[i].Volume;
end;

procedure TForm1.InitTracks;
var
  i: Integer;
begin
  for i:=0 to High(FTracks) do
  begin
    FTracks[i].Sound := NIL;
    FTracks[i].Volume:= ALS_VOLUME_MAX;
  end;
  FTracks[0].LabelFilename := Label1;
  FTracks[1].LabelFilename := Label2;
  FTracks[2].LabelFilename := Label3;
end;

function TForm1.GetSampleRate: integer;
begin
  case ComboBox4.ItemIndex of
    0: Result := 8000;
    1: Result := 11025;
    2: Result := 16000;
    3: Result := 22050;
    4: Result := 44100;
    5: Result := 48000;
    6: Result := 96000;
    7: Result := 176400;
    8: Result := 192000;
    9: Result := 352800;
    10: Result := 384000;
  end;
end;

function TForm1.GetChannel: TALSLoopbackChannel;
begin
  case ComboBox2.ItemIndex of
    0: Result := ALC_MONO_SOFT;
    1: Result := ALC_STEREO_SOFT;
    2: Result := ALC_QUAD_SOFT;
    3: Result := ALC_5POINT1_SOFT;
    4: Result := ALC_6POINT1_SOFT;
    5: Result := ALC_7POINT1_SOFT;
  end;
end;

function TForm1.GetSampleType: TALSLoopbackSampleType;
begin
  case ComboBox3.ItemIndex of
    0: Result := ALC_SHORT_SOFT;
    1: Result := ALC_INT_SOFT;
    2: Result := ALC_FLOAT_SOFT;
  end;
end;

procedure TForm1.EnableMixGUI(aState: boolean);
begin
  Panel1.Enabled := aState;
  Panel2.Enabled := aState;
  Panel3.Enabled := aState;
  Panel4.Enabled := aState;
  Panel5.Enabled := aState;
  Panel6.Enabled := aState;
  Panel7.Enabled := aState;
  BMixToFile.Enabled := aState;
  BCancel.Visible := not aState;
end;

// This is the callback OnProgress from our loopback context, fired when a new
// buffer with audio data is rendered. By default a buffer contains 10ms of audio.
// Here we inform the loopback context if buffer content must be saved to file
// and if mixing must be stopped according to the choosen time interval.
// Futhermore, as we have the rendered buffer, we compute the channels levels and peaks
//
procedure TForm1.ProcessLoopbackContextOnProgress(Sender: TALSLoopbackContext;
  aTimePos: double; const aFrameBuffer: TALSLoopbackFrameBuffer;
  var SaveBufferToFile: boolean);
begin
  FMixingTime := aTimePos;

  // update the progress bar according to the current mixing time position
  ProgressBar1.Position := Round(ProgressBar1.Max*aTimePos/FloatSpinEdit2.Value);

  // Compute channels level (and peak)
  aFrameBuffer.ComputeChannelsLevel;

  // Show current level in decibel
  ProgressBar2.Position := Round(aFrameBuffer.ChannelsLeveldB[0]);
  ProgressBar3.Position := Round(aFrameBuffer.ChannelsLeveldB[1]);

  // Sets the flag 'SaveBufferToFile' to True only when the current time
  // position is inside the interval the user entered.
  SaveBufferToFile := (aTimePos >= FloatSpinEdit1.Value) and
                      (aTimePos <= FloatSpinEdit2.Value);
end;

end.
