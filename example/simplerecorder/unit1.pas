unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, EditBtn, ComCtrls,
  ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    BStart: TSpeedButton;
    BStop: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
    Edit1: TEdit;
    ImageList1: TImageList;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label7: TLabel;
    Panel3: TPanel;
    Label3: TLabel;
    Panel2: TPanel;
    Label10: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Shape1: TShape;
    Timer1: TTimer;
    procedure CheckBox2Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure BStartClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCaptureContext: TALSCaptureContext;
    FFileFormat: longint;
    FFileExt: string;
    function CaptureContextIsReady: boolean;
    function FileParametersAreReady: boolean;
    function UserFile: string;
    procedure UpdateWidgets;
    procedure ProcessApplicationOnIdle(Sender: TObject; var Done: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

uses unit2;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // load OpenAL-Soft and LibSndFile libraries
  ALSManager.LoadLibraries;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Destroy the capture context
  if FCaptureContext <> nil then
    FCaptureContext.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  A: TStringArray;
  i: integer;
  F: ArrayOfALSSimplifiedAudioFileFormat;
begin
  Application.OnIdle := @ProcessApplicationOnIdle;

  // Check if ALSManager encounter an error while loading OpenAL-Soft and LibSndFile
  if ALSManager.Error then
    ShowMessage(ALSManager.StrError);

  // Fill combobox with available capture device names
  ComboBox1.Clear;
  A := ALSManager.ListOfCaptureDeviceName;
  for i := 0 to High(A) do
    ComboBox1.Items.Add(A[i]);

  // Fill listbox for audio file format
  F := ALSManager.ListOfAudioFileFormat_Simplified;
  ListBox1.Clear;
  for i := 0 to High(F) do
    ListBox1.Items.Add(F[i].Name + ' ( .' + F[i].FileExt + ' )'{+'  '+IntToHex(F[i].Format,8)});

  // Init path with the folder path of this application
  DirectoryEdit1.Text := ExtractFilePath(Application.Location);

  UpdateWidgets;
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
var
  i: integer;
begin
  // User have selected a format in the listbox
  i := ListBox1.ItemIndex;
  if i = -1 then
    exit;

  // Ask for the 'catalog' of supported audio file format and their sub-format
  // and keep the major format and its file extension for a later use
  FFileFormat := ALSManager.ListOfAudioFileFormat_Simplified[i].Format;
  FFileExt := ALSManager.ListOfAudioFileFormat_Simplified[i].FileExt;

  if FFileExt='opus' then
  begin
    ShowMessage('Warning: LibSndFile seems to have a problems to save opus file'+lineending+
                'please use another format');
    ListBox1.ItemIndex := -1;
  end;

  UpdateWidgets;
end;

procedure TForm1.BStartClick(Sender: TObject);
var
  res: boolean;
begin
  // checks if user have choosen all parameters
  if not ( CaptureContextIsReady and FileParametersAreReady )then
    exit;

  // Asks to our capture context to save audio data to the specified file.
  // UserFile is a function that construct the path + filename + file extension
  // The format of the output file is retrieved from ALSManager.ListOfSimpleAudioFileFormat[].Format
  res := FCaptureContext.PrepareSavingToFile(UserFile,
        ALSManager.ListOfAudioFileFormat_Simplified[ListBox1.ItemIndex].Format);

  // check if an error occured and show it
  if not res then
    ShowMessage(UserFile + LINEENDING + FCaptureContext.StrCaptureError)
  else
  begin
    // No error -> we can start the capture
    FCaptureContext.StartCapture;
    Label3.Tag := 0;
    Label3.Caption := 'Recording';
    Timer1.Enabled := True;
    BStart.Enabled := False;
    BStop.Enabled := True;
    Panel1.Enabled := FALSE;
    Panel2.Enabled := FALSE;
  end;
end;

procedure TForm1.BStopClick(Sender: TObject);
begin
  // Stop the capture and keep the error value
  FCaptureContext.StopCapture;
  Timer1.Enabled := False;
  Label3.Visible := False;
  BStart.Enabled := True;
  BStop.Enabled := False;
  Panel1.Enabled := True;
  Panel2.Enabled := True;

  // If any show the capture message error
  if FCaptureContext.CaptureError then
    ShowMessage(UserFile+LINEENDING+FCaptureContext.StrCaptureError)
  else // or play the captured audio
    Form2.Play( UserFile );

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // the timer is used to blink a label
  Timer1.Enabled := False;

  case Label3.Tag of
    0: begin
      Label3.Tag := 1;
      Label3.Caption := 'Recording';
    end;
    1: begin
      Label3.Tag := 0;
      Label3.Caption := '';
    end;
  end;

  Timer1.Enabled := True;
end;

function TForm1.CaptureContextIsReady: boolean;
begin
  Result := FCaptureContext <> NIL;
  if Result
    then Result := not FCaptureContext.Error;
end;

function TForm1.FileParametersAreReady: boolean;
begin
  Result := (DirectoryEdit1.Text<>'') and
            (Edit1.Text<>'') and
            (ListBox1.ItemIndex<>-1);
end;

function TForm1.UserFile: string;
var
  fileExt: string;
begin
  fileExt := ALSManager.ListOfAudioFileFormat_Simplified[ListBox1.ItemIndex].FileExt;
  Result := ChangeFileExt(Edit1.Text, '.' + fileExt);
  Result := ConcatPaths([DirectoryEdit1.Text, Result]);
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

  Panel3.Enabled := CaptureContextIsReady and FileParametersAreReady;
end;

procedure TForm1.ProcessApplicationOnIdle(Sender: TObject; var Done: Boolean);
begin
  // Update the progress bars with the channel's level
  if FCaptureContext<>NIL then
  begin
    if not CheckBox1.Checked then
    begin
      // In percent.
      ProgressBar1.Min:=0; ProgressBar1.Max:=100;
      ProgressBar2.Min:=0; ProgressBar2.Max:=100;
      ProgressBar1.Position := Round(FCaptureContext.ChannelsLevel[0]*100);
      ProgressBar2.Position := Round(FCaptureContext.ChannelsLevel[1]*100);
      Label13.Caption := FormatFloat('0.0', FCaptureContext.ChannelsLevel[0]*100)+'%';
      Label14.Caption := FormatFloat('0.0', FCaptureContext.ChannelsLevel[1]*100)+'%';
    end
    else
    begin
      // In decibel.
      ProgressBar1.Min:=ALS_DECIBEL_MIN_VALUE; ProgressBar1.Max:=0;
      ProgressBar2.Min:=ALS_DECIBEL_MIN_VALUE; ProgressBar2.Max:=0;
      ProgressBar1.Position := Round(FCaptureContext.ChannelsLeveldB[0]);
      ProgressBar2.Position := Round(FCaptureContext.ChannelsLeveldB[1]);
      Label13.Caption := FormatFloat('0.0', FCaptureContext.ChannelsLeveldB[0])+'dB';
      Label14.Caption := FormatFloat('0.0', FCaptureContext.ChannelsLeveldB[1])+'dB';
    end;

    Done := not FCaptureContext.MonitoringEnabled;
  end;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
var
  captureFormat: TALSCaptureFormat;
  captureFrequency: longword;
begin
  // Checks if user have choosen all parameters for the selection of capture device
  if (ComboBox1.ItemIndex = -1) or (ComboBox2.ItemIndex = -1) or
    (ComboBox3.ItemIndex = -1) or (ComboBox4.ItemIndex = -1) then
    exit;

  // Destroy the previous capture context
  if FCaptureContext <> nil then
    FCaptureContext.Free;

  // Retrieve format from combobox
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

  // Retrieve the frequency from combobox
  case ComboBox4.ItemIndex of
    0: captureFrequency := 44100;
    1: captureFrequency := 48000;
    2: captureFrequency := 96000;
  end;

  // creates the new capture context
  FCaptureContext := ALSManager.CreateCaptureContext(ComboBox1.ItemIndex,
    captureFrequency, captureFormat, 0.1);

 FCaptureContext.MonitoringEnabled := CheckBox2.Checked;

  // Check error and show the error message
  if FCaptureContext.Error then
    ShowMessage(FCaptureContext.StrError);

  UpdateWidgets;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  if FCaptureContext <> NIL then
    FCaptureContext.MonitoringEnabled := CheckBox2.Checked;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  UpdateWidgets;
end;


end.
