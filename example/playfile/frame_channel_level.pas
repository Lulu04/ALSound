unit frame_channel_level;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  ALSound;

type

  { TFrameChannelsLevel }

  TFrameChannelsLevel = class(TFrame)
    CheckBox1: TCheckBox;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label7: TLabel;
    Panel8: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Timer1: TTimer;
    procedure CheckBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentLeftLevel,
    FCurrentRightLevel: single;
  public
    constructor Create(aOwner: TComponent); override;
    procedure UpdateProgressBar(const aLeft, aRight: single);
  end;

implementation
uses Graphics;

{$R *.lfm}

{ TFrameChannelsLevel }

procedure TFrameChannelsLevel.CheckBox1Change(Sender: TObject);
begin
  // Update the progress bars.
  if CheckBox1.Checked then
  begin
    // In decibel.
    ProgressBar1.Min:=ALS_DECIBEL_MIN_VALUE;
    ProgressBar1.Max:=0;
    ProgressBar2.Min:=ALS_DECIBEL_MIN_VALUE;
    ProgressBar2.Max:=0;
    FCurrentLeftLevel := ALS_DECIBEL_MIN_VALUE;
    FCurrentRightLevel := ALS_DECIBEL_MIN_VALUE;
  end
  else begin
    // In percent.
    ProgressBar1.Min:=0;
    ProgressBar1.Max:=100;
    ProgressBar2.Min:=0;
    ProgressBar2.Max:=100;
    FCurrentLeftLevel := 0;
    FCurrentRightLevel := 0;
  end;
end;

procedure TFrameChannelsLevel.Timer1Timer(Sender: TObject);
begin
  if Label13.Tag > 0 then
    Label13.Tag := Label13.Tag -1
  else
    Label13.Color := $00CAF9CE;

  if Label14.Tag > 0 then
    Label14.Tag := Label14.Tag -1
  else
    Label14.Color := $00CAF9CE;
end;

constructor TFrameChannelsLevel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TFrameChannelsLevel.UpdateProgressBar(const aLeft, aRight: single);

  procedure MarkAsClipped(aLabel: TLabel);
  begin
    aLabel.Color := RGBToColor(251,141,136);
    aLabel.Tag := 6;
  end;

begin
  if not CheckBox1.Checked then
  begin
    // Update progress bar - Percent mode
    if aLeft >= FCurrentLeftLevel then
      FCurrentLeftLevel := aLeft
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-aLeft)*0.1;

    if aRight >= FCurrentRightLevel then
      FCurrentRightLevel := aRight
    else
      FCurrentRightLevel := FCurrentRightLevel-(FCurrentRightLevel-aRight)*0.1;

    ProgressBar1.Position := Round(FCurrentLeftLevel*100);
    ProgressBar2.Position := Round(FCurrentRightLevel*100);
    Label13.Caption := FormatFloat('0.0', aLeft*100)+'%';
    Label14.Caption := FormatFloat('0.0', aRight*100)+'%';
  end
  else
  begin
    // Update progress bar - Decibel mode
    if aLeft >= FCurrentLeftLevel then
      FCurrentLeftLevel := aLeft
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-aLeft)*0.1;

    if aRight >= FCurrentRightLevel then
      FCurrentRightLevel := aRight
    else
      FCurrentRightLevel := FCurrentRightLevel-(FCurrentRightLevel-aRight)*0.1;

    ProgressBar1.Position := Round(FCurrentLeftLevel);
    ProgressBar2.Position := Round(FCurrentRightLevel);
    Label13.Caption := FormatFloat('0.0', aLeft)+'dB';
    Label14.Caption := FormatFloat('0.0', aRight)+'dB';
  end;

  // if the max amplitude is reach the corresponding label becomes red to signal
  // the audio can be clipped.
  if aLeft >= 1.0 then
    MarkAsClipped( Label13 );

  if aRight >= 1.0 then
    MarkAsClipped( Label14 );
end;

end.

