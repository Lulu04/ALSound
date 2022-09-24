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
    procedure UpdateProgressBar(const aBuf: TALSFrameBufferBase);
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

procedure TFrameChannelsLevel.UpdateProgressBar(const aBuf: TALSFrameBufferBase);
var
  v: single;

  procedure MarkAsClipped(aLabel: TLabel);
  begin
    aLabel.Color := RGBToColor(251,141,136);
    aLabel.Tag := 6;
  end;

begin
  if not CheckBox1.Checked then
  begin
    // Update progress bar - Percent mode
    v := aBuf.ChannelsLevel[0];
    if v >= FCurrentLeftLevel then
      FCurrentLeftLevel := v
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-v)*0.1;

    v := aBuf.ChannelsLevel[1];
    if v >= FCurrentRightLevel then
      FCurrentRightLevel := v
    else
      FCurrentRightLevel := FCurrentRightLevel-(FCurrentRightLevel-v)*0.1;

    ProgressBar1.Position := Round(FCurrentLeftLevel*100);
    ProgressBar2.Position := Round(FCurrentRightLevel*100);
    Label13.Caption := FormatFloat('0.0', aBuf.ChannelsLevel[0]*100)+'%';
    Label14.Caption := FormatFloat('0.0', aBuf.ChannelsLevel[1]*100)+'%';
  end
  else
  begin
    // Update progress bar - Decibel mode
    v := aBuf.ChannelsLeveldB[0];
    if v >= FCurrentLeftLevel then
      FCurrentLeftLevel := v
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-v)*0.1;

    v := aBuf.ChannelsLeveldB[1];
    if v >= FCurrentRightLevel then
      FCurrentRightLevel := v
    else
      FCurrentRightLevel := FCurrentRightLevel-(FCurrentRightLevel-v)*0.1;

    ProgressBar1.Position := Round(FCurrentLeftLevel);
    ProgressBar2.Position := Round(FCurrentRightLevel);
    Label13.Caption := FormatFloat('0.0', aBuf.ChannelsLeveldB[0])+'dB';
    Label14.Caption := FormatFloat('0.0', aBuf.ChannelsLeveldB[1])+'dB';
  end;

  // if the max amplitude is reach the corresponding label becomes red to signal
  // the audio can be clipped.
  if aBuf.ChannelsLevel[0] >= 1.0 then
    MarkAsClipped( Label13 );

  if aBuf.ChannelsLevel[1] >= 1.0 then
    MarkAsClipped( Label14 );
end;

end.

