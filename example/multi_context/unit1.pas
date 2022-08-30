unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ALSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
  private
    FContext1,
    FContext2,
    FContext3: TALSPlaybackContext;

    FHelicopter,
    FCarEngine,
    FMusic: TALSSound;

    FEffect1,
    FEffect2,
    FEffect3: TALSEffect;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  f: string;
begin
  f := ConcatPaths([Application.Location, '..', 'data']);

  // Create 3 playback contexts with the default playback device
  FContext1 := ALSManager.CreateDefaultPlaybackContext;
  FContext2 := ALSManager.CreateDefaultPlaybackContext;
  FContext3 := ALSManager.CreateDefaultPlaybackContext;

  // Creates one effect per context
  FEffect1 := FContext1.CreateEffect( AL_EFFECT_ECHO, ECHO_PRESET_CANYON);
  FEffect2 := FContext2.CreateEffect( AL_EFFECT_FLANGER, FLANGER_PRESET_STRONG_FLANGER);
  FEffect3 := FContext3.CreateEffect( AL_EFFECT_CHORUS, CHORUS_PRESET_FLYING_SAUCER);

  // Add one sound per context.
  // method AddSound loads the whole sound data in memory. Suits for short sound.
  FHelicopter := FContext1.AddSound( ConcatPaths([f, 'helicopterloop.ogg']));
  FHelicopter.Loop := True; // loop the sound
  FHelicopter.FadeIn(1.0);  // Start playing and increase volume smoothly from 0 to 1 (max)

  //FContext2.
  FCarEngine := FContext2.AddSound( ConcatPaths([f, 'carengineloop.ogg']));
  FCarEngine.Loop := True;
  FCarEngine.FadeIn(1.0);

  // method AddStream prepares some buffer filled 'on the fly' when the sound is played
  // Suits for long sounds.
  FMusic := FContext3.AddStream( ConcatPaths([f, 'MusicEndStage.ogg']));
  FMusic.Loop := True;
  FMusic.FadeIn(1.0);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
var
  check: TCheckBox;
begin
  check := Sender as TCheckBox;
  case check.Tag of
    0: begin
      if check.Checked then
      begin
        FHelicopter.ApplyEffect( FEffect1 );
        FHelicopter.SetEffectDryWetVolume( FEffect1, TrackBar4.Position/TrackBar4.Max );
      end
      else
        FHelicopter.RemoveEffect( FEffect1 );
    end;

    1: begin
      if check.Checked then
      begin
        FCarEngine.ApplyEffect( FEffect2 );
        FCarEngine.SetEffectDryWetVolume( FEffect2, TrackBar5.Position/TrackBar5.Max );
      end
      else
        FCarEngine.RemoveEffect( FEffect2 );
    end;

    2: begin
      if check.Checked then
      begin
        FMusic.ApplyEffect( FEffect3 );
        FMusic.SetEffectDryWetVolume( FEffect3, TrackBar6.Position/TrackBar6.Max );
      end
      else
        FMusic.RemoveEffect( FEffect3 );
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free the tree contexts
  FContext1.Free;
  FContext2.Free;
  FContext3.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Display some error message or hints
  if FContext1.Error then
    ShowMessage('Context1 error'+LINEENDING+FContext1.StrError);

  if FContext2.Error then
    ShowMessage('Context2 error'+LINEENDING+FContext2.StrError);

  if FContext3.Error then
    ShowMessage('Context3 error'+LINEENDING+FContext3.StrError);

  if not FEffect1.Ready then
    ShowMessage('Effect1 not ready');

  if not FEffect2.Ready then
    ShowMessage('Effect2 not ready');

  if not FEffect3.Ready then
    ShowMessage('Effect3 not ready');
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  t: TTrackBar;
begin
  t := Sender as TTrackBar;
  case t.Tag of
   0: FContext1.MasterGain.Value := t.Position/t.Max;
   1: FContext2.MasterGain.Value := t.Position/t.Max;
   2: FContext3.MasterGain.Value := t.Position/t.Max;
  end;
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
var
  t: TTrackBar;
begin
  t := Sender as TTrackBar;
  case t.Tag of
   0: FHelicopter.SetEffectDryWetVolume( FEffect1, t.Position/t.Max );//    FEffect1.Volume := t.Position/t.Max;
   1: FCarEngine.SetEffectDryWetVolume( FEffect2, t.Position/t.Max );//FEffect2.Volume := t.Position/t.Max;
   2: FMusic.SetEffectDryWetVolume( FEffect3, t.Position/t.Max );//FEffect3.Volume := t.Position/t.Max;
  end;
end;

end.

