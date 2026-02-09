unit IT.AudioDevice;

{$MODE OBJFPC}
{$H+}
{$COPERATORS ON}
{$MACRO ON}

interface

uses
	Classes, SysUtils,
	IT2play;

type
	TITAudioDeviceBuffer = class
		Module:     TITModule;
		Updating:   Boolean;
		SampleRate: Word;
		Data:       array of Byte;
	end;

	TITAudioDevice = class
	protected
		// mixer callbacks
		function  OpenMixer(Module: TITModule; MixingFrequency, MixingBufferSize: Cardinal): Boolean; virtual; abstract;
		procedure LockMixer(Module: TITModule; Value: Boolean); virtual; abstract;
		procedure CloseMixer(Module: TITModule); virtual; abstract;
		procedure Playback(Module: TITModule; Value: Boolean); virtual; abstract;
	public
		Buffer:     TITAudioDeviceBuffer;
		Frequency:  Word;

		constructor Create(Module: TITModule; SampleRate: Word = 44100); virtual;
		destructor  Destroy; override;
	end;


implementation

{ TITAudioDevice }

constructor TITAudioDevice.Create(Module: TITModule; SampleRate: Word = 44100);
begin
	inherited Create;

	Frequency := SampleRate;
	Buffer := TITAudioDeviceBuffer.Create;
	Buffer.Module := Module;
	Buffer.SampleRate := SampleRate;
	SetLength(Buffer.Data, 1024*4);

	Module.OnOpenMixer  := @OpenMixer;
	Module.OnCloseMixer := @CloseMixer;
	Module.OnLockMixer  := @LockMixer;
	Module.OnPlayback   := @Playback;
end;

destructor TITAudioDevice.Destroy;
begin
	Buffer.Free;

	inherited Destroy;
end;

end.

