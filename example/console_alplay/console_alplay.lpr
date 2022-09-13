program console_alplay;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, crt,
  ALSound;

var
  OurPlaybackContext: TALSPlaybackContext;
  OurMusic: TALSSound;
  flagShowUsage: boolean;
  WantedDevice,
  WantedFilename: string;

begin
  // load OpenAL-Soft and LibSndFile libraries
  ALSManager.LoadLibraries;
  if ALSManager.Error then
  begin
    writeln(ALSManager.StrError);
    exit;
  end;

  WantedFilename:='';
  case ParamCount of
    1: begin
        flagShowUsage := (ParamStr(1)='--help') or (ParamStr(1)='-h');
        WantedDevice := '';
        WantedFilename := ParamStr(1);
    end;
    3: begin
        flagShowUsage := ParamStr(1)<>'-device';
        WantedDevice := ParamStr(2);
        WantedFilename := ParamStr(3);
    end
    else flagShowUsage:=TRUE;
  end;

  if flagShowUsage then begin
    writeln('Usage: '+ExtractFileName(ParamStr(0))+' [-device <name>] <filename>');
    exit;
  end;

  OurPlaybackContext := ALSManager.CreateDefaultPlaybackContext;
  if OurPlaybackContext.Error then begin
    writeln('!!! '+OurPlaybackContext.StrError+' !!!');
    OurPlaybackContext.Free;
    exit;
  end;

  OurMusic := OurPlaybackContext.AddStream( WantedFilename );
  if not OurMusic.Error then
  begin
    OurMusic.Play(TRUE);
    while OurMusic.State = ALS_PLAYING do
    begin
      write(#13+'Press a key to stop  -  Played '+
            FormatFloat('0.0', OurMusic.GetTimePosition)+'/'+
            FormatFloat('0.0', OurMusic.TotalDuration)+' s');
      sleep(200);
      if KeyPressed then OurMusic.Stop;
    end;
  end
  else writeln('ERROR >>  '+ OurMusic.StrError);

  OurPlaybackContext.Free;
end.

