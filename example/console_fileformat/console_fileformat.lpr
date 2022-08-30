program console_fileformat;

{$mode objfpc}{$H+}

uses
  {$define UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  ALSound;

var
  A: ArrayOfALSAudioFileFormat;
  i, j: integer;

begin
  writeln('LibSndFile version: '+ALSManager.LibSndFileVersion);
  writeln('supported audio format and their sub-format:');
  A := ALSManager.ListOfAudioFileFormat_Complete;
  for i:=0 to High(A) do begin
    writeln('    '+A[i].Name+'   >>>  .'+A[i].FileExt);
    for j:=0 to High(A[i].SubFormat) do
     writeln('        '+A[i].SubFormat[j].Name);
  end;
end.

