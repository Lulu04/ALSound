unit als_dsp_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ALS_DECIBEL_MIN_VALUE = -60;

type
  ArrayOfByte = array of byte;
  ArrayOfQWord = array of QWord;
  ArrayOfSmallint = array of SmallInt;
  ArrayOfSingle = array of single;

  // convert a percent value (range is [0..1]) to decibel
  function ALSPercentToDecibel(aValue: single): single;

  // convert an array of values in range [0..1] to dB.
  procedure als_dsp_ValuesToDecibel(p: PSingle; aCount: integer);

  // return the mean of the sample for each channel
  function dsp_Mean_Smallint(p: PSmallInt; aFrameCount: longword; aChannelCount: Smallint ): ArrayOfSmallint;
  function dsp_Mean_Float( p: PSingle; aFrameCount: longword; aChannelCount: Smallint ): ArrayOfSingle;

  // add a value on each sample
  procedure dsp_Add_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint; const aValues: ArrayOfSmallint );
  procedure dsp_Add_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint; const aValues: ArrayOfSingle );

  // remove DC bias
  procedure dsp_RemoveDCBias_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint);
  procedure dsp_RemoveDCBias_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint);

  // return the level of each channels, in range of [0..1]
  function dsp_ComputeLinearLevel_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint): ArrayOfSingle;
  function dsp_ComputeLinearLevel_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint): ArrayOfSingle;

implementation
uses Math;

function ALSPercentToDecibel(aValue: single): single;
begin
  if aValue > 0.0 then
    Result := Max(20*Log10(aValue), ALS_DECIBEL_MIN_VALUE)
  else
    Result := ALS_DECIBEL_MIN_VALUE;
end;

procedure als_dsp_ValuesToDecibel(p: PSingle; aCount: integer);
var
  i: integer;
begin
  for i:=1 to aCount do
  begin
    if p^ > 0.0 then
      p^ := Min(20*Log10( p^ ), ALS_DECIBEL_MIN_VALUE)
    else
      p^ := ALS_DECIBEL_MIN_VALUE;
    inc(p)
  end;
end;

function dsp_Mean_Smallint(p: PSmallInt; aFrameCount: longword; aChannelCount: Smallint): ArrayOfSmallint;
var
  i: longword;
  qwTemp: ArrayOfQWord;
begin
  qwTemp := NIL;
  SetLength( qwTemp, aChannelCount );
  FillChar( qwTemp, SizeOf(QWord)*aChannelCount, $00);
  while aFrameCount>0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      qwTemp[i] := qwTemp[i] + QWord( p^ );
      inc( p );
    end;
    dec( aFrameCount );
  end;
  Result := NIL;
  SetLength( Result, aChannelCount );
  for i:=0 to aChannelCount-1 do
    Result[i] := Round( qwTemp[i] / aFrameCount );
end;

function dsp_Mean_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint): ArrayOfSingle;
var
  i: longword;
begin
  Result := NIL;
  SetLength( Result, aChannelCount );
  FillChar( Result, SizeOf(single)*aChannelCount, $00);
  while aFrameCount>0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      Result[i] := Result[i] + p^;
      inc( p );
    end;
    dec( aFrameCount );
  end;
  for i:=0 to aChannelCount-1 do
    Result[i] := Result[i] / aFrameCount;
end;

procedure dsp_Add_Smallint(p: PSmallint; aFrameCount: longword;
  aChannelCount: Smallint; const aValues: ArrayOfSmallint);
var
  i: longword;
begin
  while aFrameCount>0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := p^ + aValues[i];
      inc( p );
    end;
    dec( aFrameCount );
  end;
end;

procedure dsp_Add_Float(p: PSingle; aFrameCount: longword;
  aChannelCount: Smallint; const aValues: ArrayOfSingle);
var
  i: longword;
begin
  while aFrameCount>0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := p^ + aValues[i];
      inc( p );
    end;
    dec( aFrameCount );
  end;
end;

procedure dsp_RemoveDCBias_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint);
var
  M: ArrayOfSmallint;
  i: integer;
begin
  M := dsp_Mean_SmallInt( p, aFrameCount, aChannelCount );
  for i:=0 to High(M) do
    M[i] := -M[i];
  dsp_Add_Smallint(p, aFrameCount, aChannelCount, M);
end;

procedure dsp_RemoveDCBias_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint);
var
  M: ArrayOfSingle;
  i: integer;
begin
  M := dsp_Mean_Float( p, aFrameCount, aChannelCount );
  for i:=0 to High(M) do
    M[i] := -M[i];
  dsp_Add_Float(p, aFrameCount, aChannelCount, M);
end;

function dsp_ComputeLinearLevel_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint): ArrayOfSingle;
var
  i: integer;
begin
  SetLength(Result{%H-}, aChannelCount);
  for i:=0 to High(Result) do
    Result[i] := 0.0;

  if p = NIL then
    exit;

  // Get peak sample values
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      if Result[i] < Abs(p^) then
         Result[i] := Abs(p^);
      inc( p );
    end;
    dec( aFrameCount );
  end;
  // rms
  for i:=0 to aChannelCount-1 do
    Result[i] := Result[i] / 32767;
  // dB
  //for i:=0 to aChannelCount-1 do
  //  Result[i] := 20 * Log10(Sqrt(Result[i]));
end;

function dsp_ComputeLinearLevel_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint): ArrayOfSingle;
var
  i: integer;
begin
  SetLength(Result{%H-}, aChannelCount);
  for i:=0 to High(Result) do
    Result[i] := 0.0;

  if p = NIL then
    exit;

  // Get peak sample values
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      if Result[i] < Abs(p^) then
         Result[i] := Abs(p^);
      inc( p );
    end;
    dec( aFrameCount );
  end;

  for i:=0 to aChannelCount-1 do
    if Result[i] > 1.0 then
      Result[i] := 1.0;


  // rms
{  for i:=0 to aChannelCount-1 do
    Result[i] := Sqrt(Result[i]);    }
  // dB
  //for i:=0 to aChannelCount-1 do
  //  Result[i] := 20 * Log10(Sqrt(Result[i]));
end;



end.

