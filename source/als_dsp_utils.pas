{
  **************************************************************************
 *                                                                          *
 *  This file is part of ALSound.                                           *
 *                                                                          *
 *  See the file LICENSE included in this distribution,                     *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This software is distributed in the hope of being useful                *
 *  for learning purposes about OpenAL-Soft and LibSndFile                  *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
  **************************************************************************

  ALSound offer a simple and easy way to play, capture and mix sounds
  using OpenAL-Soft and LibSndFile libraries under FreePascal/Lazarus.


 written by Lulu - 2022
}

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

  // Compute the level of each channels, in range of [0..1].
  procedure dsp_ComputeLinearLevel_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint; aTarget: PSingle);
  procedure dsp_ComputeLinearLevel_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint; aTarget: PSingle);

  procedure dsp_FillWithSilence_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint);
  procedure dsp_FillWithSilence_Single(p: PSingle; aFrameCount: longword; aChannelCount: Smallint);

  procedure dsp_FillWithWhiteNoise_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint);
  procedure dsp_FillWithWhiteNoise_Single(p: PSingle; aFrameCount: longword; aChannelCount: Smallint);

  procedure dsp_Amplify_Smallint(p: PSmallint; aFrameCount: longword; aChannelCount: Smallint; aGain: single);
  procedure dsp_Amplify_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint; aGain: single);

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

procedure dsp_ComputeLinearLevel_Smallint(p: PSmallint; aFrameCount: longword;
  aChannelCount: Smallint; aTarget: PSingle);
var
  i: integer;
  tar: PSingle;
begin
  tar := aTarget;
  for i:=1 to aChannelCount do
  begin
    tar^ := 0.0;
    inc(tar);
  end;

  if p = NIL then
    exit;

  // Get peak sample values
  while aFrameCount > 0 do
  begin
    tar := aTarget;
    for i:=1 to aChannelCount do
    begin
      if tar^ < Abs(p^) then
         tar^ := Abs(p^);
      inc( p );
      inc(tar);
    end;
    dec( aFrameCount );
  end;
  // rms
  tar := aTarget;
  for i:=1 to aChannelCount do
  begin
    tar^ := Sqrt(tar^ / 32767);
    inc(tar);
  end;
  // dB
  //for i:=0 to aChannelCount-1 do
  //  Result[i] := 20 * Log10(Sqrt(aTarget[i]));
end;

procedure dsp_ComputeLinearLevel_Float(p: PSingle; aFrameCount: longword;
  aChannelCount: Smallint; aTarget: PSingle);
var
  i: integer;
  tar: PSingle;
begin
  tar := aTarget;
  for i:=1 to aChannelCount do
  begin
    tar^ := 0.0;
    inc(tar);
  end;

  if p = NIL then
    exit;

  // Get peak sample values
  while aFrameCount > 0 do
  begin
    tar := aTarget;
    for i:=1 to aChannelCount do
    begin
      if tar^ < Abs(p^) then
         tar^ := Abs(p^);
      inc( p );
      inc(tar);
    end;
    dec( aFrameCount );
  end;

  tar := aTarget;
  for i:=1 to aChannelCount do
  begin
    if tar^ > 1.0 then
      tar^ := 1.0;
    tar^ := Sqrt(tar^);
    inc(tar);
  end;

  // rms
{  for i:=0 to aChannelCount-1 do
    aTarget[i] := Sqrt(aTarget[i]);    }
  // dB
  //for i:=0 to aChannelCount-1 do
  //  aTarget[i] := 20 * Log10(Sqrt(aTarget[i]));
end;

procedure dsp_FillWithSilence_Smallint(p: PSmallint; aFrameCount: longword;
  aChannelCount: Smallint);
var
  i: Integer;
begin
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := Smallint(0);
      inc(p); //p := p + SizeOf(Smallint);
    end;
    dec(aFrameCount);
  end;
end;

procedure dsp_FillWithSilence_Single(p: PSingle; aFrameCount: longword;
  aChannelCount: Smallint);
var
  i: Integer;
begin
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := 0.0;
      inc(p);
    end;
    dec(aFrameCount);
  end;
end;

procedure dsp_FillWithWhiteNoise_Smallint(p: PSmallint; aFrameCount: longword;
  aChannelCount: Smallint);
var
  i: Integer;
begin
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := Smallint(Random(32768) - Random(32769 ));
      inc(p); //p := p + SizeOf(Smallint);
    end;
    dec(aFrameCount);
  end;
end;

procedure dsp_FillWithWhiteNoise_Single(p: PSingle; aFrameCount: longword;
  aChannelCount: Smallint);
var
  i: Integer;
begin
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := Random - Random;
      inc(p);
    end;
    dec(aFrameCount);
  end;
end;

procedure dsp_Amplify_Smallint(p: PSmallint; aFrameCount: longword;
  aChannelCount: Smallint; aGain: single);
var
  i: Integer;
begin
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := Smallint(EnsureRange(Round(p^*aGain), -32768, 32767));
      inc(p);
    end;
    dec(aFrameCount);
  end;
end;

procedure dsp_Amplify_Float(p: PSingle; aFrameCount: longword;
  aChannelCount: Smallint; aGain: single);
var
  i: Integer;
begin
  while aFrameCount > 0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      p^ := p^ * aGain;
      inc(p);
    end;
    dec(aFrameCount);
  end;
end;



end.

