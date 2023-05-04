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
  ArrayOfSingle = array of Single;
  ArrayOfDouble = array of Double;

  TSplittedChannelsSmallInt = array of ArrayOfSmallint;
  TSplittedChannelsSingle = array of ArrayOfSingle;
  TSplittedChannelsDouble = array of ArrayOfDouble;

  // use LinearTodB instead
  function ALSPercentToDecibel(aValue: single): single; deprecated;
  function LinearTodB(aLinearValue: single): single;
  function dBToLinear(adBValue: single): single;

  // Splits interleaved channels
  function InterleavedToSplitted_Smallint(p: PSmallInt; aFrameCount: longword; aChannelCount: Smallint): TSplittedChannelsSmallInt;
  function InterleavedToSplitted_Float(p: PSingle; aFrameCount: longword; aChannelCount: Smallint): TSplittedChannelsSingle;
  function InterleavedToSplitted_Double(p: PDouble; aFrameCount: longword; aChannelCount: Smallint): TSplittedChannelsDouble;

  // Recompose interleaved channels
  procedure SplittedToInterleaved_Smallint(const A: TSplittedChannelsSmallInt; p: PSmallInt);
  procedure SplittedToInterleaved_Float(const A: TSplittedChannelsSingle; p: PSingle);
  procedure SplittedToInterleaved_Double(const A: TSplittedChannelsDouble; p: PDouble);

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

  procedure dsp_AmplifySample_Smallint(p: PSmallint; aFrameIndex: longword; aChannelCount: Smallint; aGain: single); inline;
  procedure dsp_AmplifySample_Float(p: PSingle; aFrameIndex: longword; aChannelCount: Smallint; aGain: single); inline;
implementation
uses Math;

function ALSPercentToDecibel(aValue: single): single;
begin
  Result := LinearTodB(aValue);
end;

function LinearTodB(aLinearValue: single): single;
begin
  if aLinearValue > 0.0 then
    Result := Max(20*Log10(aLinearValue), ALS_DECIBEL_MIN_VALUE)
  else
    Result := ALS_DECIBEL_MIN_VALUE;
end;

function dBToLinear(adBValue: single): single;
begin
  Result := Power(10, adBValue*0.05); // *0.05 same than /20
end;

function InterleavedToSplitted_Smallint(p: PSmallInt; aFrameCount: longword;
  aChannelCount: Smallint): TSplittedChannelsSmallInt;
var ichan: integer;
  isamp: longword;
begin
  Result := NIL;
  if aChannelCount = 0 then exit;
  if aFrameCount = 0 then exit;

  SetLength(Result, aChannelCount, aFrameCount);
  while aFrameCount > 0 do begin
    isamp := 0;
    for ichan:=0 to aChannelCount-1 do begin
      Result[ichan, isamp] := p^;
      inc(p);
      inc(isamp);
    end;

    dec(aFrameCount);
  end;
end;

function InterleavedToSplitted_Float(p: PSingle; aFrameCount: longword;
  aChannelCount: Smallint): TSplittedChannelsSingle;
var ichan: integer;
  isamp: longword;
begin
  Result := NIL;
  if aChannelCount = 0 then exit;
  if aFrameCount = 0 then exit;

  SetLength(Result, aChannelCount, aFrameCount);
  while aFrameCount > 0 do begin
    isamp := 0;
    for ichan:=0 to aChannelCount-1 do begin
      Result[ichan, isamp] := p^;
      inc(p);
      inc(isamp);
    end;

    dec(aFrameCount);
  end;
end;

function InterleavedToSplitted_Double(p: PDouble; aFrameCount: longword;
  aChannelCount: Smallint): TSplittedChannelsDouble;
var ichan: integer;
  isamp: longword;
begin
  Result := NIL;
  if aChannelCount <= 0 then exit;
  if aFrameCount <= 0 then exit;

  SetLength(Result, aChannelCount, aFrameCount);

  while aFrameCount > 0 do begin
    isamp := 0;
    for ichan:=0 to aChannelCount-1 do begin
      Result[ichan, isamp] := p^;
      inc(p);
      inc(isamp);
    end;

    dec(aFrameCount);
  end;
end;

procedure SplittedToInterleaved_Smallint(const A: TSplittedChannelsSmallInt;
  p: PSmallInt);
var
  samp, chan: Integer;
begin
  for samp:=0 to High(A[0]) do
   for chan:=0 to High(A) do begin
    p^ := A[chan, samp];
    inc(p);
   end;
end;

procedure SplittedToInterleaved_Float(const A: TSplittedChannelsSingle; p: PSingle);
var
  samp, chan: Integer;
begin
  for samp:=0 to High(A[0]) do
   for chan:=0 to High(A) do begin
    p^ := A[chan, samp];
    inc(p);
   end;
end;

procedure SplittedToInterleaved_Double(const A: TSplittedChannelsDouble; p: PDouble);
var
  samp, chan: Integer;
begin
  for samp:=0 to High(A[0]) do
   for chan:=0 to High(A) do begin
    p^ := A[chan, samp];
    inc(p);
   end;
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
  i, fc: longword;
begin
  Result := NIL;
  SetLength( Result, aChannelCount );
  FillChar( Result[0], SizeOf(single)*aChannelCount, $00);

  fc := aFrameCount;
  while fc>0 do
  begin
    for i:=0 to aChannelCount-1 do
    begin
      Result[i] := Result[i] + p^;
      inc( p );
    end;
    dec( fc );
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
    tar^ := tar^ / 32767;
    inc(tar);
  end;
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
    inc(tar);
  end;
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

procedure dsp_AmplifySample_Smallint(p: PSmallint; aFrameIndex: longword;
  aChannelCount: Smallint; aGain: single);
begin
  inc(p, aFrameIndex*aChannelCount);
  while aChannelCount > 0 do
  begin
    p^ := Smallint(EnsureRange(Round(p^*aGain), -32768, 32767));
    inc(p);
    dec(aChannelCount);
  end;
end;

procedure dsp_AmplifySample_Float(p: PSingle; aFrameIndex: longword;
  aChannelCount: Smallint; aGain: single);
begin
  inc(p, aFrameIndex*aChannelCount);
  while aChannelCount > 0 do
  begin
    p^ := p^ * aGain;
    inc(p);
    dec(aChannelCount);
  end;
end;



end.

