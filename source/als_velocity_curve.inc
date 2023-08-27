{$ifdef ALS_INTERFACE}
{
   Velocity curve

  Define some predefined velocity curve that can be used to change
  volume, frequency, pan, etc... over time, linearly or not.
}

function ALSCurveIDToString(ACurveID: TALSCurveID): string;
function ALSStringToCurveID(ACurveName: string): TALSCurveID;

type

  { TALSDataCurve }

  TALSDataCurve = class
  protected
    FID: word;
    FName: string;
    function GetPointCount: integer;
    procedure CreateExtremities;
  public
    Points: array of TPointF;
    constructor Create;
    destructor Destroy; override;
    // tools for curve construct
    procedure Clear(aCreateExtremities: boolean = True);

    procedure DeletePoint(aIndex: integer);
    procedure CopyPointsFrom(const aSource: array of TPointf);
    function ValidIndex(aIndex: integer): boolean;
    {$ifdef LCL}
    // Render curve on given TImage. You have to free it yourself
    procedure DrawOn(aImage: TImage);
    {$endif}
    property Name: string read FName write FName;
    property ID: word read FID;
    property PointCount: integer read GetPointCount;
  end;


  { TALSVelocityCurve }

  TALSVelocityCurve = class
  private
    FDataCurveToUse: TALSDataCurve;
    FFinished, FInvert: boolean;
    FX, FDuration, FYOrigin, FYTarget, FDeltaY: single;
    FCurrentIndexPoint1: integer;
    Fa, Fx1, Fy1, Fx2, Fy2: single;
    procedure GetSegmentCoor;
  public
    constructor Create;
    // initiate calculation
    procedure InitParameters(aCurrentValue, aTargetValue, aSeconds: single;
      aCurveID: TALSCurveID = ALS_Linear);
    // Computes and return the new value according to elapsed time
    function Compute(const AElapsedSec: single): single;
    property Finished: boolean read FFinished write FFinished;
  end;


type

  TALSParamState = (alspsNO_CHANGE, alspsADD_CONSTANT, alspsUSE_CURVE);
  TALSLockParamProc = procedure of object;
  TALSUnlockParamProc = TALSLockParamProc;

  { TALSFParam }

  TALSFParam = class
  private
    FValue, FConstPerSecond: single;
    FState: TALSParamState;
    FCurve: TALSVelocityCurve;
  private
    FOnLockParam: TALSLockParamProc;
    FOnUnlockParam: TALSUnlockParamProc;
    procedure DoLock;
    procedure DoUnlock;
  protected
    function GetValue: single; virtual;
    procedure SetValue(AValue: single); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OnElapse(const AElapsedSec: single); virtual;
    // use velocity curve
    procedure ChangeTo(aNewValue, aSecond: single; aCurveID: TALSCurveID = ALS_Linear); virtual;
    // add a constant per second
    procedure AddConstant(aConstPerSecond: single);
    // Current value of the parameter. Setting a value, stop an "ChangeTo" or "AddConstant" action.
    property Value: single read GetValue write SetValue;
    property State: TALSParamState read FState;
  end;

type
  { TALSCustomBoundedFParam }
  // parameter with boundary

  TALSCustomBoundedFParam = class(TALSFParam)
  private
    FMinValue,
    FMaxValue: single;
    FLoop: boolean;
    procedure ApplyBounds(var AValue: single);
  protected
    function GetValue: single; override;
    procedure SetValue(AValue: single); override;
  public
    // if Loop is set to TRUE, value can loop between bounds (usefull for i.e. [0..360] angle)
    // if it's set to FALSE, value is clamped to FMinValue and FMaxValue.
    constructor Create(aMin, aMax, aStartValue: single; aLooped: boolean=False);
    procedure OnElapse(const AElapsedSec: single); override;
  end;


  TALSSimpleCallBack = procedure of object;

  { TALSBoundedFParam }

  TALSBoundedFParam = class(TALSCustomBoundedFParam)
  private
    FProc: TALSSimpleCallBack;
  protected
    procedure SetValue(AValue: single); override;
  end;


type
  { TALSDataCurveList }

  TALSDataCurveList = class
  private
    FList: TList;
    FNumID: integer;
    function GetDataCurveCount: integer;
    function NextID: integer;
  protected
    procedure Clear;
    procedure DeleteByIndex(aIndex: integer);
    procedure DeleteByID(aIDCurve: word);
    procedure DeleteByName(ACurveName: string);
  public
    constructor Create;
    destructor Destroy; override;
    // Add new curve to the list. Return the ID of the created curve
    function AddCurve(const aName: string; const Pts: array of TPointF): word;
    function GetCurveByID(aID: TALSCurveID): TALSDataCurve;
    function GetCurveByIndex(aIndex: integer): TALSDataCurve;
    function CurveNameAlreadyExist(const aCurveName: string): boolean;
    property Count: integer read GetDataCurveCount;
  end;

var
  ALSVelocityCurveList: TALSDataCurveList;

{$endif}

{$ifdef ALS_IMPLEMENTATION}

function PointF(aX, aY: single): TPointF;
begin
  Result.x := aX;
  Result.y := aY;
end;

function ALSCurveIDToString(ACurveID: TALSCurveID): string;
begin
  Result := ALSVelocityCurveList.GetCurveByIndex(Ord(ACurveID)).Name
end;

function ALSStringToCurveID(ACurveName: string): TALSCurveID;
var
  i: integer;
begin
  for i:=0 to ALSVelocityCurveList.Count-1 do
    if ALSVelocityCurveList.GetCurveByIndex(i).Name = ACurveName then
    begin
      Result := TALSCurveID(i);
      exit;
    end;
  Result := ALS_Linear;
end;

{ TALSDataCurve }

function TALSDataCurve.GetPointCount: integer;
begin
  Result := Length(Points);
end;

procedure TALSDataCurve.CreateExtremities;
begin
  SetLength(Points, 2);
  Points[0] := PointF(0, 1);
  Points[1] := PointF(1, 0);
end;

constructor TALSDataCurve.Create;
begin
  inherited Create;
  CreateExtremities;
end;

destructor TALSDataCurve.Destroy;
begin
  Clear(False);
  inherited Destroy;
end;

procedure TALSDataCurve.Clear(aCreateExtremities: boolean);
begin
  SetLength(Points, 0);
  if aCreateExtremities then
    CreateExtremities;
end;

procedure TALSDataCurve.DeletePoint(aIndex: integer);
var
  i: integer;
begin
  if (aIndex < 1) or (aIndex > Length(Points) - 2) then
    exit;
  for i := GetPointCount - 1 downto aIndex do
    Points[i - 1] := Points[i];
  SetLength(Points, Length(Points) - 1);
end;

procedure TALSDataCurve.CopyPointsFrom(const aSource: array of TPointf);
var
  i: integer;
begin
  Clear(False);
  SetLength(Points, Length(aSource));
  for i := 0 to Length(aSource) - 1 do
    Points[i] := aSource[i];
end;

function TALSDataCurve.ValidIndex(aIndex: integer): boolean;
begin
  Result := (aIndex >= 0) and (aIndex < GetPointCount);
end;

{$ifdef LCL}
procedure TALSDataCurve.DrawOn(aImage: TImage);
var
  x1, y1, x2, y2, i: integer;
  cline, clineinvert: TColor;
begin
  with aImage.Canvas do
  begin
    // background
    Brush.Color := rgbToColor(50, 20, 20);
    FillRect(0, 0, Width, Height);
    cline := rgbToColor(255, 140, 0);
    clineinvert := rgbToColor(20, 80, 100);
    // inverted curve
    Pen.Color := clineinvert;
    for i := 1 to GetPointCount - 1 do
    begin
      with Points[i - 1] do
      begin
        x1 := System.round(x * Width);
        y1 := System.round(Height - y * Height);
      end;
      with Points[i] do
      begin
        x2 := System.round(x * Width);
        y2 := System.round(Height - y * Height);
      end;
      Line(x1, y1, x2, y2);
    end;
    // axis
    Pen.Color := rgbToColor(150, 100, 100);
    Line(0, Height - 1, Width, Height - 1);
    Line(0, Height - 2, Width, Height - 2);
    Line(0, 0, 0, Height);
    Line(1, 0, 1, Height);
    // normal curve
    Pen.Color := cline;
    for i := 1 to GetPointCount - 1 do
    begin
      with Points[i - 1] do
      begin
        x1 := System.round(x * Width);
        y1 := System.round(y * Height);
      end;
      with Points[i] do
      begin
        x2 := System.round(x * Width);
        y2 := System.round(y * Height);
      end;
      Line(x1, y1, x2, y2);
    end;
  end;
end;
{$endif}

{ TALSBoundedFParam }

procedure TALSBoundedFParam.SetValue(AValue: single);
begin
  inherited SetValue(AValue);
  FProc();
end;

{ TALSCustomBoundedFParam }

function TALSCustomBoundedFParam.GetValue: single;
begin
  DoLock;
  try
    Result := FValue;
    ApplyBounds( Result );
  finally
    DoUnlock;
  end;
end;

procedure TALSCustomBoundedFParam.SetValue(AValue: single);
begin
  ApplyBounds(AValue);
  inherited SetValue(AValue);
end;

constructor TALSCustomBoundedFParam.Create(aMin, aMax, aStartValue: single; aLooped: boolean);
begin
  inherited Create;

  if aMin > aMax then
  begin
    FMinValue := aMax;
    FMaxValue := aMin;
  end
  else
  begin
    FMinValue := aMin;
    FMaxValue := aMax;
  end;

  FLoop := aLooped;
  FValue := EnsureRange(aStartValue, FMinValue, FMaxValue);
end;

procedure TALSCustomBoundedFParam.ApplyBounds(var AValue: single);
var
  delta: single;
begin
  if FLoop then
  begin  // loop mode
    delta := FMaxValue - FMinValue;
    while AValue < FMinValue do AValue += delta;
    while AValue > FMaxValue do AValue -= delta;
  end
  else
  begin  // clamp mode
    if AValue < FMinValue then
      AValue := FMinValue
    else if AValue > FMaxValue then
      AValue := FMaxValue;
  end;
end;

procedure TALSCustomBoundedFParam.OnElapse(const AElapsedSec: single);
begin
  case FState of
    alspsADD_CONSTANT:
    begin
      FValue += FConstPerSecond * AElapsedSec;
      if not FLoop then
      begin
        if FValue <= FMinValue then
        begin
          FValue := FMinValue;
          FState := alspsNO_CHANGE;
        end
        else if FValue >= FMaxValue then
        begin
          FValue := FMaxValue;
          FState := alspsNO_CHANGE;
        end;
      end;
    end;

    alspsUSE_CURVE:
    begin
      FValue := FCurve.Compute(AElapsedSec);
      if FCurve.Finished then
        FState := alspsNO_CHANGE;
    end;
  end;
end;


{ TALSFParam }


function TALSFParam.GetValue: single;
begin
  DoLock;
  try
    Result := FValue;
  finally
    DoUnlock;
  end;
end;

procedure TALSFParam.SetValue(AValue: single);
begin
  DoLock;
  try
    FValue := AValue;
    FState := alspsNO_CHANGE;
  finally
    DoUnlock;
  end;
end;

constructor TALSFParam.Create;
begin
  FState := alspsNO_CHANGE;
  FValue := 0.0;
  FConstPerSecond := 0.0;
  FCurve := TALSVelocityCurve.Create;
end;

destructor TALSFParam.Destroy;
begin
  FreeAndNil(FCurve);
  inherited Destroy;
end;

procedure TALSFParam.ChangeTo(aNewValue, aSecond: single; aCurveID: TALSCurveID);
begin
  if aSecond <= 0 then
  begin
    SetValue(aNewValue);
    exit;
  end;

  DoLock;
  try
    if aNewValue <> FValue then
    begin
      FState := alspsUSE_CURVE;
      FCurve.InitParameters(FValue, aNewValue, aSecond, aCurveID);
    end
    else
      FState := alspsNO_CHANGE;
  finally
    DoUnlock;
  end;
end;

procedure TALSFParam.AddConstant(aConstPerSecond: single);
begin
  DoLock;
  try
    if aConstPerSecond <> 0 then
    begin
      FState := alspsADD_CONSTANT;
      FConstPerSecond := aConstPerSecond;
    end
    else
      FState := alspsNO_CHANGE;
  finally
    DoUnlock;
  end;
end;

procedure TALSFParam.OnElapse(const AElapsedSec: single);
begin
  case FState of
    alspsADD_CONSTANT: FValue += FConstPerSecond * AElapsedSec;
    alspsUSE_CURVE: begin
      FValue := FCurve.Compute(AElapsedSec);
      if FCurve.FFinished then
        FState := alspsNO_CHANGE;
    end;
  end;
end;

procedure TALSFParam.DoLock;
begin
  if FOnLockParam <> NIL then
    FOnLockParam();
end;

procedure TALSFParam.DoUnlock;
begin
  if FOnUnlockParam <> NIL then
    FOnUnlockParam();
end;

{ TALSVelocityCurve }

constructor TALSVelocityCurve.Create;
begin
  inherited Create;
  FFinished := True;
  FInvert := False;
end;

procedure TALSVelocityCurve.GetSegmentCoor;
var
  fp, fp1: TPointF;
begin
  fp := FDataCurveToUse.Points[FCurrentIndexPoint1];
  fp1 := FDataCurveToUse.Points[FCurrentIndexPoint1 + 1];
  Fx1 := fp.x;
  Fx2 := fp1.x;

  if FInvert then
  begin
    Fy1 := fp.y;
    Fy2 := fp1.y;
  end
  else
  begin
    Fy1 := 1 - fp.y;
    Fy2 := 1 - fp1.y;
  end;

  Fa := (Fy2 - Fy1) / (Fx2 - Fx1);
end;

procedure TALSVelocityCurve.InitParameters(aCurrentValue, aTargetValue,
  aSeconds: single; aCurveID: TALSCurveID);
begin
  FX := 0;
  FYOrigin := aCurrentValue;
  FYTarget := aTargetValue;

  FInvert := aCurrentValue > aTargetValue;

  FDeltaY := FYTarget - FYOrigin;
  FDuration := aSeconds;
  FFinished := aSeconds = 0.0;
  FCurrentIndexPoint1 := 0;

  FDataCurveToUse := ALSVelocityCurveList.GetCurveByID(aCurveID);
  if FDataCurveToUse = nil then
    raise Exception.Create('Velocities curves error ! no curve available for ID=' +
      IntToStr(Ord(aCurveID)) + ')');

  GetSegmentCoor;
end;

function TALSVelocityCurve.Compute(const AElapsedSec: single): single;
var
  xcurve, ycurve: single;
begin
  FX += AElapsedSec;
  if (FX >= FDuration) or (FDuration <= 0) then
    FFinished := True;
  if FFinished then
  begin
    Result := FYTarget;
    exit;
  end
  else
  begin
    xcurve := FX / FDuration;
    while xcurve > Fx2 do
    begin
      Inc(FCurrentIndexPoint1);
      GetSegmentCoor;
    end;

    ycurve := Fa * (xcurve - Fx1) + Fy1;
    if FInvert then
      ycurve := 1 - ycurve;

    Result := FYOrigin + ycurve * FDeltaY;
  end;
end;


{ TALSDataCurveList }

constructor TALSDataCurveList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FNumID := -1;
end;

destructor TALSDataCurveList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TALSDataCurveList.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TALSDataCurve(FList.Items[i]).Free;
  FList.Clear;
  FNumID := -1;
end;

function TALSDataCurveList.GetDataCurveCount: integer;
begin
  Result := FList.Count;
end;

function TALSDataCurveList.NextID: integer;
begin
  Inc(FNumID);
  Result := FNumID;
end;


function TALSDataCurveList.AddCurve(const aName: string; const Pts: array of TPointF): word;
var
  o: TALSDataCurve;
begin
  o := TALSDataCurve.Create;
  o.Name := aName;
  o.FID := NextID;
  o.CopyPointsFrom(Pts);
  FList.Add(o);
  Result := o.ID;
end;

procedure TALSDataCurveList.DeleteByIndex(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= FList.Count) then
    exit;
  TALSDataCurve(FList.Items[aIndex]).Free;
  FList.Delete(aIndex);
end;

procedure TALSDataCurveList.DeleteByID(aIDCurve: word);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if GetCurveByIndex(i).ID = aIDCurve then
    begin
      DeleteByIndex(i);
      exit;
    end;
end;

procedure TALSDataCurveList.DeleteByName(ACurveName: string);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if GetCurveByIndex(i).Name = ACurveName then
    begin
      DeleteByIndex(i);
      exit;
    end;
end;

function TALSDataCurveList.GetCurveByID(aID: TALSCurveID): TALSDataCurve;
var
  i: integer;
  dc: TALSDataCurve;
begin
  for i := 0 to FList.Count - 1 do
  begin
    dc := GetCurveByIndex(i);
    if dc.ID = Ord(aID) then
    begin
      Result := dc;
      exit;
    end;
  end;
  Result := GetCurveByIndex(0); // Linear curve by default
end;

function TALSDataCurveList.GetCurveByIndex(aIndex: integer): TALSDataCurve;
begin
  if (aIndex < 0) or (aIndex >= FList.Count) then
    Result := TALSDataCurve(FList.Items[0])  // Linear curve by default
  else
    Result := TALSDataCurve(FList.Items[aIndex]);
end;

function TALSDataCurveList.CurveNameAlreadyExist(const aCurveName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
    if GetCurveByIndex(i).FName = aCurveName then
    begin
      Result := True;
      exit;
    end;
end;

{$endif}

{$ifdef ALS_INITIALIZATION}

  ALSVelocityCurveList := TALSDataCurveList.Create;

  ALSVelocityCurveList.AddCurve('Linear', [PointF(0, 1), PointF(1, 0)]);

  ALSVelocityCurveList.AddCurve('StartFastEndSlow', [PointF(0, 1),
    PointF(0.020024182, 0.93652451), PointF(0.040863961, 0.875130057),
    PointF(0.062519327, 0.81581676), PointF(0.084990293, 0.758584738),
    PointF(0.108276844, 0.70343399), PointF(0.132378981, 0.65036422),
    PointF(0.157296717, 0.599375546), PointF(
    0.183030054, 0.550468266), PointF(0.209578991, 0.503642082),
    PointF(0.236943483, 0.458896935),
    PointF(0.265123576, 0.416233003), PointF(
    0.294119269, 0.375650346), PointF(0.323930591, 0.337148845),
    PointF(0.354557455, 0.300728381),
    PointF(0.385999888, 0.266389161), PointF(
    0.418257982, 0.234131128), PointF(0.451331586, 0.20395425),
    PointF(0.48522082, 0.175858527),
    PointF(0.519925535, 0.149843991), PointF(0.555446029, 0.12591061),
    PointF(0.591781974, 0.10405837), PointF(0.628933609, 0.084287308),
    PointF(0.666900694, 0.066597417), PointF(
    0.705683529, 0.050988663), PointF(0.745281875, 0.037461091),
    PointF(0.785695732, 0.026014673),
    PointF(0.826925337, 0.016649416), PointF(
    0.868970394, 0.009365318), PointF(0.911831021, 0.004162385),
    PointF(0.955507398, 0.001040612), PointF(1, 0)]);

  ALSVelocityCurveList.AddCurve('StartSlowEndFast', [PointF(0, 1),
    PointF(0.051241662, 0.998875022), PointF(
    0.101217754, 0.995674491), PointF(0.149928272, 0.990398407),
    PointF(0.197373211, 0.98304683),
    PointF(0.24355258, 0.973619819), PointF(0.288466364, 0.962117136),
    PointF(0.332114607, 0.948538899), PointF(0.374497294, 0.93288511),
    PointF(0.415614307, 0.915156007), PointF(
    0.455465853, 0.895351171), PointF(0.494051784, 0.873470724),
    PointF(0.53137207, 0.849514902),
    PointF(0.56742692, 0.823483407), PointF(0.602216125, 0.79537642),
    PointF(0.635739744, 0.76519376), PointF(0.667997777, 0.732935786),
    PointF(0.698990226, 0.698602259), PointF(0.728717089, 0.66219312),
    PointF(0.757178485, 0.623708546), PointF(0.784374237, 0.58314836),
    PointF(0.810304403, 0.540512621), PointF(
    0.834969103, 0.495801389), PointF(0.85836798, 0.449014604),
    PointF(0.880501449, 0.400152236),
    PointF(0.901369393, 0.349214405), PointF(0.920971751, 0.29620102),
    PointF(0.939308524, 0.241112053), PointF(
    0.956379592, 0.183947578), PointF(0.972185254, 0.124707572),
    PointF(0.98672545, 0.063392036), PointF(1, 0)]);

  ALSVelocityCurveList.AddCurve('Sinusoid', [PointF(0, 1),
    PointF(0.024732787, 0.998385906), PointF(
    0.049185738, 0.993821561), PointF(0.073372178, 0.986447573),
    PointF(0.097305417, 0.976404548),
    PointF(0.1209988, 0.963833213), PointF(0.14446567, 0.948873878),
    PointF(0.167719305, 0.931667387), PointF(
    0.190773025, 0.912354112), PointF(0.213640243, 0.891074657),
    PointF(0.23633422, 0.867969751),
    PointF(0.258868277, 0.843180001), PointF(
    0.281255752, 0.816845596), PointF(0.303509951, 0.789107561),
    PointF(0.325644255, 0.760106564),
    PointF(0.347671956, 0.729982734), PointF(
    0.369606346, 0.698877037), PointF(0.391460717, 0.666929662),
    PointF(0.413248628, 0.634281814),
    PointF(0.434983134, 0.601073563), PointF(
    0.456677645, 0.567445576), PointF(0.478345513, 0.53353852),
    PointF(0.500000119, 0.499493062),
    PointF(0.521654665, 0.465449661), PointF(
    0.543322504, 0.431548983), PointF(0.565017045, 0.397931635),
    PointF(0.586751521, 0.364737958),
    PointF(0.608539283, 0.332108915), PointF(
    0.630393684, 0.300184816), PointF(0.652328074, 0.269106299),
    PointF(0.674355745, 0.23901397),
    PointF(0.69648999, 0.210048467), PointF(0.718744099, 0.182350308),
    PointF(0.741131604, 0.15606007), PointF(0.763665676, 0.131318435),
    PointF(0.786359549, 0.108265862), PointF(
    0.809226751, 0.087043017), PointF(0.832280397, 0.067790486),
    PointF(0.855533957, 0.050648808),
    PointF(0.879000902, 0.035758596), PointF(
    0.902694285, 0.023260433), PointF(0.926627457, 0.013294909),
    PointF(0.95081389, 0.006002605),
    PointF(0.975266695, 0.001524106), PointF(1, 0)]);

  ALSVelocityCurveList.AddCurve('Sinusoid2', [PointF(0, 1),
    PointF(0.036280163, 0.997963548), PointF(0.070699692, 0.992895722),
    PointF(0.103349388, 0.984944582), PointF(0.134319976, 0.974257946),
    PointF(0.16370222, 0.960983336), PointF(0.191586897, 0.945268929),
    PointF(0.218064785, 0.927262723), PointF(0.243226603, 0.907112002),
    PointF(0.267163068, 0.884965181), PointF(0.289965093, 0.86097008),
    PointF(0.311723292, 0.835274339), PointF(0.332528561, 0.808026016),
    PointF(0.35247153, 0.779372931), PointF(0.371643096, 0.749462903),
    PointF(0.390133888, 0.718443811), PointF(0.408034772, 0.686463714),
    PointF(0.425436437, 0.653670132), PointF(0.442429692, 0.620211244),
    PointF(0.459105253, 0.586234868), PointF(0.47555393, 0.551888585),
    PointF(0.491866469, 0.517320752), PointF(0.50813359, 0.48267895),
    PointF(0.52444613, 0.448111027), PointF(0.540894747, 0.413764864),
    PointF(0.557570398, 0.379788399), PointF(0.574563563, 0.34632957),
    PointF(0.591965258, 0.313536048), PointF(0.609866142, 0.281555861),
    PointF(0.628356934, 0.250536829), PointF(0.647528529, 0.220626801),
    PointF(0.667471528, 0.191973701), PointF(0.688276827, 0.164725393),
    PointF(0.710035086, 0.139029726), PointF(0.732836962, 0.115034543),
    PointF(0.756773591, 0.092887774), PointF(0.781935513, 0.072737254),
    PointF(0.808413386, 0.054730862), PointF(0.836297989, 0.03901647),
    PointF(0.865680397, 0.025741952), PointF(0.89665091, 0.015055172),
    PointF(0.929300606, 0.007104006), PointF(0.963720202, 0.002036323),
    PointF(1, 0)]);
{$endif}

{$ifdef ALS_FINALIZATION}
  FreeAndNil(ALSVelocityCurveList);
{$endif}




