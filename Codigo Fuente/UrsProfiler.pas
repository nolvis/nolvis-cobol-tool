{*******************************************************}
{                                                       }
{          Check performance of code in Delphi          }
{                                                       }
{           Copyright (c) 2000 Andrei Roofin            }
{                                                       }
{              www - http://dsr.tomsk.ru                }
{             mail - ray@mail.tomsknet.ru               }
{                                                       }
{*******************************************************}
unit UrsProfiler;

interface

uses Windows, Classes, SysUtils;

const
  rsMaxPointCount = 100;

type
  TrsProfiler = class;

  TrsProfilerPoint = class
  private
    FProfiler: TrsProfiler;
    FAvgTicks: Int64;
    FLastTicks: Int64;
    FMinTicks: Int64;
    FMaxTicks: Int64;
    FCount: integer;
    FStartTicks: Int64;
    function GetAvgTicks: Int64;
  public
    Name: string;
    constructor Create(AOwner: TrsProfiler);
    function AsString: string;
    function GetTimeMS: extended;
    procedure Clear;
    procedure Start;
    procedure Stop;
    property Count: integer read FCount;
    property AvgTicks: Int64 read GetAvgTicks;
    property LastTicks: Int64 read FLastTicks;
    property MaxTicks: Int64 read FMaxTicks;
    property MinTicks: Int64 read FMinTicks;
    property TimeMS: extended read GetTimeMS;
  end;

  TrsProfiler = class
  private
    FList: TList;
    CPUClock: extended;
    function GetPoint(index: integer): TrsProfilerPoint;
  protected
    FStartStopConstant: Int64;
  public
    constructor Create;
    destructor Destroy; override;

    function  Add: integer;
    function  CalibrateCPU: Int64;
    procedure Clear;
    function  Count: integer;
    procedure Delete(const Index: integer);
    function  IndexOfName(const Value: string): integer;
    function  TicksToStr(const Value: Int64): string;
    //
    function  TicksToMS(const Value: Int64): extended;
    //
    property  Points[index: integer]: TrsProfilerPoint read GetPoint; default;
    property  CPUSpeed: extended read CPUClock;
  end;

function GetCPUTick:Int64;

var
  rsProfiler: TrsProfiler;

implementation

function GetCPUTick: Int64;
asm
   DB $0F,$31
end;

function TrsProfiler.CalibrateCPU: Int64;
var
  t: cardinal;
  PriorityClass, Priority: Integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
  //
  t := GetTickCount;
  while t=GetTickCount do;

  Result := GetCPUTick;
  while GetTickCount<(t+400) do;
  Result := GetCPUTick - result;
  CPUClock := 2.5e-6*Result;//# de ciclos de la cpu en un microsegundo
  //
  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
end;

function TrsProfiler.TicksToStr(const Value: Int64): string;
begin
  Result := FloatToStrF(Value/CPUClock,fffixed,10,2)+ ' µs';
end;

{ TrsProfilerPoint }

procedure TrsProfilerPoint.Clear;
begin
  FMinTicks := High(Int64);
  FMaxTicks := 0;
  FAvgTicks := 0;                 
  FLastTicks := 0;
  FCount := 0;
end;

constructor TrsProfilerPoint.Create(AOwner: TrsProfiler);
begin
  FProfiler := AOwner;
  Clear;
end;

function TrsProfilerPoint.AsString: string;
var
  s: string;
begin
  if Name<>'' then s := Name+': ';
  Result := Format('%sCount=%d Min=%s Max=%s Avg=%s Last=%s',
                   [s,Count,
                    FProfiler.TicksToStr(MinTicks),
                    FProfiler.TicksToStr(MaxTicks),
                    FProfiler.TicksToStr(AvgTicks),
                    FProfiler.TicksToStr(LastTicks) ]);
end;

function TrsProfilerPoint.GetAvgTicks: Int64;
begin
  if FCount>0 then Result := FAvgTicks div FCount
   else Result:= 0;
end;

procedure TrsProfilerPoint.Start;
begin
  FStartTicks := GetCPUTick;
end;

procedure TrsProfilerPoint.Stop;
begin
  FLastTicks := GetCPUTick - FStartTicks - FProfiler.FStartStopConstant;
  if FLastTicks<0 then FLastTicks := 0;
  Inc(FCount);
  if FLastTicks<FMinTicks then FMinTicks := FLastTicks;
  if FLastTicks>FMaxTicks then FMaxTicks := FLastTicks;
  Inc(FAvgTicks, FLastTicks);
end;

function TrsProfilerPoint.GetTimeMS: extended;
begin
  Result := FProfiler.TicksToMS(LastTicks);
end;

{ TrsProfiler }

function TrsProfiler.Add: integer;
var
  p: TrsProfilerPoint;
begin
  p := TrsProfilerPoint.Create(Self);
  Result := FList.Add(p);
end;

procedure TrsProfiler.Clear;
begin
  While Count>0 do Delete(0);
end;

function TrsProfiler.Count: integer;
begin
  Result := FList.Count;
end;

constructor TrsProfiler.Create;
begin
  FList := TList.Create;
  CalibrateCPU;

  Add;
  Points[0].Start;
  Points[0].Stop;
  FStartStopConstant := Points[0].FLastTicks;
  Clear;
end;

procedure TrsProfiler.Delete(const Index: integer);
begin
  if (Index>=0) and (Index<FList.Count) then begin
    TrsProfilerPoint(FList.Items[Index]).Free;
    FList.Delete(Index);
  end;
end;

destructor TrsProfiler.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TrsProfiler.GetPoint(index: integer): TrsProfilerPoint;
begin
  if Index<0 then Result := nil
   else
  if Index<FList.Count then Result := FList.Items[Index]
   else
  if Index<rsMaxPointCount then begin
    While Add<Index do ;
    Result := FList.Items[Index];
  end else Result := nil;
end;

function TrsProfiler.IndexOfName(const Value: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i:=Count-1 downto 0 do
    if AnsiCompareText(Value, Points[i].Name)=0 then begin
      Result := i;
      Break;
    end;
end;

function TrsProfiler.TicksToMS(const Value: Int64): extended;
begin
  Result := Value/CPUClock;
end;

initialization
  rsProfiler := TrsProfiler.Create;

finalization
  rsProfiler.Free;
  rsProfiler := nil;

end.
