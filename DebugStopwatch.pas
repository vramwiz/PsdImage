unit DebugStopwatch;

interface

uses
  System.SysUtils, Winapi.Windows;

type
  TDebugStopwatch = record
  private
    FStartCounter: Int64;
    class var FFrequency: Int64;
  public
    class constructor Create;
    procedure Start;
    procedure Stop(const Tag: string = '');
  end;

// 手軽に使えるグローバル関数形式も用意
procedure StopTimStart(const Index: Integer);
procedure StopTimStop(const Index: Integer; const Tag: string = '');

implementation

var
  GPerfCounter: array[0..9] of Int64;

{ TDebugStopwatch }

class constructor TDebugStopwatch.Create;
begin
  QueryPerformanceFrequency(FFrequency);
end;

procedure TDebugStopwatch.Start;
begin
  QueryPerformanceCounter(FStartCounter);
end;

procedure TDebugStopwatch.Stop(const Tag: string);
var
  StopCounter, Elapsed: Int64;
  Ms: Double;
  Msg: string;
begin
  QueryPerformanceCounter(StopCounter);
  Elapsed := StopCounter - FStartCounter;
  Ms := (Elapsed / FFrequency) * 1000.0;
  Msg := Format('Stopwatch: %s = %.3f ms', [Tag, Ms]);
  OutputDebugString(PChar(Msg));
end;

{ グローバル互換関数 }

procedure StopTimStart(const Index: Integer);
begin
  if (Index < Low(GPerfCounter)) or (Index > High(GPerfCounter)) then
    Exit;
  QueryPerformanceCounter(GPerfCounter[Index]);
end;

procedure StopTimStop(const Index: Integer; const Tag: string);
var
  StopCounter, Elapsed: Int64;
  Ms: Double;
  Msg: string;
  Freq: Int64;
begin
  if (Index < Low(GPerfCounter)) or (Index > High(GPerfCounter)) then
    Exit;
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(StopCounter);
  Elapsed := StopCounter - GPerfCounter[Index];
  Ms := (Elapsed / Freq) * 1000.0;
  Msg := Format('StopTim[%d] %s = %.3f ms', [Index, Tag, Ms]);
  OutputDebugString(PChar(Msg));
end;

end.

