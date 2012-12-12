program Analizador;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CargadorCobol in 'CargadorCobol.pas',
  UPalabrasRes in 'UPalabrasRes.pas',
  UrsProfiler in 'UrsProfiler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
