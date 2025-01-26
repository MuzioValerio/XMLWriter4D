program Example1;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
