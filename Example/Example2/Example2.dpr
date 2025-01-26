program Example2;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;


  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
