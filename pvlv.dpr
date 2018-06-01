program pvlv;

uses
  Vcl.Forms,
  main in 'Source\main.pas' {MainForm},
  packetdefs in 'Source\packetdefs.pas',
  packetparser in 'Source\packetparser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
