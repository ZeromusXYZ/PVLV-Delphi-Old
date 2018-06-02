program pvlv;

uses
  Vcl.Forms,
  main in 'Source\main.pas' {MainForm},
  packetdefs in 'Source\packetdefs.pas',
  packetparser in 'Source\packetparser.pas',
  datalookups in 'Source\datalookups.pas',
  searchdialog in 'Source\searchdialog.pas' {DlgSearch};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDlgSearch, DlgSearch);
  Application.Run;
end.
