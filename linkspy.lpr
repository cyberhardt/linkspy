program linkspy;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, main, about, agentform, editagent, miscfunc, postvars,
  config, batch, resulttab
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmUserAgent, frmUserAgent);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmEditUserAgent, frmEditUserAgent);
  Application.CreateForm(TfrmPostVars, frmPostVars);
  Application.CreateForm(TfrmConfig, frmConfig);
  Application.CreateForm(TfrmBatch, frmBatch);
  Application.Run;
end.

