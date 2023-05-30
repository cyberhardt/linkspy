unit editagent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, miscfunc;

type

  { TfrmEditUserAgent }

  TfrmEditUserAgent = class(TForm)
    btnOKS: TButton;
    btnCancelS: TButton;
    textTitle: TLabeledEdit;
    textUserAgent: TLabeledEdit;
    procedure btnCancelSClick(Sender: TObject);
    procedure btnOKSClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmEditUserAgent: TfrmEditUserAgent;

implementation

{$R *.lfm}

{ TfrmEditUserAgent }

procedure TfrmEditUserAgent.btnOKSClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmEditUserAgent.btnCancelSClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

