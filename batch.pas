unit batch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmBatch }

  TfrmBatch = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    textURLs: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmBatch: TfrmBatch;

implementation

{$R *.lfm}

{ TfrmBatch }

procedure TfrmBatch.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmBatch.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

