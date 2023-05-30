unit agentform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLIntF, LCLType, miscfunc;

type

  { TfrmUserAgent }

  TfrmUserAgent = class(TForm)
    btnOKS: TButton;
    btnAddS: TButton;
    btnEditS: TButton;
    btnDeleteS: TButton;
    btnResetS: TButton;
    listAgents: TListBox;
    procedure btnAddSClick(Sender: TObject);
    procedure btnDeleteSClick(Sender: TObject);
    procedure btnEditSClick(Sender: TObject);
    procedure btnOKSClick(Sender: TObject);
    procedure btnResetSClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmUserAgent: TfrmUserAgent;

implementation

uses editagent;

{$R *.lfm}

{ TfrmUserAgent }

procedure TfrmUserAgent.btnOKSClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmUserAgent.btnResetSClick(Sender: TObject);
begin
  UserAgents.Clear;
  listAgents.Clear;
  AddDefaultUserAgents(UserAgents,listAgents.Items);
end;

procedure TfrmUserAgent.btnAddSClick(Sender: TObject);
var
  pa: PUserAgent;
  i: integer;
begin
  frmEditUserAgent.textTitle.Text := '';
  frmEditUserAgent.textUserAgent.Text := '';
  frmEditUserAgent.Caption := 'Add UserAgent';
  if frmEditUserAgent.ShowModal = mrOK then
  begin
    new(pa);
    pa^.friendlyName := frmEditUserAgent.textTitle.Text;
    pa^.userAgent := frmEditUserAgent.textUserAgent.Text;
    UserAgents.Add(pa);
    listAgents.Clear;
    for i := 0 to UserAgents.Count -1 do
    begin
      pa := UserAgents[i];
      listAgents.Items.Add(pa^.friendlyName);
    end;
  end;
end;

procedure TfrmUserAgent.btnDeleteSClick(Sender: TObject);
var
  i: integer;
  pa: PUserAgent;
begin
  if listAgents.ItemIndex < 0 then exit;
  UserAgents.Delete(listAgents.ItemIndex);
  listAgents.Items.Clear;
  for i := 0 to UserAgents.Count -1 do
  begin
    pa := UserAgents[i];
    listAgents.Items.Add(pa^.friendlyName);
  end;
end;

procedure TfrmUserAgent.btnEditSClick(Sender: TObject);
var
  pa: PUserAgent;
begin
  if listAgents.ItemIndex < 0 then exit;
  pa := UserAgents[listAgents.ItemIndex];
  frmEditUserAgent.textTitle.Text := pa^.friendlyName;
  frmEditUserAgent.textUserAgent.Text := pa^.userAgent;
  frmEditUserAgent.Caption := 'Edit '+pa^.friendlyName;
  if frmEditUserAgent.ShowModal = mrOK then
  begin
     pa^.friendlyName := frmEditUserAgent.textTitle.Text;
     pa^.userAgent := frmEditUserAgent.textUserAgent.Text;
     UserAgents.Items[listAgents.ItemIndex] := pa;
  end;
end;

end.

