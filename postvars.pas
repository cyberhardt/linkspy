unit postvars;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Menus,
  StdCtrls, miscfunc;

type

  { TfrmPostVars }

  TfrmPostVars = class(TForm)
    btnOKS: TButton;
    btnCancelS: TButton;
    btnLoadS: TButton;
    btnSaveS: TButton;
    checkClear: TCheckBox;
    editorGrid: TStringGrid;
    menuDeleteItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure btnCancelSClick(Sender: TObject);
    procedure btnLoadSClick(Sender: TObject);
    procedure btnOKSClick(Sender: TObject);
    procedure btnSaveSClick(Sender: TObject);
    procedure editorGridEditingDone(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure menuDeleteItemClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ResetGrid;
  end;

var
  frmPostVars: TfrmPostVars;

implementation

{$R *.lfm}

{ TfrmPostVars }

procedure TfrmPostVars.ResetGrid;
begin
  editorGrid.Clear;
  editorGrid.Columns[0].Title.Caption := 'Key';
  editorGrid.Columns[1].Title.Caption := 'Value';
  editorGrid.RowCount := 2;
end;

procedure TfrmPostVars.FormShow(Sender: TObject);
begin
  editorGrid.Columns.Items[0].Width := editorGrid.ClientWidth div 2;
  editorGrid.Columns.Items[1].Width := editorGrid.ClientWidth div 2;
end;

procedure TfrmPostVars.menuDeleteItemClick(Sender: TObject);
var
  i: integer;
begin
  if editorGrid.Row > 0 then
  begin
    for i := editorGrid.Row to editorGrid.RowCount - 2 do
    begin
      editorGrid.Rows[i].Assign(editorGrid.Rows[i + 1]);
    end;
    editorGrid.RowCount := editorGrid.RowCount - 1;
  end;
end;

procedure TfrmPostVars.PopupMenu1Popup(Sender: TObject);
begin
  if editorGrid.Row > 0 then menuDeleteItem.Enabled := true
  else menuDeleteItem.Enabled := false;
end;

procedure TfrmPostVars.editorGridEditingDone(Sender: TObject);
begin
  if (editorGrid.Col = editorGrid.ColCount -1) and (editorGrid.Row = editorGrid.RowCount -1) then
  begin
    editorGrid.RowCount := editorGrid.RowCount + 1;
    editorGrid.Col := 0;
    editorGrid.Row := editorGrid.RowCount -1;
    editorGrid.EditorMode := true;
    editorGrid.SetFocus;
  end;
end;

procedure TfrmPostVars.btnOKSClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmPostVars.btnSaveSClick(Sender: TObject);
var
  i: Integer;
  pv: TPostVarList;
  f: String;
begin
  SaveDialog1.Filter := OpenDialog1.Filter;
  if SaveDialog1.Execute then
  begin
    pv := TPostVarList.Create;
    for i := 0 to editorGrid.RowCount -1 do
    begin
      if (editorGrid.Cells[0,i] <> '') and (editorGrid.Cells[1,i] <> '') then
        pv.Add(editorGrid.Cells[0,i],editorGrid.Cells[1,i]);
    end;
    if ExtractFileExt(SaveDialog1.FileName) <> '.pv' then
      f := SaveDialog1.FileName + '.pv'
    else
      f := SaveDialog1.FileName;
    pv.SaveToFile(f);
    pv.Free;
  end;
end;

procedure TfrmPostVars.btnCancelSClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmPostVars.btnLoadSClick(Sender: TObject);
var
  i: Integer;
  pv: TPostVarList;
begin
  if OpenDialog1.Execute then
  begin
    pv := TPostVarList.Create;
    pv.LoadFromFile(OpenDialog1.FileName);
    editorGrid.RowCount := pv.Count+1;
    for i := 0 to pv.Count -1 do
    begin
      editorGrid.Cells[0,i+1] := pv[i].key;
      editorGrid.Cells[1,i+1] := pv[i].value;
    end;
    pv.Free;
  end;
end;

end.

