unit config;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, miscfunc;

type

  { TfrmConfig }

  TfrmConfig = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnBGColour: TColorButton;
    btnForeColour: TColorButton;
    btnBackColour: TColorButton;
    btnDefault: TButton;
    Label4: TLabel;
    listItems: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    editPreview: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    textFont: TComboBox;
    Label1: TLabel;
    textFontSize: TSpinEdit;
    btnClearHistory: TToggleBox;
    btnItalic: TToggleBox;
    btnUnderline: TToggleBox;
    btnBold: TToggleBox;
    procedure btnBackColourColorChanged(Sender: TObject);
    procedure btnBGColourColorChanged(Sender: TObject);
    procedure btnBoldChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnForeColourColorChanged(Sender: TObject);
    procedure btnItalicChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUnderlineChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure listItemsSelect(Sender: TObject);
    procedure textFontChange(Sender: TObject);
    procedure textFontSizeChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ClearHistory: Boolean;
    FontName: String;
  end;

var
  frmConfig: TfrmConfig;

implementation

{$R *.lfm}

{ TfrmConfig }

procedure TfrmConfig.FormCreate(Sender: TObject);
begin
  ClearHistory := false;
end;

procedure TfrmConfig.FormShow(Sender: TObject);
var
  SL: TStringList;
  i: Integer;
  W1, W2: Integer;
begin
  // Reset clear history button
  btnClearHistory.Checked := false;
  if textFont.Items.Count > 0 then exit;
  { Populate font list box with only fixed width fonts, based on code from
    http://forum.lazarus.freepascal.org/index.php?topic=20193.0 }
  SL := TStringList.Create;
  try
    for i := 0 to Screen.Fonts.Count - 1 do
    begin
      Label2.Font.Name := Screen.Fonts.Strings[i];
      if (Label2.Font.IsMonoSpace) and (not IsWindows) then SL.Add(Screen.Fonts.Strings[i])
      else
      begin
        W1 := Label2.Canvas.TextWidth('WMWMWM');
        W2 := Label2.Canvas.TextWidth('iiiiii');
        if W1 = W2 then SL.Add(Label2.Font.Name);
        Application.ProcessMessages;
      end;
    end;
    textFont.Items.AddStrings(SL);
  finally
    SL.free;
  end;
  for i := 0 to textFont.Items.Count -1 do
  begin
    if textFont.Items[i] = FontName then textFont.ItemIndex := i;
  end;
  editPreview.Font.Name := FontName;
  editPreview.Font.Size := textFontSize.Value;
end;

procedure TfrmConfig.btnOKClick(Sender: TObject);
begin
  ClearHistory := btnClearHistory.Checked;
  ModalResult := mrOK;
end;

procedure TfrmConfig.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmConfig.btnDefaultClick(Sender: TObject);
begin
  editPreview.Color := clWhite;
  btnBGColour.ButtonColor := clWhite;
  SetDefaultTheme(SynHTMLSyn1);
end;

procedure TfrmConfig.btnBGColourColorChanged(Sender: TObject);
begin
  editPreview.Color := btnBGColour.ButtonColor;
end;

procedure TfrmConfig.btnBackColourColorChanged(Sender: TObject);
begin
  case listItems.ItemIndex of
    0: exit;
    1: SynHTMLSyn1.CommentAttri.Background := btnBackColour.ButtonColor;
    2: SynHTMLSyn1.IdentifierAttri.Background := btnBackColour.ButtonColor;
    3: SynHTMLSyn1.KeyAttri.Background := btnBackColour.ButtonColor;
    4: SynHTMLSyn1.SymbolAttri.Background := btnBackColour.ButtonColor;
    5: SynHTMLSyn1.TextAttri.Background := btnBackColour.ButtonColor;
    6: SynHTMLSyn1.ValueAttri.Background := btnBackColour.ButtonColor;
  end;
end;

procedure TfrmConfig.btnForeColourColorChanged(Sender: TObject);
begin
  case listItems.ItemIndex of
    0: exit;
    1: SynHTMLSyn1.CommentAttri.Foreground := btnForeColour.ButtonColor;
    2: SynHTMLSyn1.IdentifierAttri.Foreground := btnForeColour.ButtonColor;
    3: SynHTMLSyn1.KeyAttri.Foreground := btnForeColour.ButtonColor;
    4: SynHTMLSyn1.SymbolAttri.Foreground := btnForeColour.ButtonColor;
    5: SynHTMLSyn1.TextAttri.Foreground := btnForeColour.ButtonColor;
    6: SynHTMLSyn1.ValueAttri.Foreground := btnForeColour.ButtonColor;
  end;
end;

procedure TfrmConfig.btnBoldChange(Sender: TObject);
begin
  case listItems.ItemIndex of
    0: exit;
    1: if btnBold.State = cbChecked then SynHTMLSyn1.CommentAttri.Style := SynHTMLSyn1.CommentAttri.Style + [fsBold] else SynHTMLSyn1.CommentAttri.Style := SynHTMLSyn1.CommentAttri.Style - [fsBold];
    2: if btnBold.State = cbChecked then SynHTMLSyn1.IdentifierAttri.Style := SynHTMLSyn1.IdentifierAttri.Style + [fsBold] else SynHTMLSyn1.IdentifierAttri.Style := SynHTMLSyn1.IdentifierAttri.Style - [fsBold];
    3: if btnBold.State = cbChecked then SynHTMLSyn1.KeyAttri.Style := SynHTMLSyn1.KeyAttri.Style + [fsBold] else SynHTMLSyn1.KeyAttri.Style := SynHTMLSyn1.KeyAttri.Style - [fsBold];
    4: if btnBold.State = cbChecked then SynHTMLSyn1.SymbolAttri.Style := SynHTMLSyn1.SymbolAttri.Style + [fsBold] else SynHTMLSyn1.SymbolAttri.Style := SynHTMLSyn1.SymbolAttri.Style - [fsBold];
    5: if btnBold.State = cbChecked then SynHTMLSyn1.TextAttri.Style := SynHTMLSyn1.TextAttri.Style + [fsBold] else SynHTMLSyn1.TextAttri.Style := SynHTMLSyn1.TextAttri.Style - [fsBold];
    6: if btnBold.State = cbChecked then SynHTMLSyn1.ValueAttri.Style := SynHTMLSyn1.ValueAttri.Style + [fsBold] else SynHTMLSyn1.ValueAttri.Style := SynHTMLSyn1.ValueAttri.Style - [fsBold];
  end;
end;

procedure TfrmConfig.btnItalicChange(Sender: TObject);
begin
  case listItems.ItemIndex of
    0: exit;
    1: if btnItalic.State = cbChecked then SynHTMLSyn1.CommentAttri.Style := SynHTMLSyn1.CommentAttri.Style + [fsItalic] else SynHTMLSyn1.CommentAttri.Style := SynHTMLSyn1.CommentAttri.Style - [fsItalic];
    2: if btnItalic.State = cbChecked then SynHTMLSyn1.IdentifierAttri.Style := SynHTMLSyn1.IdentifierAttri.Style + [fsItalic] else SynHTMLSyn1.IdentifierAttri.Style := SynHTMLSyn1.IdentifierAttri.Style - [fsItalic];
    3: if btnItalic.State = cbChecked then SynHTMLSyn1.KeyAttri.Style := SynHTMLSyn1.KeyAttri.Style + [fsItalic] else SynHTMLSyn1.KeyAttri.Style := SynHTMLSyn1.KeyAttri.Style - [fsItalic];
    4: if btnItalic.State = cbChecked then SynHTMLSyn1.SymbolAttri.Style := SynHTMLSyn1.SymbolAttri.Style + [fsItalic] else SynHTMLSyn1.SymbolAttri.Style := SynHTMLSyn1.SymbolAttri.Style - [fsItalic];
    5: if btnItalic.State = cbChecked then SynHTMLSyn1.TextAttri.Style := SynHTMLSyn1.TextAttri.Style + [fsItalic] else SynHTMLSyn1.TextAttri.Style := SynHTMLSyn1.TextAttri.Style - [fsItalic];
    6: if btnItalic.State = cbChecked then SynHTMLSyn1.ValueAttri.Style := SynHTMLSyn1.ValueAttri.Style + [fsItalic] else SynHTMLSyn1.ValueAttri.Style := SynHTMLSyn1.ValueAttri.Style - [fsItalic];
  end;
end;

procedure TfrmConfig.btnUnderlineChange(Sender: TObject);
begin
  case listItems.ItemIndex of
    0: exit;
    1: if btnUnderline.State = cbChecked then SynHTMLSyn1.CommentAttri.Style := SynHTMLSyn1.CommentAttri.Style + [fsUnderline] else SynHTMLSyn1.CommentAttri.Style := SynHTMLSyn1.CommentAttri.Style - [fsUnderline];
    2: if btnUnderline.State = cbChecked then SynHTMLSyn1.IdentifierAttri.Style := SynHTMLSyn1.IdentifierAttri.Style + [fsUnderline] else SynHTMLSyn1.IdentifierAttri.Style := SynHTMLSyn1.IdentifierAttri.Style - [fsUnderline];
    3: if btnUnderline.State = cbChecked then SynHTMLSyn1.KeyAttri.Style := SynHTMLSyn1.KeyAttri.Style + [fsUnderline] else SynHTMLSyn1.KeyAttri.Style := SynHTMLSyn1.KeyAttri.Style - [fsUnderline];
    4: if btnUnderline.State = cbChecked then SynHTMLSyn1.SymbolAttri.Style := SynHTMLSyn1.SymbolAttri.Style + [fsUnderline] else SynHTMLSyn1.SymbolAttri.Style := SynHTMLSyn1.SymbolAttri.Style - [fsUnderline];
    5: if btnUnderline.State = cbChecked then SynHTMLSyn1.TextAttri.Style := SynHTMLSyn1.TextAttri.Style + [fsUnderline] else SynHTMLSyn1.TextAttri.Style := SynHTMLSyn1.TextAttri.Style - [fsUnderline];
    6: if btnUnderline.State = cbChecked then SynHTMLSyn1.ValueAttri.Style := SynHTMLSyn1.ValueAttri.Style + [fsUnderline] else SynHTMLSyn1.ValueAttri.Style := SynHTMLSyn1.ValueAttri.Style - [fsUnderline];
  end;
end;

procedure TfrmConfig.listItemsSelect(Sender: TObject);
begin
  case listItems.ItemIndex of
    0: begin
         btnForeColour.ButtonColor := clNone;
         btnBackColour.ButtonColor := clNone;
         btnBold.State := cbUnchecked;
         btnItalic.State := cbUnchecked;
         btnUnderline.State := cbUnchecked;
       end;
    1: begin
         btnForeColour.ButtonColor := SynHTMLSyn1.CommentAttri.Foreground;
         btnBackColour.ButtonColor := SynHTMLSyn1.CommentAttri.Background;
         if fsBold in SynHTMLSyn1.CommentAttri.Style then btnBold.State := cbChecked else btnBold.State := cbUnchecked;
         if fsItalic in SynHTMLSyn1.CommentAttri.Style then btnItalic.State := cbChecked else btnItalic.State := cbUnchecked;
         if fsUnderline in SynHTMLSyn1.CommentAttri.Style then btnUnderline.State := cbChecked else btnUnderline.State := cbUnchecked;
       end;
    2: begin
         btnForeColour.ButtonColor := SynHTMLSyn1.IdentifierAttri.Foreground;
         btnBackColour.ButtonColor := SynHTMLSyn1.IdentifierAttri.Background;
         if fsBold in SynHTMLSyn1.IdentifierAttri.Style then btnBold.State := cbChecked else btnBold.State := cbUnchecked;
         if fsItalic in SynHTMLSyn1.IdentifierAttri.Style then btnItalic.State := cbChecked else btnItalic.State := cbUnchecked;
         if fsUnderline in SynHTMLSyn1.IdentifierAttri.Style then btnUnderline.State := cbChecked else btnUnderline.State := cbUnchecked;
       end;
    3: begin
         btnForeColour.ButtonColor := SynHTMLSyn1.KeyAttri.Foreground;
         btnBackColour.ButtonColor := SynHTMLSyn1.KeyAttri.Background;
         if fsBold in SynHTMLSyn1.KeyAttri.Style then btnBold.State := cbChecked else btnBold.State := cbUnchecked;
         if fsItalic in SynHTMLSyn1.KeyAttri.Style then btnItalic.State := cbChecked else btnItalic.State := cbUnchecked;
         if fsUnderline in SynHTMLSyn1.KeyAttri.Style then btnUnderline.State := cbChecked else btnUnderline.State := cbUnchecked;
       end;
    4: begin
         btnForeColour.ButtonColor := SynHTMLSyn1.SymbolAttri.Foreground;
         btnBackColour.ButtonColor := SynHTMLSyn1.SymbolAttri.Background;
         if fsBold in SynHTMLSyn1.SymbolAttri.Style then btnBold.State := cbChecked else btnBold.State := cbUnchecked;
         if fsItalic in SynHTMLSyn1.SymbolAttri.Style then btnItalic.State := cbChecked else btnItalic.State := cbUnchecked;
         if fsUnderline in SynHTMLSyn1.SymbolAttri.Style then btnUnderline.State := cbChecked else btnUnderline.State := cbUnchecked;
       end;
    5: begin
         btnForeColour.ButtonColor := SynHTMLSyn1.TextAttri.Foreground;
         btnBackColour.ButtonColor := SynHTMLSyn1.TextAttri.Background;
         if fsBold in SynHTMLSyn1.TextAttri.Style then btnBold.State := cbChecked else btnBold.State := cbUnchecked;
         if fsItalic in SynHTMLSyn1.TextAttri.Style then btnItalic.State := cbChecked else btnItalic.State := cbUnchecked;
         if fsUnderline in SynHTMLSyn1.TextAttri.Style then btnUnderline.State := cbChecked else btnUnderline.State := cbUnchecked;
       end;
    6: begin
         btnForeColour.ButtonColor := SynHTMLSyn1.ValueAttri.Foreground;
         btnBackColour.ButtonColor := SynHTMLSyn1.ValueAttri.Background;
         if fsBold in SynHTMLSyn1.ValueAttri.Style then btnBold.State := cbChecked else btnBold.State := cbUnchecked;
         if fsItalic in SynHTMLSyn1.ValueAttri.Style then btnItalic.State := cbChecked else btnItalic.State := cbUnchecked;
         if fsUnderline in SynHTMLSyn1.ValueAttri.Style then btnUnderline.State := cbChecked else btnUnderline.State := cbUnchecked;
       end;
  end;
end;

procedure TfrmConfig.textFontChange(Sender: TObject);
begin
  editPreview.Font.Name := textFont.Items[textFont.ItemIndex];
end;

procedure TfrmConfig.textFontSizeChange(Sender: TObject);
begin
  editPreview.Font.Size := textFontSize.Value;
end;

end.

(*
Additional notes on the various parts of HTML highlighter

CommentAttri
IdentifierAttri (DOCTYPEAttri)
KeyAttri (UndefKeyAttri)
SymbolAttri (AndAttri)
TextAttri
ValueAttri

Background
Foreground
Style (set of [fsBold,fsItalic,fsStrikeOut,fsUnderline]
*)

