unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, Clipbrd, LCLIntF,
  Buttons, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  XiPanel, ExtCtrls, Menus, Grids, ATTabs, resolve, xmlparser, resulttab,
  miscfunc;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    checkPOST: TCheckBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    listBatchResults: TListView;
    menuCheckItem: TMenuItem;
    menuClearMenu: TMenuItem;
    menuRemoveItem: TMenuItem;
    btnGo: TSpeedButton;
    btnRefresh: TSpeedButton;
    btnSave: TSpeedButton;
    btnAbout: TSpeedButton;
    btnBatch: TSpeedButton;
    batchMenu: TPopupMenu;
    toolsMenu: TPopupMenu;
    tabArea: TPanel;
    panelBatch: TPanel;
    btnCloseBatch: TSpeedButton;
    btnBrowser: TSpeedButton;
    btnSaveBatch: TSpeedButton;
    Splitter1: TSplitter;
    menuCopy: TMenuItem;
    menuCheckURL: TMenuItem;
    menuCheckAll: TMenuItem;
    menuOpenURL: TMenuItem;
    menuRestore: TMenuItem;
    menuExit: TMenuItem;
    btnManageAgents: TSpeedButton;
    btnOptions: TSpeedButton;
    editorMenu: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    trayMenu: TPopupMenu;
    textURL: TComboBox;
    listAgents: TComboBox;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    HTMLHighlighter: TSynHTMLSyn;
    clipboardTimer: TTimer;
    TrayIcon1: TTrayIcon;
    updatesTimer: TTimer;
    procedure batchMenuPopup(Sender: TObject);
    procedure btnBatchClick(Sender: TObject);
    procedure btnBrowserClick(Sender: TObject);
    procedure btnCloseBatchClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnManageAgentsClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSaveBatchClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure clipboardTimerTimer(Sender: TObject);
    procedure editorMenuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure listAgentsChange(Sender: TObject);
    procedure listBatchResultsDblClick(Sender: TObject);
    procedure menuCheckAllClick(Sender: TObject);
    procedure menuCheckURLClick(Sender: TObject);
    procedure menuClearMenuClick(Sender: TObject);
    procedure menuCopyClick(Sender: TObject);
    procedure menuExitClick(Sender: TObject);
    procedure menuOpenURLClick(Sender: TObject);
    procedure menuRemoveItemClick(Sender: TObject);
    procedure menuRestoreClick(Sender: TObject);
    procedure textURLKeyPress(Sender: TObject; var Key: char);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure updatesTimerTimer(Sender: TObject);
    procedure ClipboardURLClick(Sender: TObject);
    procedure updateLabelClick(Sender: TObject);
    procedure BatchThreadTerminated(Sender: TObject);
    procedure TabClose(Sender: TObject; ATabIndex: Integer; var ACanClose,
      ACanContinue: boolean);
    procedure TabChange(Sender: TObject; ANewTabIndex: Integer;
      var ACanChange: boolean);
    procedure TabOver(Sender: TObject; ATabIndex: Integer);
    procedure TabClick(Sender: TObject);
  private
    { private declarations }
    updatePanel: TXiPanel;
    updateLabel: TLabel;
    LastURL: String;
    stopBatch: Boolean;
    URLAtCursor: String;
    th: TScanThread;
    tabControl: TATTabs;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure AddHistory(url: String);
    procedure ClearHistory;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure ApplyConfig(i: integer);
    procedure SetTrayStatus(notify: Boolean);
    procedure SetHighlightPart(part: THighlightSection);
    procedure CreateTab(url: String; response: THTTPResponse);
    procedure SetTabActive(i: Integer);
    function CheckURL(url: String; usePOST: boolean; tabIndex: integer): THTTPResponse;
    procedure SetBatchCount(c: Integer);
    procedure BatchResultComplete(response: THTTPResponse);
    procedure SetControlsEnabled(state: Boolean);
    procedure LinkOpenRequest(url: String);
  public
    { public declarations }
    EditorBGColour: TColor;
    EditorFont: TFont;
    BatchMode: Boolean;
    procedure LoadUserAgents;
    procedure SaveUserAgents;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses about, agentform, postvars, config, batch;

{ TfrmMain }

procedure TfrmMain.LoadHistory;
begin
  if not DirectoryExists(appdir) then exit;
  if FileExists(appdir + 'history.txt') then
    textURL.Items.LoadFromFile(appdir + 'history.txt');
end;

procedure TfrmMain.SaveHistory;
begin
  if not DirectoryExists(appdir) then exit;
  textURL.Items.SaveToFile(appdir + 'history.txt');
end;

procedure TfrmMain.ClearHistory;
begin
  textURL.Items.Clear;
  SaveHistory;
end;

procedure TfrmMain.AddHistory(url: String);
var
  i: integer;
  newOne: Boolean;
begin
  if Length(url) < 1 then exit;
  if not DirectoryExists(appdir) then exit;
  newOne := true;
  for i := 0 to textURL.Items.Count -1 do
    if url = textURL.Items[i] then newOne := false;
  if newOne = true then
  begin
    if textURL.Items.Count > 0 then
      textURL.Items.Insert(0,url)
    else
      textURL.Items.Add(url);
    SaveHistory;
  end;
end;

procedure TfrmMain.LoadUserAgents;
var
  Parser: TXMLParser;
  t: TStrings;
  ua,ft: String;
  pa: PUserAgent;
begin
  listAgents.Clear;
  if not DirectoryExists(appdir) then exit;
  if not FileExists(appdir + 'ua.xml') then
  begin
    AddDefaultUserAgents(UserAgents,listAgents.Items);
    exit;
  end;
  ua := '';
  ft := '';
  t := TStringList.Create;
  t.LoadFromFile(appdir + 'ua.xml');
  Parser := TXMLParser.Create(t.Text);
  while Parser.Next do
  begin
    if Parser.TagType = ttBeginTag then
    begin
      if Parser.Name = 'item' then
      begin
        ua := '';
        ft := '';
      end;
      if Parser.Name = 'useragent' then ua := Parser.ContentCode;
      if Parser.Name = 'title' then ft := Parser.ContentCode;
    end;
    if (Parser.TagType = ttEndTag) and (Parser.Name = 'item') then
    begin
      new(pa);
      pa^.userAgent:=ua;
      pa^.friendlyName:=ft;
      UserAgents.Add(pa);
      listAgents.Items.Add(ft);
    end;
  end;
  Parser.Free;
  t.Free;
end;

procedure TfrmMain.SaveUserAgents;
var
  xml: TStrings;
  pa: PUserAgent;
  i: integer;
begin
  if not DirectoryExists(appdir) then exit;
  xml := TStringList.Create;
  xml.Add('<?xml version="1.0" encoding="UTF-8" ?>');
  xml.Add('<useragents>');
  for i := 0 to UserAgents.Count -1 do
  begin
    pa := UserAgents[i];
    xml.Add('  <item>');
    xml.Add('    <title>'+pa^.friendlyName+'</title>');
    xml.Add('    <useragent>'+pa^.userAgent+'</useragent>');
    xml.Add('  </item>');
  end;
  xml.Add('</useragents>');
  xml.SaveToFile(appdir + 'ua.xml');
  xml.Free;
end;

procedure TfrmMain.LoadConfig;
var
  Parser: TXMLParser;
  sections: TArray;
  f: TStrings;
  current: THighlightSection;
  HasStyles: Boolean;
  InHighlighter: Boolean;
  tmp: TSynEdit;
begin
  if not DirectoryExists(appdir) then exit;
  SetLength(sections,6);
  HasStyles := false;
  InHighlighter := false;
  sections[0] := 'comment';
  sections[1] := 'identifier';
  sections[2] := 'key';
  sections[3] := 'symbol';
  sections[4] := 'text';
  sections[5] := 'value';
  current.section := '';
  current.foreground := clNone;
  current.background := clNone;
  current.FontStyle := [];
  if not FileExists(appdir + 'config.xml') then
  begin
    {$IFDEF WINDOWS}
    EditorFont.Name := 'Courier New';
    EditorFont.Size := 12;
    {$ELSE}
    tmp := TSynEdit.Create(Self);
    EditorFont := tmp.Font;
    tmp.Free;
    {$ENDIF}
    SetDefaultTheme(HTMLHighlighter);
    exit;
  end;
  f := TStringList.Create;
  f.LoadFromFile(appdir + 'config.xml');
  Parser := TXMLParser.Create(f.Text);
  while Parser.Next do
  begin
    if Parser.TagType = ttBeginTag then
    begin
      if Parser.Name = 'highlighter' then InHighlighter := true;
      if Parser.Name = 'font' then EditorFont.Name := Parser.ContentTrimText;
      if Parser.Name = 'fontsize' then EditorFont.Size := StrToIntDef(Parser.ContentTrimText,10);
      if Parser.Name = 'currentuseragent' then listAgents.ItemIndex := StrToIntDef(Parser.ContentTrimText,-1);
      if inArray(Parser.Name,sections) then current.section := Parser.Name;
      if Parser.Name = 'styles' then
      begin
        HasStyles := true;
        if Length(Parser.ContentTrimText) > 1 then
          TextToFontStyle(explode(',',Parser.ContentTrimText,0),current.FontStyle);
      end;
      if Parser.Name = 'foreground' then
      begin
        if Length(Parser.ContentTrimText) > 1 then current.foreground := HTMLToColour(Parser.ContentTrimText)
        else current.foreground := clNone;
      end;
      if Parser.Name = 'background' then
      begin
        if InHighlighter then
        begin
          if Length(Parser.ContentTrimText) > 1 then current.background := HTMLToColour(Parser.ContentTrimText)
          else current.background := clNone;
        end
        else EditorBGColour := HTMLToColour(Parser.ContentTrimText);
      end;
    end;
    if Parser.TagType = ttEndTag then
    begin
      if Parser.Name = current.section then
      begin
        SetHighlightPart(current);
        current.section := '';
        current.foreground := clNone;
        current.background := clNone;
        current.FontStyle := [];
      end;
    end;
  end;
  if not HasStyles then
  begin
    SetDefaultTheme(HTMLHighlighter);
  end;
  f.Free;
end;

procedure TfrmMain.SaveConfig;
var
  xml: TStrings;
  fs: TArray;
begin
  if not DirectoryExists(appdir) then exit;
  xml := TStringList.Create;
  xml.Add('<?xml version="1.0" encoding="UTF-8" ?>');
  xml.Add('<config>');
  xml.Add('  <font>' + EditorFont.Name + '</font>');
  xml.Add('  <fontsize>' + IntToStr(EditorFont.Size) + '</fontsize>');
  xml.Add('  <background>' + SetColourString(EditorBGColour) + '</background>');
  xml.Add('  <currentuseragent>' + IntToStr(listAgents.ItemIndex) + '</currentuseragent>');
  xml.Add('  <highlighter>');
  xml.Add('    <comment>');
  xml.Add('      <foreground>' + SetColourString(HTMLHighlighter.CommentAttri.Foreground) + '</foreground>');
  xml.Add('      <background>' + SetColourString(HTMLHighlighter.CommentAttri.Background) + '</background>');
  FontStyleToText(HTMLHighlighter.CommentAttri.Style,fs);
  xml.Add('      <styles>' + implode(',',fs) + '</styles>');
  xml.Add('    </comment>');
  xml.Add('    <identifier>');
  xml.Add('      <foreground>' + SetColourString(HTMLHighlighter.IdentifierAttri.Foreground) + '</foreground>');
  xml.Add('      <background>' + SetColourString(HTMLHighlighter.IdentifierAttri.Background) + '</background>');
  FontStyleToText(HTMLHighlighter.IdentifierAttri.Style,fs);
  xml.Add('      <styles>' + implode(',',fs) + '</styles>');
  xml.Add('    </identifier>');
  xml.Add('    <key>');
  xml.Add('      <foreground>' + SetColourString(HTMLHighlighter.KeyAttri.Foreground) + '</foreground>');
  xml.Add('      <background>' + SetColourString(HTMLHighlighter.KeyAttri.Background) + '</background>');
  FontStyleToText(HTMLHighlighter.KeyAttri.Style,fs);
  xml.Add('      <styles>' + implode(',',fs) + '</styles>');
  xml.Add('    </key>');
  xml.Add('    <symbol>');
  xml.Add('      <foreground>' + SetColourString(HTMLHighlighter.SymbolAttri.Foreground) + '</foreground>');
  xml.Add('      <background>' + SetColourString(HTMLHighlighter.SymbolAttri.Background) + '</background>');
  FontStyleToText(HTMLHighlighter.SymbolAttri.Style,fs);
  xml.Add('      <styles>' + implode(',',fs) + '</styles>');
  xml.Add('    </symbol>');
  xml.Add('    <text>');
  xml.Add('      <foreground>' + SetColourString(HTMLHighlighter.TextAttri.Foreground) + '</foreground>');
  xml.Add('      <background>' + SetColourString(HTMLHighlighter.TextAttri.Background) + '</background>');
  FontStyleToText(HTMLHighlighter.TextAttri.Style,fs);
  xml.Add('      <styles>' + implode(',',fs) + '</styles>');
  xml.Add('    </text>');
  xml.Add('    <value>');
  xml.Add('      <foreground>' + SetColourString(HTMLHighlighter.ValueAttri.Foreground) + '</foreground>');
  xml.Add('      <background>' + SetColourString(HTMLHighlighter.ValueAttri.Background) + '</background>');
  FontStyleToText(HTMLHighlighter.ValueAttri.Style,fs);
  xml.Add('      <styles>' + implode(',',fs) + '</styles>');
  xml.Add('    </value>');
  xml.Add('  </highlighter>');
  xml.Add('</config>');
  xml.SaveToFile(appdir + 'config.xml');
  xml.Free;
end;

procedure TfrmMain.ApplyConfig(i: integer);
var
  j: integer;
  d: TATTabData;
begin
  if i > -1 then
  begin
    d := tabControl.GetTabData(i);
    (d.TabObject as TResultTab).Configure(HTMLHighlighter,editorMenu,EditorFont,EditorBGColour);
  end
  else
  begin
    for j := 0 to tabControl.TabCount -1 do
    begin
      d := tabControl.GetTabData(j);
      (d.TabObject as TResultTab).Configure(HTMLHighlighter,editorMenu,EditorFont,EditorBGColour);
    end;
  end;
end;

procedure TfrmMain.SetHighlightPart(part: THighlightSection);
begin
  // Those of a nervous disposition should look away now.
  if part.section = 'comment' then
  begin
    with HTMLHighlighter.CommentAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
  end;
  if part.section = 'identifier' then
  begin
    with HTMLHighlighter.IdentifierAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
    with HTMLHighlighter.DOCTYPEAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
  end;
  if part.section = 'key' then
  begin
    with HTMLHighlighter.KeyAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
    with HTMLHighlighter.UndefKeyAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
  end;
  if part.section = 'symbol' then
  begin
    with HTMLHighlighter.SymbolAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
    with HTMLHighlighter.AndAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
  end;
  if part.section = 'text' then
  begin
    with HTMLHighlighter.TextAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
  end;
  if part.section = 'value' then
  begin
    with HTMLHighlighter.ValueAttri do
    begin
      Background := part.background;
      Foreground := part.foreground;
      Style := part.FontStyle;
    end;
  end;
end;

procedure TfrmMain.SetControlsEnabled(state: Boolean);
begin
  textURL.Enabled := state;
  btnGo.Enabled := state;
  checkPOST.Enabled := state;
  btnRefresh.Enabled := state;
  btnBatch.Enabled := state;
  btnSave.Enabled := state;
  listAgents.Enabled := state;
  btnManageAgents.Enabled := state;
  btnAbout.Enabled := state;
  btnOptions.Enabled := state;
  btnBrowser.Enabled := state;
end;

procedure TfrmMain.CreateTab(url: String; response: THTTPResponse);
var
  r: TResultTab;
begin
  r := TResultTab.Create(nil);
  r.Name := '';
  r.Parent := tabArea;
  r.Align := alClient;
  r.HTTPResponse := response;
  r.Configure(HTMLHighlighter, editorMenu, EditorFont, EditorBGColour);
  r.UpdateContents;
  r.OnNewLinkCheckRequest := LinkOpenRequest;
  r.OnSetControlsRequest := SetControlsEnabled;
  tabControl.AddTab(tabControl.TabIndex + 1, url, r, false, clNone);
  tabControl.SwitchTab(true);
  SetTabActive(tabControl.TabIndex);
  frmMain.Caption := APPNAME + ' ' + APPVER + ' [' + url + ']';
end;

procedure TfrmMain.SetTrayStatus(notify: Boolean);
var
  bmp: TBitmap;
  i: integer;
begin
  if notify then i := 1
  else i := 0;
  bmp := TBitMap.Create;
  ImageList1.GetBitmap(i,bmp);
  TrayIcon1.Icon.Assign(bmp);
  TrayIcon1.Show;
  bmp.Free;
end;

function TfrmMain.CheckURL(url: String; usePOST: boolean; tabIndex: integer): THTTPResponse;
var
  pa: PUserAgent;
  d: TATTabData;
  i: Integer;
begin
  Result.Response := '';
  Result.Headers := '';
  Result.ResponseSize := 0;
  Result.ResultCode := 0;
  Result.URL := url;
  Result.usesPOST := usePOST;
  if tabIndex < 0 then Result.postVars := TPostVarList.Create
  else
  begin
    d := tabControl.GetTabData(tabControl.TabIndex);
    Result.postVars := (d.TabObject as TResultTab).HTTPResponse.postVars;
  end;
  if listAgents.ItemIndex < 0 then listAgents.ItemIndex := 0;
  pa := UserAgents[listAgents.ItemIndex];
  if not usePOST then
    httpGet(url, pa^.userAgent, Result)
  else
  begin
    if tabIndex < 0 then
    begin
      if frmPostVars.checkClear.Checked then frmPostVars.ResetGrid;
      if frmPostVars.ShowModal = mrOK then
      begin
        for i := 0 to frmPostVars.editorGrid.RowCount -1 do
        begin
          if (frmPostVars.editorGrid.Cells[0,i] <> '') and (frmPostVars.editorGrid.Cells[1,i] <> '') then
          begin
            Result.postVars.Add(frmPostVars.editorGrid.Cells[0,i],frmPostVars.editorGrid.Cells[1,i]);
          end;
        end;
      end
      else exit;
    end;
    httpPost(textURL.Text, pa^.userAgent, Result.postVars, Result);
  end;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
var
  response: THTTPResponse;
begin
  if Length(textURL.Text) < 1 then exit;
  response := CheckURL(textURL.Text,checkPOST.Checked,-1);
  CreateTab(textURL.Text,response);
  StatusBar1.SimpleText := 'Server returned code '+IntToStr(response.ResultCode)+', returned size '+IntToStr(response.ResponseSize)+' bytes';
  if response.ResultCode <> 0 then AddHistory(textURL.Text);
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
var
  i: integer;
  response: THTTPResponse;
  d: TATTabData;
begin
  i := tabControl.TabIndex;
  if i < 0 then exit;
  d := tabControl.GetTabData(i);
  response := CheckURL((d.TabObject as TResultTab).HTTPResponse.URL,checkPOST.Checked,i);
  (d.TabObject as TResultTab).HTTPResponse := response;
  (d.TabObject as TResultTab).textContent.Text := response.Response;
  (d.TabObject as TResultTab).textHeaders.Text := response.Headers;
  StatusBar1.SimpleText := 'Server returned code '+IntToStr(response.ResultCode)+', returned size '+IntToStr(response.ResponseSize)+' bytes';
  if response.ResultCode <> 0 then AddHistory(textURL.Text);
end;

procedure TfrmMain.btnSaveBatchClick(Sender: TObject);
var
  combined: TStrings;
  j: Integer;
  line: String;
  sp: TArray;
begin
  if SaveDialog1.Execute then
  begin
    combined := TStringList.Create;
    combined.Add('URL,code,size');
    for j := 0 to listBatchResults.Items.Count -1 do
    begin
      line := listBatchResults.Items[j].Caption + ',';
      line := line + listBatchResults.Items[j].SubItems[0] + ',';
      sp := explode(' ', listBatchResults.Items[j].SubItems[1],0);
      line := line + trim(sp[0]);
      combined.Add(line);
    end;
    combined.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.btnBatchClick(Sender: TObject);
var
  pa: PUserAgent;
begin
{  frmBatch.textURLs.Font.Size := EditorFontSize;
  frmBatch.textURLs.Color := EditorBGColour;
  frmBatch.textURLs.Font.Color := EditorFontColour;
  frmBatch.textURLs.Font.Name := EditorFontName;}
  frmBatch.textURLs.Font := EditorFont;
  if frmBatch.ShowModal = mrOK then
  begin
    SetControlsEnabled(false);
    stopBatch := false;
    listBatchResults.Clear;
    if listAgents.ItemIndex < 0 then listAgents.ItemIndex := 0;
    pa := UserAgents[listAgents.ItemIndex];
    th := TScanThread.Create(True);
    th.OnTerminate := BatchThreadTerminated;
    th.OnResponseComplete := BatchResultComplete;
    th.OnUpdateCount := SetBatchCount;
    th.FreeOnTerminate := true;
    th.useragent := pa^.userAgent;
    th.Items.AddStrings(frmBatch.textURLs.Lines);
    th.Start;
  end;
end;

procedure TfrmMain.btnBrowserClick(Sender: TObject);
var
  d: TATTabData;
begin
  d := tabControl.GetTabData(tabControl.TabIndex);
  if Assigned(d) then
  begin
    OpenURL((d.TabObject as TResultTab).HTTPResponse.URL);
  end;
end;

procedure TfrmMain.BatchThreadTerminated(Sender: TObject);
begin
  panelBatch.Visible := true;
  Splitter1.Visible := true;
  SetControlsEnabled(true);
  StatusBar1.SimpleText := 'Done (' + IntToStr(frmBatch.textURLs.Lines.Count) + ' URLs checked)';
end;

procedure TfrmMain.SetBatchCount(c: Integer);
begin
  StatusBar1.SimpleText := 'Checking URL ' + IntToStr(c) + ' of ' + IntToStr(frmBatch.textURLs.Lines.Count);
  Application.ProcessMessages;
end;

procedure TfrmMain.BatchResultComplete(response: THTTPResponse);
var
  item: TListItem;
  res: PHTTPResponse;
begin
  listBatchResults.BeginUpdate;
  item := listBatchResults.Items.Add;
  item.Caption := response.URL;
  item.SubItems.Add(IntToStr(response.ResultCode));
  //res := @response;
  new(res);
  res^.URL := response.URL;
  res^.Response := response.Response;
  res^.Headers := response.Headers;
  res^.ResponseSize := response.ResponseSize;
  res^.ResultCode := response.ResultCode;
  res^.usesPOST := false;
  item.SubItems.AddObject(IntToStr(response.ResponseSize) + ' bytes',TObject(res));
  listBatchResults.EndUpdate;
end;

procedure TfrmMain.batchMenuPopup(Sender: TObject);
begin
  if listBatchResults.SelCount = 0 then
  begin
    menuRemoveItem.Enabled := false;
    menuCheckItem.Enabled := false;
  end
  else
  begin
    menuRemoveItem.Enabled := true;
    menuCheckItem.Enabled := true;
  end;
end;

procedure TfrmMain.btnCloseBatchClick(Sender: TObject);
begin
  Splitter1.Visible := false;
  panelBatch.Visible := false;
end;

procedure TfrmMain.btnManageAgentsClick(Sender: TObject);
var
  i: integer;
  pa: PUserAgent;
begin
  frmUserAgent.listAgents.Clear;
  for i := 0 to UserAgents.Count -1 do
  begin
    pa := UserAgents[i];
    frmUserAgent.listAgents.Items.Add(pa^.friendlyName);
  end;
  if frmUserAgent.ShowModal = mrOK then
  begin
    listAgents.Clear;
    for i := 0 to UserAgents.Count -1 do
    begin
      pa := UserAgents[i];
      listAgents.Items.Add(pa^.friendlyName);
    end;
    SaveUserAgents;
  end;
end;

procedure TfrmMain.btnOptionsClick(Sender: TObject);
begin
  frmConfig.FontName := EditorFont.Name;
  frmConfig.textFontSize.Value := EditorFont.Size;
  frmConfig.btnBGColour.ButtonColor := EditorBGColour;
  { For some reason I have to drill down into the actual types, otherwise
    strange things be going on }
  frmConfig.SynHTMLSyn1.CommentAttri.Background := HTMLHighlighter.CommentAttri.Background;
  frmConfig.SynHTMLSyn1.CommentAttri.Foreground := HTMLHighlighter.CommentAttri.Foreground;
  frmConfig.SynHTMLSyn1.CommentAttri.Style := HTMLHighlighter.CommentAttri.Style;
  frmConfig.SynHTMLSyn1.SymbolAttri.Background := HTMLHighlighter.SymbolAttri.Background;
  frmConfig.SynHTMLSyn1.SymbolAttri.Foreground := HTMLHighlighter.SymbolAttri.Foreground;
  frmConfig.SynHTMLSyn1.SymbolAttri.Style := HTMLHighlighter.SymbolAttri.Style;
  frmConfig.SynHTMLSyn1.IdentifierAttri.Background := HTMLHighlighter.IdentifierAttri.Background;
  frmConfig.SynHTMLSyn1.IdentifierAttri.Foreground := HTMLHighlighter.IdentifierAttri.Foreground;
  frmConfig.SynHTMLSyn1.IdentifierAttri.Style := HTMLHighlighter.IdentifierAttri.Style;
  frmConfig.SynHTMLSyn1.SymbolAttri.Background := HTMLHighlighter.SymbolAttri.Background;
  frmConfig.SynHTMLSyn1.SymbolAttri.Foreground := HTMLHighlighter.SymbolAttri.Foreground;
  frmConfig.SynHTMLSyn1.SymbolAttri.Style := HTMLHighlighter.SymbolAttri.Style;
  frmConfig.SynHTMLSyn1.KeyAttri.Background := HTMLHighlighter.KeyAttri.Background;
  frmConfig.SynHTMLSyn1.KeyAttri.Foreground := HTMLHighlighter.KeyAttri.Foreground;
  frmConfig.SynHTMLSyn1.KeyAttri.Style := HTMLHighlighter.KeyAttri.Style;
  frmConfig.SynHTMLSyn1.UndefKeyAttri.Background := HTMLHighlighter.UndefKeyAttri.Background;
  frmConfig.SynHTMLSyn1.UndefKeyAttri.Foreground := HTMLHighlighter.UndefKeyAttri.Foreground;
  frmConfig.SynHTMLSyn1.UndefKeyAttri.Style := HTMLHighlighter.UndefKeyAttri.Style;
  frmConfig.SynHTMLSyn1.TextAttri.Background := HTMLHighlighter.TextAttri.Background;
  frmConfig.SynHTMLSyn1.TextAttri.Foreground := HTMLHighlighter.TextAttri.Foreground;
  frmConfig.SynHTMLSyn1.TextAttri.Style := HTMLHighlighter.TextAttri.Style;
  frmConfig.SynHTMLSyn1.ValueAttri.Background := HTMLHighlighter.ValueAttri.Background;
  frmConfig.SynHTMLSyn1.ValueAttri.Foreground := HTMLHighlighter.ValueAttri.Foreground;
  frmConfig.SynHTMLSyn1.ValueAttri.Style := HTMLHighlighter.ValueAttri.Style;
  // The mess above should be the few lines below!
  {frmConfig.SynHTMLSyn1.CommentAttri := HTMLHighlighter.CommentAttri;
  frmConfig.SynHTMLSyn1.SymbolAttri := HTMLHighlighter.AndAttri;
  frmConfig.SynHTMLSyn1.IdentifierAttri := HTMLHighlighter.IdentifierAttri;
  frmConfig.SynHTMLSyn1.SymbolAttri := HTMLHighlighter.SymbolAttri;
  frmConfig.SynHTMLSyn1.KeyAttri := HTMLHighlighter.KeyAttri;
  frmConfig.SynHTMLSyn1.UndefKeyAttri := HTMLHighlighter.UndefKeyAttri;
  frmConfig.SynHTMLSyn1.TextAttri := HTMLHighlighter.TextAttri;
  frmConfig.SynHTMLSyn1.ValueAttri := HTMLHighlighter.ValueAttri;}
  if frmConfig.ShowModal = mrOK then
  begin
    if frmConfig.ClearHistory then
    begin
      ClearHistory;
      textURL.Text := 'http://';
    end;
    EditorFont.Name := frmConfig.textFont.Items[frmConfig.textFont.ItemIndex];
    EditorFont.Size := frmConfig.textFontSize.Value;
    HTMLHighlighter.CommentAttri := frmConfig.SynHTMLSyn1.CommentAttri;
    HTMLHighlighter.AndAttri := frmConfig.SynHTMLSyn1.SymbolAttri;
    HTMLHighlighter.IdentifierAttri := frmConfig.SynHTMLSyn1.IdentifierAttri;
    HTMLHighlighter.DOCTYPEAttri := frmConfig.SynHTMLSyn1.IdentifierAttri;
    HTMLHighlighter.SymbolAttri := frmConfig.SynHTMLSyn1.SymbolAttri;
    HTMLHighlighter.KeyAttri := frmConfig.SynHTMLSyn1.KeyAttri;
    HTMLHighlighter.UndefKeyAttri := frmConfig.SynHTMLSyn1.KeyAttri;
    HTMLHighlighter.TextAttri := frmConfig.SynHTMLSyn1.TextAttri;
    HTMLHighlighter.ValueAttri := frmConfig.SynHTMLSyn1.ValueAttri;
    EditorBGColour := frmConfig.btnBGColour.ButtonColor;
    EditorFont.Color := HTMLHighlighter.TextAttri.Foreground;
    SaveConfig;
    ApplyConfig(-1);
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  combined: TStrings;
  d: TATTabData;
begin
  combined := TStringList.Create;
  d := tabControl.GetTabData(tabControl.TabIndex);
  if not Assigned(d) then exit;
  if SaveDialog1.Execute then
  begin
    combined.Add('------------------------------ HEADERS ------------------------------');
    combined.AddStrings((d.TabObject as TResultTab).textHeaders.Lines);
    combined.Add('------------------------------ CONTENT ------------------------------');
    combined.AddStrings((d.TabObject as TResultTab).textContent.Lines);
    combined.SaveToFile(SaveDialog1.FileName);
  end;
  combined.Free;
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  StatusBar1.SimpleText := '';
end;

procedure TfrmMain.clipboardTimerTimer(Sender: TObject);
var
  l: TMenuItem;
  i,j: integer;
  addSep: Boolean;
  addNew: Boolean;
  csList: TStrings;
  cs: String;
  max: integer;
begin
  if Clipboard.AsText = LastURL then exit;
  csList := TStringList.Create;
  csList.Delimiter := #10;
  csList.Text := Clipboard.AsText;
  max := csList.Count;
  if max > 50 then max := 50;
  for j := 0 to max -1 do
  begin
    // Trim any whitespace or lurking CRs
    cs := trim(csList[j]);
    if (AnsiStartsStr('http://',cs)) or (AnsiStartsStr('https://',cs)) then
    begin
      addSep := true;
      addNew := true;
      for i := 0 to trayMenu.Items.Count -1 do
      begin
        if trayMenu.Items[i].Caption = '-' then addSep := false;
        if trayMenu.Items[i].Caption = cs then addNew := false;
      end;
      if addSep then
      begin
        l := TMenuItem.Create(Self);
        l.Caption := '-';
        trayMenu.Items.Insert(0,l);
      end;
      if addNew then
      begin
        l := TMenuItem.Create(Self);
        l.Caption := cs;
        l.OnClick := ClipboardURLClick;
        trayMenu.Items.Insert(0,l);
        SetTrayStatus(true);
        LastURL := Clipboard.AsText;
      end;
    end;
  end;
  if trayMenu.Items.Count > 5 then menuCheckAll.Visible := true
  else menuCheckAll.Visible := false;
  csList.Free;
end;

procedure TfrmMain.editorMenuPopup(Sender: TObject);
var
  s: String;
  t: TPoint;
  x,y: integer;
begin
  x := 0;
  y := 0;
  t := TSynEdit(editorMenu.PopupComponent).CaretXY;
  TSynEdit(editorMenu.PopupComponent).GetWordBoundsAtRowCol(t,x,y);
  s := TSynEdit(editorMenu.PopupComponent).GetWordAtRowCol(t);
  // Go backwards
  while not IsFirstChar(s,[' ','"','''','=']) do
  begin
    dec(x);
    if x < 0 then break;
    s := TSynEdit(editorMenu.PopupComponent).TextBetweenPoints[Point(x,t.Y),Point(y,t.Y)];
  end;
  // Now go forward
  while not IsLastChar(s,[' ','"','''','>']) do
  begin
    inc(y);
    if y > Length(TSynEdit(editorMenu.PopupComponent).LineText) then break;
    s := TSynEdit(editorMenu.PopupComponent).TextBetweenPoints[Point(x,t.Y),Point(y,t.Y)];
  end;
  // Give it a trim
  s := TrimSet(s,[' ','"','''','=','>']);
  if (AnsiStartsStr('http:',s)) or (AnsiStartsStr('https:',s)) then
  begin
    menuCheckURL.Enabled := true;
    menuOpenURL.Enabled := true;
    URLAtCursor := s;
  end
  else
  begin
    menuCheckURL.Enabled := false;
    menuOpenURL.Enabled := false;
    URLAtCursor := '';
  end;
  if Length(s) > 0 then
    menuCopy.Enabled := true
  else
    menuCopy.Enabled := false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  EditorBGColour := clWhite;
  EditorFont := TFont.Create;
  frmMain.Caption := APPNAME + ' ' + APPVER;
  Application.Title := APPNAME;
  appdir :=  GetUserDir + '.httpres' + PathDelim;
  try
    if not DirectoryExists(appdir) then mkdir(appdir);
  except
    appdir := ExtractFilePath(Application.ExeName);
  end;
  UserAgents := TList.Create;
  LoadHistory;
  LoadUserAgents;
  LoadConfig;
  // OS dependent alterations
  {$IFNDEF Windows}
  textURL.Style := csDropDown;
  listAgents.Style := csDropDownList;
  listAgents.Height := 26;
  checkPOST.Width := 90;
  {$ELSE}
  textURL.ItemHeight := listAgents.ItemHeight;
  {$ENDIF}
  // Create tab control
  tabControl := TATTabs.Create(tabArea);
  tabControl.Parent := tabArea;
  tabControl.Align := alTop;
  tabControl.Font.Size := 10;
  tabControl.Height := 36;
  tabControl.TabAngle := 0;
  tabControl.TabIndentInter := 2;
  tabControl.TabIndentInit := 2;
  tabControl.TabIndentTop := 4;
  tabControl.TabIndentXSize := 13;
  tabControl.TabWidthMin := 18;
  tabControl.TabDragEnabled := false;
  tabControl.TabShowPlus := false;
  tabControl.Font.Color := clBlack;
  //tabControl.ColorBg := cl3dLight;
  tabControl.ColorBg := RGBToColor(240,240,240);
  tabControl.ColorBorderActive := $ACA196;
  tabControl.ColorBorderPassive := $ACA196;
  tabControl.ColorTabActive := clBtnFace;
  tabControl.ColorTabPassive := clBtnFace;
  tabControl.ColorTabOver := $F2E4D7;
  tabControl.ColorCloseBg := clNone;
  tabControl.ColorCloseBgOver := $D5C9BD;
  tabControl.ColorCloseBorderOver := $B0B0B0;
  tabControl.ColorCloseX := $7B6E60;
  tabControl.ColorArrow := $5C5751;
  tabControl.ColorArrowOver := tabControl.ColorArrow;
  tabControl.OnTabClose := TabClose;
  tabControl.OnTabClick := TabClick;
  tabControl.OnTabOver := TabOver;
  tabControl.ShowHint := true;
  // Hide results tab
  panelBatch.Visible := false;
  Splitter1.Visible := false;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  UserAgents.Free;
  EditorFont.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin

end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if frmMain.WindowState = wsMinimized then
  begin
    frmMain.Hide;
    frmMain.WindowState := wsNormal;
    frmMain.ShowInTaskBar := stNever;
  end;
end;

procedure TfrmMain.listAgentsChange(Sender: TObject);
begin
  SaveConfig;
end;

procedure TfrmMain.listBatchResultsDblClick(Sender: TObject);
var
  i: PHTTPResponse;
begin
  i := PHTTPResponse(listBatchResults.Selected.SubItems.Objects[1]);
  CreateTab(i^.url,i^);
end;

procedure TfrmMain.menuCheckAllClick(Sender: TObject);
var
  i: integer;
  csList: TStrings;
begin
  panelBatch.Visible := true;
  Splitter1.Visible := true;
  listBatchResults.Clear;
  csList := TStringList.Create;
  csList.Delimiter := #10;
  csList.Text := LastURL;
  for i := (csList.Count -1) downto 0 do
  begin
    if AnsiStartsStr('http:',csList[i]) or AnsiStartsStr('https:',csList[i]) then
    begin
      frmBatch.textURLs.Lines.Add(csList[i]);
    end;
  end;
  for i := (trayMenu.Items.Count -1) downto 0 do
  begin
    if AnsiStartsStr('http:',trayMenu.Items[i].Caption) or AnsiStartsStr('https:',trayMenu.Items[i].Caption) then
    begin
      trayMenu.Items[i].Free;
    end;
  end;
  menuCheckAll.Visible := false;
  SetTrayStatus(false);
  TrayIcon1DblClick(Sender);
  btnBatchClick(Sender);
  csList.Free;
end;

procedure TfrmMain.menuCheckURLClick(Sender: TObject);
begin
  if Length(URLAtCursor) > 0 then
  begin
    textURL.Text := URLAtCursor;
    btnGoClick(Sender);
  end;
end;

procedure TfrmMain.menuClearMenuClick(Sender: TObject);
var
  i: Integer;
begin
  for i := (trayMenu.Items.Count -1) downto 0 do
  begin
    if AnsiStartsStr('http:',trayMenu.Items[i].Caption) or AnsiStartsStr('https:',trayMenu.Items[i].Caption) then
    begin
      trayMenu.Items[i].Free;
    end;
  end;
  SetTrayStatus(false);
end;

procedure TfrmMain.menuCopyClick(Sender: TObject);
begin
  TSynEdit(editorMenu.PopupComponent).CopyToClipboard;
end;

procedure TfrmMain.menuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.menuOpenURLClick(Sender: TObject);
begin
  if Length(URLAtCursor) > 0 then OpenURL(URLAtCursor);
end;

procedure TfrmMain.menuRemoveItemClick(Sender: TObject);
begin
  listBatchResults.Selected.Delete;
end;

procedure TfrmMain.menuRestoreClick(Sender: TObject);
begin
  Application.Restore;
  frmMain.Show;
  frmMain.ShowInTaskBar := stDefault;
end;

procedure TfrmMain.textURLKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then btnGoClick(Sender);
end;

procedure TfrmMain.TrayIcon1DblClick(Sender: TObject);
begin
  Application.Restore;
  frmMain.Show;
  frmMain.ShowInTaskBar := stDefault;
end;

procedure TfrmMain.updatesTimerTimer(Sender: TObject);
var
  response: String;
  newVer: Boolean;
begin
  updatesTimer.Enabled := false;
  newVer := false;
  try
    response := httpGet('http://www.matthewhipkin.co.uk/httpres.txt');
    response := trim(response);
    if CURRVER < StrToInt(response) then newVer := true;
  except
    newVer := false;
  end;
  if newVer then
  begin
    updatePanel := TXiPanel.Create(self);
    updatePanel.Parent := frmMain;
    updatePanel.Top := 0;
    updatePanel.Height := 65;
    updatePanel.Width := 150;
    updatePanel.Align := alBottom;
    updatePanel.Visible := true;
    updatePanel.BringToFront;
    updatePanel.ColorScheme := XiPanel.csRose;
    updateLabel := TLabel.Create(Self);
    updateLabel.Parent := updatePanel;
    updateLabel.Align := alClient;
    updateLabel.Caption := 'An update is available'#13#10'Click here to get it';
    updateLabel.Alignment := taCenter;
    updateLabel.Layout := tlCenter;
    updateLabel.Cursor := crHandPoint;
    updateLabel.Onclick := updateLabelClick;
    btnOptions.Left := updatePanel.Left - (btnOptions.Width + 4);
  end;
end;

procedure TfrmMain.updateLabelClick(Sender: TObject);
begin
  OpenURL('https://linkspy.sourceforge.io');
end;

procedure TfrmMain.ClipboardURLClick(Sender: TObject);
var
  s: String;
begin
  s := (Sender as TMenuItem).Caption;
  textURL.Text := s;
  (Sender as TMenuItem).Free;
  btnGoClick(Sender);
  if frmMain.ShowInTaskBar = stNever then
  begin
    Application.Restore;
    frmMain.Show;
    frmMain.ShowInTaskBar := stDefault;
  end
  else
  begin
    frmMain.BringToFront;
  end;
  if trayMenu.Items.Count < 4 then
  begin
    SetTrayStatus(false);
  end;
end;

procedure TfrmMain.TabClose(Sender: TObject; ATabIndex: Integer; var ACanClose, ACanContinue: boolean);
var
  d: TATTabData;
begin
  d := (Sender as TATTabs).GetTabData(ATabIndex);
  if d = nil then exit;
  (d.TabObject as TResultTab).Free;
  if tabControl.TabCount <= 1 then
  begin
    frmMain.Caption := APPNAME + ' ' + APPVER;
    StatusBar1.SimpleText := '';
  end;
  ACanClose := true;
end;

procedure TfrmMain.TabClick(Sender: TObject);
begin
  SetTabActive(tabControl.TabIndex);
end;

procedure TfrmMain.TabChange(Sender: TObject; ANewTabIndex: Integer; var ACanChange: boolean);
begin
  ACanChange := true;
end;

procedure TfrmMain.TabOver(Sender: TObject; ATabIndex: Integer);
var
  d: TATTabData;
begin
  d := (Sender as TATTabs).GetTabData(ATabIndex);
  if d = nil then exit;
  (Sender as TATTabs).Hint := (d.TabObject as TResultTab).HTTPResponse.URL;
end;

procedure TfrmMain.SetTabActive(i: Integer);
var
  j: Integer;
  d: TATTabData;
begin
  for j := 0 to tabControl.TabCount -1 do
  begin
    d := tabControl.GetTabData(j);
    if j <> i then
    begin
      d.TabColor := clBtnFace;
      (d.TabObject as TResultTab).Visible := false;
    end
    else
    begin
      d.TabColor := clRed;
      (d.TabObject as TResultTab).Visible := true;
      frmMain.Caption := APPNAME + ' ' + APPVER + ' [' + (d.TabObject as TResultTab).HTTPResponse.URL + ']';
      textURL.Text := (d.TabObject as TResultTab).HTTPResponse.URL;
      StatusBar1.SimpleText := 'Server returned code '+IntToStr((d.TabObject as TResultTab).HTTPResponse.ResultCode)+', returned size '+IntToStr((d.TabObject as TResultTab).HTTPResponse.ResponseSize)+' bytes';
    end;
  end;
end;

procedure TfrmMain.LinkOpenRequest(url: String);
var
  response: THTTPResponse;
begin
  response := CheckURL(url,false,-1);
  CreateTab(url,response);
  StatusBar1.SimpleText := 'Server returned code '+IntToStr(response.ResultCode)+', returned size '+IntToStr(response.ResponseSize)+' bytes';
  if response.ResultCode <> 0 then AddHistory(textURL.Text);
end;

end.

