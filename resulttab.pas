unit resulttab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics,
  SynHighlighterHTML, Dialogs, ExtCtrls, Menus, ComCtrls, LResources,
  StdCtrls, Buttons, resolve, pingsend, miscfunc, xmlparser;

type

  { TResultTab }
  TNewLinkCheckRequest = procedure(url: String) of Object;
  TSetControlsRequest = procedure(active: Boolean) of Object;
  TResultTab = class(TFrame)
    btnWhois: TButton;
    btnPing: TButton;
    btnTraceroute: TButton;
    groupLinks: TPanel;
    imgFlag: TImage;
    Label1: TLabel;
    labelIP: TLabel;
    listLinks: TListBox;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    textContent: TSynEdit;
    textHeaders: TSynEdit;
    textWhois: TMemo;
    panelWhoisControl: TPanel;
    panelWhois: TPanel;
    btnCloseWhois: TSpeedButton;
    btnSaveWhois: TSpeedButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure btnCloseWhoisClick(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure btnSaveWhoisClick(Sender: TObject);
    procedure btnTracerouteClick(Sender: TObject);
    procedure btnWhoisClick(Sender: TObject);
    procedure listLinksDblClick(Sender: TObject);
  private
    FHTTPResponse: THTTPResponse;
    FNewLinkCheckRequest: TNewLinkCheckRequest;
    FSetControlsRequest: TSetControlsRequest;
    PingTotal: Integer;
    PingInProgress: Boolean;
    pt: TPingThread;
    procedure ExtractLinks;
    procedure OpenLink(url: String);
    procedure PingResponse(pingtime: Integer; pingindex: Integer);
    procedure PingComplete(Sender: TObject);
    procedure SetControls(state: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Configure(h: TSynHTMLSyn; m: TPopupMenu; f: TFont; bg: TColor);
    procedure UpdateContents;
    property HTTPResponse: THTTPResponse read FHTTPResponse write FHTTPResponse;
    property OnNewLinkCheckRequest: TNewLinkCheckRequest read FNewLinkCheckRequest write FNewLinkCheckRequest;
    property OnSetControlsRequest: TSetControlsRequest read FSetControlsRequest write FSetControlsRequest;
  end;

implementation

{$R *.lfm}

procedure TResultTab.btnWhoisClick(Sender: TObject);
var
  s: String;
  U: TURIParser;
  hostparts: TArray;
  finalhost: String;
begin
  U := TURIParser.Create(Nil);
  U.ParseUri(HTTPResponse.URL);
  hostparts := explode('.',U.Host,0);
  // Check if the domain has an SLD (eg .co.uk)
  if DomainHasSLD(U.Host) then
  begin
    // take 3 entries
    finalhost := hostparts[High(hostparts)-2] + '.' + hostparts[High(hostparts)-1]
                 + '.' + hostparts[High(hostparts)];
  end
  else
  begin
    // otherwise take 2
    finalhost := hostparts[High(hostparts)-1] + '.' + hostparts[High(hostparts)];
  end;
  s := LookUpWhois(finalhost);
  panelWhois.Visible := true;
  panelWhois.BringToFront;
  textWhois.Text := s;
  U.Free;
end;

procedure TResultTab.listLinksDblClick(Sender: TObject);
begin
  if listLinks.ItemIndex > -1 then
  begin
    OpenLink(listLinks.Items[listLinks.ItemIndex]);
  end;
end;

procedure TResultTab.btnCloseWhoisClick(Sender: TObject);
begin
  if PingInProgress then
  begin
    if messagedlg('Ping in progress','Are you sure you want to stop the current ping operation?',mtWarning,[mbYes,mbNo],0) = mrYes then
    begin
      pt.CancelPing := true;
    end
    else exit;
  end;
  panelWhois.Visible := false;
end;

procedure TResultTab.btnPingClick(Sender: TObject);
var
  U: TURIParser;
begin
  U := TURIParser.Create(Nil);
  U.ParseUri(HTTPResponse.URL);
  panelWhois.Visible := true;
  panelWhois.BringToFront;
  textWhois.Clear;
  textWhois.Lines.Add('Pinging ' + U.Host + '...');
  Self.Refresh;
  PingTotal := 0;
  pt := TPingThread.Create(true);
  pt.FreeOnTerminate := true;
  pt.Host := U.Host;
  pt.OnResponseComplete := @PingResponse;
  pt.OnTerminate := @PingComplete;
  pt.Start;
  PingInProgress := true;
  U.Free;
end;

procedure TResultTab.PingResponse(pingtime: Integer; pingindex: Integer);
var
  s: String;
begin
  if pingtime > -1 then
  begin
    s := Format('[%d/%d] %d ms',[pingindex,PINGMAX,pingtime]);
    PingTotal := PingTotal + pingtime;
  end
  else s := Format('[%d/%d] Timeout',[pingindex,PINGMAX]);
  textWhois.Lines.Add(s);
end;

procedure TResultTab.PingComplete(Sender: TObject);
begin
  textWhois.Lines.Add('Done! Average time: %g ms',[PingTotal / PINGMAX]);
  PingInProgress := false;
  //SetControls(true);
end;

procedure TResultTab.btnSaveWhoisClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    textWhois.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TResultTab.btnTracerouteClick(Sender: TObject);
const
  cAnyHost = '*';
  HOPMAX = 30;
var
  U: TURIParser;
  Ping: TPingSend;
  ttl : byte;
  hopindex: Integer;
  totalping: Integer;
begin
  U := TURIParser.Create(Nil);
  U.ParseUri(HTTPResponse.URL);
  panelWhois.Visible := true;
  panelWhois.BringToFront;
  textWhois.Clear;
  Self.Refresh;
  totalping := 0;
  // Routine here based on TraceRouteHost() function from pingsend.pas
  Ping := TPINGSend.Create;
  try
    ttl := 1;
    hopindex := 0;
    repeat
      Application.ProcessMessages;
      inc(hopindex);
      ping.TTL := ttl;
      inc(ttl);
      if ttl > 30 then
        Break;
      if not ping.Ping(U.Host) then
      begin
        textWhois.Lines.Add(cAnyHost + Chr(9) + Chr(9) + ' Timeout');
        continue;
      end;
      if (ping.ReplyError <> IE_NoError)
        and (ping.ReplyError <> IE_TTLExceed) then
      begin
        textWhois.Lines.Add(Ping.ReplyFrom + Chr(9) + Ping.ReplyErrorDesc);
        break;
      end;
      textWhois.Lines.Add(Ping.ReplyFrom + Chr(9) + IntToStr(Ping.PingTime) + ' ms');
      totalping := totalping + Ping.PingTime;
    until (ping.ReplyError = IE_NoError) or (hopindex > HOPMAX);
  finally
    Ping.Free;
  end;
  if hopindex > HOPMAX then textWhois.Lines.Add('Maximum hop count of '+IntToStr(HOPMAX)+' exceeded')
  else textWhois.Lines.Add('Done! Total time = %d ms, average = %g ms',[totalping, totalping / hopindex]);
  U.Free;
end;

constructor TResultTab.Create(AOwner: TComponent);
 begin
   inherited Create(AOwner);
 end;

procedure TResultTab.Configure(h: TSynHTMLSyn; m: TPopupMenu; f: TFont; bg: TColor);
begin
  textHeaders.Highlighter := h;
  textContent.Highlighter := h;
  textHeaders.PopupMenu := m;
  textContent.PopupMenu := m;
  textHeaders.Font := f;
  textContent.Font := f;
  textHeaders.Color := bg;
  textContent.Color := bg;
end;

procedure TResultTab.UpdateContents;
var
  cc: String;
  r: TLResource;
begin
  textHeaders.Text := HTTPResponse.Headers;
  textContent.Text := HTTPResponse.Response;
  labelIP.Caption := HTTPResponse.IP;
  cc := LookupCountry(HTTPResponse.IP);
  if (cc <> '-') and (Length(cc) > 1) then
  begin
    r:=LazarusResources.Find(cc);
    if r <> nil then imgFlag.Picture.LoadFromLazarusResource(cc);
  end;
  ExtractLinks;
end;

procedure TResultTab.ExtractLinks;
var
  Parser: TXMLParser;
  headers: TStrings;
  headparts: TArray;
  i: Integer;
  location: String;
begin
  Parser := TXMLParser.Create(HTTPResponse.Response);
  while Parser.Next do
  begin
    if Parser.TagType = ttBeginTag then
    begin
      if Lowercase(Parser.Name) = 'a' then
      begin
        if CheckValidLink(trim(Parser.Value['href'])) then
        begin
          if IsAbsoluteLink(trim(Parser.Value['href'])) then
          begin
            if not InTStrings(trim(Parser.Value['href']),listLinks.Items) then
              listLinks.Items.Add(trim(Parser.Value['href']));
          end
          else
          begin
            // TODO!
          end;
        end;
      end;
    end;
  end;
  Parser.Free;
  if listLinks.Items.Count < 1 then
  begin
    // Nothing found in the HTML, look for a location header
    location := '';
    headers := TStringList.Create;
    headers.Text := HTTPResponse.Headers;
    for i := 0 to headers.Count -1 do
    begin
      if AnsiPos('Location:',headers[i]) > 0 then
      begin
        headparts := explode(' ',headers[i],0);
        location := trim(headparts[1]);
      end;
    end;
    if Length(location) > 1 then listLinks.Items.Add(location);
  end;
end;

procedure TResultTab.OpenLink(url: String);
begin
  if Assigned(FNewLinkCheckRequest) then FNewLinkCheckRequest(url);
end;

procedure TResultTab.SetControls(state: Boolean);
begin
  if Assigned(FSetControlsRequest) then FSetControlsRequest(state);
end;

initialization
  {$I flags.lrs}

end.

