unit miscfunc;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, synacode, httpsend, ssl_openssl, Graphics,
  SynEdit, StrUtils, xmlparser{$IFDEF MSWINDOWS}, Windows{$ENDIF}, ExtCtrls,
  SynHighlighterHTML, resolve, blcksock, sqlite3ds, pingsend;

type
  TArray = array of string;
  PPostPair = ^TPostPair;
  TPostPair = record
    key: String;
    value: String;
  end;
  PUserAgent = ^TUserAgent;
  TUserAgent = record
    friendlyName: String;
    userAgent: String;
  end;
  THighlightSection = record
    section: String;
    foreground: TColor;
    background: TColor;
    FontStyle: TFontStyles;
  end;
  TPostVarList = class(TList)
  private
    function Get(Index: Integer): PPostPair;
  public
    destructor Destroy; override;
    function Add(key,value: String): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure LoadFromFile(filename: String);
    procedure SaveToFile(filename: String);
    property Items[Index: Integer]: PPostPair read Get; default;
 end;
  PHTTPResponse = ^THTTPResponse;
  THTTPResponse = record
    ResultCode: Integer;
    Headers: String;
    Response: String;
    ResponseSize: Integer;
    URL: String;
    usesPOST: Boolean;
    postVars: TPostVarList;
    IP: String;
  end;
  TUpdateCountEvent = procedure(c: Integer) of Object;
  TResponseComplete = procedure(response: THTTPResponse) of Object;
  TScanThread = class(TThread)
    private
      CurrentIndex: Integer;
      CurrentResponse: THTTPResponse;
      FOnUpdateCount: TUpdateCountEvent;
      LastURL: String;
      FResponseComplete: TResponseComplete;
      procedure PerformScan;
      procedure UpdateCount;
      procedure ResponseComplete;
    protected
      procedure Execute; override;
    public
      Items: TStrings;
      useragent: String;
      Finished: Boolean;
      stop: Boolean;
      constructor Create(CreateSuspended: boolean);
      property OnUpdateCount: TUpdateCountEvent read FOnUpdateCount write FOnUpdateCount;
      property OnResponseComplete: TResponseComplete read FResponseComplete write FResponseComplete;
  end;
  TPingResponseComplete = procedure(pt: Integer; idx: Integer) of Object;
  TPingThread = class(TThread)
    private
      FHost: String;
      FPingTime: Integer;
      FPingIndex: Integer;
      FCancelPing: Boolean;
      FPingResponseComplete: TPingResponseComplete;
      procedure PerformPing;
      procedure PingResponse;
    protected
      procedure Execute; override;
    public
      property CancelPing: Boolean read FCancelPing write FCancelPing;
      property Host: String read FHost write FHost;
      property OnResponseComplete: TPingResponseComplete read FPingResponseComplete write FPingResponseComplete;
  end;

var
  appdir: String;
  UserAgents: TList;

const
  APPNAME = 'linkspy';
  CURRVER = 20180328;
  APPVER = '2.1';
  PINGMAX = 5;

{$IFDEF MSWINDOWS}function getWinVer: String;{$ENDIF}
function IsWindows: Boolean;
procedure httpGet(URL: String; userAgent: String; var response: THTTPResponse); overload;
function httpGet(URL: String): String; overload;
procedure httpPost(URL: String; userAgent: String; postItems: TPostVarList; var response: THTTPResponse);
function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
function implode(cDelimiter: String; arr: TArray): String;
function inArray(sText: String; arr: TArray): Boolean;
function InTStrings(sText: String; arr: TStrings): Boolean;
procedure AddDefaultUserAgents(var UserAgents: TList; ItemList: TStrings);
procedure TextToFontStyle(items: TArray; var fs: TFontStyles);
procedure FontStyleToText(fs: TFontStyles; var items: TArray);
function ColourToHTML(inCol: TColor): String;
function HTMLToColour(sColor: string): TColor;
function SetColourString(inCol: TColor): String;
procedure SetDefaultTheme(var highlighter: TSynHTMLSyn);
function IsFirstChar(s: String; const c: TSysCharSet): Boolean;
function IsLastChar(s: String; const c: TSysCharSet): Boolean;
procedure SaveDebug(s: String);
function LookupWhois(host: String): String;
function IP2Long(IPAddress: String): Int64;
function LookupCountry(ip: String): String;
function DomainHasSLD(host: String): Boolean;
function CheckValidLink(link: String): Boolean;
function IsAbsoluteLink(link: String): Boolean;

implementation

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
  nt: String;
begin
  nt := '';
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  if VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then nt := 'NT ';
  Result := 'Windows '+nt+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion);
end;
{$ENDIF}

function IsWindows: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := true;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

procedure httpGet(URL: String; userAgent: String; var response: THTTPResponse); overload;
var
  HTTP: THTTPSend;
  l: TStrings;
  U: TURIParser;
  iplist: TStrings;
  i: Integer;
begin
  response.Response := '';
  l := TStringList.Create;
  HTTP := THTTPSend.Create;
  //HTTP.Timeout := 5000;
  HTTP.UserAgent := userAgent;
  if HTTP.HTTPMethod('GET', URL) then
  begin
    l.LoadFromStream(Http.Document);
    response.Response := l.Text;
    response.ResultCode := HTTP.ResultCode;
    response.Headers := HTTP.Headers.Text;
    response.URL := URL;
    U := TURIParser.Create(Nil);
    U.ParseUri(URL);
    iplist := TStringList.Create;
    HTTP.Sock.ResolveNameToIP(U.Host,iplist);
    response.IP := '';
    // Find an ipv4 ip
    for i := 0 to iplist.Count -1 do
    begin
      if AnsiPos('.',iplist[i]) > 0 then response.IP := iplist[i];
    end;
    iplist.Free;
    U.Free;
    response.ResponseSize := HTTP.DownloadSize;
    if response.ResponseSize = 0 then response.ResponseSize := Length(l.Text);
  end;
  HTTP.Free;
  l.Free;
end;

function httpGet(URL: String): String; overload;
var
  HTTP: THTTPSend;
  l: TStrings;
  OS: String;
begin
  {$ifdef Windows}
  OS := getWinVer;
  {$endif}
  {$ifdef Linux}
  OS := 'Linux';
  {$endif}
  {$ifdef FreeBSD}
  OS := 'FreeBSD';
  {$endif}
  {$ifdef Darwin}
  OS := 'OS X';
  {$endif}
  Result := '';
  l := TStringList.Create;
  HTTP := THTTPSend.Create;
  HTTP.UserAgent := 'Mozilla/5.0 (compatible; '+OS+'; linkspy '+APPVER+' ('+IntToStr(CURRVER)+'))';
  if HTTP.HTTPMethod('GET', URL) then
  begin
    l.LoadFromStream(Http.Document);
    Result := l.Text;
  end;
  HTTP.Free;
  l.Free;
end;

procedure httpPost(URL: String; userAgent: String; postItems: TPostVarList; var response: THTTPResponse);
var
  HTTP: THTTPSend;
  l: TStrings;
  URLData: String;
  i: integer;
begin
  HTTP := THTTPSend.Create;
  HTTP.UserAgent := userAgent;
  l := TStringList.Create;
  URLData := '';
  for i := 0 to postItems.Count -1 do
  begin
    URLData := URLData + postItems[i].key + '=' +  EncodeURL(postItems[i].value) + '&';
  end;
  URLData := Copy(URLData,1,Length(URLData)-1);
  try
    HTTP.Document.Write(Pointer(URLData)^, Length(URLData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.HTTPMethod('POST', URL);
    l.LoadFromStream(Http.Document);
    response.Response := l.Text;
    response.ResultCode := HTTP.ResultCode;
    response.Headers := HTTP.Headers.Text;
    response.ResponseSize := HTTP.DownloadSize;
    if response.ResponseSize = 0 then response.ResponseSize := Length(l.Text);
  finally
    HTTP.Free;
    l.Free;
  end;
end;

function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
var
  s : string; i,p : integer;
begin
  s := sValue;
  i := 0;
  while length(s) > 0 do
  begin
    inc(i);
    SetLength(result, i);
    p := pos(cDelimiter,s);
    if ( p > 0 ) and ( ( i < iCount ) OR ( iCount = 0) ) then
    begin
      result[i - 1] := copy(s,0,p-1);
      s := copy(s,p + length(cDelimiter),length(s));
    end else
    begin
      result[i - 1] := s;
      s :=  '';
    end;
  end;
end;

function implode(cDelimiter: String; arr: TArray): String;
var
  i: integer;
begin
  Result := '';
  for i := Low(arr) to High(arr) do
  begin
    Result := Result + arr[i] + cDelimiter;
  end;
  Result := TrimRightSet(Result,[' ',cDelimiter[1]]);
end;

function inArray(sText: String; arr: TArray): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := Low(arr) to High(arr) do
  begin
    if sText = arr[i] then Result := true;
  end;
end;

function InTStrings(sText: String; arr: TStrings): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to arr.Count -1 do
  begin
    if sText = arr[i] then Result := true;
  end;
end;

procedure AddDefaultUserAgents(var UserAgents: TList; ItemList: TStrings);
var
  pa: PUserAgent;
  response: String;
  Parser: TXMLParser;
  ua,ft: String;
begin
  response := httpGet('https://linkspy.sourceforge.io/default-useragents.php');
  Parser := TXMLParser.Create(response);
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
      ItemList.Add(ft);
    end;
  end;
  Parser.Free;
end;

procedure TextToFontStyle(items: TArray; var fs: TFontStyles);
var
  i: integer;
begin
  fs := [];
  for i := 0 to High(items) do
  begin
    if items[i] = 'underline' then fs := fs + [fsUnderline];
    if items[i] = 'bold' then fs := fs + [fsBold];
    if items[i] = 'italic' then fs := fs + [fsItalic];
  end;
end;

procedure FontStyleToText(fs: TFontStyles; var items: TArray);
var
  i: integer;
begin
  i := 0;
  SetLength(items,i);
  if fsBold in fs then
  begin
    inc(i);
    SetLength(items,i);
    items[i-1] := 'bold';
  end;
  if fsUnderline in fs then
  begin
    inc(i);
    SetLength(items,i);
    items[i-1] := 'underline';
  end;
  if fsItalic in fs then
  begin
    inc(i);
    SetLength(items,i);
    items[i-1] := 'italic';
  end;
end;

function ColourToHTML(inCol: TColor): String;
var
  i: Integer;
  S: String;
begin
  i := Integer(inCol);
  S := '#';
  S := S + copy(IntToHex(i, 7), 6, 2);
  S := S + copy(IntToHex(i, 7), 4, 2);
  S := S + copy(IntToHex(i, 7), 2, 2);
  Result := S;
end;

function HTMLToColour(sColor: string): TColor;
begin
  Result :=
    RGBToColor(
      StrToInt( '$'+Copy( sColor, 2, 2 ) ),
      StrToInt( '$'+Copy( sColor, 4, 2 ) ),
      StrToInt( '$'+Copy( sColor, 6, 2 ) )
    );
end;

function SetColourString(inCol: TColor): String;
begin
  if inCol <> clNone then Result := ColourToHTML(inCol)
  else Result := '';
end;

procedure SetDefaultTheme(var highlighter: TSynHTMLSyn);
begin
  highlighter.CommentAttri.Style := [fsItalic];
  highlighter.CommentAttri.Foreground := HTMLToColour('#C0C0C0');
  highlighter.CommentAttri.Background := clNone;
  highlighter.AndAttri.Style := [];
  highlighter.AndAttri.Foreground := HTMLToColour('#808040');
  highlighter.AndAttri.Background := clNone;
  highlighter.SymbolAttri.Style := [];
  highlighter.SymbolAttri.Foreground := HTMLToColour('#808040');
  highlighter.SymbolAttri.Background := clNone;
  highlighter.IdentifierAttri.Style := [];
  highlighter.IdentifierAttri.Foreground := HTMLToColour('#0000B9');
  highlighter.IdentifierAttri.Background := clNone;
  highlighter.DOCTYPEAttri.Style := [];
  highlighter.DOCTYPEAttri.Foreground := HTMLToColour('#0080FF');
  highlighter.DOCTYPEAttri.Background := clNone;
  highlighter.KeyAttri.Style := [];
  highlighter.KeyAttri.Foreground := HTMLToColour('#008080');
  highlighter.KeyAttri.Background := clNone;
  highlighter.UnDefKeyAttri.Style := [];
  highlighter.UnDefKeyAttri.Foreground := HTMLToColour('#008080');
  highlighter.UnDefKeyAttri.Background := clNone;
  highlighter.TextAttri.Style := [];
  //highlighter.TextAttri.Foreground := HTMLToColour('#0080C0');
  highlighter.TextAttri.Foreground := HTMLToColour('#000000');
  highlighter.TextAttri.Background := clNone;
  highlighter.ValueAttri.Style := [];
  highlighter.ValueAttri.Foreground := HTMLToColour('#800000');
  highlighter.ValueAttri.Background := clNone;
end;

function IsFirstChar(s: String; const c: TSysCharSet): Boolean;
begin
  if Length(s) > 0 then
  begin
    if s[1] in c then Result := true
    else Result := false;
  end
  else Result := false;
end;

function IsLastChar(s: String; const c: TSysCharSet): Boolean;
begin
  if Length(s) > 0 then
  begin
    if s[Length(s)] in c then Result := true
    else Result := false;
  end
  else Result := false;
end;

procedure SaveDebug(s: String);
var
  f: TStrings;
begin
  f := TStringList.Create;
  if FileExists('debug.txt') then f.LoadFromFile('debug.txt');
  f.Add(s);
  f.SaveToFile('debug.txt');
  f.Free;
end;

function LookupWhois(host: String): String;
var
  b: TTCPBlockSocket;
  rparts: TStrings;
  refer: String;
  i: Integer;
  response: TStringStream;
  rlines: TStrings;
begin
  refer := '';
  // First connect to whois.iana.org and find the whois server for the domain
  b := TTCPBlockSocket.Create;
  response := TStringStream.Create('');
  b.Connect(b.ResolveName('whois.iana.org'),'43');
  b.SendString(host + #13#10);
  b.RecvStreamRaw(response,60000);
  b.CloseSocket;
  b.Free;
  // Find the refer: line
  rlines := TStringList.Create;
  rlines.Text := response.DataString;
  for i := 0 to rlines.Count -1 do
  begin
    if AnsiPos('refer:',rlines[i]) > 0 then
    begin
      rparts := TStringList.Create;
      ExtractStrings([':'], [], PChar(rlines[i]), rparts);
      refer := trim(rparts[1]);
      rparts.Free;
      break;
    end;
  end;
  rlines.Free;
  if Length(refer) < 1 then Result := 'Could not lookup whois for ' + host
  else
  begin
    // Now we connect to the referred server
    b := TTCPBlockSocket.Create;
    b.ConvertLineEnd := false;
    b.Connect(b.ResolveName(refer),'43');
    // .com and .net are handled differently
    if (AnsiEndsStr('.com',host)) or (AnsiEndsStr('.net',host)) then
      b.SendString('domain ' + host + #13#10)
    else
      b.SendString(host + #13#10);
    response := TStringStream.Create('');
    b.RecvStreamRaw(response,60000);
    Result := response.DataString;
    response.Free;
    b.CloseSocket;
    b.Free;
  end;
end;

function IP2Long(IPAddress: String): Int64;
var
  parts: TStrings;
begin
  parts := TStringList.Create;
  ExtractStrings(['.'], [], PChar(IPAddress), parts);
  if parts.Count = 4 then
  begin
    Result :=
    StrToInt64(parts[0]) shl 24 +
    StrToInt64(parts[1]) shl 16 +
    StrToInt64(parts[2]) shl 8 +
    StrToInt64(parts[3]);
  end
  else
  begin
    Result := 0;
  end;
  parts.Free;
end;

function LookupCountry(ip: String): String;
var
  dSrc: TSQLite3Dataset;
begin
  Result := '';
  if FileExists('geoip.db') then
  begin
    dSrc := TSqlite3Dataset.Create(nil);
    with dSrc do
    begin
      FileName := 'geoip.db';
      TableName := 'geoip';
      Sql := 'SELECT country_code FROM geoip WHERE ip_from <= ' + IntToStr(IP2Long(ip))
              + ' AND ip_to >= ' + IntToStr(IP2Long(ip));
      Open;
      First;
      Result := Fields[0].AsString;
    end;
    dSrc.Free;
  end
  else Result := '';
end;

function DomainHasSLD(host: String): Boolean;
var
  i, j: Integer;
  SLDs: TStrings;
  hostparts: TArray;
  sldparts: TArray;
  parts: Integer;
  s: Integer;
  hostsld: String;
begin
  Result := false;
  SLDs := TStringList.Create;
  {$I slds.inc}
  hostparts := explode('.',host,0);
  if Length(hostparts) > 2 then
  begin
    for i := 0 to SLDs.Count -1 do
    begin
      sldparts := explode('.',SLDs[i],0);
      // Some SLDs have 3 parts!
      parts := High(sldparts);
      s := High(hostparts) - parts;
      hostsld := '.';
      for j := (s+1) to High(hostparts) do
      begin
        hostsld := hostsld + hostparts[j] + '.';
      end;
      hostsld := trimrightset(hostsld,['.']);
      if hostsld = SLDs[i] then
      begin
        Result := true;
        break;
      end;
    end;
  end;
  SLDs.Free;
end;

function CheckValidLink(link: String): Boolean;
begin
  Result := true;
  if AnsiStartsStr('mailto:',link) then Result := false;
  if AnsiStartsStr('javascript:',link) then Result := false;
  if AnsiStartsStr('skype:',link) then Result := false;
  if AnsiStartsStr('ftp:',link) then Result := false;
  if AnsiStartsStr('news:',link) then Result := false;
  if AnsiStartsStr('gopher:',link) then Result := false;
  if AnsiStartsStr('file:',link) then Result := false;
  if AnsiStartsStr('//',link) then Result := false;
  if AnsiStartsStr('#',link) then Result := false;
  if Length(link) < 1 then Result := false;
end;

function IsAbsoluteLink(link: String): Boolean;
begin
  if (AnsiStartsStr('http://',link)) or (AnsiStartsStr('https://',link)) then
    Result := true
  else Result := false;
end;

{ TPostVarList }

function TPostVarList.Add(key,value: String): Integer;
var
  PostItem: PPostPair;
begin
  new(PostItem);
  PostItem^.key := key;
  PostItem^.value := value;
  Result := inherited Add(PostItem);
end;

function TPostVarList.Get(Index: Integer): PPostPair;
begin
  Result := PPostPair(inherited Get(Index));
end;

procedure TPostVarList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    FreeMem(Items[i]);
  inherited Clear;
end;

procedure TPostVarList.LoadFromFile(filename: String);
var
  i: Integer;
  s: TStrings;
  p: TArray;
begin
  if not FileExists(filename) then exit;
  Clear;
  s := TStringList.Create;
  s.LoadFromFile(filename);
  for i := 0 to s.Count -1 do
  begin
    p := explode('|',s[i],0);
    Add(DecodeBase64(p[0]),DecodeBase64(p[1]));
  end;
  s.Free;
end;

procedure TPostVarList.SaveToFile(filename: String);
var
  i: Integer;
  s: TStrings;
begin
  s := TStringList.Create;
  for i := 0 to Count -1 do
  begin
    s.Add(EncodeBase64(Items[i].key)+'|'+EncodeBase64(Items[i].value));
  end;
  s.SaveToFile(filename);
  s.Free;
end;

procedure TPostVarList.Delete(Index: Integer);
begin
  FreeMem(Items[Index]);
  inherited Delete(Index);
end;

destructor TPostVarList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FreeMem(Items[i]);
  inherited;
end;

{ TScanThread }
constructor TScanThread.Create(CreateSuspended: Boolean);
begin
  currentIndex := 0;
  Items := TStringList.Create;
  Finished := false;
  stop := false;
  inherited Create(CreateSuspended);
end;

procedure TScanThread.PerformScan;
var
  i: integer;
  response: THTTPResponse;
begin
  for i := 0 to Items.Count -1 do
  begin
    if stop = true then
    begin
      break;
    end;
    CurrentIndex := i + 1;
    Synchronize(UpdateCount);
    response.URL := '';
    response.ResultCode := 0;
    response.ResponseSize := 0;
    response.Response := '';
    response.Headers := '';
    response.usesPOST := false;
    httpGet(Items[i], useragent, response);
    if LastURL <> Items[i] then
    begin
      LastURL := Items[i];
      if Length(response.URL) > 0 then
      begin
        CurrentResponse := response;
        Synchronize(ResponseComplete);
      end;
    end;
  end;
  Finished := true;
end;

procedure TScanThread.UpdateCount;
begin
  if Assigned(FOnUpdateCount) then
  begin
    FOnUpdateCount(CurrentIndex);
  end;
end;

procedure TScanThread.ResponseComplete;
begin
  if Assigned(FResponseComplete) then
  begin
    FResponseComplete(CurrentResponse);
  end;
end;

procedure TScanThread.Execute;
begin
  PerformScan;
end;

 { TPingThread }

procedure TPingThread.PerformPing;
var
  i: Integer;
begin
  for i := 1 to PINGMAX do
  begin
    if FCancelPing then break;
    FPingTime := PingHost(FHost);
    FPingIndex := i;
    Synchronize(PingResponse);
  end;
end;

procedure TPingThread.PingResponse;
begin
  if Assigned(FPingResponseComplete) then
  begin
    FPingResponseComplete(FPingTime, FPingIndex);
  end;
end;

procedure TPingThread.Execute;
begin
  FCancelPing := false;
  PerformPing;
end;

end.

