unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LclIntf, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    imgWWW: TImage;
    imgLaz: TImage;
    imgTwitter: TImage;
    Label7: TLabel;
    labelGeoIP: TLabel;
    labelATTabs: TLabel;
    Label6: TLabel;
    labelSynapse: TLabel;
    labelMYTHcode: TLabel;
    labelXiControls: TLabel;
    sfImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgLazClick(Sender: TObject);
    procedure imgTwitterClick(Sender: TObject);
    procedure imgWWWClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure labelATTabsClick(Sender: TObject);
    procedure labelGeoIPClick(Sender: TObject);
    procedure labelMYTHcodeClick(Sender: TObject);
    procedure labelSynapseClick(Sender: TObject);
    procedure labelXiControlsClick(Sender: TObject);
    procedure sfImageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Label1.Width := frmAbout.ClientWidth;
  Label2.Width := frmAbout.ClientWidth;
  Label3.Width := frmAbout.ClientWidth;
  Label4.Width := frmAbout.ClientWidth;
  Label5.Width := frmAbout.ClientWidth;
end;

procedure TfrmAbout.imgLazClick(Sender: TObject);
begin
  OpenURL('http://www.lazarus-ide.org');
end;

procedure TfrmAbout.imgTwitterClick(Sender: TObject);
begin
  OpenURL('https://twitter.com/hippy2094');
end;

procedure TfrmAbout.imgWWWClick(Sender: TObject);
begin
  OpenURL('https://www.matthewhipkin.co.uk');
end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
  OpenURL('https://linkspy.sourceforge.io');
end;

procedure TfrmAbout.labelATTabsClick(Sender: TObject);
begin
  OpenURL('https://github.com/Alexey-T/ATTabs');
end;

procedure TfrmAbout.labelGeoIPClick(Sender: TObject);
begin
  OpenURL('http://lite.ip2location.com/');
end;

procedure TfrmAbout.labelMYTHcodeClick(Sender: TObject);
begin
  OpenURL('http://www.mythcode.org');
end;

procedure TfrmAbout.labelSynapseClick(Sender: TObject);
begin
  OpenURL('http://www.ararat.cz/synapse/doku.php/start');
end;

procedure TfrmAbout.labelXiControlsClick(Sender: TObject);
begin
  OpenURL('http://www.deadlogic.co.nr');
end;

procedure TfrmAbout.sfImageClick(Sender: TObject);
begin
  OpenURL('https://sourceforge.net/p/linkspy/');
end;

end.

