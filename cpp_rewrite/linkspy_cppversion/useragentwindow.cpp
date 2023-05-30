#include "useragentwindow.h"
#include "ui_useragentwindow.h"

#ifdef Q_OS_WIN
QString GetWinVer() {
    // http://web3.codeproject.com/Messages/4823136/Re-Another-Way.aspx
    NTSTATUS (WINAPI *RtlGetVersion)(LPOSVERSIONINFOEXW);
    OSVERSIONINFOEXW osInfo;

    *(FARPROC*)&RtlGetVersion = GetProcAddress(GetModuleHandleA("ntdll"), "RtlGetVersion");
    QString result;
    if (NULL != RtlGetVersion) {
        osInfo.dwOSVersionInfoSize = sizeof(osInfo);
        RtlGetVersion(&osInfo);
        result = "Windows ";
        if(osInfo.dwPlatformId == VER_PLATFORM_WIN32_NT) result += "NT ";
        result += QString::number(osInfo.dwMajorVersion) + "." + QString::number(osInfo.dwMinorVersion);
    }
    return result;
}
#endif

QString DefaultUserAgent() {
    QString OS;
#ifdef Q_OS_LINUX
    OS = "Linux";
#endif
#ifdef Q_OS_FREEBSD
    OS = "FreeBSD";
#endif
#ifdef Q_OS_OPENBSD
    OS = "OpenBSD";
#endif
#ifdef Q_OS_WIN
    OS = GetWinVer();
#endif
#ifdef Q_OS_MAC
    OS = "Mac OS X";
#endif
    if(OS.isEmpty()) OS = "Unknown";
    return "Mozilla/5.0 (compatible; "+OS+"; "+APPNAME+" "+APPVER+" ("+QString::number(CURRVER)+"))";
}

void DefaultUserAgents(QList<TUserAgent> &rAgents) {
    httpResponse response;
    TUserAgent agent;
    QString ua;
    QString ft;
    ua = DefaultUserAgent();
    //QString url = "http://etc.matthewhipkin.co.uk/httpres/default-useragents.php";
    QString url = "http://httpres.sourceforge.net/default-useragents.php";
    HTTPObj resp;
    response = resp.httpGet(url,ua);
    QXmlStreamReader xmlReader(response.content);
    xmlReader.readNext();
    while(!xmlReader.isEndDocument()) {
        if(xmlReader.isStartElement()) {
            if(xmlReader.name() == "item") {
                ua.clear();
                ft.clear();
            }
            if(xmlReader.name() == "useragent") ua = xmlReader.readElementText();
            if(xmlReader.name() == "title") ft = xmlReader.readElementText();
        }
        if(xmlReader.isEndElement()) {
            if(xmlReader.name() == "item") {
                agent.friendlyName = ft;
                agent.userAgent = ua;
                rAgents.append(agent);
            }
        }
        xmlReader.readNext();
    }
}

UserAgentWindow::UserAgentWindow(QWidget *parent) : QDialog(parent), ui(new Ui::UserAgentWindow) {
    ui->setupUi(this);
    ListChanged = false;
}

UserAgentWindow::~UserAgentWindow() {
    delete ui;
}

void UserAgentWindow::PopulateList() {
    int i;
    ui->listUserAgents->clear();
    for(i=0;i < UserAgents.count();i++) {
        ui->listUserAgents->addItem(UserAgents.at(i).friendlyName);
    }
}

void UserAgentWindow::on_btnOK_clicked() {
    ListChanged = true;
    this->accept();
}

void UserAgentWindow::on_btnAdd_clicked() {
    UserAgentEdit fmAgentEdit;
    fmAgentEdit.setWindowTitle("Add Useragent");
    fmAgentEdit.exec();
    if(fmAgentEdit.result() == QDialog::Accepted) {
        TUserAgent agent;
        agent.friendlyName = fmAgentEdit.Title;
        agent.userAgent = fmAgentEdit.Useragent;
        UserAgents.append(agent);
        // Make sure indices match
        PopulateList();
    }
}

void UserAgentWindow::on_btnEdit_clicked() {
    UserAgentEdit fmAgentEdit;
    QString cTitle, cUseragent;
    int index = ui->listUserAgents->currentRow();
    cTitle = UserAgents.at(index).friendlyName;
    cUseragent = UserAgents.at(index).userAgent;
    fmAgentEdit.SetUserAgent(cTitle,cUseragent);
    fmAgentEdit.setWindowTitle("Edit " + cTitle);
    fmAgentEdit.exec();    
    if(fmAgentEdit.result() == QDialog::Accepted) {
        TUserAgent agent;
        agent.friendlyName = fmAgentEdit.Title;
        agent.userAgent = fmAgentEdit.Useragent;
        UserAgents.replace(index,agent);
        // Make sure indices match
        PopulateList();
    }
}

void UserAgentWindow::on_btnDelete_clicked() {
    int index = ui->listUserAgents->currentRow();
    ui->listUserAgents->model()->removeRow(ui->listUserAgents->currentRow());
    UserAgents.removeAt(index);
}

void UserAgentWindow::on_btnDefault_clicked() {
    UserAgents.clear();
    DefaultUserAgents(UserAgents);
    PopulateList();
}

