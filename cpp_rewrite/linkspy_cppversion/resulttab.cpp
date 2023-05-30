#include "resulttab.h"

bool IsFirstChar(QString inStr) {
    bool Result;
    Result = false;
    if(inStr.startsWith("\"")) Result = true;
    if(inStr.startsWith(" ")) Result = true;
    if(inStr.startsWith("'")) Result = true;
    if(inStr.startsWith("=")) Result = true;
    return Result;
}

bool IsLastChar(QString inStr) {
    bool Result;
    Result = false;
    if(inStr.endsWith("\"")) Result = true;
    if(inStr.endsWith(" ")) Result = true;
    if(inStr.endsWith("'")) Result = true;
    if(inStr.endsWith("=")) Result = true;
    return Result;
}

long IP2Long(QString ip) {
    QStringList parts;
    parts << ip.split(".");
    if(parts.count() == 4) {
        return (parts[0].toLong() << 24) | (parts[1].toLong() << 16) | (parts[2].toLong() << 8) | parts[3].toLong();
    }
    else return 0;
}

htmlDocPtr ParseHTML(char *doclocation, char *encoding, char *baseurl) {
  htmlDocPtr doc;
  doc = htmlReadMemory(doclocation, strlen(doclocation),baseurl, encoding, HTML_PARSE_RECOVER | HTML_PARSE_NOIMPLIED | HTML_PARSE_NOBLANKS | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING | HTML_PARSE_NONET);
  return doc;
}

bool IsValidLink(QString link) {
    bool Result = true;
    if(link.startsWith("ftp:")) Result = false;
    if(link.startsWith("mailto:")) Result = false;
    if(link.startsWith("javascript:")) Result = false;
    if(link.startsWith("skype:")) Result = false;
    if(link.startsWith("news:")) Result = false;
    if(link.startsWith("gopher:")) Result = false;
    if(link.startsWith("file:")) Result = false;
    if(link.startsWith("#")) Result = false;
    if(link.length() < 1) Result = false;
    return Result;
}

bool IsAbsoluteLink(QString link) {
    if(link.startsWith("http://") || link.startsWith("https://")) return true;
    else return false;
}

QString ResolveLink(QString link, QString host) {
    QString Result = "";
    QUrl baseURL(host);
    QUrl relativeURL(link);
    if(link.startsWith("//")) {
        Result = baseURL.scheme() + ":" + link;
    }
    else {
        Result = baseURL.resolved(relativeURL).toString();
    }
    return Result;
}

ResultTab::ResultTab(QFont font, QWidget *parent) : QWidget(parent) {
    hlayout = new QHBoxLayout(this);
    layout = new QVBoxLayout(this);
    hlayout->setContentsMargins(0,0,0,0);
    layout->setContentsMargins(0,0,0,0);
    splitter = new QSplitter(Qt::Vertical);
    hsplitter = new QSplitter(Qt::Horizontal);
    textContent = new QTextEdit(this);
    textHeaders = new QTextBrowser(this);
    textContent->setLineWrapMode(QTextEdit::NoWrap);
    textHeaders->setLineWrapMode(QTextEdit::NoWrap);
    textHeaders->setGeometry(0,0,10,150);    
    //textContent->setOpenExternalLinks(true);
    textContent->setAcceptRichText(false);
    textContent->setReadOnly(true);
    textContent->setUndoRedoEnabled(false);
    textHeaders->setOpenExternalLinks(true);    
    textContent->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(textContent,SIGNAL(customContextMenuRequested(QPoint)),this,SLOT(TextBrowserContextMenu(QPoint)));
    highlighter = new XmlHighlighter(textContent->document());
    highlighter->DefaultScheme(font);
    SetEditorFont(font);
    tabToolBar = new QToolBar(this);
    tabToolBar->setMaximumHeight(38);
    tabToolBar->setMinimumHeight(38);
    tabToolBar->setOrientation(Qt::Horizontal);
    labelCountry = new QLabel(this);
    labelIP = new QLabel(this);
    tabToolBar->addWidget(labelCountry);
    tabToolBar->addWidget(labelIP);
    btnWhois = new QPushButton(this);
    btnWhois->setText("Whois");
    tabToolBar->addWidget(btnWhois);
    btnPing = new QPushButton(this);
    btnPing->setText("Ping");
    tabToolBar->addWidget(btnPing);
    splitter->addWidget(textHeaders);
    splitter->addWidget(textContent);
    QList<int> sizes;
    sizes << 100 << 200;
    splitter->setSizes(sizes);
    layout->addWidget(tabToolBar);
    layout->addWidget(splitter);
    viewArea = new QWidget(this);
    viewArea->setLayout(layout);
    hsplitter->addWidget(viewArea);
    sizes.clear();
    sizes << 800 << 100;
    hsplitter->setSizes(sizes);
    listFoundLinks = new QListWidget(this);
    hsplitter->addWidget(listFoundLinks);
    hlayout->addWidget(hsplitter);
    setLayout(hlayout);
}

void ResultTab::SetHeaderText(QString t) {
    textHeaders->document()->setPlainText(t);
    SetCountry();
}

QString ResultTab::GetHeaderText() {
    return textHeaders->document()->toPlainText();
}

void ResultTab::SetContentText(QString t) {
    textContent->document()->setPlainText(t);
}

QString ResultTab::GetContentText() {
    return textContent->document()->toPlainText();
}

void ResultTab::TextBrowserContextMenu(const QPoint& aPosition) {
    QTextCursor tc = textContent->cursorForPosition(aPosition);
    tc.select(QTextCursor::WordUnderCursor);
    QString s;
    bool n = false;
    while(!n) {
        if(tc.movePosition(QTextCursor::PreviousCharacter,QTextCursor::KeepAnchor)) {
            n = IsFirstChar(tc.selectedText());
        }
        else {
            n = true;
        }
    }
    s = tc.selectedText();
    n = false;
    while(!n) {
        if(tc.movePosition(QTextCursor::NextCharacter,QTextCursor::KeepAnchor)) {
            n = IsLastChar(tc.selectedText());
        }
        else {
            n = true;
        }
    }
    s += tc.selectedText();
    s.replace("\n","");
    s.replace("<","");
    s.replace("=","");
    s.replace("'","");
    s.replace("\"","");
    QMenu *menu = new QMenu;
    if(s.startsWith("http://") || s.startsWith("https://")) {
        menu->addAction("Check URL");
        menu->addAction("Open URL in default browser");

    }
    menu->addAction("Copy");
    menu->exec(textContent->mapToGlobal(aPosition));
}

void ResultTab::SetEditorFont(QFont f) {
    textContent->setFont(f);
    textHeaders->setFont(f);
    //highlighter->SetFont(f);
    //highlighter->rehighlight();
}

void ResultTab::FindLinks() {
    htmlDocPtr doc;
    doc = ParseHTML((char*)textContent->document()->toPlainText().toStdString().c_str(), (char*)"UTF-8", (char*)response.URL.toStdString().c_str());
    xmlNode *cur = NULL;
    cur = xmlDocGetRootElement(doc);
    if (cur == NULL) {
        xmlFreeDoc(doc);
        return;
    }
    ParseLinks(cur);
}

void ResultTab::ParseLinks(xmlNode *a_node) {
    xmlNode *cur_node = NULL;

    for(cur_node = a_node; cur_node; cur_node = cur_node->next) {
        if(cur_node->type == XML_ELEMENT_NODE) {
            if(std::strcmp((char*)cur_node->name,"a") == 0) {
                xmlAttr *attr = cur_node->properties;
                QString link;
                while(attr) {
                    QString cattr = (char*)attr->name;
                    if(cattr == "href") link = QString((char*)attr->children->content);
                    attr = attr->next;
                }
                if(!link.isEmpty()) {
                    if(IsValidLink(link) && IsAbsoluteLink(link)) listFoundLinks->addItem(link.trimmed());
                    else {
                        listFoundLinks->addItem(ResolveLink(link,response.URL));
                    }
                }
            }
        }
        ParseLinks(cur_node->children);
    }
}

QString ResultTab::GetCountry(QString ip) {
    QSqlDatabase db;
    QString Result;
    db = QSqlDatabase::addDatabase("QSQLITE");
    db.setDatabaseName(QDir::currentPath() + "/geoip.db");
    if(db.open()) {
        QSqlQuery query;
        QString num;
        num = QString::number(IP2Long(ip.trimmed()));
        if(query.exec("SELECT country_code FROM geoip WHERE ip_from <= "+num+" AND ip_to >= "+num)) {
            if(query.first()) {
                Result = query.record().value("country_code").toString();
            }
        }
        db.close();
    }
    return Result;
}

void ResultTab::SetCountry() {
    QUrl host;
    host.setUrl(response.URL);
    QHostInfo info = QHostInfo::fromName(host.host());
    if (!info.addresses().isEmpty()) {
        QHostAddress address = info.addresses().first();
        QString country = GetCountry(address.toString());
        if(!country.isEmpty()) {
            QPixmap pix(":/images/flags/"+country+".png");
            labelCountry->setPixmap(pix);
            labelIP->setText(address.toString());
        }
    }
}
