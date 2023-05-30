#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) : QMainWindow(parent), ui(new Ui::MainWindow) {
    ui->setupUi(this);
    int h = 32;
    this->setWindowTitle(APPNAME);
    ui->mainToolBar->setFixedHeight(42);
    connect(ui->tabWidget, SIGNAL(tabCloseRequested(int)), this, SLOT(closeTab(int)));
    textURL = new QComboBox(this);
    textURL->setMinimumWidth(400);
    textURL->setEditable(true);
    textURL->setFixedHeight(h);
    ui->mainToolBar->addWidget(textURL);
    btnGo = new QToolButton(this);
    btnGo->setMaximumSize(h,h);
    btnGo->setStyleSheet("background-repeat: no-repeat; background-position: center; background-image: url(:/images/playback_play_icon.png);");
    connect(btnGo,SIGNAL(clicked()),this,SLOT(btnGoClick()));
    ui->mainToolBar->addWidget(btnGo);
    checkPOST = new QCheckBox(this);
    checkPOST->setText("Use POST");
    ui->mainToolBar->addWidget(checkPOST);
    btnRefresh = new QToolButton(this);
    btnRefresh->setMaximumSize(h,h);
    btnRefresh->setStyleSheet("background-repeat: no-repeat; background-position: center; background-image: url(:/images/reload_icon.png);");
    ui->mainToolBar->addWidget(btnRefresh);
    btnBatch = new QToolButton(this);
    btnBatch->setMaximumSize(h,h);
    btnBatch->setStyleSheet("background-repeat: no-repeat; background-position: center; background-image: url(:/images/playback_ff_icon.png);");
    connect(btnBatch,SIGNAL(clicked()),this,SLOT(btnBatchClick()));
    ui->mainToolBar->addWidget(btnBatch);
    ui->mainToolBar->addSeparator();
    btnSave = new QToolButton(this);
    btnSave->setMaximumSize(h,h);
    btnSave->setStyleSheet("background-repeat: no-repeat; background-position: center; background-image: url(:/images/save_icon.png);");
    ui->mainToolBar->addWidget(btnSave);
    ui->mainToolBar->addSeparator();
    listUserAgents = new QComboBox(this);
    listUserAgents->setMinimumWidth(150);
    listUserAgents->setFixedHeight(h);
    ui->mainToolBar->addWidget(listUserAgents);
    btnUserAgentEdit = new QToolButton(this);
    btnUserAgentEdit->setMaximumSize(h,h);
    btnUserAgentEdit->setText("...");
    connect(btnUserAgentEdit,SIGNAL(clicked()),this,SLOT(btnAgentEditClick()));
    ui->mainToolBar->addWidget(btnUserAgentEdit);
    ui->mainToolBar->addSeparator();
    btnConfig = new QToolButton(this);
    btnConfig->setMaximumSize(h,h);
    btnConfig->setStyleSheet("background-repeat: no-repeat; background-position: center; background-image: url(:/images/wrench_plus_2_icon.png);");
    connect(btnConfig,SIGNAL(clicked()),this,SLOT(btnConfigClick()));
    ui->mainToolBar->addWidget(btnConfig);
    btnAbout = new QToolButton(this);
    btnAbout->setMaximumSize(h,h);
    btnAbout->setStyleSheet("background-repeat: no-repeat; background-position: center; background-image: url(:/images/info_icon.png);");
    connect(btnAbout,SIGNAL(clicked()),this,SLOT(btnAboutClick()));
    ui->mainToolBar->addWidget(btnAbout);
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui->mainToolBar->addWidget(spacer);
    imgThrobber = new QLabel(this);
    imgThrobber->setMaximumSize(h,h);
    ui->mainToolBar->addWidget(imgThrobber);
    // Batch list results
    widgetBatch = new QWidget(this);
    labelBatchTitle = new QLabel(this);
    labelBatchTitle->setText("Batch Results");
    btnCloseBatch = new QToolButton(this);
    btnCloseBatch->setMaximumSize(h,h);
    connect(btnCloseBatch,SIGNAL(clicked()),this,SLOT(btnCloseBatchClick()));
    QVBoxLayout *batchLayout = new QVBoxLayout;
    batchLayout->setContentsMargins(0,0,0,0);
    QFrame *batchTopFrame = new QFrame;
    batchTopFrame->setMaximumHeight(25);
    QHBoxLayout *batchTopFrameLayout = new QHBoxLayout;
    batchTopFrameLayout->setContentsMargins(4,0,4,0);
    batchTopFrameLayout->addWidget(labelBatchTitle);
    batchTopFrameLayout->addWidget(btnCloseBatch);
    batchTopFrame->setLayout(batchTopFrameLayout);
    batchLayout->addWidget(batchTopFrame);
    listResults = new QTableWidget(this);
    listResults->setMinimumHeight(100);
    listResults->setEditTriggers(QAbstractItemView::NoEditTriggers);
    listResults->setSelectionMode(QAbstractItemView::SingleSelection);
    listResults->setSelectionBehavior(QAbstractItemView::SelectRows);
    listResults->horizontalHeader()->setVisible(false);
    listResults->verticalHeader()->setVisible(false);
    batchLayout->addWidget(listResults);
    connect(listResults,SIGNAL(cellDoubleClicked(int,int)),this,SLOT(listResultsCellDoubleClicked(int,int)));
    widgetBatch->setLayout(batchLayout);
    splitterBatch = new QSplitter(Qt::Vertical);
    ResetTable();
    ShowBatchTable(false);
    // App dir
    appDir = QDir::homePath() + "/.linkspy/";
    if(!appDir.exists()) appDir.mkdir(appDir.absolutePath());
    LoadHistory();
    LoadUserAgents();
    // Config
    LoadConfig();
    // Tray icon
    QIcon appicon(":/images/httpres-16px.png");
    trayIcon = new QSystemTrayIcon(this);
    trayIcon->setIcon(appicon);
    trayIcon->setVisible(true);
    connect(trayIcon,SIGNAL(activated(QSystemTrayIcon::ActivationReason)),this,SLOT(trayIconActivated(QSystemTrayIcon::ActivationReason)));
    trayMenu = new QMenu(this);
    menuRestore = new QAction("Restore",this);
    connect(menuRestore,SIGNAL(triggered()),this,SLOT(menuRestoreClick()));
    menuExit = new QAction("Exit",this);
    connect(menuExit,SIGNAL(triggered()),this,SLOT(menuExitClick()));
    trayMenu->addAction(menuRestore);
    trayMenu->addAction(menuExit);
    // Clipboard
    board = QApplication::clipboard();
    connect(board,SIGNAL(changed(QClipboard::Mode)),this,SLOT(clipboardChanged(QClipboard::Mode)));
    // Check for updates
    QTimer::singleShot(200, this, SLOT(checkUpdates()));
}

MainWindow::~MainWindow() {
    delete ui;
}

int MainWindow::FindTab(QString url) {
    int i;
    int tab = -1;
    if(ui->tabWidget->count() > 0) {
        for(i = 0; i < ui->tabWidget->count(); i++) {
            if(qobject_cast<ResultTab*>(ui->tabWidget->widget(i))->response.URL == url) tab = i;
        }
    }
    return tab;
}

void MainWindow::checkUpdates() {
    httpResponse response;
    QString ua;
    ua = DefaultUserAgent();
    QString url = "http://www.matthewhipkin.co.uk/httpres.txt";
    HTTPObj resp;
    response = resp.httpGet(url,ua);
    QString ver = response.content;
    if(ver.trimmed().toInt() > CURRVER) {
        labelUpdate = new QLabel(this);
        labelUpdate->setText("<p>A new version is available, <a href=\"http://www.matthewhipkin.co.uk\">click here</a> to get it!");
        labelUpdate->setVisible(true);
        labelUpdate->setStyleSheet("background-color: qlineargradient(spread:pad, x1:0, y1:1, x2:0, y2:0, stop:0 rgba(255, 204, 204, 255), stop:1 rgba(255, 255, 255, 255)); }");
        labelUpdate->setTextInteractionFlags(Qt::TextBrowserInteraction);
        labelUpdate->setOpenExternalLinks(true);
        ui->centralWidget->layout()->addWidget(labelUpdate);
    }
}

void MainWindow::LoadUserAgents() {
    QFile xmlFile(appDir.absoluteFilePath("ua.xml"));
    if(!xmlFile.exists()) {
        /*QMessageBox::StandardButton reply;
        reply = QMessageBox::question(this, "No Useragents", "You have no useragents loaded.\nDownload defaults now?", QMessageBox::Yes|QMessageBox::No);
        if (reply == QMessageBox::Yes) {
            AddDefaultUserAgents();
            SaveUserAgents();
        }
        else {
            on_btnAgentEdit_clicked();
        }
        return;*/
    }
    if(xmlFile.open(QIODevice::ReadOnly)) {
        QXmlStreamReader xmlReader;
        xmlReader.setDevice(&xmlFile);
        QString ua;
        QString ft;
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
                    listUserAgents->addItem(ft,QVariant(ua));
                }
            }
            xmlReader.readNext();
        }
    }
}

void MainWindow::SaveUserAgents() {
    QFile xmlFile(appDir.absoluteFilePath("ua.xml"));
    int i;
    if(xmlFile.open(QIODevice::WriteOnly)) {
        QTextStream outstream(&xmlFile);
        outstream << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
        outstream << "<useragents>\n";
        for(i=0;i<listUserAgents->count();i++) {
            outstream << "  <item>\n";
            outstream << "    <title>" << listUserAgents->itemText(i) << "</title>\n";
            outstream << "    <useragent>" << listUserAgents->itemData(i).toString() << "</useragent>\n";
            outstream << "  </item>\n";
        }
        outstream << "</useragents>\n";
        xmlFile.close();
    }
}

void MainWindow::LoadHistory() {
    QFile file(appDir.absoluteFilePath("history.txt"));
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QTextStream in(&file);
        while (!in.atEnd()) {
            QString line = in.readLine();
            textURL->addItem(line);
        }
        file.close();
    }
}

void MainWindow::SaveHistory() {
    QFile file(appDir.absoluteFilePath("history.txt"));
    file.open(QIODevice::WriteOnly | QIODevice::Text);
    QTextStream out(&file);
    for(int x = 0; x < textURL->count();x++) {
        out << textURL->itemText(x) << "\n";
    }
    file.close();
}

void MainWindow::LoadConfig() {
    // Set defaults
    editorFontName = "Monospace";
    editorFontSize = 11;
    QFile xmlFile(appDir.absoluteFilePath("config.xml"));
    if(xmlFile.exists()) {
        if(xmlFile.open(QIODevice::ReadOnly)) {
            QXmlStreamReader xmlReader;
            xmlReader.setDevice(&xmlFile);
            xmlReader.readNext();
            while(!xmlReader.isEndDocument()) {
                if(xmlReader.isStartElement()) {
                    if(xmlReader.name() == "font") editorFontName = xmlReader.readElementText();
                    if(xmlReader.name() == "fontsize") editorFontSize = xmlReader.readElementText().toInt();
                }
                xmlReader.readNext();
            }
        }
        xmlFile.close();
    }
    ApplyConfig();
}

void MainWindow::SaveConfig() {
    QFile xmlFile(appDir.absoluteFilePath("config.xml"));
    if(xmlFile.open(QIODevice::WriteOnly)) {
        QTextStream outstream(&xmlFile);
        outstream << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
        outstream << "<config>\n";
        outstream << "  <font>" << editorFontName << "</font>\n";
        outstream << "  <fontsize>" << editorFontSize << "</fontsize>\n";
        outstream << "</config>\n";
    }
    xmlFile.close();
}

void MainWindow::ApplyConfig() {
    QFont font(editorFontName);
    font.setPointSize(editorFontSize);
    font.setStyleHint(QFont::TypeWriter);
    int i;
    for(i = 0; i < ui->tabWidget->count(); i++) {
        qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->SetEditorFont(font);
    }
}

void MainWindow::ResetTable() {
    QStringList headers;
    headers << "URL" << "Response Code" << "Response Size";
    listResults->setColumnCount(3);
    listResults->setRowCount(0);
    listResults->setHorizontalHeaderLabels(headers);
    listResults->horizontalHeader()->setVisible(true);
    listResults->horizontalHeader()->setStretchLastSection(true);
}

void MainWindow::ShowBatchTable(bool m) {
    if(m) {
        splitterBatch->addWidget(ui->tabWidget);
        splitterBatch->addWidget(widgetBatch);
        splitterBatch->setVisible(true);
        widgetBatch->setVisible(true);
        ui->centralWidget->layout()->addWidget(splitterBatch);
    }
    else {
        splitterBatch->setVisible(false);
        ui->centralWidget->layout()->removeWidget(splitterBatch);
        widgetBatch->setVisible(false);
        ui->centralWidget->layout()->addWidget(ui->tabWidget);
    }
}

void MainWindow::ShowThrobber(bool m) {
    if(m) {
        throbberAnim = new QMovie(":/images/throbber.gif");
        imgThrobber->setMovie(throbberAnim);
        throbberAnim->start();
    }
    else {
        throbberAnim = new QMovie(":/images/Transparent.gif");
        imgThrobber->setMovie(throbberAnim);
        throbberAnim->start();
    }
}

httpResponse MainWindow::CheckURL(QString url, QString useragent, bool usePOST, QList<TPostVariable> postvars, bool IsBatch) {
    int tabIndex = FindTab(url);
    httpResponse response;
    HTTPObj resp;
    if(usePOST) {
        response = resp.httpPost(url,useragent,postvars);
    }
    else {
        response = resp.httpGet(url,listUserAgents->itemData(listUserAgents->currentIndex()).toString());
    }
    if(!IsBatch) {
        if(tabIndex == -1) {
            QFont font(editorFontName);
            font.setPointSize(editorFontSize);
            font.setStyleHint(QFont::TypeWriter);
            tabIndex = ui->tabWidget->addTab(new ResultTab(font,this), textURL->currentText());
        }
        ui->tabWidget->setCurrentIndex(tabIndex);
        char status[150];
        sprintf(status,"Server returned code %i (%li bytes)",response.rescode,response.ressize);
        ui->statusBar->showMessage(status,0);
        qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->response = response;
        qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->SetHeaderText(response.header);
        // Adjust size of header
        int h;
        h = qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->size().height();
        qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->textHeaders->setMaximumHeight(h + 10);
        qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->SetContentText(response.content);        
        qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->FindLinks();
        if(usePOST) {
            qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->UsePOST = true;
            qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->postvars = postvars;
        }
    }
    return response;
}

void MainWindow::btnGoClick() {
    // JUST A TEST
    /*socket = new QTcpSocket(this);
    socket->connectToHost("whois.iana.org", 43);
    if(socket->waitForConnected(3000)) {
        socket->write("bbc.co.uk\r\n");
        socket->waitForBytesWritten(1000);
        socket->waitForReadyRead(3000);
        qDebug() << "Reading: " << socket->bytesAvailable();

        qDebug() << socket->readAll();

        socket->close();
    }

    return;*/
    // END OF TEST
    ShowThrobber(true);
    httpResponse response;
    if(checkPOST->isChecked()) {
        PostWindow fmPost;
        fmPost.exec();
        if(fmPost.result() != QDialog::Accepted) return;       
        response = CheckURL(textURL->currentText(),listUserAgents->itemData(listUserAgents->currentIndex()).toString(),true,fmPost.postvars,false);
    }
    else {
        response = CheckURL(textURL->currentText(),listUserAgents->itemData(listUserAgents->currentIndex()).toString(),false,QList<TPostVariable>(),false);
    }
    // Add to history
    bool addNew = true;
    for(int x = 0; x < textURL->count();x++) {
        if(textURL->itemText(x) == textURL->lineEdit()->text()) addNew = false;
    }
    if(textURL->currentText() == "http://") addNew = false;
    if(addNew) {
        textURL->addItem(textURL->lineEdit()->text());
    }
    SaveHistory();
    ShowThrobber(false);
}

void MainWindow::closeTab(const int& index) {
    if (index == -1) {
        return;
    }
    QWidget* tabItem = ui->tabWidget->widget(index);
    ui->tabWidget->removeTab(index);
    delete(tabItem);
    tabItem = nullptr;
}

void MainWindow::trayIconActivated(QSystemTrayIcon::ActivationReason reason) {
    if(reason == QSystemTrayIcon::DoubleClick) {
        if( isVisible() ) {
            hide();
        }
        else {
            show();
            setWindowState(Qt::WindowActive);
        }
    }
    if(reason == QSystemTrayIcon::Context) {
        trayMenu->popup(QCursor::pos());
    }
}

void MainWindow::changeEvent(QEvent *e) {
    QMainWindow::changeEvent(e);
    switch (e->type()) {
        case QEvent::WindowStateChange: {
                if(isMinimized()) {
                    QTimer::singleShot(0, this, SLOT(hide()));
                }
            }
            break;
        default:
            break;
    }
}

void MainWindow::clipboardChanged(QClipboard::Mode mode) {
    if(QApplication::clipboard()->mimeData()->hasText()) {
        if(lastClipboardURL != board->text()) {
            //trayIcon->showMessage("New URL","A new URL has been found in the clipboard,\nClick here to check it");
            QStringList boarditems = board->text().split("\n");
            int i;
            QString currentitem;
            for(i = 0; i < boarditems.count(); i++) {
                currentitem = boarditems.at(i);
                if(currentitem.startsWith("http://") || currentitem.startsWith("https://")) {
                    ClipboardURLs << currentitem.trimmed();
                }
            }
            QIcon appicon(":/images/httpres-16px-highlighted.png");
            trayIcon->setIcon(appicon);
            trayIcon->setVisible(true);
            lastClipboardURL = board->text();
        }
    }
}

void MainWindow::btnAgentEditClick() {
    UserAgentWindow fmAgent;
    TUserAgent a;
    int i;
    for(i = 0; i < listUserAgents->count(); i++) {
        a.friendlyName = listUserAgents->itemText(i);
        a.userAgent = listUserAgents->itemData(i).toString();
        fmAgent.UserAgents.append(a);
    }
    fmAgent.PopulateList();
    fmAgent.exec();
    if(fmAgent.Accepted && fmAgent.ListChanged) {
        listUserAgents->clear();
        for(i=0;i<fmAgent.UserAgents.count();i++) {
            listUserAgents->addItem(fmAgent.UserAgents[i].friendlyName,QVariant(fmAgent.UserAgents[i].userAgent));
        }
        SaveUserAgents();
    }
}

void MainWindow::btnRefreshClick() {

}

void MainWindow::btnConfigClick() {
    OptionsForm fmOptions;
    fmOptions.SetOptions(editorFontName,editorFontSize);
    int r = fmOptions.exec();
    if(r == QDialog::Accepted) {
        if(fmOptions.ClearHistory) {
            textURL->clear();
            SaveHistory();
        }
        editorFontName = fmOptions.DisplayConfig.FontName;
        editorFontSize = fmOptions.DisplayConfig.FontSize;
        SaveConfig();
        ApplyConfig();
    }
}

void MainWindow::menuRestoreClick() {
    show();
    setWindowState(Qt::WindowActive);
}

void MainWindow::menuExitClick() {
    QApplication::quit();
}

void MainWindow::btnAboutClick() {
    QString html;
    html = "<p><b style=\"font-size: 14pt\">"+QString(APPNAME)+"</b> "+QString(APPVER)+"<br>\n";
    html.append("&copy;2013-2018 <a href=\"https://www.matthewhipkin.co.uk\" style=\"color: #FF0000\">Matthew Hipkin</a><br>\n");
    html.append("<a href=\"https://linkspy.sourceforge.io\" style=\"color: #FF0000\">https://linkspy.sourceforge.io</a></p>\n");
    html.append("<p>A simple HTTP response checking application.</p>");
    html.append("<p><a href=\"https://twitter.com/hippy2094\"><img src=\":/images/logo_twitter_25px.png\"></a> <a href=\"https://sourceforge.net/projects/linkspy/\"><img src=\":/images/sourceforge_logo_small.png\"></a></p>");
    QMessageBox::about(this,"About "+QString(APPNAME),html);
}

void MainWindow::btnBatchClick() {    
    BatchList fmBatch;
    QFont font(editorFontName);
    font.setPointSize(editorFontSize);
    font.setStyleHint(QFont::TypeWriter);
    fmBatch.SetEditorFont(font);
    int r = fmBatch.exec();
    if(r == QDialog::Accepted) {
        QStringList lines = fmBatch.GetEditorText().split("\n");
        int i;
        ShowThrobber(true);
        httpResponse response;
        for(i = 0; i < lines.count(); i++) {
            char status[50];
            sprintf(status,"Checking URL %i of %i",(i+1),lines.count());
            ui->statusBar->showMessage(status);
            response = CheckURL(lines.at(i).trimmed(),listUserAgents->itemData(listUserAgents->currentIndex()).toString(),false,QList<TPostVariable>(),true);
            QTableWidgetItem* url = new QTableWidgetItem;
            QTableWidgetItem* rescode = new QTableWidgetItem;
            QTableWidgetItem* ressize = new QTableWidgetItem;
            url->setText(response.URL);
            QVariant res;
            res.setValue(response);
            url->setData(Qt::UserRole,res);
            rescode->setText(QString::number(response.rescode));
            ressize->setText(QString::number(response.ressize));
            listResults->insertRow(i);
            listResults->setItem(i,0,url);
            listResults->setItem(i,1,rescode);
            listResults->setItem(i,2,ressize);
            //qApp->processEvents();            
        }
        listResults->resizeColumnsToContents();
        ShowThrobber(false);
        ui->statusBar->showMessage("Done");
        ShowBatchTable(true);
    }
}

void MainWindow::btnCloseBatchClick() {
    ShowBatchTable(false);
}

void MainWindow::listResultsCellDoubleClicked(int row, int column) {  
    QTableWidgetItem* item;
    item = listResults->item(row,0);
    QVariant r;
    r.setValue(item->data(Qt::UserRole));
    httpResponse response = r.value<httpResponse>();
    int tabIndex = FindTab(response.URL);
    if(tabIndex == -1) {
        QFont font(editorFontName);
        font.setPointSize(editorFontSize);
        font.setStyleHint(QFont::TypeWriter);
        tabIndex = ui->tabWidget->addTab(new ResultTab(font,this), response.URL);
    }
    ui->tabWidget->setCurrentIndex(tabIndex);
    qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->SetHeaderText(response.header);
    qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->SetContentText(response.content);
    qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->response = response;
    char status[150];
    sprintf(status,"Server returned code %i (%li bytes)",response.rescode,response.ressize);
    ui->statusBar->showMessage(status,0);
    // Adjust size of header
    int h;
    h = qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->size().height();
    qobject_cast<ResultTab*>(ui->tabWidget->currentWidget())->textHeaders->setMaximumHeight(h + 10);

}

void MainWindow::on_tabWidget_currentChanged(int index) {
    if(index < 0) return;
    httpResponse response;
    response = qobject_cast<ResultTab*>(ui->tabWidget->widget(index))->response;
    char status[150];
    sprintf(status,"Server returned code %i (%li bytes)",response.rescode,response.ressize);
    ui->statusBar->showMessage(status,0);
}
