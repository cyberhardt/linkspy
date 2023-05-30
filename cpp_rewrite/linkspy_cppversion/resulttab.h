#ifndef RESULTTAB_H
#define RESULTTAB_H

#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QTextBrowser>
#include <QTextEdit>
#include <QSplitter>
#include <QMenu>
#include <QListWidget>
#include <QToolBar>
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QSqlError>
#include <QSqlRecord>
#include <QDir>
#include <QLabel>
#include <QPushButton>
#include <QHostInfo>
#include <QDebug>
#include <cstring>
#include <libxml/HTMLparser.h>
#include "xmlhighlighter.h"
#include "httpfunc.h"

class ResultTab : public QWidget
{
    Q_OBJECT
public:
    explicit ResultTab(QFont font, QWidget *parent = 0);
    httpResponse response;
    bool UsePOST;
    QList<TPostVariable> postvars;
    QTextEdit *textContent;
    QTextBrowser *textHeaders;
    QVBoxLayout *layout;
    QHBoxLayout *hlayout;
    QSplitter *splitter;
    QSplitter *hsplitter;
    QListWidget *listFoundLinks;
    QWidget *viewArea;
    QToolBar *tabToolBar;
    QLabel *labelCountry;
    QLabel *labelIP;
    QPushButton *btnWhois;
    QPushButton *btnPing;
    void SetHeaderText(QString t);
    QString GetHeaderText();
    void SetContentText(QString t);
    QString GetContentText();
    XmlHighlighter *highlighter;
    void SetEditorFont(QFont f);
    void FindLinks();    

private:
    void ParseLinks(xmlNode *a_node);
    QString GetCountry(QString ip);
    void SetCountry();
signals:

public slots:
    void TextBrowserContextMenu(const QPoint& aPosition);
};

#endif // RESULTTAB_H
