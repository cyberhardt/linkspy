#ifndef USERAGENTWINDOW_H
#define USERAGENTWINDOW_H

#include <QDialog>
#include <QString>
#include <QXmlStreamReader>
#include "useragentedit.h"
#include "appvars.h"
#ifdef Q_OS_WIN
    #include "windows.h"
    #include "winnt.h"
#endif
#include "httpfunc.h"

struct TUserAgent {
    QString friendlyName;
    QString userAgent;
};

void DefaultUserAgents(QList<TUserAgent> &rAgents);
QString DefaultUserAgent();

namespace Ui {
class UserAgentWindow;
}

class UserAgentWindow : public QDialog
{
    Q_OBJECT

public:
    explicit UserAgentWindow(QWidget *parent = 0);

    ~UserAgentWindow();
    QList<TUserAgent> UserAgents;
    void PopulateList();
    bool ListChanged;

private slots:
    void on_btnOK_clicked();
    void on_btnAdd_clicked();
    void on_btnEdit_clicked();
    void on_btnDelete_clicked();
    void on_btnDefault_clicked();

private:
    Ui::UserAgentWindow *ui;
};

#endif // USERAGENTWINDOW_H
