#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QComboBox>
#include <QToolButton>
#include <QCheckBox>
#include <QFile>
#include <QDir>
#include <QMessageBox>
#include <QXmlStreamReader>
#include <QSystemTrayIcon>
#include <QLineEdit>
#include <QClipboard>
#include <QMimeData>
#include <QTimer>
#include <QLabel>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QMovie>
#include <QHeaderView>
#include <QTcpSocket>
#include <QDebug>
#include "resulttab.h"
#include "xmlhighlighter.h"
#include "httpfunc.h"
#include "postwindow.h"
#include "useragentwindow.h"
#include "options.h"
#include "batchlist.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow {
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();
    QComboBox *textURL;
    QToolButton *btnGo;
    QToolButton *btnRefresh;
    QToolButton *btnBatch;
    QComboBox *listUserAgents;
    QToolButton *btnUserAgentEdit;
    QToolButton *btnSave;
    QToolButton *btnConfig;
    QToolButton *btnAbout;
    QCheckBox *checkPOST;
    QSystemTrayIcon *trayIcon;
    QMenu *trayMenu;
    QAction *menuRestore;
    QAction *menuExit;
    QLabel *labelUpdate;
    QWidget *widgetBatch;
    QLabel *labelBatchTitle;
    QToolButton *btnCloseBatch;
    QTableWidget *listResults;
    QSplitter *splitterBatch;
    QLabel *imgThrobber;
    QMovie *throbberAnim;
    QString editorFontName;
    int editorFontSize;

private slots:
    void btnGoClick();
    void btnAgentEditClick();
    void btnConfigClick();
    void btnAboutClick();
    void btnBatchClick();
    void btnRefreshClick();
    void closeTab(const int& index);
    void clipboardChanged(QClipboard::Mode mode);
    void trayIconActivated(QSystemTrayIcon::ActivationReason reason);
    void menuRestoreClick();
    void menuExitClick();
    void checkUpdates();
    void btnCloseBatchClick();    
    void listResultsCellDoubleClicked(int row, int column);
    void on_tabWidget_currentChanged(int index);

private:
    Ui::MainWindow *ui;
    QDir appDir;
    QClipboard *board;
    QString lastClipboardURL;
    QStringList ClipboardURLs;
    QTcpSocket *socket;
    void LoadUserAgents();
    void SaveUserAgents();
    void AddDefaultUserAgents();
    void SaveHistory();
    void LoadHistory();
    void LoadConfig();
    void SaveConfig();
    void ApplyConfig();
    void ResetTable();
    void ShowBatchTable(bool m);
    void ShowThrobber(bool m);
    void changeEvent(QEvent *e);    
    int FindTab(QString url);
    httpResponse CheckURL(QString url, QString useragent, bool usePOST, QList<TPostVariable> postvars, bool IsBatch);
};

#endif // MAINWINDOW_H
