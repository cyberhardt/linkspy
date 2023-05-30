#ifndef POSTWINDOW_H
#define POSTWINDOW_H

#include <QDialog>
#include <QFileDialog>
#include <QDebug>
#include "httpfunc.h"

namespace Ui {
class PostWindow;
}

class PostWindow : public QDialog
{
    Q_OBJECT

public:
    explicit PostWindow(QWidget *parent = 0);
    ~PostWindow();
    QList<TPostVariable> postvars;

private slots:
    void on_tableWidget_cellChanged(int row, int column);
    void on_btnOK_clicked();
    void on_btnCancel_clicked();
    void on_btnLoad_clicked();
    void on_btnSave_clicked();

private:
    Ui::PostWindow *ui;
    void SetVars();
    QString base64_decode(QString string);
    QString base64_encode(QString string);
};

#endif // POSTWINDOW_H
