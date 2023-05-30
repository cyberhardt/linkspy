#ifndef USERAGENTEDIT_H
#define USERAGENTEDIT_H

#include <QDialog>

namespace Ui {
class UserAgentEdit;
}

class UserAgentEdit : public QDialog
{
    Q_OBJECT

public:
    explicit UserAgentEdit(QWidget *parent = 0);
    ~UserAgentEdit();
    QString Title;
    QString Useragent;
    void SetUserAgent(QString t, QString u);

private slots:
    void on_buttonBox_accepted();

private:
    Ui::UserAgentEdit *ui;
};

#endif // USERAGENTEDIT_H
