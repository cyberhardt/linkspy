#include "useragentedit.h"
#include "ui_useragentedit.h"

UserAgentEdit::UserAgentEdit(QWidget *parent) : QDialog(parent), ui(new Ui::UserAgentEdit) {
    ui->setupUi(this);
}

UserAgentEdit::~UserAgentEdit() {
    delete ui;
}

void UserAgentEdit::on_buttonBox_accepted() {
    Title = ui->textTitle->text();
    Useragent = ui->textUseragent->text();
}

void UserAgentEdit::SetUserAgent(QString t, QString u) {
    ui->textTitle->setText(t);
    ui->textUseragent->setText(u);
}
