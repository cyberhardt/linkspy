#include "batchlist.h"
#include "ui_batchlist.h"

BatchList::BatchList(QWidget *parent) : QDialog(parent), ui(new Ui::BatchList) {
    ui->setupUi(this);
}

BatchList::~BatchList() {
    delete ui;
}

void BatchList::SetEditorFont(QFont f) {
    ui->plainTextEdit->setFont(f);
}

void BatchList::ClearEditor() {
    ui->plainTextEdit->clear();
}

QString BatchList::GetEditorText() {
    return ui->plainTextEdit->toPlainText();
}
