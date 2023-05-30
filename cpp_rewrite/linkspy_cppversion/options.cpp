#include "options.h"
#include "ui_options.h"

OptionsForm::OptionsForm(QWidget *parent) : QDialog(parent), ui(new Ui::OptionsForm) {
    ui->setupUi(this);
    ClearHistory = false;
}

OptionsForm::~OptionsForm() {
    delete ui;
}

void OptionsForm::SetOptions(QString fontname, int fontsize) {
    int i;
    for(i=0;i<ui->fontComboBox->count();i++) {
        if(ui->fontComboBox->itemText(i) == fontname) ui->fontComboBox->setCurrentIndex(i);
    }
    ui->textFontSize->setValue(fontsize);
}

void OptionsForm::on_btnClearHistory_clicked() {
    ClearHistory = true;
}

void OptionsForm::on_btnOK_clicked() {
    DisplayConfig.FontName = ui->fontComboBox->currentText();
    DisplayConfig.FontSize = ui->textFontSize->value();
    this->accept();
}

void OptionsForm::on_btnCancel_clicked() {
    this->reject();
}
