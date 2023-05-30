#include "postwindow.h"
#include "ui_postwindow.h"

PostWindow::PostWindow(QWidget *parent) : QDialog(parent), ui(new Ui::PostWindow) {
    ui->setupUi(this);
    QStringList headers;
    headers << "Key" << "Value";
    ui->tableWidget->setColumnCount(2);
    ui->tableWidget->setRowCount(1);
    ui->tableWidget->setHorizontalHeaderLabels(headers);
    int w;
    w = ui->tableWidget->width() - 2;
    ui->tableWidget->setColumnWidth(0,w/2);
    ui->tableWidget->horizontalHeader()->setVisible(true);
    ui->tableWidget->horizontalHeader()->setStretchLastSection(true);
    postvars.clear();
}

PostWindow::~PostWindow() {
    delete ui;
}

QString PostWindow::base64_encode(QString string) {
    QByteArray ba;
    ba.append(string);
    return ba.toBase64();
}

QString PostWindow::base64_decode(QString string) {
    QByteArray ba;
    ba.append(string);
    return ba.fromBase64(ba);
}

void PostWindow::on_tableWidget_cellChanged(int row, int column) {
    if(row == (ui->tableWidget->rowCount()-1)) {
        ui->tableWidget->setRowCount(ui->tableWidget->rowCount()+1);
    }
}

void PostWindow::on_btnOK_clicked() {
    TPostVariable pv;
    int row;
    QTableWidgetItem* item;
    for(row = 0; row < ui->tableWidget->rowCount()-1; row++) {
        pv.key.clear();
        pv.value.clear();
        item = ui->tableWidget->item(row,0);
        if(item && !item->text().isEmpty()) {
            pv.key = item->text();
        }
        item = ui->tableWidget->item(row,1);
        if(item && !item->text().isEmpty()) {
            pv.value = item->text();
        }
        postvars.append(pv);
    }
    this->accept();
}

void PostWindow::on_btnCancel_clicked() {
    this->reject();
}

void PostWindow::on_btnLoad_clicked() {
    QString filename = QFileDialog::getOpenFileName(this, "Load file", "", "*.pv");
    if(filename.isEmpty()) return;
    int row;
    QFile f(filename);
    QStringList contents;
    if(f.open(QIODevice::ReadOnly)) {
        QTextStream instream(&f);
        while(!instream.atEnd()) {
            contents.append(instream.readLine());
        }
        f.close();
    }
    if(contents.count() < 1) return;
    ui->tableWidget->setRowCount(contents.count()+1);
    QStringList parts;
    for(row=0;row < contents.count();row++) {
        QTableWidgetItem* key = new QTableWidgetItem;
        QTableWidgetItem* value = new QTableWidgetItem;
        parts = contents.at(row).split("|");
        key->setText(base64_decode(parts.at(0)));
        ui->tableWidget->setItem(row,0,key);
        value->setText(base64_decode(parts.at(1)));
        ui->tableWidget->setItem(row,1,value);
    }
}

void PostWindow::on_btnSave_clicked() {
    QString filename = QFileDialog::getSaveFileName(this, "Save file", "", "*.pv");
    if(filename.isEmpty()) return;
    TPostVariable pv;
    QList<TPostVariable> tmpVars;
    int row;
    QTableWidgetItem* item;
    for(row = 0; row < ui->tableWidget->rowCount()-1; row++) {
        pv.key.clear();
        pv.value.clear();
        item = ui->tableWidget->item(row,0);
        if(item && !item->text().isEmpty()) {
            pv.key = item->text();
        }
        item = ui->tableWidget->item(row,1);
        if(item && !item->text().isEmpty()) {
            pv.value = item->text();
        }
        tmpVars.append(pv);
    }
    if(!filename.endsWith(".pv")) filename.append(".pv");
    QFile f(filename);
    if(f.open(QIODevice::WriteOnly)) {
        QTextStream outstream(&f);
        for(row = 0; row < tmpVars.count();row++) {
            outstream << base64_encode(tmpVars.at(row).key) << "|" << base64_encode(tmpVars.at(row).value) << "\n";
        }
        f.close();
    }
}
