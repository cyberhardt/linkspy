#ifndef BATCHLIST_H
#define BATCHLIST_H

#include <QDialog>

namespace Ui {
class BatchList;
}

class BatchList : public QDialog
{
    Q_OBJECT

public:
    explicit BatchList(QWidget *parent = 0);
    ~BatchList();
    void SetEditorFont(QFont f);
    void ClearEditor();
    QString GetEditorText();

private:
    Ui::BatchList *ui;
};

#endif // BATCHLIST_H
