#ifndef OPTIONS_H
#define OPTIONS_H

#include <QDialog>
#include <QColorDialog>

struct TColourConfig {
    QString FontName;
    int FontSize;
    QColor Comment;
    QColor Tag;
    QColor Attribute;
    QColor Entity;
    QColor AttVal;
    QColor Doctype;
    QColor StandardText;
    bool CommentBold;
    bool TagBold;
    bool AttributeBold;
    bool EntityBold;
    bool AttValBold;
    bool DoctypeBold;
    bool StandardTextBold;
    bool CommentItalic;
    bool TagItalic;
    bool AttributeItalic;
    bool EntityItalic;
    bool AttValItalic;
    bool DoctypeItalic;
    bool StandardTextItalic;
};

namespace Ui {
class OptionsForm;
}

class OptionsForm : public QDialog {
    Q_OBJECT

public:
    explicit OptionsForm(QWidget *parent = 0);
    ~OptionsForm();
    bool ClearHistory;
    TColourConfig DisplayConfig;
    void SetOptions(QString fontname, int fontsize);

private slots:
    void on_btnClearHistory_clicked();
    void on_btnOK_clicked();
    void on_btnCancel_clicked();

private:
    Ui::OptionsForm *ui;
};

#endif // OPTIONS_H
