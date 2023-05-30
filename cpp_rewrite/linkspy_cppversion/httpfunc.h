#ifndef HTTPFUNC_H
#define HTTPFUNC_H

#include <QString>
#include <QList>
#include <QtNetwork/QNetworkRequest>
#include <QtNetwork/QNetworkReply>
#include <QUrl>
#include <QEventLoop>
#if QT_VERSION > QT_VERSION_CHECK(5, 0, 0)
#include <QUrlQuery>
#endif
#include <QtGlobal>
#include "appvars.h"

struct TPostVariable {
    QString key;
    QString value;
};

struct httpResponse {
    QString URL;
    QString header;
    QString content;
    int rescode;
    long ressize;
};

Q_DECLARE_METATYPE(httpResponse)

class HTTPObj : public QObject{
  Q_OBJECT
public:
    httpResponse httpGet(QUrl url, QString useragent);
    httpResponse httpPost(QString url, QString useragent, QList<TPostVariable> postvars);
};

#endif // HTTPFUNC_H
