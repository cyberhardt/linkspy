#include "httpfunc.h"

httpResponse HTTPObj::httpGet(QUrl url, QString useragent) {
    httpResponse response;
    response.content = "";
    response.header = "";
    response.rescode = 0;

    QNetworkAccessManager nam;
    QNetworkRequest req(url);
    QByteArray ua = useragent.toUtf8();
    req.setRawHeader("User-Agent", ua);
    QEventLoop loop;
    QNetworkReply* reply = nam.get(req);

    QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    loop.exec();
    QByteArray bytes = reply->readAll();
    response.content = QString::fromUtf8(bytes);
    response.URL = url.toString();
    response.rescode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
    response.ressize = response.content.length();
    QList<QByteArray> headerList = reply->rawHeaderList();
    foreach(QByteArray head, headerList) {
        response.header += head + ": " + reply->rawHeader(head) + "\n";
    }
    return response;
}

httpResponse HTTPObj::httpPost(QString url, QString useragent, QList<TPostVariable> postvars) {
    httpResponse response;
    response.content = "";
    response.header = "";
    response.rescode = 0;

    QNetworkAccessManager nam;
    QNetworkRequest req(url);
    QByteArray ua = useragent.toUtf8();
    req.setRawHeader("User-Agent", ua);
    req.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");
    QUrl postData;
#if QT_VERSION < QT_VERSION_CHECK(5, 0, 0)
    int i;
    for(i = 0; i < postvars.count(); i++) {
        postData.addQueryItem(postvars.at(i).key,postvars.at(i).value);
    }
#else
    QUrlQuery query;
    int i;
    for(i = 0; i < postvars.count(); i++) {
        query.addQueryItem(postvars.at(i).key,postvars.at(i).value);
    }
    postData.setQuery(query);
#endif
    QEventLoop loop;
#if QT_VERSION < QT_VERSION_CHECK(5, 0, 0)
    QNetworkReply* reply = nam.post(req,postData.encodedQuery());
#else
    QNetworkReply* reply = nam.post(req,query.query().toUtf8());
#endif
    QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    loop.exec();
    QByteArray bytes = reply->readAll();
    response.URL = url;
    response.content = QString::fromUtf8(bytes);
    response.rescode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
    QList<QByteArray> headerList = reply->rawHeaderList();
    foreach(QByteArray head, headerList) {
        response.header += head + ": " + reply->rawHeader(head) + "\n";
    }
    return response;
}
