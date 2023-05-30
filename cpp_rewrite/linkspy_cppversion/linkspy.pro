#-------------------------------------------------
#
# Project created by QtCreator 2016-08-09T11:21:28
#
#-------------------------------------------------

QT       += core gui network sql

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = linkspy
TEMPLATE = app

HEADERS = mainwindow.h \
    xmlhighlighter.h \
    useragentwindow.h \
    useragentedit.h \
    postwindow.h \
    options.h \
    appvars.h \
    httpfunc.h \
    resulttab.h \
    batchlist.h

SOURCES = xmlhighlighter.cpp \
    mainwindow.cpp \
    main.cpp \
    useragentwindow.cpp \
    useragentedit.cpp \
    postwindow.cpp \
    options.cpp \
    httpfunc.cpp \
    resulttab.cpp \
    batchlist.cpp

FORMS    += mainwindow.ui \
    useragentwindow.ui \
    useragentedit.ui \
    postwindow.ui \
    options.ui \
    batchlist.ui

RESOURCES += \
    appres.qrc
    
RC_FILE = linkspy.rc

unix:CONFIG += link_pkgconfig

unix:PKGCONFIG += libxml-2.0

win32:CONFIG(release, debug|release): LIBS += -L$$PWD/libxml-build/libiconv-win-build/build-VS2013/release/ -llibiconv
else:win32:CONFIG(debug, debug|release): LIBS += -L$$PWD/libxml-build/libiconv-win-build/build-VS2013/debug/ -llibiconv

INCLUDEPATH += $$PWD/libxml-build/libiconv-win-build/include

win32:CONFIG(release, debug|release): LIBS += -L$$PWD/libxml-build/libxml2-win-build/build-VS2013/release/ -llibxml2
else:win32:CONFIG(debug, debug|release): LIBS += -L$$PWD/libxml-build/libxml2-win-build/build-VS2013/debug/ -llibxml2

INCLUDEPATH += $$PWD/libxml-build/libxml2-win-build/include
