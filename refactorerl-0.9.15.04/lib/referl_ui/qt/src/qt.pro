EI_PATH = $(ERL_INTERFACE)

PROJECT_FILE_DIR = $$PWD

QT       += core gui svg

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets svg

TARGET = RefErlQt
TEMPLATE = app

DEPENDPATH += . \
                src \

INCLUDEPATH += $$EI_PATH/include

SOURCES +=\
        view/mainwindow.cc \
    main.cc \
    model/referlconnect.cc \
    view/dependencygraphwidget.cc \
    view/duplicatedcodewidget.cc \
    view/filebrowserwidget.cc \
    view/investigationswidget.cc \
    view/querieswidget.cc \
    model/referlreceive.cc \
    view/directoryprogressdialog.cc \
    view/addfileprogressdialog.cc \
    view/dropfileprogressdialog.cc \
    view/codebrowser/codebrowser.cc \
    view/codebrowser/linenumberarea.cc \
    view/codebrowser/erlsyntaxhighlighter.cc \
    model/errorstandarditemmodel.cc \
    view/mdiwidget.cc \
    model/skeletonstandarditemmodel.cc \
    model/queryresult.cc \
    view/codebrowser/codebrowserwidget.cc \
    model/querystandarditemmodel.cc \
    view/skeletondialog.cc \
    model/dependencygraph/node.cc \
    model/dependencygraph/dotparser.cc \
    model/dependencygraph/edge.cc \
    model/dependencygraph/dependencygraph.cc \
    view/dependencygraph/nodeitem.cc \
    view/dependencygraph/edgeitem.cc \
    view/dependencygraph/graphwidget.cc \
    model/referlmodel.cc \
    model/investigations/investigationnode.cc \
    model/investigations/investigationgraph.cc \
    view/investigations/investigationitem.cc \
    view/investigations/investigationsgraphwidget.cc \
    view/investigations/investigationedgeitem.cc \
    view/investigations/investigationproxyitem.cc \
    model/parameter.cc \
    view/filelistwidget.cc \
    view/duplicatedcodealgorithmwidget.cc

HEADERS  += view/mainwindow.h \
    model/referlconnect.h \
    view/filebrowserwidget.h \
    view/dependencygraphwidget.h \
    view/duplicatedcodewidget.h \
    view/querieswidget.h \
    view/investigationswidget.h \
    model/referlreceive.h \
    model/common.h \
    view/mdiwidget.h \
    view/directoryprogressdialog.h \
    view/addfileprogressdialog.h \
    view/dropfileprogressdialog.h \
    view/codebrowser/codebrowser.h \
    view/codebrowser/linenumberarea.h \
    view/codebrowser/erlsyntaxhighlighter.h \
    model/errorstandarditemmodel.h \
    model/skeletonstandarditemmodel.h \
    model/queryresult.h \
    model/querystandarditemmodel.h \
    view/codebrowser/codebrowserwidget.h \
    view/skeletondialog.h \
    model/dependencygraph/node.h \
    model/dependencygraph/dotparser.h \
    model/dependencygraph/edge.h \
    model/dependencygraph/dependencygraph.h \
    view/dependencygraph/nodeitem.h \
    view/dependencygraph/edgeitem.h \
    view/dependencygraph/graphwidget.h \
    model/referlmodel.h \
    model/investigations/investigationnode.h \
    model/investigations/investigationgraph.h \
    view/investigations/investigationitem.h \
    view/investigations/investigationsgraphwidget.h \
    view/investigations/investigationedgeitem.h \
    view/investigations/investigationproxyitem.h \
    model/parameter.h \
    view/filelistwidget.h \
    view/duplicatedcodealgorithmwidget.h

FORMS    += view/mainwindow.ui \
    view/filebrowserwidget.ui \
    view/dependencygraphwidget.ui \
    view/duplicatedcodewidget.ui \
    view/querieswidget.ui \
    view/investigationswidget.ui \
    view/directoryprogressdialog.ui \
    view/addfileprogressdialog.ui \
    view/dropfileprogressdialog.ui \
    view/codebrowser/codebrowserwidget.ui \
    view/skeletondialog.ui \
    view/investigations/investigationitem.ui \
    view/filelistwidget.ui \
    view/duplicatedcodealgorithmwidget.ui

LIBS += -L$$EI_PATH/lib \
        -lerl_interface \
        -lei \
        -lpthread

MOC_DIR = $$PROJECT_FILE_DIR/../temp
OBJECTS_DIR = $$PROJECT_FILE_DIR/../temp
CONFIG(debug, debug|release):DESTDIR = $$PROJECT_FILE_DIR/../bin/debug
CONFIG(release, debug|release):DESTDIR = $$PROJECT_FILE_DIR/../bin

QMAKE_CXXFLAGS += -std=c++11 -O2
