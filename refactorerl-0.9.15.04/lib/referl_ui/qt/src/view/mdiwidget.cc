// -*- coding: latin-1 -*-

// This file is part of RefactorErl.
//
// RefactorErl is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// RefactorErl is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
//
// The Original Code is RefactorErl.
//
// The Initial Developer of the Original Code is Eötvös Loránd University.
// Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
// are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
// and Ericsson Hungary. All Rights Reserved.

//Author: Mátyás Kuti

#include <QCoreApplication>
#include <QMessageBox>
#include <QInputDialog>
#include "mdiwidget.h"
#include "codebrowser/codebrowserwidget.h"

MdiWidget::MdiWidget(QWidget *parent) :
    QMdiArea(parent)
{
    qRegisterMetaType<Qt::Orientation>("Qt::Orientation");

    ref_erl_model_ = new RefErlModel();
    model_thread_ = new QThread();
    ref_erl_model_->moveToThread(model_thread_);

    connect( ref_erl_model_, SIGNAL( CatFileSignal(QString, QString) ),
        this, SLOT( ShowCodeBrowser(QString, QString) ) );

    connect( ref_erl_model_, SIGNAL( ErrorMessageSignal(QString) ),
        this, SLOT( ShowErrorMessage(QString) ) );

    connect( ref_erl_model_, SIGNAL( QueryFinishedSignal() ),
        this, SLOT( ShowQueryResult() ) );

    connect( ref_erl_model_, SIGNAL( DupcodeSelectedSearchSignal(QString,QString,int,int) ),
        this, SLOT( StartSelectedDupcode(QString,QString,int,int) ) );

    //Try to connect to the Erlang node
    if( !ref_erl_model_->StartConnection(
        QCoreApplication::arguments().at(1).toStdString(),
        QCoreApplication::arguments().at(2).toStdString() )
    ) {
        QMessageBox::warning( this, "Connection error",
            "Could not connect to RefErl node" );
        delete ref_erl_model_;
        exit(1);
    }

    model_thread_->start();
    ref_erl_model_->EmitStartReceiving(); //Start the constant message receiving

    ShowFileBrowser();
}

MdiWidget::~MdiWidget()
{
    model_thread_->quit();
    model_thread_->wait();
    delete model_thread_;
    delete ref_erl_model_;
}

const QString MdiWidget::GetUserName() const
{
    return ref_erl_model_->GetUserName()
            + QString( ref_erl_model_->IsClient() ? " as client" : "" );
}

void MdiWidget::ShowFileBrowser()
{
    if(file_browser_widget_ != NULL) return;
    file_browser_widget_ = addSubWindow( new FileBrowserWidget(this, ref_erl_model_) );
    file_browser_widget_->setAttribute(Qt::WA_DeleteOnClose);
    connect( file_browser_widget_->widget(), SIGNAL( ShowFileNeeded() ),
        this, SLOT( ShowFileNeeded() ) );
    connect( file_browser_widget_->widget(), SIGNAL( destroyed() ),
        this, SLOT( FileBrowserDestroyed() ));
    file_browser_widget_->show();
    if(viewMode() == QMdiArea::SubWindowView) {
        tileSubWindows();
    }
}

void MdiWidget::FileBrowserDestroyed()
{
    file_browser_widget_ = NULL;
}

void MdiWidget::ShowDependencyGraph()
{
    if(dependency_graph_widget_ != NULL) return;
    dependency_graph_widget_ = addSubWindow( new DependencyGraphWidget(this, ref_erl_model_) );
    dependency_graph_widget_->setAttribute(Qt::WA_DeleteOnClose);
    connect( dependency_graph_widget_->widget(), SIGNAL( destroyed() ),
        this, SLOT( DependencyGraphDestroyed() ) );
    dependency_graph_widget_->show();
    if(viewMode() == QMdiArea::SubWindowView) {
        tileSubWindows();
    }
}

void MdiWidget::DependencyGraphDestroyed()
{
    dependency_graph_widget_ = NULL;
}

void MdiWidget::ShowQueries()
{
    if(queries_widget_ != NULL) return;
    queries_widget_ = addSubWindow( new QueriesWidget(this, ref_erl_model_) );
    queries_widget_->setAttribute(Qt::WA_DeleteOnClose);
    connect( queries_widget_->widget(), SIGNAL( destroyed() ),
        this, SLOT( QueriesDestroyed() ));
    queries_widget_->show();
    if(viewMode() == QMdiArea::SubWindowView) {
        tileSubWindows();
    }
}

void MdiWidget::QueriesDestroyed()
{
    queries_widget_ = NULL;
}

void MdiWidget::ShowDuplicatedCode()
{
    if(duplicated_code_widget_ != NULL) return;
    duplicated_code_widget_ = addSubWindow( new DuplicatedCodeWidget(this, ref_erl_model_) );
    duplicated_code_widget_->setAttribute(Qt::WA_DeleteOnClose);
    connect( duplicated_code_widget_->widget(), SIGNAL( destroyed() ),
        this, SLOT( DuplicatedCodeDestroyed() ));
    duplicated_code_widget_->show();
    if(viewMode() == QMdiArea::SubWindowView) {
        tileSubWindows();
    }
}

void MdiWidget::DuplicatedCodeDestroyed()
{
    duplicated_code_widget_ = NULL;
}

void MdiWidget::ShowInvestigations()
{
    if(investigations_widget_ != NULL) return;
    investigations_widget_ = addSubWindow( new InvestigationsWidget(this, ref_erl_model_) );
    investigations_widget_->setAttribute(Qt::WA_DeleteOnClose);
    connect( investigations_widget_->widget(), SIGNAL( destroyed() ),
        this, SLOT( InvestigationsDestroyed() ) );
    investigations_widget_->show();
    if(viewMode() == QMdiArea::SubWindowView) {
        tileSubWindows();
    }
}

void MdiWidget::InvestigationsDestroyed()
{
    investigations_widget_ = NULL;
}

void MdiWidget::ShowCodeBrowser(const QString &path, const QString &content)
{
    if(!open_file_) {
        return;
    }
    CodeBrowserWidget *code_browser_widget =
        new CodeBrowserWidget(this, ref_erl_model_, path, content);

    QList<QStandardItem*> items =
        ref_erl_model_->ErrorModel()->findItems(path, 0, 1);

    for(const QStandardItem *item : items) {
        int row =  ref_erl_model_->ErrorModel()->indexFromItem(item).row();
        QModelIndex start_index = ref_erl_model_->ErrorModel()->index(row, 3);
        QModelIndex end_index = ref_erl_model_->ErrorModel()->index(row, 4);
        int start =
            ref_erl_model_->ErrorModel()->QStandardItemModel::data(start_index).toInt();
        int end =
            ref_erl_model_->ErrorModel()->QStandardItemModel::data(end_index).toInt();
        code_browser_widget->HighLightError(start, end);
    }

    QMdiSubWindow *code_browser =
        addSubWindow( code_browser_widget );
    code_browser->setAttribute(Qt::WA_DeleteOnClose);
    code_browser->show();
    open_file_ = false;
    if(viewMode() == QMdiArea::SubWindowView) {
        tileSubWindows();
    }
}

void MdiWidget::ShowErrorMessage(const QString &msg)
{
    QMessageBox::critical(this, "Error", msg);
}

void MdiWidget::ShowFileNeeded()
{
    open_file_ = true;
}

void MdiWidget::ShowQueryResult()
{
    ShowQueries();
    QueriesWidget *qw = dynamic_cast<QueriesWidget*>(queries_widget_->widget());
    qw->setFocus();
    qw->SwitchToResult();
}

void MdiWidget::StartSelectedDupcode(const QString &algorithm_key,
                                     const QString &file_path,
                                     const int &start,
                                     const int &end)
{
    ShowDuplicatedCode();
    DuplicatedCodeWidget *dc = dynamic_cast<DuplicatedCodeWidget*>(duplicated_code_widget_->widget());
    dc->DupcodeSelectedSearch(algorithm_key, file_path, start, end);
}

void MdiWidget::ActionFileTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->AddFileButtonClicked();
}

void MdiWidget::ActionAppbaseTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->AddAppBaseButtonClicked();
}

void MdiWidget::ActionIncludeTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->AddIncludeButtonClicked();
}

void MdiWidget::ActionDirectoryTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->AddDirButtonClicked();
}

void MdiWidget::ActionSyncTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->SycDatabaseButtonClicked();
}

void MdiWidget::ActionDropTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->DropFileButtonClicked();
}

void MdiWidget::ActionResetDatabaseTriggered()
{
    ShowFileBrowser();
    FileBrowserWidget *fb = dynamic_cast<FileBrowserWidget*>(file_browser_widget_->widget());
    fb->setFocus();
    fb->ResetDatabaseButtonClicked();
}

void MdiWidget::ActionQueryToRunTriggered()
{
    QString query = QInputDialog::getText(this, "Run query", "Query");
    if(query.isEmpty()) return;
    ref_erl_model_->RunQuery(query);
    ShowQueries();
    QueriesWidget *qw = dynamic_cast<QueriesWidget*>(queries_widget_->widget());
    qw->setFocus();
}

void MdiWidget::ActionSkeletonTriggered()
{
    ShowQueries();
    QueriesWidget *qw = dynamic_cast<QueriesWidget*>(queries_widget_->widget());
    qw->SaveSkeletonButtonClicked();
}

void MdiWidget::ActionLastResultsTriggered()
{
    ShowQueries();
    QueriesWidget *qw = dynamic_cast<QueriesWidget*>(queries_widget_->widget());
    qw->setFocus();
    qw->SwitchToResult();
}

void MdiWidget::ActionRunningQueriesTriggered()
{
    ShowQueries();
    QueriesWidget *qw = dynamic_cast<QueriesWidget*>(queries_widget_->widget());
    qw->setFocus();
    qw->SwitchToRunning();
}

void MdiWidget::ActionModuleTriggered(const DependencyType &type)
{
    ShowDependencyGraph();
    DependencyGraphWidget *dgw = dynamic_cast<DependencyGraphWidget*>(dependency_graph_widget_->widget());
    dgw->setFocus();
    ref_erl_model_->DrawGraph(QString(), Module, type);
}

void ::MdiWidget::ActionFunctionTriggered(const DependencyType &type)
{
    ShowDependencyGraph();
    DependencyGraphWidget *dgw = dynamic_cast<DependencyGraphWidget*>(dependency_graph_widget_->widget());
    dgw->setFocus();
    ref_erl_model_->DrawGraph(QString(), Function, type);
}

void MdiWidget::ActionModuleGroupTriggered(const DependencyType &type)
{
    ShowDependencyGraph();
    DependencyGraphWidget *dgw = dynamic_cast<DependencyGraphWidget*>(dependency_graph_widget_->widget());
    dgw->setFocus();
    ref_erl_model_->DrawGraph(QString(), ModuleGroup, type);
}
