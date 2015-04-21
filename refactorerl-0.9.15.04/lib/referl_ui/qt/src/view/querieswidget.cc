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

#include "querieswidget.h"
#include "ui_querieswidget.h"
#include <model/common.h>
#include "skeletondialog.h"
#include <QMessageBox>
#include <QInputDialog>
#include <QSizePolicy>

QueriesWidget::QueriesWidget(QWidget *parent, RefErlModel *ref_erl_model) :
    QWidget(parent),
    ui(new Ui::QueriesWidget),
    ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);
    splitter_ = new QSplitter(this);
    ui->fileTableView->setModel( ref_erl_model_->FileModel() );
    ui->skeletonsTableView->setModel( ref_erl_model_->SkeletonModel() );
    ui->queriesTableView->setModel( ref_erl_model_->QueriesModel() );
    ui->queryResultTreeView->setModel( ref_erl_model_->QueryResultModel() );
    ui->runningQueriesTableView->setModel( ref_erl_model_->RunningQueriesModel() );

    code_browser_widget_ = new CodeBrowserWidget(this, ref_erl_model_);

    ui->mainLayout->removeWidget(ui->tabWidget);
    splitter_->addWidget(ui->tabWidget);
    splitter_->addWidget(code_browser_widget_);
    ui->mainLayout->addWidget(splitter_);
    code_browser_widget_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    completer_model_ = new QStringListModel(this);
    completer_ = new QCompleter(this);
    completer_->setModel(completer_model_);
    completer_->setWidget(ui->queryEdit);

    connect( ref_erl_model_, SIGNAL( QueryFinishedSignal() ),
        this, SLOT( SwitchToResult() ) );
    connect( ui->saveSkeletonButton, SIGNAL( clicked() ),
        this, SLOT( SaveSkeletonButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( SaveSkeletonSignal(QString) ),
        this, SLOT( SaveSkeleton(QString) ) );
    connect( ref_erl_model, SIGNAL( DeleteSkeletonSignal(QString) ),
        this, SLOT( DeleteSkeleton(QString) ) );
    connect( ui->runButton, SIGNAL( clicked() ),
        this, SLOT( RunQueryButtonClicked() ) );
    connect( ui->deleteButton, SIGNAL( clicked() ),
        this, SLOT( DeleteQueryButtonClicked() ) );
    connect( ui->skeletonsTableView, SIGNAL(clicked(QModelIndex) ),
        this, SLOT( ChangeDeleteSkeletonButtonState(QModelIndex) ) );
    connect( ui->modifySkeletonButton, SIGNAL( clicked() ),
        this,  SLOT( ModifySkeletonButtonClicked() ) );
    connect( ui->deleteSkeletonButton, SIGNAL( clicked() ),
        this, SLOT( DeleteSkeletonButtonClicked() ) );
    connect( ui->queriesTableView, SIGNAL( doubleClicked(QModelIndex) ),
        this, SLOT( QueryDoubleClicked(QModelIndex) ) );
    connect( ui->queryResultTreeView, SIGNAL( doubleClicked(QModelIndex) ),
        this, SLOT( QueryResultDoubleClicked(QModelIndex) ) );
    connect( ref_erl_model_, SIGNAL( CatFileSignal(QString,QString) ),
        this, SLOT( ShowFile(QString,QString) ) );
    connect( ui->skeletonsTableView, SIGNAL( doubleClicked(QModelIndex) ),
        this, SLOT( SkeletonDoubleClicked(QModelIndex) ) );
    connect( ref_erl_model_, SIGNAL( SkeletonCallFormatSignal(QString) ),
        this, SLOT( SkeletonCallFormat(QString) ) );
    connect( ui->killButton, SIGNAL( clicked() ),
        this, SLOT( KillQueryButtonClicked() ) );
    connect( ui->fileTableView, SIGNAL( doubleClicked(QModelIndex) ),
        this, SLOT( FileDoubleClicked(QModelIndex) ) );
    connect( ref_erl_model_->RunningQueriesModel(), SIGNAL( dataChanged(QModelIndex, QModelIndex) ),
         this, SLOT( AdjustTableSize(QModelIndex,QModelIndex) ) );
    connect( ref_erl_model_->SkeletonModel(), SIGNAL( dataChanged(QModelIndex, QModelIndex) ),
         this, SLOT( AdjustTableSize(QModelIndex,QModelIndex) ) );
    connect( ref_erl_model_->QueriesModel(), SIGNAL( dataChanged(QModelIndex, QModelIndex) ),
         this, SLOT( AdjustTableSize(QModelIndex,QModelIndex) ) );
    connect( ref_erl_model_->FileModel(), SIGNAL( dataChanged(QModelIndex, QModelIndex) ),
         this, SLOT( AdjustTableSize(QModelIndex,QModelIndex) ) );

    //Autocomplete
    connect( ref_erl_model_, SIGNAL( AutoCompleteSignal(QStringList, QStringList) ),
        this, SLOT( AutoComplete(QStringList, QStringList) ) );
    connect( ui->queryEdit, SIGNAL( textEdited(QString) ),
        this, SLOT( QueryEditTextEdited(QString) ) );
    connect( completer_, SIGNAL( activated(QString) ), this, SLOT( SetCompletion(QString) ) );

    ref_erl_model_->GetSkeletons();
    ref_erl_model_->GetQueries();
    ref_erl_model_->GetRunningQueries();
    AdjustTableSize(QModelIndex(), QModelIndex());
}

QueriesWidget::~QueriesWidget()
{
    delete ui;
}

void QueriesWidget::SwitchToResult()
{
    ui->tabWidget->setCurrentIndex(1);
    ui->queryResultTreeView->expandAll();
}

void QueriesWidget::SwitchToRunning()
{
    ui->tabWidget->setCurrentIndex(2);
}

void QueriesWidget::SaveSkeletonButtonClicked()
{
    SkeletonDialog dialog;
    dialog.SetSkeleton(ui->queryEdit->text());
    dialog.exec();
    if(dialog.Accepted()) {
        ref_erl_model_->SaveSkeleton(
            dialog.GetName(),
            dialog.GetSkeleton(),
            dialog.GetComment()
       );
    }
    ref_erl_model_->GetSkeletons();
}

void QueriesWidget::SaveSkeleton(const QString &name)
{
    QMessageBox::information(this, "Skeleton saved",
        QString("%1 was saved").arg(name)
    );
}

void QueriesWidget::DeleteSkeleton(const QString &name)
{
    QMessageBox::information(this, "Skeleton deleted",
        QString("%1 was deleted").arg(name)
    );
}

void QueriesWidget::ModifySkeleton(const QString &name)
{
    QMessageBox::information(this, "Skeleton modified",
        QString("%1 was modified").arg(name)
    );
}

void QueriesWidget::RunQueryButtonClicked()
{
    QString query = ui->queryEdit->text();
    if(query.isEmpty()) {
        QMessageBox::warning(this, "Warning", "There is no query given!");
    } else {
        ref_erl_model_->RunQuery(query);
    }
    ref_erl_model_->GetQueries();
    ref_erl_model_->GetRunningQueries();
}

void QueriesWidget::DeleteQueryButtonClicked()
{
    QModelIndex current_index =  ui->queriesTableView->currentIndex();
    QString query_str, file;
    int position;
    bool ok;
    if( !current_index.isValid() ) {
        return;
    } else {
        int row = current_index.row();
        QModelIndex query_index = ref_erl_model_->QueriesModel()->index(row, 0);
        QModelIndex file_index = ref_erl_model_->QueriesModel()->index(row, 1);
        QModelIndex position_index = ref_erl_model_->QueriesModel()->index(row, 2);
        query_str = ref_erl_model_->QueriesModel()->data(query_index).toString();
        if( QMessageBox::question(this, "Delete query", "Are you sure to delete selected query?",
            QMessageBox::Yes|QMessageBox::No) == QMessageBox::No ) {
            return;
        }
        file = ref_erl_model_->QueriesModel()->data(file_index).toString();
        position = ref_erl_model_->QueriesModel()->data(position_index).toString().toInt(&ok);
        if(file == "" && !ok) {
            ref_erl_model_->DeleteQuery(query_str);
        } else {
            ref_erl_model_->DeleteQuery(query_str, file, position);
        }
    }
    ref_erl_model_->GetQueries();
}

void QueriesWidget::ModifySkeletonButtonClicked()
{
    QModelIndex current_index =  ui->skeletonsTableView->currentIndex();
    int row = current_index.row();

    QModelIndex name_index = ref_erl_model_->SkeletonModel()->index(row, 0);
    QString name = ref_erl_model_->SkeletonModel()->data(name_index).toString();
    QModelIndex body_index = ref_erl_model_->SkeletonModel()->index(row, 1);
    QString body = ref_erl_model_->SkeletonModel()->QStandardItemModel::data(body_index).toString();
    QModelIndex owner_index = ref_erl_model_->SkeletonModel()->index(row, 2);
    QString owner = ref_erl_model_->SkeletonModel()->QStandardItemModel::data(owner_index).toString();
    QModelIndex comment_index = ref_erl_model_->SkeletonModel()->index(row, 3);
    QString comment = ref_erl_model_->SkeletonModel()->QStandardItemModel::data(comment_index).toString();

    SkeletonDialog dialog;
    dialog.SetName(name);
    dialog.SetNameEnabled(false);
    dialog.SetSkeleton(body);
    dialog.SetComment(comment);
    dialog.exec();
    if(dialog.Accepted()) {
        ref_erl_model_->ModifySkeleton(
            dialog.GetName(),
            dialog.GetSkeleton(),
            dialog.GetComment()
       );
    }
    ref_erl_model_->GetSkeletons();
}

void QueriesWidget::ChangeDeleteSkeletonButtonState(const QModelIndex &index)
{
    if( !index.isValid() ) {
        QMessageBox::warning(this, "Warning", "Select a skeleton first!");
        return;
    } else {
        int row = index.row();
        QModelIndex skeleton_index = ref_erl_model_->SkeletonModel()->index(row, 1);
        QString owner = ref_erl_model_->SkeletonModel()->data(skeleton_index).toString();
        if(owner != ref_erl_model_->GetUserName()) {
            ui->deleteSkeletonButton->setEnabled(false);
            ui->modifySkeletonButton->setEnabled(false);
        } else {
            ui->deleteSkeletonButton->setEnabled(true);
            ui->modifySkeletonButton->setEnabled(true);
        }
    }
}

void QueriesWidget::AdjustTableSize(QModelIndex, QModelIndex)
{
    ui->runningQueriesTableView->resizeColumnsToContents();
    ui->skeletonsTableView->resizeColumnsToContents();
    ui->queriesTableView->resizeColumnsToContents();
    ui->fileTableView->resizeColumnsToContents();
}

void QueriesWidget::DeleteSkeletonButtonClicked()
{
    QModelIndex current_index =  ui->skeletonsTableView->currentIndex();
    int row = current_index.row();
    QModelIndex name_index = ref_erl_model_->SkeletonModel()->index(row, 0);
    QString name = ref_erl_model_->SkeletonModel()->data(name_index).toString();
    ref_erl_model_->DeleteSkeleton(name);
    ref_erl_model_->GetSkeletons();
}

void QueriesWidget::QueryDoubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QModelIndex query_index = ref_erl_model_->QueriesModel()->index(row, 0);
    QModelIndex file_index = ref_erl_model_->QueriesModel()->index(row, 1);
    QModelIndex position_index = ref_erl_model_->QueriesModel()->index(row, 2);
    QString query = ref_erl_model_->QueriesModel()->data(query_index).toString();
    QString file = ref_erl_model_->QueriesModel()->data(file_index).toString();
    bool ok;
    int position;
    position = ref_erl_model_->QueriesModel()->data(position_index).toInt(&ok);
    if(file.isEmpty() || !ok) {
        ref_erl_model_->RunQuery(query);
    } else {
        ref_erl_model_->RunQuery(query, file, position);
    }
    ref_erl_model_->GetRunningQueries();
}

void QueriesWidget::KillQueryButtonClicked()
{
    QModelIndex current_index = ui->runningQueriesTableView->currentIndex();
    int row = current_index.row();
    QModelIndex qid_index = ref_erl_model_->RunningQueriesModel()->index(row, 0);
    bool ok;
    int qid = ref_erl_model_->RunningQueriesModel()->data(qid_index).toInt(&ok);
    if(ok) {
        ref_erl_model_->KillQuery(qid);
    }
    ref_erl_model_->GetRunningQueries();
}

void QueriesWidget::QueryResultDoubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QModelIndex file_index = ref_erl_model_->QueryResultModel()->index(row, 1, index.parent());
    QModelIndex start_index = ref_erl_model_->QueryResultModel()->index(row, 2, index.parent());
    QModelIndex end_index = ref_erl_model_->QueryResultModel()->index(row, 3, index.parent());
    QString file = ref_erl_model_->QueryResultModel()->QStandardItemModel::data(file_index).toString();
    bool ok;
    int start_position, end_position;
    start_position = ref_erl_model_->QueryResultModel()->QStandardItemModel::data(start_index).toInt(&ok);
    end_position = ref_erl_model_->QueryResultModel()->QStandardItemModel::data(end_index).toInt(&ok);
    latest_start_ = start_position;
    latest_end_ = end_position;
    if(file.isEmpty() || !ok) {
        QMessageBox::warning(this, "Warning", "No information!");
    } else {
        if(file != code_browser_widget_->GetPath()) {
            open_file_ = true;
            ref_erl_model_->CatFile(file);
        }
        else {
            code_browser_widget_->JumpToPosition(latest_start_);
            code_browser_widget_->HighLightSection(latest_start_, latest_end_);
        }
    }
}

void QueriesWidget::FileDoubleClicked(const QModelIndex &index) {
    int row = index.row();
    QModelIndex file_index = ref_erl_model_->FileModel()->index(row, 1);
    QString file = ref_erl_model_->FileModel()->data(file_index).toString();
    if(file != code_browser_widget_->GetPath()) {
        ref_erl_model_->CatFile(file);
        open_file_ = true;
    }
}

void QueriesWidget::ShowFile(const QString &path, const QString &content)
{
    if(!open_file_) {
        return;
    }
    code_browser_widget_->SetContent(path, content);
    code_browser_widget_->JumpToPosition(latest_start_);
    code_browser_widget_->HighLightSection(latest_start_, latest_end_);
    open_file_ = false;
}

void QueriesWidget::SkeletonCallFormat(const QString &format)
{
    QString query =
        QInputDialog::getText(
            this, "Run skeleton", "Fill the parameters",
            QLineEdit::Normal, format
        );
    if(!query.isEmpty()) {
        ref_erl_model_->RunQuery(query);
    }
    ref_erl_model_->GetQueries();
}

void QueriesWidget::SkeletonDoubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QModelIndex skeleton_index = ref_erl_model_->SkeletonModel()->index(row, 0);
    QString name = ref_erl_model_->SkeletonModel()->data(skeleton_index).toString();
    ref_erl_model_->GetSkeletonCallFormat(name);
}

void QueriesWidget::QueryEditTextEdited(const QString &text)
{
    autocomplete_ = true;
    ref_erl_model_->AutoComplete(text);
}

void QueriesWidget::AutoComplete(const QStringList &autocomplete_list,
                                 const QStringList &completed_list)
{
    if(!autocomplete_) return;
    completer_model_->setStringList(autocomplete_list);
    last_completed_ = completed_list;
    completer_->complete();
    autocomplete_ = false;
}

void QueriesWidget::SetCompletion(const QString &text)
{
    int index = completer_model_->stringList().indexOf(text);
    ui->queryEdit->setText( last_completed_.at(index) );
}
