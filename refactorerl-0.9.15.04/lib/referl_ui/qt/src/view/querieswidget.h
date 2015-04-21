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

#ifndef QUERIESWIDGET_H
#define QUERIESWIDGET_H

#include <QWidget>
#include <QCompleter>
#include <QStandardItemModel>
#include <QStringList>
#include <QStringListModel>
#include <QSplitter>
#include "model/referlmodel.h"
#include "codebrowser/codebrowserwidget.h"

namespace Ui {
class QueriesWidget;
}

//Provides user interface for the semantic queries
class QueriesWidget : public QWidget
{
    Q_OBJECT

    private:
        Ui::QueriesWidget *ui;
        RefErlModel *ref_erl_model_;
        CodeBrowserWidget *code_browser_widget_;
        bool open_file_ = false;
        int latest_start_ = -1;
        int latest_end_ = -1;
        bool autocomplete_ = false;
        QStringListModel *completer_model_;
        QCompleter *completer_;
        QStringList last_completed_;

        QSplitter *splitter_;

    public:
        explicit QueriesWidget(
            QWidget *parent = 0,
            RefErlModel *ref_erl_model = 0);
        ~QueriesWidget();

        void ConnectSignals(RefErlModel *ref_erl_model);
private slots:
        void AutoComplete(const QStringList &autocomplete_list,
                          const QStringList &completed_list);
        void QueryEditTextEdited(const QString &text);
        void SetCompletion(const QString &text);
        void SaveSkeleton(const QString &name);
        void DeleteSkeleton(const QString &name);
        void ModifySkeleton(const QString &);
        void QueryDoubleClicked(const QModelIndex &index);
        void QueryResultDoubleClicked(const QModelIndex &index);
        void SkeletonDoubleClicked(const QModelIndex &index);
        void SkeletonCallFormat(const QString &format);
        void ShowFile(const QString &path, const QString &content);
        void FileDoubleClicked(const QModelIndex &index);
        void ChangeDeleteSkeletonButtonState(const QModelIndex &index);
        void AdjustTableSize(QModelIndex, QModelIndex);

    public slots:
        void SwitchToResult();
        void SwitchToRunning();
        void SaveSkeletonButtonClicked();
        void ModifySkeletonButtonClicked();
        void RunQueryButtonClicked();
        void DeleteQueryButtonClicked();
        void DeleteSkeletonButtonClicked();
        void KillQueryButtonClicked();

};

#endif // QUERIESWIDGET_H
