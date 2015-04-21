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

#ifndef MDIWIDGET_H
#define MDIWIDGET_H

#include <QMdiArea>
#include <QMdiSubWindow>
#include <QThread>
#include "model/referlmodel.h"
#include "filebrowserwidget.h"
#include "dependencygraphwidget.h"
#include "querieswidget.h"
#include "duplicatedcodewidget.h"
#include "investigationswidget.h"

//Manages sub windows and tabs
class MdiWidget : public QMdiArea
{
    Q_OBJECT

    private:
        RefErlModel *ref_erl_model_ = NULL;
        QThread *model_thread_ = NULL;
        bool open_file_ = false;

        QMdiSubWindow *file_browser_widget_ = NULL;
        QMdiSubWindow *dependency_graph_widget_ = NULL;
        QMdiSubWindow *queries_widget_ = NULL;
        QMdiSubWindow *duplicated_code_widget_ = NULL;
        QMdiSubWindow *investigations_widget_ = NULL;

    public:
        explicit MdiWidget(QWidget *parent = 0);
        ~MdiWidget();
        const QString GetUserName() const;

    private slots:
        void ShowCodeBrowser(const QString &path, const QString &content);
        void ShowErrorMessage(const QString &msg);
        void ShowQueryResult();
        void StartSelectedDupcode(const QString &algorithm_key,
                                  const QString &file_path,
                                  const int &start,
                                  const int &end);
        void FileBrowserDestroyed();
        void DependencyGraphDestroyed();
        void QueriesDestroyed();
        void DuplicatedCodeDestroyed();
        void InvestigationsDestroyed();

    public slots:
        void ShowFileBrowser();
        void ShowDependencyGraph();
        void ShowQueries();
        void ShowDuplicatedCode();
        void ShowInvestigations();
        void ShowFileNeeded();
        //Menu actions
        void ActionFileTriggered();
        void ActionAppbaseTriggered();
        void ActionIncludeTriggered();
        void ActionDirectoryTriggered();
        void ActionSyncTriggered();
        void ActionDropTriggered();
        void ActionResetDatabaseTriggered();
        void ActionQueryToRunTriggered();
        void ActionSkeletonTriggered();
        void ActionLastResultsTriggered();
        void ActionRunningQueriesTriggered();
        void ActionModuleTriggered(const DependencyType &type);
        void ActionFunctionTriggered(const DependencyType &type);
        void ActionModuleGroupTriggered(const DependencyType &type);
};

#endif // MDIWIDGET_H
