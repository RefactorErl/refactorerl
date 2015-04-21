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

#ifndef DEPENDENCYGRAPHWIDGET_H
#define DEPENDENCYGRAPHWIDGET_H

#include <QWidget>
#include <QProgressDialog>
#include <QStringListModel>
#include <QCompleter>
#include "dependencygraph/graphwidget.h"
#include "model/referlmodel.h"

namespace Ui {
class DependencyGraphWidget;
}

//Allows user interaction for the dependency graph drawing functionality
//User interface is defined in the dependencygraphwidget.ui file
class DependencyGraphWidget : public QWidget
{
    Q_OBJECT

    private:
        Ui::DependencyGraphWidget *ui;
        RefErlModel *ref_erl_model_;
        QProgressDialog *dialog_;

		    QStringListModel *excluded_lib_model_;
        QStringListModel *excluded_nodes_model_;
        QStringListModel *excluded_leaves_model_;
		    QStringListModel *starting_nodes_model_;
		    QStringListModel *connection_nodes_model_;
		    QStringListModel *groups_model_;
        GraphWidget *graph_widget_;

        QCompleter *start_node_completer_;
		QCompleter *connection_node_completer_;
        QCompleter *nodes_completer_;
        QCompleter *leaves_completer_;

    private:
        void DrawGraph(const QString &path,
                       const DependencyDrawMethod &method);

    public:
        explicit DependencyGraphWidget(QWidget *parent = 0,
                                       RefErlModel *ref_erl_model = 0);
        ~DependencyGraphWidget();

    private slots:
		    void AddExcludedLibButtonClicked();
        void AddExcludedButtonClicked();
        void AddLeavesButtonClicked();
		    void AddStartingButtonClicked();
		    void AddConnectionButtonClicked();
		    void AddGroupsButtonClicked();
		    void DeleteExcludedLibButtonClicked();
        void DeleteExcludedButtonClicked();
        void DeleteLeavesButtonClicked();
		    void DeleteStartingButtonClicked();
		    void DeleteConnectionButtonClicked();
		    void DeleteGroupsButtonClicked();
        void ErrorHandler(QString);
        void DrawButtonClicked();
        void DrawGraphButtonClicked();
        void DrawSVGButtonClicked();
        void LevelComboboxCurrentTextChanged(const QString &current);

    public slots:
        void ShowSVG(const QString &path);
        void ShowGraph(const QString &path);
        void ShowGraph(DependencyGraph *graph);

};

#endif // DEPENDENCYGRAPHWIDGET_H
