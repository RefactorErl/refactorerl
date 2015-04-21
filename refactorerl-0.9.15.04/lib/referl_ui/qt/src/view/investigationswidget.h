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

#ifndef INVESTIGATIONSWIDGET_H
#define INVESTIGATIONSWIDGET_H

#include <QWidget>
#include <QCompleter>
#include "model/referlmodel.h"
#include "investigations/investigationsgraphwidget.h"

namespace Ui {
class InvestigationsWidget;
}

//Provides user interface for investigations
class InvestigationsWidget : public QWidget
{
    Q_OBJECT

    private:
        Ui::InvestigationsWidget *ui;
        RefErlModel *ref_erl_model_;
        InvestigationsGraphWidget *graph_widget_;
        InvestigationNodeList latest_results_;
        QStandardItemModel *latest_info_;
        QCompleter *completer_;

    public:
        explicit InvestigationsWidget(QWidget *parent = 0,
                                      RefErlModel *ref_erl_model = 0);
        ~InvestigationsWidget();

    private slots:
        void Changed();
        void LoadInvestigation(const InvestigationGraph &graph);
        void LoadButtonClicked();
        void RunButtonClicked();
        void AddButtonClicked();
        void DeleteButtonClicked();
        void ShareButtonClicked();
        void InvestigationQuery(const InvestigationNodeList &nodes);
        void DeleteInvestigation(const QString &name);
        void SaveInvestigation(const QString &name);
        void ShareInvestigation(const QString &name,
                                const QString &user);
        void NoSourceNode();

    public slots:
        void SetName(const QString &name);

};

#endif // INVESTIGATIONSWIDGET_H
