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

#ifndef INVESTIGATIONITEM_H
#define INVESTIGATIONITEM_H

#include <QWidget>
#include <QList>
#include "model/referlmodel.h"
#include "view/codebrowser/codebrowser.h"
#include "model/investigations/investigationnode.h"

class InvestigationEdgeItem;
class InvestigationsGraphWidget;

namespace Ui {
class InvestigationItem;
}

//This class is and investigation-box widget
//Displays an InvestigationNode's contents (label, text, etc)
// and allows query running from it. The node's text is displayed in a
// CodeBrowser (view/codebrowser/codebrowser.h)
class InvestigationItem : public QWidget
{
    Q_OBJECT

    private:
        Ui::InvestigationItem *ui;
        CodeBrowser *code_browser_;
        InvestigationNode *node_;
        RefErlModel *ref_erl_model_;

    private:
        //Set the query area's visibility
        void SetQueriesVisible(const bool &visible);

    public:
        InvestigationItem(InvestigationNode *node,
                          RefErlModel *ref_erl_model);
        ~InvestigationItem();
        InvestigationNode *Node();

    private slots:
        void CursorChanged(); //Ask for predefined queries when the cursor
                              //position changes
        void NameButtonClicked();
        void AddMemoButtonClicked();

        //Sets the contents of the query edit according to the
        //chosen predefined query
        void PredefComboBoxCurrentIndexChanged(int index);
        void RunQueryButtonClicked();
        void HideButtonClicked();
        void DeleteButtonClicked();
        void NewButtonClicked();
        void MoveToNewButtonClicked();
        //If the current node is a memo, saves the text when its changed
        void TextChanged();

    signals:
        void DeleteSignal(const QString &id);
        void NewSignal(const QString &id);
        void MoveToNewSignal(const QString &id);
        void ChangedSignal();
};

#endif // INVESTIGATIONITEM_H
