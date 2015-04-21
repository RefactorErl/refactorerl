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

#ifndef INVESTIGATIONSGRAPHWIDGET_H
#define INVESTIGATIONSGRAPHWIDGET_H

#include <QGraphicsView>
#include "model/investigations/investigationnode.h"
#include "model/investigations/investigationgraph.h"
#include "model/referlmodel.h"
#include "investigationproxyitem.h"

//Draws and manages an investigation grah
class InvestigationsGraphWidget : public QGraphicsView
{
    Q_OBJECT

    private:
        InvestigationGraph graph_; //The graph model
        RefErlModel *ref_erl_model_;
        bool is_new_ = false; //Indicates that the current graph is new (usaved)
        bool changed_ = false; //Indicates that the current graph has changed

    protected:
        //Redefined wheelEvent to allow zooming
        void wheelEvent(QWheelEvent *event) override;
        //Actually scale (zoom) the contents
        void scaleView(qreal scaleFactor);

    public:
        InvestigationsGraphWidget(QWidget *parent = 0,
                                  RefErlModel *ref_erl_model = 0);
        //Draws and saves the graph given
        void DrawInvestigationGraph(const InvestigationGraph &graph);
        //Sets is_new_
        void SetIsNew(const bool &is_new);
        //Returns is_new
        const bool &GetIsNew() const;
        //Returns changed
        const bool &GetChanged() const;
        //Returns the graph name
        const QString &GetGraphName();

    private slots:
        //Event handler for the InvestigationItems' (investigation box widgets)
        //DeleteSignal signal. Deletes the node from the graph
        void Delete(const QString &id);
        //Event handler for the InvestigationItems' (investigation box widgets)
        //NewSignal signal. Deletes everything in the graph but the node.
        void New(const QString &id);
        //Event handler for the InvestigationItems' (investigation box widgets)
        //MoveToNewSignal signal. Modifies the graph to be the node's subgraph
        void MoveToNew(const QString &id);
        //Event handler for the InvestigationItems' (investigation box widgets)
        //ChangedSignal signal.Notifies the containing InvestigationsWidget about
        //the graph change and sets the internal change_ flag.
        void Changed();

    public slots:
        void zoomIn();
        void zoomOut();
        void AddMemo(const QString &id,
                     const QString &parent);
        void AddNode(InvestigationNode* node);
        void Save();
        void SaveAs();
        //Event handler for the ref_erl_model_'s DatabaseChangedSignal
        //if the database has changed sets the graph nodes not to allow querying
        void DatabaseChanged();

    signals:
        //When the graph is saved signals the new name
        void NewName(const QString &name);
        //Signals changes in the graph
        void ChangedSignal();
        //Signals that the source node doesn't exists
        void NoSourceNode();
};

#endif // INVESTIGATIONSGRAPHWIDGET_H
