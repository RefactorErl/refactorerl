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

#ifndef INVESTIGATIONPROXYITEM_H
#define INVESTIGATIONPROXYITEM_H

#include <QGraphicsProxyWidget>
#include <QGraphicsSceneMouseEvent>
#include <QPointer>
#include "investigationitem.h"
#include "investigationedgeitem.h"
#include "investigationsgraphwidget.h"

class InvestigationEdgeItem;
class InvestigationItem;
class InvestigationsGraphWidget;

//The class allows to use the InvestigationItem widget in a QGraphicsView
//Paints the widget and acts as a graphic item, letting through button clicks,
//text editin, etc. to the underlying widget
class InvestigationProxyItem : public QGraphicsProxyWidget
{
    Q_OBJECT

    private:
        InvestigationsGraphWidget *graph_;
        QList<InvestigationEdgeItem *> edge_list_;
        InvestigationItem *item_;
        bool grabbed_; //True if the mouse click is meant for the widget

    protected:
        //Adjusts the edges when moved
        QVariant itemChange(GraphicsItemChange change,
                            const QVariant &value) override;
        void mousePressEvent(QGraphicsSceneMouseEvent *event) override;
        void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) override;
        void mouseMoveEvent(QGraphicsSceneMouseEvent *event) override;

    public:
        InvestigationProxyItem(InvestigationsGraphWidget *graph,
                               InvestigationItem *item);
        void AddEdge(InvestigationEdgeItem *edge);
        QList<InvestigationEdgeItem *> Edges() const;
        InvestigationNode *Node();

    private slots:
        void PositionChanged(); //Event handler for the xChanged and yChanged
                                //signal. Saves the positions in the node (model)

};

#endif // INVESTIGATIONPROXYITEM_H
