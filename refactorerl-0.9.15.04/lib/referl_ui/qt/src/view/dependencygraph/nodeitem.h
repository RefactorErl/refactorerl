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

#ifndef NODEITEM_H
#define NODEITEM_H

#include <QGraphicsItem>
#include <QList>
#include "graphwidget.h"
#include "edgeitem.h"
#include "model/dependencygraph/node.h"

class EdgeItem;
class GraphWidget;
class QGraphicsSceneMouseEvent;

//This class is used to draw a dependeny graph node on a QGraphicsView
//The color of the node depends on the underlying model's DependencyType
class NodeItem : public QGraphicsItem
{
    private:
        GraphWidget *graph_;
        QList<EdgeItem *> edge_list_; //Edges to or from this NodeItem
        Node node_; //The model representing the node
        int label_width_; //With of the label information
        QPointF newPos;

    protected:
        //Redefined itemChange to also adjust the edges
        QVariant itemChange(GraphicsItemChange change, const QVariant &value) override;
        void mousePressEvent(QGraphicsSceneMouseEvent *event) override;
        void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) override;

    public:
        NodeItem(GraphWidget *graph_widget, Node node);
        void AddEdge(EdgeItem *edge); //Add a new edge to the edge list
        QList<EdgeItem *> Edges() const; //Returns the edge list
        QRectF boundingRect() const override;

        //QPainterPath shape() const override;

        //Redefined paint function to actually paint the node's box and
        // the label
        void paint(QPainter *painter,
                   const QStyleOptionGraphicsItem *option,
                   QWidget *widget) override;
        const QString &GetId(); //Return the unique ID of the node_ (model)

};

#endif // NODEITEM_H
