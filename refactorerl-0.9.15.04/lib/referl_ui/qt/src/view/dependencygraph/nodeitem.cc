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

#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include <QStyleOption>
#include <QFontMetrics>
#include <QFont>
#include "nodeitem.h"

NodeItem::NodeItem(GraphWidget *graph_widget, Node node) :
    graph_(graph_widget), node_(node)
{
    setFlag(ItemIsMovable);
    setFlag(ItemSendsGeometryChanges);
    setCacheMode(DeviceCoordinateCache);
    setZValue(-1);
    QFont font;
    QFontMetrics fm(font);
    label_width_ = fm.width(node.GetLabel());
}

void NodeItem::AddEdge(EdgeItem *edge)
{
    edge_list_ << edge;
    edge->Adjust();
}

QList<EdgeItem *> NodeItem::Edges() const
{
    return edge_list_;
}

QRectF NodeItem::boundingRect() const
{
    return QRectF( -(label_width_+30)/2, -20,
                label_width_ + 33, 43);
}
/*
QPainterPath NodeItem::shape() const
{
     QPainterPath path;
     path.addRect(-(label_width_+30)/2, -20, label_width_ + 20, 40);
     return path;
}*/

void NodeItem::paint(QPainter *painter,
                     const QStyleOptionGraphicsItem *option, QWidget *)
{
    QColor color;

    switch(node_.Level()) {
        case Module:
            color = QColor(Qt::yellow).light(160);
            break;
        case Function:
            color = QColor(Qt::blue).light(160);
            break;
        case ModuleGroup:
            color = QColor(Qt::red).light(160);
            break;
        default:
            color = QColor(Qt::green).light(160);
    }
    if(node_.GetLabel() == "ROOT") {
        color = QColor(Qt::green).light(160);
    }

    QRadialGradient gradient(-3, -3, 10);
    if (option->state & QStyle::State_Sunken) {
        gradient.setCenter(3, 3);
        gradient.setFocalPoint(3, 3);
        gradient.setColorAt(1, color.light(120));
        gradient.setColorAt(0, color.light(120));
    } else {
        gradient.setColorAt(0, color);
        gradient.setColorAt(1, color);
    }

    painter->setBrush(gradient);
    painter->setPen(QPen(Qt::black, 0));

    painter->drawRect(-(label_width_+30)/2, -20, label_width_+30, 40);
    painter->drawText(0 - label_width_/2, 0, node_.GetLabel());
    //painter->drawEllipse(QPoint(0,0), 5, 5);
}

QVariant NodeItem::itemChange(GraphicsItemChange change, const QVariant &value)
{
     switch (change) {
         case ItemPositionHasChanged:
             for(EdgeItem *edge : edge_list_) {
                 edge->Adjust();
             }
             //graph_->ItemMoved();
             break;
         default:
             break;
     };

     return QGraphicsItem::itemChange(change, value);
}

void NodeItem::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    QGraphicsItem::mousePressEvent(event);
}

void NodeItem::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    QGraphicsItem::mouseReleaseEvent(event);
}

const QString &NodeItem::GetId()
{
    return node_.GetId();
}
