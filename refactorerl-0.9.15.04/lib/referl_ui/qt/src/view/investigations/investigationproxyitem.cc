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

#include "investigationproxyitem.h"

InvestigationProxyItem::InvestigationProxyItem(InvestigationsGraphWidget *graph,
                                               InvestigationItem *item) :
    QGraphicsProxyWidget()
{
    item_ = item;
    setWidget(item_);
    setFlag(ItemIsMovable);
    setFlag(ItemSendsGeometryChanges);
    setCacheMode(DeviceCoordinateCache);
    setZValue(-2);
    connect( this, SIGNAL( xChanged() ), this, SLOT( PositionChanged() ) );
    connect( this, SIGNAL( yChanged() ), this, SLOT( PositionChanged() ) );
}

QVariant InvestigationProxyItem::itemChange(GraphicsItemChange change,
                                            const QVariant &value)
{
     switch (change) {
         case ItemPositionHasChanged:
             for(InvestigationEdgeItem *edge : edge_list_) {
                 edge->Adjust();
             }
             break;
         default:
             break;
     };

     return QGraphicsItem::itemChange(change, value);
}

void InvestigationProxyItem::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    QPointF pos = event->pos();
    QPointer<QWidget> alien = widget()->childAt(pos.toPoint());
    if(qobject_cast<QWidget*>(alien)) {
        QGraphicsProxyWidget::mousePressEvent(event);
        grabbed_ = true;
    } else {
        QGraphicsItem::mousePressEvent(event);
        grabbed_= false;
    }
}

void InvestigationProxyItem::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    if(grabbed_) {
        QGraphicsProxyWidget::mouseReleaseEvent(event);
    } else {
        QGraphicsItem::mouseReleaseEvent(event);
    }
    grabbed_ = false;
}

void InvestigationProxyItem::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    if(grabbed_) return;
    QGraphicsItem::mouseMoveEvent(event);
}

void InvestigationProxyItem::AddEdge(InvestigationEdgeItem *edge)
{
    edge_list_ << edge;
}

QList<InvestigationEdgeItem *> InvestigationProxyItem::Edges() const
{
    return edge_list_;
}

InvestigationNode *InvestigationProxyItem::Node()
{
    return item_->Node();
}

void InvestigationProxyItem::PositionChanged()
{
    item_->Node()->SetX( x() );
    item_->Node()->SetY( y() );
}
