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

#include "edgeitem.h"
#include <QPainter>
#include "nodeitem.h"
#include <math.h>

static const double pi = 3.14159265358979323846264338327950288419717;
static double two_pi = 2.0 * pi;

EdgeItem::EdgeItem(NodeItem *from, NodeItem *to) : arrow_size_(10)
{
     setAcceptedMouseButtons(0);
     from_ = from;
     to_ = to;
     from_->AddEdge(this);
     to_->AddEdge(this);
     Adjust();
     setZValue(-2);
}

NodeItem *EdgeItem::From() const
{
    return from_;
}

NodeItem *EdgeItem::To() const
{
    return to_;
}

void EdgeItem::Adjust()
{
    if(!from_ || !to_)
        return;

    QLineF line(mapFromItem(from_, 0, 0), mapFromItem(to_, 0, 0));
    qreal length = line.length();

    prepareGeometryChange();

    if( from_->GetId() == to_->GetId() ) {
        from_point_ = to_point_ = QPointF( to_->x(), to_->y() );
        return;
    }
    if (length > qreal(20.)) {
        QPointF to_edge_offset((line.dx() * to_->boundingRect().width()/2) / length,
                               (line.dy() * to_->boundingRect().height()/2) / length);
        QPointF from_edge_offset((line.dx() * from_->boundingRect().width()/2) / length,
                                 (line.dy() * from_->boundingRect().height()/2) / length);
        from_point_ = line.p1() + from_edge_offset;
        to_point_ = line.p2() - to_edge_offset;
    } else {
        from_point_ = to_point_ = line.p1();
    }
}

QRectF EdgeItem::boundingRect() const
{
    if(!from_ || !to_)
        return QRectF();

    qreal penWidth = 1;
    qreal extra = (penWidth + arrow_size_) / 2.0;

    if(from_->GetId() == to_->GetId()) {
        return QRectF(to_point_.x() + to_->boundingRect().width()/2 - to_->boundingRect().height()/2,
                      to_point_.y() - to_->boundingRect().height()/2,
                      to_->boundingRect().height(),
                      to_->boundingRect().height())
            .normalized()
            .adjusted(-extra, -extra, extra, extra);
    }
    return QRectF(from_point_, QSizeF(to_point_.x() - from_point_.x(),
                                      to_point_.y() - from_point_.y()))
        .normalized()
        .adjusted(-extra, -extra, extra, extra);
}

void EdgeItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *, QWidget *)
{
     if(!from_ || !to_)
         return;

     QLineF line(from_point_, to_point_);
     double angle = -pi / 2 - pi / 10;

     if(from_->GetId() == to_->GetId()) {
         painter->setPen(QPen(Qt::red, 1, Qt::DotLine, Qt::RoundCap, Qt::RoundJoin));
         painter->drawEllipse(to_point_.x() + to_->boundingRect().width()/2 - to_->boundingRect().height()/2,
                              to_point_.y() - to_->boundingRect().height(),
                              to_->boundingRect().height(),
                              to_->boundingRect().height());

         //Draw the arrow
         QPointF tip(to_point_.x() + to_->boundingRect().width()/2 - to_->boundingRect().height()/2,
                     to_point_.y() - to_->boundingRect().height()/2);
         QPointF dest_arrow_p1 = tip + QPointF(sin(angle - pi / 3) * arrow_size_,
                                                   cos(angle - pi / 3) * arrow_size_);
         QPointF dest_arrow_p2 = tip + QPointF(sin(angle - pi + pi / 3) * arrow_size_,
                                                   cos(angle - pi + pi / 3) * arrow_size_);

         painter->setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin));
         painter->setBrush(Qt::black);
         painter->drawPolygon(QPolygonF() << tip << dest_arrow_p1 << dest_arrow_p2);


     } else if( qFuzzyCompare(line.length(), qreal(0.)) ) {
         return;
     } else {
         painter->setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin));
         painter->drawLine(line);
         angle = ::acos(line.dx() / line.length());

         // Draw the arrow
         if (line.dy() >= 0)
             angle = two_pi - angle;

         QPointF dest_arrow_p1 = to_point_ + QPointF(sin(angle - pi / 3) * arrow_size_,
                                                   cos(angle - pi / 3) * arrow_size_);
         QPointF dest_arrow_p2 = to_point_ + QPointF(sin(angle - pi + pi / 3) * arrow_size_,
                                                   cos(angle - pi + pi / 3) * arrow_size_);

         painter->setBrush(Qt::black);
         painter->drawPolygon(QPolygonF() << line.p2() << dest_arrow_p1 << dest_arrow_p2);
     }
}
