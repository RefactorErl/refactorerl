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

#include "investigationedgeitem.h"

static const double pi = 3.14159265358979323846264338327950288419717;
static double two_pi = 2.0 * pi;

InvestigationEdgeItem::InvestigationEdgeItem(InvestigationProxyItem *from,
                                             InvestigationProxyItem *to) :
    arrow_size_(10)
{
    from_ = from;
    to_ = to;
    from_->AddEdge(this);
    to_->AddEdge(this);
    Adjust();
    setZValue(-1);
    setAcceptedMouseButtons(0);
}

QRectF InvestigationEdgeItem::boundingRect() const
{
    if(!from_ || !to_)
        return QRectF();

    qreal penWidth = 1;
    qreal extra = (penWidth + arrow_size_) / 2.0;

    return QRectF(from_point_, QSizeF(to_point_.x() - from_point_.x(),
                                      to_point_.y() - from_point_.y()))
        .normalized()
        .adjusted(-extra, -extra, extra, extra);
}

void InvestigationEdgeItem::paint(QPainter *painter,
                                  const QStyleOptionGraphicsItem *, QWidget *)
{
     if(!from_ || !to_)
         return;

     QLineF line(from_point_, to_point_);
     double angle = -pi / 2 - pi / 10;

     if(qFuzzyCompare(line.length(), qreal(0.))) return;

     painter->setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin));
     painter->drawLine(line);
     angle = ::acos(line.dx() / line.length());

     //Draw text
     qreal text_x = (line.p1().x() + line.p2().x()) / 2;
     qreal text_y = (line.p1().y() + line.p2().y()) / 2;
     painter->drawText(QPointF( text_x, text_y ), to_->Node()->GetEdgeLabel());

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

void InvestigationEdgeItem::Adjust()
{
    if(!from_ || !to_)
        return;

    QLineF line(mapFromItem(from_, 0, 0), mapFromItem(to_, 0, 0));
    qreal length = line.length();

    prepareGeometryChange();

    if (length > qreal(20.)) {
        QPointF to_edge_offset((line.dx() * to_->boundingRect().width()/2) / length,
                               (line.dy() * to_->boundingRect().height()/2) / length);
        QPointF from_edge_offset((line.dx() * from_->boundingRect().width()/2) / length,
                                 (line.dy() * from_->boundingRect().height()/2) / length);

        from_point_ = line.p1() + from_edge_offset + QPointF( from_->boundingRect().width()/2, from_->boundingRect().height()/2 );
        to_point_ = line.p2() - to_edge_offset + QPointF( to_->boundingRect().width()/2, to_->boundingRect().height()/2 );
    } else {
        from_point_ = to_point_ = line.p1();
    }
}
