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

#ifndef EDGEITEM_H
#define EDGEITEM_H

#include <QGraphicsItem>
#include "nodeitem.h"

class NodeItem;

//The class is used to draw a line between two NodeItems on a QGraphicsView
class EdgeItem : public QGraphicsItem
{

    private:
        NodeItem *from_, *to_;
        QPointF from_point_; //Starting point of the line on the graphics area
        QPointF to_point_; //Ending point of the line on the graphics area
        qreal arrow_size_;

    protected:
        QRectF boundingRect() const override;
        //Redefined paint function to paint the line itself and the arrow
        // at its end
        void paint(QPainter *painter,
                   const QStyleOptionGraphicsItem *option,
                   QWidget *widget) override;

    public:
        EdgeItem(NodeItem *from, NodeItem *to);
        NodeItem *From() const;
        NodeItem *To() const;
        //If the two NodeItem's position have changed this function
        // calculates the new starting and ending positions of the line
        void Adjust();
};

#endif // EDGEITEM_H
