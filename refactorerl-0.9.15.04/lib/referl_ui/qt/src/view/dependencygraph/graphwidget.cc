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

#include <QtGui>
#include <math.h>
#include "graphwidget.h"
#include "edgeitem.h"
#include "nodeitem.h"
#include "graphwidget.h"

GraphWidget::GraphWidget(QWidget *parent) :
    QGraphicsView(parent)
{
    QGraphicsScene *scene = new QGraphicsScene(this);
    scene->setItemIndexMethod(QGraphicsScene::NoIndex);
    setScene(scene);
    setCacheMode(CacheBackground);
    setViewportUpdateMode(BoundingRectViewportUpdate);
    setRenderHint(QPainter::Antialiasing);
    setTransformationAnchor(AnchorUnderMouse);
    scale(qreal(0.8), qreal(0.8));
}

void GraphWidget::wheelEvent(QWheelEvent *event)
{
    if(event->modifiers() & Qt::ControlModifier) {
        scaleView(pow((double)2, event->delta() / 240.0));
    } else {
        QGraphicsView::wheelEvent(event);
    }
}

void GraphWidget::scaleView(qreal scaleFactor)
{
    qreal factor = transform().scale(scaleFactor, scaleFactor)
            .mapRect(QRectF(0, 0, 1, 1)).width();
    if (factor < 0.07 || factor > 100)
        return;

    scale(scaleFactor, scaleFactor);
}

void GraphWidget::zoomIn()
{
    scaleView(qreal(1.2));
}

void GraphWidget::zoomOut()
{
    scaleView(1 / qreal(1.2));
}

void GraphWidget::DrawGraph(DependencyGraph *graph, int multiplier)
{
    scene()->clear();
    QMap<QString, NodeItem*> node_items;
    for(Node* node : graph->GetNodes()) {
        node_items[node->GetId()] = new NodeItem(this, *node);
        scene()->addItem(node_items[node->GetId()]);
        node_items[node->GetId()]->setPos( multiplier*node->GetX(), multiplier*node->GetY() );
    }
    for(QList<Edge *> edges : graph->GetEdges()) {
        for(Edge* edge : edges) {
            scene()->addItem( new EdgeItem( node_items[edge->From()->GetId()],
                              node_items[edge->To()->GetId()] ) );
        }
    }
}
