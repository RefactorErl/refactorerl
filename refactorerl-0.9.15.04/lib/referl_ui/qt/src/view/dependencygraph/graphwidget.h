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

#ifndef GRAPHWIDGET_H
#define GRAPHWIDGET_H

#include <QGraphicsView>
#include "model/dependencygraph/dependencygraph.h"

class NodeItem;

//The class is used to display a dependency graph that can be zoomed
// and the nodes can be moved around
class GraphWidget : public QGraphicsView
{
    Q_OBJECT

    private:
        int timerId;

    protected:
        //Redefined wheelEvent to allow zooming
        void wheelEvent(QWheelEvent *event) override;
        //Actually scale (zoom) the contents
        void scaleView(qreal scaleFactor);

    public:
        GraphWidget(QWidget *parent = 0);
        //Draw the given graph
        void DrawGraph(DependencyGraph *graph, int multiplier = 1);

    public slots:
        void zoomIn();
        void zoomOut();

};

#endif // GRAPHWIDGET_H
