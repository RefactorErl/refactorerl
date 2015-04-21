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

#include "dependencygraph.h"
#include <qmath.h>

DependencyGraph::DependencyGraph(const double &w, const double &h) :
    width_(w), height_(h)
{
}

const QMap<QString, Node*> &DependencyGraph::GetNodes() const
{
    return nodes_;
}

const QMap<QString, QList<Edge *> > &DependencyGraph::GetEdges() const
{
    return adj_;
}

void DependencyGraph::AddNode(Node *node)
{
    nodes_[node->GetId()] = node;
}

bool DependencyGraph::AddEdge(const QString &from_id, const QString &to_id)
{
    if( nodes_.count(from_id) > 0 && nodes_.count(to_id) > 0 ) {
        adj_[from_id].push_back( new Edge( nodes_[from_id], nodes_[to_id] ) );
        return true;
    }
    return false;
}


void DependencyGraph::SetSize(const double &w, const double &h)
{
    width_ = w;
    height_ = h;
}

const double &DependencyGraph::Width() const
{
    return width_;
}

const double &DependencyGraph::Height() const
{
    return height_;
}

void DependencyGraph::Shuffle()
{
    for(Node *node: nodes_) {
        node->SetX(qrand() % (int)Width());
        node->SetY(qrand() % (int)Height());
    }
}

void DependencyGraph::Arrange()
{
    /*double size = qSqrt(nodes_.count()) * 60;
    SetSize(size, size);
    Shuffle();*/
    for(Node *node: nodes_) {
        node->SetX(0);
        node->SetY(0);
    }

    int max_iterations = qPow(nodes_.count(), 2);

    bool items_moved = true;
    QMap<QString, QPair<double, double>> new_positions;
    for(Node *node : nodes_) {
        new_positions[node->GetId()] = QPair<double, double>(node->GetX(), node->GetY());
    }
    while(items_moved && max_iterations > 0) {
        --max_iterations;
        for(Node *node : nodes_) {
            //Push
            double xvel = 0.0;
            double yvel = 0.0;
            double x = node->GetX();
            double y = node->GetY();
            for(Node *other : nodes_) {
                if(node->GetId() == other->GetId()) {
                    continue;
                }
                double ox = other->GetX();
                double oy = other->GetY();

                double dx = x - ox;
                double dy = y - oy;
                double l = 2.0 * (dx * dx + dy * dy);
                if (l > 0) {
                    xvel += (dx * 100.0) / l;
                    yvel += (dy * 100.0) / l;
                } else {
                    xvel = 60 * (qPow(-1, qrand() % 2));
                    yvel = 60 * (qPow(-1, qrand() % 2));
                }
            }

            //Pull
            double weight = (adj_[node->GetId()].size() + 1) * 10;
            for(Edge *edge : adj_[node->GetId()]) {
                double dx, dy;
                if(edge->From() == node) {
                    dx = x - edge->To()->GetX();
                    dy = y - edge->To()->GetY();
                } else {
                    dx = x - edge->From()->GetX();
                    dy = y - edge->From()->GetY();
                }

                xvel -= dx / weight;
                yvel -= dy / weight;
            }

            if (qAbs(xvel) < 10.0 && qAbs(yvel) < 10.0)
                xvel = yvel = 0;

            double newx = x + xvel;
            double newy = y + yvel;
            new_positions[node->GetId()].first = newx;
            new_positions[node->GetId()].second = newy;
        }

        items_moved = false;
        for(Node *node : nodes_) {
            double new_x = new_positions[node->GetId()].first;
            double new_y = new_positions[node->GetId()].second;
            if(node->GetX() !=  new_x || node->GetY() != new_y) {
                node->SetX(new_x);
                node->SetY(new_y);
                items_moved = true;
            }
        }
   }
}
