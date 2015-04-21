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

#ifndef DEPENDENCYGRAPH_H
#define DEPENDENCYGRAPH_H

#include <QMap>
#include <QList>
#include <QString>
#include "node.h"
#include "edge.h"

//Used to represent a dependency graph, using edge-list representation
class DependencyGraph
{
    private:
        QMap<QString, Node*> nodes_; //NodeId, Node
        QMap<QString, QList<Edge*> > adj_; //NodeId, Edge list
        double width_; //Grpah width
        double height_; //Graph height

    public:
        //Constructor
        DependencyGraph(const double &w = 0, //Graph width
                        const double &h = 0); //Graph height

        //Add a node to the graph. It is the user's responsibility
        //  not to pass NULL or uninitialized pointer.
        void AddNode(Node *node);

        //Add an edge to the graph. If any of the given
        // ids is not present in the graph the edge is not
        // added and the function returns false.
        bool AddEdge(const QString &from_id,
                     const QString &to_id);

        //Return the nodes indexed by their unique id
        const QMap<QString, Node *> &GetNodes() const;

        //Return the edges, the edge lists are indexed
        //  by the parent node's id
        const QMap<QString, QList<Edge *> > &GetEdges() const;

        //Set the graph size
        void SetSize(const double &w, //Graph width
                     const double &h); //Graph height

        //Return the graph width
        const double &Width() const;

        //Return the graph height
        const double &Height() const;

        void Shuffle();
        void Arrange();
};

#endif // DEPENDENCYGRAPH_H
