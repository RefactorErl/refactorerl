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

#ifndef INVESTIGATIONGRAPH_H
#define INVESTIGATIONGRAPH_H

#include <QMap>
#include <QList>
#include "investigationnode.h"
#include "model/common.h"

//Represents an investigation graph, using edge-list representation
//Nodes can be added and removed, edges can be added too.
//It's possible to transfor the graph into a subgraph of a given node.
class InvestigationGraph
{
    private:
        QMap<QString, InvestigationNode*> nodes_; //NodeId, Node
        QMap<QString, QList<InvestigationNode*> > adj_; //NodeId, Edge list
        QString name_ = QString(); //Investigation name
        DbHash db_hash_; //Database hash

    public:
        //Constructor
        InvestigationGraph();

        //Destructor
        ~InvestigationGraph();

        //Add a node to the graph. It is the user's responsibility
        //  not to pass NULL or uninitialized pointer.
        void AddNode(InvestigationNode *node);

        //Add an edge to the graph. If any of the given
        // ids is not present in the graph the edge is not
        // added and the function returns false.
        bool AddEdge(const QString &from_id,
                     const QString &to_id);

        //Return the nodes indexed by their unique id
        const QMap<QString, InvestigationNode*> &GetNodes() const;

        //Return the edges, the edge lists are indexed
        //  by the parent node's id
        const QMap<QString, QList<InvestigationNode*> > &GetEdges() const;

        //Return the investigation name
        const QString &GetName() const;

        //Set the investigation name
        void SetName(const QString &name);

        //Set the database hash
        void SetDbHash(const DbHash &hash);

        //Return the database hash
        const DbHash &GetDbHash() const;

        //Remove a node with the given id
        void RemoveNode(const QString &id);

        const QStringList GetChildIds(const QString &id);

        //Create sub graph from the given node
        void CreateSubGraph(const QString &id);
};

#endif // INVESTIGATIONGRAPH_H
