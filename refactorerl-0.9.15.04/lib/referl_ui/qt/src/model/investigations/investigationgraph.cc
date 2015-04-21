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

#include <QStringList>
#include "investigationgraph.h"

InvestigationGraph::InvestigationGraph()
{
}

InvestigationGraph::~InvestigationGraph()
{
    adj_.clear();
    nodes_.clear();
}

const QMap<QString, InvestigationNode*> &InvestigationGraph::GetNodes() const
{
    return nodes_;
}

const QMap<QString, QList<InvestigationNode*> >
    &InvestigationGraph::GetEdges() const
{
    return adj_;
}

void InvestigationGraph::AddNode(InvestigationNode *node)
{
    nodes_[node->GetId()] = node;
}

bool InvestigationGraph::AddEdge(const QString &from_id,
                                 const QString &to_id)
{
    if(nodes_.count(from_id) > 0 && nodes_.count(to_id) > 0) {
        adj_[from_id].push_back(nodes_[to_id]);
        return true;
    }
    return false;
}

const QString &InvestigationGraph::GetName() const
{
    return name_;
}

void InvestigationGraph::SetName(const QString &name)
{
    name_ = name;
}

void InvestigationGraph:: SetDbHash(const DbHash &hash)
{
    db_hash_ = hash;
}

const DbHash &InvestigationGraph::GetDbHash() const
{
    return db_hash_;
}

void InvestigationGraph::RemoveNode(const QString &id)
{
    if(nodes_.count(id) == 0) return; //If the node is not in the nodes'
                                      //list do nothing
    InvestigationNode *node = nodes_[id];
    if(adj_.count(id) != 0) { //If the node has no children, move on
        for(InvestigationNode *child : adj_[id] ) {
            RemoveNode(child->GetId());
        }
    }

    //Get the index of the node in its parent's edge list
    int index = 0;
    for(InvestigationNode *pchild : adj_[node->GetParent()]) {
        if(pchild->GetId() == id) {
            break;
        }
        ++index;
    }
    adj_[node->GetParent()].removeAt(index); //Remove the node from the
                                             //parent's edge list
    nodes_.remove(id); //Remove the node from the nodes' list
    delete node; //Actually delete the node
}

const QStringList InvestigationGraph::GetChildIds(const QString &id)
{
    QStringList children;
    if(adj_.count(id) == 0) return children;
    for(InvestigationNode *child : adj_[id]) {
        children << child->GetId();
        children = children + GetChildIds(child->GetId());
    }
    return children;
}

void InvestigationGraph::CreateSubGraph(const QString &id)
{
    if(nodes_.count(id) == 0) return; //If the node is not in the nodes'
                                      //list do nothing
    InvestigationNode *top = nodes_[id];
    QStringList children_ids = GetChildIds(id);
    children_ids << id;
    for(InvestigationNode *node : nodes_) {
        if(children_ids.indexOf(node->GetId()) == -1) {
            nodes_.remove(node->GetId());
            delete node;
        }
    }
    adj_.clear();
    top->SetParent("no_parent");
    //Rebuild edges
    for(InvestigationNode *node : nodes_) {
        AddEdge(node->GetParent(), node->GetId());
    }

}
