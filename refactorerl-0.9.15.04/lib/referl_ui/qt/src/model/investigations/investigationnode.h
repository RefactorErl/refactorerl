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

#ifndef INVESTIGATIONNODE_H
#define INVESTIGATIONNODE_H

#include <QString>
#include <tuple>

typedef std::tuple<QString, QString, int> GraphNode;

//Represents an investigation node
class InvestigationNode
{
    private:
        QString id_; //Unique id (Erlang Pid in string format)
        QString parent_ = "no_parent"; //Parent id, "no_parent" by default
        bool is_memo_; //True if the node is a memo
        QString name_ = QString(); //Node name, can be empty
        bool is_shown_ = true; //True if the node is shown
        GraphNode node_;  //RefactorErl graph node identifier represented as
                          //a 3-tuple:
                          //<QString, QString, int> ("'&gn'", <type>, <number>)
        QString text_ = QString(); //Code snippet
        QString label_ = QString(); //Node label (label of the graph node)
        QString edge_label_ = QString(); //Label of the edge to this node
                                         // if there is any. A query string or
                                         //the string "Memo"
        QString file_path_ = QString(); //File path of the node, can be empty
        int offset_position_ = -1; //Text offset position for queries
        int line_number_ = -1; //Starting line number of the text
        int start_position_ = -1; //Starting position in the file
                                  //of the text (if any)
        int end_position_ = -1; //Ending position in the file
                                //of the text (if any)
        int x_; //Horizontal position
        int y_; //Vertical position
        bool query_enabled_ = true; //Is querying enabled

    public:
        //Constructor
        InvestigationNode(const QString &id,
                          const GraphNode &node,
                          const bool &is_memo = false);
        /********** Getters **********/
        const QString &GetId() const;
        const QString &GetParent() const;
        const bool &IsMemo() const;
        const QString &GetName() const;
        const bool &IsShown() const;
        const GraphNode &GetNode() const;
        const QString &GetText() const;
        const QString &GetLabel() const;
        const QString &GetPath() const;
        const QString &GetEdgeLabel() const;
        const int &GetOffset() const;
        const int &GetLineNumber() const;
        const int &GetStartPosition() const;
        const int &GetEndPosition() const;
        const int &GetX() const;
        const int &GetY() const;
        const bool &IsQueryEnabled() const;
        /********** Getters **********/

        /********** Setters **********/
        void SetParent(const QString &parent);
        void SetName(const QString &name);
        void SetShown(const bool &shown);
        void SetText(const QString &text);
        void SetLabel(const QString &label);
        void SetEdgeLabel(const QString &edge_label);
        void SetPath(const QString &file_path);
        void SetOffset(const int &offset);
        void SetLineNumber(const int &line_number);
        void SetStartPosition(const int &start);
        void SetEndPosition(const int &end);
        void SetX(const int &x);
        void SetY(const int &y);
        void SetQueryEnabled(const bool &enabled);
        /********** Setters **********/
};

#endif // INVESTIGATIONNODE_H
