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

#include "investigationnode.h"

InvestigationNode::InvestigationNode(const QString &id,
                                     const GraphNode &node,
                                     const bool &is_memo) :
    id_(id), is_memo_(is_memo), node_(node)
{
}

const QString &InvestigationNode::GetId() const
{
    return id_;
}

const QString &InvestigationNode::GetParent() const
{
    return parent_;
}

const bool &InvestigationNode::IsMemo() const
{
    return is_memo_;
}

const QString &InvestigationNode::GetName() const
{
    return name_;
}

const bool &InvestigationNode::IsShown() const
{
    return is_shown_;
}

const GraphNode &InvestigationNode::GetNode() const
{
    return node_;
}

const QString &InvestigationNode::GetText() const
{
    return text_;
}

const QString &InvestigationNode::GetLabel() const
{
    return label_;
}

const QString &InvestigationNode::GetPath() const
{
    return file_path_;
}

const QString &InvestigationNode::GetEdgeLabel() const
{
    return edge_label_;
}

const int &InvestigationNode::GetOffset() const
{
    return offset_position_;
}

const int &InvestigationNode::GetLineNumber() const
{
    return line_number_;
}

const int &InvestigationNode::GetStartPosition() const
{
    return start_position_;
}

const int &InvestigationNode::GetEndPosition() const
{
    return end_position_;
}

const int &InvestigationNode::GetX() const
{
    return x_;
}

const int &InvestigationNode::GetY() const
{
    return y_;
}

const bool &InvestigationNode::IsQueryEnabled() const
{
    return query_enabled_;
}

void InvestigationNode::SetParent(const QString &parent)
{
    parent_ = parent;
}

void InvestigationNode::SetName(const QString &name)
{
    name_ = name;
}

void InvestigationNode::SetShown(const bool &shown)
{
    is_shown_ = shown;
}

void InvestigationNode::SetText(const QString &text)
{
    text_ = text;
}

void InvestigationNode::SetLabel(const QString &label)
{
    label_ = label;
}

void InvestigationNode::SetEdgeLabel(const QString &edge_label)
{
    edge_label_ = edge_label;
}

void InvestigationNode::SetPath(const QString &file_path)
{
    file_path_ = file_path;
}

void InvestigationNode::SetOffset(const int &offset)
{
    offset_position_ = offset;
}

void InvestigationNode::SetLineNumber(const int &line_number)
{
    line_number_ = line_number;
}

void InvestigationNode::SetStartPosition(const int &start)
{
    start_position_ = start;
}

void InvestigationNode::SetEndPosition(const int &end)
{
    end_position_ = end;
}

void InvestigationNode::SetX(const int &x)
{
    x_ = x;
}

void InvestigationNode::SetY(const int &y)
{
    y_ = y;
}

void InvestigationNode::SetQueryEnabled(const bool &enabled)
{
    query_enabled_ = enabled;
}
