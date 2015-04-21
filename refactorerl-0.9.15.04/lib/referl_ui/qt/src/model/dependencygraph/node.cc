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

#include "node.h"

Node::Node(const QString &id, const double &x,
           const double &y, const QString &label,
           const QString &info, const DependencyLevel &level) :
    id_(id), x_(x), y_(y), label_(label), info_(info), level_(level)
{
}

const QString &Node::GetId() const
{
    return id_;
}

const double &Node::GetX() const
{
    return x_;
}

void Node::SetX(const double &x)
{
    x_ = x;
}

const double &Node::GetY() const
{
    return y_;
}

void Node::SetY(const double &y)
{
    y_ = y;
}

const QString &Node::GetLabel() const
{
    return label_;
}

const QString &Node::GetInfo() const
{
    return info_;
}

const DependencyLevel &Node::Level() const
{
    return level_;
}
