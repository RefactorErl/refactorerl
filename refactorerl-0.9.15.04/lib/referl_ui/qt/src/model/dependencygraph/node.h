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

#ifndef NODE_H
#define NODE_H

#include "model/common.h"

//Represents a node in a dependency graph
class Node
{
    private:
        QString id_; //Unique id of the node given by RefactorErl
        double x_; //Horizontal position
        double y_; //Vertical position
        QString label_; //Label of the node
        QString info_; //RefactorErl graph node identifier as text
        DependencyLevel level_; //Level of the dependency node,
                                //Module, ModuleGroup or Function

    public:
        //Constructor
        Node(const QString &id,
             const double &x,
             const double &y,
             const QString &label,
             const QString &info,
             const DependencyLevel &level);

        //Return unique node id
        const QString &GetId() const;

        //Return horizontal position
        const double &GetX() const;

        //Return vertical position
        const double &GetY() const;

        //Set horizontal position
        void SetX(const double &x);

        //Set vertical position
        void SetY(const double &y);

        //Return node label
        const QString &GetLabel() const;

        //Return the graph node identifier as text
        const QString &GetInfo() const;

        //Return dependency level
        const DependencyLevel &Level() const;
};

#endif // NODE_H
