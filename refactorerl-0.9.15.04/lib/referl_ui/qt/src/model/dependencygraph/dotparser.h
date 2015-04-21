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

#ifndef DOTPARSER_H
#define DOTPARSER_H

#include "dependencygraph.h"
#include "model/common.h"

//Parses a plain-text dot file, representing a dependency graph.
//  It's the users responsibility that the
//  dot file is syntactically correct
class DotParser
{
    private:
        QString path_; // File path
        double pt_to_px_ = 10.0; //Point to pixel ratio

    public:
        //Constructor
        DotParser(const QString &path);

        //Parser function. Parses the dot file
        //  and returns a DependencyGraph pointer.
        //  It's the user's responsibility to free the graph
        //  after usage
        DependencyGraph *Parse();
};

#endif // DOTPARSER_H
