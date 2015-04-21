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

#ifndef QUERYRESULT_H
#define QUERYRESULT_H

#include "common.h"

//The class represents and stores a hierarchical (tree structure) RefactorErl
// semantic query result.
class QueryResult
{
    private:
        //Top level result with 
        // <<file path, start position, end position>  , <actual result>>
        QueryElem result_ = QueryElem(FilePosition("", -1, -1), "");
        //Children of the top level result in the hierarchy
        QList< QueryResult > children_;

    public:
        //Constructor
        QueryResult();

        //Add a new query result (also hierarchical) to the list of children
        void AddChild(const QueryResult &child);
        
        //Return the top level result
        const QueryElem &GetResult() const;
        
        //Set the top level result
        void SetResult(const QueryElem &result);
        
        //List children
        const QList< QueryResult > &GetChildren() const;
};

#endif // QUERYRESULT_H
