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

#include "dotparser.h"
#include <QFile>
#include <QTextStream>
#include <QStringList>

DotParser::DotParser(const QString &path) : path_(path)
{
}

DependencyGraph *DotParser::Parse()
{
    QFile file(path_);
    DependencyGraph *graph = new DependencyGraph(0.0, 0.0);
    if(file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        QTextStream in(&file);
        QString line;
        QStringList line_items;
        bool ok;
        while( !in.atEnd() ) {
            line = in.readLine();
			if(line.right(1) == "<") continue;
            line_items = line.split(" ");
            QString type = line_items.at(0);
            if(type == "graph") {
                double w, h;
                w = line_items.at(2).toDouble(&ok);
                h = line_items.at(3).toDouble(&ok);
                graph->SetSize(w, h);
            } else if(type == "node") {
                QString id, temp, label, info;
                double x, y;
                id = line_items.at(1); //Id
                x = line_items.at(2).toDouble(&ok); //X Coordinate
                y = line_items.at(3).toDouble(&ok);; //Y Coordinate
                label = line_items.at(6); //Name
				if(label[0] == '"') {
					label.resize(label.size()-1);
					label = label.right(label.size()-1);
				}
                DependencyLevel level;
                if(id[0] == 'm') {
                    level = Module;
					info = "{'$gn',module," + id.right(id.length() - 6) + "}";
                } else if( id[0] == 'f') {
                    level = Function;
					info = "{'$gn',func," + id.right(id.length() - 4) + "}";
                } else {
                    level = Function;
					info = "{'$gn',root,0}";
                }
                graph->AddNode( new Node(id,
                                         x,
                                         y, label, info, level) );
            } else if(type == "edge") {
                QString temp1, temp2;
                temp1 = line_items.at(1);
                temp2 = line_items.at(2);
                graph->AddEdge(temp1, temp2);
            }
        }
        file.close();
    }

    return graph;
}
