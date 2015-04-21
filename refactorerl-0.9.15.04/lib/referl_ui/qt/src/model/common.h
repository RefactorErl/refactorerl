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

#ifndef COMMON_H
#define COMMON_H

#include <tuple>
#include <QVector>
#include <QPair>
#include <QList>

#include "parameter.h"

//Type definitions and enumerated types used by the application's other classes.
//The types defined here represent RefactorErl parameters or result data types.

//Database positioning mode
enum PositioningMode { Absolute, Relative };

//Environment type
enum ConfigurationType { Include, Appbase };
//Configuration - < Type (Include or Appbase), File Path >
typedef QPair<ConfigurationType, QString> Configuration;
//List of configurations
typedef QList< Configuration > ConfigurationList;

//Position in a file - <Start Pos, End Pos>
typedef QPair<int, int> Position;
//Path-Error pair - <Path of File, Error Message>
typedef QPair<QString, QString> PathError;
//Error - < Error Message and Path, Error Position >
typedef QPair<PathError, Position> Error;
//List of errors
typedef QList< Error > ErrorList;

//Dependency graph level
enum DependencyLevel { Module, ModuleGroup, Function};
//Dependency graph type
enum DependencyType { All, Cycle };
//Dependency drawing method
enum DependencyDrawMethod { GraphViz, GraphVizSVG, BuiltIn };

//Skeleton - < Name, Body, Owner, Comment>
typedef std::tuple<QString, QString, QString, QString> Skeleton;
//List of skeletons
typedef QList<Skeleton> SkeletonList;

//Query - <Query String, File Path, Position>
typedef std::tuple<QString, QString, int> Query;
//List of queries
typedef QList<Query> QueryList;
//Query result position - <File Path, Start Position, End Position>
typedef std::tuple<QString, int, int> FilePosition;
//Query result element - < Position in file, Text result >
typedef QPair<FilePosition, QString> QueryElem;
//Running query - < Uniqe Query Id, Query String >
typedef QPair<int, QString> QueryId;
//List of query ids
typedef QList<QueryId> QueryIdList;
//Predefined query - < Title, Query String >
typedef QPair<QString, QString> PredefQuery;

//Graph Node - < "'$gn'", Type, Number >
typedef std::tuple<QString, QString, int> GraphNode;
//Database hash
typedef std::tuple<int, int, int, int> DbHash;

//Duplicated code element
//<File Path, Start Position, End Position>
typedef std::tuple<QString, int, int> DupcodeElem;
//Duplicated Code Group - list of matched elems
typedef QVector<DupcodeElem> DupcodeGroup;

//Duplicated code algorithm
//<key, label, parameteters>
typedef std::tuple<QString, QString, QList<Parameter>> DuplicatedCodeAlgorithm;

//Investigation - < Name, List of Users >
typedef QPair<QString, QStringList> Investigation;
//List of investigations
typedef QList<Investigation> InvestigationList;

class InvestigationNode;
//Vector of investigation nodes
typedef QVector<InvestigationNode*> InvestigationNodeList;

#endif // COMMON_H
