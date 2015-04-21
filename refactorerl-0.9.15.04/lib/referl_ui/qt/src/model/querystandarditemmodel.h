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

#ifndef QUERYSTANDARDITEMMODEL_H
#define QUERYSTANDARDITEMMODEL_H

#include <QStandardItemModel>

//The class stores the result of the semantic query.
//The data and columnCount functions are redefined
//in order to hide the file information about the sub-results
//of the query
class QueryStandardItemModel : public QStandardItemModel
{
    Q_OBJECT

    public:
        explicit QueryStandardItemModel(QObject *parent = 0);

    protected:
        //Returns 1.
        int columnCount(const QModelIndex &) const override;
        //Returns data only on the valid indexes (column 0)
        QVariant data(const QModelIndex &index, int role) const override;
};

#endif // QUERYSTANDARDITEMMODEL_H
