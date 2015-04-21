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

#include "errorstandarditemmodel.h"

ErrorStandardItemModel::ErrorStandardItemModel(QObject *parent) :
    QStandardItemModel(parent)
{
}

int ErrorStandardItemModel::columnCount(const QModelIndex &parent) const
{
    return QStandardItemModel::columnCount(parent) - 1;
}

QVariant ErrorStandardItemModel::data(const QModelIndex &index, int role) const
{
    if( !index.isValid() ) {
        return QVariant(); //No data returned on an invalid index
    }

    if(index.column() == 3 && role == Qt::DisplayRole) {
        //In the 3rd column return the error positions in "From-To" format
        // to display
        int row = index.row();
        QString start_position =
            QStandardItemModel::data(this->index(row, 3), 
                                    Qt::DisplayRole).toString();
        QString end_position =
            QStandardItemModel::data(this->index(row, 4), 
                                    Qt::DisplayRole).toString();

        return QVariant( start_position + '-' + end_position );
    } else {
        return QStandardItemModel::data(index, role);
    }
}
