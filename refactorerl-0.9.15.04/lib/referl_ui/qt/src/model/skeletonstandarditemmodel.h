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

#ifndef SKELETONSTANDARITEMMODEL_H
#define SKELETONSTANDARITEMMODEL_H

#include <QStandardItemModel>

//The class stores a list of skeletons with their name,
//body, and owner. In order to hide the skeleton body
//the columnCount and data functions are redefined.
class SkeletonStandardItemModel : public QStandardItemModel
{
    Q_OBJECT

    public:
        explicit SkeletonStandardItemModel(QObject *parent = 0);

    protected:
        //Returns 2
        int columnCount(const QModelIndex &parent) const override;
        //On the index 1 returns the column at index 2
        QVariant data(const QModelIndex &index, int role) const override;

};

#endif // SKELETONSTANDARITEMMODEL_H
