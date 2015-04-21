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

#ifndef ERRORSTANDARDITEMMODEL_H
#define ERRORSTANDARDITEMMODEL_H

#include <QStandardItemModel>

//The class stores the list of errors in the loaded files of RefactorErl.
// columnCount and data functions are redefined in order to display the errors'
// positions in a "From-To" format but still store the original values
class ErrorStandardItemModel : public QStandardItemModel
{
    Q_OBJECT

    public:
        explicit ErrorStandardItemModel(QObject *parent = 0);

    protected:
        //Redefined columnCount function. Returns <original> - 1
        int columnCount(const QModelIndex &parent) const override;
        //Redefined data function. In the 3rd column returns the error
        // positions in "From-To" format when it is to be displayed.
        QVariant data(const QModelIndex &index, int role) const override;
};

#endif // ERRORSTANDARDITEMMODEL_H
