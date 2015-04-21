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

#ifndef PARAMETER_H
#define PARAMETER_H

#include <QString>
#include <QVariant>

class Parameter
{
    private:
        QVariant value_;
        QString key_;
        QString label_;
        QString type_;
        bool is_atom_;
        bool is_enum_;

    public:
        Parameter(QVariant value, QString key,
                  QString label, QString type,
                  bool is_atom = false,
                  bool is_enum = false);
        const QString &GetKey() const;
        const QString &GetLabel() const;
        const QVariant &GetValue() const;
        const bool &GetIsAtom() const;
        const bool &GetIsEnum() const;
        const QString &GetType() const;
        void SetValue(const QVariant &value);
};

#endif // PARAMETER_H
