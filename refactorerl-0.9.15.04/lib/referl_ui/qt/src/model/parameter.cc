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

#include "parameter.h"

Parameter::Parameter(QVariant value,
                     QString key,
                     QString label, QString type,
                     bool is_atom,
                     bool is_enum) :
    value_(value), key_(key), label_(label), type_(type),
    is_atom_(is_atom), is_enum_(is_enum)
{
}

const QString &Parameter::GetKey() const
{
    return key_;
}

const QString &Parameter::GetLabel() const
{
    return label_;
}

const QVariant &Parameter::GetValue() const
{
    return value_;
}

const bool &Parameter::GetIsAtom() const
{
    return is_atom_;
}

const bool &Parameter::GetIsEnum() const
{
    return is_enum_;
}

const QString &Parameter::GetType() const
{
    return type_;
}

void Parameter::SetValue(const QVariant &value)
{
    value_ = value;
}
