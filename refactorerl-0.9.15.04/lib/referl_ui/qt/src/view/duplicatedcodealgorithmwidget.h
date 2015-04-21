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

#ifndef DUPLICATEDCODEALGORITHMWIDGET_H
#define DUPLICATEDCODEALGORITHMWIDGET_H

#include <QWidget>
#include <QList>
#include <QString>
#include <QStringList>
#include <QLabel>

#include "model/parameter.h"
#include "model/referlmodel.h"

namespace Ui {
class DuplicatedCodeAlgorithmWidget;
}

class DuplicatedCodeAlgorithmWidget : public QWidget
{
    Q_OBJECT

    private:
        QString key_;
        QString label_;

        QList<QWidget*> elements_;
        QList<Parameter> parameters_;

        RefErlModel *ref_erl_model_;

        Ui::DuplicatedCodeAlgorithmWidget *ui;

    public:
        explicit DuplicatedCodeAlgorithmWidget(QWidget *parent = 0,
                                               QString key = "",
                                               QString label = "",
                                               RefErlModel *ref_erl_model = 0);
        ~DuplicatedCodeAlgorithmWidget();
        void AddElem(Parameter param);
        const QString &GetKey() const;
        const QString &GetLabel() const;
        const QList<Parameter> &GetParameters();

};

#endif // DUPLICATEDCODEALGORITHMWIDGET_H
