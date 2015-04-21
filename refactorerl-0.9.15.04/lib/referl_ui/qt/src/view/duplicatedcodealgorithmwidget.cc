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

#include "duplicatedcodealgorithmwidget.h"
#include "ui_duplicatedcodealgorithmwidget.h"

#include <QComboBox>
#include <QLineEdit>
#include <QListWidget>
#include <QSpinBox>
#include <QDoubleSpinBox>
#include <QCheckBox>
#include <QPushButton>

#include "filelistwidget.h"

DuplicatedCodeAlgorithmWidget::DuplicatedCodeAlgorithmWidget(QWidget *parent,
                                                             QString key,
                                                             QString label,
                                                             RefErlModel *ref_erl_model) :
    QWidget(parent), key_(key), label_(label), ref_erl_model_(ref_erl_model),
    ui(new Ui::DuplicatedCodeAlgorithmWidget)
{
    ui->setupUi(this);
    ui->nameLabel->setText(label);
}

DuplicatedCodeAlgorithmWidget::~DuplicatedCodeAlgorithmWidget()
{
    delete ui;
}

void DuplicatedCodeAlgorithmWidget::AddElem(Parameter param)
{
    bool ok;
    QVariant value = param.GetValue();
    if(param.GetType() == "boolean") {
        QCheckBox *box = new QCheckBox(this);
        box->setChecked(value.toBool());
        ui->formLayout->addRow(param.GetLabel(), box);
        elements_.push_back(box);
        parameters_.push_back(param);
        box->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    } else if(param.GetType() == "float") {
        QDoubleSpinBox *spin = new QDoubleSpinBox(this);
        spin->setValue(value.toDouble(&ok));
        ui->formLayout->addRow(param.GetLabel(), spin);
        elements_.push_back(spin);
        parameters_.push_back(param);
        spin->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    } else if(param.GetType() == "integer") {
        QSpinBox *spin = new QSpinBox(this);
        spin->setValue(value.toInt(&ok));
        ui->formLayout->addRow(param.GetLabel(), spin);
        elements_.push_back(spin);
        parameters_.push_back(param);
        spin->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    } else if(param.GetType() == "atom" || param.GetType() == "string") {
        QLineEdit *edit = new QLineEdit(this);
        edit->setText(value.toString());
        ui->formLayout->addRow(param.GetLabel(), edit);
        elements_.push_back(edit);
        parameters_.push_back(param);
        edit->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    } else if(param.GetType() == "enum") {
        QComboBox *combo = new QComboBox(this);
        combo->addItems(value.toStringList());
        ui->formLayout->addRow(param.GetLabel(), combo);
        elements_.push_back(combo);
        parameters_.push_back(param);
        combo->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    } else if(param.GetType() == "atoms") {
        FileListWidget *list = new FileListWidget(this);
        list->SetCompleterModel(ref_erl_model_->ModuleModel());
        elements_.push_back(list);
        ui->formLayout->addRow(list);
        parameters_.push_back(param);
        list->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    }
    adjustSize();
}

const QList<Parameter> &DuplicatedCodeAlgorithmWidget::GetParameters()
{
    for(int i = 0; i < parameters_.size(); ++i) {
        Parameter param = parameters_[i];
        if(param.GetType() == "boolean") {
            QCheckBox *box = dynamic_cast<QCheckBox *>(elements_[i]);
            parameters_[i].SetValue( QVariant(box->isChecked()) );
        } else if(param.GetType() == "float") {
            QDoubleSpinBox *spin = dynamic_cast<QDoubleSpinBox *>(elements_[i]);
            parameters_[i].SetValue( QVariant(spin->value()) );
        } else if(param.GetType() == "integer") {
            QSpinBox *spin = dynamic_cast<QSpinBox *>(elements_[i]);
            parameters_[i].SetValue( QVariant(spin->value()) );
        } else if(param.GetType() == "atom") {
            QLineEdit *edit = dynamic_cast<QLineEdit *>(elements_[i]);
            parameters_[i].SetValue( QVariant(edit->text()) );
        } else if(param.GetType() == "string") {
            QLineEdit *edit = dynamic_cast<QLineEdit *>(elements_[i]);
            parameters_[i].SetValue( QVariant(edit->text()) );
        } else if(param.GetType() == "enum") {
            QComboBox *combo = dynamic_cast<QComboBox *>(elements_[i]);
            parameters_[i].SetValue( QVariant(combo->currentText()) );
        } else if(param.GetType() == "atoms") {
            FileListWidget *list = dynamic_cast<FileListWidget *>(elements_[i]);
            parameters_[i].SetValue( QVariant(list->Value()) );
        }
    }

    return parameters_;
}
