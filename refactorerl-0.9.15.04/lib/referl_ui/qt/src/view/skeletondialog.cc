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

#include "skeletondialog.h"
#include "ui_skeletondialog.h"
#include <QMessageBox>

SkeletonDialog::SkeletonDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::SkeletonDialog)
{
    ui->setupUi(this);
    connect( ui->saveButton, SIGNAL( clicked() ),
        this, SLOT( SaveButton_clicked() ) );
    connect( ui->cancelButton, SIGNAL( clicked() ),
        this, SLOT( CancelButton_clicked() ) );
}

SkeletonDialog::~SkeletonDialog()
{
    delete ui;
}

void SkeletonDialog::SetComment(const QString &comment)
{
    comment_ = comment;
    ui->commentEdit->setText(comment_);
}

void SkeletonDialog::SetName(const QString &name)
{
    name_ = name;
    ui->nameEdit->setText(name_);
}

void SkeletonDialog::SetNameEnabled(const bool &enabled)
{
    ui->nameEdit->setEnabled(enabled);
}

void SkeletonDialog::SetSkeleton(const QString &skeleton)
{
    skeleton_ = skeleton;
    ui->skeletonEdit->setText(skeleton_);
}

const QString &SkeletonDialog::GetComment()
{
    return comment_;
}

const QString &SkeletonDialog::GetName()
{
    return name_;
}

const QString &SkeletonDialog::GetSkeleton()
{
    return skeleton_;
}

bool SkeletonDialog::Accepted()
{
    return accepted_;
}

void SkeletonDialog::SaveButton_clicked()
{
    SetName(ui->nameEdit->text());
    SetSkeleton(ui->skeletonEdit->text());
    SetComment(ui->commentEdit->toPlainText());
    if(!name_.isEmpty() && !skeleton_.isEmpty()) {
        accepted_ = true;
        accept();
    } else {
        QMessageBox::warning(this, "Warning!",
                             "You must give a name and a skeleton body");
    }
}

void SkeletonDialog::CancelButton_clicked()
{
    reject();
}
