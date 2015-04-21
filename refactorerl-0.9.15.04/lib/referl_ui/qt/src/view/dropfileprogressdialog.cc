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

#include "dropfileprogressdialog.h"
#include "ui_dropfileprogressdialog.h"

DropFileProgressDialog::DropFileProgressDialog(QWidget *parent,
    const QStringList &path_list) :
    QDialog(parent),
    ui(new Ui::DropFileProgressDialog),
    path_list_(path_list)
{
    ui->setupUi(this);
    ui->fileProgressBar->setValue(0);
    ui->overallProgressBar->setValue(0);
}

DropFileProgressDialog::~DropFileProgressDialog()
{
    delete ui;
}

void DropFileProgressDialog::setProgress(const QString &path,
                                         const int &percent,
                                         const double &)
{
    ui->fileNameLabel->setText(path);
    ui->fileProgressBar->setValue(percent);

    int index = path_list_.indexOf(path);
    ui->overallProgressBar->setValue(
        percent * ( 1 / static_cast<double>(path_list_.size()) )
        +
        100 * ( static_cast<double>(index) / static_cast<double>(path_list_.size()) )
    );
}
