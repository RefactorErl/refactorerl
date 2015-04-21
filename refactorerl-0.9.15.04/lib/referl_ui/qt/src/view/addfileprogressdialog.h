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

#ifndef ADDFILEPROGRESSDIALOG_H
#define ADDFILEPROGRESSDIALOG_H

#include <QDialog>

namespace Ui {
class AddFileProgressDialog;
}

//The dialog is used to display the progress of file adding
class AddFileProgressDialog : public QDialog
{
    Q_OBJECT

    public:
        explicit AddFileProgressDialog(QWidget *parent = 0);
        ~AddFileProgressDialog();

    private:
        Ui::AddFileProgressDialog *ui;

    public slots:
        void setProgress(const QString &path,
                         const int &percent,
                         const double &speed);
        void Completed(const int &percent);
};

#endif // ADDFILEPROGRESSDIALOG_H
