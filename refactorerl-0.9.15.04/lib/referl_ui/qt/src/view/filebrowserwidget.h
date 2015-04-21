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

#ifndef FILEBROWSERWIDGET_H
#define FILEBROWSERWIDGET_H

#include <QWidget>
#include "model/referlmodel.h"

namespace Ui {
class FileBrowserWidget;
}

//Provides user interface for database management
class FileBrowserWidget : public QWidget
{
    Q_OBJECT

    private:
        Ui::FileBrowserWidget *ui;
        RefErlModel *ref_erl_model_;

    private:
        void UpdateFileInfo();

    public:
        explicit FileBrowserWidget(QWidget *parent = 0,
                                   RefErlModel *ref_erl_model = 0);
        ~FileBrowserWidget();

    private slots:
        void ResetDatabaseFinished(const bool &success);
        void AdjustTableSize(QModelIndex, QModelIndex);
        void SetButtons(const int &index);
        void ErrorInfFileWarning();
        void FileDoubleClicked(const QModelIndex &index);

    public slots:
        void ResetDatabaseButtonClicked();
        void AddAppBaseButtonClicked();
        void AddIncludeButtonClicked();
        void AddFileButtonClicked();
        void AddDirButtonClicked();
        void DropFileButtonClicked();
        void DeleteEnvButtonClicked();
        void SycDatabaseButtonClicked();

    signals:
        void ShowFileNeeded();

};

#endif // FILEBROWSERWIDGET_H
