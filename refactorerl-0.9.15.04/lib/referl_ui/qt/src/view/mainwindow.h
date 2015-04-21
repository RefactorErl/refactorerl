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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "mdiwidget.h"

namespace Ui {
class MainWindow;
}

//Main window. Displays menus and the central MdiWidget
class MainWindow : public QMainWindow
{
    Q_OBJECT

    private:
        Ui::MainWindow *ui;
        MdiWidget *mdi_widget_;

    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();

    private slots:
        void AboutRefactorErl(); //RefactorErl info
        void ActionSubWindowsTriggered(); //Put UI widgets in sub windows
        void ActionTabsTriggered(); //Put UI widgets in tabs
        void ActionAllTriggered(const bool &checked);
        void ActionCirclesTriggered(const bool &checked);
        void ActionModuleTriggered();
        void ActionFunctionTriggered();
        void ActionModuleGroupTriggered();
        void ActionNewInvestigationTriggered();
};

#endif // MAINWINDOW_H
