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

#include <QMessageBox>
#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "model/common.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this); //Set up UI

    mdi_widget_ = new MdiWidget(this);
    setCentralWidget(mdi_widget_);

    setWindowTitle(
        QString("RefactorErl %1").arg(
            mdi_widget_->GetUserName()
        )
    );

    /*****************************Event handlers*****************************/
    //Help menu
    connect( ui->actionQt, SIGNAL( triggered() ), qApp, SLOT( aboutQt() ) );
    connect( ui->actionRefactorErl, SIGNAL( triggered() ),
        this, SLOT( AboutRefactorErl()) );
    //File menu
    connect( ui->actionQuit, SIGNAL( triggered() ), qApp, SLOT( quit() ) );
    //View menu - open subwindows
    connect( ui->actionFile_Browser, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ShowFileBrowser() ) );
    connect( ui->actionQueries, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ShowQueries() ) );
    connect( ui->actionDependency_Graph, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ShowDependencyGraph() ) );
    connect( ui->actionDuplicated_Code, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ShowDuplicatedCode() ) );
    connect( ui->actionInvestigations, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ShowInvestigations() ) );
    //View menu - order subwindows
    connect( ui->actionSub_Windows, SIGNAL( triggered() ),
        this, SLOT( ActionSubWindowsTriggered() ) );
    connect( ui->actionTabs, SIGNAL( triggered() ),
        this, SLOT( ActionTabsTriggered() ) );
    connect( ui->actionTile_Windows, SIGNAL( triggered() ),
        mdi_widget_, SLOT( tileSubWindows() ) );
    connect( ui->actionCascade_Windows, SIGNAL( triggered() ),
        mdi_widget_, SLOT( cascadeSubWindows() ) );
    //File menu
    connect( ui->actionFile, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionFileTriggered() ));
    connect( ui->actionAppbase, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionAppbaseTriggered() ));
    connect( ui->actionInclude, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionIncludeTriggered() ));
    connect( ui->actionDirectory, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionDirectoryTriggered() ));
    connect( ui->actionSync, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionSyncTriggered() ));
    connect( ui->actionDrop, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionDropTriggered() ));
    connect( ui->actionReset_database, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionResetDatabaseTriggered() ));
    //Queries menu
    connect( ui->actionQuery_to_run, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionQueryToRunTriggered() ) );
    connect( ui->actionSkeleton, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionSkeletonTriggered() ) );
    connect( ui->actionLast_results, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionLastResultsTriggered() ) );
    connect( ui->actionRunning_queries, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ActionRunningQueriesTriggered() ) );
    //Dependency graph menu
    connect( ui->actionAll, SIGNAL( triggered(bool) ),
        this, SLOT( ActionAllTriggered(bool) ) );
    connect( ui->actionCircles, SIGNAL( triggered(bool) ),
        this, SLOT( ActionCirclesTriggered(bool) ) );
    connect( ui->actionModule, SIGNAL( triggered() ),
        this, SLOT( ActionModuleTriggered() ) );
    connect( ui->actionFunction, SIGNAL( triggered() ),
        this, SLOT( ActionFunctionTriggered() ) );
    connect( ui->actionModule_group, SIGNAL( triggered() ),
        this, SLOT( ActionModuleGroupTriggered() ) );
    //Duplicated code menu
    connect( ui->actionNew_duplicated_code_analysis, SIGNAL( triggered() ),
        mdi_widget_, SLOT( ShowDuplicatedCode() ) );
    //Investigation menu
    connect( ui->actionNew_investigation, SIGNAL( triggered() ),
        this, SLOT( ActionNewInvestigationTriggered() ) );
    /*****************************Event handlers*****************************/

    showMaximized();

}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::AboutRefactorErl()
{
    QMessageBox::about(
        this, "About RefactorErl",
        trUtf8("RefactorErl is an open-source static source code analyser and transformer tool for ​Erlang, developed by the Department of Programming Languages and Compilers at the Faculty of Informatics, ​Eötvös Loránd University, ​Budapest, ​Hungary.")
    );
}

void MainWindow::ActionSubWindowsTriggered()
{
    mdi_widget_->setViewMode(QMdiArea::SubWindowView);
    ui->actionSub_Windows->setEnabled(false);
    ui->actionTabs->setEnabled(true);
    ui->actionTile_Windows->setEnabled(true);
    ui->actionCascade_Windows->setEnabled(true);
}

void MainWindow::ActionTabsTriggered()
{
    mdi_widget_->setViewMode(QMdiArea::TabbedView);
    ui->actionTabs->setEnabled(false);
    ui->actionSub_Windows->setEnabled(true);
    ui->actionTile_Windows->setEnabled(false);
    ui->actionCascade_Windows->setEnabled(false);
}

void MainWindow::ActionAllTriggered(const bool &checked)
{
    ui->actionCircles->setChecked(!checked);
}

void MainWindow::ActionCirclesTriggered(const bool &checked)
{
    ui->actionAll->setChecked(!checked);
}

void MainWindow::ActionModuleTriggered()
{
    mdi_widget_->ActionModuleTriggered(ui->actionAll->isChecked() ? All : Cycle );
}

void MainWindow::ActionFunctionTriggered()
{
    mdi_widget_->ActionFunctionTriggered(ui->actionAll->isChecked() ? All : Cycle );
}

void MainWindow::ActionModuleGroupTriggered()
{
    mdi_widget_->ActionModuleGroupTriggered(ui->actionAll->isChecked() ? All : Cycle );
}

void MainWindow::ActionNewInvestigationTriggered()
{
    mdi_widget_->ShowInvestigations();
}
