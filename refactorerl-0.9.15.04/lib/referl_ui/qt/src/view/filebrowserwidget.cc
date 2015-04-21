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
#include <QFileDialog>
#include <QInputDialog>
#include "filebrowserwidget.h"
#include "ui_filebrowserwidget.h"
#include "addfileprogressdialog.h"
#include "directoryprogressdialog.h"
#include "dropfileprogressdialog.h"

FileBrowserWidget::FileBrowserWidget(QWidget *parent, RefErlModel *ref_erl_model) :
    QWidget(parent),
    ui(new Ui::FileBrowserWidget),
    ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);

    //Button click event handlers
    connect( ui->resetDatabaseButton, SIGNAL( clicked() ),
        this, SLOT( ResetDatabaseButtonClicked() ) );
    connect( ui->addAppBaseButton, SIGNAL( clicked() ),
        this, SLOT( AddAppBaseButtonClicked() ) );
    connect( ui->addIncludeButton, SIGNAL( clicked() ),
        this, SLOT( AddIncludeButtonClicked() ) );
    connect( ui->addFileButton, SIGNAL( clicked() ),
        this, SLOT( AddFileButtonClicked() ) );
    connect( ui->addDirButton, SIGNAL( clicked() ),
        this, SLOT( AddDirButtonClicked() ) );
    connect( ui->dropFileButton, SIGNAL( clicked() ),
        this, SLOT( DropFileButtonClicked() ));
    connect( ui->deleteEnvButton, SIGNAL( clicked() ),
        this, SLOT( DeleteEnvButtonClicked() ) );
    connect( ui->syncDatabaseButton, SIGNAL( clicked() ),
        this, SLOT( SycDatabaseButtonClicked() ) );

    connect( ref_erl_model_, SIGNAL( ResetDatabaseSignal(bool) ),
        this, SLOT( ResetDatabaseFinished(bool) ) );
    connect( ref_erl_model_, SIGNAL( ErrorInFileSignal() ),
        this, SLOT( ErrorInfFileWarning() ) );

    //Other event handlers
    connect( ui->tabWidget, SIGNAL( currentChanged(int) ),
        this, SLOT( SetButtons(int) ) );
    connect( ui->filesTableView, SIGNAL( doubleClicked(QModelIndex) ),
        this, SLOT( FileDoubleClicked(QModelIndex) ) );

    ui->configTableView->setModel( ref_erl_model_->ConfigurationModel() );
    ui->filesTableView->setModel( ref_erl_model_->FileModel() );
    ui->errorsTableView->setModel( ref_erl_model_->ErrorModel() );

    //Data loading event handlers
    connect( ref_erl_model_->ErrorModel(), SIGNAL( dataChanged(QModelIndex,QModelIndex) ),
        this, SLOT(AdjustTableSize(QModelIndex,QModelIndex) ) );
    connect( ref_erl_model_->FileModel(), SIGNAL( dataChanged(QModelIndex,QModelIndex) ),
        this, SLOT(AdjustTableSize(QModelIndex,QModelIndex) ) );
    connect( ref_erl_model_->ConfigurationModel(), SIGNAL( dataChanged(QModelIndex,QModelIndex) ),
        this, SLOT(AdjustTableSize(QModelIndex,QModelIndex) ) );

    UpdateFileInfo();
}

FileBrowserWidget::~FileBrowserWidget()
{
    delete ui;
}

void FileBrowserWidget::UpdateFileInfo()
{
    ref_erl_model_->GetConfigurations();
    ref_erl_model_->GetFiles();
    ref_erl_model_->GetErrors();
    ref_erl_model_->GetModules();
    ref_erl_model_->GetFunctions();
    ref_erl_model_->GetDbHash();
}

void FileBrowserWidget::ResetDatabaseButtonClicked()
{
    QMessageBox messageBox;
    messageBox.setWindowTitle("Reset database");
    messageBox.setText("Are you sure you want to reset the database?");
    QAbstractButton *absoluteButton =
        messageBox.addButton("Yes, in absolute mode", QMessageBox::YesRole);
    QAbstractButton *relativeButton =
        messageBox.addButton("Yes, in relative mode", QMessageBox::YesRole);
    QAbstractButton *noButton =
        messageBox.addButton("No", QMessageBox::NoRole);
    messageBox.setIcon(QMessageBox::Question);
    messageBox.exec();

    if( messageBox.clickedButton() == absoluteButton ) {
        ref_erl_model_->ResetDatabase(PositioningMode::Absolute);

    } else if( messageBox.clickedButton() == relativeButton ) {
        ref_erl_model_->ResetDatabase(PositioningMode::Relative);

    } else if(messageBox.clickedButton() == noButton) {
        //Ignore
    }
    UpdateFileInfo();
}

void FileBrowserWidget::AddAppBaseButtonClicked()
{
    QString path;
    if(ref_erl_model_->IsClient()) {
        path = QInputDialog::getText(this, "Add appbase directory on server", "Appbase directory path on server");
    } else {
        path = QFileDialog::getExistingDirectory(this, "Choose appbase directory");
    }
    if (path.isEmpty()) {
        return;
    }
    ref_erl_model_->AddAppbase(path);
}

void FileBrowserWidget::AddIncludeButtonClicked()
{
    QString path;
    if(ref_erl_model_->IsClient()) {
        path = QInputDialog::getText(this, "Add include directory on server", "Include directory path on server");
    } else {
        path = QFileDialog::getExistingDirectory(this, "Choose include directory");
    }
    if (path.isEmpty()) {
        return;
    }
    ref_erl_model_->AddInclude(path);
}

void FileBrowserWidget::AddFileButtonClicked()
{
    QString path;
    if(ref_erl_model_->IsClient()) {
        path = QInputDialog::getText(this, "Add file on server", "File path on server");
    } else {
        path = QFileDialog::getOpenFileName(this, "Choose file to add");
    }
    if (path.isEmpty()) {
        return;
    }

    AddFileProgressDialog dialog(this);
    connect( ref_erl_model_, SIGNAL( FileProgressSignal(QString,int,double) ),
        &dialog, SLOT( setProgress(QString,int,double) ) );
    connect( ref_erl_model_, SIGNAL( CompletedSignal(int) ),
        &dialog, SLOT( Completed(int) ) );

    ref_erl_model_->AddPath(path);

    dialog.exec();
}

void FileBrowserWidget::AddDirButtonClicked()
{
    QString path;
    if(ref_erl_model_->IsClient()) {
        path = QInputDialog::getText(this, "Add directory on server", "Directory path on server");
    } else {
        path = QFileDialog::getExistingDirectory(this, "Choose directory to add");
    }
    if (path.isEmpty()) {
        return;
    }

    DirectoryProgressDialog dialog(this, path);
    connect( ref_erl_model_, SIGNAL( CompletedSignal(int) ),
        &dialog, SLOT( SetOverallProgress(int) ) );
    connect( ref_erl_model_, SIGNAL( FileProgressSignal(QString,int,double) ),
        &dialog, SLOT( SetProgress(QString,int,double) ) );

    ref_erl_model_->AddPath(path);

    dialog.exec();
}

void FileBrowserWidget::DropFileButtonClicked()
{
    QModelIndexList selected_list = ui->filesTableView->selectionModel()->selectedRows();

    QStringList path_list;

    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        QModelIndex path_index = ref_erl_model_->FileModel()->index(row, 1);
        path_list << ref_erl_model_->FileModel()->data(path_index).toString();
    }

    DropFileProgressDialog dialog(this, path_list);
    connect( ref_erl_model_, SIGNAL( FileProgressSignal(QString,int,double) ),
        &dialog, SLOT( setProgress(QString,int,double) ) );
    connect( ref_erl_model_, SIGNAL( DroppedSignal() ),
        &dialog, SLOT( accept() ) );

    ref_erl_model_->DropFiles(path_list);

    dialog.exec();
}

void FileBrowserWidget::DeleteEnvButtonClicked()
{
    QModelIndexList selected_list = ui->configTableView->selectionModel()->selectedRows();

    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();

        QModelIndex type_index = ref_erl_model_->ConfigurationModel()->index(row, 0);
        QString type_string =
                ref_erl_model_->ConfigurationModel()->data(type_index).toString();

        ConfigurationType type =
                type_string == "appbase" ?
                ConfigurationType::Appbase :
                ConfigurationType::Include;

        QModelIndex path_index = ref_erl_model_->ConfigurationModel()->index(row, 1);
        QString path =
                ref_erl_model_->ConfigurationModel()->data(path_index).toString();

        ref_erl_model_->DeleteEnv(type, path);
    }
}

void FileBrowserWidget::SycDatabaseButtonClicked()
{
    ref_erl_model_->SyncronizeDatabase();
}

void FileBrowserWidget::ResetDatabaseFinished(const bool &success)
{
    if(success) {
        QMessageBox::information(this, "Reset database", "Database was resetted");
    } else {
        QMessageBox::warning(this, "Reset database", "Database reset was denied");
    }
    UpdateFileInfo();
}

void FileBrowserWidget::AdjustTableSize(QModelIndex, QModelIndex)
{
    ui->errorsTableView->resizeColumnsToContents();
    ui->filesTableView->resizeColumnsToContents();
    ui->configTableView->resizeColumnsToContents();
}

void FileBrowserWidget::SetButtons(const int &index)
{
    switch(index) {
        case 0: {
            ui->deleteEnvButton->setEnabled(false);
            ui->dropFileButton->setEnabled(true);
            break;
        }
        case 1: {
            ui->deleteEnvButton->setEnabled(false);
            ui->dropFileButton->setEnabled(false);
            break;
        }
        case 2: {
            ui->deleteEnvButton->setEnabled(true);
            ui->dropFileButton->setEnabled(false);
            break;
        }
    }
}

void FileBrowserWidget::ErrorInfFileWarning()
{
    QMessageBox::warning(this, "Errors!",
        "There are some errors in the loaded files, check the Errors tab!");
}

void FileBrowserWidget::FileDoubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QModelIndex path_index = ref_erl_model_->FileModel()->index(row, 1);
    emit ShowFileNeeded();
    ref_erl_model_->CatFile(ref_erl_model_->FileModel()->data(path_index).toString());
}
