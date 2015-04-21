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

#include "filelistwidget.h"
#include "ui_filelistwidget.h"

FileListWidget::FileListWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::FileListWidget)
{
    ui->setupUi(this);
    file_list_model_ = new QStringListModel(this);
    file_list_model_->setStringList(file_list_);
    ui->fileListView->setModel(file_list_model_);
    connect(ui->addButton, SIGNAL( clicked() ), this, SLOT( AddButtonClicked() ) );
    connect(ui->removeButton, SIGNAL( clicked() ), this, SLOT( RemoveButtonClicked() ) );
    file_completer_ = new QCompleter(this);
}

FileListWidget::~FileListWidget()
{
    delete ui;
}

void FileListWidget::AddButtonClicked()
{
    QString file_name = ui->fileEdit->text();
    if(!file_name.isEmpty()) {
        file_list_ << file_name;
        file_list_model_->setStringList(file_list_);
    }
}

void FileListWidget::RemoveButtonClicked()
{
    QModelIndex file_index = ui->fileListView->currentIndex();
    if( !file_index.isValid() ) {
        return;
    } else {
        int row = file_index.row();
        file_list_.removeAt(row);
        file_list_model_->setStringList(file_list_);
    }
}

const QStringList &FileListWidget::Value() const
{
    return file_list_;
}

void FileListWidget::SetCompleterModel(QStringListModel *model)
{
    file_completer_->setModel(model);
    ui->fileEdit->setCompleter(file_completer_);
}
