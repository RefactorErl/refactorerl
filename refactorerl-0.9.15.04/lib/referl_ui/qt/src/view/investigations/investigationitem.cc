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

#include "investigationitem.h"
#include "ui_investigationitem.h"
#include <QMessageBox>
#include <QInputDialog>

InvestigationItem::InvestigationItem(InvestigationNode *node,
                                     RefErlModel *ref_erl_model) :
    ui(new Ui::InvestigationItem), ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);
    node_ = node;
    code_browser_ = new CodeBrowser(this);
    ui->verticalLayout->addWidget(code_browser_);
    code_browser_->setPlainText(node_->GetText());
    code_browser_->SetLineNumberAreaVisible(false);
    if(!node_->IsMemo()) {
        code_browser_->setReadOnly(true);
    } else { //Memo specialities
        code_browser_->DisableHighlighter();
        connect( code_browser_, SIGNAL( textChanged() ),
            this, SLOT( TextChanged() ) );
        ui->addMemoButton->hide();
        ui->hideButton->hide();
        ui->newButton->hide();
        ui->moveToNewButton->hide();
    }
    if(!node_->IsShown()) {
        code_browser_->hide();
        ui->labelLabel->show();
        ui->hideButton->setText("Show");
    } else {
        ui->labelLabel->hide();
    }

    ui->nameLabel->setText(node_->GetName());
    ui->labelLabel->setText(node_->GetLabel());

    ui->predefComboBox->setModel(ref_erl_model_->PredefQueriesModel());
    ui->predefComboBox->setModelColumn(0);

    connect( code_browser_, SIGNAL( cursorPositionChanged() ),
        this, SLOT( CursorChanged() ) );
    connect( ui->nameButton, SIGNAL( clicked() ),
        this, SLOT( NameButtonClicked() ) );
    connect( ui->addMemoButton, SIGNAL( clicked() ),
        this, SLOT( AddMemoButtonClicked() ) );
    connect( ui->predefComboBox, SIGNAL( currentIndexChanged(int) ),
        this, SLOT( PredefComboBoxCurrentIndexChanged(int) ) );
    connect( ui->runQueryButton, SIGNAL( clicked() ),
        this, SLOT( RunQueryButtonClicked() ) );
    connect( ui->hideButton, SIGNAL( clicked() ),
        this, SLOT( HideButtonClicked() ) );
    connect( ui->deleteButton, SIGNAL( clicked() ),
        this, SLOT( DeleteButtonClicked() ) );
    connect( ui->newButton, SIGNAL( clicked() ),
        this, SLOT( NewButtonClicked() ) );
    connect( ui->moveToNewButton, SIGNAL( clicked() ),
        this, SLOT( MoveToNewButtonClicked() ) );

    SetQueriesVisible(false);
}

InvestigationItem::~InvestigationItem()
{
    delete ui;
}

InvestigationNode *InvestigationItem::Node()
{
    return node_;
}

void InvestigationItem::CursorChanged()
{
    if(node_->IsMemo() || !node_->IsQueryEnabled()) return;
    int position = code_browser_->textCursor().position() + node_->GetOffset();
    ui->positionLabel->setText( QString::number(position) );
    ref_erl_model_->GetPredefQueries(node_->GetPath(), position);
    SetQueriesVisible(true);
    ui->queryEdit->setFocus();
}

void InvestigationItem::NameButtonClicked()
{
    QString new_name = QInputDialog::getText(this,
                                             "Node name",
                                             "New name",
                                             QLineEdit::Normal, node_->GetName());
    if(new_name.isEmpty()) return;
    node_->SetName(new_name);
    emit ChangedSignal();
    ui->nameLabel->setText(node_->GetName());
}

void InvestigationItem::AddMemoButtonClicked()
{
    ref_erl_model_->InvestigationMemo(node_->GetId());
    emit ChangedSignal();
}

void InvestigationItem::HideButtonClicked()
{
    if(node_->IsShown()) {
        code_browser_->hide();
        ui->labelLabel->show();
        ui->hideButton->setText("Show");
        node_->SetShown(false);
    } else {
        code_browser_->show();
        ui->labelLabel->hide();
        ui->hideButton->setText("Hide");
        node_->SetShown(true);
    }
    emit ChangedSignal();
}

void InvestigationItem::DeleteButtonClicked()
{
    emit DeleteSignal(node_->GetId());
}

void InvestigationItem::NewButtonClicked()
{
    emit NewSignal(node_->GetId());
}

void InvestigationItem::MoveToNewButtonClicked()
{
    emit MoveToNewSignal(node_->GetId());
}

void InvestigationItem::SetQueriesVisible(const bool &visible)
{
    if(visible) {
        ui->queryEdit->show();
        ui->runQueryLabel->show();
        ui->runQueryButton->show();
        ui->predefLabel->show();
        ui->predefComboBox->show();
        ui->positionLabel->show();
    } else {
        ui->queryEdit->hide();
        ui->runQueryLabel->hide();
        ui->runQueryButton->hide();
        ui->predefLabel->hide();
        ui->predefComboBox->hide();
        ui->positionLabel->hide();
    }
}

void InvestigationItem::PredefComboBoxCurrentIndexChanged(int index)
{
    QModelIndex query_index = ref_erl_model_->PredefQueriesModel()->index(index, 1);
    QString query  = ref_erl_model_->PredefQueriesModel()->data(query_index).toString();
    ui->queryEdit->setText( query );
}

void InvestigationItem::RunQueryButtonClicked()
{
    int position = code_browser_->textCursor().position() + node_->GetOffset();
    if(ui->queryEdit->text().isEmpty()) {
        QMessageBox::warning(this, "Warning", "No query was given!");
        return;
    } else {
        ref_erl_model_->InvestigationQuery(node_->GetId(),
                                           ui->queryEdit->text(),
                                           node_->GetPath(), position);
    }
}

void InvestigationItem::TextChanged()
{
    node_->SetText(code_browser_->toPlainText());
    emit ChangedSignal();
}
