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

#include "investigationswidget.h"
#include "ui_investigationswidget.h"
#include <QInputDialog>
#include <QMessageBox>

InvestigationsWidget::InvestigationsWidget(QWidget *parent, RefErlModel *ref_erl_model) :
    QWidget(parent),
    ui(new Ui::InvestigationsWidget),
    ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);

    latest_info_ = new QStandardItemModel(this);
    latest_info_->setHorizontalHeaderLabels( QStringList { "Text", "File", "Line number" } );
    ui->latestResultTable->setModel(latest_info_);

    graph_widget_ = new InvestigationsGraphWidget(this, ref_erl_model_);
    ui->horizontalLayout->addWidget(graph_widget_);
    ui->investigatonsTable->setModel(ref_erl_model_->InvestigationsModel());
    ref_erl_model_->GetInvestigations();

    completer_ = new QCompleter(this);
    completer_->setModel( ref_erl_model_->FunctionModel() );
    //completer_->setWidget( ui->startingEdit );
    ui->startingEdit->setCompleter(completer_);

    connect( ref_erl_model_, SIGNAL( LoadInvestigationSignal(InvestigationGraph) ),
        this, SLOT( LoadInvestigation(InvestigationGraph) ) );

    connect( ref_erl_model_, SIGNAL( DeleteInvestigationSignal(QString) ),
        this, SLOT( DeleteInvestigation(QString) ) );

    connect( ref_erl_model_, SIGNAL( SaveInvestigationSignal(QString) ),
        this, SLOT( SaveInvestigation(QString) ) );

    connect( ref_erl_model_, SIGNAL( ShareInvestigationSignal(QString, QString) ),
        this, SLOT( ShareInvestigation(QString, QString) ) );

    connect( ui->loadButton, SIGNAL( clicked() ),
        this, SLOT( LoadButtonClicked() ) );
    connect( ui->deleteButton, SIGNAL( clicked() ),
        this, SLOT( DeleteButtonClicked() ) );
    connect( ui->shareButton, SIGNAL( clicked() ),
        this, SLOT( ShareButtonClicked() ) );
    connect( ui->runButton, SIGNAL( clicked() ),
        this, SLOT( RunButtonClicked() ) );
    connect( ui->addButton, SIGNAL( clicked() ),
        this, SLOT( AddButtonClicked() ) );

    connect( ui->saveButton, SIGNAL( clicked() ),
        graph_widget_, SLOT( Save() ) );
    connect( ui->saveAsButton, SIGNAL( clicked() ),
        graph_widget_, SLOT( SaveAs() ) );

    connect( ref_erl_model_, SIGNAL( InvestigationQuerySignal(InvestigationNodeList) ),
        this, SLOT( InvestigationQuery(InvestigationNodeList) ) );

    connect( graph_widget_, SIGNAL( NewName(QString) ),
        this, SLOT( SetName(QString) ) );
    connect( graph_widget_, SIGNAL( ChangedSignal() ),
        this, SLOT( Changed() ) );

    connect( graph_widget_, SIGNAL( NoSourceNode() ),
        this, SLOT( NoSourceNode() ) );
}

InvestigationsWidget::~InvestigationsWidget()
{
    delete ui;
}

void InvestigationsWidget::SetName(const QString &name)
{
    ui->investigatonNameLabel->setText(name);
}

void InvestigationsWidget::Changed()
{
    QString name = graph_widget_->GetGraphName().isEmpty() ?
                    QString("Unsaved") :
                    graph_widget_->GetGraphName();
    ui->investigatonNameLabel->setText(name + "*");
}

void InvestigationsWidget::LoadInvestigation(const InvestigationGraph &graph)
{
    QString name = graph_widget_->GetGraphName().isEmpty() ?
                    QString("Unsaved") :
                    graph_widget_->GetGraphName();
    ui->investigatonNameLabel->setText(name);
    latest_info_->clear();
    latest_results_.clear();
    graph_widget_->DrawInvestigationGraph(graph);
}

void InvestigationsWidget::LoadButtonClicked()
{
    QModelIndex current_index =  ui->investigatonsTable->currentIndex();
    if(!current_index.isValid()) {
        return;
    }
    int row = current_index.row();
    QModelIndex inv_name_index = ref_erl_model_->InvestigationsModel()->index(row, 0);
    QString inv_name = ref_erl_model_->InvestigationsModel()->data(inv_name_index).toString();
    ref_erl_model_->LoadInvestigation(inv_name);
}

void InvestigationsWidget::DeleteButtonClicked()
{
    QModelIndex current_index =  ui->investigatonsTable->currentIndex();
    if(!current_index.isValid()) {
        return;
    }
    int row = current_index.row();
    QModelIndex inv_name_index = ref_erl_model_->InvestigationsModel()->index(row, 0);
    QString inv_name = ref_erl_model_->InvestigationsModel()->data(inv_name_index).toString();
    if( QMessageBox::question(this, "Delete investigation",
                              QString("Are you sure you want to delete investigation %1?").arg(inv_name),
                              QMessageBox::Yes|QMessageBox::No) == QMessageBox::No ) {
        return;
    }
    ref_erl_model_->DeleteInvestigation(inv_name);
}

void InvestigationsWidget::ShareButtonClicked()
{
    QModelIndex current_index =  ui->investigatonsTable->currentIndex();
    if(!current_index.isValid()) {
        return;
    }
    int row = current_index.row();
    QModelIndex inv_name_index = ref_erl_model_->InvestigationsModel()->index(row, 0);
    QString inv_name = ref_erl_model_->InvestigationsModel()->data(inv_name_index).toString();
    QString user = QInputDialog::getText(this, "Add a user to share investigation", "User");
    if(user.isEmpty()) return;
    ref_erl_model_->ShareInvestigation(inv_name, user);
}

void InvestigationsWidget::RunButtonClicked()
{
    QString start_function = ui->startingEdit->text();
    if(start_function.isEmpty()) {
        return;
    } else if(graph_widget_->GetChanged()) {
        if( QMessageBox::question(this, "Save investigation",
                                 "The current investigation is not saved. Would you like to save it?",
                                 QMessageBox::Yes|QMessageBox::No) == QMessageBox::Yes ) {
            graph_widget_->Save();
        }
    } else {
        QStringList first = start_function.split(":");
        if(first.size() != 2) {
            QMessageBox::warning(this, "Warning", "The function format is wrong! Use M/F:A format!");
            return;
        }
        QString module = first.at(0);
        QStringList second = first.at(1).split("/");
        if(second.size() != 2) {
            QMessageBox::warning(this, "Warning", "The function format is wrong! Use M/F:A format!");
            return;
        }
        QString function = second.at(0);
        QString arity_string = second.at(1);
        bool ok;
        int arity = arity_string.toInt(&ok);
        if(!ok) {
            QMessageBox::warning(this, "Warning", "The arity given is not a number!");
            return;
        }
        ref_erl_model_->StartInvestigation(module, function, arity);
        graph_widget_->SetIsNew(true);
    }
}

void InvestigationsWidget::AddButtonClicked()
{
    QModelIndex current_index = ui->latestResultTable->currentIndex();
    if(!current_index.isValid()) {
        return;
    }
    int row = current_index.row();
    graph_widget_->AddNode( latest_results_[row] );
}

void InvestigationsWidget::InvestigationQuery(const InvestigationNodeList &nodes)
{
    latest_results_ = nodes;
    latest_info_->clear();
    latest_info_->setColumnCount(3);
    latest_info_->setRowCount(nodes.size());
    latest_info_->setHorizontalHeaderLabels( QStringList { "Text", "File", "Line number" } );
    int line_count = 0;
    for(InvestigationNode* node : latest_results_) {
        QStandardItem* text_item = new QStandardItem(node->GetLabel());
        QStandardItem* file_item = new QStandardItem(node->GetPath());
        QStandardItem* line_item = new QStandardItem( QString::number(node->GetLineNumber()) );
        latest_info_->setItem(line_count, 0, text_item);
        latest_info_->setItem(line_count, 1, file_item);
        latest_info_->setItem(line_count, 2, line_item);
        ++line_count;
    }
}

void InvestigationsWidget::DeleteInvestigation(const QString &name)
{
    QMessageBox::information(this, "Investigation deleted", QString("%1 was deleted").arg(name));
    ref_erl_model_->GetInvestigations();
}

void InvestigationsWidget::SaveInvestigation(const QString &name)
{
    QMessageBox::information(this, "Investigation saved", QString("%1 was saved").arg(name));
    ref_erl_model_->GetInvestigations();
    ui->investigatonNameLabel->setText(graph_widget_->GetGraphName());
}

void InvestigationsWidget::ShareInvestigation(const QString &name, const QString &user)
{
    QMessageBox::information(this, "Investigation shared", QString("%1 was shared with %2").arg(name, user));
}

void InvestigationsWidget::NoSourceNode()
{
    QMessageBox::warning(this, "Warning", "The source node is already deleted!");
}
