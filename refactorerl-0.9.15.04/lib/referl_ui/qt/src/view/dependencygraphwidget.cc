// -*- coding: latin-1 -*-

// This file is part of RefErl.
//
// RefErl is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// RefErl is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RefErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
//
// The Original Code is RefErl.
//
// The Initial Developer of the Original Code is Eötvös Loránd University.
// Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
// are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
// and Ericsson Hungary. All Rights Reserved.

//Author: Mátyás Kuti

#include "dependencygraphwidget.h"
#include "ui_dependencygraphwidget.h"
#include <QFileDialog>
#include <QMessageBox>
#include <QGraphicsSvgItem>
#include <QGraphicsScene>
#include "model/dependencygraph/dotparser.h"
#include "dependencygraph/graphwidget.h"

DependencyGraphWidget::DependencyGraphWidget(QWidget *parent,
                                             RefErlModel *ref_erl_model) :
    QWidget(parent),
    ui(new Ui::DependencyGraphWidget),
    ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);

    dialog_ = new QProgressDialog(this);
    dialog_->setMaximum(0);
    dialog_->setWindowTitle("Generating dependency graph");

    connect( ui->drawSVGButton, SIGNAL( clicked() ),
        this, SLOT( DrawSVGButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( SVGSignal(QString) ),
        this, SLOT( ShowSVG(QString) ) );
    connect( ref_erl_model_, SIGNAL( GraphSignal(QString) ),
        this, SLOT( ShowGraph(QString) ) );
    connect( ref_erl_model_, SIGNAL( DrawGraphSignal(DependencyGraph*) ),
        this, SLOT( ShowGraph(DependencyGraph*) ) );
	  connect( ui->addLibButton, SIGNAL( clicked() ),
        this, SLOT( AddExcludedLibButtonClicked() ) );
    connect( ui->addExcludedButton, SIGNAL( clicked() ),
        this, SLOT( AddExcludedButtonClicked() ) );
    connect( ui->addLeavesButton, SIGNAL( clicked() ),
        this, SLOT( AddLeavesButtonClicked() ) );
	  connect( ui->addStartingButton, SIGNAL( clicked() ),
          this, SLOT( AddStartingButtonClicked() ) );
	  connect( ui->addConnectionButton, SIGNAL( clicked() ),
          this, SLOT( AddConnectionButtonClicked() ) );
	  connect( ui->addGroupsButton, SIGNAL( clicked() ),
          this, SLOT( AddGroupsButtonClicked() ) );
	  connect( ui->deleteLibButton, SIGNAL( clicked() ),
        this, SLOT( DeleteExcludedLibButtonClicked() ) );
    connect( ui->deleteExcludedButton, SIGNAL( clicked() ),
        this, SLOT( DeleteExcludedButtonClicked() ) );
    connect( ui->deleteLeavesButton, SIGNAL( clicked() ),
        this, SLOT( DeleteLeavesButtonClicked() ) );
	  connect( ui->deleteStartingButton, SIGNAL( clicked() ),
          this, SLOT( DeleteStartingButtonClicked() ) );
	  connect( ui->deleteConnectionButton, SIGNAL( clicked() ),
          this, SLOT( DeleteConnectionButtonClicked() ) );
	  connect( ui->deleteGroupsButton, SIGNAL( clicked() ),
        this, SLOT( DeleteGroupsButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( ErrorMessageSignal(QString) ),
        this, SLOT( ErrorHandler(QString) ) );
    connect( ui->drawButton, SIGNAL( clicked() ),
        this, SLOT( DrawButtonClicked() ) );
    connect( ui->drawNewButton, SIGNAL( clicked() ),
        this, SLOT( DrawGraphButtonClicked() ) );

    graph_widget_ = new GraphWidget(this);
    graph_widget_->setScene( new QGraphicsScene(graph_widget_) );
    graph_widget_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui->horizontalLayout->addWidget(graph_widget_);

    ui->levelComboBox->addItems( QStringList { "Module", "Module group", "Function" } );
    ui->typeComboBox->addItems( QStringList { "All", "Cycles" } );

    nodes_completer_ = new QCompleter(this);
    nodes_completer_->setModel(ref_erl_model_->ModuleModel());
    leaves_completer_ = new QCompleter(this);
    leaves_completer_->setModel(ref_erl_model_->ModuleModel());
    start_node_completer_ = new QCompleter(this);
    start_node_completer_->setModel(ref_erl_model_->ModuleModel());
	connection_node_completer_ = new QCompleter(this);
    connection_node_completer_->setModel(ref_erl_model_->ModuleModel());

    ui->nodesEdit->setCompleter(nodes_completer_);
    ui->leavesEdit->setCompleter(leaves_completer_);
    ui->startingEdit->setCompleter(start_node_completer_);
	ui->connectionEdit->setCompleter(connection_node_completer_);

	excluded_lib_model_ = new QStringListModel(this);
    excluded_nodes_model_ = new QStringListModel(this);
    excluded_leaves_model_ = new QStringListModel(this);
	starting_nodes_model_ = new QStringListModel(this);
	connection_nodes_model_ = new QStringListModel(this);
	groups_model_ = new QStringListModel(this);

	ui->excludedLibListView->setModel(excluded_lib_model_);
    ui->excludedListView->setModel(excluded_nodes_model_);
    ui->excludedLeavesListView->setModel(excluded_leaves_model_);
	ui->startingListView->setModel(starting_nodes_model_);
	ui->connectionListView->setModel(connection_nodes_model_);
	ui->groupsListView->setModel(groups_model_);
	ui->groupsGroupBox->hide();

    ref_erl_model_->GetModules();
    ref_erl_model_->GetFunctions();
    connect( ui->levelComboBox, SIGNAL( currentIndexChanged(QString) ),
        this, SLOT( LevelComboboxCurrentTextChanged(QString) ) );
}

DependencyGraphWidget::~DependencyGraphWidget()
{
    delete ui;
}

void DependencyGraphWidget::LevelComboboxCurrentTextChanged(const QString &current)
{
    if(current == "Module") {
        ui->excludedNodesGroupBox->setTitle("Excluded modules");
		ui->excludedLeavesGroupBox->setTitle("Excluded children of modules");
		ui->startingNodesGroupBox->setTitle("Starting modules");
		ui->connectionNodesGroupBox->setTitle("Connection modules");
        nodes_completer_->setModel(ref_erl_model_->ModuleModel());
        leaves_completer_->setModel(ref_erl_model_->ModuleModel());
        start_node_completer_->setModel(ref_erl_model_->ModuleModel());
		connection_node_completer_->setModel(ref_erl_model_->ModuleModel());
		excluded_lib_model_->setStringList(QStringList());
        excluded_nodes_model_->setStringList(QStringList());
        excluded_leaves_model_->setStringList(QStringList());
		starting_nodes_model_->setStringList(QStringList());
		connection_nodes_model_->setStringList(QStringList());
		ui->excludedLibGroupBox->show();
        ui->excludedLeavesGroupBox->show();
        ui->excludedNodesGroupBox->show();
		ui->startingNodesGroupBox->show();
		ui->connectionNodesGroupBox->show();
		ui->groupsGroupBox->hide();
        ui->excludeOtpCheckBox->show();
    } else if(current == "Function") {
        ui->excludedNodesGroupBox->setTitle("Excluded functions");
		ui->excludedLeavesGroupBox->setTitle("Excluded children of functions");
		ui->startingNodesGroupBox->setTitle("Starting functions");
		ui->connectionNodesGroupBox->setTitle("Connection functions");
        nodes_completer_->setModel(ref_erl_model_->FunctionModel());
        leaves_completer_->setModel(ref_erl_model_->FunctionModel());
        start_node_completer_->setModel(ref_erl_model_->FunctionModel());
		connection_node_completer_->setModel(ref_erl_model_->FunctionModel());
		excluded_lib_model_->setStringList(QStringList());
        excluded_nodes_model_->setStringList(QStringList());
        excluded_leaves_model_->setStringList(QStringList());
		starting_nodes_model_->setStringList(QStringList());
		connection_nodes_model_->setStringList(QStringList());
		ui->excludedLibGroupBox->show();
        ui->excludedLeavesGroupBox->show();
        ui->excludedNodesGroupBox->show();
		ui->startingNodesGroupBox->show();
		ui->connectionNodesGroupBox->show();
		ui->groupsGroupBox->hide();
        ui->excludeOtpCheckBox->show();
    } else {
		groups_model_->setStringList(QStringList());
		ui->excludedLibGroupBox->hide();
        ui->excludedLeavesGroupBox->hide();
        ui->excludedNodesGroupBox->hide();
		ui->startingNodesGroupBox->hide();
		ui->connectionNodesGroupBox->hide();
		ui->groupsGroupBox->show();
        ui->excludeOtpCheckBox->hide();
    }
	ui->libEdit->clear();
    ui->nodesEdit->clear();
    ui->leavesEdit->clear();
	ui->startingEdit->clear();
	ui->connectionEdit->clear();
	//ui->groupsEdit->clear();
}

void DependencyGraphWidget::DrawButtonClicked()
{
    dialog_->setLabelText("Generating .dot file");
    DrawGraph(QString(), GraphViz);
}

void DependencyGraphWidget::DrawGraphButtonClicked()
{
    dialog_->setLabelText("Drawing dependency graph");
    DrawGraph(QString(), BuiltIn);
}

void DependencyGraphWidget::DrawSVGButtonClicked()
{
    QString path;
    path = QFileDialog::getSaveFileName(this,
                                        "Choose the SVG path",
                                        QString(),
                                        "SVG file (*.svg)" );
    if( path.isEmpty() ) {
        return;
    }

    dialog_->setLabelText(QString("Generating %1").arg(path));

    DrawGraph(path, GraphVizSVG);

}

void DependencyGraphWidget::ShowGraph(const QString &path)
{
    dialog_->accept();
    DotParser d(path);
    DependencyGraph *g = d.Parse();
    graph_widget_->DrawGraph(g, 100);
    delete g;
	
}

void DependencyGraphWidget::ShowGraph(DependencyGraph *graph)
{
    graph_widget_->DrawGraph(graph, 3);
    dialog_->accept();
    delete graph;
}

void DependencyGraphWidget::DrawGraph(const QString &path,
                                      const DependencyDrawMethod &method)
{
    DependencyLevel level = Module;
    DependencyType type = All;

    QString level_text = ui->levelComboBox->currentText();
    if( level_text == "Module group" ) {
        level = ModuleGroup;
    } else if( level_text == "Module" ) {
        level = Module;
    } else {
        level = Function;
    }

    QString type_text = ui->typeComboBox->currentText();

    if( type_text == "All" ) {
        type = All;
    } else {
        type = Cycle;
    }

    ref_erl_model_->DrawGraph(
        path, level, type,
        ui->excludeOtpCheckBox->isChecked(),
        starting_nodes_model_->stringList(),
		    connection_nodes_model_->stringList(),
        excluded_nodes_model_->stringList(),
        excluded_leaves_model_->stringList(),
		    excluded_lib_model_->stringList(),
		    groups_model_->stringList(), method
    );

    dialog_->exec();
}

void DependencyGraphWidget::AddExcludedLibButtonClicked()
{
    QString current = ui->libEdit->text();
    if(current == "") return;
    QStringList current_list = excluded_lib_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    excluded_lib_model_->setStringList(current_list);
}

void DependencyGraphWidget::AddExcludedButtonClicked()
{
    QString current = ui->nodesEdit->text();
    if(current == "") return;
    QStringList current_list = excluded_nodes_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    excluded_nodes_model_->setStringList(current_list);
}

void DependencyGraphWidget::AddLeavesButtonClicked()
{
    QString current = ui->leavesEdit->text();
    if(current == "") return;
    QStringList current_list = excluded_leaves_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    excluded_leaves_model_->setStringList(current_list);

}

void DependencyGraphWidget::AddStartingButtonClicked()
{
    QString current = ui->startingEdit->text();
    if(current == "") return;
    QStringList current_list = starting_nodes_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    starting_nodes_model_->setStringList(current_list);

}

void DependencyGraphWidget::AddConnectionButtonClicked()
{
    QString current = ui->connectionEdit->text();
    if(current == "") return;
    QStringList current_list = connection_nodes_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    connection_nodes_model_->setStringList(current_list);

}

void DependencyGraphWidget::AddGroupsButtonClicked()
{
    QString current = ui->groupsEdit->text();
    if(current == "") return;
    QStringList current_list = groups_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    groups_model_->setStringList(current_list);

}

void DependencyGraphWidget::DeleteExcludedLibButtonClicked()
{
    QModelIndexList selected_list =
            ui->excludedLibListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        excluded_lib_model_->removeRow(row);
    }
}

void DependencyGraphWidget::DeleteExcludedButtonClicked()
{
    QModelIndexList selected_list =
            ui->excludedListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        excluded_nodes_model_->removeRow(row);
    }
}

void DependencyGraphWidget::DeleteLeavesButtonClicked()
{
    QModelIndexList selected_list =
            ui->excludedLeavesListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        excluded_leaves_model_->removeRow(row);
    }
}

void DependencyGraphWidget::DeleteStartingButtonClicked()
{
    QModelIndexList selected_list =
            ui->startingListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        starting_nodes_model_->removeRow(row);
    }
}

void DependencyGraphWidget::DeleteConnectionButtonClicked()
{
    QModelIndexList selected_list =
            ui->connectionListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        connection_nodes_model_->removeRow(row);
    }
}

void DependencyGraphWidget::DeleteGroupsButtonClicked()
{
    QModelIndexList selected_list =
            ui->groupsListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        groups_model_->removeRow(row);
    }
}

void DependencyGraphWidget::ShowSVG(const QString &path)
{
    dialog_->accept();
    QMessageBox::information(
        this, "SVG saved", QString("SVG was saved to %1").arg(path)
    );

    QGraphicsScene *sc = new QGraphicsScene(graph_widget_);
    graph_widget_->setScene(sc);
    sc->clear();
    QGraphicsSvgItem *svg_item = new QGraphicsSvgItem(path);
    svg_item->setZValue(0);
    sc->addItem(svg_item);
}

void DependencyGraphWidget::ErrorHandler(QString)
{
    dialog_->close();
}
