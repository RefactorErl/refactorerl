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

#include "duplicatedcodewidget.h"
#include "ui_duplicatedcodewidget.h"

#include "duplicatedcodealgorithmwidget.h"

#include <QMessageBox>

DuplicatedCodeWidget::DuplicatedCodeWidget(QWidget *parent,
                                           RefErlModel *ref_erl_model) :
    QWidget(parent),
    ui(new Ui::DuplicatedCodeWidget),
    ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);

    left_browser_ = new CodeBrowserWidget(this, ref_erl_model_);
    right_browser_ = new CodeBrowserWidget(this, ref_erl_model_);

    names_completer_ = new QCompleter(this);
    names_completer_->setModel(ref_erl_model_->DupcodeResultsModel());
    ui->previousEdit->setCompleter(names_completer_);

    ui->horizontalLayout->addWidget(left_browser_);
    ui->horizontalLayout->addWidget(right_browser_);

    dialog_ = new QProgressDialog(this);
    dialog_->setMaximum(0);
    dialog_->setWindowTitle("Running duplicated code analysis.");

    connect( ui->runButton, SIGNAL( clicked() ),
        this, SLOT( RunButtonClicked() ) );
    connect( ui->loadPreviousButton, SIGNAL( clicked() ),
        this, SLOT( LoadButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( DupcodeSearchSignal(QString, QString, QVector<DupcodeGroup>) ),
        this, SLOT( DupcodeSearch(QString, QString, QVector<DupcodeGroup> ) ) );
    connect( ref_erl_model_, SIGNAL( CatFileSignal( QString, QString) ),
        this, SLOT( ShowFile(QString, QString) ) );
    connect( ui->groupComboBox, SIGNAL( currentIndexChanged(int) ),
        this, SLOT( GroupComboboxCurrentIndexChanged(int) ) );
    connect( ui->leftComboBox, SIGNAL( currentIndexChanged(int) ),
        this, SLOT( LeftComboboxCurrentIndexChanged(int) ) );
    connect( ui->rightComboBox, SIGNAL( currentIndexChanged(int) ),
        this, SLOT( RightComboboxCurrentIndexChanged(int) ) );

    connect( ref_erl_model_,
             SIGNAL( DupcodeAlgorithmsSignal() ),
             this,
             SLOT( DisplayAlgorithms() ) );

    ref_erl_model_->GetDupcodeResultNames();
    DisplayAlgorithms();
}

DuplicatedCodeWidget::~DuplicatedCodeWidget()
{
    delete ui;
}

void DuplicatedCodeWidget::DupcodeSelectedSearch(const QString &algorithm_key,
                                                 const QString &file_path,
                                                 const int &start,
                                                 const int &end)
{
    load_file_ = false;
    ui->nameLabel->setText(QString());
    ui->pathLabel->setText(QString());
    ui->groupComboBox->clear();
    ui->leftComboBox->clear();
    ui->rightComboBox->clear();

    ref_erl_model_->DuplicatedCodeSelectedSearch(algorithm_key,
                                                 file_path,
                                                 start,
                                                 end);
    dialog_->exec();
}

void DuplicatedCodeWidget::RunButtonClicked()
{
    load_file_ = false;
    ui->nameLabel->setText(QString());
    ui->pathLabel->setText(QString());
    ui->groupComboBox->clear();
    ui->leftComboBox->clear();
    ui->rightComboBox->clear();

    const int alg_index = ui->algorithmsWidget->currentIndex();

    QString alg_name = std::get<1>(ref_erl_model_->DuplicatedCodeAlgorithms()->at(alg_index));
    dialog_->setLabelText(QString("Duplicated code analysis using %1 algorithm").arg(alg_name));

    QString alg_key = std::get<0>(ref_erl_model_->DuplicatedCodeAlgorithms()->at(alg_index));

    DuplicatedCodeAlgorithmWidget *widget =
            dynamic_cast<DuplicatedCodeAlgorithmWidget *>(ui->algorithmsWidget->widget(alg_index));
    ref_erl_model_->DuplicatedCodeSearch(alg_key,
                                         widget->GetParameters());
    dialog_->exec();
}

void DuplicatedCodeWidget::LoadButtonClicked()
{
    QString name = ui->previousEdit->text();
    if(!name.isEmpty()) {
        ref_erl_model_->DuplicatedCodePreviousSearch(name);
    }
}

void DuplicatedCodeWidget::DupcodeSearch(const QString &name,
                                         const QString &path,
                                         const QVector<DupcodeGroup> &results)
{
    load_file_ = false;
    last_result_ = results;
    ui->groupComboBox->clear();
    ui->leftComboBox->clear();
    ui->rightComboBox->clear();
    ui->groupComboBox->addItem("");
    ui->leftComboBox->addItem("");
    ui->rightComboBox->addItem("");
    for(int i = 0; i < results.size(); ++i) {
        ui->groupComboBox->addItem( QString("Group %1").arg( QString::number(i+1) ) );
    }

    ui->groupComboBox->setCurrentIndex(0);
    ui->leftComboBox->setCurrentIndex(0);
    ui->rightComboBox->setCurrentIndex(0);
    load_file_ = true;
    QMessageBox::information(this,
                             "Duplicated code analysis",
                             "The latest duplicated code analysis was finished!");
    ui->nameLabel->setText(name);
    ui->pathLabel->setText(path);
    ref_erl_model_->GetDupcodeResultNames();
    dialog_->accept();
}

void DuplicatedCodeWidget::GroupComboboxCurrentIndexChanged(int index)
{
    if(index == 0 || !load_file_) {
        return;
    } else {
        load_file_ = false;
        ui->leftComboBox->clear();
        ui->rightComboBox->clear();
        ui->leftComboBox->addItem("");
        ui->rightComboBox->addItem("");
        for(int i = 0; i < last_result_[index-1].size(); ++i) {
            ui->leftComboBox->addItem( QString("Code %1").arg(i+1) );
            ui->rightComboBox->addItem( QString("Code %1").arg(i+1) );
        }
        ui->leftComboBox->setCurrentIndex(0);
        ui->rightComboBox->setCurrentIndex(0);
    }
    load_file_ = true;
}

void DuplicatedCodeWidget::LeftComboboxCurrentIndexChanged(int index)
{
    if(!load_file_ || index == 0 || ui->groupComboBox->currentIndex() == 0) return;
    ref_erl_model_->CatFile( std::get<0>(last_result_[ui->groupComboBox->currentIndex()-1][index-1]) );
    latest_start_ = std::get<1>(last_result_[ui->groupComboBox->currentIndex()-1][index-1]);
    latest_end_ = std::get<2>(last_result_[ui->groupComboBox->currentIndex()-1][index-1]);
    load_left_ = true;
    setEnabled(false);
}

void DuplicatedCodeWidget::RightComboboxCurrentIndexChanged(int index)
{
    if(!load_file_ || index == 0 || ui->groupComboBox->currentIndex() == 0) return;
    ref_erl_model_->CatFile( std::get<0>(last_result_[ui->groupComboBox->currentIndex()-1][index-1]));
    latest_start_ = std::get<1>(last_result_[ui->groupComboBox->currentIndex()-1][index-1]);
    latest_end_ = std::get<2>(last_result_[ui->groupComboBox->currentIndex()-1][index-1]);
    load_right_ = true;
    setEnabled(false);
}

void DuplicatedCodeWidget::ShowFile(const QString &path, const QString &content)
{
    if(load_left_) {
        left_browser_->SetContent(path, content);
        left_browser_->JumpToPosition(latest_start_);
        left_browser_->HighLightSection(latest_start_, latest_end_);
        load_left_ = false;
    } else if(load_right_) {
        right_browser_->SetContent(path, content);
        right_browser_->JumpToPosition(latest_start_);
        right_browser_->HighLightSection(latest_start_, latest_end_);
        load_right_ = false;
    }
    setEnabled(true);
}

void DuplicatedCodeWidget::DisplayAlgorithms()
{
    for(int i = 0; i < ref_erl_model_->DuplicatedCodeAlgorithms()->size(); ++i) {
        DuplicatedCodeAlgorithm algorithm = ref_erl_model_->DuplicatedCodeAlgorithms()->at(i);
        DuplicatedCodeAlgorithmWidget *widget =
                new DuplicatedCodeAlgorithmWidget(this,
                                                  std::get<0>(algorithm),
                                                  std::get<1>(algorithm),
                                                  ref_erl_model_);
        for(Parameter param : std::get<2>(algorithm)) {
            widget->AddElem(param);
        }
        ui->algorithmsWidget->addTab(widget, std::get<1>(algorithm));
    }
}
