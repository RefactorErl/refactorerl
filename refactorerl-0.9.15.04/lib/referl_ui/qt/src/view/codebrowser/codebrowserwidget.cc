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

#include "codebrowserwidget.h"
#include "ui_codebrowserwidget.h"
#include <QMessageBox>
#include <QInputDialog>

CodeBrowserWidget::CodeBrowserWidget(QWidget *parent,
                                     RefErlModel *ref_erl_model,
                                     const QString &path,
                                     const QString &content) :
    QWidget(parent),
    ui(new Ui::CodeBrowserWidget),
    ref_erl_model_(ref_erl_model),
    path_(path)
{
    ui->setupUi(this);
    code_browser_ = new CodeBrowser(this);
    SetContent(path, content);
    code_browser_->setReadOnly(true);
    ui->mainLayout->insertWidget(0, code_browser_);
    completer_ = new QCompleter(this);
    completer_model_ = new QStringListModel(this);
    completer_->setModel(completer_model_);
    completer_->setWidget(ui->queryEdit);

    if(path != "") {
        code_browser_->setToolTip(QString("%1 module").arg(path));
    }

    ui->predefQueriesComboBox->setModel(ref_erl_model_->PredefQueriesModel());
    ui->predefQueriesComboBox->setModelColumn(0);

    HideSearchArea();
    HideQueryArea();
    HideDupcodeArea();

    connect( ui->hideButton, SIGNAL( clicked() ),
        this, SLOT( HideSearchArea() ) );
    connect( ui->findEdit, SIGNAL( textChanged(QString) ),
        this, SLOT( SearchTextChanged(QString) ) );
    connect( code_browser_, SIGNAL( cursorPositionChanged() ),
        this, SLOT( RunQueryFromPosition() ) );
    connect( ui->runButton, SIGNAL( clicked() ),
        this, SLOT( RunQueryButtonClicked() ) );
    connect( ui->runDupcodeButton, SIGNAL( clicked() ),
        this, SLOT( RunDupcodeButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( AutoCompleteSignal(QStringList, QStringList) ),
        this, SLOT( AutoComplete(QStringList, QStringList) ) );
    connect( completer_, SIGNAL( activated(QString) ),
        this, SLOT( SetCompletion(QString) ) );
    connect( ui->queryEdit, SIGNAL(textEdited(QString)),
        this, SLOT( QueryTextChanged(QString) ) );
    connect( ui->predefQueriesComboBox, SIGNAL( currentIndexChanged(int) ),
        this, SLOT( PredefComboBoxCurrentIndexChanged(int) ) );

    dupcode_algorithms_.push_back( QPair<QString, QString>("matrix", "Matrix") );
    dupcode_algorithms_.push_back( QPair<QString, QString>("sw_metrics", "Software metrics") );
    dupcode_algorithms_.push_back( QPair<QString, QString>("matrixfilter", "Matrix filter") );
    dupcode_algorithms_.push_back( QPair<QString, QString>("suffix_tree", "Suffix tree") );
    dupcode_algorithms_.push_back( QPair<QString, QString>("filtered_suffix_tree", "Filtered suffix tree") );

    for(auto& algorithm : dupcode_algorithms_) {
        ui->algorithmCombo->addItem(algorithm.second);
    }

}

CodeBrowserWidget::~CodeBrowserWidget()
{
    delete ui;
}

const QString &CodeBrowserWidget::GetPath()
{
    return path_;
}

void CodeBrowserWidget::HighLightError(const int &start, const int &end)
{
    query_ = false;
    error_positions_ << start;
    QTextDocument *document = code_browser_->document();
    QTextCursor cursor(document);

    cursor.movePosition(QTextCursor::Start);

    cursor.movePosition(QTextCursor::NextCharacter,
                        QTextCursor::MoveAnchor, start);
    cursor.movePosition(QTextCursor::NextCharacter,
                        QTextCursor::KeepAnchor, end - start);

    QTextCharFormat format;
    format.setBackground( QBrush( QColor(255, 230, 230) ) );

    cursor.mergeCharFormat(format);

    cursor.movePosition(QTextCursor::Start);
    query_ = true;
}

void CodeBrowserWidget::JumpToLine(const int &line_number)
{
    query_ = false;
    QTextCursor cursor = code_browser_->textCursor();
    cursor.movePosition(QTextCursor::Start);
    cursor.movePosition(QTextCursor::NextBlock, QTextCursor::MoveAnchor, line_number - 1);
    code_browser_->setTextCursor(cursor);
    code_browser_->centerCursor();

    /*int cursorY = code_browser_->cursorRect(cursor).top();
    QScrollBar *vbar = code_browser_->verticalScrollBar();
    vbar->setValue(vbar->value() + cursorY);*/

    query_ = true;
}

void CodeBrowserWidget::JumpToPosition(const int &position)
{
    query_ = false;
    QTextCursor cursor = code_browser_->textCursor();
    cursor.movePosition(QTextCursor::Start);
    cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::MoveAnchor, position);
    code_browser_->setTextCursor(cursor);
    code_browser_->centerCursor();

    /*int cursorY = code_browser_->cursorRect(cursor).top();
    QScrollBar *vbar = code_browser_->verticalScrollBar();
    vbar->setValue(vbar->value() + cursorY);*/
    query_ = true;
}

void CodeBrowserWidget::SetContent(const QString &path, const QString &content)
{
    query_ = false;
    code_browser_->setPlainText(content);
    setWindowTitle(path);
    path_ = path;
    query_ = true;
    code_browser_->setToolTip(QString("%1 module").arg(path));
}

void CodeBrowserWidget::Find(const QString &str, const bool &backward)
{
    query_ = false;
    if( !code_browser_->find(
        str,
        backward ? QTextDocument::FindBackward : QTextDocument::FindFlag(0)) ) {

        ui->findEdit->setStyleSheet( "background-color: #ff9292" );
        if( QMessageBox::question(
                this, "Couldn't find text",
                "The searched text was not found. Would you like to start over?",
                QMessageBox::Yes | QMessageBox::No
             ) == QMessageBox::Yes ) {

            QTextCursor cursor = code_browser_->textCursor();
            cursor.movePosition(QTextCursor::Start);
            code_browser_->setTextCursor(cursor);
        }
    } else {
        ui->findEdit->setStyleSheet("background-color: #ffffff");
    }
    query_ = true;
}

void CodeBrowserWidget::keyPressEvent(QKeyEvent *event)
{
    if( event->key() == Qt::Key_Escape ) {
        HideSearchArea();
        HideQueryArea();
        HideDupcodeArea();
        QWidget::keyPressEvent(event);
    } else if( event->key() == Qt::Key_Enter || event->key() == Qt::Key_Return ) {
        if(!ui->findEdit->isHidden()) {
            Find(ui->findEdit->text());
        } else if(!ui->queryEdit->isHidden()) {
            RunQueryButtonClicked();
        }
    } else if( 32 <= event->key() && event->key() <= 126 ) {
        ShowSearchArea();
        ui->findEdit->setText( ui->findEdit->text() + event->text() );
        ui->findEdit->setFocus();
    }
}

void CodeBrowserWidget::ShowSearchArea()
{
    ui->previousButton->show();
    ui->nextButton->show();
    ui->findEdit->show();
    ui->hideButton->show();
}

void CodeBrowserWidget::HideSearchArea()
{
    ui->previousButton->hide();
    ui->nextButton->hide();
    ui->findEdit->hide();
    ui->hideButton->hide();
}

void CodeBrowserWidget::ShowQueryArea()
{
    ui->label->show();
    ui->positionLabel->show();
    ui->queryEdit->show();
    ui->runButton->show();
    ui->predefLabel->show();
    ui->predefQueriesComboBox->show();
}

void CodeBrowserWidget::HideQueryArea()
{
    ui->label->hide();
    ui->positionLabel->hide();
    ui->queryEdit->hide();
    ui->runButton->hide();
    ui->predefLabel->hide();
    ui->predefQueriesComboBox->hide();
}

void CodeBrowserWidget::ShowDupcodeArea()
{
    ui->algorithmCombo->show();
    ui->runDupcodeButton->show();
    ui->selectionRangeLabel->show();
    ui->selectionLabel->show();
}

void CodeBrowserWidget::HideDupcodeArea()
{
    ui->algorithmCombo->hide();
    ui->runDupcodeButton->hide();
    ui->selectionRangeLabel->hide();
    ui->selectionLabel->hide();
}

void CodeBrowserWidget::SearchTextChanged(const QString &text)
{
    query_ = false;
    if(!text.isEmpty()) {
        QTextCursor cursor = code_browser_->textCursor();
        cursor.movePosition(QTextCursor::Start);
        code_browser_->setTextCursor(cursor);
        Find(text);
    }
    query_ = true;
}

void CodeBrowserWidget::HighLightSection(const int &start,
                                         const int &end,
                                         const bool &permanent)
{
    query_ = false;
    if(permanent) {
        QTextDocument *document = code_browser_->document();
        QTextCursor dcursor(document);
        dcursor.movePosition(QTextCursor::Start);
        dcursor.movePosition(QTextCursor::NextCharacter,
                             QTextCursor::MoveAnchor, start - 1);
        dcursor.movePosition(QTextCursor::NextCharacter,
                             QTextCursor::KeepAnchor, end - start + 1);
        QTextCharFormat format;
        format.setBackground( QBrush( QColor(Qt::yellow).lighter(160) ) );

        dcursor.mergeCharFormat(format);
    }
    QTextCursor cursor = code_browser_->textCursor();
    cursor.movePosition(QTextCursor::Start);
    cursor.movePosition(QTextCursor::NextCharacter,
                        QTextCursor::MoveAnchor, start - 1);
    if(!permanent) {
        cursor.movePosition(QTextCursor::NextCharacter,
                            QTextCursor::KeepAnchor, end - start + 1);
    }
    code_browser_->setTextCursor(cursor);
    query_ = true;
}

void CodeBrowserWidget::ClearHighLight()
{
    query_ = false;
    QTextDocument *document = code_browser_->document();
    QTextCursor cursor(document);
    cursor.movePosition(QTextCursor::Start);
    cursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
    QTextCharFormat format;
    format.setBackground( QBrush( QColor(255, 255, 255) ) );
    cursor.mergeCharFormat(format);
    query_ = true;
}

void CodeBrowserWidget::RunQueryFromPosition()
{
    if(!query_) return;
    QTextCursor cursor = code_browser_->textCursor();

     HideSearchArea();

    if(cursor.selectionStart() != cursor.selectionEnd()) {
        ShowDupcodeArea();
        ui->selectionRangeLabel->setText(
                    QString::number(cursor.selectionStart()) +
                    QString(" - ") +
                    QString::number(cursor.selectionEnd()) );
    } else {
        HideDupcodeArea();
    }

    ShowQueryArea();
    int position = cursor.selectionStart();
    ui->positionLabel->setText( QString::number(position) );
    ref_erl_model_->GetPredefQueries(path_, position);
    ui->queryEdit->setFocus();
}

void CodeBrowserWidget::RunQueryButtonClicked()
{
    QTextCursor cursor = code_browser_->textCursor();
    int position = cursor.position();
    QString query = ui->queryEdit->text();
    if(!query.isEmpty()) {
        ref_erl_model_->RunQuery(query, path_, position);
    }
}

void CodeBrowserWidget::RunDupcodeButtonClicked()
{
    QString algorithm_key = dupcode_algorithms_[ui->algorithmCombo->currentIndex()].first;
    QTextCursor cursor = code_browser_->textCursor();
    ref_erl_model_->TriggerDupcodeSelectedSearch(algorithm_key,
                                                 path_,
                                                 cursor.selectionStart(),
                                                 cursor.selectionEnd());
}

void CodeBrowserWidget::AutoComplete(const QStringList &autocomplete_list,
                                     const QStringList &completed_list)
{
    if(!autocomplete_) return;
    completer_model_->setStringList(autocomplete_list);
    last_completed_ = completed_list;
    completer_->complete();
    autocomplete_ = false;
}

void CodeBrowserWidget::QueryTextChanged(const QString &text)
{
    autocomplete_ = true;
    ref_erl_model_->AutoComplete(text);
}

void CodeBrowserWidget::SetCompletion(const QString &text)
{
    int index = completer_model_->stringList().indexOf(text);
    ui->queryEdit->setText( last_completed_.at(index) );
}

void CodeBrowserWidget::PredefComboBoxCurrentIndexChanged(const int &index)
{
    QModelIndex query_index = ref_erl_model_->PredefQueriesModel()->index(index, 1);
    QString query  = ref_erl_model_->PredefQueriesModel()->data(query_index).toString();
    ui->queryEdit->setText( query );
}
