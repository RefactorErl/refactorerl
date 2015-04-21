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

#ifndef CODEBROWSERWIDGET_H
#define CODEBROWSERWIDGET_H

#include <QWidget>
#include <QCompleter>
#include <QStringListModel>
#include <QVector>
#include "codebrowser.h"
#include "model/referlmodel.h"

namespace Ui {
class CodeBrowserWidget;
}

//The class allows the user to display an Erlang module with
//syntax highlighting and line numbbers providing a dynamic search area
//and starting semantic queries from an arbitrary position with autocompletion
class CodeBrowserWidget : public QWidget
{
    Q_OBJECT

    private:
        Ui::CodeBrowserWidget *ui; //Ui
        CodeBrowser *code_browser_; //Code browser itselt
        RefErlModel *ref_erl_model_; //RefErlModel to send queries and get
                                     // autocomplete
        QList<int> error_positions_; //List of error start positions
        QString path_; //The path of the file displayed
        bool query_ = false; //If false the query input area is not shown
                             // when the cursor changes
        bool autocomplete_ = false; //If false the class doesn't react
                                    // to the ref_erl_model_ AutoCompleteSignal
        QCompleter *completer_; //Provides autocomlete for a line edit
        QStringListModel *completer_model_; //Model for completion (filled when
                                            // AutoCompleteSignal is handled)
        QStringList last_completed_; //List of the last completion's fully
                                     // completed query

        QVector<QPair<QString, QString> > dupcode_algorithms_;

    protected:
        //Provides that the search area is shown when a key is pressed and
        // the search and query areas are hidden when the Esc key is pressed
        void keyPressEvent(QKeyEvent *event) override;

    public:
        //Constructor
        explicit CodeBrowserWidget(
            QWidget *parent = 0,
            RefErlModel *ref_erl_model = 0,
            const QString &path = "",
            const QString &content = "" );
        //Destructor
        ~CodeBrowserWidget();
        //Returns the path of the currently loaded file
        const QString &GetPath();

    private slots:
        void ShowSearchArea();
        void HideSearchArea();
        void ShowQueryArea();
        void HideQueryArea();
        void ShowDupcodeArea();
        void HideDupcodeArea();
        //Event handler for the search edit's textChanged signal
        //Calls the Find function to find the new text in the module
        void SearchTextChanged(const QString &text);
        //Event handler for the query edit's textChanged signal
        //Asks for autocompletion
        void QueryTextChanged(const QString &text);
        //Event handler for the completer_'s activated signal.
        //Autocompletes the query with the given text
        void SetCompletion(const QString &text);
        void RunQueryButtonClicked();
        void RunDupcodeButtonClicked();
        //Sets the query edit's contents according to the predefined query
        // selected
        void PredefComboBoxCurrentIndexChanged(const int &index);

    public slots:
        //Highlights the text between the given positions as errors
        void HighLightError(const int &start,
                            const int &end);
        void JumpToLine(const int &line_number);
        void JumpToPosition(const int &position);

        //Selects the text between the given positions.
        //If permanent is true the selection will be highlighted
        void HighLightSection(const int &start,
                              const int &end,
                              const bool &permanent = false);

        void ClearHighLight(); //Clears the text's highlights
        void SetContent(const QString &path,
                        const QString &content);
        //Search the given text, if found jump to its position and select it.
        //When backward is true the text is searched backwards in the module
        void Find(const QString &str,
                  const bool &backward = false);

        //Event handler for the code_browser_'s cursorPositionChanged signal
        // gets the predefined queries for the new cursor position
        void RunQueryFromPosition();

        void AutoComplete(const QStringList &autocomplete_list,
                          const QStringList &completed_list);
};

#endif // CODEBROWSERWIDGET_H
