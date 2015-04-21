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

#ifndef CODEBROWSER_H
#define CODEBROWSER_H

#include "erlsyntaxhighlighter.h"
#include <QPlainTextEdit>

//Predeclaration of class LineNumberArea.
//Declaration and definition can be found in linenumberarea.h and
//  linenumberarea.cc respectively.
class LineNumberArea;

//Code browser widget with a line number area
//  and syntax highlight
class CodeBrowser : public QPlainTextEdit
{
    Q_OBJECT

    private:
        QWidget *line_number_area_; //Line number area
        ErlSyntaxHighlighter *highlighter_; //Syntax highlighter

    protected:
        //Redefined resizeEvent function
        //Calls the base class's (QPlainTextEdit) resizeEvent
        //  and updates the size of the line number area
        void resizeEvent(QResizeEvent *event) override;

    public:
        //Constructor
        CodeBrowser(QWidget *parent = 0);

        void LineNumberAreaPaintEvent(QPaintEvent *event);

        //Returns the width of the line number area based on the number
        // of lines in the widget
        int LineNumberAreaWidth();

    private slots:
        //Updates the width of the line number area based on the number
        //  of lines in the widget
        void UpdateLineNumberAreaWidth(int);

        //If the widget is not read only, highlights the current line
        //  with a light yellow background
        void HighlightCurrentLine();

        //Scrolls and resizes the line number area
        void UpdateLineNumberArea(const QRect &, int);

    public slots:
        //Sets the line number area visible/invisible
        void SetLineNumberAreaVisible(const bool &visible);
        //Disables the syntax highlighter
        void DisableHighlighter();


};

#endif // CODEBROWSER_H
