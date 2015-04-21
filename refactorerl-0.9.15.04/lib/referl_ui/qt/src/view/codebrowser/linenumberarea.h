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

#ifndef LINENUMBERAREA_H
#define LINENUMBERAREA_H

#include <QWidget>
#include <QSize>
#include "codebrowser.h"

//Provides a line number area for CodeBrowswer class
class LineNumberArea : public QWidget
{
    Q_OBJECT

    private:
        CodeBrowser *code_browser_; //Code browser

    protected:
        //Redefined paintEvent function
        //Calls the LineNumberreaPaintEvent of the CodeBrowser
        void paintEvent(QPaintEvent *event) override;

    public:
        //Constructor
        LineNumberArea(CodeBrowser *browser = 0);

        //Redefined sizeHint function
        //Returns the width of the line number area based on the number
        //  of lines in the code browser
        QSize sizeHint() const override;

};

#endif // LINENUMBERAREA_H
