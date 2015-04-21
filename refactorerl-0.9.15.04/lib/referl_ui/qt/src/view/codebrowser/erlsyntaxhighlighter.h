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

#ifndef ERLSYNTAXHIGHLIGHTER_H
#define ERLSYNTAXHIGHLIGHTER_H

#include <QSyntaxHighlighter>

class ErlSyntaxHighlighter : public QSyntaxHighlighter
{
    Q_OBJECT

    private:
        struct HighlightingRule
        {
            QRegExp pattern;
            QTextCharFormat format;
        };

         QVector<HighlightingRule> highlighting_rules_;

         QTextCharFormat keyword_format_;
         QTextCharFormat comment_format_;
         QTextCharFormat atom_format_;
         QTextCharFormat string_format_;
         QTextCharFormat function_format_;
         QTextCharFormat module_format_;
         QTextCharFormat variable_format_;
         QTextCharFormat export_import_format_;
         QTextCharFormat macro_format_;

    public:
        ErlSyntaxHighlighter(QTextDocument *parent = 0);

    protected:
        //Redefined highlight block function to add the syntax
        //highlighting
        void highlightBlock(const QString &text) override;

};

#endif // ERLSYNTAXHIGHLIGHTER_H
