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

#include "erlsyntaxhighlighter.h"
#include <QStringList>

ErlSyntaxHighlighter::ErlSyntaxHighlighter(QTextDocument *parent) :
    QSyntaxHighlighter(parent)
{
    HighlightingRule rule;

    //Atoms
    atom_format_.setForeground( QBrush( QColor(82, 0, 124) ) );
    rule.pattern = QRegExp("[a-z]\\w+");
    rule.format = atom_format_;
    highlighting_rules_.append(rule);
    rule.pattern = QRegExp("\'[@\\.><=/-+\\w]+\'");
    highlighting_rules_.append(rule);

    //Functions
    function_format_.setForeground(Qt::blue);
    rule.pattern = QRegExp("\\b\\w+(?=\\()");
    rule.format = function_format_;
    highlighting_rules_.append(rule);

    //Modules
    module_format_.setForeground(Qt::darkBlue);
    rule.pattern = QRegExp("\\b\\w+:");
    rule.format = module_format_;
    highlighting_rules_.append(rule);

    //Variables
    variable_format_.setForeground(Qt::darkMagenta);
    rule.pattern = QRegExp("[A-Z]\\w*");
    rule.format = variable_format_;
    highlighting_rules_.append(rule);

    //Export or import lists
    export_import_format_.setForeground(Qt::red);
    rule.pattern = QRegExp("\\w+/[0-9]+");
    rule.format = export_import_format_;
    highlighting_rules_.append(rule);

    //Strings
    string_format_.setForeground(Qt::darkCyan);
    rule.pattern = QRegExp("\".*\"");
    rule.format = string_format_;
    highlighting_rules_.append(rule);

    //Macros
    macro_format_.setForeground(Qt::darkBlue);
    rule.pattern = QRegExp("\\?\\w+:");
    rule.format = macro_format_;
    highlighting_rules_.append(rule);

    //Keywords
    keyword_format_.setForeground(Qt::darkBlue);
    keyword_format_.setFontWeight(QFont::Bold);

    QStringList keyword_patterns;
    keyword_patterns << "[-]export\\b" << "[-]compile\\b" << "[-]define\\b"
                     << "\\bcase\\b" << "\\bof\\b" << "\\bbegin\\b" << "\\bend\\b"
                     << "[-]import\\b" << "\\bwhen\\b" << "\\bif\\b" << "\\breceive\\b"
                     << "\\bafter\\b" << "\\bfun\\b" << "\\bcatch\\b" << "\\bthrow\\b"
                     << "\\btry\\b" << "[-]module\\b" << "[-]include_lib\\b" << "[-]include\\b";

    for(const QString &pattern : keyword_patterns) {
          rule.pattern = QRegExp(pattern);
          rule.format = keyword_format_;
          highlighting_rules_.append(rule);
    }

    //Comments
    comment_format_.setForeground(Qt::darkGreen);
    rule.pattern = QRegExp("%[^\n]*");
    rule.format = comment_format_;
    highlighting_rules_.append(rule);

}

void ErlSyntaxHighlighter::highlightBlock(const QString &text)
{
    for(const HighlightingRule &rule : highlighting_rules_) {
        QRegExp expression(rule.pattern);
        int index = expression.indexIn(text);
        while (index >= 0) {
            int length = expression.matchedLength();
            setFormat(index, length, rule.format);
            index = expression.indexIn(text, index + length);
        }
    }

    setCurrentBlockState(0);
}
