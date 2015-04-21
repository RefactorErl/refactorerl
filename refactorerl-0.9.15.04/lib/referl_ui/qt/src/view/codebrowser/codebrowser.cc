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

#include "codebrowser.h"
#include "linenumberarea.h"
#include <QPainter>
#include <QTextBlock>
#include <QMenu>
#include <QMessageBox>

CodeBrowser::CodeBrowser(QWidget *parent) :
    QPlainTextEdit(parent)
{
    line_number_area_ = new LineNumberArea(this);
    highlighter_ = new ErlSyntaxHighlighter(document());

    //Connect event handlers - start
    connect( this, SIGNAL( blockCountChanged(int) ),
        this, SLOT( UpdateLineNumberAreaWidth(int) ) );
    connect( this, SIGNAL( updateRequest(QRect,int) ),
        this, SLOT( UpdateLineNumberArea(QRect,int) ) );
    //connect( this, SIGNAL( cursorPositionChanged() ),
    //    this, SLOT( HighlightCurrentLine() ) );
    //Connect event handlers - end

     UpdateLineNumberAreaWidth(0);
     setContextMenuPolicy(Qt::CustomContextMenu);
     //HighlightCurrentLine();
}

int CodeBrowser::LineNumberAreaWidth()
{
    int digits = 1;
    int max = qMax(1, blockCount());

    //Calculate the number of digits to be displayed in the maximum
    //  line number
    while (max >= 10) {
        max /= 10;
        ++digits;
    }

    //Calculate the maximum width of the digits
    int space = 3 + fontMetrics().width(QLatin1Char('9')) * digits;

    return space;
}

void CodeBrowser::UpdateLineNumberAreaWidth(int)
{
    setViewportMargins(LineNumberAreaWidth(), 0, 0, 0);
}

void CodeBrowser::UpdateLineNumberArea(const QRect &rect, int dy)
{
    if(dy) {
        line_number_area_->scroll(0, dy);
    } else {
        line_number_area_->update(0, rect.y(),
                                  line_number_area_->width(), rect.height());
    }

    if( rect.contains(viewport()->rect()) ) {
        UpdateLineNumberAreaWidth(0);
    }
}

void CodeBrowser::resizeEvent(QResizeEvent *e)
{
    QPlainTextEdit::resizeEvent(e);

    QRect cr = contentsRect();
    line_number_area_->setGeometry(
                QRect(cr.left(), cr.top(),
                      LineNumberAreaWidth(), cr.height()));
}

void CodeBrowser::SetLineNumberAreaVisible(const bool &visible)
{
    if(visible) {
        line_number_area_->show();
    } else {
        line_number_area_->hide();
    }
}

void CodeBrowser::DisableHighlighter()
{
    highlighter_->setDocument(0);
}

void CodeBrowser::HighlightCurrentLine()
{
     QList<QTextEdit::ExtraSelection> extraSelections;

     if(!isReadOnly()) {
         QTextEdit::ExtraSelection selection;

         QColor lineColor = QColor(Qt::yellow).lighter(160);

         selection.format.setBackground(lineColor);
         selection.format.setProperty(QTextFormat::FullWidthSelection, true);
         selection.cursor = textCursor();
         selection.cursor.clearSelection();
         extraSelections.append(selection);
     }

     setExtraSelections(extraSelections);
}

void CodeBrowser::LineNumberAreaPaintEvent(QPaintEvent *event)
{
     QPainter painter(line_number_area_);
     painter.fillRect(event->rect(), Qt::lightGray);

     QTextBlock block = firstVisibleBlock();
     int blockNumber = block.blockNumber();
     int top =
        (int)blockBoundingGeometry(block).translated(contentOffset()).top();
     int bottom = top + (int)blockBoundingRect(block).height();

     while (block.isValid() && top <= event->rect().bottom()) {
         if (block.isVisible() && bottom >= event->rect().top()) {
             QString number = QString::number(blockNumber + 1);
             painter.setPen(Qt::black);
             painter.drawText(0, top, line_number_area_->width(),
                              fontMetrics().height(),
                              Qt::AlignRight, number);
         }

         block = block.next();
         top = bottom;
         bottom = top + (int) blockBoundingRect(block).height();
         ++blockNumber;
    }
}
