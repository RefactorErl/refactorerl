;; -*- coding: utf-8 -*-

;; This file is part of RefactorErl.
;;
;; RefactorErl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; RefactorErl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
;;
;; The Original Code is RefactorErl.
;;
;; The Initial Developer of the Original Code is Eötvös Loránd University.
;; Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
;; are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
;; and Ericsson Hungary. All Rights Reserved.

(provide 'refactorerl-def)

(eval-when-compile
 (require 'cl))

(defvar refactorerl-menu-groups (list))
(defstruct (menu-group)
  (label)
  (children))


(defmacro* define-menu-group (name label)
  `(progn
     (when (assq (quote ,name) refactorerl-menu-groups)
       (warn "Menu group %s already defined" ,name))
     (setf refactorerl-menu-groups (cons (cons (quote ,name) (make-menu-group :label ,label :children (list))) refactorerl-menu-groups))))

(define-menu-group rename "Renaming")
(define-menu-group move "Move to another module")
(define-menu-group func "Function")
(define-menu-group introelim "Introduce/eliminate")
(define-menu-group upgrade "Upgrade")
(define-menu-group query "Semantic query")
(define-menu-group metrics "Metrics")

(defvar refactorerl-mode-map (make-sparse-keymap))
(defvar refactorerl-mode-keybindings nil)
(defvar refactorerl-mode-menu nil)
(defvar refactorerl-mode-menu/ops nil)

(defun* add-to-menu-group (group-name name label &key enable)
  (let ((menu-group (cdr (assq group-name refactorerl-menu-groups))))
    (unless menu-group
      (error "Menu group %s not defined" group-name))
    (setf (menu-group-children menu-group)
          (append (menu-group-children menu-group) (list (list label name :enable enable))))))

(defmacro* defun/interactive (name (&rest args) doc &body body)
  ;; TODO: Support for special initializers not actually supported by (interactive), such as line and col number
  (flet ((interactive-code (init-spec)
           (if (stringp init-spec)
               (interactive-code `(:string ,init-spec))
               (destructuring-bind (init-op &rest init-args) (if (consp init-spec) init-spec (list init-spec))
                 (flet ((prompt (directive)
                          (concat directive (apply #'concat init-args) ": ")))
                   (ecase init-op
                     ((:string)
                      (prompt "s"))
                     ((:number)
                      (prompt "n"))
                     ((:dir)
                      (prompt "D"))
                     ((:file)
                      (prompt "f"))
                     ((:newfile)
                      (prompt "F"))
                     ((:point)
                      "d")
                     ((:region)
                      "r"))))))
         (collect-argument (arg-spec)
           (cond ((atom arg-spec) (list arg-spec))
                 ((consp (car arg-spec)) (car arg-spec))
                 (t (list (car arg-spec))))))

    (let ((args/names (loop for arg in args
                            append (collect-argument arg)))
          (args/interactive (mapconcat (lambda (arg)
                                         (interactive-code (if (consp arg) (cadr arg) (symbol-name arg))))
                                       args "\n")))
      `(defun ,name ,args/names
         ,@(when (stringp doc) (list doc))
         ,(if args/interactive
              `(interactive ,args/interactive)
              '(interactive))
         ,(unless (stringp doc) doc)
         ,@body))))

(defun* expand-define-refac-operation (name &key menu-group menu key-group key precondition help args doc body)
  `(progn
     ,(when key
        `(push '(,key ,name) ,key-group))
     ,(when menu
        `(add-to-menu-group ',menu-group ',name ,menu :enable ,precondition))
     (defun/interactive ,name (,@args) ,doc ,@body)))

(defmacro* define-refac-operation ((name &key (menu-group 'refactor) menu key precondition help) args
                                   doc &body body)
    (expand-define-refac-operation name
                                 :menu-group menu-group :menu menu :key key :key-group 'refactorerl-mode-keybindings
                                 :precondition precondition :help help
                                 :args args :doc doc :body body))
