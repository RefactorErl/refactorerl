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

(require 'widget)
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))

(provide 'refactorerl-form)

(defmacro* destructuring-case (expr &body cases)
  (let ((e (gensym))
        (headvar (gensym)))
    `(let ((,e ,expr))
       (case (car ,e)
         ,@(loop for ((head . paramspec) . body) in cases
                 collect `(',head
                           (destructuring-bind (,headvar ,@paramspec) ,e
                             ,@body)))))))

(defmacro* with-form-popup (buffer-name &body body)
  (let ((buffer (gensym)))
    `(let ((,buffer (generate-new-buffer ,buffer-name)))
       (with-current-buffer ,buffer
         ,@body
         (use-local-map widget-keymap)
         (widget-setup)
         (widget-forward 1))
       (display-buffer ,buffer)
       (pop-to-buffer ,buffer t)
       )))

(defun add-label (text)
  (when text
    (widget-insert text)
    (widget-insert " ")))

(defun create-form (fields cb-submit cb-cancel)
  "Pops up a form showing `fields', then calls `cb-submit' with a list containing the value of each field"
  (with-form-popup "query"
    (lexical-let ((buffer (current-buffer))
                  (widgets (list))
                  (cb-submit cb-submit)
                  (cb-cancel cb-cancel)
                  (my-hook (lambda () (funcall cb-cancel))))
      
     (add-hook 'kill-buffer-hook my-hook)

      (lexical-let ((radios (list)))
        (dolist (fieldspec fields)
          (push (destructuring-case fieldspec
                                    ((info &key text &allow-other-keys)
                                     (widget-insert text)
                                     (widget-insert "\n\n")
                                     nil)
                                    ((checkbox &key text default &allow-other-keys)
                                     (prog1 (widget-create 'checkbox (eq default 'true))
                                       (add-label text)
                                       (widget-insert "\n")))
                                    ((textbox &key text default &allow-other-keys)
                                     (when (eq default -1) (setf default nil))
                                     (add-label text)
                                     (prog1 (widget-create 'editable-field (prin1-to-string (or default "")))))

                                    ((radio &key text default &allow-other-keys)
                                     (let ((widget (widget-create 'radio-button
                                                                  :notify (lambda (self &rest ignore)
                                                                            (dolist (radio radios)
                                                                              (unless (eql radio self)
                                                                                (widget-value-set radio nil)))))))
                                       (add-label text)
                                       (widget-insert "\n")
                                       (push widget radios)
                                       widget))
                                    ((yesno &key text default &allow-other-keys)
                                      (prog1 (widget-create 'checkbox (eq default 'true))
                                       (add-label text)
                                       (widget-insert "\n")))
                                    )
                widgets)))

      (widget-insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (let ((results (list)))
                                 (dolist (widget widgets)
                                   (push (if widget (widget-value widget) 'info) results))
                                 (remove-hook 'kill-buffer-hook my-hook)
                                 (kill-buffer buffer)
                                 (delete-window (get-buffer-window buffer))
                                 (apply cb-submit results)))
                     "Submit")
      (widget-insert " ")

      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (funcall cb-cancel)
                               (delete-window (get-buffer-window buffer))
                               (remove-hook 'kill-buffer-hook my-hook)
                               (kill-buffer buffer))
                     "Cancel")
      (widget-insert "\n"))))

(defun refactorerl-test-form ()
  (interactive)
  (let ((fields
         ;; TODO: radio
         '((info :text "Select macros to move")
           (checkbox :text "Macro1" :default nil)
           (checkbox :text "Macro2" :default nil)
           (textbox :text "Destination module" :validator file))))
    (create-form fields (lambda (&rest results)
                          (message "Form results: %s" results)))))

(defun parse-formspec (formspec)
  (loop for fieldspec in formspec
        collect  (let* (format
                        (plist (loop for (key value) in fieldspec
                                     when (equal key 'format) do (setf format value)
                                     else append (list (intern (concat ":" (symbol-name key))) value))))
                   (cons format plist))))

(defun refactorerl-test-rename-fun ()
  (interactive)
  (let ((fields '((textbox :text "The given function name is already used. Please type in a new function name:" :default nil))))
        (create-form fields (lambda (new-name)
                          (message "Form results: %s" new-name)))))

