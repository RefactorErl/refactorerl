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
(require 'cl)
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))

(require 'refactorerl-operations)
(require 'refactorerl-customization)
(require 'refactorerl-server)
(require 'refactorerl-mode)
(require 'refactorerl-custom-undo-mode)

(require 'refactorerl-clustering)

(require 'refactorerl-depanal)
(require 'refactorerl-dupcode)

(provide 'refactorerl)

;; Functions implementing interactive functionality (usually bound to a key)

(defun refactorerl-quit ()
  "Stops the RefactorErl server process."
  (interactive)
  (when (refac-processes-are-running t)
    (message "Initiating shutdown...")
    (refac-send-command 'stop)
    (while (refac-server-is-running)
      (sleep-for 0.1))
	;(refac-send-bufsrv-command "quit")
	;(while (refac-buffer-server-is-running)
    ;  (sleep-for 0.1))
	;(refactorerl-buffer-server-stop)
    (message "RefactorErl server has stopped."))
  (when (refac-shell-running)
		(kill-process (get-process "RefactorErlShell"))))

(defun refactorerl-restart ()
  "Restarts the RefactorErl server process."
  (interactive)
  (when (refac-server-is-running)
    (refactorerl-quit)
    (sleep-for 0.5))
  (refactorerl-start))

(defun refactorerl-start-metrics ()
  "Turns on the metrics mode."
  (interactive)
  (if (refac-server-is-running)
      (progn
      (refac-send-command 'metric_mode 'true)
      (setf refac-buffer-state-metrics 'true))
      (error "RefactorErl server isn't running.")))

(defun refactorerl-stop-metrics ()
  "Turns off the metrics mode."
  (interactive)
  (if (refac-server-is-running)
      (progn
      (refac-send-command 'metric_mode 'false)
      (setf refac-buffer-state-metrics nil))
      (error "RefactorErl server isn't running.")))

(defun refactorerl-add-file ()
  "Add the visited file to the active refactoring set."
  (interactive)
  (cond ((not (refac-server-is-running))
         (error "RefactorErl server isn't running."))
        ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state :off))
         (error "Already added"))
        (t
         (refac-set-buffer-state nil)
         (refac-send-command 'add buffer-file-name))))

(defun refactorerl-drop-file ()
  "Remove the visited file from the active refactoring set."
  (interactive)
  (cond ((not (refac-server-is-running))
         (error "RefactorErl server isn't running."))
        ((not buffer-file-name)
         (error "No visited file"))
        ((eq refac-buffer-state :off)
         (error "Not in the active set"))
        (t
         (refac-set-buffer-state nil)
         (refac-send-command 'drop buffer-file-name))))

(defun refactorerl-control-buffer ()
  "Selects the buffer *RefactorErl*"
  (interactive)
  (switch-to-buffer refac-server-buffer))

(defun refactorerl-draw-graph (file type)
  "Draw the contents of the graph into a file. The generated file is suitable
as an input to the graphviz package.

A numerical prefix argument can be used to filter the contents of the graph:
 1: full graph
 2: semantical, structural, and syntactical links
 3: structural and syntactical links
 4: syntactical links (bare syntax tree)
 5: syntactical and lexical links
 6: lexical links
 7: tokens
 8: all except lexical edges (for testing purpose)"
  (interactive "GGraph file: \np")
  (if (file-directory-p file)
   	  (error "Not a file")
      (if (refac-server-is-running)
   	    (refac-send-command 'draw (expand-file-name file) type)
        (error "RefactorErl server isn't running."))        
    ))

(defun refactorerl-debug-shell ()
  "Start an inferior Erlang shell (using the standard Erlang mode package)
that connects to the RefactorErl server."
  (interactive)
  (let ((inferior-erlang-machine-options
         '("-sname" "debug"
           "-remsh" "refactorerl@localhost")))
    (erlang-shell)))

(defun refactorerl-toggle-test ()
  (interactive)
  (if (boundp 'refac-test-mode)
      (setq refac-test-mode (not refac-test-mode))
    (set (make-local-variable 'refac-test-mode) t)))

;(defun refactorerl-buffer-undo ()
;  "Steps backward on the buffer state."
;  (interactive)
;  ;(setq refac-redo-available t)
;  (refac-send-bufsrv-command "end-update")
;  (sleep-for 0 100)
;  (refac-send-bufsrv-command (concat "undo \"" buffer-file-name "\" 0"))
;  )
;
;(defun refactorerl-buffer-redo ()
;  "Steps forward on the buffer state."
;  (interactive)
;  ;(setq refac-undo-available t)
;  (refac-send-bufsrv-command "end-update")
;  (sleep-for 0 100)
;  (refac-send-bufsrv-command (concat "redo \"" buffer-file-name "\" 0"))
;  )
;
;(defun refresh-undo-menu (buffer new-list)
;  (interactive)
; (with-current-buffer buffer
;  (dolist (change refac-undo-available)
;  	(easy-menu-remove-item nil '("Refactor" "Undo") change)
;	)
;  (setq refac-undo-available (reverse new-list))
;  (let (n)
;  (setq n 1)
;  (dolist (change refac-undo-available)
;	(message change)
;  	(easy-menu-add-item nil '("Refactor" "Undo") `[,change (lambda () (interactive) (refac-sel-undo buffer-file-name ,n))])
;	(setq n (+ n 1))
;	)
;  )))
;
;(defun refresh-redo-menu (buffer new-list)
;  (interactive)
; (with-current-buffer buffer
;  (dolist (change refac-redo-available)
;  	(easy-menu-remove-item nil '("Refactor" "Redo") change)
;	)
;  (setq refac-redo-available (reverse new-list))
;  (let (n)
;  (setq n 1)
;  (dolist (change refac-redo-available)
;	(message change)
;  	(easy-menu-add-item nil '("Refactor" "Redo") `[,change (lambda () (interactive) (refac-sel-redo buffer-file-name ,n))])
;	(setq n (+ n 1))
;	)
;  )))
;
;(defun refac-sel-undo (name n)
;  (interactive)
;  (message name)
;  (refac-send-bufsrv-command (concat "undo \"" name "\" " (number-to-string n)))
;  )
;
;(defun refac-sel-redo (name n)
;  (interactive)
;  (message name)
;  (refac-send-bufsrv-command (concat "redo \"" name "\" " (number-to-string n)))
;  )

(defun refactorerl-undo ()
  "Steps backward on the refactoring database."
  (interactive)
  (cond ((not (refac-server-is-running))
         (error "RefactorErl server isn't running."))
        ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state :ok))
         (error "File is not ready for refactoring"))
        ((yes-or-no-p "All changes since last refactoring will be lost. Continue? ")
         (refac-send-command 'undo buffer-file-name))))

(defun refactorerl-list-files (&optional same-win)
  "Shows the contents of the active refactoring set."
  (interactive)
  (refac-send/callbacks ('filelist)
     (:reply (ok status-lines)
       (with-refac-buffer-list
         (let ((inhibit-read-only t))
           (erase-buffer)
           (set (make-local-variable 'last-file-dir) ""))
         (refac-debug (message "Status lines: %s" status-lines))
         (if (listp status-lines)
             (dolist (status-line status-lines)
               (destructuring-bind (file flags) status-line
                 (refac-widget-link file)
                 (widget-insert ":")
                  ;; (widget-insert
                  ;; (format "%s" type) "\t"
                  ;; (if (eql error 'yes) (propertize "Err" 'face 'refactorerl-error) "OK ") "\t"
                  ;; (if (eql lastmod 'undefined) "" (format "%s\t" lastmod))
                  ;; (format "%s" flags))
                  (widget-insert "\n")))
           (widget-insert "(no files in database)"))
         (pop-to-buffer (current-buffer) t)))))
         
         

;; TODO: unify this with list-files and other popup commands
(defun refactorerl-show-errors ()
  "Shows the parser's error messages"
  (interactive)
  (let ((buf (refac-buffer-list-ensure)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (pop-to-buffer buf t)
    (refac-send/callbacks ('status_info '())
                          (:reply (ok parse-errors)
                                  (refac-handle-errorlist parse-errors)))))

(defun refac-handle-errorlist (parse-errorfs)
  (if (not (consp parse-errorfs))
      (message "No errors found")
    (save-excursion
      (with-refac-buffer-list
       (dolist (parse-errorf parse-errorfs)
        (progn
         (setq filepath (elt parse-errorf 0))
         (setq parse-error-prop (elt parse-errorf 1))
         (setq parse-errors (elt (assoc 'error parse-error-prop) 1))
         (if parse-errors
          (dolist (parse-error parse-errors)
              (refac-debug
               (message "perr: %s" parse-error))
          (setf err-proplst (plist-from-lproplist parse-error)) ;; Lisp style error proplist
          (case (elt err-proplst 0)
             (:nexttokentext
              (progn
               (destructuring-bind (&key nexttokentext position) err-proplst
                (let ((start-pos (elt position 0))
                      (end-pos (1+ (elt position 1))))
                  (widget-insert "Parse error at ")
                  (refac-widget-link filepath
                                     :start-pos start-pos :end-pos end-pos
                                     :style 'refactorerl-error
                                     :label (format "%s:%d-%d" (file-name-nondirectory filepath) start-pos end-pos))
                  (widget-insert " near '" nexttokentext "'")
                  (widget-insert "\n")))))
             (:no_include_file
                 (progn 
                   (widget-insert "No include file: ")
                   (widget-insert (prin1-to-string (elt err-proplst 1)))
                   (widget-insert "\n")
                 ))
             (otherwise    
                  (progn 
                   (widget-insert "Error: ")
                   (widget-insert (prin1-to-string (elt err-proplst 0)))                  
                   (widget-insert "\n")                 
                  ))
                                         )))))))))



(defun refactorerl-load-dir (dirname)
  "Adds the contents of a directory to the active refactoring set."
  (interactive "D")
;  (refac-send-command 'add_dir (expand-file-name dirname)))
(setf ff (expand-file-name dirname))
(refac-send/callbacks ('add_dir ff)
	 (:reply (ok result)
		)
	(:reply (completed filenumber index) (progn (message 
										(concatenate 'string "Completed: "
															 (prin1-to-string index)
															 "/"
															 (prin1-to-string filenumber)))
										(sleep-for 3)))
	(:reply (add file percent formcount formmax kbps) (message (concatenate 'string file " - " (prin1-to-string (truncate (* percent 100))) "%%")))) )

;; Move function

(defvar refac-move-fun-buffer nil)
(defun refac-move-fun-params ()
  (let ((source (buffer-file-name)))
    (with-current-buffer
        (setq refac-move-fun-buffer (generate-new-buffer "*Move Function*"))
      (widget-insert
       (propertize "Move multiple functions to another module\n"
                   'face 'bold))
      (widget-insert (concat "Source file: " source "\n"))
      (set (make-local-variable 'target-entry)
           (widget-create 'editable-field
                          :size 30
                          :format "Target module: %v"
                          ""))
      (widget-insert "\nSelect functions to be moved:\n")
      (set (make-local-variable 'source-file) source))
    (refac-send-command 'funlist source)))

(defun refac-move-fun-apply (&rest args)
  (let ((funlist (widget-value function-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal funlist nil)
      (error "No functions selected"))
    (refac-send-command 'transform
                        'reftr_move_fun
                        (list (vector :file source-file)
                              (vector :funlist funlist)
                              (vector :name target)))
    (refac-move-fun-cleanup)))

(defun refac-move-fun-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-fun-buffer))
  (kill-buffer refac-move-fun-buffer))

;; Move record

(defvar refac-move-rec-buffer nil)
(defun refac-move-rec-params ()
  (let ((source (buffer-file-name)))
    (with-current-buffer
        (setq refac-move-rec-buffer (generate-new-buffer "*Move Record*"))
      (widget-insert
       (propertize "Move records to another file\n"
                   'face 'bold))
      (widget-insert (concat "Source file: " source "\n"))
      (set (make-local-variable 'target-entry)
           (widget-create 'editable-field
                          :size 30
                          :format "Target file path: %v"
                          ""))
      (widget-insert "\nSelect records to be moved:\n")
      (set (make-local-variable 'source-file) source))
    (refac-send-command 'recordlist source)))

(defun refac-move-rec-apply (&rest args)
  (let ((recordlist (widget-value record-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal recordlist nil)
      (error "No records selected"))
    (refac-send-command 'transform
                        'reftr_move_rec
                        (list (vector :file source-file)
                              (vector :reclist recordlist)
                              (vector :filename target)))
  (refac-move-rec-cleanup)))

(defun refac-move-rec-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-rec-buffer))
  (kill-buffer refac-move-rec-buffer))

;; Move macro

(defvar refac-move-mac-buffer nil)
(defun refac-move-mac-params ()
  (let ((source (buffer-file-name)))
    (with-current-buffer
        (setq refac-move-mac-buffer (generate-new-buffer "*Move Macro*"))
      (widget-insert
       (propertize "Move macros to another file\n"
                   'face 'bold))
      (widget-insert (concat "Source file: " source "\n"))
      (set (make-local-variable 'target-entry)
           (widget-create 'editable-field
                          :size 30
                          :format "Target file path: %v"
                          ""))
      (widget-insert "\nSelect macros to be moved:\n")
      (set (make-local-variable 'source-file) source))
    (refac-send-command 'macrolist source)))

(defun refac-move-mac-apply (&rest args)
  (let ((macrolist (widget-value macro-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal macrolist nil)
      (error "No macros selected"))
    (refac-send-command 'transform
                        'reftr_move_mac
                        (list (vector :file source-file)
                              (vector :maclist macrolist)
                              (vector :filename target)))
  (refac-move-mac-cleanup)))

(defun refac-move-mac-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-mac-buffer))
  (kill-buffer refac-move-mac-buffer))

;; Implementation of non-interactive functionality

(defun refac-ask-question (type text)
  (condition-case nil
      (cond ((eq type 'string)
             (read-string (concat text " ")))
            ((eq type 'yesno)
             (if (yes-or-no-p (concat text " "))
                 'yes 'no))
        ((destructuring-bind (select values)(list-from-vector type)
           (cond ((eq select 'select)
              (widget-choose text (pairlis values values)))))))
    (quit nil)))
