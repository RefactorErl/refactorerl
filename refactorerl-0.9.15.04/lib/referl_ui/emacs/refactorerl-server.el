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
(require 'erlang)
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))
(require 'refactorerl-custom-undo-mode)

(require 'refactorerl-handlers)
(require 'refactorerl-ui)
(require 'refactorerl-customization)
(require 'refactorerl-form)

(provide 'refactorerl-server)

(defvar refac-debug-mode nil)
(defmacro* refac-debug (&body body)
  `(when refac-debug-mode ,@body))

(defvar refac-server-process nil
  "The RefactorErl server process.")

;(defvar refac-buffer-server-process nil 
;  "The RefactorErl buffer handler server process.")

(defun refactorerl-start ()
  "Checks if the RefactorErl server is running, and starts it if neccessary."
  (when (equal refactorerl-base-path "")
    (error "Configuration error: RefactorErl base path is not set"))
  (when (not (refac-server-is-running))
    (let* ((base-path (file-name-as-directory refactorerl-base-path))
           (server-cmd (concat base-path "bin/referl"))
           ;(buffer-server-cmd (concat base-path "bin/BufferServer"))
           (dbtype (cond ((eq refactorerl-db-type nil) "mnesia")
                         ((eq refactorerl-db-type 'Mnesia) "mnesia")
                         ((eq refactorerl-db-type 'Nif) "nif")
                         ((eq refactorerl-db-type 'Kyoto) "kcmini")))
           (server-args (append (list "-base" refactorerl-base-path)
                                (list "-erl" refactorerl-erlang-runtime)
                                (list "-db" dbtype)
                                ;; (when (stringp refactorerl-wrangler-path)
                                ;;   (list "-wrangler" refactorerl-wrangler-path))
                                )))
	(cond ((eq refactorerl-undo-mode 'one-step) nil)
          ((eq refactorerl-undo-mode 'selective) 
		   (add-hook 'refactorerl-mode-hook 'refactorerl-custom-undo-mode)))
      (when (eq refactorerl-server-type 'shell)
        (make-directory (refactorerl-data-dir) t)
        (let ((default-directory (refactorerl-data-dir)))
          (with-current-buffer (apply #'make-comint "RefactorErlShell" server-cmd nil 
				      (append server-args
					      (list (if (eq refactorerl-sname-type 'short) 
							"-sname"
						      "-name")
						    "refactorerlshell@localhost")
					      ))
            (require 'erlang)
			;(require 'edit-mode)
			;(edit-mode)
            (when (featurep 'erlang)
              (erlang-shell-mode)))))
      (setf refac-server-buffer
            (save-excursion (refac-make-server-buffer)))
      (setf refac-server-process
            (apply #'start-process "RefactorErl" refac-server-buffer server-cmd
                   (append server-args (if (eq refactorerl-server-type 'internal)
                                           (list "-emacs")
                                           (list "-emacs" "-client"
                                                 (if (eq refactorerl-sname-type 'short) 
                                                     "-sname"
                                                   "-name") refactorerl-client-name
                                                   "-srvname" refactorerl-server-name)))))
	  ;(setf refac-buffer-server-process
      ;     (start-process "BufferServer" refac-buffer-server-buffer buffer-server-cmd))
	  ;(set-process-buffer refac-buffer-server-process (get-buffer-create "BufferServer"))
      ;; (with-current-buffer refac-server-buffer
      ;;   (make-local-variable '*reqid*)
      ;;   (make-local-variable '*reqid-queue*))
      (setf *reqid-queue* nil) ;; We clear this variable
      (set-process-filter refac-server-process 'refac-output-filter)
      (set-process-sentinel refac-server-process 'refac-process-sentinel)
	  ;(refactorerl-buffer-server-start)
      ;(set-process-filter refac-buffer-server-process 'refac-bufsrv-output-filter)
	  )))

(defvar *refac-callbacks* (make-hash-table :test 'equal))

;(defun refac-bufsrv-output-filter (proc string)
;  (when (and (equal proc refac-buffer-server-process) (> (length string) 0))
;	(message string)
;	(let (lists)
;	  (setq lists (split-string (substring string 0 -1) ";"))
;	  (when (> (length (nth 0 lists)) 2)
;		(dolist (str (split-string (substring (nth 0 lists) 1 -1) ","))
;		  (message str)
;		  (let (buffer) 
;			(setq buffer (get-file-buffer (substring str 1 -1)))
;			(if buffer
;			  (with-current-buffer buffer
;								   (remove-hook 'after-change-functions 'refac-bufsrv-load t)
;								   (revert-buffer t t t)
;								   ;(setq refac-undo-available t)
;								   ;(setq refac-redo-available t)
;								   (sleep-for 0 100)
;								   (refac-send-command 'add (substring str 1 -1))
;								   (add-hook 'after-change-functions 'refac-bufsrv-load t t)
;								   )
;			  (progn (remove-hook 'after-change-functions 'refac-bufsrv-load t)
;					 (sleep-for 0 100)
;					 (refac-send-command 'add (substring str 1 -1))
;					 (add-hook 'after-change-functions 'refac-bufsrv-load t t))
;			  )	
;			)	
;		  )
;		)
;	  (when (> (length (nth 1 lists)) 2)
;		(let (list-undos)
;		  (setq list-undos (split-string (substring (nth 1 lists) 0 ) "|"))
;		  (message (nth 0 list-undos))
;		  (dolist (str-undo list-undos)
;			(message str-undo)
;			(let (lists2)
;			  (setq lists2 (split-string str-undo ":"))
;			  (message (nth 0 lists2))
;			  (let (buffer) 
;				(setq buffer (get-file-buffer (substring (nth 0 lists2) 1 -1)))
;				(if buffer
;				  (with-current-buffer buffer
;									   (let (new-undo-list) 
;										 (setq new-undo-list nil)
;										 ;(setq refac-undo-available nil)
;										 (dolist (str2 (split-string (substring (nth 1 lists2) 1 -1 ) ","))
;										   (message str2)
;										   (when (> (length str2) 2)
;											 (add-to-list 'new-undo-list (substring str2 1 -1))
;											 ))
;										 (refresh-undo-menu buffer new-undo-list)
;										 )
;									   )))))))
;	  (when (> (length (nth 2 lists)) 2)
;		(let (list-redos)
;		  (setq list-redos (split-string (substring (nth 2 lists) 0 ) "|"))
;		  (message (nth 0 list-redos))
;		  (dolist (str-redo list-redos)
;			(message str-redo)
;			(let (lists2)
;			  (setq lists2 (split-string str-redo ":"))
;			  (message (nth 0 lists2))
;			  (let (buffer) 
;				(setq buffer (get-file-buffer (substring (nth 0 lists2) 1 -1)))
;				(if buffer
;				  (with-current-buffer buffer
;									   (let (new-redo-list)
;										 (setq new-redo-list nil)
;										 ;(setq refac-redo-available nil)
;										 (dolist (str2 (split-string (substring (nth 1 lists2) 1 -1 ) ","))
;										   (message str2)
;										   (when (> (length str2) 2)
;											 (add-to-list 'new-redo-list (substring str2 1 -1))
;											 ))
;										 ;(message (nth 0 new-redo-list))
;										 (refresh-redo-menu buffer new-redo-list)
;										 )
;									   ;(message (nth 0 refac-redo-available))
;									   ))))))
;		))))

(defun refac-output-filter (proc string)
  "Analyses server output, puts unrecognised messages into the process buffer."
  (when (equal proc refac-server-process)
	;(message (concat "refac-srv output:" string))
    (setf refac-output-buffer (concat refac-output-buffer string))
    (save-match-data
      (let (eol line)
        (while (setq eol (string-match "\n" refac-output-buffer))
          (setf line (substring refac-output-buffer 0 eol))
          (setf refac-output-buffer (substring refac-output-buffer (1+ eol)))
          (if (or (equal line "")
                  (not (equal (elt line 0) ?\02)))
              (with-current-buffer (process-buffer proc)
                (save-excursion
                  (goto-char (point-max))
                  (widget-insert (concat line "\n"))))
            (let ((data (read (substring line 1))))
              (refac-debug
               (message "RECV: %s" (substring line 1)))
              ;; This is either a new ID or part of an existing conversation
              (if (eql (elt data 0) 'reqid)
                  (handle-reqid (elt data 1))

                (lexical-let ((reqid (elt data 0)))
                  (case (elt data 1)
                    (statusinfo
                     (mapc #'refac-handle-statusinfo (list-from-vector (elt data 2))))

                    (reply
                     (let ((callback (gethash reqid *refac-callbacks*)))
                       (when callback
                         (unwind-protect
                             (apply callback (list-from-vector (elt data 2)))
                           (remhash reqid *refac-callbacks*)))))

                    (question
                     (setf is-user-asked t)
                     (refac-debug (message "%s" (elt data 2)))
                     (destructuring-bind (qid formspec)
                         (list-from-vector (elt data 2))

                       (lexical-let ((qid qid))
                         ;; (refac-send-command 'cancel reqid)
                         (create-form (parse-formspec formspec)
                                      (lambda (&rest results)
                                        (refac-send reqid 'reply qid (to-yesno results))
                                        (setf is-user-asked nil))
                                      (lambda ()
                                        (refac-send reqid 'cancel qid)
                                        (setf is-user-asked nil))))))

                    (progress
                     (refac-debug
                      (message "PROGRESS %s" (list-from-vector (elt data 2))))
					 (let ((callback (gethash reqid *refac-callbacks*)))
                      (when callback
                            (apply callback (list-from-vector (elt data 2)))
                          )) ;; << -- here it comes!
																			  )
																				))))))))))

(defun to-yesno (list)
  (loop for x in list
        collect (if x
                    (case x
                      ('t 'yes)
                      (otherwise x))
                  'no)))

(defun refac-handle-statusinfo (statusinfo)
  (let ((key (elt statusinfo 0))
        (payload (elt statusinfo 1)))
    (case key
      (change
       (refac-handle-change payload))
      (shutdown
       (message "%s" payload)))))

(defun refac-handle-change (changes)
  (dolist (change changes)
    (refac-debug
     (message "Change: %s" change))
    (destructuring-bind (filename events) change
      (let ((buffer (get-file-buffer filename)))
        (if buffer
          (with-current-buffer buffer
            (loop for (event-key . event-args) in events
                  do (case event-key
                       (added
						 (run-hook-with-args 'bufsrv-message-functions (concat "load-opened \"" filename "\""))
						;(refac-send-bufsrv-command (concat "load-opened \"" filename "\""))
                        (message "Added: %s" filename)
                        (refac-set-buffer-state :ok))
                       (dropped
						(run-hook-with-args 'bufsrv-message-functions (concat "drop \"" filename "\""))
						(setq refac-buffer-opened nil)
                        ;(refac-send-bufsrv-command (concat "drop \"" filename "\""))
						(message "Dropped: %s" filename)
						(setq refac-undo-available nil)
						(setq refac-redo-available nil)

                        (refac-set-buffer-state :off))
                       (error
                        (message "Error in: %s" filename)
                        (refac-set-buffer-state :err))
                       (present
                        (refac-set-buffer-state (case (car event-args)
                                                  (true (progn 
														  (message "Added: %s" filename) 
														  (when (and refactorerl-custom-undo-mode (not refac-buffer-opened) (not (eq refac-buffer-state :off)))
														  (buffer-disable-undo)
						 								  (run-hook-with-args 'bufsrv-message-functions (concat "open \"" filename "\""))
														  ;(refac-send-bufsrv-command (concat "open \"" filename "\""))
														  (setq refac-buffer-opened t))
															   :ok))
                                                  (false (progn 
														   (message "File not present in RefErl database: %s" filename)
														  (when (and refactorerl-custom-undo-mode refac-buffer-opened)
														  ;(when (and refactorerl-custom-undo-mode refac-buffer-opened)
														   (run-hook-with-args 'bufsrv-message-functions (concat "drop \"" filename "\""))
														   ;(refac-send-bufsrv-command (concat "drop \"" filename "\"")) 
														   (buffer-enable-undo)
														   (setq refac-undo-available nil)
														   (setq refac-redo-available nil)
														   (setq refac-buffer-opened nil))
														   :off)))))
                       (content
						(run-hook-with-args 'bufsrv-message-functions (concat "load-opened \"" filename "\""))
						;(refac-send-bufsrv-command (concat "load-opened \"" filename "\""))
						(remove-hook 'after-change-functions 'refac-bufsrv-load t)
                        (revert-buffer t t t)
						(add-hook 'after-change-functions 'refac-bufsrv-load t t)
						)
                       (rename
                        (set-visited-file-name (car event-args))
						(run-hook-with-args 'bufsrv-message-functions (concat "rename-opened \"" filename  "\" \"" (car event-args) "\""))))))
						;(refac-send-bufsrv-command (concat "rename-opened \"" filename  "\" \"" (car event-args) "\""))))))
		  (loop for (event-key . event-args) in events
				do
				(case event-key
				  (added
					)
				  (dropped
					)
				  (error
					)
				  (present
					)
				  (content
					(run-hook-with-args 'bufsrv-message-functions (concat "load-closed \"" filename "\""))
					;(refac-send-bufsrv-command (concat "load-closed \"" filename "\""))
					)
				  (rename
					(run-hook-with-args 'bufsrv-message-functions (concat "rename-closed \"" filename "\" \"" (car event-args) "\""))
					;(refac-send-bufsrv-command (concat "rename-closed \"" filename "\" \"" (car event-args) "\""))
					)
				  )
				)
		  )))))

;(defun after-kill-buffer ()
;  (refac-send-bufsrv-command (concat "close \"" buffer-file-name "\""))
;  )

(defun refac-process-sentinel (proc event)
  (when (equal proc refac-server-process)
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (concat "RefactorErl input process " event))))))

(defun refac-get-prop (props key default-value)
  (loop for prop in props
        when (and (vectorp prop) (eq (elt prop 0) key)) return (elt prop 1)
        else when (eq prop key) return 'true
        finally return default-value))

;(defun refac-bufsrv-load (begin end len)
;  ;(setq refac-undo-available t)
;  (refac-send-bufsrv-command "end-update")
;  (write-region nil nil (make-backup-file-name (buffer-file-name)))
;  ;(setq refac-buffer-edited t)
;  (refac-send-bufsrv-command (concat "load-edited \"" (make-backup-file-name (buffer-file-name)) "\" \"" (buffer-file-name) "\""))
;  (message "")
;  )


(defun refac-file-saved ()
  "Informs the server that a file is changed. Runs from `after-save-hook'."
  (when (member refac-buffer-state '(:ok :err))
    (refac-set-buffer-state nil)
    (refac-send-command 'add buffer-file-name)))

(defun refactorerl-update-status ()
  "Requests a file status update from the server."
  (interactive)
  (when buffer-file-name
    (refac-send/callbacks ('status buffer-file-name)
                          (:reply (ok changes)
                                  (refac-handle-change changes)))))

;; Generic supporting functions

;; Conversation IDs
(defvar *reqid*)
(defvar *reqid-queue* nil)

(defun handle-reqid (new-id)
  (setf *reqid* new-id)
  (when *reqid-queue*
    (funcall *reqid-queue*)
    (setf *reqid-queue* nil)))

(defun refac-send (&rest args)
  "Send `args' directly to the server. Use `refac-send-command'
to automatically get a new conversation ID"
    (when (not (refac-server-is-running))
            (error "RefactorErl server isn't running!"))
  (let ((msg (refac-erl-format (vconcat args))))
;	(message (concat "sending:" msg))
    (refac-debug (message "SEND: %s" msg))
    (process-send-string refac-server-process (concat msg ".\n"))))

(defun refac-get-id ()
  (refac-send 'getid))

(defmacro* with-refac-id ((&rest vars) &body body)
  "Send `getid' to the server, and call `body' once the response
with the new ID has arrived. By then, global variable `*reqid*'
is bound to the latest request ID."
  `(progn
    (when (not (refac-server-is-running))
            (error "RefactorErl server isn't running!"))
     (when *reqid-queue*
       (error "Some internal error occured. Please, restart Emacs!"))
     (lexical-let (,@(loop for var in vars
                           collect `(,var ,var)))
       (setf *reqid-queue* (lambda ()
                             (unwind-protect
                                 (progn ,@body)
                               (setf *reqid-queue* nil)))))
     (refac-get-id)))

(defmacro* refac-send-command (&rest cmdargs)
  "Send a command to the server. After getting a new conversation
ID, a tuple is sent to the server process, the first element is
`cmd', the rest comes from `args'. See `refac-erl-format' for
available types."
 (run-hook-with-args 'bufsrv-message-functions "end-update")
 ;(refac-send-bufsrv-command "end-update")
 (sleep-for 0 100)
 (run-hook-with-args 'bufsrv-message-functions "begin-update")
 ;(refac-send-bufsrv-command "begin-update")
  `(refac-send/callbacks ,cmdargs (:reply (ok cmdres)
                                  (refac-handle-cmdres cmdres))))

;(defun refac-send-bufsrv-command (cmd)
;    (process-send-string refac-buffer-server-process (concat cmd "\n")))

(defun refac-handle-cmdres (cmdres)
  (if (or (equal cmdres "")
          (equal cmdres 'metric_mode_on)
          (equal cmdres 'metric_mode_off)
          (not cmdres)
;          (not (equal (list-length cmdres) 3))
          )
    (message "ok")
	(sleep-for 0 100)
   (progn (setq type (elt cmdres 0))
          (setq data (elt cmdres 1))
          (setq badsmells (elt cmdres 2))
    (case type
     ('result
;       (queryres badsmells)
;       (refac-handle-query-res badsmells )
       (refac-handle-query-res (list 'result (list (list 'result badsmells))) )
;       (refac-handle-query-res (list 'result badsmells) )
;       (refac-handle-query-res badsmells )
;       (refac-handle-query-res badsmells)
;       (show-query-res (list 'result (list (list 'result badsmells))) )
;!!!!!!!!!!!!!!!!!!!!!!       (refac-handle-query-res (list 'result badsmells) )
       (message "RefactorErl: transformation done."))
     ('abort (let ((text (elt data 1)))
              (message "RefactorErl: denied: %s" text)))
;     ('error (let ((text (elt data 1)))
;              (message "RefactorErl: error: %s" text)))
     (otherwise
;              (message "RefactorErl: unknown error: %s" cmdres)
              (message "ok") ; TODO: separate funs for refactoring and other commands
              ))))
 	(run-hook-with-args 'bufsrv-message-functions "end-update")
	;(refac-send-bufsrv-command "end-update")
  )

(defmacro* refac-send/callbacks ((cmd &rest args) &body body)
  "Send a command to server, and then handle progress and reply messages. See the following example:
  (refac-send/callbacks ('status buffer-file-name)
    (:reply (ok changes)
      (refac-handle-change changes)))"
  `(progn
     (lexical-let ((cmd ,cmd)
                  (args (list ,@args)))
      (refac-debug (message "Cmd: %s" cmd))
      (refac-debug (message "args: %s" args))
      (setf qstr nil)
      (when (eq (elt args 0) 'semantic_query)
            (progn (setf arglist (elt args 1))
                    (car                 
        (dolist (x arglist)
          (when (eql (elt x 0) 'querystr)
            (setf qstr (elt x 1)))
          )))
        )
      (with-refac-id ()
                     (setf (gethash *reqid* *refac-callbacks*)
                           (lambda (status &rest status-args)
                             (case status
                               ,@(loop for (kind . clause) in body when (eql kind :reply)
                                       collect (destructuring-bind ((status &rest lambda-form) &body action) clause
                                                 `(,status
                                                   (destructuring-bind ,lambda-form status-args
                                                     ,@action))))
                               ((error)
                                (message "RefactorErl error: %s" (elt (elt status-args 0) 1))))))
                     (apply #'refac-send *reqid* cmd args)))))

;; Conversion between Erlang and ELisp representation
(defun refac-erl-format (data)
  "Turns a piece of data into Erlang string representation. Available types:
 * symbols become atoms (the leading \":\" is stripped from keywords)
 * strings become strings
 * numbers become numbers
 * lists become lists with their elements converted recursively
 * vectors become tuples with their elements converted recursively"
  (cond ((equal data nil)
         "[]")
        ((keywordp data)
         (substring (symbol-name data) 1))
        ((symbolp data)
         (symbol-name data))
        ((stringp data)
         (concat "\"" (apply #'concat (mapcar 'refac-escape-char data)) "\""))
        ((numberp data)
         (number-to-string data))
        ((listp data)
         (concat "[" (mapconcat 'refac-erl-format data ",") "]"))
        ((vectorp data)
         (concat "{" (mapconcat 'refac-erl-format data ",") "}"))))

(defun refac-escape-char (char)
  "Perform character escaping according to Erlang rules."
  (cond ((eq char ?\")     "\\\"")
        ((eq char ?\\)     "\\\\")
        ((or (< char 32)
             (> char 126)) (format "\\%03o" char))
        (t                 (string char))))


;; Server maintenance
;(defun refac-buffer-server-is-running ()
;  "Checks if the BufferServer is running."
;  (and refac-buffer-server-process
;       (equal 'run (process-status refac-buffer-server-process))))


(defun refac-processes-are-running (check-shell-if-running)
  "Checks if the RefactorErl server is running."
	(interactive)
;(message (prin1-to-string (get-process "RefactorErlShell")))
  (and refac-server-process
       (equal 'run (process-status refac-server-process))
	   (if check-shell-if-running
			(refac-shell-running)
			t)))

(defun refac-shell-running ()
  (if (get-process "RefactorErlShell")
	  (equal 'run (process-status (get-process "RefactorErlShell")))
	  nil)
)

(defun refac-server-is-running ()
  "Checks if the RefactorErl server is running."
  (refac-processes-are-running nil))

(defun refac-save-config (&rest args)
  (refac-send-command 'saveconfig
                      (widget-value appdir-list)
                      (widget-value incdir-list)
                      (widget-value outdir-menu))
  (refac-hide-config))

(defun refactorerl-server-show-files ()
  (interactive)
  (async-with-refac-buffer-list (refac-send-command 'status_info (list))))

(defun refactorerl-server-reset-db ()
  (interactive)
  (when (yes-or-no-p "Clear database contents? ")
    (refac-send-command 'reset)))

