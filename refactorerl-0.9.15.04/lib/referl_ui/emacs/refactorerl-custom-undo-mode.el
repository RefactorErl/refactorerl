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

;(require 'refactorerl-mode)
(provide 'refactorerl-custom-undo-mode)


(defmacro* define-my-minor-mode* (name doc &key lighter map menu on off)
  (let ((manual-menu-add (featurep 'xemacs)))
    `(define-minor-mode ,name ,doc
       :lighter ,lighter
       :map map
       (if ,name
           (progn
             ;,(when manual-menu-add
               ;     `(easy-menu-add ,menu))
             ,on)
           (progn
             ;,(when manual-menu-add
             ;       `(easy-menu-remove ,menu))
             ,off)))))



(define-my-minor-mode* refactorerl-custom-undo-mode
  ""
  :lighter ()
  :map ()
  :menu ()
  :on (progn
        ;; (make-local-variable 'refac-ephemeral-overlays)
		;(setf edit-mode-on t)
        (loop for var in '(refac-undo-available 
						   refac-redo-available 
						   refac-buffer-opened)
              do (make-local-variable var)
			  ;(setq refac-undo-available nil)
			  ;(setq refac-redo-available nil)
			  ;(setq refac-buffer-opened nil)
			  )
        (refactorerl-buffer-server-start)
		(sleep-for 0 100)
		(load-opened-files)
		(sleep-for 0 100)
        (add-hook 'after-change-functions 'refac-bufsrv-load t t)
        (add-hook 'menu-bar-update-hook 'refac-refresh-menu t t)
        (add-hook 'bufsrv-message-functions 'refac-send-bufsrv-command t t)
        (add-hook 'kill-buffer-hook 'after-kill-buffer t t))
  :off (progn
		 ;(setf edit-mode-on nil)
		 (buffer-enable-undo)
         (refactorerl-buffer-server-stop)
         (remove-hook 'after-change-functions 'refac-bufsrv-load t)
         (remove-hook 'menu-bar-update-hook 'refac-refresh-menu t)
         (remove-hook 'bufsrv-message-functions 'refac-send-bufsrv-command t)
         (remove-hook 'kill-buffer-hook 'after-kill-buffer t)))


(defvar refac-buffer-server-buffer nil)
(defvar refac-buffer-server-process nil 
  "The RefactorErl buffer handler server process.")

(defvar refac-buffer-opened nil)
(defvar refac-undo-available nil)
(defvar refac-redo-available nil)
(defvar refac-undo-actual nil)
(defvar refac-redo-actual nil)


(defun refactorerl-buffer-server-start ()
  "Checks if the BufferServer is running, and starts it if neccessary."
  (when (not refactorerl-mode)
    (error "Refactorerl mode is required."))
  (when (not (refac-buffer-server-is-running))
    (let* ((base-path (file-name-as-directory refactorerl-base-path))
           (buffer-server-cmd (concat base-path "bin/BufferServer")))
           ;(server-cmd (concat base-path "bin/referl")))
	  (setf refac-buffer-server-process
            (start-process "BufferServer" refac-buffer-server-buffer buffer-server-cmd))
	  (set-process-buffer refac-buffer-server-process (get-buffer-create "BufferServer"))
      (set-process-filter refac-buffer-server-process 'refac-bufsrv-output-filter))))


(defun load-opened-files ()
  ;(dolist (buffer (buffer-list))
	(with-current-buffer (current-buffer)
						 ;(when (and buffer-file-name (not (eq refac-buffer-state :off)) (not refac-buffer-opened))
						   (buffer-disable-undo)
						   (refac-send-bufsrv-command (concat "open \"" (buffer-file-name) "\""))
						   (setq refac-buffer-opened t)
						   ));)
;)

(defun refactorerl-buffer-server-stop ()
  (when (refac-buffer-server-is-running)
    (refac-send-bufsrv-command "quit")
	(while (refac-buffer-server-is-running)
      (sleep-for 0.1))))


(defun refac-bufsrv-output-filter (proc string)
  (when (and (equal proc refac-buffer-server-process) (> (length string) 0))
  (dolist (line (split-string string "\n"))
	(let (lists)
	  (setq lists nil)
	  (setq lists (split-string line ";"))
	  (when (> (length (nth 0 lists)) 2)
		(dolist (str (split-string (substring (nth 0 lists) 1 -1) ","))
		  (let (buffer) 
			(setq buffer (get-file-buffer (substring str 1 -1)))
			;(message (concat "revert:" str))
			(if buffer
			  (with-current-buffer buffer
								   (remove-hook 'after-change-functions 'refac-bufsrv-load t)
								   (revert-buffer t t t)
								   ;(setq refac-undo-available t)
								   ;(setq refac-redo-available t)
								   (sleep-for 0 100)
								   ;(message (concat "revert: " (substring str 1 -1)))
								   (refac-send-command 'add (substring str 1 -1))
								   (add-hook 'after-change-functions 'refac-bufsrv-load t t)
								   )
			  (progn (remove-hook 'after-change-functions 'refac-bufsrv-load t)
					 (sleep-for 0 100)
					 (refac-send-command 'add (substring str 1 -1))
					 (add-hook 'after-change-functions 'refac-bufsrv-load t t))
			  )	
			)	
		  )
		)
		
	  (when (> (length (nth 1 lists)) 5)
		(let (list-undos)
		  (setq list-undos (split-string (substring (nth 1 lists) 0 ) "|"))
		  (dolist (str-undo list-undos)
			;(message str-undo)
			(let (lists2)
			  (setq lists2 (split-string str-undo ":"))
			  ;(message (nth 0 lists2))
			  (let (buffer) 
				(setq buffer (get-file-buffer (substring (nth 0 lists2) 1 -1)))
				(if buffer
				  (with-current-buffer buffer
									   (let (new-undo-list) 
										 (setq new-undo-list nil)
										 ;(setq refac-undo-available nil)
										 (dolist (str2 (split-string (substring (nth 1 lists2) 1 -1 ) ","))
										   ;(message str2)
										   (when (> (length str2) 2)
											 (add-to-list 'new-undo-list (substring str2 1 -1))
											 ))
										 (refresh-undo-list buffer new-undo-list)
										 )
									   ))))))
										 (refac-refresh-undo)
		)
	  (when (> (length (nth 2 lists)) 5)
		(let (list-redos)
		  (setq list-redos (split-string (substring (nth 2 lists) 0 ) "|"))
		  (dolist (str-redo list-redos)
			;(message str-redo)
			(let (lists2)
			  (setq lists2 (split-string str-redo ":"))
			  ;(message (nth 0 lists2))
			  ;(message (nth 1 lists2))
			  (let (buffer) 
				(setq buffer (get-file-buffer (substring (nth 0 lists2) 1 -1)))
				(if buffer
				  (with-current-buffer buffer
									   (let (new-redo-list)
										 (setq new-redo-list nil)
										 ;(setq refac-redo-available nil)
										 ;(message "ss")
										 ;(message (substring (nth 1 lists2) 1 -1 ))
										 (when (> (length (substring (nth 1 lists2) 1 -1 )) 2)
										 (dolist (str2 (split-string (substring (nth 1 lists2) 1 -1 ) ","))
										   ;(message str2)
										   (when (> (length str2) 2)
											 (add-to-list 'new-redo-list (substring str2 1 -1))
											 )))
										 ;(message (nth 0 new-redo-list))
										 (refresh-redo-list buffer new-redo-list)
										 )
									   ;(message (nth 0 refac-redo-available))
									   ))))))
										 (refac-refresh-redo)
		)
	(when (and (> (length lists) 3) (> (length (nth 3 lists)) 2))
		(let (list-renames-opened)
		  (setq list-renames-opened (split-string (substring (nth 3 lists) 0 ) ","))
		  (dolist (str-rename list-renames-opened)
			;(message str-redo)
			(let (lists2)
			  (setq lists2 (split-string str-rename "->"))
			  ;(message (nth 0 lists2))
			  (let (buffer) 
				(setq buffer (get-file-buffer (substring (nth 0 lists2) 1 -1)))
				(if buffer
				  (with-current-buffer buffer
					(remove-hook 'after-change-functions 'refac-bufsrv-load t)
					(revert-buffer t t t)
                    ;(message (substring (nth 1 lists2) 1 -1))
					;(message (concat "rename: " (substring (nth 0 lists2) 1 -1)))
					(refac-send-command 'drop (substring (nth 0 lists2) 1 -1))
                    (rename-file (substring (nth 0 lists2) 1 -1) (substring (nth 1 lists2) 1 -1))
                    (rename-buffer (substring (nth 1 lists2) 1 -1))
					(set-visited-file-name (substring (nth 1 lists2) 1 -1))
					(set-buffer-modified-p nil)
					;(save-buffer)
					(sleep-for 0 100)
    				(refac-send-command 'add (substring (nth 1 lists2) 1 -1))
					(add-hook 'after-change-functions 'refac-bufsrv-load t t)
    															 )))))))
		(when (and (> (length lists) 4) (> (length (nth 4 lists)) 2))
		(let (list-renames-closed)
		  (setq list-renames-closed (split-string (substring (nth 4 lists) 0 ) ","))
		  ;(message (nth 4 lists))
		  (dolist (str-rename list-renames-closed)
			;(message str-redo)
			(let (lists2)
			  (setq lists2 (split-string str-rename "->"))
			  ;(message (nth 0 lists2))
			  ;(message (nth 1 lists2))
    				;(refac-send-command 'drop (substring (nth 0 lists2) 0))
    				;(refac-send-command 'add (substring (nth 1 lists2) 0))
											 )))))	
	  )))


(defun after-kill-buffer ()
  (refac-send-bufsrv-command (concat "close \"" buffer-file-name "\""))
  )


(defun refac-refresh-undo ()
 (interactive)
 ;(when refac-changes-updated 
 (when (not (equal refac-undo-actual refac-undo-available))
   (with-current-buffer (current-buffer)
  (dolist (change refac-undo-actual)
  	(easy-menu-remove-item nil '("Refactor" "Undo") change)
	)
  (let (n)
  (setq n 1)
  (setq refac-undo-actual refac-undo-available)
  (dolist (change refac-undo-available)
	;(message change)
  	(easy-menu-add-item nil '("Refactor" "Undo") `[,change (lambda () (interactive) (refac-sel-undo buffer-file-name ,n))])
	(setq n (+ n 1))
	)
  ))  (message "")
  ;)
 ;(setq refac-changes-updated nil)
 ))


(defun refac-refresh-redo ()
 (interactive)
 (when (not (equal refac-redo-actual refac-redo-available))
 (with-current-buffer (current-buffer)
  (dolist (change refac-redo-actual)
  	(easy-menu-remove-item nil '("Refactor" "Redo") change)
	)
  (let (n)
  (setq n 1)
  (setq refac-redo-actual refac-redo-available)
  (dolist (change refac-redo-available)
	;(message change)
  	(easy-menu-add-item nil '("Refactor" "Redo") `[,change (lambda () (interactive) (refac-sel-redo buffer-file-name ,n))])
	(setq n (+ n 1))
	)
  ))  (message "")
  ;)
 ;(setq refac-changes-updated nil)
 ))

(defun refac-refresh-menu ()
 (interactive)
 (refac-refresh-undo)
 (refac-refresh-redo)
 )


(defun refac-bufsrv-load (begin end len)
  ;(setq refac-undo-available t)
  (refac-send-bufsrv-command "end-update")
  (write-region nil nil (make-backup-file-name (buffer-file-name)))
  ;(setq refac-buffer-edited t)
  (refac-send-bufsrv-command (concat "load-edited \"" (make-backup-file-name (buffer-file-name)) "\" \"" (buffer-file-name) "\""))
  (message "")
  )

(defun refac-buffer-server-is-running ()
  "Checks if the BufferServer is running."
  (and refac-buffer-server-process
       (equal 'run (process-status refac-buffer-server-process))))

;(defvar edit-mode-on nil)

(defun refac-send-bufsrv-command (cmd)
  	;(if edit-mode
    (process-send-string refac-buffer-server-process (concat cmd "\n")))
;)

(defvar bufsrv-message-functions nil 
  "")

(defun refactorerl-buffer-undo ()
  "Steps backward on the buffer state."
  (interactive)
  ;(setq refac-redo-available t)
  (refac-send-bufsrv-command "end-update")
  (sleep-for 0 100)
  (refac-send-bufsrv-command (concat "undo \"" buffer-file-name "\" 0"))
  )

(defun refactorerl-buffer-redo ()
  "Steps forward on the buffer state."
  (interactive)
  ;(setq refac-undo-available t)
  (refac-send-bufsrv-command "end-update")
  (sleep-for 0 100)
  (refac-send-bufsrv-command (concat "redo \"" buffer-file-name "\" 0"))
  )

(defun refresh-undo-list (buffer new-list)
 (interactive)
 (with-current-buffer buffer
  ;(dolist (change refac-undo-available)
  ;	(easy-menu-remove-item nil '("Refactor") "Undo") change)
;	)
  	;(easy-menu-add-item nil '("Refactor") `["Undo" (lambda () (interactive) ())])
  	;(easy-menu-add-item nil '("Refactor" "Undo") `["last change" (lambda () (interactive) (refactorerl-buffer-undo ))])
  (setq refac-undo-available (reverse new-list))
  ;(let (n)
  ;(setq n 1)
  ;(dolist (change refac-undo-available)
	;(message change)
  ;	(easy-menu-add-item nil '("Refactor" "Undo") `[,change (lambda () (interactive) (refac-sel-undo buffer-file-name ,n))])
;	(setq n (+ n 1))
;	)
  ;)
 ))

(defun refresh-redo-list (buffer new-list)
  (interactive)
 (with-current-buffer buffer
  ;(dolist (change refac-redo-available)
  ;	(easy-menu-remove-item nil '("Refactor" "Redo") change)
;	)
  (setq refac-redo-available (reverse new-list))
 ; (let (n)
  ;(setq n 1)
  ;(dolist (change refac-redo-available)
  ;	(easy-menu-add-item nil '("Refactor" "Redo") `[,change (lambda () (interactive) (refac-sel-redo buffer-file-name ,n))])
;	(setq n (+ n 1))
;	)
  ;)
 ))

(defun refac-sel-undo (name n)
  (interactive)
  ;(message name)
  ;(message (number-to-string n))
  (refac-send-bufsrv-command (concat "undo \"" name "\" " (number-to-string n)))
  )

(defun refac-sel-redo (name n)
  (interactive)
  ;(message name)
  ;(message (number-to-string n))
  (refac-send-bufsrv-command (concat "redo \"" name "\" " (number-to-string n)))
  )

