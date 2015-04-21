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
(require 'overlay)
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))

(provide 'refactorerl-ui)

(defvar refac-output-buffer "")

(defconst +refac-buffer-list+ "*RefactorErl Results*")
(defun refac-buffer-list-ensure ()
    (or (get-buffer +refac-buffer-list+)
        (with-current-buffer (get-buffer-create +refac-buffer-list+)
          (toggle-read-only t)
          (use-local-map
           (let ((map widget-keymap))
             (set-keymap-parent map refactorerl-mode-map)
             map))
          (widget-setup)
          (current-buffer))))

(defun refac-buffer-get-new-or-default ()
    (with-current-buffer (generate-new-buffer +refac-buffer-list+)
          (toggle-read-only t)
          (use-local-map
           (let ((map widget-keymap))
             (set-keymap-parent map refactorerl-mode-map)
             map))
          (widget-setup)
          (current-buffer)))

(defmacro* with-refac-buffer-list (&body body)
  `(with-current-buffer (refac-buffer-list-ensure)
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

(defmacro* with-refac-buffer (buffer &body body)
  `(with-current-buffer buffer
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

(defmacro* async-with-refac-buffer-list (&body body)
  `(let ((buf (refac-buffer-list-ensure)))
     (with-current-buffer buf
       (let ((inhibit-read-only t))
         (erase-buffer))
       ,@body
       (pop-to-buffer buf t))))

;; Server buffer manipulation
;(defvar refac-buffer-server-buffer nil)
(defvar refac-server-buffer nil)

(defun refac-make-server-buffer (&rest args)
  (set-buffer (get-buffer-create "*RefactorErl*"))
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert "**RefactorErl Server Control Buffer**\n")
  (widget-create 'push-button
                 :notify 'refac-show-config
                 "Configuration")
  (widget-insert "\n")
  (set (make-local-variable 'config-start-marker) (point-marker))
  (widget-insert "------------------------------------------------\n")
  (widget-create 'push-button
                 :notify (lambda (&rest args) (refactorerl-server-reset-db))
                 "Reset database")
  (widget-create 'push-button :notify (lambda (&rest args) (refactorerl-list-files))
                 "Show files")
  (widget-insert "\n")
  (widget-create 'push-button :notify (lambda (&rest args) (refactorerl-show-errors))
                 "Show parse errors")
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify 'refac-make-server-buffer
                 "Clear buffer")
  (widget-insert "\n================================================\n")
  (use-local-map
   (let ((map widget-keymap))
     (set-keymap-parent map refactorerl-mode-map)
     map))
  (widget-setup)
  (current-buffer))

(defun refac-show-config (&rest args)
  (refac-send/callbacks ('showconfig)
                          (:reply (ok config)
                                  (refac-handle-showconfig config))))


(defun refac-handle-showconfig (config)
  (with-current-buffer refac-server-buffer
    (set (make-local-variable 'config-inc-dir) nil)
    (set (make-local-variable 'config-app-dir) nil)
    (set (make-local-variable 'config-out-dir) "/tmp")
    (if (equal config "") (setq config nil))
    (dolist (cfg config)
      (cond ((eq (elt cfg 0) 'appbase)
             (add-to-list 'config-app-dir (elt cfg 1)))
            ((eq (elt cfg 0) 'include)
             (add-to-list 'config-inc-dir (elt cfg 1)))
            ((eq (elt cfg 0) 'output)
             (setq config-out-dir (elt cfg 1)))))
    (goto-char config-start-marker)
    (widget-insert "Application directories:\n")
    (set (make-local-variable 'appdir-list)
         (widget-create 'editable-list
                        :value config-app-dir
                        '(editable-field)))
    (widget-insert "\nInclude directories:\n")
    (set (make-local-variable 'incdir-list)
         (widget-create 'editable-list
                        :value config-inc-dir
                        '(editable-field)))
    (widget-insert "\n")
    (set (make-local-variable 'outdir-menu)
         (widget-create 'menu-choice
                        :value config-out-dir
                        :tag "Output directory"
                        '(const :tag "Original" original)
                        '(editable-field :menu-tag "Specify" "")))
    (widget-insert "\n")
    (set (make-local-variable 'save-button)
         (widget-create 'push-button
                        :notify 'refac-save-config
                        "Save"))
    (set (make-local-variable 'cancel-button)
         (widget-create 'push-button
                        :notify 'refac-hide-config
                        "Cancel"))
    (widget-insert "\n")
    (set (make-local-variable 'config-end-marker) (point-marker))))

(defun refac-hide-config (&rest args)
  (widget-delete appdir-list)
  (widget-delete incdir-list)
  (widget-delete outdir-menu)
  (widget-delete save-button)
  (widget-delete cancel-button)
  (let ((inhibit-read-only t))
    (delete-region config-start-marker config-end-marker))
  (set-marker config-end-marker nil))



(defvar refac-ephemeral-overlays (list))
(defvar refac-stacked-overlay-groups (list))

(defun refac-remove-overlays ()
  (mapc #'delete-overlay refac-ephemeral-overlays)
  (setf refac-ephemeral-overlays (list)))

(defun refac-start-overlay-group ()
  (push (list) refac-stacked-overlay-groups))

(defun refactorerl-pop-overlay-group ()
  (interactive)
  (let ((overlay-group (pop refac-stacked-overlay-groups)))
    (mapc #'delete-overlay overlay-group)))

(defun refac-remove-stacked-overlays ()
  (loop while refac-stacked-overlay-groups
        do (refac-pop-overlay-group)))

;;; We don't want the overlay to be removed as soon as the visiting
;;; callback finishes, because that would amount to not showing the
;;; overlay at all. So the idea is to "wait for a whole day" after
;;; adding an overlay -- effectively suspending the execution of
;;; post-command-hook until the next command.
;(defconst +really-long-time+ 86400)

(defun* refac-flash-overlay (start-pos end-pos &optional (face 'refactorerl-highlight))
  (let ((overlay (make-overlay start-pos end-pos)))
    (overlay-put overlay 'face face)
    (sit-for 3)
    (delete-overlay overlay)
    ))

(defun* refac-add-overlay-to-group (start-pos end-pos &optional (face 'refactorerl-highlight))
  (let ((overlay (make-overlay start-pos end-pos)))
    (overlay-put overlay 'face face)
    (push overlay (car refac-stacked-overlay-groups))))

(defun refactorerl-configure ()
  (interactive)
  (customize-group 'refactorerl))

(defun refac-highlight-txt (start end)
    (interactive)
  (let ((x (make-overlay start end)))
  (overlay-put x 'face '(:background "orange"))))
