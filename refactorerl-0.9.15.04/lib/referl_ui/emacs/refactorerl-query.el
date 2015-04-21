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

(provide 'refactorerl-query)
(require 'refactorerl-def)
(eval-when-compile
  (require 'cl))

(defmacro* define-refac-query/point ((name &key menu key) query)
  `(define-refac-operation (,name :menu ,menu :key ,key :menu-group query :precondition :buffer-state)
     ((pos :point))
     (refac-send/callbacks ('transform 'semantic_query (list
      (vector 'querystr ,query)
      (vector 'start_opt (list
       (vector 'ask_missing 'false)
       (vector 'file buffer-file-name)
       (vector 'position pos)))
      (vector 'display_opt '([positions scalar] [output msg]))))
                          (:reply (ok queryres) ;; str comes from refac send callbacks when
                                                ;; asking for a query
                                  (refac-handle-query-res queryres qstr)))))

(define-refac-operation (refactorerl-semantic-query :menu "Run query"
                                                    :key "sq"
                                                    :menu-group query
                                                    :precondition :server)
  ((pos :point) (query "Query"))
  "Runs a semantic query."
     (refac-send/callbacks ('transform 'semantic_query (list
      (vector 'querystr query)
      (vector 'start_opt (list
       (vector 'ask_missing 'false)
       (vector 'file buffer-file-name)
       (vector 'position pos)))
      (vector 'display_opt '([positions scalar] [output msg]))))
                          (:reply (ok queryres)
                                  (refac-handle-query-res queryres qstr))))

(define-refac-operation (refactorerl-running-queries :menu "Running queries"
                                                    :key "sr"
                                                    :menu-group query
                                                    :precondition :server)
  ()
  "Shows running queries."
     (refac-send/callbacks ('get_running_queries 'str_output)
                          (:reply (ok queries)
                                  (progn (pop-to-buffer "Query list")
                                  (erase-buffer)
                                  (widget-insert "Running queries:\n")
                                  ;;(message (prin1-to-string queries)) (widget-insert (prin1-to-string queries))
                                  (cond 
                                    ((listp queries)
                                        (dolist (query queries)
                                            (if (stringp query)
                                                    (widget-insert query)
                                                    (widget-insert (prin1-to-string query))
                                            )
                                            (widget-insert "\n"))
                                    )
                                    ((equal "" queries)
                                        (widget-insert "No running queries.\n")
                                    )
                                    (t (widget-insert (prin1-to-string queries))
                                    )
                                  )
                                            ))))

(define-refac-operation (refactorerl-kill-semantic-query :menu "Kill running query"
                                                    :key "sk"
                                                    :menu-group query
                                                    :precondition :server)
  ((query-id "Query id"))
  "Kills a running query."
  (progn 
     ;; (message (prin1-to-string query-id))
     (refac-send/callbacks ('kill_query (string-to-number query-id))
                          (:reply (ok reply)
                                  (case reply
                                    ('not_found (message "Query not found."))
                                    (otherwise (message "Query killed."))
                                    )))))

(define-refac-operation (refactorerl-show-metrics-bad-smells :menu "Show bad smells"
                                                    :key "sb"
                                                    :menu-group query
                                                    :precondition :server)
  ()
  "Shows bad smells."
     (refac-send/callbacks ('metric_mode 'show) ;('transform 'query_badsmells ())
                           (:reply (ok badsmells)
                                (progn
                                    (setf refac-buffer-state-metrics 'true)
                                    (refac-handle-query-res 
                          (list 'result (list (list 'result badsmells))) ))) ))


(define-refac-query/point (refactorerl-goto-def :menu "Go to definition" :key ("\M-."))
  "@def")

(define-refac-query/point (refactorerl-find-funrefs :menu "Find function references" :key "qr")
  "@fun.refs")

(define-refac-operation (refactorerl-metric-query   :menu "Run metric query"
                                                    :key "mq"
                                                    :menu-group query
                                                    :precondition :server)
  ((mquery "Metric Query"))
  "Runs a metric query."
     (refac-send/callbacks ('transform 'metric_query (list
      (vector 'querys mquery)))
                          (:reply (ok queryres)
                                  (refac-handle-metric-res queryres))))

(defun refac-handle-metric-res (args)
 (refac-handle-res 'show-metric-res args)
)
(defun refac-handle-query-res (args &rest rest)
 (refac-handle-res 'show-query-res args (car rest))
)

(defun show-metric-res (string)
  (display-buffer (refac-buffer-list-ensure))
;  (refac-debug
;  (message "string: %s" string))
  (with-refac-buffer-list
   (erase-buffer)
   (widget-insert string "\n")))

(defun refac-handle-res (handler args &rest string)
  (setq type (elt args 0))
  (setq data (elt args 1))
  (case type
    ('result
      (progn
       (setq results nil)
       (loop for kv in data when (equal (elt kv 0) 'result)
             do (setq results (elt kv 1)))
       (if (or (equal results "") (not results))
        (message "RefactorErl: No results.")
        (apply handler (cons results string)))))
    ('abort (let ((text (elt data 1)))
             (message "RefactorErl: denied: %s" text)))
    ('error (let ((text (elt data 1)))
             (message "RefactorErl: error: %s" text)))
   (otherwise         (message "RefactorErl: unknown error: %s" args))))

(defvar refac-find-refs-popup t)
(defvar refac-find-refs-highlight nil)

(defun show-query-res (results &rest rest)
    (setf string (car rest))
    (cl-flet ((result-no-pos (result)
                             (equal (elt result 0) 'nopos)))
      (let ((highlight (or refac-find-refs-highlight
                           ;; (= (length (remove-if #'result-no-pos results)) 1)
                           ))
            (buffer (refac-buffer-get-new-or-default)))
        (when refac-find-refs-popup
          (with-refac-buffer buffer
           (erase-buffer)
           ;; (erlang-font-lock-init)
           ;; (erlang-font-lock-level-3)
           ))
        (when highlight
          (refac-start-overlay-group))
	(with-refac-buffer buffer
                           (when string
                             (widget-insert "Query: ")
                             (widget-insert string)
                             (widget-insert "\n")
                             (widget-insert "Result:\n\n")))
        (dolist (result results)
          (if (result-no-pos result)
              (when refac-find-refs-popup
                (with-refac-buffer buffer
                                   (widget-insert (elt result 1))))
            (destructuring-bind ((file start-pos end-pos) text)
                (list-from-vector result)
              (when refac-find-refs-popup
                (with-refac-buffer
		 buffer
		 (refac-widget-link file :start-pos start-pos :end-pos (1+ end-pos) :label text)))
                                        ;:start-line start-pos ; only while start-line is not available
	    (when highlight
	      (refac-highlight file :start-pos start-pos :end-pos (1+ end-pos)))
	    )))
      (when refac-find-refs-popup
	(pop-to-buffer buffer)))))
