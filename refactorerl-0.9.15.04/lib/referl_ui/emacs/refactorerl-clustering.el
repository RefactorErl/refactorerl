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


(provide 'refactorerl-clustering)

(defun refactorerl-full-cluster ()
  (interactive)
  (setf clustering-running t)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'ask_missing 'true)))))
                   
(defun refactorerl-cluster-agglom-mod ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'ask_missing 'true)
                   (vector 'algorithm 'agglom_attr)
                   (vector 'cluster_options (vector 'entities 'module))))))

(defun refactorerl-cluster-genetic-mod ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'ask_missing 'true)
                   (vector 'algorithm 'genetic)
                   (vector 'cluster_options (vector 'entities 'module))))))

(defun refactorerl-cluster-agglom-fun ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'ask_missing 'true)
                   (vector 'algorithm 'agglom_attr)
                   (vector 'cluster_options (vector 'entities 'function))))))
                   
(defun refactorerl-cluster-genetic-fun ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'ask_missing 'true)
                   (vector 'algorithm 'genetic)
                   (vector 'cluster_options (vector 'entities 'function))))))

(defvar cluster-ui-buffer nil)
  
(defun cluster-ui-options-agglom()
  (refac-send-command 'cl_options 'agglom_attr))


;Buffer of the clustering
(defvar cluster-ui-result-buffer nil)
(defun res-apply-aggl (&rest args)
  (let ((value)
        (algo alg)
        (createdb (widget-value create))
       )
    (dolist (elt cl-options-list value)
      (setq value (cons (widget-value elt) value)))
    (setq cluster-ui-result-buffer
          (generate-new-buffer "*Clustering*"))
    (switch-to-buffer cluster-ui-result-buffer)
    (refac-send-command 'run_cl (reverse value) algo createdb)))

(defun cluster-ui-refresh (&rest args)
   (refac-send-command 'cl_refresh))


(defun cluster-ui-cleanup (&rest args)
  (delete-window (get-buffer-window cluster-ui-buffer))
  (kill-buffer cluster-ui-buffer))
