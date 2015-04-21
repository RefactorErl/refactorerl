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

(provide 'refactorerl-flasher)
(require 'eldoc)
(eval-when-compile
  (require 'cl))

(defun refactorerl-eldoc ()
  ;; TODO: Call async context-sensitive info getter
  nil)

(defun refactorerl-flasher (enablep)
  (if (not enablep)
      (eldoc-mode 0)
    (make-local-variable 'eldoc-documentation-function)
    (setf eldoc-documentation-function 'refactorerl-eldoc)
    (eldoc-mode 1)))
