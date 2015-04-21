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

(provide 'refactorerl-customization)

;; Customization variables

(defgroup refactorerl nil
  "Erlang Refactorer"
  :tag "RefactorErl"
  :group 'tools)

(defcustom refactorerl-base-path ""
  "RefactorErl base directory."
  :type 'directory
  :group 'refactorerl)

(defcustom refactorerl-erlang-runtime "erl"
  "Erlang emulator command to be used with RefactorErl."
  :type 'string
  :group 'refactorerl)

(defcustom refactorerl-data-dir "~/.refactorerl"
  "Directory that is used to store RefactorErl data."
  :type '(choice (const :tag "Base directory" base)
                 (directory :tag "Given directory"))
  :group 'refactorerl)

(defun refactorerl-data-dir ()
  (file-name-as-directory
   (case refactorerl-data-dir
     ((base)
      refactorerl-base-path)
     (otherwise
      refactorerl-data-dir))))

(defcustom refactorerl-server-type 'shell
  "Specifies how to start the RefactorErl server process.
If its value is internal, the server process is managed internally.
It its value is external, the server should be started manually.
If its value is shell, a separate process is started by Emacs, with its
  Erlang shell accessible through the buffer *RefactorErlShell*."
  :type '(choice (const :tag "Internally managed server" internal)
                 (const :tag "Externally started server" external)
                 (const :tag "Managed server with shell access" shell))
  :group 'refactorerl)

;; (defcustom refactorerl-wrangler-path 'disabled
;;   "Wrangler installation directory."
;;   :type '(choice (const :tag "No wrangler installation available" disabled)
;;                  (directory :tag "Directory"))
;;   :group 'refactorerl)

(defcustom refactorerl-sname-type 'short
  "The name convention with the server and the client is
started. Since the interface is connencted to an Erlang node both
have to be started with the same name conventions."
  :type '(choice (const :tag "Short name" short)
                 (const :tag "Fully qualified name" long))
  :group 'refactorerl)

(defcustom refactorerl-undo-mode 'one-step
  "The mode of undo/redo operation."
  :type '(choice (const :tag "One step only" one-step)
                 (const :tag "Selective undo" selective))
  :group 'refactorerl)

(defcustom refactorerl-server-name "refactorerlshell@localhost"
  "The name of the RefactorErl server. Attention: server should
be started with fully qualified name!"
  :type 'string
  :group 'refactorerl)

(defcustom refactorerl-client-name "emacs@localhost"
  "The name of the RefactorErl client. Note: Client is started
with fully qualified name! This name should be UNIQUE!"
  :type 'string
  :group 'refactorerl)

(defcustom refactorerl-db-type 'Mnesia
  "Type of database to use. Only matters if the tool is started from emacs."
  :type '(choice (const :tag "Mnesia" Mnesia)
                 (const :tag "Nif graph" Nif)
                 (const :tag "Kyoto cabinet" Kyoto))
  :group 'refactorerl)

(defgroup refactorerl-faces nil
  "Faces used by RefactorErl"
  :group 'refactorerl)

(defface refactorerl-header '((t (:inherit bold)))
  "Face used by RefactorErl for section headers"
  :group 'refactorerl-faces)

(defface refactorerl-error '((t (:inherit bold :foreground "red")))
  "Face used by RefactorErl for errors"
  :group 'refactorerl-faces)

(defface refactorerl-link '((t (:foreground "blue" :underline t)))
  "Face used by RefactorErl for links"
  :group 'refactorerl-faces)

(defface refactorerl-highlight '((t (:background "darkseagreen2")))
  "Face used by RefactorErl for highlights"
  :group 'refactorerl-faces)

