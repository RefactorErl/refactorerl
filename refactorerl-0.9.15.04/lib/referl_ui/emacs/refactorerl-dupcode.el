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

(provide 'refactorerl-dupcode)

(require 'ediff)
(require 'ediff-util)
(require 'ediff-wind)

(require 'cl)
(require 'widget)
(require 'refactorerl-depanal)
(require 'refactorerl-form)

(eval-when-compile
  (require 'wid-edit))

(defun refactorerl-show-dupcode ()
  "Function to call refactorerl show dupcode function"
  (interactive)
  (setf code-to-show (car (read-from-string (read-from-minibuffer "Name of the duplicated code to show:"))))
  (refac-send/callbacks ('get_dupcode_result code-to-show :ui_format :linecol)
	 (:reply (ok result)
	 	(if (equal result "")
	 	 (print "No duplicated code with this name")
	     (process-dupcode-info result))))
  ) ;get_dupcode_result

(defun refactorerl-dupcode-selected-st ()
  (interactive)
  (refactorerl-dupcode-selected 'suffix_tree)
)

(defun refactorerl-dupcode-selected-mx ()
  (interactive)
  (refactorerl-dupcode-selected 'matrix)
)

(defun refactorerl-dupcode-selected-swm ()
  (interactive)
  (refactorerl-dupcode-selected 'sw_metrics)
)

(defun refactorerl-dupcode-selected-mxf ()
  (interactive)
  (refactorerl-dupcode-selected 'matrixfilter)
)

(defun refactorerl-dupcode-selected-fst ()
  (interactive)
  (refactorerl-dupcode-selected 'filtered_suffix_tree)
)

(defun refactorerl-dupcode-selected (algorithm)
  (message (concat "Searching in area: "
  				   (prin1-to-string (region-beginning)) 
  				   "-" 
  				   (prin1-to-string (region-end))))
  (setf modulename (buffer-file-name))
  (refac-send/callbacks ('clone_identifierl_by_pos algorithm modulename (region-beginning) (region-end) :linecol :ui_format)
	 (:reply (ok result)
	 	(if (equal result "")
	 	 (print "No duplicated code found")
	     (process-dupcode-info result))))
)

(defvar globally-used-algorithm)

(defvar algorithmvar)
;; Advanced dupcode search
(defun refactorerl-dupcode-advanced ()
  "Widget to set dupcode options"
  (interactive)
  (switch-to-buffer "*Duplicated code analysis algorithms*")
  (kill-all-local-variables)         
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))
  (refac-send/callbacks ('get_algorithms)
    (:reply (ok result)
    	(setq widg (to-elisp-choicelist result))
    	(create-widgets widg)
		))
)

(defun to-elisp-choicelist (lst)
	(mapcar 'to-a-choice lst)
)

(defun to-a-choice (element)
	(setf key (nth 0 element))
	(setf data (nth 1 element))
	(setf label (getprop data 'label))
	`(quote (item :tag ,label :value ,key))
)

(defun getprop (alist prop)
	(car (assoc-default prop alist))
)

(defun create-widgets (algs)
  (interactive)
  (progn
  	  (make-local-variable 'selected-alg)
	  (setf alg (eval (car algs)))
      (destructuring-bind (item tagsym tag valsym val) alg
  		(setf initial-value val))
	  (widget-insert "\nAlgorithm: \n")
	  (setq menu-choice-placer 
	  		`(widget-create 'menu-choice
			  		  :tag "Choose"
			  		  :value initial-value
				      :help-echo "Choose an algorithm, please!"
				      :notify (lambda (widget &rest ignore)
				                (widget-setup))
				      ,@algs))
	  (setq chooser-widget (eval menu-choice-placer))
	  (widget-insert "\n\n")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
			 		(setq selected-alg (widget-value chooser-widget))
			 		(setf buffer (current-buffer))
			 		;(kill-buffer buffer)
			 		(setf globally-used-algorithm selected-alg)
				    (refactorerl-dupcode-advanced-with-algorithm selected-alg))
			 "Select algorithm")
	  (widget-insert " ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore) 
				   (kill-this-buffer))
			 "Cancel")
	  (use-local-map widget-keymap)
	  (widget-setup)
  )
)

(defun refactorerl-dupcode-advanced-with-algorithm (algorithm)
	"Generate widgets for a specific algorithm"
    (refac-send/callbacks ('get_algorithm_data algorithm)
	    	(:reply (ok result)
	    			(switch-to-buffer "*Duplicated code analysis*")
					(kill-all-local-variables)         
					(let ((inhibit-read-only t))
					  (erase-buffer))
					(let ((all (overlay-lists)))
					  ;; Delete all the overlays.
					  (mapcar 'delete-overlay (car all))
					  (mapcar 'delete-overlay (cdr all)))
	    			(setf dup-widgets (mapcar 'process-dupwidget-info result))
	    			(widget-create 'push-button
							 :notify (lambda (&rest ignore)
							 			(setf buffer (current-buffer))
							 			;(kill-buffer buffer)
								    	(setf options (refactorerl-get-widget-data dup-widgets globally-used-algorithm))
								    	(setf arglist (makearglist (eliminate-empty-options options)))
								    	;;(message (concat "Options sent: " (prin1-to-string arglist)))
										(refac-send/callbacks ('clone_identifierl arglist)
											(:reply (ok result)
												(message "Processing duplicated code analysis results")
												(process-dupcode-info result)))
											)
							 "Run")
					(widget-insert "   ")
					(widget-create 'push-button
							 :notify (lambda (&rest ignore) 
								   (kill-this-buffer))
							 "Cancel")
					(use-local-map widget-keymap)
					(widget-setup)))
)

(defun radio-choice-list-from-enum (enum-options)
	(mapcar 'to-a-radio-item enum-options)
)

(defun to-a-radio-item (enum-item)
	`(quote (choice-item :format ,(concat (prin1-to-string enum-item) " ") ,enum-item))
)

(defun process-dupwidget-info (widget-info)
	(destructuring-bind (atom props) widget-info
		(setf wkey (getprop props 'key))
		(setf wlabel (getprop props 'label))
		(setf wdefault (getprop props 'default))
		(setf wtype (getprop props 'type))
		(setf woptions (getprop props 'options))
		(setf wenumtype (getprop props 'enumtype))
		(case wtype
			('enum
				(setf choice-list (radio-choice-list-from-enum woptions))
			    (widget-insert wlabel)
				(widget-insert "\n")
  				(setf widget-for-enum `(widget-create 'radio-button-choice
  										:value ',wdefault
					       				,@choice-list))	
  				(setf a-widget (eval widget-for-enum))
			    (widget-insert "\n\n")
			    (list wkey wtype wenumtype a-widget))
			('atoms
			    (widget-insert (concat wlabel " (elements separated with commas)"))
				(widget-insert "\n")
			    (setf a-widget (widget-create 'editable-field
							  	   :size 13
							       :value wdefault))
			    (widget-insert "\n\n")
			    (list wkey wtype wenumtype a-widget))
			('boolean   
				(widget-insert wlabel)
				(widget-insert "\n")
  				(setf a-widget (widget-create 'radio-button-choice
				       				:value wdefault
				       				'(choice-item :format "True \t " true) 
				       				'(choice-item :format "False\n" false)))
  				(widget-insert "\n\n")
  				(list wkey wtype wenumtype a-widget))
			(otherwise  ;;'float 'integer 'atom
				(widget-insert (concat wlabel " (" (prin1-to-string wtype) ")"))
				(widget-insert "\n")
  				(setf a-widget (widget-create 'editable-field
				     				:size 13
				     				:value (to-string-if-needed wdefault)))
  				(widget-insert "\n\n")
  				(list wkey wtype wenumtype a-widget))))
)

(defun to-string-if-needed (object)
	(cond ((stringp object) object)
		  ((eq nil object) "")
		  (t (prin1-to-string object))
	)
)

(defun get-widget-value (dup-widgets-with-info)
	(destructuring-bind (key type enumtype widget) dup-widgets-with-info
		(progn
			(setf value (widget-value widget))
			(setf vkey (atom-maker `,key))
			(case type
				('atoms   `(,vkey ,(mapcar 'str-to-atom (to-entity-list value))))
				('integer `(,vkey ,(string-to-integer value)))
				('float   `(,vkey ,(string-to-float value)))
				('enum
					(case enumtype
						('string `(,vkey ,value))
						('atom    `(,vkey ,(atom-maker value)))))
				('atom    `(,vkey ,(str-to-atom value)))
				('boolean `(,vkey ,(atom-maker value)))
				(otherwise nil)
			)
		))
)

(defun string-to-float (str)
	(string-to-something 'floatp 'float str))

(defun string-to-integer (str)
	(string-to-something 'integerp 'floor str))

(defun string-to-something (predicate convert str)
	(setf num (string-to-number str))
	(if (eval `(,predicate num))
		num
		(eval `(,convert num))))

(defun refactorerl-get-widget-data (dup-widgets-with-info alg-key)
	(setf options
		(append
			(mapcar 'get-widget-value dup-widgets-with-info)
			(default-options alg-key)))
	;; (message (concat "Options in widgets: " (prin1-to-string options)))
	options
)

(defun default-options (alg-key)
	`((:format, :ui_format)
	  (:postype :linecol)
	  (:algorithm ,(atom-maker alg-key)))
)

(defun name-option-emptyp (optionlist)
	(interactive)
	(eval `(or ,@(mapcar (lambda (option)
					(if option
						(destructuring-bind (key prop) option
							(eq prop :))
						(not option)))
				   optionlist)))
)

(defun eliminate-empty-options (optionlist)
	(interactive)
	(eval `(append ,@(mapcar (lambda (option)
						(if option
						      (destructuring-bind (key prop) option
									(if (find prop (list : ""))
										nil
						       			`(list ',option)))
							  nil))
				optionlist)))
)


(defun process-dupcode-info (dinfo)
	"Processes duplicated code info send by RefactorErl, generates links for duplicates
	and shows the number of them and creates a buffer for this"
	(setf saved-as (car (assoc-default 'clone_name dinfo)))
	(setf list (car (assoc-default 'detected_clones dinfo)))
	(if (eq list "") 
		(setf list nil)
		nil)
	(switch-to-buffer "Duplicated code result")		
	(let ((inhibit-read-only t))
		   (erase-buffer)
		   (set (make-local-variable 'last-file-dir) ""))
	(widget-insert "\n *** Duplicated code information: *** \n\n")
	(let ((ln (length list))) (widget-insert (concatenate 'string (prin1-to-string ln) " duplicates found. \n\n")))
	(widget-insert (concatenate 'string "Saved as " (prin1-to-string saved-as) "\n\n"))
	(lexical-let ((i 1) (thisbuf (current-buffer))) nil
	(dolist (res list)
		(progn
	 	(lexical-let ((ii i) (rs res)) (widget-create 'link
				 :notify (lambda (&rest ignore)
				   (dupcode-window-open rs thisbuf))
		 (concatenate 'string (number-to-string i) ". duplicated code")))
		(widget-insert "\n")
		(setf i (+ i 1)))))
    (use-local-map widget-keymap)
	(widget-setup)
	(toggle-read-only 0)
	(delete-other-windows)
)


(defvar code-A nil)
(defvar code-B nil)

(defun dupcode-window-open (result all-duplicates-buf)
	;; (message (prin1-to-string result))
	(setq items 
			(loop for infopiece in result 
						collect (destructuring-bind ((fn filename) (st (stposl stposc)) (end (endposl endposc))) infopiece 
	 			`(quote (item :tag ,filename :value ((fn ,filename) (st (,stposl ,stposc)) (end (,endposl ,endposc)))))))) ;(nds ,,nodes)
	(progn  
	 	(delete-other-windows)
	 	(setq wind-ctrl (selected-window))
	 	(split-window-vertically)
	 	(other-window 1)
	 	(setq wind-A (selected-window))
	 	(split-window-vertically)
	 	(other-window 1)
	 	(setq wind-B (selected-window))
	  	(setf buf-ctrl (get-buffer-create "CtrlBuf"))
	 	(setf buf-A (get-buffer-create "BufA"))
	 	(setf buf-B (get-buffer-create "BufB"))
		(set-window-buffer wind-A buf-A)
		(set-window-buffer wind-B buf-B)
		(select-window wind-ctrl)
		(switch-to-buffer buf-ctrl)
        (erase-buffer)
		  ;(setf def_value `(nth 4 (nth 1 (car items)))) ;(nth 1 (nth 2 (nth 1 (car items)))))
		(widget-insert "Please select the codes you would like to compare with the following lists! \n \n")
		(eval (dupcode-file-chooser "Code A's list:" 'code-A items))
		(eval (dupcode-file-chooser "Code B's list:" 'code-B items))
  		(widget-create 'push-button
		 :notify (lambda (&rest ignore) 
					(if (and code-A code-B)
						(progn (dupcode-window-load-texts buf-A buf-B wind-A wind-B code-A code-B))
					))
		 "Load code and highlight")
		(widget-insert "\t")
		(lexical-let ((all-dup-buf all-duplicates-buf))
	  		(widget-create 'push-button
			 :notify (lambda (&rest ignore)
			 					(kill-buffer buf-ctrl)
			 					(delete-other-windows)
							 	(switch-to-buffer all-dup-buf))
			 "Back")
			(use-local-map widget-keymap))
			(widget-setup) 
		(fit-window-to-buffer wind-ctrl)
	)
)

(defun dupcode-file-chooser (label var items)
	(interactive)
	(lexical-let ((vr var) (items items) (val (nth 1 (car (nth 4 (nth 1 (car items)))))))
	(concatenate 'list
	`(widget-create 'menu-choice
		 :tag ,label
		 :value nil
		 :help-echo "Choose code duplicates!"
		 :notify '(lambda (widget &rest ignore)
					(setf ,vr (widget-value widget)))
		 )
	 items
	))
)

(defun dupcode-window-load-texts (buf-A buf-B wind-A wind-B code-A code-B)
	(progn
		(select-window wind-A)
		(destructuring-bind ((fn filename) (st (stposl stposc)) (end (endposl endposc))) code-A 
             (load-dup-file "A_BUF" filename stposl stposc endposl endposc))
		(select-window wind-B)	
		(destructuring-bind ((fn filename) (st (stposl stposc)) (end (endposl endposc))) code-B 
			(load-dup-file "B_BUF" filename stposl stposc endposl endposc))
	)
)

(defun load-dup-file (bufname file stposl stposc endposl endposc)
  "Load a file into a buffer, and highlight an area of it"
  (progn
  (setq buff (get-buffer-create bufname))
  (set-buffer buff)
  (erase-buffer)
  (insert-file-contents file)
  (switch-to-buffer buff)
  (refac-highlight-txt (- (point-from-pos stposl stposc) 1) 
                          (point-from-pos endposl endposc))
  buff))

(defun atom-maker (key)
 (str-to-atom (symbol-name key)))

(defun str-to-atom (str)
 (intern (concat ":" str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNUSED

(defun jump-to-dup (result)
	 (let ((inhibit-read-only t))
	       (erase-buffer)
	       (set (make-local-variable 'last-file-dir) ""))
	 (widget-insert "Duplicated code in the following files: \n")
	 (dolist (dupcode result)
	 	(progn
	 		(destructuring-bind ((fn filename) (st (stposl stposc)) (end (endposl endposc)) (nds nodes)) dupcode 
	 			(refac-widget-link filename :start-line stposl :start-col stposc :end-line endposl :end-col endposc))
	 		(widget-insert "\n")
	 	))
)

(defun ifnotextremal-int (int-elem ret)
  	"If a string is not -1 then it returns ret in a list (cause of append), 
  	else it returns nil"
  	(interactive)
  	(if (equal int-elem "-1")
  		nil
  		(list ret) )
)
