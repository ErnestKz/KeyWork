(require 'cl-lib)
(require 'eieio)

(defvar keywork--map (make-sparse-keymap)
  "Initial map of keywork.")

(define-minor-mode keywork-mode
  "Toggle keywork minor mode."
  :global t
  :lighter " keywork"
  :keymap keywork--map
  :group 'keywork)

(defun keywork--priority-minor-mode-map (_file)
  "Try to ensure that keybindings retain priority over other minor modes.

Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'keywork-mode)
    (let ((mykeys (assq 'keywork-mode minor-mode-map-alist)))
      (assq-delete-all 'keywork-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'after-load-functions 'keywork--priority-minor-mode-map)

(defun keywork-on (list-of-map)
  (let* ((colors-and-map (keywork--activate-compile list-of-map))
	 (colors (car colors-and-map))
	 (map    (cdr colors-and-map)))
    (set-cursor-color (car colors))
    (setf (cdr (assq 'keywork-mode minor-mode-map-alist))
	  map)))

(cl-defstruct map
  (binds nil)
  (pred nil :type function)
  (col nil :type string))

;; returns (colour keymap)
(defun keywork--activate-compile (list-of-map)
  (let* ((true-maps  (cl-remove-if-not
		     (lambda (map) (funcall (map-pred map)))
		     list-of-map))
	 (colors-list nil)
	 (list-of-keymaps
	  (mapcar
	   (lambda (child-map)
	     (push (map-col child-map) colors-list)
	     (let ((compiled-child-map (make-sparse-keymap)))
	       (mapcar
		(lambda (bind)
		  (let ((key (car bind))
			(rhs (cadr bind)))
		    (cl-etypecase rhs
		      ((list-of map)
		       (let* ((colors-and-map (keywork--activate-compile rhs))
			      (_colors        (car colors-and-map))
			      (map            (cdr colors-and-map)))
			 (define-key compiled-child-map (kbd key) map)))
		      (t (define-key compiled-child-map (kbd key) rhs)))))
		(map-binds child-map))
	       compiled-child-map))
	   true-maps)))
    (cons colors-list
	  (make-composed-keymap
	   list-of-keymaps (make-sparse-keymap)))))

(defun tt (&rest _) t)

(defvar keywork-default-cursor-color "#ffffff")

(cl-defun keywork--make-map (&key (pred 'tt)
				  (color keywork-default-cursor-color)
				  map)
  (list (make-map
	 :binds map
	 :pred pred
	 :col color)))

(defun keywork--make-list-of-map (map-symbol)
  (let* ((child-lists-of-map
	  (apply 'append
		 (mapcar 'keywork--make-list-of-map
			 (get map-symbol 'children)))))
    
    (append child-lists-of-map (eval map-symbol))))

(defun keywork--add-child (map-symbol child-symbol)
  (put map-symbol 'children
       (append (list child-symbol) (get map-symbol 'children))))

(setq keywork--last-activated nil)

(defmacro kw-on (map-symbol)
  `(lambda () (interactive)
     (setq keywork--last-activated ,map-symbol)
     (keywork-on (keywork--make-list-of-map ,map-symbol))))

(defun keywork-refresh ()
  (when keywork--last-activated
    (keywork-on (keywork--make-list-of-map keywork--last-activated))))

(defmacro kw-c (&rest body)
  `(lambda () (interactive)
     ,@body))

(defmacro kw-seq (map-symbol)
  `(keywork--make-list-of-map ,map-symbol))

(defun kw-m (binds)
  (keywork--make-map :map binds))

(provide 'keywork)
