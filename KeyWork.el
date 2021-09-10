;;; KeyWork.el --- Description -*- lexical-binding: t -*-

;;; Commentary:
;; Comentary goes here.

;;; Code:
(defvar KeyWork--map (make-sparse-keymap)
  "Initial map of KeyWork.")

(define-minor-mode KeyWork-mode
  "Toggle KeyWork minor mode."
  :global t
  :lighter " KeyWork"
  :keymap KeyWork--map
  :group 'KeyWork)

;; --------------------------
;; KeyWork symbol generation.

(defvar KeyWork--gensymbol-count 0
  "The number of symbols that have been generated via KeyWork--gensymbol.")

(defun KeyWork--gensymbol ()
  "Generate a symbol."
  (setq KeyWork--gensymbol-count (1+ KeyWork--gensymbol-count))
  (intern (concat "KeyWork--"(number-to-string KeyWork--gensymbol-count))))

(defun KeyWork--genmap ()
  "Generate a symbol and assign it an empty keymap."
  (let ((symbol (KeyWork--gensymbol)))
    (set symbol (make-sparse-keymap))
    symbol))

;; ------------------------------------
;; KeyWork minor mode map management.

(defvar KeyWork--root nil
  "The symbol that corresponds to the KeyWork keymap that is currently active.")

(defun KeyWork-on (symbol)
  "Replace KeyWork minor mode map with keymap stored in SYMBOL.

   The the symbol can also have additional data stored in the
                  plist, namely: :modes, :style, :colour."
  (setq KeyWork--root symbol)
  (let (($augmentation (KeyWork--augmentation symbol)))
    (if $augmentation
	(KeyWork--activate $augmentation)
      (KeyWork--activate symbol))))

(defun KeyWork--augmentation (symbol)
  "Check if the map associated with SYMBOL has any activatable augmentations.

   Retruns a symbol that stores the activatable map, otherwise returns nil"
  (let (($a (get symbol ':modes))) ; find better name instead of :modes
    (KeyWork--augmentation-check $a)))

(defun KeyWork--augmentation-check (l)
  "L :: [(fn . symbol)].

  Recursively check the list of tuples L to find the first
     tuple whose car eval to true, return the cdr of that tuple."
  (cond
   ((null l) nil)
   (t (let* (($tuple (car l)) ;; tuple :: (fn . symbol)
	     ($pred (car $tuple))
	     ($keymap (cdr $tuple)))
	(if (funcall $pred)
	    $keymap
	  (KeyWork--augmentation-check (cdr l)))))))

(defun KeyWork--activate (symbol)
  "SYMBOL."
  (setq-default cursor-type (get symbol ':style)) ; need to make sure they do have a style and colour.
  (set-cursor-color (get symbol ':colour))
  (setf (cdr (assq 'KeyWork-mode minor-mode-map-alist)) (eval symbol)))

;; --------
;; Parsing.

(load-file "~/Files/SystemConfig/Emacs/ParserMonad.el")

(defconst KeyWork--P-inline-lambda (Parser-fmap
				    (lambda (x) `(lambda () (interactive),x))
				    Parser-quoted-list-unwrap))

(defconst KeyWork--P-: (monad-do Parser
			 (_ (Parser-equal ':)) 
			 (x KeyWork--P-anonmap)
			 (return x)))


(defconst KeyWork--P-!l (monad-do Parser
			  (_ (Parser-equal '!))
			  (x KeyWork--P-anonmap)
			  (return `(lambda () (interactive) (KeyWork-on ,x)))))

(defconst KeyWork--P-colour Parser-string)

(defconst KeyWork--P-style Parser-symbol)

(defun KeyWork--P-symbol-prefix (prefix) (monad-do Parser
					   (s Parser-symbol)
					   (if (and (not (equal (intern prefix) s)) (equal prefix (substring (symbol-name s) 0 1)))
					       (Parser-return (intern (substring (symbol-name s) 1)))
					     Parser-zero)))

(defconst KeyWork--P-!s (monad-do Parser
			  (x (KeyWork--P-symbol-prefix "!"))
			  (return `(lambda () (interactive) (KeyWork-on (quote ,x))))))


(defun KeyWork--P-appearance (symbol) (Parser-many
				       (Parser-plus-n
					(Parser-fmap (lambda (x) `(put ,symbol ':colour ,x)) KeyWork--P-colour)
					(Parser-fmap (lambda (x) `(put ,symbol ':style (quote ,x))) KeyWork--P-style))))

(defun KeyWork--P-bindings (keymap) (monad-do Parser
				      (x (Parser-many (Parser-nest
						       (monad-do Parser
							 (key Parser-string)
							 (command KeyWork--P-binding-rhs)
							 (return `(define-key ,keymap (kbd ,key) ,command))))))
				      (return x)))

(defun KeyWork--P-predicate-to-mode-list (parent-map)
  (Parser-many (Parser-nest
		(monad-do Parser
		  (pred (Parser-plus
			 Parser-quoted-symbol
			 KeyWork--P-inline-lambda))
		  (map Parser-quoted-symbol-unwrap)
		  (return `(push ,`(cons ,pred (quote ,map)) (get ,parent-map :modes) ))))))



(defconst KeyWork--P-binding-rhs (monad-do Parser
				   (x (Parser-plus-n
				       KeyWork--P-:                               ;; If : is encountered then its an inline map definition
				       KeyWork--P-!s                              ;; Take off the ! prefix on the symbol and activate it as a map ;; this needs to go before 
				       KeyWork--P-!l                              ;; Create map anon map and activate
				       Parser-quoted-symbol                      ;; Either just an interactive command or keymap
				       (Parser-fmap 'eval Parser-unquoted-list)  ;; Unquoted list which we just evauluate
				       KeyWork--P-inline-lambda))
				   (return x)))

(defconst KeyWork--P-map (monad-do Parser
			   (n Parser-quoted-symbol-unwrap)
			   (s (KeyWork--P-appearance `(quote ,n)))
			   (b (KeyWork--P-bindings n))
			   (m (KeyWork--P-predicate-to-mode-list `(quote ,n)))
			   (return `(progn (setq ,n (make-sparse-keymap))
					   ,@b
					   ,@s
					   ,@m
					   ,n))))

(defconst KeyWork--P-anonmap (Parser-nest (monad-do Parser
				     (n (Parser-oneornone Parser-quoted-symbol))
				     (s (KeyWork--P-appearance 'map-symbol))
				     (b (KeyWork--P-bindings '(eval map-symbol)))
				     (m (KeyWork--P-predicate-to-mode-list 'map-symbol))
				     (return `(let ((map-symbol ,(if n n '(KeyWork--gensymbol))))
						(set map-symbol (make-sparse-keymap))
						,@b
 						,@s
						,@m
						(fset map-symbol (eval map-symbol))
						map-symbol)))))
;; -----------------
;; User Interface

(defmacro KeyWork (&rest a)
  "KeyWork map declaration A."
  (cadr (Parser-run KeyWork--P-map a)))

(provide 'KeyWork)
;;; KeyWork.el ends here

;; (KeyWork 'KW-command
;; 	 "#ffffff" hollow
;; 	 ("i" 'previous-line)
;; 	 ("k" 'next-line)
;; 	 ("l" 'forward-char)
;; 	 ("j" 'backward-char))
