;;; -*- lexical-binding: t -*-
;;; KeyWork.el --- 🗝️👷🏼‍♂️ - Elisp DSL for declaring modal bindings in Emacs. 

;; Copyright (C) 2021  Ernests Kuznecovs

;; Author: Ernests Kuznecovs <ernestkuznecovs@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:



;;; Code:
(require 'ParserMonad)

(defvar KeyWork--map (make-sparse-keymap)
  "Initial map of KeyWork.")

(define-minor-mode KeyWork-mode
  "Toggle KeyWork minor mode."
  :global t
  :lighter " KeyWork"
  :keymap KeyWork--map
  :group 'KeyWork)


(defun KeyWork--priority-minor-mode-map (_file)
  "Try to ensure that keybindings retain priority over other minor modes.

Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'KeyWork-mode)
    (let ((mykeys (assq 'KeyWork-mode minor-mode-map-alist)))
      (assq-delete-all 'KeyWork-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'after-load-functions 'KeyWork--priority-minor-mode-map)

;; --------------------------
;; KeyWork symbol generation.

(defvar KeyWork--gensymbol-count 0
  "The number of symbols that have been generated via KeyWork--gensymbol.")

(defun KeyWork--gensymbol ()
  "Generate a symbol."
  (setq KeyWork--gensymbol-count (1+ KeyWork--gensymbol-count))
  (intern (concat "KeyWork--" (number-to-string KeyWork--gensymbol-count))))

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

   Returns a symbol that stores the activatable map, otherwise returns nil."
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
  "Set the map stored in SYMBOL as the current KeyWork minor mode map."
  (setq-default cursor-type (get symbol ':style)) ; need to make sure they do have a style and colour.
  (set-cursor-color (get symbol ':colour))
  (setf (cdr (assq 'KeyWork-mode minor-mode-map-alist)) (eval symbol)))

;; --------
;; Parsing.

(defun KeyWork--P-symbol-prefix (prefix)
  "Docstring goes here"
  (monad-do Parser
    (s Parser-symbol)
    (if (and (not (equal (intern prefix) s))
	     (equal prefix (substring (symbol-name s) 0 1)))
	(Parser-return (intern (substring (symbol-name s) 1)))
      Parser-zero)))

(defun KeyWork--P-appearance (m)
  "Parses :colour <string> :style <symbol>"
  (Parser-many
   (Parser-plus-n
    (Parser-fmap (lambda (x) `(put ,m ':colour ,x)) Parser-string)
    (Parser-fmap (lambda (x) `(put ,m ':style (quote ,x))) Parser-symbol))))

(defun KeyWork--P-composed-keymaps (m)
  "Takes map M and matches a list of symbols i.e '(<SYMBOL> <SYMBOL> ...)' which are also maps.

    The list of symbols and M are combined with make-composed-map."
  (Parser-oneornone
   (monad-do Parser
     (maps (Parser-nest (Parser-many1 Parser-symbol)))
     (return `((set ,m (make-composed-keymap (list (eval ,m) ,@maps))))))))

(defun KeyWork--P-bindings (keymap)
  "Parses multiple key bindings.
    
      '(STRING RHS)
       (STRING RHS)
       (STRING RHS)
       .
       .
       .'"
  (monad-do Parser
    (x (Parser-many (Parser-nest
		     (monad-do Parser
		       (key Parser-string)
		       (command KeyWork--P-binding-rhs)
		       (return `(define-key ,keymap (kbd ,key) ,command))))))
    (return x)))

(defun KeyWork--P-predicate-to-mode-list (parent-map)
  "Matches many '<LIST or SYMBOL> <SYMBOL>'

    The match on the LHS is either a KeyWork--P-lambda-command or a Symbol this LHS is used
    to as a function that returns a boolean.

    The RHS <SYMBOL> is a symbol that contains a map."
  (Parser-many (Parser-nest
		(monad-do Parser
		  (pred (Parser-plus
			 (Parser-fmap 'custom-quote Parser-symbol)
			 KeyWork--P-lambda-command))
		  (map Parser-symbol)
		  (return `(push ,`(cons ,pred (quote ,map)) (get ,parent-map :modes)))))))

(defconst KeyWork--P-map
  (monad-do Parser
    (n (Parser-oneornone Parser-symbol))
    (s (KeyWork--P-appearance 'map-symbol))
    (c (KeyWork--P-composed-keymaps 'map-symbol))
    (b (KeyWork--P-bindings '(eval map-symbol)))
    (m (KeyWork--P-predicate-to-mode-list 'map-symbol))
    (return `(let ((map-symbol ,(if n `(quote ,n) '(KeyWork--gensymbol))))
	       (set map-symbol (make-sparse-keymap))
	       ,@b
 	       ,@s
 	       ,@m
	       ,@c			
	       (fset map-symbol (eval map-symbol))
	       map-symbol)))
  "Parser that matches a map structure and generates the elisp code that constructs the map.")

(defconst KeyWork--P-lambda-command
  (Parser-fmap (lambda (x) `(lambda () (interactive) ,x)) Parser-list)
  "Matches a list, and wraps it into an interactive lambda.
     For example:
                                       (message \"hello\")
     will be shorthand for:
                                       (lambda () (interactive) (message \"hello\"))")
(defconst KeyWork--P-prefix-key-map-declaration
  (Parser-blind (Parser-equal ':) (Parser-nest KeyWork--P-map))
  "Matches the list: ' : (<map>) ', and returns a parsed <map> via KeyWork--P-map.
     For example:
                                              :((\"a\" (message \"a\"))
                                                (\"b\" (message \"b\")))
     will be shorthand for:
                                       (KeyWork (\"a\" (message \"a\"))
                                                (\"b\" (message \"b\")))")
(defconst KeyWork--P-stored-map-activation
  (monad-do Parser
    (x (KeyWork--P-symbol-prefix "!"))
    (return `(lambda () (interactive) (KeyWork-on (quote ,x)))))
  "Matches a symbol which starts with an !.
   Returns an interactive command that calls KeyWork-on on the parsed symbol without the !.
     For example:
                                               '!my-map'
     will be shorthand for:
                               '(lambda () (interactive) (KeyWork-on my-map))'")
(defconst KeyWork--P-inline-map-activation
  (monad-do Parser
    ((Parser-equal '!))
    (x (Parser-nest KeyWork--P-map))
    (let ((map-symbol (eval x)))
      (return `(lambda () (interactive) (KeyWork-on (quote ,map-symbol))))))

  
  "Matches the list: ' ! (<map>) '.
   Returns an interactive command that calls KeyWork-on on the parsed <map> via KeyWork--P-map.
     For example:
                                              !((\"a\" (message \"a\"))
                                                (\"b\" (message \"b\")))
     will be shorthand for:
         (lambda () (interactive) (KeyWork-on (KeyWork (\"a\" (message \"a\"))
                                                (\"b\" (message \"b\")))))")
(defconst KeyWork--P-binding-rhs
  (monad-do Parser
    (x (Parser-plus-n
	KeyWork--P-lambda-command
	KeyWork--P-stored-map-activation
	KeyWork--P-prefix-key-map-declaration
	KeyWork--P-inline-map-activation
	(Parser-fmap 'custom-quote Parser-symbol)))
    (return x))
  "Structures that can go on the right hand side of a bind.")

;; -----------------
;; User Interface
(defmacro KeyWork (&rest a)
  "KeyWork map declaration A"
  (cadr (Parser-run KeyWork--P-map a)))

(provide 'KeyWork)
;;; KeyWork.el ends here
