# KeyWork 🗝️👷🏼‍♂️

## Installation
To install the packages via [straight](https://github.com/radian-software/straight.el):
```
(use-package monad
  :straight
  (monad
   :type git
   :host github
   :repo "ernestkz/monad.el"))

(use-package ParserMonad
  :straight
  (ParserMonad
   :type git
   :host github
   :repo "ernestkz/ParserMonad.el"))

(use-package KeyWork
  :straight
  (KeyWork
   :type git
   :host github
   :repo "ernestkz/KeyWork"))
```

## Example
```
(KeyWork
 KW-command
 "#ffffff" hollow
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char)
 ("o" forward-word)
 ("u" backward-word)

 (";" end-of-line)
 ("h" beginning-of-line)
 
 ("e" backward-kill-word)
 ("r" kill-word)

 ("t" set-mark-command)
 ("x" cut)
 
 ("v" (if (eq last-command 'yank) (yank-pop) (yank)))
 ("y" undo)

 ("3" delete-other-windows)
 ("4" split-window-vertically)
 ("," other-window)

 ("m" comint-previous-input)
 ("." comint-next-input)
 ("a" execute-extended-command)

 ("f" !(KW-insert
	"#fff000" hollow
	("C-<tab>" completion-at-point)
	("<f8>" !KW-command)))

 ("<f8>" !KW-command)
 
 ("SPC" :(("i" :(("e" find-file)))
	      ("," :(("m" eval-last-sexp)))
	  
	      ("j" :(("j" helpful-function)
		         ("k" helpful-key)
		         ("l" helpful-variable)))
	  
	      ("w" :(("m" (find-file "~/.emacs"))
		  ("x" (find-file "/sudo::/etc/nixos/configuration.nix"))
		  ("o" (browse-url "https://search.nixos.org/packages"))

	      ("u" (kill-buffer (current-buffer)))
	      ("m" dired-jump)
	      (";" save-buffer)
	      ("f" switch-to-buffer)
	      ("4" split-window-horizontally)
	      ("3" delete-window))))
	  
	  
(setq KeyWork-mode t)
(KeyWork-on 'KW-command)
(add-hook 'minibuffer-setup-hook (lambda () (KeyWork-on 'KW-insert)))
(add-hook 'minibuffer-exit-hook (lambda () (KeyWork-on 'KW-command)))
```

