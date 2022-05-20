# KeyWork 🗝️👷🏼‍♂️

## Installation
To install the package and its dependencies via [straight.el](https://github.com/radian-software/straight.el):
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
Repositories of the dependencies:
- [ParserMonad.el](https://github.com/ErnestKz/ParserMonad.el)
- [monad.el](https://github.com/ErnestKz/monad.el)

## Example
Below is an excerpt from a [config](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L418) which utilises KeyWork to a great extent.
```
(KeyWork
 KW-command           ;; here we give the map the name "KW-command"
 "#ffffff" hollow     ;; here we declare cursor/point style when this map is active
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
 ;; this is implicilty turned into a lambda
 
 ("y" undo)
 ("3" delete-other-windows)
 ("4" split-window-vertically)
 ("," other-window)

 ("m" comint-previous-input)
 ("." comint-next-input)
 ("a" execute-extended-command)
 
 ("<f8>" !KW-command)
 ;; a symbol prefixed with "!" denotes
 ;; a keymap/mode to be activated upon button press

 ("f" !(KW-insert     
	"#fff000" hollow
	("C-<tab>" completion-at-point)
	("<f8>" !KW-command)))
 ;; here, another map is declared, but this time in-line
 ;; "f" will activate the KW-insert map when pressed

 ("s" !(prog-map
        "#fff000" hollow
		("<f8>" !KW-command)
		((string-equal major-mode "pdf-view-mode") pdf-map)
		((string-equal major-mode "python-mode") KW-python)
		((string-equal major-mode "org-mode") KW-org)))
;; the three lines above list predicates:
;; e.g (string-equal major-mode "org-mode") 
;; these predicates are checked upon "prog-map" activation
;; and the the first to return true will activate the 
;; corresponding map: 
;; e.g KW-org (definition of KW-org is omitted in the readme, but can be 
;; found in the config linked above)

 ;; below, we define "SPC" to be a leader key, denoted by ":",
 ;; then, for example, we further define "j" to be a leader key,
 ;; and finally, bind commands to "j", "k", and "l".
 
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
;; nicer to have insert mode already active when entering the minibuffer
```

