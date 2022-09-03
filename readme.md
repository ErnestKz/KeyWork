# New keywork version.
A new keywork implementation has been written, keywork.el, as opposed to the old KeyWork.el. The new implementation does not require any third party dependencies and also is not as ambitious with a custom syntax macro and mostly uses regular functions instead.

New keywork also has the ability to merge maps such that even keysequences are merged.

Map merging and not using macros allows for modular declaration of keymaps.

# keywork (new)
## Example
```
(setq
 kw-insert
 (keywork--make-map
  :color "#008888"
  :map
  `(("<f8>" ,(kw-on 'kw-command)))))

(setq
 kw-command
 (keywork--make-map
  :map
  `(("a" execute-extended-command)
    ("q" minions-mode))))

(keywork--add-child 'kw-command 'kw-command-mode-select)
(keywork--add-child 'kw-command 'kw-command-window)
(keywork--add-child 'kw-command 'kw-command-files-and-buffers)
(keywork--add-child 'kw-command 'kw-command-file-jump-shortcuts)
(keywork--add-child 'kw-command 'kw-command-elisp)


(setq
 kw-command-mode-select
 (keywork--make-map
  :map
  `(("<f8>" ,(kw-on 'kw-command))
    ("f"    ,(kw-on 'kw-insert))
    ("d"    ,(kw-on 'kw-scroll)))))

(setq
 kw-command-window
 (keywork--make-map
  :map
  `(("SPC 3" delete-window)
    ("4" split-window-vertically)
    ("," other-window)
    ("SPC 4" split-window-horizontally)
    ("4" split-window-vertically)
    ("3" delete-other-windows))))

(setq
 kw-command-files-and-buffers
 (keywork--make-map
  :map
  `(("SPC i e" find-file)
    ("SPC i d" dired)
    ("SPC u" ,(kw-c (kill-buffer (current-buffer))))
    ("SPC m" dired-jump)
    ("SPC n" ek-kill-jump)
    ("SPC ;" save-buffer)
    ("SPC f" switch-to-buffer))))

(setq
 kw-command-elisp
 (keywork--make-map
  :map
  `(("SPC e j" eval-last-sexp)
    ("SPC e b" eval-buffer)
    ("SPC e m" pp-macroexpand-last-sexp)
    ("SPC j"   help)
    ("SPC J f" describe-function)
    ("SPC J v" describe-variable))))

(setq
 kw-shortcuts
 (keywork--make-map
  :map
  `(("n" minmacs-find-source)
    ("k" ,(kw-c (find-file
		 (concat minmacs-source-directory
			 "minmacs-keybindings.el"))))
    ("x" ,(kw-c (find-file (concat system-nix-directory "system.nix"))))
    ("h" ,(kw-c (ek-find-file-default system-nix-directory)))
    ("f" ,(kw-c (ek-find-file-default system-haskell-directory)))
    ("u" ,(kw-c (ek-find-file-default system-desktop-ui-directory)))
    ("m" ,(kw-c (find-file system-xmonad-config-file)))
    ("s" ek-nixos-rebuild-switch)
    ("o" ,(kw-c (ek-browse-url-chromium "https://search.nixos.org/packages")))
    ("j" ek-message-buffer)
    ("p" ek-visit-straight-package))))

(setq
 kw-command-file-jump-shortcuts
 (keywork--make-map
  :map
  `(("SPC w" ,(kw-seq 'kw-shortcuts)))))

(setq
 kw-scroll
 (keywork--make-map
  :color "#bb8833"
  :map
  `(("<f8>" ,(kw-on 'kw-command))
    ("f"    ,(kw-on 'kw-insert))
    ("q"    minions-mode)
    
    ("u" ,(kw-c (ek-scroll-preserve-position t)))
    ("i" ,(kw-c (ek-scroll-preserve-position nil)))
    ("j" ,(kw-c (scroll-up-command 4)))
    ("k" ,(kw-c (scroll-down-command 4)))
    
    ("M-i" ,(kw-c (previous-line 8)))
    ("M-k" ,(kw-c (next-line 8)))
    ("M-u" ,(kw-c (previous-line 3)))
    ("M-j" ,(kw-c (next-line 3)))
    ("M-d" recenter-top-bottom)
    
    ("d" move-to-window-line)
    ("m" end-of-buffer)
    ("," beginning-of-buffer)

    ("t" set-mark-command)
    ("x" ek-cut)
    ("c" kill-ring-save)
    ("v" ek-paste)


    ("g" avy-goto-word-0)
    ("G" avy-goto-line)
    )))
	
	
(setq
 kw-lsp
 (keywork--make-map
  :pred (lambda () (bound-and-true-p lsp-mode))
  :map
  `(("m" ,(kw-m `(("i" imenu)	
                  ("I" lsp-ui-imenu)
		  		  
		  ("r" lsp-rename)
		  ("1" lsp-document-highlight)
		  ("l" lsp-lens-mode)
              		  						  
        	  ("k" ,(kw-c (lsp-avy-lens)
        		      (lsp-lens-refresh t)))
        
        	  ("3" lsp-format-buffer)	  
        	  ("s" lsp-ido-workspace-symbol)   		  
        	  ("a" lsp-execute-code-action)	  
        	  ("D" lsp-describe-thing-at-point)
        	  ("d" lsp-ui-doc-glance)
        		  
                  ("b" lsp-headerline-breadcrumb-mode)
        	  ("v" lsp-modeline-code-actions-mode)
        		  
        	  ("f" lsp-ui-peek-find-references)
        	  ("F" lsp-find-references)
        		  
        	  ("t" lsp-treemacs-call-hierarchy)))))))

(keywork--add-child 'kw-command 'kw-lsp)

(keywork-mode)
(funcall (kw-on 'kw-command))
(add-hook 'minibuffer-setup-hook (kw-on 'kw-insert))
(add-hook 'minibuffer-exit-hook (kw-on 'kw-command))
(push (lambda (_) (keywork-refresh)) window-selection-change-functions)
(push (lambda (_) (keywork-refresh)) window-buffer-change-functions)

```


# KeyWork 🗝️👷🏼‍♂️ (old)

### Table of Contents
1. [Motivation](#1-motivation)
2. [Commentary on Implementation](#2-commentary-on-implementation)
3. [Commentary on Usage](#3-commentary-on-usage)
4. [Installation](#4-installation)
5. [Examples](#5-examples)

## 1. Motivation
This package is aimed at **being able to specify arbitrary keymap modes along with arbitrary key sequence setups from scratch as concisely as possible**.

My reasons for a modal, key sequence, oriented bindings DSL:
- Reduce wrist and hand strain from the vigorous key-chording I used to do.
- Have major/minor mode specific bindings closer to the home row, for speed and ease.
- Be able to rapidly iterate over my keyboard config to see what feels good.
- Reduce the friction of me trying to specialise the bindings of a new major/mode.

I initially was using [xah-fly-keys](https://github.com/xahlee/xah-fly-keys), which was nice as it felt quite efficient, though I began theory-crafting even more ways of making efficient interfaces and got very creative, and wanted a way to express it, xah-fly-keys was a not very suitable to build off of, as it had baked in the "command-mode" modal mechanism. Though my visions were of arbitrary amount of modes, and modes that merge together based on the environment.

## 2. Commentary on Implementation
I started writing this the summer of 2021, and was finding it difficult and complex to parse the symbols of the language, but then I encountered [monadic parsers](https://github.com/ErnestKz/ParserMonad.el), which made it much easier.

The actual implementation of mode switching is only 60 lines in total (including whitespace and comments), it's done by having a single minor mode map "KeyWork", and just replacing the minor mode map that's held within the KeyWork-map symbol with the one you want activated. The rest of the code is parsing and expanding the macros.

## 3. Commentary on Usage
Towards the end of the summer I got it to a personally usable state, and was using throughout the subsequent college terms to great effect.

The main modes that I ended up creating and using in KeyWork the most were:

- A command mode ([code](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L418), (my F8 keys is bound to CapsLock)), where all the keys close to the home row are responsible for moving around text, cutting, yanking, marking, inserting comments, undoing, etc. And the space key is a leader key for various key sequences. This command mode is heavy influenced by the command mode of xah-fly-keys.

- A scroll mode ([code](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L483)), which is activated from the command mode with "d", and I mostly used for smoothly scrolling the buffer either rapidly, or slowly, with (j,k), and (u,i).

- An insert mode ([code](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L469)), for inserting text.

- An org programming mode ([code](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L599)), which is [activated](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L474) from the command mode with "s". I used this mode when programming python within emacs-jupyter . I setup the mode to be able to navigate between org source blocks, expand/collapse them, execute them, move them up or down, create new source blocks, hide/show results, and some more, and almost all of these commands using the high value home-row and surrounding keys without needing to press leader keys or chords.

During the year I was switching between the web-based colab (for group projects and gpu usage) and my specialised org setup, I noticed when I eventually would come back to org, the process of experimenting bits of code in isolated source blocks (quite a core part of workflow) was much smoother and I would:
- do it more often, due to the reduced friction of not needing to stretch my hands to move mouse or press key chords. 
- and faster; efficient keys placement, creating blocks, cutting, pasting, executing, deleting blocks with minimum keystrokes

## 4. Installation
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

## 5. Examples

1. Below is an excerpt from a [config](https://github.com/ErnestKz/SystemConfig/blob/e27372d722e99aa12465ed37f0b02038c63d2d0d/Emacs/.emacs#L418) which utilises KeyWork to a great extent.
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
		         ("o" (browse-url "https://search.nixos.org/packages"))))

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

2. Maps can be combined as if they were defined in the one place:
```
(KeyWork
 DeleteVerb
 ("d" :(("w" kill-word)
    	("b" backward-kill-word)
    	("$" kill-line))))

(KeyWork
 MoveVerb
 ("m" :(("w" forward-word)
    	("b" backward-word)
    	("$" end-of-line))))

(KeyWork
 CommandMode
 "#ffffff" hollow
 (DeleteVerb MoveVerb)  ;; syntax to combine maps
 ("<f8>" !KW-command))

(KeyWork-on 'CommandMode))
```
