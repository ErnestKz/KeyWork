# KeyWork 🗝️👷🏼‍♂️
### Table of Contents
1. [Motivation](#motivation)
2. [Commentary on Implementation](#commentary-on-implementation)
3. [Commentary on Usage](#commentary-on-usage)
4. [Installation](#installation)
5. [Example](#example)

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

## 5. Example
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
