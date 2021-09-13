(KeyWork
 :Name Map-Name
 :Cursor hollow "#ffffff"
 :Bindings
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char)
 :Augmentations
 ((string-equal major-mode "python-mode") KW-python)
 ((string-equal major-mode "org-mode") KW-org))

(KeyWork
 Map-Name
 hollow "#ffffff"
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char)
 ((string-equal major-mode "python-mode") KW-python)
 ((string-equal major-mode "org-mode") KW-org))

(KeyWork
 Map-Name
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char))

(KeyWork
 hollow "#ffffff"
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char))

(KeyWork
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char))

(KeyWork
 ("i" (message "hello"))
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char))


(KeyWork
 ("i" (message "hello i"))
 ("k" :(Map-Name2
	("i" (message "hello k-i"))
	("j" (message "hello k-j"))))
 ("l" forward-char)
 ("j" backward-char))


(KeyWork
 ("i" (message "hello i"))
 ("k" :(("i" (message "hello k-i"))
	("j" (message "hello k-j"))))
 ("l" forward-char)
 ("j" backward-char))

(KeyWork
 Map-Name
 ("i" (message "hello i"))
 ("k" !(("i" (message "hello k-i"))
	("j" (message "hello k-j"))
	("a" !Map-Name)))
 ("l" forward-char)
 ("j" backward-char))

(KeyWork
 Map-Name
 ("i" (message "hello i"))
 ("k" :(("i" (message "hello k-i"))
	("j" (message "hello k-j"))
	("a" Map-Name)))
 ("l" forward-char)
 ("j" backward-char))

(KeyWork
 :Name Map-Name4
 :Cursor hollow "#ffffff"
 :Inherit (Map-Name Map-Name2)
 :Bindings
 ("i" previous-line)
 ("k" next-line)
 ("l" forward-char)
 ("j" backward-char))


(KeyWork
 Map-Name
 ("i" (message "hello i"))
 ("k" !(Map-Name2
	(Map-Name)
	("i" (message "hello k-i"))
	("j" (message "hello k-j"))
	("a" !Map-Name)))
 ("l" !Map-Name2)
 ("j" backward-char))


