;;;
;;; Stevey's .emacs file. Loads up lots of libraries.
;;;
 ;; I use the Common Lisp stuff all the time
(require 'cl)

(add-to-list 'load-path "/opt/net/tools/share/elisp")
(add-to-list 'load-path "~/.elisp")

;; Start emacsclient server
(server-start)

;; Bind my personal keys
(global-set-key "\C-cl" 'goto-line)

;; Todo - ctrl-O  for list of methods/classes in file.
 

 (require 'emacs-type) ;; support for multiple emacs platforms
 (load-library "ekeys") ;; my key bindings and some aliases
(load-library "modes") ;; configuration for 100-odd emacs modes
(if-not-terminal
 (progn (ignore-errors (require 'jde))
 (load-library "myfont"))) ;; my own fonts and window colors
 (load-library "efuncs") ;; a bunch of my own utility functions
(load-library "my-config") ;; one-off variable settings
(load-library "java-config") ;; java/python/ruby IDE setup
(load-library "mail-config") ;; smtp config for various locations
(load-library "perl-config") ;; extra support for perl coding
(load-library "screen-config") ;; window positioning for all platforms
(load-library "xml-config") ;; a few xml and html helper functions
 (if-not-xemacs (require 'kawa-support)) ;; some helpers for Kawa Scheme
 ;; startup "script" for when we've got a window system
(if-not-terminal
 (progn ;; start gnuserv, so apps can talk to us (e.g. p4, browsers)
 (when (or google
	 (and winnt (not cygwin)))
 (require 'gnuserv)
 (setq gnuserv-frame (selected-frame))
 (gnuserv-start))

 ;; set the fonts and colors I like
 (global-font-lock-mode t)
 (parchment-screen)
 (set-default-font linux-font)
 (set-mouse-color "black")

 ;; position window automatically based on display resolution
 (size-screen)))

;; load in customizations, which I keep in their own little petting zoo
(load-library "~/.custom")

;; I always run a shell in Emacs. Always always.
(shell)

;;; end .emacs 
