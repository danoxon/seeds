;; .emacs dbarrett
;; Some elisp sources in:
;;   /usr/share/emacs/site-lisp/
;;   /usr/share/emacs/23.1/lisp/

;;====================  Start emacsclient server ================
;; (server-start)

(load "server")
(unless (server-running-p) (server-start))

;; =========== misc paths ====================================
(add-to-list 'load-path "/opt/net/tools/share/elisp")
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d")

;; =========== Put path in title bar, not host ============
(setq frame-title-format
  '("emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))


(message "======= CEDET  ===================")
;; semantic, senator, ecb, etc.

;; enable
(add-hook 'c-mode-common-hook
    '(lambda ()
       (semantic-mode 1)
       (global-ede-mode 1)
       (global-semantic-idle-breadcrumbs-mode 1)
       (global-semantic-show-unmatched-syntax-mode 1)

       (global-semantic-highlight-func-mode 1)
       (global-semantic-idle-local-symbol-highlight-mode 1)
       (global-semantic-decoration-mode 1)
       (global-semantic-idle-completions-mode 1)
 ))

;; Misc semantic commands and modes

;; Two ways to see all completions at once
;;   idle mode shows 1 completion.
;;   M-x semantic-speedbar-analysis  
;; Command: semantic-analyze-possible-completions  ;; c-c , l

(message "==============  Programming helpers ===================")
;;TODO : replace customer highlighers w/ emacs 24 semantic/cedet built-in's?
;; global-semantic-highlight-func-mode
;; global-semantic-idle-local-symbol-highlight-mode
;; ?? emacs24.cedet built-in replacement for goto-match-paren?

(setq-default indent-tabs-mode t)  ;; setq-default, only for buffers without local value
(setq-default fill-column 79)

;; ======== Paren/Brace matching  ============
(setq blink-matching-open t)

;;; Use "%" to jump to the matching parenthesis.
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t                    (self-insert-command (or arg 1))) ))

(global-set-key "%" `goto-match-paren)


;; Highlight word under cursor after delay.
;; Not tag specific like ecb-highlight-tag-with-point, but simpler config

(load-library "idle-highlight-mode")
(add-hook 'c-mode-common-hook
    '(lambda ()
       (idle-highlight-mode t)
 ))

; auto create matching parens, quotes, etc.
;; (require 'autopair)
;;(autopair-global-mode) ;; to enable in all buffers
;; (add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))

;; Highlight manually selected symbol under point
(require 'highlight-symbol)
(global-set-key "\C-ch" 'highlight-symbol-at-point)  ;; conflicts w/ gtags minor mode
(global-set-key "\C-cc" 'highlight-symbol-at-point)  ;; conflicts w/ gtags minor mode


;; key bindings
(global-set-key "\C-cl" 'goto-line)
(global-set-key [f4] 'refresh-file)
(global-set-key (kbd "<XF86New>") 'refresh-file) ;; in case F4 name not valid
(global-set-key [f5] 'speedbar)

;; Try to use Ctrl-C <letter> for user mappings
(global-set-key "\C-co" 'occur)
(global-set-key "\C-cp" 'ff-find-other-file)  ;;[p]air. Switch between .cpp/.h pair


(which-func-mode t)
;;(hl-line-mode t)

;; Netezza has two standards
;;  kernel/driver c is tabified, while .cpp is space indented.
;; .h appears to use 4 space offset even for kernel/driver code. See dsi.h
(defun nz-c-mode ()
  "C mode with adjusted defaults for use at Netezza."
  (interactive)
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-indent-level 4)
  (c-set-offset 'substatement-open '0)
;;  (require 'fillcode)
;;  (fillcode-mode t)
  )

(defun nz-c++-mode-hook ()
  (setq indent-tabs-mode nil)
;;  (setq c-basic-offset 4)
;;  (setq c-indent-level 4)
;;  (setq tab-width 4)
)

(add-hook 'c-mode-common-hook 'nz-c-mode) ; set the default C mode
(add-hook 'c++-mode-hook 'nz-c++-mode-hook)
;;(add-hook 'c-mode-hook 'nz-c-mode) ; set the default C mode



(message "==============  Theme / Color ===================")
;; set the fonts and colors 
(global-font-lock-mode t)
;; (parchment-screen)
;; (set-default-font linux-font)
(set-mouse-color "black")

;; System specific, by system name
(cond ((string-match "wintermute" (system-name)) 
	(message "Home settings installed...")
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
	(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs24_defthemes")

	(load-theme 'zenburn t)
      )

        ((string-match "dbarrettrhl" (system-name)) 
	 (message "WORK settings installed...")
	 ;; Emacs 24 
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
	(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs24_defthemes")
	(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
	(load-theme 'solarized-dark t)
        )
  )


(message "=========== Buffer Control ===================")
;; emacs doesn't distinguish between "using" a buffer and cycling through looking for one,
;; like Alt-Tab in Gnome windows.  So two commonly used buffers may not be near each other
;; in the search list.  
;;     http://www.emacswiki.org/emacs/SwitchingBuffers

;; iswitchb - buffer switch via incremental completion.  built-in circa Emacs20.
;; icicles - minibuffer completion (not just buffer names) by partial match...
;;    iswitchb, icicles overlapping func.  icicle mores general?
;; ibuffer - C-x C-b   View buffers like dired. .  Complementary to/with icicles

;; IDO  - 

;;  TODO: emacs 24 what's built in?

;;  ===== ibuffer filtering
(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
  (quote (("default"      
           ;; ("Tests"
           ;; (filename . "src/tests/"))
	    
            ("Programming" 
              (or
                (mode . c-mode)
		(mode . c++-mode)
                (mode . perl-mode)
                (mode . python-mode)
               ;; (mode . emacs-lisp-mode)
                ;; etc
                ))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; Filter buffer list ( >= emacs 23.1 )
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*") 

;; Use ibuffer instead of default C-x, C-b behavior
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; ===== iswitchb    - Switch buffers by name search.
;; C-x b - switch-to-buffer. Shows all matches in minibuf, with incremental matching.
;; C-s to rotate list
;; (require 'iswitchb)    
;; (iswitchb-mode 1)
;; TODO  (setq iswitchb-buffer-ignore '("^ " "*Buffer"))
;; USE? (setq iswitchb-default-method 'samewindow)


;; Yoni Rabkin's frame switcher func 
(defun switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer))
)
;; TODO (global-set-key "\C-c xxxxxx" 'switch-buffers-between-frames)


(message "=========== Window & Frame control  ===================")
;; Change emacs window start size
(if (window-system) (set-frame-size (selected-frame) 90 50))

;;; WINDOW SPLITING
(global-set-key (kbd "M-4") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-3") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-s") 'other-window) ; was center-line
(global-set-key (kbd "M-b") 'balance-windows) ; was center-line

;; ========  New Frame instead of buffer for occur mode and others ==========
;; special-display-regexps  '("[ ]?[*][^*]+[*]"))
;; FIXME:  if other frame already exists, it'll create the buffer in that frame
;;         At least in emacsclient.
;;         function name completion pops up frame, but minibuffer loses focus.


;; ===== Change new windows to be side-by-side ===
;; From LindyDancer:
;; The default behaviour of `display-buffer' is to always create a new
;; window, and below the current one.
;;
;; Reuse existing windows, unless there's a single window open on a large display.  
;; Then open a new window to the _side_.

;; (setq pop-up-windows nil)

;; TODO: dmb - do for occur buffers only....
(defun my-display-buffer-function (buf not-this-window)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or not-this-window
               (not (eq (window-buffer (selected-window)) buf)))
           (> (frame-width) 162))
      (split-window-horizontally))
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer' -- Why, oh, why!
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    (display-buffer buf not-this-window)))

(setq display-buffer-function 'my-display-buffer-function)


;; Set these vars to change behavior of split-window-preferred-function, split-window-sensibly
;;    Not great. Any way to do this for only occur mode?  
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)


;; bigger default window for wide 24" screens
(setq window-min-width 30)

;; Switch windows with Shift-arrow keys vs. C-x o.
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))



(message "======= GTAGS - GNU Global  ===================")

(setq gtags-suggested-key-mapping t)
(add-to-list 'load-path "/usr/share/gtags/")
(load-library "gtags")
(autoload 'gtags-mode "gtags" "" t)


;; Turn on gtags for c-mode
(add-hook 'c-mode-common-hook
    '(lambda ()
       (gtags-mode 1)
 ))

;; Make gtags select mode easy to see
(add-hook 'gtags-select-mode-hook
   '(lambda ()
      (setq hl-line-face 'underline)
      (hl-line-mode 1)
 ))


;; === Gtag cycling by William Wong
(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil))
          ) ))


(global-set-key "\M-;" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
(global-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
;; FIXME emacs24 (global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
;; (global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.


(message " ============ Misc customizations and key re-maps  ===================")
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

;;(menu-bar-mode nil)
(setq inhibit-splash-screen t)
(column-number-mode t)


;; A more ergonomic Meta than the Alt key, especially if Capslock key acting as Ctrl
;; x & c mapped for typo forgiveness. 
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Backward-kill-word is handy.
;;global-set-key "\C-w" 'backward-kill-word)
;;(global-set-key "\C-x\C-k" 'kill-region)
;;(global-set-key "\C-c\C-k" 'kill-region);;


(message "=========== Custom-mode settings...  ===================")
;; load in customizations, 
;;(load-library "~/.custom")

;;; end .emacs 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((((class color) (min-colors 88) (background dark)) (:background "LightGreen" :foreground "black"))) nil "Foreground and background colors are reversed for some reason..."))

