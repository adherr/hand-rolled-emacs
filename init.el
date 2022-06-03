(defvar line-length 98)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; load my path
(use-package exec-path-from-shell
  :demand
  :config (exec-path-from-shell-initialize))

;; base Emacs config
(use-package emacs
  :config
  (global-display-line-numbers-mode 1)
  (blink-cursor-mode -1)
  ;; toolbar isn't on in TTY
  (tool-bar-mode -1)
  ;; Don't use messages that you don't read
  (setq initial-scratch-message "")
  ;; Get back that column
  (scroll-bar-mode -1)
  ;; y or n instead of typing
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; flash the modeline instead of bell (not sure I need this)
  (setq ring-bell-function
        (lambda ()
          (let ((orig-fg (face-foreground 'mode-line)))
            (set-face-foreground 'mode-line "#F2804F")
            (run-with-idle-timer 0.1 nil
                                 (lambda (fg) (set-face-foreground 'mode-line fg))
                                 orig-fg))))
  ;; nice scrolling
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)
  ;; more useful frame title, that show either a file or a
  ;; buffer name (if the buffer isn't visiting a file)
  (setq frame-title-format
        '("" (:eval (if (buffer-file-name)
                        (abbreviate-file-name (buffer-file-name))
                      "%b"))))
  ;; confirm exit because fat fingers
  (setq confirm-kill-emacs 'y-or-n-p)
  ;; desktop saving
  (defvar savefile-dir (expand-file-name "savefile" user-emacs-directory) "Where we save emacs's state containing files")
  (unless (file-exists-p savefile-dir)
    (make-directory savefile-dir t))
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (setq desktop-path (list savefile-dir))
  (setq desktop-dirname savefile-dir)
  (setq desktop-restore-eager 6)
  (desktop-save-mode t)
  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  ;; revert buffers automatically when underlying files are changed externally
  (global-auto-revert-mode t)
  ;; uniquify buffer names better
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  ;; saveplace remembers your location in a file when saving files
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  (save-place-mode 1)
  ;; savehist keeps track of some history
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1)
  ;; move between visible windows with Shift + arrows
  (windmove-default-keybindings)

  ;; Editorish things
  (setq-default indent-tabs-mode nil) ;; don't use tabs to indent
  (setq-default tab-width 8) ;; but maintain correct appearance
  (setq require-final-newline t) ;; Newline at end of file
  (delete-selection-mode t) ;; delete the selection with a keypress
  ;; hippie-expand some things
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           yas-hippie-try-expand
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  (setq tab-always-indent 'complete) ;; works with hippie-expand to complete if already indented. QUESTIONABLE
  (setq blink-matching-paren nil) ;; disable annoying blink-matching-paren
  ;; ispell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (flyspell-mode +1)
  ;; highlight the current line
  (global-hl-line-mode +1)
  ;; show whitespace
  (setq whitespace-line-column line-length) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (global-whitespace-mode +1)
  ;; cleanup whitespace on save
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; enable narrowing commands (C-x n ...) HIGHLY QUESTIONABLE
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; enabled change region case commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; enable erase-buffer command HIGHLY QUESTIONABLE
  (put 'erase-buffer 'disabled nil)

  )
;; end base emacs

;; MacOS specific settings
(use-package emacs
  :if (eq system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta) ; swap M and s on mac keyboards
  (setq mac-option-modifier 'super)
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq auth-sources '(macos-keychain-internet)) ; lets things like forge get credz from keychain

  ;; fire up the server, since we don't have systemd
  (use-package server
    :demand
    :config (unless (server-running-p) (server-start)))

  ;; emoji
  :if (fboundp 'set-fontset-font)
  :config (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  )
;; end MacOS

;; love me some zenburn theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; show all of the completions from the keys entered so far
(use-package which-key
  :config
  (which-key-mode))

;;; EDITORish things vvv

;; Useful functions from bbatsov
;; https://github.com/bbatsov/crux
(use-package crux
  :bind (("C-c o" . crux-open-with)
         ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
         ("C-a" . crux-move-beginning-of-line)
         ([S-return] . crux-smart-open-line)
         ("M-o" . crux-smart-open-line)
         ([C-S-return] . crux-smart-open-line-above)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c s" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("C-^" . crux-top-join-line)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("s-o" . crux-smart-open-line-above)))

;; Move a line or region up and down
;; https://github.com/emacsfodder/move-text
(use-package move-text
  :bind (([C-S-up] . move-text-up)
         ([C-S-down] . move-text-down)))

;; highlights pasted text and undos, etc QUESTIONABLE utility
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; expand region resonably QUESTIONABLE because I dont' use it
;; but it's magnars so it's probably good.
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(use-package super-save
  :config
  (setq super-save-actions '(ace-window
                             avy-goto-line
                             avy-goto-word-or-subword-1
                             counsel-find-file
                             counsel-projectile-find-file
                             counsel-projectile-ag
                             counsel-projectile-rg
                             ivy-resume
                             rubocop-check-project
                             rubocop-format-project
                             rubocop-check-directory
                             rubocop-format-directory
                             rubocop-check-current-file
                             rubocop-autocorrect-project
                             rubocop-format-current-file
                             rubocop-autocorrect-directory
                             rubocop-autocorrect-current-file))
  (dolist (action super-save-actions)
    (add-to-list 'super-save-triggers action))
  (super-save-mode +1))



(use-package ivy :ensure t
  ;; :diminish (ivy-mode . "")             ; does not display ivy in the modeline
  :init
  (ivy-mode 1)                          ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line)) ; Make highlight extend all the way to the right


;; Jump to a definition in any open buffer
;; https://github.com/vspinu/imenu-anywhere
(use-package imenu-anywhere
  ;; TODO: probably need ivy before we bind functions in it
  :bind ("C-." . ivy-imenu-anywhere))

;; smartparens. use it more
(use-package smartparens
  :commands (sp-wrap-with-pair)
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-from-inside nil)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  :bind
  (("M-(" . sp-wrap-round)
   ("M-\"" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
   ("M-{" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))))

(use-package magit
  :config
  (setq magit-define-global-key-bindings t)
  (setq magit-bury-buffer-function 'magit-mode-quit-window)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-section-initial-visibility-alist '((stashes . hide) (unpushed . show)))
  :bind
  (("C-c g" . magit-file-dispatch)
   :map global-map
   :prefix-map magit-super-map
   :prefix "s-m"
   ("m" . magit-status)
   ("j" . magit-dispatch)
   ("k" . magit-file-dispatch)
   ("l" . magit-log-buffer-file)
   ("b" . magit-blame)))
