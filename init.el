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
  ;; I don't particularly like recentf, but I don't know how to stop it, so let's configure it
  (require 'recentf)
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
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
  (setq windmove-wrap-around t)

  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; clean up buffers untouched for 3 days automatically
  (midnight-delay-set 'midnight-delay "4:30am")

  ;; tramp, for sudo access
  ;; keep in mind known issues with zsh - see
  ;; https://blog.karssen.org/2016/03/02/fixing-emacs-tramp-mode-when-using-zsh/
  (setq tramp-default-method "ssh")

  ;; compilation settings
  ;; https://stackoverflow.com/a/63710493/4534357
  ;; https://github.com/atomontage/xterm-color#compilation-buffers
  (use-package xterm-color)
  (setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first error
      compilation-environment '("TERM=xterm-256color")
      )
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter)  ;; colorize compilation buffer

  ;; Editorish things
  (setq-default indent-tabs-mode nil) ;; don't use tabs to indent
  (setq-default tab-width 8) ;; but maintain correct appearance
  (setq require-final-newline t) ;; Newline at end of file
  (delete-selection-mode t) ;; delete the selection with a keypress
  ;; hippie-expand some things
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           ;; yas-hippie-try-expand
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

  ;; enable narrowing commands (C-x n ...) HIGHLY QUESTIONABLE
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; enabled change region case commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; C-left &c. window navigation
  (winner-mode +1)

  ;; enable erase-buffer command HIGHLY QUESTIONABLE
  (put 'erase-buffer 'disabled nil)

  ;; ediff - don't start another frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; make a shell script executable automatically on save
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; .zsh file is shell script too
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\.zshrc\\'" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))

  :bind
  ("M-/" . hippie-expand)

  :hook
  ;; enable some really cool extensions like C-x C-j(dired-jump)
  ((dired-load . (lambda () (load "dired-x")))
   ;; cleanup whitespace on save
   (before-save . whitespace-cleanup))
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

;; undo-tree
;; https://gitlab.com/tsc25/undo-tree
(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; highlights pasted text and undos, etc QUESTIONABLE utility
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; Useful functions from bbatsov
;; https://github.com/bbatsov/crux
(use-package crux
  :config (crux-with-region-or-line kill-region)
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

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  :bind
  (("M-g g" . avy-goto-line)))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-ignore-current t)
  (setq aw-minibuffer-flag t)
  :bind
  (("s-w" . ace-window)))

(use-package ivy
  ;; :diminish (ivy-mode . "")             ; does not display ivy in the modeline
  :init
  (ivy-mode 1)                          ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
  (setq ivy-wrap t)
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line)  ; Make highlight extend all the way to the right
  :bind
  (("<f6>" . ivy-resume)))

(use-package swiper
  :bind
  (("C-s" . swiper-isearch)))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h o" . counsel-describe-symbol)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char))
  ;;:bind (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history))
  )

;; alternative M-x with history and sorting
;; it's not really editorish, but ivy needs to be loaded first
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :config
  (setq-default amx-save-file (expand-file-name "amx-history" savefile-dir))
  (amx-mode))

;; visual feedback on search and replace
;; https://github.com/emacsorphanage/anzu
(use-package anzu
  :init
  (global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;; Jump to a definition in any open buffer
;; https://github.com/vspinu/imenu-anywhere
(use-package imenu-anywhere
  :bind ("C-." . ivy-imenu-anywhere))

;; smartparens. use it more
(use-package smartparens
  :commands (sp-wrap-with-pair)
  :init
  (require 'smartparens-config)
  :config
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

;; add the git diff to highlights to the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package projectile
  :init (projectile-mode t)
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)))
