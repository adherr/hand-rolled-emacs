;; -*- lexical-binding: t; -*-
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package (now built in)
(use-package use-package
  :config (setq use-package-always-demand t))

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-host-usernames `((github . "adherr"))))

;; load my path
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
        (message "Native comp is available")
        (when (eq system-type 'darwin)
          (progn
            ;; bin inside the Emacs.app
            (add-to-list 'exec-path (concat invocation-directory "bin") t)
            ;; this is a combination of https://xenodium.com/trying-out-gccemacs-on-macos/
            ;; and stuff from this thread https://github.com/d12frosted/homebrew-emacs-plus/issues/378#issuecomment-1666548762
            (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                           (when (getenv "LIBRARY_PATH")
                                             ":")
                                           (mapconcat (lambda (path) (car (file-expand-wildcards path)))
                                                      '("/usr/local/opt/gcc/lib/gcc/*"
                                                        "/usr/local/opt/libgccjit/lib/gcc/*"
                                                        "/usr/local/opt/gcc/lib/gcc/*/gcc/*/*"
                                                        "/opt/homebrew/opt/gcc/lib/gcc/*"
                                                        "/opt/homebrew/opt/libgccjit/lib/gcc/*"
                                                        "/opt/homebrew/opt/gcc/lib/gcc/*/gcc/*/*")
                                                      ":")))))
        ;; Only set after LIBRARY_PATH can find gcc libraries.
        (setq comp-deferred-compilation t))
    (message "Native comp is *not* available"))
  (dolist (var '("LANG" "LC_CTYPE" "LIBRARY_PATH" "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var)))

(defvar line-length 120)
;; base Emacs config
(use-package emacs
  :config
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)
  (blink-cursor-mode -1)
  ;; toolbar isn't on in TTY
  (tool-bar-mode -1)
  ;; Don't use messages that you don't read
  (setq initial-scratch-message "")
  ;; Get back that column
  (scroll-bar-mode -1)
  ;; I'll take the lines too
  (menu-bar-mode -1)
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
  ;; I don't think I've ever successfully transposed words, but it's a mess when I open tabs in emacs
  (unbind-key "M-t" global-map)
  ;; desktop saving
  (defvar savefile-dir (expand-file-name "savefile" user-emacs-directory) "Where we save emacs's state containing files")
  (unless (file-exists-p savefile-dir)
    (make-directory savefile-dir t))
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (setq desktop-path (list savefile-dir))
  (setq desktop-dirname savefile-dir)
  (setq desktop-restore-eager 6)
  (desktop-save-mode 1)
  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  ;; setup recenf mode because sometimes it's helpful
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode)
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
        '(search-ring regexp-search-ring vertico-repeat-history)
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
  ;; https://stackoverflow.com/a/71785402
  (setq compilation-ask-about-save nil  ; Just save before compiling
        compilation-always-kill t       ; Just kill old compile processes before starting the new one
        compilation-scroll-output 'first-error) ; Automatically scroll to first error
  (use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

  ;;; advice for find-file to open at line-number using <filename>:<line-number> format
  ;; from https://www.emacswiki.org/emacs/find-file-with-line-number
  (define-advice find-file (:around (proc filename &optional wildcards) with-line-number)
    "if format is <filename>:#, open file at line-number #"
    (let* (;; fap-<junk> deals with ffap stripping line numbers
           (fap (thing-at-point 'filename t))
           (fap-lino-idx (if fap (string-match ":[0-9]+$" fap)))
           (fap-line-num (if fap-lino-idx
                             (string-to-number (substring fap (1+ (match-beginning 0)) (match-end 0)))))
           (fap-name (if fap (expand-file-name (if fap-lino-idx (substring fap 0 fap-lino-idx) fap))))
           ;; fn-<junk> deals with the filename in the minibuffer
           (fn-lino-idx (string-match ":[0-9]+$" filename))
           (fn-line-num (if fn-lino-idx
                            (string-to-number (substring filename (1+ (match-beginning 0)) (match-end 0)))))
           (filename (if fn-lino-idx (substring filename 0 fn-lino-idx) filename))
           ;; pick out the right line number (fap- or fn-, which may have been edited by the user)
           (line-number (cond (;; the first condition is necessary becaue fn-line-num nil with
                               ;; fap-line-num non-nil would default to wrong line number
                               (not (equal filename fap-name)) fn-line-num)
                              (fn-line-num fn-line-num)   ; prefer user's line-num ...
                              (fap-line-num fap-line-num) ; ... over fap's line-num
                              (t nil)))                   ; no line numbers anywhere
           (res (apply proc filename '(wildcards)))) ; funcall also works with same syntax
      (when line-number
        (goto-char (point-min))
        (forward-line (1- line-number)))
      res))

  ;; Editorish things
  (set-frame-font "BlexMono Nerd Font JBM Ligatured CCG 9" nil t)
  (setq-default indent-tabs-mode nil) ;; don't use tabs to indent
  (setq-default tab-width 8) ;; but maintain correct appearance
  (setq require-final-newline t) ;; Newline at end of file
  (delete-selection-mode t) ;; delete the selection with a keypress
  ;; hippie-expand some things
  ;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
  ;;                                          try-expand-dabbrev-all-buffers
  ;;                                          yas-hippie-try-expand
  ;;                                          try-expand-dabbrev-from-kill
  ;;                                          try-complete-file-name-partially
  ;;                                          try-complete-file-name
  ;;                                          try-expand-all-abbrevs
  ;;                                          try-expand-list
  ;;                                          try-expand-line
  ;;                                          try-complete-lisp-symbol-partially
  ;;                                          try-complete-lisp-symbol))
  (setq tab-always-indent 'complete) ;; trigger corfu if already indented
  (setq blink-matching-paren nil) ;; disable annoying blink-matching-paren
  ;; ispell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (setq text-mode-ispell-word-completion nil)
  (flyspell-mode +1)
  ;; highlight the current line
  (global-hl-line-mode +1)
  ;; show whitespace
  (setq whitespace-line-column line-length) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)) ;; add lines-tail to highlight the end of long lines when required
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
  :mode
  ("\\.zsh\\'" . shell-script-mode)
  ("\\.zshrc\\'" . shell-script-mode)
  ("\\.env\\'" . shell-script-envrc)
  ("\\.mode\\'" . shell-script-mode)
  ;; ruby mode should include rbi files
  ("\\.rbi\\'" . ruby-ts-mode)


  :bind
  (
   ;; ("M-/" . hippie-expand) ;; replaced with dabbrev expand and corfu
   ("C-x O" . (lambda () (interactive)
                (other-window -1)))
   ("s-[" . (lambda () (interactive) (insert-char #x201c)))
   ("s-{" . (lambda () (interactive) (insert-char #x201d)))
   ("s-]" . (lambda () (interactive) (insert-char #x2018)))
   ("s-}" . (lambda () (interactive) (insert-char #x2019)))
   ("<M-down-mouse-1>" . browse-url-at-mouse)
   ("s-u" . revert-buffer))

  ;; go to definition help functions
  (:map help-map
        ("C-f" . find-function)
        ("C-k" . find-function-on-key)
        ("C-v" . find-variable)
        ("C-l" . find-library)
        ("C-i" . info-display-manual))

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
    :config (unless (server-running-p) (server-start)))

  ;; emoji
  :if (fboundp 'set-fontset-font)
  :config (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  )
;; end MacOS

(use-package minions
  :config
  (minions-mode +1)
  :bind ("<S-down-mouse-3>" . minions-minor-modes-menu))

;; smartparens. use it more
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (setq sp-show-pair-from-inside nil)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  ;; this bombs a bunch of bindings into everywhere, so keep this near the top, so custom bindings aren't overridden
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  :bind
  (("M-(" . sp-wrap-round)
   ("M-\"" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
   ("M-{" . sp-wrap-curly)
   ("C-s-k" . sp-kill-hybrid-sexp)
   ("C-M-k" . sp-kill-sexp)))

;; love me some zenburn theme
(use-package zenburn-theme
  :straight (:host github :repo "bbatsov/zenburn-emacs")
  :config
  (load-theme 'zenburn t))

;; show all of the completions from the keys entered so far
(use-package which-key
  :config
  (which-key-mode))

;; let's vterm for getting a terminal in emacs
;; https://github.com/akermu/emacs-libvterm
(use-package vterm)

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
  :bind (;; mimic popular IDEs binding, note that it doesn't work in a terminal session
         ("C-a" . crux-move-beginning-of-line)
         ([S-return] . crux-smart-open-line)
         ("M-o" . crux-smart-open-line)
         ([C-S-return] . crux-smart-open-line-above)
         ([C-backspace] . crux-kill-line-backwards)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
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
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("s-o" . crux-smart-open-line-above)))


;; expand region resonably QUESTIONABLE because I dont' use it
;; but it's magnars so it's probably good.
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(use-package super-save
  :config
  (setq super-save-actions '(ace-window
                             avy-goto-char-timer
                             avy-goto-line
                             avy-goto-word-or-subword-1
                             consult-imenu-multi
                             consult-ripgrep
                             find-file
                             minitest-rerun
                             minitest-verify
                             minitest-verify-all
                             minitest-verify-single
                             projectile-find-file
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
  (("M-g g" . avy-goto-line)
   ("C-c j" . avy-goto-char-timer)))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  :bind
  (("s-w" . ace-window)))

;; (use-package swiper
;;   :bind
;;   (("C-s" . swiper-isearch)))

;; (use-package ivy
;;   ;; :diminish (ivy-mode . "")             ; does not display ivy in the modeline
;;   :init
;;   (ivy-mode 1)                          ; enable ivy globally at startup
;;   :config
;;   (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
;;   (setq ivy-wrap t)
;;   (setq ivy-height 20)                   ; set height of the ivy window
;;   (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page
;;   (setq ivy-display-style 'fancy)
;;   :bind
;;   (("<f6>" . ivy-resume)))

;; (use-package counsel
;;   :bind
;;   (("M-x" . counsel-M-x)
;;    ("M-y" . counsel-yank-pop)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-h a" . counsel-apropos)
;;    ("C-h f" . counsel-describe-function)
;;    ("C-h o" . counsel-describe-symbol)
;;    ("C-h v" . counsel-describe-variable)
;;    ("<f2> i" . counsel-info-lookup-symbol)
;;    ("<f2> u" . counsel-unicode-char))
;;   ;;:bind (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history))
;;   )

;; replaced with vertico history sort!
;; alternative M-x with history and sorting
;; it's not really editorish, but ivy needs to be loaded first
;; https://github.com/DarwinAwardWinner/amx
;; (use-package amx
;;   :config
;;   (setq-default amx-save-file (expand-file-name "amx-history" savefile-dir))
;;   (amx-mode))

;; replace ivy with vertico. It does one thing well rather than replacing all of the commands
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  ;; why would we sort by length?
  (setq vertico-sort-function 'vertico-sort-history-alpha)
  ;; this is ivy's S-SPC
  (defun +vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
  :bind (:map vertico-map
              ("S-SPC" . +vertico-restrict-to-matches)))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; this is probably not necessary since I figured out how to set the default sort
;; (use-package vertico-multiform
;;   :straight nil
;;   :after vertico
;;   :config
;;   ;; Configure the default sorting function for symbols and files
;;   ;; See `vertico-sort-function'.
;;   (setq vertico-multiform-categories
;;         '((symbol (vertico-sort-function . vertico-sort-alpha))
;;           (command (vertico-sort-function . vertico-sort-history-alpha))
;;           (file (vertico-sort-function . sort-directories-first))))

;;   (defun sort-directories-first (files)
;;     ;; Still sort by history position and alphabetically
;;     (setq files (vertico-sort-history-alpha files))
;;     ;; But then move directories first
;;     (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
;;            (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
;;   (vertico-multiform-mode))

;; idk, sometimes I'm coming back to the frame from elsewhere and there's completion going on so I click on shit
(use-package vertico-mouse
  :straight nil
  :after vertico
  :config
  (vertico-mouse-mode))

;; this is a bit like ivy's ivy-resume. Note it saves its history between sessions with savehist above
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("<f6>" . vertico-repeat))

;; Allow searches to match space separated keywords in any order (as regexes)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; add help information to the completion results in the minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; do stuff from where we are. Config lifted straight from
;; https://github.com/oantolin/embark
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   (:map embark-become-file+buffer-map ("p" . projectile-find-file)))


  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; various searching commands
(use-package consult
  :config
  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))
  :bind
  (("M-i" . consult-imenu)
   ;; ("C-." . consult-imenu-multi)
   ("C-x b" . consult-buffer)
   ("C-c b" . consult-project-buffer)
   ("M-y" . consult-yank-replace)
   ("C-c f" . consult-recent-file)
   ("C-s" . consult-line)))

;; cuz it's awesome. Used by consult, so we don't config here
;; https://github.com/nlamirault/ripgrep.el
(use-package ripgrep)

;; allow us to edit a grep buffer
;; https://github.com/mhayashi1120/Emacs-wgrep
;; How to use: consult-ripgrep -> embark-consult / embark-export -> change grep buffer to wgrep C-c C-p -> edit lines -> C-x s apply changes and save all buffers
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; better help
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))

;; company to complete anywhere
;; (use-package company
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-active-map
;;               ("<tab>" . company-complete-selection))

;;   :custom
;;   (company-backends '((company-capf company-dabbrev-code)))
;;   (company-idle-delay 0.2)
;;   (company-minimum-prefix-length 3)
;;   (company-tooltip-align-annotations t)
;;   (company-tooltip-limit 20)

;;   :config
;;   (setq lsp-completion-provider :capf))

;; ;; a new frontend that's hopefully better. suggested by lsp-mode
;; ;; https://github.com/sebastiencs/company-box
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; completion at point with a popup
;; https://github.com/minad/corfu
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1)
  (corfu-cycle t)
  (cofu-quit-no-match t)
  (corfu-preselect 'valid))

;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons)
;; icons for corfu, cuz it's important
;; https://github.com/LuigiPiucco/nerd-icons-corfu
(use-package nerd-icons-corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;; dired too
;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
;; treemacs is better with default icons, although they probably don't work in console 🤷
;; https://github.com/rainstormstudio/treemacs-nerd-icons
;; (use-package treemacs-nerd-icons
;;   :config
;;   (treemacs-load-theme "nerd-icons"))

;; add more completion at point functions
;; https://github.com/minad/cape
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'prog-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions
                          #'cape-keyword nil t))))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  ;; :bind (("M-/" . dabbrev-completion)
         ;; ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

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
   ("t" . git-timemachine)
   ("h" . git-link)
   ("b" . magit-blame)))

;; browse old versions of a file
;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine)

;; I like to paste working github links into slack
;; https://github.com/sshaw/git-link
(use-package git-link
  :commands git-link
  :custom
  (git-link-default-branch "main"))

;; add the git diff highlights to the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; Projects with projectile (although maybe we should switch to built-in project.el)
(use-package projectile
  :init (projectile-mode t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              :map projectile-command-map
              ;; consult ripgrep obeys project setting, and it's nicer than the default projectile command
              ("s r" . consult-ripgrep)
              ("w f" . +kill-project-file-path)
              ("w l" . +kill-project-file-line-path))
  :config
  (defun +project-file-path ()
    (file-relative-name buffer-file-name (projectile-project-root)))

  (defun +kill-project-file-path ()
    (interactive)
    (let ((path (+project-file-path)))
      (kill-new path)
      (message path)))

  (defun +kill-project-file-line-path ()
    (interactive)
    (let ((path-line (format "%s:%s"
                             (+project-file-path)
                             (line-number-at-pos))))
      (kill-new path-line)
      (message path-line)))
  ;; TODO: make sure this is a git repo before running magit-status and default to something else otherwise
  :custom (projectile-switch-project-action 'magit-status))

;; Switch env vars when you navigate to a .envrc project
(use-package direnv
  :config
  (direnv-mode))

;; treemacs for that file browser goodness
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :config
  (setq treemacs-width 65)
  (defun treemacs-exclusive-show ()
    (interactive)
    (treemacs-display-current-project-exclusively)
    (treemacs-select-window))
  :bind
  (("<f8>" . treemacs)
   ("<f9>" . treemacs-exclusive-show)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Actually edit text ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Move a line or region up and down
;; https://github.com/emacsfodder/move-text
(use-package move-text
  :bind (([C-S-up] . move-text-up)
         ([C-S-down] . move-text-down)))

;; visual, more powerful zap-to-char
;; https://github.com/thierryvolpiatto/zop-to-char
(use-package zop-to-char
  :bind
  ([remap zap-to-char] . zop-to-char))

;; I rely on M-w to copy whole line with no region. This is available with easy-kill
;; https://github.com/leoliu/easy-kill
(use-package easy-kill
  :bind
  ("M-w" . easy-kill))

;; visual feedback on search and replace
;; https://github.com/emacsorphanage/anzu
(use-package anzu
  :init
  (global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;; editing lisp sorta sucks without this
(use-package rainbow-delimiters
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

;; full angry-fruit-salad mode. Maybe I should try prism again, as I'm not sure this is valuable enough to add this much chaos
;; (use-package rainbow-identifiers
;;   :hook (prog-mode . rainbow-identifiers-mode))

;; prism colors by code nesting depth
;; Tried this, didn't love the way it handles comments-I think they should always be the same color, not just desaturated at the level they appear.
;; (use-package prism
  ;; you need different modes for whitespace delimited languages
  ;; :hook ((elisp-mode ruby-ts-mode) . prism-mode))

;; ligatures, for fun
;; https://github.com/jming422/fira-code-mode
;; (use-package fira-code-mode
;;   :config
;;   ;; (fira-code-mode-install-fonts) ;; this prompts every time :(
;;   (global-fira-code-mode))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("--" "---" "==" "===" "!=" "!==" "=!="
                              "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                              "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                              "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                              "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                              "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                              "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                              "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                              "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                              "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                              ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                              "<:<" ";;;"))
  (global-ligature-mode t))

;; snippets! LSP wants this and I want to make a logging snippet
;; https://jdhao.github.io/2021/10/06/yasnippet_setup_emacs/
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :config
  (setq yas-indent-line 'auto)
  (yas-global-mode 1))
(use-package yasnippet-snippets)

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;


;;;;;;;; General

;; eglot for LSP. Maybe we'll try LSP mode later for sorbet if we need to LSs at the same time
;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                `((ruby-mode ruby-ts-mode) . ,(eglot-alternatives
;;                                               '(("srb" "tc" "--lsp")
;;                                               ("solargraph" "socket" "--port" :autoport))))))
;;                '(ruby-base-mode .
;;                                     '(("solargraph")
;;                                       )))))

;; it looks like LSP mode supports sorbet out of the box
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; orderless completion setup from https://github.com/minad/corfu/wiki#advanced-example-configuration-with-orderless
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))


  (setq lsp-enabled-clients '(sorbet-ls ruby-ls graphql-lsp ts-ls eslint elixir-ls))
  ;; (setq lsp-enabled-clients '(sorbet-ls ruby-lsp-ls graphql-lsp ts-ls eslint))
  :config
  ;; these are emacs settings for lsp performance
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000) ;; 100mib

  (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("bundle" "exec" "rubocop" "--lsp"))
                      :activation-fn (lsp-activate-on "ruby")
                      :add-on? t
                      :server-id 'my-rubocop-ls))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp\\'")
  :custom
  (lsp-completion-provider :none) ;; corfu
  (lsp-sorbet-as-add-on t)
  (lsp-elixir-local-server-command "/usr/lib/elixir-ls/language_server.sh")
  ;; (lsp-eslint-server-command '("node" "/Users/andrew.herr/.vscode/extensions/dbaeumer.vscode-eslint-2.4.2/server/out/eslintServer.js" "--stdio"))
  :hook (((elixir-ts-mode graphql-mode js-base-mode ruby-base-mode typescript-ts-base-mode) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)))

;; (use-package lsp-treemacs)
;; (use-package lsp-ui)

;; GH CoPilot??
;; https://github.com/zerolfx/copilot.el
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; flycheck mode to highlight warnings and errors in code
;; https://www.flycheck.org/en/latest
(use-package flycheck
  :init (global-flycheck-mode))

;; get the treesit goodness without specifying grammar download locations or major mode translations
;; REMEMBER that hooks don't transfer to the ts mode
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; tree-sitter navigation and semantic editing
;; https://github.com/mickeynp/combobulate
(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; subword mode is required! (built in)
(use-package subword
  :straight nil
  :config (global-subword-mode 1))

;;;;;;;;; Languages

;; markdown mode
(use-package markdown-mode
  ;; :ensure-system-package pandoc
  :commands gfm-mode
  :mode (("\\.md$" . gfm-mode))
  :config
  (custom-set-faces
   '(markdown-pre-face ((t nil))))

  (setq markdown-command "pandoc --standalone --mathjax --from=gfm"
        markdown-disable-tooltip-prompt t
        markdown-fontify-code-blocks-natively t))

;; this is built in, but not associated with .yml files
(use-package yaml-ts-mode
  :mode (("\\.yml" . yaml-ts-mode)
         ("\\.yaml" . yaml-ts-mode)))

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

;; web mode to deal with templates and regular html
;; https://web-mode.org/
(use-package web-mode
  :mode ("\\.erb$"
         "\\.html$"
         "\\.php$"
         "\\.rhtml$")

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2))

;; config here is from HRS with a few changes
;; https://github.com/pezra/rspec-mode/
(use-package rspec-mode
  ;; :after ruby-base-mode
  ;; :ensure-system-package (rspec . "gem install rspec")

  :hook (css-mode
         deadgrep-mode
         js-mode
         magit-status-mode
         ruby-base-mode
         scss-mode
         web-mode
         yard-mode)

  :config
  (defun +rspec-package-root-directory-p (directory)
    (file-regular-p (expand-file-name "package.yml" directory)))

  (defun +rspec-package-root (&optional directory)
    "Find the root directory of the package.
     Walk the directory tree until it finds a package.yml file."
    (let ((directory (file-name-as-directory (or directory default-directory))))
      (cond ((rspec-root-directory-p directory)
             (error "Could not determine the project root."))
            ((+rspec-package-root-directory-p directory) (expand-file-name directory))
            (t (+rspec-package-root (file-name-directory (directory-file-name directory)))))))

  (defun rspec-target-in-holder-dir-p (a-file-name)
    (string-match (concat "^" (concat
                               (regexp-quote
                                (+rspec-package-root a-file-name))
                               (regexp-opt rspec-primary-source-dirs)
                               "/"))
                  a-file-name))
  :bind (:map rspec-verifiable-mode-keymap
                ("s" . rspec-verify-single))
  :custom
  ;; this is for Gusto/zenpayroll where the binstub takes care of bundler and spring
  (rspec-use-spring-when-possible nil)
  (rspec-use-bundler-when-possible nil)
  (rspec-spec-command "bin/rspec --no-profile")
  (rspec-command-options "--color"))

;; standard test mode keybindings, not as featureful as rspec-mode
;; https://github.com/arthurnn/minitest-emacs
(use-package minitest
  :after ruby-base-mode
  :config (setq minitest-use-rails t)
  :hook (ruby-base-mode . minitest-mode))

;; only activate rspec-mode or minitest-mode depending on the project I'm working in
;; lifted from HRS here: https://github.com/hrs/dotfiles/blob/main/emacs/.config/emacs/configuration.org#ruby
(defvar +ruby-testable-mode-hooks
  '(css-mode-hook
    deadgrep-mode-hook
    js-mode-hook
    magit-status-mode-hook
    ruby-base-mode-hook
    scss-mode-hook
    web-mode-hook
    yard-mode-hook))

(defun +current-project-uses-minitest-p ()
  (and (project-current)
       (file-directory-p (expand-file-name "test" (project-root (project-current))))))

(defun +activate-ruby-tests-mode ()
  (if (+current-project-uses-minitest-p)
      (progn
        (minitest-mode 1)
        (rspec-mode 0)
        (rspec-verifiable-mode 0))
      (progn
        (minitest-mode 0)
        (rspec-mode 1)
        (rspec-verifiable-mode 1))))

(dolist (hook +ruby-testable-mode-hooks)
  (add-hook hook #'+activate-ruby-tests-mode))

;; give me an interactive shell if we hit a breakpoint
(use-package inf-ruby
  :config
  (inf-ruby-enable-auto-breakpoint)
  :hook (ruby-base-mode . inf-ruby-minor-mode))

;; it would be nice to be able to run bundle without switching apps
;; https://github.com/endofunky/bundler.el
(use-package bundler
  :defer t
  :commands bundle-install)

;; projectile-rails so I theoretically never need to use the terminal
;; https://github.com/asok/projectile-rails
(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map ("C-c e" . projectile-rails-command-map)))

;; I'd like to run rubocop manually until I can figure out how to get the lsp to do it
;; (figured it out, but it requires the lsp to be in the bundle, which will be hard to manage. lsp-format-buffer)
(use-package rubocop)

;; autoformat with rubocop via the lsp. We'll see
(use-package rubocopfmt
  :hook
  (ruby-base-mode . rubocopfmt-mode)
  :custom
  (rubocopfmt-on-save-use-lsp-format-buffer t))

;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;
(use-package typescript-ts-mode
  :straight nil
  :config
  (setq js-indent-level 2)
  ;; let the lsp do the goto
  (unbind-key "M-." js-mode-map)
  (unbind-key "M-." js-ts-mode-map)
  :mode
  (("\\.tsx\\'" . tsx-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode)
   ("\\.js\\'" . typescript-ts-mode)))

;; I couldn't make it work, and the keybindings are in all the wrong places
;; (use-package jest-test-mode
;;   :commands jest-test-mode
;;   :config
;;   (setq jest-test-mode-map "C-c ,")
;;   :custom
;;   (jest-test-options '())
;;   (jest-test-command-string "yarn %s test %s %s")
;;   :hook (typescript-ts-base-mode))

;; run jest tests with a popup (jest-popup)
;; https://github.com/emiller88/emacs-jest
(use-package jest
  :custom
  (jest-executable "yarn test")
  (jest-unsaved-buffers-behavior 'save-current)
  :bind
  (:map jest-minor-mode-map
        (("C-c , v" . jest-file)
         ("C-c , r" . jest-repeat)
         ("C-c , RET" . jest-popup)))
  :hook (typescript-ts-base-mode . jest-minor-mode))

(use-package graphql-mode
  :mode
  ("\\.graphql\\'" . graphql-mode))

(use-package prettier
  :config
  (add-to-list 'prettier-major-mode-parsers '(typescript-ts-base-mode . (typescript babel-ts)))
  (global-prettier-mode))

;;;;;;;;;;;;
;; Elixir ;;
;;;;;;;;;;;;

;; just for elixir-format
(use-package elixir-mode)

;; I think this is what we want?
(use-package elixir-ts-mode
  :mode ("\\.ex\\'". elixir-ts-mode))

;;;;;;;;;;;;;;;;
;; Arch Linux ;;
;;;;;;;;;;;;;;;;

;; this is a thing? cool.
;; https://github.com/UndeadKernel/pacfiles-mode
(use-package pacfiles-mode)
