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
  ;; mise activate resolves versions per-shell-cd, which exec-path-from-shell
  ;; can't replicate (it captures PATH once at startup); mise's shims dir
  ;; re-resolves the right version per invocation, so prioritize it here.
  (add-to-list 'exec-path (expand-file-name "~/.local/share/mise/shims"))
  (setenv "PATH" (concat (expand-file-name "~/.local/share/mise/shims") ":" (getenv "PATH")))
  ;; native comp stolen from https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-package-extensions.el#L19
  (if (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
      (progn
	(message "Native comp is available")
	(when (eq system-type 'darwin)
	  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
	(setq native-comp-async-report-warnings-errors 'silent)
	;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
	;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
	;; Append to path to give priority to values from exec-path-from-shell-initialize.
	(add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
	(setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
				       (when (getenv "LIBRARY_PATH")
					 ":")
				       ;; This is where Homebrew puts libgccjit libraries.
				       (car (file-expand-wildcards
					     (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")))))
	;; Only set after LIBRARY_PATH can find gcc libraries.
	(setq comp-deferred-compilation t)
	(setq comp-speed 3))
    (message "Native comp is *not* available"))
  (dolist (var '("LANG" "LC_CTYPE" "LIBRARY_PATH" "LSP_USE_PLISTS" "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var)))

(defvar line-length 120)
;; redirect custom to its own file so it doesn't pollute init.el. The old
;; custom.el predated Emacs auto-inserting a lexical-binding cookie on save
;; (cus-edit.el only adds it when the buffer is empty), so it kept loading
;; without one and warning. Deleted it so the next customize-save starts
;; fresh and picks up the cookie.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; base Emacs config
(use-package emacs
  :custom
  (custom-safe-themes
   '("f87c86fa3d38be32dc557ba3d4cedaaea7bc3d97ce816c0e518dfe9633250e34" default))
  (safe-local-variable-values
   '((git-link-default-branch . "master") (encoding . utf-8)))
  :config
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)
  (blink-cursor-mode -1)
  (setq mouse-wheel-tilt-scroll t) ; two-finger left/right trackpad scroll
  (setq mouse-wheel-flip-direction t) ; direction came in backwards
  ;; `scroll-left' has no built-in limit, so with truncate-lines on it'll
  ;; happily scroll past the end of every visible line into blank space
  (advice-add 'scroll-left :around
              (lambda (orig-fn &optional arg set-minimum)
                (when (< (window-hscroll)
                         (- (save-excursion (end-of-line) (current-column))
                            (window-width) -2))
                  (funcall orig-fn arg set-minimum))))
  ;; tmux's TERM ("tmux-256color") isn't a recognized xterm variant, so
  ;; terminal-init-xterm wouldn't otherwise run for tmux ttys
  (add-to-list 'term-file-aliases '("tmux-256color" . "xterm-256color"))
  ;; not gated on (display-graphic-p): that's only true for the frame
  ;; that exists when init.el loads, not later emacsclient tty frames
  (xterm-mouse-mode 1)
  (setq mouse-wheel-mode t)

  ;; dotenv/.envrc files are consumed by bash-ish tooling (dotenv loaders,
  ;; direnv), not zsh - don't let sh-mode fall back to $SHELL's dialect
  (defun my/sh-mode-bash ()
    (sh-mode)
    (sh-set-shell "bash" nil nil))
  ;; toolbar isn't on in TTY
  (tool-bar-mode -1)
  ;; Don't use messages that you don't read
  (setq initial-scratch-message "")
  ;; Get back that column
  (scroll-bar-mode -1)
  ;; I'll take the lines too
  (menu-bar-mode -1)
  ;; y or n instead of typing
  (setopt use-short-answers t)
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
  ;; propagate frame-title-format to the terminal's OSC 2 title too
  ;; (GUI frames get this for free via the native titlebar)
  (setq xterm-set-window-title t)
  ;; confirm exit because fat fingers
  (setq confirm-kill-emacs 'y-or-n-p)
  ;; dotfiles are a symlink farm into a git-controlled repo; always follow
  ;; without asking, instead of prompting per-buffer on every restore
  (setq vc-follow-symlinks t)
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
  (midnight-delay-set 'midnight-delay "04:30")

  ;; tramp, for sudo access
  ;; keep in mind known issues with zsh - see
  ;; https://blog.karssen.org/2016/03/02/fixing-emacs-tramp-mode-when-using-zsh/
  (setq tramp-default-method "ssh")

  ;; compilation settings
  ;; https://stackoverflow.com/a/71785402
  (setq compilation-ask-about-save nil  ; Just save before compiling
	compilation-always-kill t       ; Just kill old compile processes before starting the new one
	compilation-scroll-output 'first-error ; Automatically scroll to first error
	compilation-max-output-line-length nil) ; Don't hide long lines
  (use-package ansi-color
    :straight nil
    :hook (compilation-filter . ansi-color-compilation-filter))

  (defun +func-region (start end func)
    "run a function over the region between START and END in current buffer."
    (save-excursion
      (let ((text (delete-and-extract-region start end)))
	(insert (funcall func text)))))

  (use-package url
    :straight nil
    :config
    (defun +hex-region (start end)
      "urlencode the region between START and END in current buffer."
      (interactive "r")
      (+func-region start end #'url-hexify-string))

    (defun +unhex-region (start end)
      "de-urlencode the region between START and END in current buffer."
      (interactive "r")
      (+func-region start end #'url-unhex-string)))

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
  (set-frame-font "Plex Mono 12" nil t)
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
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; highlight the current line
  (global-hl-line-mode +1)
  ;; show whitespace
  (setq whitespace-line-column line-length) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)) ;; add lines-tail to highlight the end of long lines when required
  (global-whitespace-mode +1)

  ;; put me on the last copy when I duplicate stuff
  (setq duplicate-region-final-position -1)
  (setq duplicate-line-final-position -1)

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

  ;; don't validate XML schemas, because nXML mode only works with RELAX NG schemata, and it seems like a lot of work to set those up
  ;; https://www.gnu.org/software/emacs/manual/html_mono/nxml-mode.html#Locating-a-schema
  ;; https://fedoraproject.org/wiki/How_to_use_Emacs_for_XML_editing
  ;; trang https://relaxng.org/#conversion is available with `brew install jing-trang`
  ;; sometimes it'll crash emacs trying to validate on save
  (setq rng-nxml-auto-validate-flag nil)

  ;; .zsh file is shell script too
  :mode
  ("\\.zsh$" . shell-script-mode)
  ("\\.zshrc$" . shell-script-mode)
  ("\\(/\\|\\`\\)\\.envrc\\'" . my/sh-mode-bash)
  ("\\.env\\(\\..*\\)?\\'" . my/sh-mode-bash)
  ("Procfile*" . conf-mode)
  ;; ruby mode should include rbi files
  ("\\.rbi\\'" . ruby-ts-mode)
  ("\\.rb\\'" . ruby-ts-mode)
  ;; sass files are scss
  ("\\.sass\\'" . scss-mode)

  :bind
  (
   ("C-c d" . duplicate-dwim)
   ;; ("M-/" . hippie-expand) ;; replaced with dabbrev expand and corfu
   ("C-x O" . (lambda () (interactive)
		(other-window -1)))
   ("M-`" . other-frame)
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
   ;; (before-save . whitespace-cleanup)
   )
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

  ;; emoji — target only actual emoji codepoints, avoiding dual text/emoji chars
  ;; like ⏺ (U+23FA) and ✳ (U+2733) which Core Text would render double-width
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t '(#x1F000 . #x1FAFF) "Apple Color Emoji" nil 'prepend)
    (set-fontset-font t '(#x1F1E0 . #x1F1FF) "Apple Color Emoji" nil 'prepend)) ; flags
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
   ("C-s-f" . sp-end-of-sexp)
   ("C-s-b" . sp-beginning-of-sexp)
   ("C-M-k" . sp-kill-sexp)
    ([C-M-backspace] . sp-backward-kill-sexp)
   ("C-M-SPC" . sp-mark-sexp)))

;; love me some zenburn theme
;; (use-package zenburn-theme
;;  :straight (:host github :repo "bbatsov/zenburn-emacs")
;;  :config
;;  (load-theme 'zenburn t))

;; Try it out to be like jxpx777
(use-package base16-theme
  :straight (:host github :repo "tinted-theming/base16-emacs")
  :init
  ;; straight pulls the raw repo, not a packaged build, so the themes under
  ;; build/ never land on custom-theme-load-path automatically (see the
  ;; "Development" section of base16-emacs's README).
  (add-to-list 'custom-theme-load-path
               (expand-file-name "build" (file-name-directory (locate-library "base16-theme"))))
  :config
  ;; ghostty/tmux terminal frames (via emacsclient) both do true color, so use
  ;; the real hex values there too instead of base16's generic ANSI-name mapping.
  (setq base16-theme-256-color-source 'colors)
  (setq base16-highlight-mode-line 'contrast)
  (global-hl-line-mode -1) ;; line highlight doesn't play nice with text colors
  (load-theme 'base16-tomorrow t)
  ;; base16-theme.el (as of the current build) doesn't theme these packages, or
  ;; the Emacs 29+ font-lock-* faces tree-sitter modes use. Patched in using the
  ;; theme's own base16-theme-set-faces helper + base16-tomorrow-theme-colors,
  ;; so it stays in sync if the palette changes and works for any base16 variant.
  (base16-theme-set-faces
   'base16-tomorrow base16-tomorrow-theme-colors
   '((vertico-current                        :inherit highlight)
     (vertico-group-title                    :foreground base03 :weight bold)
     (vertico-group-separator                :foreground base03 :strike-through t)
     (vertico-multiline                      :foreground base04)

     (consult-preview-line                   :inherit highlight)
     (consult-highlight-match                :foreground base0A :weight bold)
     (consult-async-running                  :foreground base0B)
     (consult-async-failed                   :foreground base08)
     (consult-async-finished                 :foreground base04)
     (consult-file                           :foreground base0D)
     (consult-key                            :foreground base0E)

     (embark-keybinding                      :foreground base0E)
     (embark-target                          :inherit highlight)
     (embark-selected                        :background base02)

     (marginalia-key                         :foreground base0E)
     (marginalia-documentation               :foreground base04 :slant italic)
     (marginalia-file-name                   :foreground base05)
     (marginalia-size                        :foreground base09)
     (marginalia-date                        :foreground base0C)
     (marginalia-modified                    :foreground base09)
     (marginalia-file-priv-dir               :foreground base0D)
     (marginalia-file-priv-exec              :foreground base0B)

     (which-key-key-face                     :foreground base0E :weight bold)
     (which-key-command-description-face     :foreground base05)
     (which-key-group-description-face       :foreground base0D)
     (which-key-local-map-description-face   :foreground base0A)
     (which-key-separator-face               :foreground base03)
     (which-key-note-face                    :foreground base04)
     (which-key-special-key-face             :foreground base08 :weight bold)
     (which-key-docstring-face               :foreground base04 :slant italic)

     (treemacs-root-face                     :foreground base0D :weight bold)
     (treemacs-directory-face                :foreground base05)
     (treemacs-file-face                     :foreground base05)
     (treemacs-git-modified-face             :foreground base0A)
     (treemacs-git-added-face                :foreground base0B)
     (treemacs-git-renamed-face              :foreground base0C)
     (treemacs-git-ignored-face              :foreground base03)
     (treemacs-git-untracked-face            :foreground base09)
     (treemacs-git-conflict-face             :foreground base08 :weight bold)
     (treemacs-tags-face                     :foreground base0C)
     (treemacs-hl-line-face                  :inherit highlight)
     (treemacs-fringe-indicator-face         :foreground base0D)

     (mc/cursor-face                         :inverse-video t)
     (mc/region-face                         :inherit region)
     (mc/cursor-bar-face                     :background base0D :foreground base00)

     (wgrep-face                             :foreground base0B :weight bold)
     (wgrep-file-face                        :foreground base0D)
     (wgrep-done-face                        :foreground base0C)
     (wgrep-delete-face                      :foreground base08 :strike-through t)
     (wgrep-reject-face                      :foreground base08 :weight bold)

     (git-timemachine-minibuffer-author-face :foreground base0B)
     (git-timemachine-minibuffer-detail-face :foreground base09)

     (font-lock-variable-use-face            :inherit font-lock-variable-name-face)
     (font-lock-function-call-face           :inherit font-lock-function-name-face)
     (font-lock-property-name-face           :inherit font-lock-variable-name-face)
     (font-lock-property-use-face            :inherit font-lock-property-name-face)
     (font-lock-operator-face                :inherit font-lock-keyword-face)
     (font-lock-number-face                  :inherit font-lock-constant-face)
     (font-lock-escape-face                  :foreground base0C)
     (font-lock-bracket-face                 :foreground base05)
     (font-lock-delimiter-face               :foreground base05)
     (font-lock-misc-punctuation-face        :foreground base05))))

;; show all of the completions from the keys entered so far
(use-package which-key
  :config
  (which-key-mode))

;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-shell (or (executable-find "zsh") shell-file-name)))

;; a better terminal emulator for emacs?
;; https://codeberg.org/akib/emacs-eat
(use-package eat
 :straight (:type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
	       "*.ti" ("terminfo/e" "terminfo/e/*")
	       ("terminfo/65" "terminfo/65/*")
	       ("integration" "integration/*")
	       (:exclude ".dir-locals.el" "*-tests.el"))))



;; https://github.com/dakra/ghostel
(use-package ghostel
  ;; `ghostel--define-terminal-keys' binds C-a..C-z and C-/ to the PTY but
  ;; misses plain C-_ (Emacs's global `undo'), which ghostel buffers don't
  ;; support anyway (buffer-disable-undo) - send it to the shell instead.
  :bind (:map ghostel-semi-char-mode-map
              ("C-_" . ghostel--send-event))
  :config
  ;; ghostel-mode's cursor-type is entirely terminal-driven (DECSCUSR), with
  ;; no frame-focus awareness; ghostel--buffer-focused-p is the same internal
  ;; predicate ghostel itself uses for terminal focus escapes (ghostel--).
  (defun my/ghostel-refresh-cursor-focus (&rest _)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'ghostel-mode)
            (ghostel--apply-cursor-style))))))
  (advice-add 'ghostel--apply-cursor-style :after
              (lambda ()
                (unless (ghostel--buffer-focused-p (current-buffer))
                  (setq cursor-type 'hollow))))
  ;; Same hooks ghostel--focus-change itself uses, so window selection
  ;; changes within a frame (not just frame focus) refresh the cursor too.
  (add-function :after after-focus-change-function #'my/ghostel-refresh-cursor-focus)
  (add-hook 'window-selection-change-functions #'my/ghostel-refresh-cursor-focus)
  (add-hook 'window-buffer-change-functions #'my/ghostel-refresh-cursor-focus))

;; ghostel-compile/-comint are separate features ghostel.el requires
;; internally; straight installs each as its own package sharing the same
;; repo checkout, but without an explicit :files their build dirs miss
;; etc/terminfo (only the main `ghostel' recipe on MELPA has that), which is
;; what triggers the "Bundled terminfo not found" warning.

;; use ghostel for compile buffers
(use-package ghostel-compile
  :straight (:type git :host github :repo "dakra/ghostel"
             :files (:defaults "etc" "src" "vendor" "build.zig" "build.zig.zon" "symbols.map"))
  :hook (after-init . ghostel-compile-global-mode))

;; use ghostel for comint buffers
(use-package ghostel-comint
  :straight (:type git :host github :repo "dakra/ghostel"
             :files (:defaults "etc" "src" "vendor" "build.zig" "build.zig.zon" "symbols.map"))
  :hook (after-init . ghostel-comint-global-mode))

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
	 ;; ("C-c d" . crux-duplicate-current-line-or-region) ;; replaced with builtin duplicate-dwim
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
  ;; window-selection-change-functions fires after the selection has already
  ;; moved, so a current-buffer-only save would save the window you land on,
  ;; not the one you switched away from.
  (setq super-save-all-buffers t)
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
			     project-find-file
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
  ;; from https://karthinks.com/software/avy-can-do-anything/#a-division-of-responsibility
  (defun avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("M-g g" . avy-goto-line)
   ("C-c j" . avy-goto-char-timer)))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (ace-window-display-mode 1)
  (defun +ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
	 (setq
	  window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
	  type 'reuse)
	 (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  :bind
  (("s-w" . ace-window)
   ("C-x 4 o" . +ace-window-prefix)))

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
   (:map embark-become-file+buffer-map ("p" . project-find-file)))


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
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol))
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  :bind
  (("M-i" . consult-imenu)
   ;; ("C-." . consult-imenu-multi)
   ("C-x b" . consult-buffer)
   ("C-c b" . consult-project-buffer)
   ("M-y" . consult-yank-replace)
   ("s-y" . yank-pop)
   ("C-c f" . consult-recent-file)
   ("C-s" . consult-line))
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register))

;; cuz it's awesome. Used by consult, so we don't config here
;; https://github.com/nlamirault/ripgrep.el
(use-package ripgrep)

;; allow us to edit a grep buffer
;; https://github.com/mhayashi1120/Emacs-wgrep
;; How to use: consult-ripgrep -> embark-consult / embark-export -> change grep buffer to wgrep C-c C-p -> edit lines -> C-x C-s apply changes and save all buffers
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; better help
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))

;; completion at point with a popup
;; https://github.com/minad/corfu
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1)
  (corfu-cycle t)
  (corfu-quit-no-match t)
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
  ;; (add-hook 'completion-at-point-functions #'cape-dict)
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
  ;; I need to paste my branch name into slack a lot these days (Vimeo)
  (defun +magit-add-current-branch-to-kill-ring ()
    "Show the current branch in the echo-area and add it to the `kill-ring'."
    (interactive)
    (let ((branch (magit-get-current-branch)))
      (if branch
	  (progn (kill-new branch)
		 (message "%s" branch))
	(user-error "There is not current branch"))))
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
   ;; TODO: add this to the branch menu in magit?
   ("r" . +magit-add-current-branch-to-kill-ring)
   ("b" . magit-blame)))

;; browse old versions of a file
;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine)

;; I like to paste working github links into slack
;; https://github.com/sshaw/git-link
(use-package git-link
  :commands git-link
  :custom
  (git-link-default-branch "main")
  (git-link-consider-ssh-config t))

;; add the git diff highlights to the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; Projects with built-in project.el
(use-package project
  :straight nil
  ;; project-prefix-map is a keymap variable, not a command, so it needs
  ;; :bind-keymap (plain :bind signals "Wrong type argument: commandp")
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
	 ("p" . +project-switch-project)
	 ;; consult ripgrep obeys project setting, and it's nicer than the default project-find-regexp
	 ("g" . consult-ripgrep)
	 ("w f" . +kill-project-file-path)
	 ("w l" . +kill-project-file-line-path))
  :config
  (defun +project-file-path ()
    (file-relative-name buffer-file-name (project-root (project-current t))))

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

  ;; `project-switch-commands' only rebinds `project-current-directory-override',
  ;; which most commands (including `magit-status') don't consult - they read
  ;; `default-directory'. Rebind that directly instead, same as
  ;; `projectile-switch-project-action' used to.
  ;; TODO: make sure this is a git repo before running magit-status and default to something else otherwise
  (defun +project-switch-project (dir)
    "Switch to another project by opening `magit-status' there."
    (interactive (list (funcall project-prompter)))
    (project-remember-project (project-current t dir))
    (let ((default-directory dir))
      (magit-status dir)))

  ;; treat submodules (e.g. this config, a submodule of ~/dotfiles) as their
  ;; own project instead of folding them into the parent repo
  :custom (project-vc-merge-submodules nil))

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

;; ligatures, for fun. Replaced by Plex mono with ligatures from https://github.com/liangjingkanji/PlexMono and `ligature` below
;; https://github.com/jming422/fira-code-mode
;; (use-package fira-code-mode
;;   :config
;;   ;; (fira-code-mode-install-fonts) ;; this prompts every time :(
;;   (global-fira-code-mode))

;; when the font has the ligatures inside it
;; https://github.com/mickeynp/ligature.el
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
			'(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
			  ;; =:= =!=
			  ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
			  ;; ;; ;;;
			  (";" (rx (+ ";")))
			  ;; && &&&
			  ("&" (rx (+ "&")))
			  ;; !! !!! !. !: !!. != !== !~
			  ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
			  ;; ?? ??? ?:  ?=  ?.
			  ("?" (rx (or ":" "=" "\." (+ "?"))))
			  ;; %% %%%
			  ("%" (rx (+ "%")))
			  ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
			  ;; |->>-||-<<-| |- |== ||=||
			  ;; |==>>==<<==<=>==//==/=!==:===>
			  ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
					  "-" "=" ))))
			  ;; \\ \\\ \/
			  ("\\" (rx (or "/" (+ "\\"))))
			  ;; ++ +++ ++++ +>
			  ("+" (rx (or ">" (+ "+"))))
			  ;; :: ::: :::: :> :< := :// ::=
			  (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
			  ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
			  ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
					  "="))))
			  ;; .. ... .... .= .- .? ..= ..<
			  ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
			  ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
			  ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
			  ;; *> */ *)  ** *** ****
			  ("*" (rx (or ">" "/" ")" (+ "*"))))
			  ;; www wwww
			  ("w" (rx (+ "w")))
			  ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
			  ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
			  ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
			  ;; << <<< <<<<
			  ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
					  "-"  "/" "|" "="))))
			  ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
			  ;; >> >>> >>>>
			  (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
			  ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
			  ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
				       (+ "#"))))
			  ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
			  ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
			  ;; __ ___ ____ _|_ __|____|_
			  ("_" (rx (+ (or "_" "|"))))
			  ;; Fira code: 0xFF 0x12
			  ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
			  ;; Fira code:
			  "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
			  ;; The few not covered by the regexps.
			  "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
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

  (setq lsp-enabled-clients '(ts-ls eslint tfmls ruby-lsp-ls ty-ls ruff))
  :config
  ;; these are emacs settings for lsp performance
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000) ;; 100mib

  ;; this seems cool but isn't noticeably better than treesitter highlighting
  ;; (setq lsp-semantic-tokens-enable t)
  ;; (setq lsp-semantic-tokens-honor-refresh-requests t)

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("bundle" "exec" "rubocop" "--lsp"))
  ;;                   :activation-fn (lsp-activate-on "ruby")
  ;;                   :add-on? t
  ;;                   :server-id 'my-rubocop-ls))
  ;; (add-to-list 'lsp-language-id-configuration '(yaml-ts-mode . "yaml"))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp\\'")
  :custom
  (lsp-completion-provider :none) ;; corfu
  (lsp-signature-auto-activate nil) ;; this momentarily steals focus and triggers auto-save that runs rubocop because of rubocopfmt-mode
  ;; lsp-ruff.el's default [] for lint-select serializes as JSON [] (select zero rules),
  ;; not "no override" — ruff then ignores pyproject.toml and reports nothing. nil -> JSON null fixes it.
  (lsp-ruff-lint-select nil)
  :hook (((js-base-mode typescript-ts-base-mode terraform-mode ruby-base-mode python-base-mode) . lsp-deferred)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-completion-mode . my/lsp-mode-setup-completion)))

(use-package lsp-treemacs)
(use-package lsp-ui)

;; lsp-mode's headerline breadcrumb icons are hardwired to all-the-icons
;; (lsp-icons.el), not nerd-icons -- without this it silently falls back
;; to plain text breadcrumbs.
(use-package all-the-icons)

;; Claude code IDE in emacs
;; https://github.com/manzaltu/claude-code-ide.el
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'ghostel)
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-mcp-server-tools
        (seq-filter (lambda (tool)
                      (member (plist-get tool :name)
                              '("claude-code-ide-mcp-xref-find-references"
                                "claude-code-ide-mcp-project-info"
                                "claude-code-ide-mcp-imenu-list-symbols")))
                      claude-code-ide-mcp-server-tools)))

;; flycheck mode to highlight warnings and errors in code
;; https://www.flycheck.org/en/latest
(use-package flycheck
  :init (global-flycheck-mode))


;; on MacOS Sequoia++, treesit-language-available-p is super slow see https://github.com/renzmann/treesit-auto/issues/135
;; This is the solution linked there, from https://github.com/jeremyf/dotemacs/blob/75410e2f56273b2be4abf10d0d72627ec4ad6a85/emacs.d/init.el#L5176-L5197
(use-package treesit
  :straight (:type built-in)
  :init
  (setopt treesit-font-lock-level 4)
  :config
  (defvar jf/treesit-lang-cache
    (make-hash-table :test 'equal)
    "Cache the expensive computation of treelit language availability.

See `jf/treesit-language-available-p' for usage.")

  (defun jf/treesit-language-available-p (fn lang &rest rest)
    "Caching around the CPU expensive `treesit-language-available-p'."
    ;; I did some profiling of `treesit-language-available-p', and found
    ;; that when moving around via consult (and therefore preview) this
    ;; function was contributing to 75% of the CPU time.  And it was run
    ;; each time.
    (let ((cached-value
	    (gethash lang jf/treesit-lang-cache 'miss)))
      (if (eq 'miss cached-value)
	(let ((value
		(apply fn lang rest)))
	  (puthash lang value jf/treesit-lang-cache)
	  value)
	cached-value)))
  (advice-add #'treesit-language-available-p
    :around #'jf/treesit-language-available-p)
  (setq treesit-language-source-alist
    '((awk        "https://github.com/Beaglefoot/tree-sitter-awk")
      (bash       "https://github.com/tree-sitter/tree-sitter-bash")
      (bibtex     "https://github.com/latex-lsp/tree-sitter-bibtex")
      (blueprint  "https://github.com/huanie/tree-sitter-blueprint")
      (c          "https://github.com/tree-sitter/tree-sitter-c")
      (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
      (clojure    "https://github.com/sogaiu/tree-sitter-clojure")
      (cmake      "https://github.com/uyha/tree-sitter-cmake")
      (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
      (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
      (css        "https://github.com/tree-sitter/tree-sitter-css")
      (dart       "https://github.com/ast-grep/tree-sitter-dart")
      (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
      (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
      (glsl       "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
      (go         "https://github.com/tree-sitter/tree-sitter-go")
      (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
      (heex       "https://github.com/phoenixframework/tree-sitter-heex")
      (html       "https://github.com/tree-sitter/tree-sitter-html")
      (janet      "https://github.com/sogaiu/tree-sitter-janet-simple")
      (java       "https://github.com/tree-sitter/tree-sitter-java")
      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master")
      (json       "https://github.com/tree-sitter/tree-sitter-json")
      (julia      "https://github.com/tree-sitter/tree-sitter-julia")
      (kotlin     "https://github.com/fwcd/tree-sitter-kotlin")
      (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
      (magik      "https://github.com/krn-robin/tree-sitter-magik")
      (make       "https://github.com/tree-sitter-grammars/tree-sitter-make")
      (nickel     "https://github.com/nickel-lang/tree-sitter-nickel")
      (nix        "https://github.com/nix-community/tree-sitter-nix")
      (nu         "https://github.com/nushell/tree-sitter-nu")
      (org        "https://github.com/milisims/tree-sitter-org")
      (perl       "https://github.com/ganezdragon/tree-sitter-perl")
      (proto      "https://github.com/mitchellh/tree-sitter-proto")
      (python     "https://github.com/tree-sitter/tree-sitter-python")
      (r          "https://github.com/r-lib/tree-sitter-r")
      (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
      (rust       "https://github.com/tree-sitter/tree-sitter-rust")
      (scala      "https://github.com/tree-sitter/tree-sitter-scala")
      (sql        "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
      (surface    "https://github.com/connorlay/tree-sitter-surface")
      (toml       "https://github.com/tree-sitter/tree-sitter-toml")
      (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
      (typst      "https://github.com/uben0/tree-sitter-typst" "master")
      (verilog    "https://github.com/gmlarumbe/tree-sitter-verilog")
      (vhdl       "https://github.com/alemuller/tree-sitter-vhdl")
      (vue        "https://github.com/tree-sitter-grammars/tree-sitter-vue")
      (wast       "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src")
      (wat        "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src")
      (wgsl       "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
      (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))

;; get the treesit goodness without specifying grammar download locations or major mode translations
;; REMEMBER that hooks don't transfer to the ts mode
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; tree-sitter navigation and semantic editing
;; https://github.com/mickeynp/combobulate
;; (use-package combobulate
;;   :preface
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c o")

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook ((python-ts-mode . combobulate-mode)
;;          (js-ts-mode . combobulate-mode)
;;          (css-ts-mode . combobulate-mode)
;;          (yaml-ts-mode . combobulate-mode)
;;          (json-ts-mode . combobulate-mode)
;;          (typescript-ts-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode)))

;; subword mode is required! (built in)
(use-package subword
  :straight nil
  :config (global-subword-mode 1))

;;;;;;;;; Languages

(use-package org
  :straight nil
  :custom
  (org-agenda-files '("~/src/focused/drafthouse/notes/scott.org"
		       "/Users/andrewherr/src/focused/drafthouse/notes/inbox.org")))

;; markdown mode
(use-package markdown-mode
  ;; :ensure-system-package pandoc
  :commands gfm-mode
  :mode (("\\.md\\'" . gfm-mode))
  :custom-face
  (markdown-pre-face ((t nil)))
  :config
  (setq markdown-command "pandoc --standalone --mathjax --from=gfm"
	markdown-disable-tooltip-prompt t
	markdown-fontify-code-blocks-natively t))

;; switch to yaml-mode package, because built-in yaml-ts-mode sucks
;; why doesn't this work :'(
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" ))

(use-package dockerfile-ts-mode
  :straight nil
  :mode ("Dockerfile\\'" . dockerfile-ts-mode))

(use-package csv-mode
  :straight (:host github :repo "emacs-straight/csv-mode")
  :mode ("\\.csv$")
  :config
  (defun +csv-highlight (&optional separator)
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
	 (n (count-matches (string separator) (pos-bol) (pos-eol)))
	 (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
			  collect (apply #'color-rgb-to-hex
					 (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2
	     for c in colors
	     for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
	     do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  :hook
  ((csv-mode . +csv-highlight)
   (csv-mode . csv-align-mode)
   (csv-mode . (lambda () (toggle-truncate-lines 1)))))

;; nix-mode https://github.com/NixOS/nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")

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
  (defvar +rspec-outline-blocks
    '("context"
      "describe"
      "include_examples"
      "it"
      "it_behaves_like"
      "it_should_behave_like"
      "shared_examples_for"
      "specify"))

  (defun +rspec-outline ()
    "Use `occur' to create a linked outline of the spec associated with the current file, which may be either a spec or a target."
    (interactive)
    (let ((list-matching-lines-face nil)
	  (spec-buffer (if (rspec-buffer-is-spec-p)
			   (current-buffer)
			 (find-file-noselect (rspec-spec-file-for (buffer-file-name))))))
      (with-current-buffer spec-buffer
	(occur (rx-to-string `(seq line-start
				   (zero-or-more whitespace)
				   (optional "RSpec.")
				   (or ,@+rspec-outline-blocks)
				   (one-or-more whitespace)
				   (or "\"" "'" "A-Z" "{ ")))
	       0)))
    (occur-rename-buffer))

  ;; This is for packwerk vvv
  ;; (defun +rspec-package-root-directory-p (directory)
  ;;   (file-regular-p (expand-file-name "package.yml" directory)))

  ;; (defun +rspec-package-root (&optional directory)
  ;;   "Find the root directory of the package.
  ;;    Walk the directory tree until it finds a package.yml file."
  ;;   (let ((directory (file-name-as-directory (or directory default-directory))))
  ;;     (cond ((rspec-root-directory-p directory)
  ;;            (error "Could not determine the project root."))
  ;;           ((+rspec-package-root-directory-p directory) (expand-file-name directory))
  ;;           (t (+rspec-package-root (file-name-directory (directory-file-name directory)))))))

  ;; (defun rspec-target-in-holder-dir-p (a-file-name)
  ;;   (string-match (concat "^" (concat
  ;;                              (regexp-quote
  ;;                               (+rspec-package-root a-file-name))
  ;;                              (regexp-opt rspec-primary-source-dirs)
  ;;                              "/"))
  ;;                 a-file-name))
  ;; ^^^ packwerk
  :bind (:map rspec-verifiable-mode-keymap
	      ("s" . rspec-verify-single)
	      ("o" . +rspec-outline))
  ;; :custom
  ;; this is for Gusto/zenpayroll where the binstub takes care of bundler and spring
  ;; (rspec-use-spring-when-possible nil)
  ;; (rspec-use-bundler-when-possible t)
  ;; (rspec-spec-command "bin/rspec --no-profile")
  ;; (rspec-command-options "--color")
  )

(use-package minitest
  :after ruby-mode
  :custom
  (compilation-scroll-output nil))

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
       (not (file-directory-p (expand-file-name "spec" (project-root (project-current)))))))

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

;; I'd like to run rubocop manually until I can figure out how to get the lsp to do it
;; (figured it out, but it requires the lsp to be in the bundle, which will be hard to manage. lsp-format-buffer)
;; (use-package rubocop)

;; autoformat with rubocop via the lsp. We'll see
;; (use-package rubocopfmt
;;   :hook
;;   (ruby-base-mode . rubocopfmt-mode)
;;   :custom
;;   (rubocopfmt-on-save-use-lsp-format-buffer t))

;; slim templates? ugh
;; https://github.com/slim-template/emacs-slim
;; (use-package slim-mode
;;	     :straight (:host github :repo "slim-template/emacs-slim")
;;	     :mode ("\\.slim\\'" . slim-mode))

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

(use-package graphql-mode
  :mode
  ("\\.graphql\\'" . graphql-mode))

(use-package prettier
  :config
  (add-to-list 'prettier-major-mode-parsers '(typescript-ts-base-mode . (typescript babel-ts)))
  ;; prettier.el doesn't shell out to a `prettier` binary — it runs Emacs'
  ;; node and requires the `prettier` npm module directly, so Homebrew's
  ;; `prettier` formula doesn't satisfy it. The module is installed via
  ;; mise's npm backend (`prettier` in ~/.config/mise/config.toml's
  ;; [tools], not node's own global node_modules), so resolve its
  ;; NODE_PATH explicitly here.
  (let* ((mise-prettier (string-trim
                         (shell-command-to-string "mise where prettier 2>/dev/null")))
         (prettier-modules (expand-file-name "node_modules" mise-prettier)))
    (when (file-directory-p prettier-modules)
      (setenv "NODE_PATH" prettier-modules)))
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

;;;;;;;;;;;;;;;;;;;;;;
;; Other misc modes ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package terraform-mode
  :custom
  (terraform-format-on-save t))

;; https://gitlab.com/bricka/emacs-kotlin-ts-mode
(use-package kotlin-ts-mode
  :straight (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kt\\'")

