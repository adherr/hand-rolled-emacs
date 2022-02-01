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

;; base Emacs config
(use-package emacs
  :config
  (global-display-line-numbers-mode 1)
  ;; load my path
  (use-package exec-path-from-shell
    :demand
    :config (exec-path-from-shell-initialize))
  :custom
  (confirm-kill-emacs 'y-or-n-p)
  (desktop-base-file-name "desktop")
  (desktop-base-lock-name "desktop.lock")
  (desktop-path (add-to-list desktop-path prelude-savefile-dir))
  (desktop-dirname prelude-savefile-dir)
  (desktop-save-mode t))


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
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("s-o" . crux-smart-open-line-above)))

;; Move a line or region up and down
;; https://github.com/emacsfodder/move-text
(use-package move-text
  :bind (([C-S-up] . move-text-up)
         ([C-S-down] . move-text-down)))

;; Jump to a definition in any open buffer
;; https://github.com/vspinu/imenu-anywhere
(use-package imenu-anywhere
  :bind ("C-." . imenu-anywhere))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-from-inside nil)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)

  (show-smartparens-global-mode +1))
