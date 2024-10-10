;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
;; no titlebar please
(add-to-list 'default-frame-alist '(undecorated-round . t))
;; lsp-mode performance
(setenv "LSP_USE_PLISTS" "true")
