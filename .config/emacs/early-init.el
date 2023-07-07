;;; early-init.el --- early init -*- lexical-binding: t; -*-

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Minimal UI
(setq
 inhibit-startup-screen t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil)

;; Inhibit startup message in echo area
(fset 'display-startup-echo-area-message #'ignore)

;; Match theme color early on (smoother transition).
(add-to-list 'default-frame-alist '(background-color . "#000000"))

;; If you want transaparent background
(push (cons 'alpha-background 93) default-frame-alist)

;; set "$LSP_USE_PLISTS=true" to improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")
