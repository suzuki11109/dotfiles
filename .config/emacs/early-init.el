;;; early-init.el --- early init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Minimal UI
(setq
 inhibit-startup-screen t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil)

;; Inhibit startup message in echo area
(fset 'display-startup-echo-area-message #'ignore)

;; If you want transaparent background
;; (push (cons 'alpha-background 93) default-frame-alist)

;; Improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")
;;; early-init.el ends here
