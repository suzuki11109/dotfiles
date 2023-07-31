;;; early-init.el --- early init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Do not make installed packages available when Emacs starts
(setq package-enable-at-startup nil)

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Increase the garbage collection (GC) threshold for faster startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; Do not wast time checking the modification time of each file
(setq load-prefer-newer t)

;; Remove some unneeded UI elements
(setq
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (fullscreen . maximized)))
(setq
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil)

;; Disable start-up screen
(setq-default inhibit-startup-screen t)
(setq-default initial-scratch-message "")

;; If you want transaparent background
;; (push (cons 'alpha-background 93) default-frame-alist)

;; Inhibit startup message in echo area
(fset 'display-startup-echo-area-message #'ignore)

;; Increase single chunk bytes to read from subprocess (default 4096)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")
;;; early-init.el ends here
