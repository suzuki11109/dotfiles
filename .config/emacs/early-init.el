;;; early-init.el --- early init -*- lexical-binding: t; -*-

(setq
 ;; Do not make installed packages available when Emacs starts
 package-enable-at-startup nil
 ;; Increase the garbage collection (GC) threshold for faster startup.
 gc-cons-threshold most-positive-fixnum
 ;; Do not wast time checking the modification time of each file
 load-prefer-newer noninteractive
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

;; In Emacs29+, frames can have a transparent background via the
;; `alpha-background' parameter
(when (>= emacs-major-version 29)
    (push (cons 'alpha-background 93) default-frame-alist))

;; set "$LSP_USE_PLISTS=true" to improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")

;; set default font
(set-face-attribute 'default nil :family "monospace" :height 110)

