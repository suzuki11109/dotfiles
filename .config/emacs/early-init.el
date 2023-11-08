;;; early-init.el --- early init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Increase the garbage collection (GC) threshold for faster startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive.
   native-comp-async-report-warnings-errors nil
   ;; Make native compilation happens asynchronously
   native-comp-jit-compilation t))

;; Do not wast time checking the modification time of each file
(setq load-prefer-newer noninteractive)

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Slightly faster re-display
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; This timeout adds latency to frame operations
(setq pgtk-wait-for-event-timeout 0.001)

;; Increase single chunk bytes to read from subprocess (default 4096)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Inhibits fontification while receiving input
(setq redisplay-skip-fontification-on-input t)

;; Improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")

;; Remove some unneeded UI elements
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Disable start-up screen
(setq-default inhibit-startup-screen t)
(setq-default initial-scratch-message "")

;; Set initial buffer to fundamental-mode for faster load
(setq initial-major-mode 'fundamental-mode)

;; Inhibit startup message in echo area
(fset 'display-startup-echo-area-message #'ignore)

;; Maximize frame by default
(push '(fullscreen . maximized) default-frame-alist)
;;; early-init.el ends here
