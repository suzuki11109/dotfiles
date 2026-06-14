;;; early-init.el --- early init -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 4 1024 1024))

(setq site-run-file nil)

;; `file-name-handler-alist' is consulted often. Unsetting it offers a notable saving in startup time.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun my/reset-file-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))))
  (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist 101))

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Font compacting can be very resource-intensive.
(setq inhibit-compacting-font-caches t)

;; Emacs will try to resize itself to a specific column size without this.
(setq frame-inhibit-implied-resize t)

;; Reduce *Message* noise at startup. An empty scratch buffer for faster display.
(setq inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t)
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add #'display-startup-screen :override #'ignore)
(fset #'display-startup-echo-area-message #'ignore)

;; Disable bidirectional text scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-async-jobs-number 2)

;; Unset command line options irrelevant to the current OS
(setq command-line-x-option-alist nil)

;; Disable warnings
(setq warning-suppress-types '((defvaralias) (lexical-binding)))
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; Disable warnings from the legacy advice API. They aren't actionable or useful.
(setq ad-redefinition-action 'accept)

;; Minimal UI
(setq mode-line-format '("%e"))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Maximize windows from start
(push '(fullscreen . maximized) default-frame-alist)

(push '(background-color . "#1e1e2e") default-frame-alist)
(push '(foreground-color . "#cdd6f4") default-frame-alist)

;; Transparent Titlebar
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
