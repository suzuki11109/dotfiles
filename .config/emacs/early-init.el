;;; early-init.el --- early init -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.2)
(setq read-process-output-max (* 1024 1024))

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

(setq frame-inhibit-implied-resize t)

(setq mode-line-format nil)

(setq inhibit-startup-screen t
      inhibit-startup-buffer-menu t)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable bidirectional text scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(push '(fullscreen . maximized) default-frame-alist)

(setq native-comp-async-report-warnings-errors 'silent)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs loaded in %s with %d garbage collections."
;;                      (format "%.03fs" (float-time (time-subtract elpaca-after-init-time before-init-time))) gcs-done)))

(setenv "LSP_USE_PLISTS" "true")

(setq package-enable-at-startup nil)

;;; early-init.el ends here
