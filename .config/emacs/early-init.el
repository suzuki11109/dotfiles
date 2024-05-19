;;; early-init.el --- early init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Increase the garbage collection (GC) threshold for faster startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el support
(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

;; `file-name-handler-alist' is consulted often. Unsetting it offers a notable saving in startup time.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun my/reset-file-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))))
  (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist 101))

;; Remove some unneeded UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq use-dialog-box nil)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Resizing the Emacs frame  appears to impact startup time dramatically.
(setq frame-inhibit-implied-resize t)

;; Reduce *Message* noise at startup.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; Set initial buffer to fundamental-mode for faster load
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Maximize frame by default
(push '(fullscreen . maximized) default-frame-alist)

;; Make native compilation silent
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)) ; Emacs 28 with native compilation

;; Increase single chunk bytes to read from subprocess (default 4096)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; ;; Improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")
;;; early-init.el ends here

;; Slightly faster re-display
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Accelerate scrolling with the trade-off of sometimes delayed accurate fontification
(setq fast-but-imprecise-scrolling t)

;; Inhibits fontification while receiving input
(setq redisplay-skip-fontification-on-input t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0
      jit-lock-defer-time 0)

;; Don’t compact font caches during GC (not sure if it has effect on unix)
;; (setq inhibit-compacting-font-caches t)

;; This timeout adds latency to frame operations
(setq pgtk-wait-for-event-timeout 0.001)
