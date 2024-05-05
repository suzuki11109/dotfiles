;;; early-init.el --- early init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Increase the garbage collection (GC) threshold for faster startup.
(setq gc-cons-threshold most-positive-fixnum)

;; `file-name-handler-alist' is consulted often. Unsetting it offers a notable saving in startup time.
(defvar +file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq file-name-handler-alist +file-name-handler-alist)))

;; Reduce *Message* noise at startup.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; Set initial buffer to fundamental-mode for faster load
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)

;; Disable package.el support
(setq package-enable-at-startup nil)

;; Remove some unneeded UI elements
(setq use-dialog-box nil)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Resizing the Emacs frame  appears to impact startup time dramatically.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Maximize frame by default
(push '(fullscreen . maximized) default-frame-alist)


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.03f seconds"
                             (float-time (time-subtract (current-time) before-init-time)))
                     gcs-done)))

;;; early-init.el ends here

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Slightly faster re-display
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0
      jit-lock-defer-time 0)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; This timeout adds latency to frame operations
(setq pgtk-wait-for-event-timeout 0.001)

;; Accelerate scrolling with the trade-off of sometimes delayed accurate fontification
(setq fast-but-imprecise-scrolling t)

;; Increase single chunk bytes to read from subprocess (default 4096)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Inhibits fontification while receiving input
(setq redisplay-skip-fontification-on-input t)

;; ;; Improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")
