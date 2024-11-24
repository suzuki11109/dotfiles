;;; early-init.el --- early init -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;;; Code:

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))

;; Increase the garbage collection (GC) threshold for faster startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Don't use precious startup time checking mtime on elisp bytecode.
(setq load-prefer-newer noninteractive)

;; `file-name-handler-alist' is consulted often. Unsetting it offers a notable saving in startup time.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun my/reset-file-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))))
  (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist 101))

;; Resizing the Emacs frame appears to impact startup time dramatically.
(setq frame-inhibit-implied-resize t)

;; Reduce *Message* noise at startup.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;; disable the mode-line until we can see one.
(put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
(setq-default mode-line-format nil)
(dolist (buf (buffer-list))
  (with-current-buffer buf (setq mode-line-format nil)))

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Accelerate scrolling with the trade-off of sometimes delayed accurate fontification
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)

;; Don’t compact font caches during GC (not sure if it has effect on unix)
(setq inhibit-compacting-font-caches t)

;; Increase single chunk bytes to read from subprocess (default 4096)
(setq read-process-output-max (* 1024 1024))

;; Inhibits fontification while receiving input
(setq redisplay-skip-fontification-on-input t)

;; Bump the chunk value so it can fontify more src block
(setq jit-lock-chunk-size 15000)

;; Remove some unneeded UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(when IS-MAC
  ;; NOTE: The correct way to disable this hack is to toggle `menu-bar-mode' (or
  ;;   put it on a hook). Don't try to undo the hack below, as it may change
  ;;   without warning, but will always respect `menu-bar-mode'.
  (setcdr (assq 'menu-bar-lines default-frame-alist) 'tty)
  (add-hook 'after-make-frame-functions
            (defun init-menu-bar-on-macos-h (&optional frame)
              (if (eq (frame-parameter frame 'menu-bar-lines) 'tty)
                  (set-frame-parameter frame 'menu-bar-lines
                                       (if (display-graphic-p frame) 1 0))))))

;; Unset a non-trivial list of command line options that aren't relevant to this session
(unless (memq initial-window-system '(x pgtk))
  (setq command-line-x-option-alist nil))

(set-language-environment "UTF-8")
(setq default-input-method nil)

;; UX: Suppress compiler warnings and don't inundate users with their popups.
;;   They are rarely more than warnings, so are safe to ignore.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-warning-on-missing-source nil)

;; Disable warnings from the legacy advice API. They aren't actionable or useful.
(setq ad-redefinition-action 'accept)

;; Maximize frame by default
(push '(fullscreen . maximized) default-frame-alist)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.03f seconds"
                             (float-time (time-subtract (current-time) before-init-time)))
                     gcs-done)))

;; ;; Improve `lsp-mode' performances
(setenv "LSP_USE_PLISTS" "true")

(setq package-enable-at-startup nil)

;;; early-init.el ends here
