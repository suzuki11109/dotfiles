;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Aki Suzuki"
      user-mail-address "suzuki11109@gmail.com")

(setq doom-font (font-spec :family "monospace" :size 13 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "monospace" :size 13 :weight 'regular)
      doom-big-font (font-spec :family "monospace" :size 15 :weight 'regular))
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
(add-hook! 'doom-load-theme-hook :append
  (set-fontset-font t 'thai (font-spec :family "SFThonburi")))

;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq-default line-spacing 2)
(setq-default x-stretch-cursor t)
(setq scroll-margin 2)
(setq truncate-string-ellipsis "…")

(setq evil-want-fine-undo t)
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(setq evil-escape-key-sequence "kj")
(setq evil-escape-delay 0.30)
(setq +evil-want-o/O-to-continue-comments nil)
(add-hook 'after-change-major-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq doom-unreal-buffer-functions '(minibufferp))
(setq doom-modeline-buffer-file-name-style 'file-name)
(setq doom-modeline-vcs-max-length 18)
(setq doom-modeline-percent-position nil)
(setq doom-modeline-env-version nil)
(setq all-the-icons-scale-factor 1)

;; By default changes made via a customisation interface are added to init.el.
;; I prefer the idea of using a separate file for this. We just need to change a setting, aaaaa and load it if it exists.
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq frame-title-format
      (list '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
            '(:eval
              (let ((project-name (projectile-project-name)))
                (unless (string= "-" project-name)
                  (format " — %s" project-name))))))

;;I also think that having evil- appear in so many popups is a bit too verbose,
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(setq company-require-match nil)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-maximum-width 120)
(setq company-idle-delay 0)

(after! company
  (define-key company-active-map [tab] 'company-complete-selection))

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
;; (setq lsp-clients-angular-language-server-command
;;   '("ngserver"
;;     "--ngProbeLocations"
;;     "/Users/aki/.config/yarn/global/node_modules"
;;     "--tsProbeLocations"
;;     "/Users/aki/.config/yarn/global/node_modules"
;;     "--stdio"))
(setq lsp-go-use-gofumpt t)
;;(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(add-hook! 'go-mode-hook
  (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))

(setq orderless-matching-styles '(orderless-literal orderless-flex))

(use-package! tree-sitter
  :hook (prog-mode . turn-on-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(defun remove-dos-eol ()
 "Do not show ^M in files containing mixed UNIX and DOS line endings."
 (interactive)
 (setq buffer-display-table (make-display-table))
 (aset buffer-display-table ?\^M []))

(map! :nv ";" #'evil-ex)
(map! :nv "s" #'evil-avy-goto-char-2)
(map! :leader
      "SPC" #'execute-extended-command
      (:prefix "w"
       "C-o" #'enlarge-window
       "o" #'delete-other-windows))
(map! (:after flycheck
       :m "[e" #'flycheck-previous-error
       :m "]e" #'flycheck-next-error))

(setq vterm-timer-delay 0.01)
(setq vterm-always-compile-module t)
(add-hook 'vterm-mode-hook
         (lambda ()
           (setq-local evil-insert-state-cursor '(box "#00FF00"))
           (evil-insert-state)))
(add-hook 'vterm-mode-hook #'doom-mark-buffer-as-real-h)
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

