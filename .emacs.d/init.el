;;; init.el --- Initialization -*- lexical-binding: t; -*-

;; build emacs

;; popper & terminal height
;; split early-init
;; formatter
;; yaml/toml/json

;; git gutter new
;; flymake bitmap
;; flymake golint

;; emacs-lisp lookup key

;; eat
;; eshell
;; runserver
;; detache?

;; python & pyenv
;; rust
;; scala
;; ruby

;; dockerfile
;; docker
;; terraform
;; angular
;; lua

;; dashboard
;; bookmarks

;; later
;; lispy
;; clojure
;; emoji/unicode
;; github forge
;; k8s
;; org
;; rest
;; ocaml
;; elixir
;; haskell
;; vue
;; slides
;; ftp/tramp



;; maybe in early-init
;; Increase the GC threshold for faster startup
(setq gc-cons-threshold most-positive-fixnum)

;; revert back after init
(add-hook 'after-init-hook
	    (lambda nil 
                 (setq gc-cons-threshold (* 16 1024 1024))))

;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer t)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Make native compilation happens asynchronously
(setq native-comp-deferred-compilation t)

;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Ignore X resources;
(advice-add #'x-apply-session-resources :override #'ignore)

;; fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; custom file

(setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
;; lisp folder

;; Package system
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; use-package.el is no longer needed at runtime
;; This means you should put the following at
;; the top of your Emacs, to further reduce load time:
(eval-when-compile
  (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;; Performance
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

;; slightly faster re-display
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; support files with long lines
(global-so-long-mode 1)

;; Update UI less often
(setq idle-update-delay 1.0
      jit-lock-defer-time 0)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

(setq fast-but-imprecise-scrolling t)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq inhibit-compacting-font-caches t)

(setq redisplay-skip-fontification-on-input t)


;; adjust GC threshold and run GC during idle
(use-package gcmh
  :hook
  (after-init-hook . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto)
	(setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold (* 16 1024 1024)) 
	)

;; GC when not focus emacs
(add-function :after after-focus-change-function
  (defun me/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

;; libraries
(use-package all-the-icons)

;; set font
(set-face-attribute 'default nil :font "monospace" :height 100)
(set-face-attribute 'variable-pitch nil :inherit 'default :family "Noto Sans" :height 1.0)
(set-face-attribute 'fixed-pitch nil :inherit 'default :height 1.0)
(setq-default line-spacing 1)


;;; Default
(setq create-lockfiles nil)
(setq make-backup-files nil)
;; (setq vc-make-backup-files nil)
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't allow cursor in minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Give some room to minibuffer
(setq max-mini-window-height 0.3)
(setq resize-mini-windows 'grow-only)

;;; shell
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  )


;;; default tab-width

;;;UX

;; remove visual indicators from non selected windows
(setq highlight-nonselected-windows nil)
(setq-default cursor-in-non-selected-windows nil)

;;;  better scrolling
  (setq scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
	auto-window-vscroll nil)

  ;; Hide commands in M-x which don't work in the current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq use-dialog-box nil)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq ring-bell-function #'ignore)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Use trash-cli rather than rm when deleting files.
(customize-set-variable 'delete-by-moving-to-trash t) 

 ;; Escape quits everything
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Simpler tabbar
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-tab-to 'rightmost)
(setq tab-bar-new-button nil)

;;; History
(use-package savehist
  :init
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  (setq savehist-save-minibuffer-history t)
  (setq history-delete-duplicates t)
  (setq history-length t)
  :config
  (savehist-mode 1))


(use-package recentf
  :straight (:type built-in)
  :init
  (setq recentf-exclude
        (list "/tmp/"                        ; Temp-files
              "/ssh:"                        ; Files over SSH
              "/TAGS$"                       ; Tag files
              "^/\\.git/.+$"                 ; Git contents
              "\\.?ido\\.last$"
              "\\.revive$"
              "^/var/folders/.+$"))
  (setq recentf-filename-handlers '(abbreviate-file-name))
  (setq recentf-max-menu-items 0)
  (setq recentf-max-saved-items 300)
  (setq recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

;;; UI
;; Underline line at descent position, not baseline position.
(setq x-underline-at-descent-line t)

;; No blinking cursor
(blink-cursor-mode -1)

;; electric
(electric-pair-mode 1)

;; line num
  ;; (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-width 3)
  ;;(setq-default display-line-numbers-widen t)
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))
  (dolist (mode '(org-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Theme
(use-package catppuccin-theme
  :config
  (setq catppuccin-height-title1 1.5)
  (load-theme 'catppuccin t))

;; modeline
(column-number-mode)
(setq mode-line-percent-position nil)
(use-package doom-modeline
  :init
  (setq all-the-icons-scale-factor 1.0
	doom-modeline-bar-width 2
	doom-modeline-buffer-file-name-style 'buffer-name
	doom-modeline-major-mode-icon nil
	doom-modeline-vcs-max-length 20
	doom-modeline-env-version nil)
  :hook (after-init . doom-modeline-mode))

;; Editing
(use-package general
  :config
;;  (general-evil-setup)
;;  
  (general-create-definer my/leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer my/local-leader-def
    :states '(normal visual)
    :keymaps 'local
    :prefix "SPC m")

  (my/leader-def
    "SPC" '(execute-extended-command :wk "execute command")) ;; an alternative to 'M-x'
;;    
;;  (my/leader-def
;;    "c" '(:ignore t :wk "code"))
;;
;;  (my/leader-def
;;    "ht" '(load-theme :wk "load theme")) 

  (my/leader-def
    "m" '(:ignore t :wk "local"))

  (my/leader-def
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") 
    "bd" '(kill-this-buffer :wk "kill this buffer")
    "bD" '(kill-buffer :wk "kill a buffer")
    "bi" '(ibuffer :wk "ibuffer")
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "bo" '(switch-to-buffer-other-window :k "switch buffer other window")
    "br" '(revert-buffer :wk "reload buffer")
    "bR" '(rename-buffer :wk "rename buffer")
    "bx" '(scratch-buffer :wk "switch to scratch buffer")
    "bz" '(bury-buffer :wk "bury buffer"))
;;    
  (my/leader-def
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file")
    "fd" '(dired :wk "find directory")
;;    ;; "fe" '(find-file :wk "find file .emacs.d")
    "fr" '(recentf :wk "recent files") ;; gets overridden by consult
;;    ;; "fy" '(write-file :wk "yank file path")
    "fS" '(write-file :wk "save as")
    "fs" '(save-buffer :wk "save file"))
;;
;;  (my/leader-def
;;    "o" '(:ignore t :wk "open"))
;;
  (my/leader-def
    "q" '(:ignore t :wk "quit/session")
    "qk" '(kill-emacs :wk "kill emacs"))
;;
;;  (my/leader-def
;;    "s" '(:ignore t :wk "search")
;;    "sF" '(locate :wk "locate"))
  )

;; Undo
(use-package undo-fu
  :init
  (setq undo-limit 400000
        undo-strong-limit 3000000
        undo-outer-limit 48000000))

(use-package undo-fu-session
  :init
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode))

;; Evil
(use-package evil
  :general
  (my/leader-def
    "w" '(:keymap evil-window-map :wk "window")) 
  :init
  (setq evil-want-integration t 
        evil-visual-state-cursor 'hollow
        evil-want-keybinding nil
        evil-want-C-u-scroll t 
        evil-want-C-d-scroll t 
	evil-want-C-h-delete t
        evil-want-C-i-jump nil 
        evil-respect-visual-line-mode t
        evil-ex-search-vim-style-regexp t
        evil-split-window-below t
        evil-vsplit-window-right t
	evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
	evil-undo-system 'undo-fu) 
  :config
  (evil-mode t) 
  
  (evil-select-search-module 'evil-search-module 'evil-search)

  (define-key evil-motion-state-map ";" #'evil-ex)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (dolist (mode '(custom-mode
		  eshell-mode
		  magit-diff-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))


(use-package evil-escape
  :after evil
  :init
  (setq evil-escape-key-sequence "kj")
  (setq evil-escape-delay 0.25)
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(treemacs-mode))
  :config
  (evil-escape-mode))

(use-package evil-collection 
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :general
  (:states '(normal visual)
    "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)) 

;; UI
;; which-key
(use-package which-key
  :defer 1
  :commands
  (which-key-mode
   which-key-setup-side-window-bottom
   which-key-key-order-alpha)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-add-column-padding 1)
  (setq which-key-min-display-lines 5)
  (setq which-key-idle-delay 1)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;;; Complete

(setq tab-always-indent 'complete)

(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-prefixes orderless-flex orderless-regexp))
  (setq completion-styles '(basic orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
	'((file (styles . (basic partial-completion orderless)))
	  (eglot (styles . (basic substring orderless)))))
  )


;; autocomplete
(use-package corfu
  :defer 1
  :init
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-preselect 'first)
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.0)
  (setq corfu-echo-documentation 0.5)
  (setq corfu-on-exact-match nil)
  :config
  (global-corfu-mode 1)
  (advice-add 'evil-escape-func :after 'corfu-quit))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) 
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :custom
  (yas-triggers-in-field t)
  :config
  (yas-reload-all)
  )

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(use-package company)
(use-package cape)

;; minibuffer
(use-package vertico
  :straight (:host github :repo "minad/vertico"
                   :files (:defaults "extensions/*")
                   :includes (vertico-buffer
                              vertico-directory
                              vertico-flat
                              vertico-indexed
                              vertico-mouse
                              vertico-quick
                              vertico-repeat
                              vertico-reverse))
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :after vertico
  :general
  (my/leader-def
    "cx" '(consult-flymake :wk "list errors")
    "sb" '(consult-line :wk "search buffer")
    "ss" '(consult-ripgrep :wk "ripgrep")
    "sy" '(consult-yank-from-kill-ring :wk "consult yank from kill ring"))
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap evil-show-marks]               . consult-mark)
  ([remap imenu]                         . consult-imenu)
  ([remap locate]                        . consult-locate)
  ([remap load-theme]                    . consult-theme)
  ([remap man]                           . consult-man)
  ([remap recentf]                       . consult-recent-file)
  ([remap switch-to-buffer]              . consult-buffer)
  ([remap yank-pop]                      . consult-yank-pop)
  )

(use-package embark
  :general
  (:keymaps
   '(minibuffer-local-map
     minibuffer-local-ns-map
     minibuffer-local-completion-map
     minibuffer-local-must-match-map)
   "C-." 'embark-dwim
   "C-;" 'embark-act)
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t 
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;; Git
(use-package magit
  :defer 2
  :preface
  ;; (autoload 'magit-show-commit "magit-diff" nil t)
  (defun +git-commit-set-fill-column ()
    "Set `fill-column' for git commit."
    (setq-local fill-column 72))
  :hook
  (git-commit-mode-hook . +git-commit-set-fill-column)
  :commands (magit-status)
  :general
  (my/leader-def
    "g" '(:ignore t :wk "git")
    "gb" '(magit-branch-checkout :wk "checkout branch")
    "gs" '(magit-stage :wk "stage hunk")
    "gS" '(magit-stage-file :wk "stage file")
    "gl" '(magit-log-buffer-file :wk "log this file")
    "gg" '(magit-status :wk "status"))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers nil)
  (setq magit-revision-insert-related-refs nil)
  (setq magit-log-buffer-file-locked t)
  ;; (setq magit-refs-show-commit-count 'all)
  ;; (setq magit-save-repository-buffers nil)
  :config
  ;; Unset pager as it is not supported properly inside emacs.
  (setenv "GIT_PAGER" ""))

(use-package diff-hl
  :defer 2
  :commands
  (global-diff-hl-mode)
  :preface
  (autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" nil t)
  (autoload 'diff-hl-dired-mode "diff-hl-dired" nil t)
  ;;(defrepeater #'diff-hl-next-hunk)
  ;;(defrepeater #'diff-hl-previous-hunk)
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ;; :general
  ;; (:keymaps
  ;;  'diff-hl-mode-map
  ;;  :prefix next-prefix
  ;;  "d" '(diff-hl-next-hunk :wk "Diff Hunk"))
  ;; (:keymaps
  ;;  'diff-hl-mode-map
  ;;  :prefix prev-prefix
  ;;  "d" '(diff-hl-previous-hunk :wk "Diff Hunk"))
  ;; ([remap diff-hl-next-hunk] 'diff-hl-next-hunk-repeat
  ;;  [remap diff-hl-previous-hunk] 'diff-hl-previous-hunk-repeat)
  :init
  ;; (setq diff-hl-ask-before-revert-hunk nil)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))


;;; project

(use-package projectile
  :general
  (my/leader-def
    "p" '(:keymap projectile-command-map :wk "project"))
  :demand t
  :config
  (projectile-mode)
  :custom
  (projectile-enable-caching (not noninteractive))
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-ignored-projects '("~/"))
  (projectile-switch-project-action #'projectile-find-file))

(use-package perspective
  :demand t
  :general
  (:states 'motion
        "]w" 'persp-next
        "[w" 'persp-prev)
  (my/leader-def
    ;; "bd" '(persp-remove-buffer* :wk "kill workspace buffer")
    "<tab>" '(:ignore t :wk "workspace")
    "<tab><tab>" '(persp-switch :wk "switch workspace")
    "<tab>b" '(persp-switch-to-buffer* :wk "switch workspace buffer")
    "<tab>d" '(persp-kill :wk "delete workspace")
    "<tab>l" '(persp-switch-last :wk "switch to last workspace")
    "<tab>n" '(persp-next :wk "next workspace")
    "<tab>p" '(persp-prev :wk "prev workspace")
    "<tab>r" '(persp-rename :wk "rename workspace")
    "<tab>x" '(persp-switch-to-scratch-buffer :wk "switch to workspace scratch"))

  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-initial-frame-name "main")
  (persp-show-modestring nil)
  :config
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package persp-projectile
  :demand t
  :general
  (my/leader-def
    "pp" '(projectile-persp-switch-project :wk "switch project")))

(use-package perspective-tabs
  :after (perspective)
  :straight (:host sourcehut :repo "woozong/perspective-tabs")
  :config
  (perspective-tabs-mode +1))

;; help
(use-package helpful
  :general
  (my/leader-def
    "h" '(:keymap help-map :wk "help"))
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;; navigation
(use-package avy
  :general
  (:states 'normal
           "s" 'evil-avy-goto-char-2)
  :custom
  (avy-background t))

;; packages
(use-package default-text-scale
  :general
  ("C-M-=" 'default-text-scale-increase)
  ("C-M--" 'default-text-scale-decrease))

(use-package autorevert
  :init
  (setq glboal-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose t)
  (global-auto-revert-mode 1))

(use-package saveplace
  :defer 2
  :config
  (save-place-mode 1))

(use-package anzu
  :config
  (global-anzu-mode +1))

(use-package evil-anzu
  :after (evil anzu))

(use-package ws-butler
  :config
  (ws-butler-global-mode))

(use-package rainbow-delimiters
  :hook
  ((clojure-mode
    emacs-lisp-mode
    ielm-mode
    lisp-mode
    racket-mode) . rainbow-delimiters-mode)
  :init
  (setq rainbow-delimiters-max-face-count 3))

;;; treesitter

(use-package tree-sitter
  :config
  (use-package tree-sitter-langs)
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "A" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer")))
  (define-key evil-inner-text-objects-map "A" (evil-textobj-tree-sitter-get-textobj ("parameter.inner" "call.inner"))))

;;; lsp

(use-package eglot
  :commands (eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-send-changes-idle-time 0.5)
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
  ;;      its popup rule causes eglot to steal focus too often.
  (eglot-auto-display-help-buffer nil)
  :config
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       (cape-company-to-capf #'company-yasnippet)))))
  :general
  (my/leader-def
    "c" '(:ignore t :wk "lsp")
    "ca" '(eglot-code-actions :wk "code action")
    "cr" '(eglot-rename :wk "rename")
    "cf" '(eglot-format-buffer :wk "format buffer")
    "ci" '(eglot-find-implementation :wk "find implementation"))
  :hook
  (eglot-managed-mode . my/eglot-capf))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :straight (:type built-in))

(use-package flymake
  :straight (:type built-in)
  :general
  (:states 'motion
    "]c" 'flymake-goto-next-error
    "[c" 'flymake-goto-prev-error))

(use-package flymake-popon
  :straight (flymake-popon :type git :repo "https://codeberg.org/akib/emacs-flymake-popon.git")
  :init
  (setq flymake-popon-delay 0.5)
  :hook
  (flymake-mode . flymake-popon-mode))


;;; Langs
;; emacs lisp
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
;; markdown


(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;;; Go
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(defun my/find-eglot-go-module (dir)
  (when-let* ((found (locate-dominating-file dir "go.mod")))
    (cons 'go-module found)))

(add-hook 'project-find-functions 'my/find-eglot-go-module nil nil)

(use-package go-mode
  :general
  (my/local-leader-def
    :keymaps 'go-mode-map
    "b" '(:ignore t :wk "build")
    "br" 'go-run)
  :hook
  (go-mode . eglot-ensure)
  (go-mode . my/go-mode-hook)
  :init
  (defun my/go-mode-hook()
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    (defun my-eglot-organize-imports () (interactive)
	   (eglot-code-actions nil nil "source.organizeImports" t))
    (add-hook 'before-save-hook 'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook 'my-eglot-organize-imports -10 t))
  )

(use-package gotest
  :after go-mode
  :general
  (my/local-leader-def
    :keymaps 'go-mode-map
    "t" '(:ignore t :wk "test")
    "ts" 'go-test-current-test
    "tt" 'go-test-current-test-cache
    "tf" 'go-test-current-file
    "ta" 'go-test-current-project
    "tb" 'go-test-current-benchmark))

(use-package go-tag
  :after go-mode
  :general
  (my/local-leader-def
   :keymaps 'go-mode-map
   "a" '(:ignore t :wk "tag")
   "ar" 'go-tag-refresh
   "aa" 'go-tag-add
   "ad" 'go-tag-remove))

;;; typescript
(use-package rjsx-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :hook
  (rjsx-mode . eglot-ensure)
  :init
  (setq js-chain-indent t
        js2-basic-offset 2
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-missing-semi-warning nil
        js2-highlight-level 3
        js2-idle-timer-delay 0.5)
  )

(use-package typescript-mode
  :hook
  (typescript-mode . eglot-ensure)
  :init
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'rjsx-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package web-mode
  :mode "\\.\\(erb\\|html?\\|tmpl\\)\\'"
  :init
  (setq web-mode-enable-html-entities-fontification t)
  ;; Highlight enclosing tags of the element under cursor
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; (setq web-mode-enable-current-element-highlight t)
  ;; No extra indentation for blocks.
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))


;;; terminal
(use-package vterm
  :general
  (my/leader-def
    "oT" 'vterm)
  :commands (vterm vterm-other-window)
  :init
  (setq vterm-buffer-name-string "vterm - %s")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local confirm-kill-processes nil)
              (setq-local hscroll-margin 0)
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))
