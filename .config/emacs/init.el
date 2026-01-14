;;; init.el --- init file -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar elpaca-core-date '(20250814))
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Load general for :general
(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-create-definer leader-def
    :states '(visual normal motion)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer localleader-def
          :states '(visual normal motion)
          :keymaps 'local
          :prefix "SPC m"))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if init-file-debug
       (progn ,@forms)
     (let ((message-log-max nil))
       (with-temp-message (or (current-message) "") ,@forms))))

(defun +org-agenda-today ()
  (interactive)
  (org-agenda nil "a"))

(defun +find-notes ()
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively 'find-file)))

(defun +find-init-org ()
  "Open init.org file."
  (interactive)
  (find-file (expand-file-name "init.org" user-emacs-directory)))

(defun +go-playground ()
  (interactive)
  (find-file "~/code/exp/playground/main.go"))

(defun +download-file (url)
  "Download file from URL."
  (interactive "sEnter URL: ")
  (url-copy-file url (read-file-name "Save as: ")))

(defun +save-buffer-absolute-path ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied %s to clipboard" (buffer-file-name)))

;; Save custom vars to separate file from init.el.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(use-package gcmh
  :init
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-mode 1)
  (add-function :after after-focus-change-function
                (lambda ()
                  (unless (frame-focus-state)
                    (garbage-collect))))
  )

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME" "KUBECONFIG"))
  :init
  (exec-path-from-shell-initialize))

(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")
(setenv "GIT_EDITOR" "emacsclient")

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t)
  :custom
  (evil-echo-state nil)
  (evil-want-keybinding nil)
  (evil-v$-excludes-newline t)
  (evil-mode-line-format nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-symbol-word-search t)
  (evil-visual-state-cursor '(hollow))
  (evil-goto-definition-functions '(evil-goto-definition-xref
                                         evil-goto-definition-imenu
                                         evil-goto-definition-semantic
                                         evil-goto-definition-search))
  :config
  (evil-mode 1)
  (modify-syntax-entry ?_ "w")
  (defalias 'forward-evil-word 'forward-evil-symbol)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-redo)
  (evil-define-key '(motion) 'global "j" 'evil-next-visual-line)
  (evil-define-key '(motion) 'global "k" 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map [remap cua-paste-pop] nil)
  (define-key evil-normal-state-map [remap yank-pop] nil)
  (evil-set-initial-state 'shell-command-mode 'normal)
  (evil-set-initial-state 'comint-mode 'normal)

  (defun evil-insert-at-process-mark ()
    (interactive)
    (comint-goto-process-mark)
    (evil-insert-state))

  (leader-def
    "SPC" '(execute-extended-command :wk "M-x")
    ":" 'pp-eval-expression
    "u" '("C-u" . universal-argument)

    "b" '(:ignore t :wk "buffer")
    "bb" 'switch-to-buffer
    "bd" 'kill-current-buffer
    "bi" 'ibuffer
    "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :wk "Open Messages")
    "bx" 'remember-notes

    "f" '(:ignore t :wk "file")
    "fd" 'dired
    "ff" 'find-file
    "fi" '+find-init-org
    "fj" 'dired-jump
    "fr" 'recentf
    "fS" 'save-some-buffers
    "fs" 'save-buffer
    "fW" '((lambda () (interactive) (dired "~/Downloads")) :wk "Go to Downloads")
    "fy" '+save-buffer-absolute-path

    "c" '(:ignore t :wk "code")
    "cd" 'xref-find-definitions
    "cD" 'xref-find-definitions-other-window
    "cR" 'xref-find-references

    "n" '(:ignore t :wk "notes")
    "nt" 'org-todo-list
    "nf" '+find-notes

    "o" '(:ignore t :wk "open")
    "ol" 'browse-url
    "ow" '+download-file

    "s" '(:ignore t :wk "search")
    "si" 'imenu
    "so" 'occur

    "g" '(:ignore t :wk "git")
    "w" '(:keymap evil-window-map :wk "window"))
  )

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-key-blacklist '("C-y"))
  :config
  (evil-collection-init)
  )

(use-package evil-nerd-commenter
  :general
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines)
  (:states '(normal visual)
           "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :defer 1
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package avy
  :general
  (:states '(normal visual)
           "s" 'evil-avy-goto-char-2)
  (:keymaps 'isearch-mode-map
            "C-j" 'avy-isearch)
  :custom
  (avy-background t))

(repeat-mode 1)

(use-package which-key
  :defer 1
  :ensure nil
  :custom
  (which-key-ellipsis "..")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-add-column-padding 1)
  (which-key-side-window-slot -10)
  (which-key-min-display-lines 5)
  (which-key-show-operator-state-maps t)
  :config
  (which-key-mode 1))

(setq-default line-spacing 0.3)

(use-package emacs
  :ensure nil
  :bind
  ("M-=" . global-text-scale-adjust)
  ("M--" . global-text-scale-adjust)
  ("M-0" . global-text-scale-adjust)
  :hook
  (elpaca-after-init . (lambda ()
                         (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 130)
                         (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 0.9)
                         (set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 1.1)
                         (set-face-attribute 'mode-line nil :family (face-attribute 'variable-pitch :family) :height 1.0)
                         (set-face-attribute 'mode-line-active nil :family (face-attribute 'variable-pitch :family) :height 1.0)
                         (set-face-attribute 'mode-line-inactive nil :family (face-attribute 'variable-pitch :family) :height 1.0)
                         (set-face-attribute 'tab-bar nil :family (face-attribute 'variable-pitch :family) :height 0.9)
                         (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
                         ))
  )

(use-package nerd-icons
  :defer t)

(setq fast-but-imprecise-scrolling t)
(setq auto-window-vscroll nil)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 10)

(use-package ultra-scroll
  :when (fboundp 'pixel-scroll-precision-mode)
  :hook
  (elpaca-after-init . ultra-scroll-mode)
  :config
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode))

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :hook ((org-mode markdown-mode) . (lambda () (display-line-numbers-mode 0)))
  :custom
  (display-line-numbers-type 'relative)
  )

(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(global-visual-line-mode 1)

(setq split-width-threshold 160
      split-height-threshold nil)

(setq window-resize-pixelwise nil)
(setq window-combination-resize t)
(setq frame-resize-pixelwise t)

(use-package ace-window
  :defer t
  :custom-face
  (aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 2.8))))
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (aw-dispatch-always t)
  :general
  (leader-def
    "wa" 'ace-window))

(use-package popper
  :defer 0.5
  :demand t
  :bind
  ("C-`"  . popper-toggle)
  ("C-\\" . popper-cycle)
  ("C-~"  . popper-toggle-type)
  :custom
  (popper-window-height 0.40)
  (popper-group-function #'popper-group-by-project)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "Output\\*$"
     ("\\*Compile-Log\\*" . hide)
     "\\*Async Shell Command\\*$"
     compilation-mode
     comint-mode
     "^\\*term.*\\*$" term-mode
     "^\\*shell.*\\*$" shell-mode shell-command-mode
     ;; "^\\*eshell" eshell-mode
     "-eshell\\*$"
     ;; "^\\*vterm" vterm-mode
     "-vterm\\*$"
     "\\*Go Test\\*$"
     "\\*Flycheck errors\\*"
     "\\*rake-compilation\\*"
     "\\*rspec-compilation\\*"
     "\\*Org Select\\*"
     help-mode
     lsp-help-mode
     helpful-mode
     "\\*Capture\\*"
     "^CAPTURE-"
     "\\*Org Links\\*"
     "\\*xref\\*"
     "\\*eldoc\\*"
     "\\magit-process:"
     inf-ruby-mode
     sbt-mode
     "\\*Embark Export:"
     "\\*Embark Collect:"
     flutter-mode
     "\\*LSP Dart tests\\*"
     "\\*LSP Dart commands\\*"
     "\\*sdcv"
     "\\*sly-description\\*"
     "\\*Calendar"
     "\\*rust-analyzer"
     "\\*cargo-"
     "\\*rustic-"
     cargo-compilation-mode
     "\\*LSP Error List\\*"
     "\\*Process List\\*"
     ))
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(blink-cursor-mode -1)
(save-place-mode 1)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package catppuccin-theme
  :custom
  (catppuccin-height-title-3 1.1)
  :init
  (load-theme 'catppuccin :no-confirm)
  (custom-set-faces
   `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))
  (custom-set-faces
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))
  (custom-set-faces
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green))))))
  )

(line-number-mode 1)
(column-number-mode 1)
(setq mode-line-percent-position nil)

(use-package doom-modeline
  :defer t
  :custom
  (doom-modeline-bar-width 0)
  (doom-modeline-height 32)
  (doom-modeline-buffer-file-name-style 'buffer)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-modal nil)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-env-version nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-indent-info t)
  (doom-modeline-check 'simple)
  :config
  ;; new logic makes some texts become normal weight in inactive
  (defun doom-modeline-face (&optional face inactive-face)
    "Display FACE in active window, and INACTIVE-FACE in inactive window.
IF FACE is nil, `mode-line' face will be used.
If INACTIVE-FACE is nil, `mode-line-inactive' face will be used."
    (if (doom-modeline--active)
        (or (and (facep face) `(:inherit (doom-modeline ,face)))
            (and (facep 'mode-line-active) '(:inherit (doom-modeline mode-line-active)))
            '(:inherit (doom-modeline mode-line)))
      (or (and (facep face) `(:inherit (doom-modeline mode-line-inactive ,face)))
          (and (facep inactive-face) `(:inherit (doom-modeline ,inactive-face)))
          '(:inherit (doom-modeline mode-line-inactive)))))

  ;; show compiling
  (doom-modeline-def-segment compilation
    (and (bound-and-true-p compilation-in-progress)
         (derived-mode-p '(compilation-mode comint-mode))
         (propertize "[Compiling] "
                     'face (doom-modeline-face 'doom-modeline-compilation)
                     'help-echo "Compiling; mouse-2: Goto Buffer"
                     'mouse-face 'doom-modeline-highlight
                     'local-map
                     (make-mode-line-mouse-map
                      'mouse-2
                      #'compilation-goto-in-progress-buffer))))

  :hook
  (elpaca-after-init . doom-modeline-mode))

(use-package anzu
  :defer 1
  :general
  (:states '(visual)
           "M-s r" 'anzu-query-replace-regexp)
  (leader-def
    "sr" #'anzu-query-replace)
  :bind
  (:map isearch-mode-map
        ([remap isearch-query-replace] . anzu-isearch-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
        ("M-s r" . anzu-isearch-query-replace-regexp)
        )
  :config
  (global-anzu-mode 1))

(use-package evil-anzu
  :after (evil anzu))

(use-package hl-todo
  :defer 1
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode 1))

(setq use-short-answers t)
(setq confirm-kill-emacs #'y-or-n-p)

;; Disable warnings from the legacy advice API. They aren't actionable or useful.
(setq ad-redefinition-action 'accept)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq ring-bell-function #'ignore)

(setq kill-whole-line t)
(delete-selection-mode 1)

(use-package autorevert
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (auto-revert-use-notify nil)
  :config
  ;; global-auto-revert-mode can slow things down. try to enable it per active window.
  (add-to-list 'window-state-change-functions
               (defun ar/window-state-state-change (state)
                 (let* ((old-selected-window (old-selected-window))
                        (old-buffer (when old-selected-window
                                      (window-buffer old-selected-window)))
                        (selected-window (selected-window))
                        (new-buffer (when selected-window
                                      (window-buffer selected-window))))
                   (when old-buffer
                     (with-current-buffer old-buffer
                       (when buffer-file-name
                         (auto-revert-mode -1))))
                   (when new-buffer
                     (with-current-buffer new-buffer
                       (when buffer-file-name
                         (auto-revert-mode +1))))))))

(use-package emacs
  :ensure nil
  :init
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  (setq confirm-nonexistent-file-or-buffer nil)
  (setq large-file-warning-threshold nil)
  (setq delete-by-moving-to-trash t)

  (setq find-file-visit-truename t)
  (setq vc-follow-symlinks t)

  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-after-kill-buffer-p t)

  (setq save-some-buffers-default-predicate #'save-some-buffers-root)

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package recentf
  :ensure nil
  :defer .5
  :hook ((buffer-list-update . recentf-track-opened-file))
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 50)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("/auto-install/"
                     ".recentf"
                     "/repos/"
                     "/elpa/"
                     "\\.mime-example"
                     "\\.ido.last"
                     "COMMIT_EDITMSG"
                     ".gz"
                     "~$"
                     "/ssh:"
                     "/sudo:"
                     "/scp:"))
  :config
  (quiet! (recentf-mode 1)))

(use-package crux
  :general
  (leader-def
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer))

(use-package dired
  :defer t
  :ensure nil
  :commands dired
  :custom
  (dired-use-ls-dired nil)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-listing-switches "-lah")
  (dired-kill-when-opening-new-dired-buffer t))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(setq remember-notes-initial-major-mode 'org-mode)
(setq remember-notes-buffer-name "*scratch*")
(setq initial-buffer-choice 'remember-notes)

(setq kill-do-not-save-duplicates t)
(setq save-interprogram-paste-before-kill t)

(use-package electric-pair-mode
  :ensure nil
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-skip-whitespace nil)
  :hook
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  ((prog-mode text-mode conf-mode) . electric-pair-local-mode)
  :preface
  (defun +add-pairs (pairs)
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))

(use-package paren
  :ensure nil
  :defer 1
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package undo-fu-session
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init
  (undo-fu-session-global-mode 1))

(setq find-program "fd")
(setq frame-title-format
      (list
       '(:eval
         (let ((project (project-current)))
           (when project
             (format "%s — " (project-name project)))))
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
       ))

(use-package project
  :defer t
  :ensure nil
  :custom
  (project-switch-commands 'project-dired)
  :general
  (leader-def
    "p" '(:keymap project-prefix-map :wk "project")))

(use-package tab-bar
  :ensure nil
  :defer t
  :commands (tab-bar-mode)
  :preface
  (defun +tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat
        (propertize " " 'display '(space :width (8)))
        (alist-get 'name tab)
        (propertize " " 'display '(space :width (8)))
        )
       'face (funcall tab-bar-tab-face-function tab))))
  (defun +tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " ")
  :custom
  (tab-bar-close-tab-select 'recent)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-format '(tab-bar-format-tabs #'+tab-bar-suffix))
  (tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)
  (tab-bar-new-tab-choice "*dashboard*")
  )

(use-package tabspaces
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "home")
  (tabspaces-include-buffers '("*dashboard*" "*Messages*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session nil)
  :bind
  ([remap project-switch-project] . tabspaces-open-or-create-project-and-workspace)
  :general
  (leader-def
    "TAB" '(:keymap tabspaces-command-map :wk "workspaces"))
  :init
  (tabspaces-mode 1)
  (tab-bar-mode 1)
  (tab-bar-rename-tab tabspaces-default-tab) ;; Rename intial tab to default tab

  (define-key tabspaces-command-map (kbd "TAB") 'tab-bar-switch-to-tab)
  (define-key tabspaces-command-map (kbd "n") 'tab-bar-switch-to-next-tab)
  (define-key tabspaces-command-map (kbd "p") 'tab-bar-switch-to-prev-tab)

  (with-eval-after-load 'consult
    (consult-customize consult-source-buffer :hidden t :default nil)

    (defvar consult-source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate (lambda (x) (and (tabspaces--local-buffer-p x) (not (popper-popup-p x))))
                                  :sort 'visibility
                                  :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult-source-workspace)
    )
  )

(use-package dashboard
  :hook
  (elpaca-after-init . dashboard-insert-startupify-lists)
  (elpaca-after-init . dashboard-initialize)
  :custom
  (initial-buffer-choice #'dashboard-open)
  (dashboard-startup-banner "~/.config/emacs/kannasan1.png")
  (dashboard-image-banner-max-height 450)
  (dashboard-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-items '((projects . 5)))
  :config
  (dashboard-setup-startup-hook)
  )

(setq echo-keystrokes-help nil)

(use-package help
  :ensure nil
  :defer t
  :custom
  (help-window-select t)
  :general
  (leader-def
    "h" '(:keymap help-map :wk "help")
    "ha" 'describe-face)
  :bind
  (:map help-mode-map
        ([remap quit-window] . kill-buffer-and-window))
  )

(use-package helpful
  :defer t
  :custom
  (helpful-max-buffers 1)
  :commands (helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  (:map helpful-mode-map
        ([remap quit-window] . kill-buffer-and-window))
  :general
  (leader-def
    "hy" 'helpful-macro)
  )

(use-package hydra
  :defer t)

(use-package transient
  :defer t
  :custom
  (transient-default-level 5)
  (transient-display-buffer-action
   '(display-buffer-below-selected
     (dedicated . t)
     (inhibit-same-window . t)))
  (transient-show-during-minibuffer-read t)
  :config
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one))

(use-package magit
  :defer t
  :general
  (leader-def
    "gB" 'magit-blame-addition
    "gC" 'magit-clone
    "gL" 'magit-log-buffer-file
    "gg" 'magit-status)
  :bind (:map magit-diff-section-map
              ("RET" . magit-diff-visit-worktree-file)
              ("<C-return>" . magit-diff-visit-file))
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-uniquify-buffer-names nil)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)

  (dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.cfg")))
  (dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (transient-replace-suffix 'magit-branch "b"
    '("b" "branch" magit-branch-checkout))
  (transient-replace-suffix 'magit-branch "l"
    '("l" "revision" magit-checkout))

  (with-eval-after-load 'tabspaces
    (defun +magit-open-project-workspace ()
      (tabspaces-open-or-create-project-and-workspace default-directory))
    (add-hook 'magit-post-clone-hook #'+magit-open-project-workspace))

  :hook
  (magit-process-mode . goto-address-mode) ;; Turn ref links into clickable buttons.
  )

(use-package git-commit
  :ensure nil
  :after magit
  :hook
  (git-commit-setup . (lambda ()
                        (setq-local fill-column 72)
                        (when (and (bound-and-true-p evil-mode)
                                   (bobp) (eolp))
                          (evil-insert-state))))
  :config
  (setq git-commit-summary-max-length 72)
  (setq git-commit-style-convention-checks '(overlong-summary-line))
  (global-git-commit-mode 1))

(use-package browse-at-remote
  :commands (browse-at-remote)
  :general
  (leader-def
    "gw" 'browse-at-remote)
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^git\\.xspringas\\.com$" :type "gitlab")))

(use-package smerge-mode
  :ensure nil
  :general
  (leader-def
    "gm" 'hydra-smerge/body
    )
  :config
  (defhydra hydra-smerge (:color pink
                                 :hint nil
                                 :pre (smerge-mode 1)
                                 :post (smerge-auto-leave))
    "
                                                                [smerge]
       Move        Keep             Diff              Other        │
    ╭──────────────────────────────────────────────────────────────╯
    │  [_p_] prev    [_u_] upper      [_<_] upper/base    [_C_] Combine
    │  [_k_] ↑       [_l_] lower      [_=_] upper/lower   [_r_] resolve
    │  [_j_] ↓       [_a_] all        [_>_] base/lower    [_R_] remove
    │  [_n_] next    [_b_] base       [_H_] hightlight
    │              _RET_ current    [_E_] ediff         [_q_] quit
    ╰────────────────────────────────────────────────╯
  "
    ("n" (progn (goto-char (point-min)) (smerge-next)))
    ("p" (progn (goto-char (point-max)) (smerge-prev)))
    ("j" (lambda () (interactive) (evil-next-visual-line)))
    ("k" (lambda () (interactive) (evil-previous-visual-line)))
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ;; Often after calling `smerge-vc-next-conflict', the cursor will land at
    ;; the bottom of the window
    ;; ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
    ("q" nil :color blue))
  :hook
  (find-file . (lambda ()
                 (unless (bound-and-true-p smerge-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward "^<<<<<<< " nil t)
                       (smerge-mode 1)
                       ))))))

(setq history-delete-duplicates t)
(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package savehist
  :ensure nil
  :custom
  (history-length 500)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval nil)
  (savehist-additional-variables '(kill-ring register-alist search-ring regexp-search-ring comint-input-ring))
  :config
  (add-to-list 'savehist-additional-variables 'project-compile-history-alist)
  :hook
  (elpaca-after-init . savehist-mode))

(use-package vertico
  :defer t
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p) ;; hide commands that does not work
  (vertico-resize nil)
  (vertico-count 15)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (elpaca-after-init . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(use-package marginalia
  :defer t
  :custom
  (marginalia-align 'right)
  :hook
  (elpaca-after-init . marginalia-mode))

(use-package consult
  :general
  (leader-def
    "sB" #'consult-line-thing-at-point
    "sb" #'consult-line
    "sj" #'consult-outline
    "sP" #'consult-ripgrep-in-dir
    "sp" #'consult-ripgrep)
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap goto-line]                     . consult-goto-line)
  ([remap evil-show-marks]               . consult-mark)
  ([remap imenu]                         . consult-imenu)
  ([remap locate]                        . consult-locate)
  ([remap load-theme]                    . consult-theme)
  ([remap man]                           . consult-man)
  ([remap recentf]                       . consult-recent-file)
  ([remap switch-to-buffer]              . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap yank-pop]                      . consult-yank-pop)
  ([remap project-switch-to-buffer]      . consult-project-buffer)
  :bind
  (:map minibuffer-local-map
        ("M-r" . consult-history))
  (:map isearch-mode-map
        ("C-b" . consult-line-from-isearch))
  :preface
  (defun consult-line-from-isearch ()
    "Invoke `consult-line' from isearch."
    (interactive)
    (let ((query (if isearch-regexp

                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (consult-line query)))
  (defun consult-ripgrep-in-dir ()
    "Search with `rg' for files in DIR selected from prompt"
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'consult-ripgrep))
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<")
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))

  (defalias 'consult-line-thing-at-point 'consult-line)

  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))

  )

(use-package consult-dir
  :custom
  (consult-dir-shadow-filenames nil)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)
         )
  )

(use-package embark
  :commands (embark-act embark-dwim)
  :bind
  ([remap describe-bindings] . embark-bindings)
  :preface
  (defun +embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                (x (user-error "Embark category %S doesn't support writable export" x)))))
           (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export)))
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

  (eval-when-compile
    (defmacro +embark-ace-action (fn)
      `(defun ,(intern (concat "+embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-show-dispatch-help))
             ;; (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  ;; (general-define-key
  ;;  :keymaps 'embark-file-map
  ;;  "o" (+embark-ace-action find-file))
  ;; (general-define-key
  ;;  :keymaps 'embark-buffer-map
  ;;  "o" (+embark-ace-action switch-to-buffer))
  ;; (general-define-key
  ;;  :keymaps 'embark-general-map
  ;;  "D" #'xref-find-definitions-other-window)
  ;; :general
  ;; (:keymaps 'minibuffer-local-map
  ;;           "C-c C-e" #'+embark-export-write)
  :bind
  ("C-;" . embark-act))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :after corfu
  :config
  (add-hook 'org-mode-hook
            (defun +corfu-add-cape-elisp-block-h ()
              (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t)))

  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package corfu
  :hook
  ((prog-mode text-mode conf-mode) . corfu-mode)
  ((eshell-mode comint-mode) . corfu-enable-in-shell)
  (minibuffer-setup . corfu-enable-in-shell)
  :preface
  (defun corfu-enable-in-shell ()
    (setq-local corfu-auto t)
    (corfu-mode 1))
  :init
  (setq text-mode-ispell-word-completion nil)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.15)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-count 14)
  (setq corfu-preview-current nil)
  (setq corfu-preselect 'first)
  (setq corfu-on-exact-match 'show)
  :config
  (set-face-attribute 'corfu-default nil :inherit 'default)
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))

  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)

  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  )

(use-package corfu-history
  :ensure nil
  :after (savehist corfu)
  :config
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (defun +corfu-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
    (let ((candidates
           (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
             (if display-sort-func
                 (funcall display-sort-func candidates)
               candidates))))
      (if corfu-sort-function
          (funcall corfu-sort-function candidates)
        candidates)))

  (setq corfu-sort-override-function #'+corfu-combined-sort))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet
  :after corfu
  :config
  (setq yas-use-menu nil)
  (setq yas-verbosity 2)
  (yas-global-mode +1)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-keymap [(tab)] nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "C-<return>") (yas-filtered-definition 'yas-next-field-or-maybe-expand))
  )

(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package orderless
  :init
  (setq completion-ignore-case t
        completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)
  )

(defun +org-insert-item-after-current ()
  (interactive)
  (org-beginning-of-line)
  (org-insert-item)
  (org-move-item-down)
  (end-of-line 1)
  (evil-insert-state))

(defun +org-insert-heading-after-current ()
  (interactive)
  (org-insert-heading-after-current)
  (evil-insert-state))

(defun +org-smart-insert-after-current ()
  (interactive)
  (cond
   ((org-at-heading-p)
    (+org-insert-heading-after-current))
   ((org-at-item-p)
    (+org-insert-item-after-current))
   (t
    (org-return)))
  )

(use-package org
  :ensure nil
  :defer t
  :hook (org-mode . variable-pitch-mode)
  :init
  (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/") ;; don't know why custom doesn not work
  (setq org-use-fast-todo-selection 'expert)
  (setq org-src-window-setup 'current-window)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation t)
  (setq org-pretty-entities t)
  (setq org-ellipsis "…")
  (setq org-auto-align-tags nil)
  (setq org-imenu-depth 6)
  (setq org-insert-heading-respect-content t)
  (setq org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)))
  (setq org-use-sub-superscripts '{})
  (setq org-tags-column 0)
  (setq org-startup-indented t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-log-into-drawer t)
  :config
  ;; Resize Org headings
  (dolist (face '((org-document-title . 1.44)
                  (org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (dolist (face '(org-table org-tag org-verbatim org-list-dt org-hide
                            org-date org-todo org-done org-formula
                            org-checkbox org-special-keyword))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  (set-face-attribute 'org-block nil :foreground (catppuccin-get-color 'text) :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))

  (add-to-list 'org-modules 'org-habit t)

  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key '(normal insert) org-mode-map
  ;;     (kbd "C-<return>") '+org-smart-insert-after-current
  ;;     )
  ;;   )
  )


(use-package org-indent
  :ensure nil
  :hook
  (org-mode . org-indent-mode))

(use-package evil-org
  :after (org evil)
  ;; :preface
  ;; (defun evil-org-goto-last-heading ()
  ;;   "Go to the last heading in the current Org buffer, append a space if needed, and enter insert mode."
  ;;   (interactive)
  ;;   (goto-char (point-max))
  ;;   (when (re-search-backward org-heading-regexp nil t)
  ;;     (end-of-line)
  ;;     (unless (looking-back " " 1)
  ;;       (insert " "))
  ;;     (evil-org-append-line 1)))
  :config
  (setf evil-org-key-theme '(textobjects insert navigation additional todo))
  ;; (evil-define-key '(normal insert) 'evil-org-mode
  ;;   (kbd "<C-s-return>") 'evil-org-goto-last-heading
  ;;   (kbd "<C-return>") (evil-org-define-eol-command org-insert-heading-after-current)
  ;;   (kbd "<C-S-return>") (evil-org-define-bol-command org-insert-heading))
  :hook
  (org-mode . evil-org-mode)
  (org-agenda-mode . (lambda ()
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-table nil)
  (org-modern-hide-stars nil)
  (org-modern-list
   '((?* . "•")
     (?+ . "‣")))
  )

(use-package org-modern-indent
  :ensure (:host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-mode . org-modern-indent-mode))

(use-package org-tempo
  :after org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  )

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                      (todo tag-up priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d!)")
          (sequence "[ ](T)" "|" "[X](x!)")))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '(("tasks.org" :maxlevel . 1)))
  (setq org-agenda-files nil)
  (setq org-agenda-confirm-kill nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-capture-templates
        `(("t" "Task" entry (file "tasks.org")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))")
          ("e" "Emacs todo" entry (file+olp "~/.config/emacs/init.org" "Todos")
           "* %?")
          ("v" "Vocalubary" entry (file "vocab.org")
           "* %?")
          ))

  :hook
  (org-agenda-mode . hl-line-mode)
  (org-agenda-mode . (lambda ()
                       (interactive) (org-element-cache-reset 'all)))
  (org-capture-mode . evil-insert-state)
  (org-capture-mode . (lambda ()
                        (setq header-line-format
                              (format "%s%s%s"
                                      (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                                  'face 'font-lock-string-face)
                                      " → "
                                      header-line-format))))
  :config
  ;; Refresh agenda after capturing.
  (add-hook 'org-capture-after-finalize-hook 'org-agenda-maybe-redo)

  ;; Save agenda buffers after doing these actions
  (dolist (hook '(org-refile
                  org-agenda-archive
                  org-agenda-add-note
                  org-agenda-deadline
                  org-agenda-kill
                  org-agenda-todo
                  org-agenda-refile
                  org-agenda-schedule
                  org-agenda-set-property
                  org-agenda-set-tags))
    ;; https://github.com/bbatsov/helm-projectile/issues/51
    (advice-add hook :after (lambda (&rest _) (org-save-all-org-buffers))))

  ;; need this because syncing updates from cloud show categories as ???
  (advice-add #'org-agenda-redo :after (lambda (&rest _) (org-element-cache-reset t)))

  (with-eval-after-load 'evil
    (evil-define-key 'emacs org-agenda-mode-map
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "x" 'org-agenda-bulk-action
      "q" 'org-agenda-exit

      "M-j" 'org-agenda-drag-line-forward
      "M-k" 'org-agenda-drag-line-backward

      "dd" 'org-agenda-kill
      "dA" 'org-agenda-archive
      )
    )
  )

;; (use-package org-super-agenda
;;   :after (org-agenda)
;;   :config
;;   (setq org-agenda-span 'day)
;;   (setq org-agenda-time-grid '((daily) () "" ""))
;;   (setq org-agenda-prefix-format
;;         '(
;;           (agenda . " %i %-12:c%?-12t ")
;;           (todo . " %i %-12:c")
;;           (tags . " %i %-12:c")
;;           (search . " %i %-12:c")
;;           ))
;;   (setq org-super-agenda-groups
;;         '(
;;           (:name "Habits" :and(:category "habits" :todo "TODO")) ;; warning
;;           (:name "Today" :time-grid t)
;;           (:name "Emacs" :tag "emacs")
;;           (:name "Shopping" :tag "shopping")
;;           (:name "Others" :todo "TODO")
;;           ))
;;   (setq org-super-agenda-header-map (make-sparse-keymap))
;;   (org-super-agenda-mode 1)
;;   )

(use-package eldoc
  :defer t
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (dart "https://github.com/UserNobody14/tree-sitter-dart")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (templ . ("https://github.com/vrischmann/tree-sitter-templ" "master" "src"))
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

  :preface
  (defun treesit-install-all-language-grammers ()
    "Build and install the tree-sitter language grammar libraries

for all languages configured in `treesit-language-source-alist'."
    (interactive)
    (dolist (source treesit-language-source-alist)
      (unless (treesit-ready-p (car source))
        (treesit-install-language-grammar (car source)))))
  )

(setq standard-indent 2)
(setq-default indent-tabs-mode nil) ;; Use only spaces
(setq-default tab-width 2) ;; Tab width 8 is too long
(setq tab-always-indent nil) ;; Hitting TAB behavior
(setq sentence-end-double-space nil) ;; Use single space between sentences
(setq require-final-newline t) ;; Always add final newline
(add-hook 'before-save-hook 'whitespace-cleanup) ;; Delete trailing whitespaces on save

(use-package apheleia
  :commands apheleia-mode
  :config
  (setf (alist-get 'prettier-typescript apheleia-formatters)
        '("apheleia-npx" "prettierd" "--stdin-filepath" filepath "--parser=typescript"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
  (setf (alist-get 'prettier apheleia-formatters)
        '("apheleia-npx" "prettierd" "--stdin-filepath" filepath
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
  (setf (alist-get 'erb-formatter apheleia-formatters)
        '("erb-format" "--print-width=140" filepath))

  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
        '(ruby-standard))

  (add-to-list 'apheleia-mode-alist '(erb-mode . erb-formatter))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier-markdown))
  )

(use-package editorconfig
  :defer .5
  :ensure nil
  :config
  (editorconfig-mode 1))

(setq xref-prompt-for-identifier nil)

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-install-server)
  :preface
  (defun +orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun +lsp-mode-setup-completion ()
    (progn
      (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq-local completion-at-point-functions
                  (list (cape-capf-super #'non-greedy-lsp #'yasnippet-capf))))

    (add-hook 'orderless-style-dispatchers #'+orderless-dispatch-flex-first nil 'local)
    )
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-completion-provider :none)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-auto-execute-action nil)
  (lsp-disabled-clients '(rubocop-ls semgrep-ls))
  (lsp-pylsp-plugins-ruff-enabled t)
  (lsp-clients-typescript-prefer-use-project-ts-server t)
  (lsp-typescript-suggest-complete-js-docs nil)
  (lsp-javascript-suggest-complete-js-docs nil)
  (lsp-javascript-implicit-project-config-check-js t)
  (lsp-treemacs-error-list-current-project-only t)
  (lsp-treemacs-error-list-expand-depth 3)
  :hook
  (lsp-completion-mode . +lsp-mode-setup-completion)
  (lsp-managed-mode . (lambda ()
                        (setq-local evil-lookup-func 'lsp-describe-thing-at-point)))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ))

  (add-to-list 'lsp-language-id-configuration '(templ-ts-mode . "templ"))

  (lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection '("templ" "lsp"))
                      :activation-fn (lsp-activate-on "templ")
                      :server-id 'templls))

  :general
  (leader-def
    "ca" 'lsp-execute-code-action
    "ci" 'lsp-find-implementation
    "co" 'lsp-organize-imports
    "ch" 'lsp-describe-thing-at-point
    "cH" 'lsp-describe-session
    "cr" 'lsp-rename
    "cQ" 'lsp-workspace-restart
    "cq" 'lsp-workspace-shutdown)
  )

(use-package consult-lsp
  :general
  (leader-def
    :keymaps 'lsp-mode-map
    "cj" 'consult-lsp-symbols
    "cx" 'consult-lsp-diagnostics))

(use-package flycheck
  :defer t
  :init
  (setq flycheck-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )

(use-package sideline
  :custom
  (sideline-backends-right '(sideline-flycheck))
  :hook
  (flycheck-mode . sideline-mode))

(use-package sideline-flycheck
  :hook
  (flycheck-mode . sideline-flycheck-setup))

;; (use-package flyover
;;   :hook
;;   (flycheck-mode . flyover-mode)
;;   :custom
;;   (flyover-hide-checker-name nil)
;;   (flyover-show-at-eol t)
;;   (flyover-virtual-line-type nil)
;;   (flyover-show-icon nil)
;;   )

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :mode ("go\\.mod\\'" . go-mod-ts-mode)
  :custom
  (go-ts-mode-indent-offset 4)
  :preface
  (defun +go-mode-setup ()
    (setq tab-width 4)
    (add-hook 'before-save-hook 'lsp-organize-imports nil t)
    (+add-pairs '((?` . ?`))))
  :hook
  (go-ts-mode . +go-mode-setup)
  (go-ts-mode . apheleia-mode)
  (go-ts-mode . lsp-deferred)
  )

(use-package templ-ts-mode
  :mode "\\.templ\\'"
  :preface
  (defun +templ-ts-mode-setup ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))
  :hook
  (templ-ts-mode . +templ-ts-mode-setup)
  (templ-ts-mode . lsp-deferred))

(use-package gotest
  :after go-ts-mode
  :general
  (localleader-def
    :keymaps 'go-ts-mode-map
    "b" '(:ignore t :wk "build")
    "br" 'go-run
    "t" '(:ignore t :wk "test")
    "ta" 'go-test-current-project
    "tb" 'go-test-current-benchmark
    "tf" 'go-test-current-file
    "tt" 'go-test-current-test)
  :custom
  (go-test-verbose t))

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :ensure nil
  :hook
  (rust-ts-mode . apheleia-mode)
  (rust-ts-mode . lsp-deferred)
  (lsp-managed-mode . (lambda () ;; format rust analyzer eldoc
                        (when (derived-mode-p 'rust-mode 'rust-ts-mode)
                          (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
                            (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
                                   (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
                                   (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                                                    ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                                                    (t nil)))
                                   (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
                                   (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                                                    ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                                                    (t (-first-item groups))))
                                   (sig (->> sig-group
                                             (--drop-while (s-equals? "```rust" it))
                                             (--take-while (not (s-equals? "```" it)))
                                             (--map (s-replace-regexp "//.*" "" it))
                                             (--map (s-trim it))
                                             (s-join " "))))
                              (lsp--render-element (concat "```rust\n" sig cmt "\n```")))))))
  ;; :config
  ;; switch to evil-mode after using Wrap with widget actions
  ;; (advice-add 'lsp--execute-code-action :around
  ;;             (defun +wrap-code-action-insert-mode (orig-fn &rest args)
  ;;               (let* ((first-arg (nth 0 args))
  ;;                      (result (apply orig-fn args))
  ;;                      (action-name (plist-get first-arg :title)))
  ;;                 (when (and (derived-mode-p 'rust-ts-mode)
  ;;                            (bound-and-true-p evil-mode)
  ;;                            (string-match "Generate" action-name))
  ;;                   (evil-insert-state))
  ;;                 result)))
  :general
  (localleader-def
    :keymaps 'rust-ts-mode-map
    "q" 'lsp-rust-analyzer-expand-macro
    "tr" 'lsp-rust-analyzer-related-tests)
  )

(use-package cargo-mode
  :hook
  (rust-ts-mode . cargo-minor-mode)
  :config
  ;;   (defun cargo-mode--start (name command project-root &optional prompt)
  ;;     "Start the `cargo-mode` process with NAME and return the created process.
  ;; Cargo command is COMMAND.
  ;; The command is started from directory PROJECT-ROOT.
  ;; If PROMPT is non-nil, modifies the command."
  ;;     (let* ((path-to-bin (shell-quote-argument (cargo-mode--find-bin)))
  ;;            (base-cmd (concat "cargo" " " command))
  ;;            (cmd (cargo-mode--maybe-add-additional-params base-cmd prompt))
  ;;            (default-directory (or project-root default-directory)))
  ;;       (save-some-buffers (not compilation-ask-about-save)
  ;;                          (lambda ()
  ;;                            (and project-root
  ;;                                 buffer-file-name
  ;;                                 (string-prefix-p project-root (file-truename buffer-file-name)))))
  ;;       (setq cargo-mode--last-command (list name cmd project-root))
  ;;       (compile cmd)))

  (defun cargo-mode-run (&optional prefix)
    "Run the `cargo run` command.
  If PREFIX is non-nil, prompt for additional params."
    (interactive "P")
    (let ((project-root (cargo-mode--project-directory)))
      (cargo-mode--start "execute" "run" project-root prefix)))

  :general
  (localleader-def
    :keymaps 'cargo-minor-mode-map
    "x" 'cargo-mode-execute-task
    "b" '(:ignore t :wk "build")
    "bb" 'cargo-mode-build
    "br" 'cargo-mode-run
    "t" '(:ignore t :wk "test")
    "ta" 'cargo-mode-test
    "tf" 'cargo-mode-test-current-buffer
    "tt" 'cargo-mode-test-current-test
    )
  )

(use-package css-ts-mode
  :mode "\\.css\\'"
  :ensure nil
  :custom
  (css-indent-offset 2)
  :hook
  (css-ts-mode . lsp-deferred)
  (css-ts-mode . apheleia-mode))

;; (use-package emmet-mode
;;   :preface (defvar emmet-mode-keymap (make-sparse-keymap))
;;   :custom
;;   (emmet-indentation 2)
;;   (emmet-indent-after-insert nil)
;;   :config
;;   (add-to-list 'emmet-jsx-major-modes 'jtsx-tsx-mode)
;;   (add-to-list 'emmet-jsx-major-modes 'jtsx-jsx-mode)
;;   :general-config
;;   (:states '(insert)
;;            :keymaps 'emmet-mode-keymap
;;            "<C-return>" 'emmet-expand-line)
;;   (:states '(visual)
;;            :keymaps 'emmet-mode-keymap
;;            "<C-return>" 'emmet-wrap-with-markup)
;;   :hook
;;   ((jtsx-tsx-mode jtsx-jsx-mode) . emmet-mode)
;;   (html-ts-mode . emmet-mode)
;;   (web-mode . emmet-mode))

(use-package jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :custom
  ;; (js-chain-indent t)
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  (jtsx-enable-jsx-element-tags-auto-sync t)
  (jtsx-indent-statement-block-regarding-standalone-parent t)
  ;; :preface
  ;; (defun +jsx-comment-or-uncomment-region (beg end)
  ;;   (cond
  ;;    ((jtsx-jsx-attribute-context-p)
  ;;     (let* ((comment-start "/* ")
  ;;            (comment-end " */")
  ;;            (comment-use-syntax nil)
  ;;            (comment-start-skip "\\(?:/\\*+\\)\\s-*")
  ;;            (comment-end-skip "\\s-*\\(\\*+/\\)"))
  ;;       (evilnc-comment-or-uncomment-region-internal beg end)))
  ;;    ((jtsx-jsx-context-p)
  ;;     (let* ((comment-start "{/* ")
  ;;            (comment-end " */}")
  ;;            (comment-use-syntax nil)
  ;;            (comment-start-skip "\\(?:{?/\\*+\\)\\s-*")
  ;;            (comment-end-skip "\\s-*\\(\\*+/}?\\)"))
  ;;       (evilnc-comment-or-uncomment-region-internal beg end)))
  ;;    (t (evilnc-comment-or-uncomment-region-internal beg end))))

  ;; (defun jsx-jump-item ()
  ;;   "Smart jumping using jtsx for JSX tags, evil-jump-item for braces and JS expressions."
  ;;   (interactive)
  ;;   (if (and (jtsx-jsx-context-p)
  ;;            (not (jtsx-js-nested-in-jsx-context-p))
  ;;            (not (memq (char-after) '(?{ ?})))
  ;;            (not (memq (char-before) '(?{ ?}))))
  ;;       (jtsx-jump-jsx-element-tag-dwim)
  ;;     (evil-jump-item)))
  :hook
  ;; ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . (lambda ()
  ;;                                                         (setq-local evilnc-comment-or-uncomment-region-function '+jsx-comment-or-uncomment-region)))
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . (lambda ()
                                                          (+add-pairs '((?` . ?`)))))
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . lsp-deferred)
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . apheleia-mode)
  ;; :general
  ;; (:states '(normal visual)
  ;;          :keymaps 'jtsx-tsx-mode-map
  ;;          "%" #'jsx-jump-item)
  )

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :init
  (setq web-mode-enable-html-entities-fontification t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-markup-comment-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq web-mode-auto-close-style 1)
  (setq web-mode-comment-style 2)

  (define-derived-mode erb-mode web-mode
    "Web[erb]")
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . erb-mode))
  :config
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'"))
  :hook
  (web-mode . apheleia-mode)
  )

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'")

(use-package pythontest
  :general
  (localleader-def
    :keymaps 'python-ts-mode-map
    "ta" 'pythontest-test-all
    "tf" 'pythontest-test-file
    "tt" 'pythontest-test-at-point))

(use-package pyvenv
  :init
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("venv:" pyvenv-virtual-env-name " ")))
  :hook
  ((python-mode python-ts-mode) . pyvenv-mode))

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  :hook
  (python-ts-mode . apheleia-mode)
  (python-ts-mode . lsp-deferred))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :ensure nil
  :hook
  (ruby-ts-mode . apheleia-mode)
  (ruby-ts-mode . lsp-deferred))

(use-package inf-ruby
  :hook (compilation-filter . inf-ruby-auto-enter)
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode)
  :init
  (setq inf-ruby-console-environment "development")
  :config
  (evil-set-initial-state 'inf-ruby-mode 'insert)
  :bind
  (:map inf-ruby-mode-map
        ("M-r" . consult-history)))

(use-package ruby-end
  :after (ruby-ts-mode))

;; (use-package rspec-mode
;;   :mode ("/\\.rspec\\'" . text-mode))

;; (defvar rails-command-prefix "bundle exec rails")

;; (defvar rails-generators
;;   '(("assets" (("app/assets/"
;;                 "app/assets/\\(?:stylesheets\\|javascripts\\)/\\(.+?\\)\\..+$")))
;;     ("controller" (("app/controllers/" "app/controllers/\\(.+\\)_controller\\.rb$")))
;;     ("generator" (("lib/generator/" "lib/generators/\\(.+\\)$")))
;;     ("helper" (("app/helpers/" "app/helpers/\\(.+\\)_helper.rb$")))
;;     ("integration_test" (("test/integration/" "test/integration/\\(.+\\)_test\\.rb$")))
;;     ("job" (("app/jobs/" "app/jobs/\\(.+\\)_job\\.rb$")))
;;     ("mailer" (("app/mailers/" "app/mailers/\\(.+\\)\\.rb$")))
;;     ("migration" (("db/migrate/" "db/migrate/[0-9]+_\\(.+\\)\\.rb$")))
;;     ("model" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
;;     ("resource" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
;;     ("scaffold" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
;;     ("task" (("lib/tasks/" "lib/tasks/\\(.+\\)\\.rake$")))))

;; (defun rails-generate ()
;;   "Execute Rails generate COMMAND with input completion."
;;   (interactive)
;;   (let ((default-directory (project-root (project-current t))))
;;     (async-shell-command (rails-command-with-completion " generate "))))

;; (defun rails-destroy ()
;;   "Execute Rails destroy COMMAND with input completion."
;;   (interactive)
;;   (let ((default-directory (project-root (project-current t))))
;;     (async-shell-command (rails-command-with-completion " destroy "))))

;; (defun rails-command-with-completion (command)
;;   "Build Rails command from COMMAND with input completion."
;;   (let ((keymap (copy-keymap minibuffer-local-map))
;;         (command-prefix (concat rails-command-prefix command)))
;;     (define-key keymap (kbd "<tab>") 'rails--completion-in-region)
;;     (concat command-prefix (read-from-minibuffer command-prefix nil keymap))))

;; (defun rails--completion-in-region ()
;;   "Apply Rails generators for text completion in region."
;;   (interactive)
;;   (let ((generators (--map (concat (car it) " ") rails-generators)))
;;     (when (<= (minibuffer-prompt-end) (point))
;;       (completion-in-region (minibuffer-prompt-end) (point-max)
;;                             generators))))

;; (defun rails-server ()
;;   "Run rails server command."
;;   (interactive)
;;   (let ((default-directory (project-root (project-current t))))
;;     (async-shell-command (concat rails-command-prefix " server"))))

;; (defun rails-console ()
;;   "Start a rails console at project root."
;;   (interactive)
;;   (inf-ruby-console-rails (project-root (project-current t))))

;; (defun project-find-file-in-dir (dir)
;;   "Visit a file (with completion) in the current project.
;; The filename at point (determined by `thing-at-point'), if any,
;; is available as part of \"future history\"."
;;   (interactive)
;;   (let* ((pr (project-current t))
;;          (dirs (list (expand-file-name dir (project-root pr)))))
;;     (project-find-file-in (thing-at-point 'filename) dirs pr)))

;; (defun rails-find-controller ()
;;   (interactive)
;;   (project-find-file-in-dir "app/controllers/"))

;; ;; refactor to macro?
;; (defun rails-find-model ()
;;   (interactive)
;;   (project-find-file-in-dir "app/models/"))

;; (defun rails-find-view ()
;;   (interactive)
;;   (project-find-file-in-dir "app/views/"))

;; (defun rails-find-helper ()
;;   (interactive)
;;   (project-find-file-in-dir "app/helpers/"))

;; (defun rails-find-test ()
;;   (interactive)
;;   (project-find-file-in-dir "app/tests/"))

;; (defun rails-find-javascript ()
;;   (interactive)
;;   (project-find-file-in-dir "app/javascript/"))

;; (defun rails-find-job ()
;;   (interactive)
;;   (project-find-file-in-dir "app/jobs/"))

;; (defun rails-find-mailer ()
;;   (interactive)
;;   (project-find-file-in-dir "app/mailers/"))

;; ;; non macro
;; (defun rails-find-spec ()
;;   (interactive)
;;   (project-find-file-in-dir "app/spec/"))

;; (defun rails-find-migration ()
;;   (interactive)
;;   (project-find-file-in-dir "db/migrate/"))

;; (defun rails-find-stylesheet ()
;;   (interactive)
;;   (project-find-file-in-dir "app/assets/stylesheets/"))

;; (defun rails-find-initializer ()
;;   (interactive)
;;   (project-find-file-in-dir "config/initializers/"))

;; (defun rails-find-locale ()
;;   (interactive)
;;   (project-find-file-in-dir "config/locales/"))

;; (defvar rails-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "a") 'rails-find-locale)
;;     (define-key map (kbd "b") 'rails-find-job)
;;     (define-key map (kbd "c") 'rails-find-controller)
;;     (define-key map (kbd "d") 'rails-destroy)
;;     (define-key map (kbd "g") 'rails-generate)
;;     (define-key map (kbd "h") 'rails-find-helper)
;;     (define-key map (kbd "i") 'rails-find-initializer)
;;     (define-key map (kbd "j") 'rails-find-javascript)
;;     (define-key map (kbd "m") 'rails-find-model)
;;     (define-key map (kbd "n") 'rails-find-migration)
;;     (define-key map (kbd "p") 'rails-find-spec)
;;     (define-key map (kbd "r") 'rails-console)
;;     (define-key map (kbd "R") 'rails-server)
;;     (define-key map (kbd "s") 'rails-find-stylesheet)
;;     (define-key map (kbd "t") 'rails-find-test)
;;     (define-key map (kbd "u") 'rails-find-fixture)
;;     (define-key map (kbd "v") 'rails-find-view)
;;     (define-key map (kbd "w") 'rails-find-component)
;;     (define-key map (kbd "@") 'rails-find-mailer)
;;     map)
;;   "Keymap after `rails-keymap-prefix'.")
;; (fset 'rails-command-map rails-command-map)

;; (use-package lsp-java
;;   :preface
;;   (defun +java-mode-setup ()
;;     (setq tab-width 4)
;;     (add-hook 'before-save-hook 'lsp-format-buffer nil t)
;;     (setq-local lsp-enable-snippet nil)
;;     )
;;   :init
;;   (setq lsp-java-save-actions-organize-imports t)
;;   (setq lsp-java-completion-max-results 10)
;;   (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
;;   :config
;;   (require 'lsp-java-boot)

;;   ;; workaround completions not showing up after .
;;   ;; (advice-add 'lsp-completion--looking-back-trigger-characterp :around
;;   ;;             (defun lsp-completion--looking-back-trigger-characterp@fix-dart-trigger-characters (orig-fn trigger-characters)
;;   ;;               (funcall orig-fn
;;   ;;                        (if (and (derived-mode-p 'java-ts-mode) (not trigger-characters))
;;   ;;                            ["."]
;;   ;;                          trigger-characters))))
;;   :hook
;;   (conf-javaprop-mode . lsp-deferred)
;;   ((java-mode java-ts-mode) . lsp-deferred)
;;   ((java-mode java-ts-mode) . +java-mode-setup))

(use-package dart-mode
  :ensure (:host github :repo "emacsorphanage/dart-mode")
  :mode "\\.dart\\'")

(use-package lsp-dart
  :preface
  (defun +dart-mode-setup ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))
  :hook
  (dart-mode . +dart-mode-setup)
  (dart-mode . lsp-deferred)
  :custom
  (lsp-dart-line-length 200)
  (lsp-dart-flutter-fringe-colors nil)
  (lsp-dart-flutter-widget-guides nil)
  (lsp-dart-main-code-lens nil)
  (lsp-dart-test-code-lens nil)
  ;; (lsp-dart-dap-flutter-hot-reload-on-save t)
  :config
  ;; (defun lsp-dart--run-command (command args)
  ;;   "Run COMMAND with ARGS from the project root."
  ;;   (lsp-dart-from-project-root
  ;;    (async-shell-command (format "%s %s" (string-join command " ") args) lsp-dart-commands-buffer-name)))

  ;; workaround for dart not returning completions after "."
  (advice-add 'lsp-completion--looking-back-trigger-characterp :around
              (defun lsp-completion--looking-back-trigger-characterp@fix-trigger-characters (orig-fn trigger-characters)
                (funcall orig-fn
                         (if (and (derived-mode-p '(dart-mode)) (not trigger-characters))
                             ["." "(" "$"]
                           trigger-characters))))

  ;; switch to evil-mode after using Wrap with widget actions
  ;; (advice-add 'lsp--execute-code-action :around
  ;;             (defun +wrap-code-action-insert-mode (orig-fn &rest args)
  ;;               (let* ((first-arg (nth 0 args))
  ;;                      (result (apply orig-fn args))
  ;;                      (action-name (plist-get first-arg :title)))
  ;;                 (when (and (derived-mode-p 'dart-mode)
  ;;                            (bound-and-true-p evil-mode)
  ;;                            (string= "Wrap with widget..." action-name))
  ;;                   (evil-insert-state))
  ;;                 result)))

  :general-config
  (localleader-def
    :keymaps 'dart-mode-map
    "p" 'lsp-dart-pub-get)
  )

(use-package flutter
  :preface
  (defun +flutter-hot-reload ()
    "Run `flutter-hot-reload' only if flutter-mode is running."
    (when (and (fboundp 'flutter--running-p) (flutter--running-p))
      (flutter-hot-reload)))

  (defun +flutter-mode-setup ()
    (add-hook 'after-save-hook '+flutter-hot-reload nil t))
  :hook
  (dart-mode . +flutter-mode-setup)
  :general
  (localleader-def
    :keymaps '(dart-mode-map flutter-mode-map)
    "fq" 'flutter-quit
    "fr" 'flutter-hot-reload
    "fR" 'flutter-hot-restart
    "ff" 'flutter-run-or-hot-reload

    "ta" 'flutter-test-all
    "tf" 'flutter-test-current-file
    "tt" 'flutter-test-at-point
    )
  )

(use-package feature-mode
  :mode ("/\\.feature\\'" . feature-mode)
  :config
  (require 'org-table))

(use-package lua-ts-mode
  :ensure nil
  :mode "\\.lua\\'")

(use-package csharp-ts-mode
  :ensure nil
  :mode "\\.cs\\'")

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . apheleia-mode)
  (emacs-lisp-mode . (lambda () (setq-local evil-lookup-func 'helpful-at-point)))
  )

(use-package eros
  :custom
  (eros-eval-result-prefix "⟹ ")
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package markdown-mode
  :ensure nil
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook
  (markdown-mode . apheleia-mode)
  :custom
  (markdown-command "multimarkdown")
  (markdown-asymmetric-header t)
  (markdown-fontify-whole-heading-line t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-mouse-follow-link nil)
  )

(use-package markdown-xwidget
  :after markdown-mode
  :ensure (markdown-xwidget
           :host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources"))
  :general
  (localleader-def
    :keymaps 'markdown-mode-map
    "x" 'markdown-xwidget-preview-mode)
  :custom
  (markdown-xwidget-github-theme "dark")
  (markdown-xwidget-mermaid-theme "dark")
  (markdown-xwidget-code-block-theme "dark")
  )

(use-package json-ts-mode
  :ensure nil
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :preface
  (defun +json-mode-setup ()
    (add-hook 'before-save-hook 'json-pretty-print-buffer t t))
  :hook
  (json-ts-mode . +json-mode-setup))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  )

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'"
  :hook
  (toml-ts-mode . lsp-deferred))

(use-package dockerfile-ts-mode
  :ensure nil
  :mode "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'"
  :hook
  (dockerfile-ts-mode . lsp-deferred))

(use-package bazel
  :mode ("\\Tiltfile\\'" . bazel-starlark-mode))

(use-package conf-mode
  :ensure nil
  :defer t
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :mode ("rc?\\'" . conf-mode))

(use-package jenkinsfile-mode
  :mode "Jenkinsfile.*\\'")

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package git-modes
  :defer t
  :mode
  ("\\.dockerignore\\'" . gitignore-mode))

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode))

(use-package comint
  :defer t
  :ensure nil
  :custom
  (comint-use-prompt-regexp t)
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048)
  :config
  (defun +comint-quit-if-done ()
    "Quit comint buffer only if the process is finished."
    (interactive)
    (let ((proc (get-buffer-process (current-buffer))))
      (if (or (not proc) (memq (process-status proc) '(exit signal)))
          (quit-window)
        (message "Process still running!"))))
  :general
  (:states '(normal)
           :keymaps 'comint-mode-map
           "q" #'+comint-quit-if-done)
  (:states '(normal visual)
           :keymaps 'comint-mode-map
           "<return>" #'evil-insert-at-process-mark)
  (:states '(insert)
           :keymaps 'comint-mode-map
           "C-y" #'yank)
  )

(use-package compile
  :defer t
  :ensure nil
  :preface
  (defun +compilation-buffer-name (compilation-mode)
    "Rename buffer to whatever command was used.
  eg. *compile: python main.py*"
    (format "*compile: %s*" compile-command))

  (defun +project-compilation-buffer-name (compilation-mode)
    "Meant to be used for `compilation-buffer-name-function`.
  Argument COMPILATION-MODE is the name of the major mode used for the
  compilation buffer."
    (concat (+compilation-buffer-name compilation-mode)
            (if (project-current) (concat "<" (project-name (project-current)) ">") "")))
  :init
  (setq compilation-buffer-name-function '+compilation-buffer-name)
  :custom
  (compile-command "make ")
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-max-output-line-length nil)
  (project-compilation-buffer-name-function '+project-compilation-buffer-name)
  :config
  (assq-delete-all 'compilation-in-progress mode-line-modes)

  (defun +compile-with-comint (orig-fun command &optional comint)
    "Advice for `compile' to always set COMINT to t."
    (funcall orig-fun command t))
  (advice-add 'compile :around #'+compile-with-comint)

  (defun +compile-finish-read-only (buffer string)
    (with-current-buffer buffer
      (setq-local buffer-read-only t)))

  (add-hook 'compilation-finish-functions '+compile-finish-read-only)
  :hook
  (compilation-filter . comint-truncate-buffer)
  (compilation-filter . ansi-color-compilation-filter)
  (shell-command-mode . compilation-shell-minor-mode)
  :bind
  ("M-o" . project-or-cwd-compile-from-history)
  ("M-O" . compile-in-dir)
  :general
  (leader-def
    "!" 'project-or-cwd-compile
    "pc" 'project-or-cwd-compile
    "pt" #'project-test-project
    "pg" #'project-configure-project
    "pR" #'project-run-project
    )
  )

(defvar project-compile-history-alist nil
  "Alist of project roots to their compile histories.")

(defun get-project-compile-history ()
  "Get compile history for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (history-key (or project-root default-directory)))
    (alist-get history-key project-compile-history-alist nil nil #'string=)))

(defun save-project-compile-history (history)
  "Save compile history for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (history-key (or project-root default-directory)))
    (setf (alist-get history-key project-compile-history-alist nil nil #'string=)
          history)))

(defun project-compile-read-command (orig-fun &rest args)
  "Advice to use project-specific compile history."
  (let ((compile-history (get-project-compile-history)))
    (prog1 (apply orig-fun args)
      (save-project-compile-history compile-history))))

(advice-add 'compilation-read-command :around #'project-compile-read-command)

(defun compile-region (start end)
  "Send region from START to END to `compile'and display the result."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (let ((cmd (string-trim (buffer-substring-no-properties start end))))
    (compile cmd)))

(defun project-or-cwd-compile-from-history ()
  "Run `compile' with a choice from compile history or project's compile history."
  (interactive)
  (let ((project (project-current)))
    (if project
        (project-compile-from-history)
      (compile-from-history))))

(defun compile-from-history ()
  "Run `compile' with a choice from compile history."
  (interactive)
  (let* ((initial-text (when (use-region-p)
                         (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (command (completing-read
                   (format-message "Compile command in `%s': " (abbreviate-file-name default-directory))
                   compile-history nil nil initial-text 'compile-history)))
    (when (and command (not (string-empty-p command)))
      (compile command))))

(defun project-compile-from-history ()
  "Run `compile' with a choice from project's compile history."
  (interactive)
  (let* ((project-history (get-project-compile-history))
         (project (project-current))
         (compile-dir (when project (project-root project)))
         (compilation-buffer-name-function project-compilation-buffer-name-function)
         (initial-text (when (use-region-p)
                         (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (command (completing-read
                   (format-message "Compile command in `%s': " (abbreviate-file-name compile-dir))
                   (or project-history '()) nil nil initial-text 'compile-history)))
    (when (and command (not (string-empty-p command)))
      (compile-in-dir compile-dir command)
      (let ((updated-history (cons command (remove command (or project-history '())))))
        (save-project-compile-history updated-history)))))

(defun compile-in-dir (dir &optional command)
  "Run `compile' in the selected directory."
  (interactive "DCompile in: ")
  (let ((default-directory dir))
    (if command
        (compile command)
      (call-interactively #'compile))))

(defun project-or-cwd-compile (&optional command)
  "Run `compile' in the current project's root directory or current working directory."
  (interactive)
  (let* ((project-history (get-project-compile-history))
         (project (project-current))
         (compilation-buffer-name-function (if project project-compilation-buffer-name-function compilation-buffer-name-function))
         (compile-dir (if project (project-root project) default-directory)))
    (compile-in-dir compile-dir command)
    (when project
      (let ((updated-history (cons command (remove command (or project-history '())))))
        (save-project-compile-history updated-history))))
  )

(defmacro define-project-command (name variable docstring)
  "Define a function to run a preconfigured command in project root.
NAME is the function name suffix (e.g., 'test' for project-test-compile).
VARIABLE is the variable name containing the command.
DOCSTRING describes what the command does."
  (let ((func-name (intern (format "project-%s-project" name))))
    `(progn
       ;;;###autoload
       (defun ,func-name ()
         ,(format "Run the preconfigured %s command in project root." docstring)
         (interactive)
         (if-let ((project (project-current)))
             (let ((default-directory (project-root project))
                   (command (and (boundp ',variable)
                                 ,variable)))
               (if command
                   (compile command)
                 (user-error "No %s command configured" ,docstring)))
           (user-error "No project found"))))))

(define-project-command test project-test-command "test")
(define-project-command run project-run-command "run")
(define-project-command configure project-configure-command "configure")

(use-package fish-completion
  :if (executable-find "fish")
  :custom
  (fish-completion-fallback-on-bash-p t)
  (fish-completion-inhibit-missing-fish-command-warning t)
  :hook
  (minibuffer-setup . fish-completion-mode)
  (eshell-mode . fish-completion-mode))

;; (use-package shell-command-pro
;;   :load-path "~/code/shell-command-pro"
;;   :commands (compile-in-dir compile-from-history
;;                             async-shell-command-in-dir async-shell-command-from-history
;;                             project-or-cwd-async-shell-command project-or-cwd-async-shell-command-from-history
;;                             project-run-project project-or-cwd-compile project-or-cwd-compile-from-history)
;;   :preface
;;   (defun +project-or-cwd-default-directory ()
;;     (let ((project (project-current)))
;;       (if project
;;           (project-root (project-current t))
;;         default-directory)))

;;   (defun project-or-cwd-compile (&optional command)
;;     "Run `compile' in the current project's root directory."
;;     (declare (interactive-only compile))
;;     (interactive)
;;     (compile-in-dir (+project-or-cwd-default-directory) command))

;;   (defun project-or-cwd-compile-from-history ()
;;     "Run `compile' with a choice from its command history in
;; current project's root directory."
;;     (interactive)
;;     (let ((default-directory (+project-or-cwd-default-directory)))
;;       (call-interactively #'compile-from-history)))

;;   (defun project-or-cwd-async-shell-command (&optional command)
;;     "Run `async-shell-command' in the current project's root directory or in the current directory."
;;     (declare (interactive-only async-shell-command))
;;     (interactive)
;;     (async-shell-command-in-dir (+project-or-cwd-default-directory) command))

;;   (defun project-or-cwd-async-shell-command-from-history ()
;;     "Run `async-shell-command' with a choice from its command history in
;; current project's root directory."
;;     (interactive)
;;     (let ((default-directory (+project-or-cwd-default-directory)))
;;       (call-interactively #'async-shell-command-from-history)))

;;   :bind
;;   ("M-r" . project-or-cwd-async-shell-command-from-history)
;;   ("M-o" . project-or-cwd-compile-from-history)
;;   ("M-O" . project-or-cwd-compile)
;;   ("M-*" . compile-in-dir)
;;   :general
;;   (leader-def
;;     "p!" 'project-or-cwd-async-shell-command-from-history
;;     "pc" 'project-or-cwd-compile-from-history)
;;   )

(use-package shell
  :ensure nil
  :bind
  ([remap shell-command-on-region] . async-shell-command-region)
  :init
  (setq async-shell-command-display-buffer nil)
  (setq async-shell-command-buffer 'new-buffer)
  (setq shell-command-prompt-show-cwd t)
  :preface
  (defun async-shell-command-region (start end)
    "Send region from START to END to `async-shell-command'and display the result."
    (interactive "r")
    (unless (region-active-p)
      (user-error "No region"))
    (let ((cmd (string-trim (buffer-substring-no-properties start end))))
      (async-shell-command cmd)))
  )

(use-package shell-command-x
  :after shell
  :config
  (setq shell-command-x-buffer-name-async-format "*shell %p:%a*")
  (setq shell-command-x-buffer-name-format "*shell %p:%a*")
  (shell-command-x-mode 1))

(use-package eshell
  :ensure nil
  :commands (eshell eshell-new)
  :general
  (leader-def
    "oE" 'eshell-new
    "oe" 'eshell)
  (:states '(normal visual)
           :keymaps 'eshell-mode-map
           "<return>" #'+eshell-reset-cursor-insert)
  (:states '(normal insert visual)
           :keymaps 'eshell-mode-map
           "C-t" #'+shell-interactive-cd)
  (:states '(normal visual insert)
           :keymaps 'eshell-mode-map
           "M-r" #'consult-history)
  (:states '(insert)
           :keymaps 'eshell-mode-map
           "C-y" #'yank)
  :preface
  (defun +eshell-reset-cursor-insert ()
    (interactive)
    ;; Move to end of buffer and ensure we're at prompt
    (end-of-buffer)
    (when (get-buffer-process (current-buffer))
      (while (not (looking-back eshell-prompt-regexp (line-beginning-position)))
        (comint-send-input)))
    (evil-insert-state))

  (defun +shell-interactive-cd (dir)
    "Prompt for a directory and cd to it."
    (interactive "Dcd ")
    (let ((inhibit-read-only t))
      (insert (concat "cd " dir)))
    (pcase major-mode
      ('shell-mode (comint-send-input))
      ('eshell-mode (eshell-send-input)))
    )

  (defun eshell-new ()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))
  :custom
  (eshell-banner-message "
\x1b[32m                             'c.                    \x1b[0m
\x1b[32m                          ,xNMM.                    \x1b[0m
\x1b[32m                        .OMMMMo                     \x1b[0m
\x1b[32m                        OMMM0,                      \x1b[0m
\x1b[32m              .;loddo:' loolloddol;.                \x1b[0m
\x1b[32m            cKMMMMMMMMMMNWMMMMMMMMMM0:              \x1b[0m
\x1b[33m          .KMMMMMMMMMMMMMMMMMMMMMMMWd.              \x1b[0m
\x1b[33m          XMMMMMMMMMMMMMMMMMMMMMMMX.                \x1b[0m
\x1b[31m        ;MMMMMMMMMMMMMMMMMMMMMMMM:                  \x1b[0m
\x1b[31m        :MMMMMMMMMMMMMMMMMMMMMMMM:                  \x1b[0m
\x1b[31m        .MMMMMMMMMMMMMMMMMMMMMMMMX.                 \x1b[0m
\x1b[31m         kMMMMMMMMMMMMMMMMMMMMMMMMWd.               \x1b[0m
\x1b[35m          .XMMMMMMMMMMMMMMMMMMMMMMMMMMk             \x1b[0m
\x1b[35m           .XMMMMMMMMMMMMMMMMMMMMMMMMK.             \x1b[0m
\x1b[34m             kMMMMMMMMMMMMMMMMMMMMMMd               \x1b[0m
\x1b[34m              ;KMMMMMMMWXXWMMMMMMMk.                \x1b[0m
\x1b[34m                .cooc,.    .,coo:.                  \x1b[0m

\x1b[34m                        _/                  _/  _/  \x1b[0m
\x1b[34m     _/_/      _/_/_/  _/_/_/      _/_/    _/  _/   \x1b[0m
\x1b[34m  _/_/_/_/  _/_/      _/    _/  _/_/_/_/  _/  _/    \x1b[0m
\x1b[34m _/            _/_/  _/    _/  _/        _/  _/     \x1b[0m
\x1b[34m  _/_/_/  _/_/_/    _/    _/    _/_/_/  _/  _/      \x1b[0m

")
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-history-size (* 10 1024))
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  :hook
  (eshell-mode . goto-address-mode)
  )

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  :preface
  (defun project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root     (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
        (vterm))))

  (defun vterm-reset-cursor-insert ()
    (interactive)
    (vterm-reset-cursor-point)
    (evil-insert-state))
  :general
  (leader-def
    "os" 'vterm
    "ps" 'project-vterm)
  (:states '(normal visual)
           :keymaps 'vterm-mode-map
           "<return>" #'vterm-reset-cursor-insert)
  (:states '(insert)
           :keymaps 'vterm-mode-map
           "C-y" #'vterm-yank)
  )

(use-package ielm
  :commands (ielm)
  :ensure nil
  :general-config
  (:states '(insert)
           :keymaps 'inferior-emacs-lisp-mode-map
           "C-y" #'yank)
  )

(use-package envrc
  :init
  (envrc-global-mode 1))

(use-package ediff
  :ensure nil
  :defer t
  :init
  ;; These need to be set before ediff loads
  (setq ediff-diff-options "-w"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar +ediff-saved-wconf nil)
  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (setq +ediff-saved-wconf (current-window-configuration))))
  (defun +ediff-restore-wconf-h ()
    (when (window-configuration-p +ediff-saved-wconf)
      (set-window-configuration +ediff-saved-wconf)))
  (add-hook 'ediff-quit-hook '+ediff-restore-wconf-h)
  (add-hook 'ediff-suspend-hook '+ediff-restore-wconf-h))

(use-package isearch
  :ensure nil
  :config
  (setq search-whitespace-regexp ".*?")
  (setq isearch-allow-scroll 'unlimited)
  (setq isearch-lazy-count t)
  (setq isearch-repeat-on-direction-change t)
  (setq isearch-wrap-pause 'no)

  (defvar isearch-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'isearch-repeat-forward)
      (define-key map "r" 'isearch-repeat-backward)
      map))
  (put 'isearch-repeat-forward  'repeat-map 'isearch-repeat-map)
  (put 'isearch-repeat-backward 'repeat-map 'isearch-repeat-map)
  )

;; use selection to search
(defun +isearch-default-selected-text (orig-fun forward &optional regexp op-fun recursive-edit word-p)
  "Advice for `isearch-mode' to use selected text as default search string."
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        (funcall orig-fun forward regexp op-fun recursive-edit word-p)
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    (funcall orig-fun forward regexp op-fun recursive-edit word-p)))

(advice-add 'isearch-mode :around #'+isearch-default-selected-text)

(use-package rg
  :commands (rg rg-project rg-dwim-project-dir)
  :general
  (leader-def
    "sg" 'rg-project
    "sG" 'rg-dwim-project-dir)
  )

(use-package link-hint
  :commands (link-hint-open-link)
  :general
  (leader-def
    "oL" 'link-hint-open-link))

;; (use-package verb
;;   :after org
;;   :custom
;;   (verb-auto-kill-response-buffers t)
;;   (verb-json-use-mode 'json-ts-mode)
;;   :config
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                (append org-babel-load-languages
;;                                        '((verb     . t))))
;;   (add-to-list 'org-structure-template-alist '("vb" . "src verb :wrap src ob-verb-response :op send get-body"))
;;   :general-config
;;   (leader-def
;;    :keymaps 'org-mode-map
;;    "vh" 'verb-show-vars
;;    "vs" 'verb-set-var
;;    "vu" 'verb-unset-var
;;    "vv" 'verb-send-request-on-point-other-window-stay)
;;   )

(use-package dir-config
  :custom
  (dir-config-allowed-directories '("~/code"))
  :hook
  (elpaca-after-init . dir-config-mode))

(use-package jwt
  :commands (jwt-decode jwt-decode-at-point jwt-decode-region))

(use-package agent-shell
  :preface
  (defun +agent-shell-buffer-side-window (buffer alist)
    "Place the agent-shell buffer in the side window."
    (display-buffer-in-side-window
     buffer '((side . right)
              (slot . 0)
              (dedicated . t)
              (window-width . 0.45)
              (window-parameters . (no-delete-other-windows . t))))
    )
  :config
  (setq agent-shell-qwen-authentication (agent-shell-qwen-make-authentication :login t))
  (setq agent-shell-display-action '(+agent-shell-buffer-side-window))
  :general
  (leader-def
    "ag" 'agent-shell)
  )
