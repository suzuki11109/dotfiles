;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)) ; Emacs 28 with native compilation

;; Do not wast time checking the modification time of each file
(setq load-prefer-newer t)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-install-upgrade-built-in nil)

(setq use-package-enable-imenu-support t)

(setq use-package-always-ensure t)

(setq package-vc-register-as-project nil) ; Emacs 30

(eval-when-compile
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

(use-package on
  :vc (:fetcher github :repo ajgrf/on.el))

;; Some constants
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))

(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer +leader-def
    :states '(visual normal motion)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer +local-leader-def
    :states '(visual normal motion)
    :keymaps 'local
    :prefix "SPC m")

  (+leader-def
    "SPC" '(execute-extended-command :wk "M-x")
    ":"   '(pp-eval-expression :wk "Eval expression")
    "X"   #'org-capture
    "u"   '(universal-argument :wk "C-u")
    "!"   #'project-or-cwd-async-shell-command
    "|"   #'async-shell-command-region

    "b"   '(nil :wk "buffer")
    "bb"  '(switch-to-buffer :wk "Switch buffer")
    "bd"  '(kill-this-buffer :wk "Kill this buffer")
    "bD"  '(kill-buffer :wk "Kill buffer")
    "bi"  #'ibuffer
    "bo"  '(switch-to-buffer-other-window :wk "Switch buffer other window")
    "bs"  '(save-buffer :wk "Save file")
    "bS"  '(save-some-buffers :wk "Save buffers")
    "br"  '(revert-buffer :wk "Revert buffer")
    "bR"  '(rename-buffer :wk "Rename buffer")
    "bx"  '(scratch-buffer :wk "Switch to scratch")
    "bz"  '(bury-buffer :wk "Bury buffer")

    "c"  '(nil :wk "code")
    "cc" '(compile :wk "Compile")
    "cC" '(recompile :wk "Recompile")
    "cd" '(xref-find-definitions :wk "Go to definitions")
    "cD" '(xref-find-definitions-other-windows :wk "Go to definitions other window")

    "f"   '(nil :wk "file")
    "fd"  #'dired
    "fD"  '(+delete-this-file :wk "Delete this file")
    "fe"  '((lambda () (interactive)
              (let ((default-directory user-emacs-directory))
                (call-interactively 'find-file))) :wk "Find in emacs config")
    "ff"  '(find-file :wk "Find file")
    "fg"  '((lambda () (interactive) (find-file "~/.gitconfig")) :wk "Edit .gitconfig")
    "fh"  '((lambda () (interactive)
              (let ((default-directory "~/"))
                (call-interactively 'find-file))) :wk "Find in home")
    "fi"  '((lambda () (interactive) (find-file (expand-file-name "init.org" user-emacs-directory))) :wk "Edit init.org")
    "fl"  #'locate
    "fr"  '(recentf :wk "Recent files")
    "fR"  '(+rename-this-file :wk "Rename/move file")
    "fs"  '(save-buffer :wk "Save file")
    "fS"  '(write-file :wk "Save as ...")
    "fy"  '((lambda () (interactive) (kill-new (buffer-file-name)) (message "Copied %s to clipboard" (buffer-file-name))) :wk "Yank buffer file name")
    "fz"  '((lambda () (interactive) (find-file "~/.zshrc")) :wk "Edit zsh config")

    "g"   '(nil :wk "git")

    "h" '(nil :wk "help")
    "hb" #'about-emacs
    "he" #'view-echo-area-message
    "hg" #'general-describe-keybindings
    "hi" #'info
    "hI" #'info-display-manual
    "hm" #'describe-mode
    "hp" #'describe-package
    "h'" #'describe-char

    "i"   '(nil :wk "insert")
    "iu"  '(insert-char :wk "Unicode char")
    "ie"  `(,(when (>= emacs-major-version 29) #'emoji-search) :wk "Emoji")
    "iy"  '(yank-pop :wk "From clipboard")

    "k"  '(nil :wk "bookmark")
    "ks"  #'bookmark-set
    "kk"  #'bookmark-jump
    "kl"  #'list-bookmarks
    "kd"  #'bookmark-delete

    ;; "l"  '(nil :wk "package")

    "m"   '(nil :wk "mode-specific")

    "n"   '(nil :wk "notes")
    "na"  #'org-agenda
    "nf"  '((lambda () (interactive)
              (let ((default-directory org-directory))
                (call-interactively 'find-file))) :wk "Find notes")
    "nm" #'org-tags-view
    "nt" #'org-todo-list

    "o"   '(nil   :wk "app/open")
    "oa"  #'org-agenda
    "of"  #'make-frame
    "oF"  #'select-frame-by-name
    "ol"  #'browse-url
    "o-"  #'dired-jump

    "p"   '(nil :wk "project")

    "q"   '(nil :wk "quit/session")
    "qf"  '(delete-frame :wk "Delete this frame")
    "qq"  '(save-buffers-kill-terminal :wk "Quit emacs")
    "qR"  '(restart-emacs :wk "Restart emacs")

    ;;; <leader> r --- remote

    "s"   '(nil :wk "search")
    "si" #'imenu
    "st" #'dictionary-lookup-definition
    "sT" #'dictionary

    "t"   '(nil :wk "toggle")
    "tc" '(global-display-fill-column-indicator-mode :wk "Fill column indicator")
    "tf"  '(toggle-frame-fullscreen :wk "Frame fullscreen")
    "th"  '(load-theme :wk "Load theme")
    "tr"  '(read-only-mode :wk "Read-only mode")
    )
  )

;; Escape once
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :custom
  (which-key-ellipsis "..")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-add-column-padding 1)
  :hook
  (on-first-input . which-key-mode))

(use-package gcmh
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024))
  :hook
  (on-first-buffer . gcmh-mode))

(use-package recentf
  :ensure nil
  :init
  (setq
   recentf-max-saved-items 100
   recentf-case-fold-search t
   recentf-exclude
   `(,(rx (* any)
          (or
           "elfeed-db"
           "eln-cache"
           "/cache/"
           ".maildir/"
           ".cache/")
          (* any)
          (? (or "html" "pdf" "tex" "epub")))
     ,(rx "/"
          (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
          (* any))))
  (recentf-mode 1))

;; Move stuff to trash
(setq delete-by-moving-to-trash t)

;; Better unique buffer names for files with the same base name.
(setq uniquify-buffer-name-style 'forward)

(setq
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Disable making backup files
 make-backup-files nil)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (expand-file-name "auto-save/" user-emacs-directory)
      tramp-auto-save-directory  (expand-file-name "tramp-auto-save/" user-emacs-directory)
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Auto load files changed on disk
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  :hook
  (on-first-file . global-auto-revert-mode))

;;  funtions put to custom lisp file
(defun +delete-this-file (&optional forever)
  "Delete the file associated with `current-buffer'.
If FOREVER is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let ((file (or (buffer-file-name)
                       (user-error "Current buffer is not visiting a file")))
             ((y-or-n-p "Delete this file? ")))
    (delete-file file (not forever))
    (kill-buffer (current-buffer))))

(defun +rename-this-file ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Automatically make script executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Guess the major mode after saving a file in `fundamental-mode' (adapted
;; from Doom Emacs).
(add-hook
 'after-save-hook
 (defun +save--guess-file-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.
    e.g. A shebang line or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window)))
            (set-auto-mode))))))

;; Better handling for files with so long lines
(use-package so-long
  :ensure nil
  :hook
  (on-first-file . global-so-long-mode))

;; Saving multiple files saves only in sub-directories of current project
(setq save-some-buffers-default-predicate #'save-some-buffers-root)

(setq
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t)

;; suppress large file opening confirmation
(setq large-file-warning-threshold nil)

(defun bury-or-kill ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil) t))
(add-hook 'kill-buffer-query-functions #'bury-or-kill)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package dired
  :ensure nil
  :commands dired
  :custom
  (dired-listing-switches "-ahl")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
)

(use-package dired-aux
  :ensure nil
  :after dired
  :custom
  (dired-create-destination-dirs 'always)
  (dired-do-revert-buffer t)
  (dired-vc-rename-file t))

;; Dired fontlock
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package project
  :ensure nil
  :demand t
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :config
  (setq project-switch-commands 'project-find-file)
  ;; (project-forget-zombie-projects) ;; really need to this to make tabspaces works
  :general
  (+leader-def
    "p" '(:keymap project-prefix-map :wk "project")
    "p!" #'project-async-shell-command
    ))

(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0.6)
;; (global-eldoc-mode 1)

(setq help-window-select t)
(use-package helpful
  :hook
  (emacs-lisp-mode . (lambda () (setq-local evil-lookup-func 'helpful-at-point)))
  :bind
  ([remap describe-command]  . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :preface
  (defun +helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

  The logic is simple, if we are currently in the helpful buffer,
  reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name)))
  :custom
  (helpful-switch-buffer-function #'+helpful-switch-to-buffer)
  (helpful-max-buffers 1)
  :config
  (define-key helpful-mode-map [remap quit-window]
              'kill-buffer-and-window)
  (define-key help-mode-map [remap quit-window]
              'kill-buffer-and-window)
  :general
  (+leader-def
    :infix "h"
    "a" #'describe-face
    "c" #'helpful-macro
    "f" #'helpful-callable
    "F" #'helpful-function
    "k" #'helpful-key
    "o" #'helpful-symbol
    "v" #'helpful-variable
    "x" #'helpful-command))

(use-package catppuccin-theme
  :init
  (setq catppuccin-height-title-3 1.1)
  (load-theme 'catppuccin t))

;; Set default fonts
(set-face-attribute 'default nil :font "monospace" :height 100)
(set-face-attribute 'variable-pitch nil :family "Inter" :height 1.1)
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 0.9)
;; Set thai font
(set-fontset-font t 'thai "SF Thonburi")
(set-fontset-font t 'thai (font-spec :script 'thai) nil 'append)

;; Font scaling
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease)
  :general
  ("M--" 'default-text-scale-decrease)
  ("M-=" 'default-text-scale-increase))

;; Font icons
(use-package nerd-icons
  :demand t
  :general
  (+leader-def
    "in" '(nerd-icons-insert :wk "Nerd icons"))
  :custom
  (nerd-icons-scale-factor 1.0))

;; Stretch cursor to the glyph width
(setq x-stretch-cursor t)
;; Remove visual indicators from non selected windows
(setq-default cursor-in-non-selected-windows nil)
;; No blinking cursor
(blink-cursor-mode -1)
;; Remember cursor position in files
(use-package saveplace
  :ensure nil
  :hook
  (on-first-file . save-place-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-widen t)
  :init
  (dolist (mode '(org-mode-hook markdown-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(setq
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Do not adjust window-vscroll to view tall lines. Fixes some lag issues
 auto-window-vscroll nil
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 scroll-conservatively 101
 ;; Scroll at a margin
 scroll-margin 3)

;; Horizontal scrolling
(setq hscroll-step 1
      hscroll-margin 2)

;; Fluid scrolling
(setq pixel-scroll-precision-use-momentum t)
(pixel-scroll-precision-mode 1)

;; Show current key-sequence in minibuffer
(setq echo-keystrokes 0.02)

;; Show recursion depth in minibuffer
(minibuffer-depth-indicate-mode 1)

;; Enable recursive calls to minibuffer
(setq enable-recursive-minibuffers t)

;; Use y or n instead of yes or no
(setq use-short-answers t)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable saving minibuffer history
(use-package savehist
  :ensure nil
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring register-alist search-ring regexp-search-ring))
  :config
  (setq history-delete-duplicates t)
  (savehist-mode)
)

;; Show line, columns number in modeline
(size-indication-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 0)
  (doom-modeline-buffer-file-name-style 'buffer)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-modal nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding 'nondefault)
  :hook
  (after-init . doom-modeline-mode))

;; Show search count in modeline
(use-package anzu
  :after (evil)
  :config
  (global-anzu-mode 1))

(use-package evil-anzu
  :after (evil anzu))

;; New frame initial buffer
;; (defun +set-frame-scratch-buffer (frame)
;;   (with-selected-frame frame
;;     (switch-to-buffer "*scratch*")))
;; (add-hook 'after-make-frame-functions #'+set-frame-scratch-buffer)

(use-package tab-bar
  :ensure nil
  :after (project)
  :custom
  (tab-bar-show t)
  (tab-bar-close-button nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-tab-select 'recent)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-button nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-tabs
                    +tab-bar-suffix
                    tab-bar-format-add-tab))
  (tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)
  :config
  (defun +tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat
        (propertize " " 'display '(space :width (8)))
        (alist-get 'name tab)
        (or (and tab-bar-close-button-show
                 (not (eq tab-bar-close-button-show
                          (if current-p 'non-selected 'selected)))
                 tab-bar-close-button)
            "")
        (propertize " " 'display '(space :width (8))))
       'face (funcall tab-bar-tab-face-function tab))))
  (defun +tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " ")
  )

(use-package tabspaces
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "home")
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-keymap-prefix nil)
  (tabspaces-initialize-project-with-todo nil)
  :general
  (+leader-def
    "<tab>" '(:keymap tabspaces-command-map :wk "workspaces")
    "<tab><tab>" #'tab-bar-switch-to-tab
    "<tab>n" #'tab-bar-switch-to-next-tab
    "<tab>p" #'tab-bar-switch-to-prev-tab)
  (+leader-def
    "pp" #'tabspaces-open-or-create-project-and-workspace)
  :init
  (tabspaces-mode 1)
  (tab-bar-rename-tab tabspaces-default-tab)

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)

    (defvar consult--source-workspace
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
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  )

;; Frame title
(setq frame-title-format
      (list
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
       '(:eval
         (let ((project (project-current)))
           (when project
             (format " — %s" (project-name project)))))))

;; Resize a frame by pixel
(setq frame-resize-pixelwise t)

;; Always prompt in minibuffer (no GUI)
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Confirm before quitting
(setq confirm-kill-emacs #'y-or-n-p)

;; No beep or blink
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Window layout undo/redo
(winner-mode 1)

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-dispatch-always t)
  (aw-minibuffer-flag t))

(use-package popper
  :general
  ("C-`" 'popper-toggle)
  ("C-\\"  'popper-cycle)
  ("C-~" 'popper-toggle-type)
  :init
  (setq popper-window-height 0.40)
  (setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
    '("\\*Messages\\*"
      "\\*Warnings\\*"
      "Output\\*$"
      "\\*Async Shell Command\\*$"
      compilation-mode
      "\\*Go Test\\*$"
      "\\*eshell\\*"
      "-eshell\\*$"
      ;; eshell-mode
      "\\*shell\\*"
      shell-mode
      "\\*term\\*"
      term-mode
      "-eat\\*$"
      "\\*eat\\*"
      ;; eat-mode
      "\\*rake-compilation\\*"
      "\\*rspec-compilation\\*"
      "\\*Flymake "
      "\\*Flycheck errors\\*"
      "\\*Org Select\\*"
      help-mode
      lsp-help-mode
      helpful-mode
      "\\*Org Select\\*"
      "\\*Capture\\*"
      "^CAPTURE-"
      "\\*xref\\*"
      "\\*eldoc\\*"
      "\\magit-process:"
      inf-ruby-mode
      sbt-mode
      deadgrep-mode
      forge-post-mode
      ))
  :hook
  (after-init . popper-mode)
  (after-init . popper-echo-mode)
)

(use-package transient
  :ensure nil
  :defer t
  :config
  ;; Map ESC and q to quit transient
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one))

(use-package paren
  :ensure nil
  :hook
  (on-first-buffer . show-paren-mode)
  :init
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :hook
  ((prog-mode text-mode conf-mode) . hl-todo-mode))

(use-package hotfuzz
  :custom
  (completion-ignore-case t)
  (completion-styles '(basic substring initials hotfuzz))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion))))) ;; add remote
)

(use-package consult
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
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
  :general
  (+leader-def
    "sb"  #'consult-line
    "sB"  #'consult-line-multi
    "sf"  #'consult-find
    "sp"  #'consult-ripgrep
    "hI"  #'consult-info)
  :bind
  (:map minibuffer-local-map
        ("M-r" . consult-history))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :init
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  )

(use-package embark
  :commands (embark-act embark-dwim)
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
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (general-define-key
   :keymaps 'embark-file-map
   "o" (+embark-ace-action find-file))
  (general-define-key
   :keymaps 'embark-buffer-map
   "o" (+embark-ace-action switch-to-buffer))
  (general-define-key
   :keymaps 'embark-general-map
   "D" #'xref-find-definitions-other-window)
  :bind
  ("C-." . embark-dwim)
  ("C-;" . embark-act))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :after vertico
  :custom
  (setq marginalia-align 'right)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package vertico
  :init
  (setq vertico-resize nil
        vertico-count 16)
  (setq read-extended-command-predicate
	    #'command-completion-default-include-p)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :general
  (+leader-def
    "." '(vertico-repeat-select :wk "Resume previous search"))
  :hook
  (on-first-input . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

;; Why use anything but UTF-8?
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

;; Use only spaces
(setq-default indent-tabs-mode nil)
;; Tab width 8 is too long
(setq-default tab-width 4)
;; Delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Use single space between sentences
(setq sentence-end-double-space nil)
;; Always add final newline
(setq require-final-newline t)

;; lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
;; Wrap long lines
(global-visual-line-mode 1)

(setq kill-do-not-save-duplicates t
      ;; Save existing clipboard text into the kill ring before replacing it.
      save-interprogram-paste-before-kill t)

(use-package evil
  :defer .3
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-v$-excludes-newline t)
  (evil-mode-line-format nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-symbol-word-search t)
  :general
  (+leader-def
    "w" '(:keymap evil-window-map :wk "window"))
  (:states 'motion
           "j" 'evil-next-visual-line
           "k" 'evil-previous-visual-line
           ";" 'evil-ex)
  (:states '(normal visual)
           "$" 'evil-end-of-line)
  :config
  (modify-syntax-entry ?_ "w")
  (defalias 'forward-evil-word 'forward-evil-symbol)
  (setq evil-visual-state-cursor '(hollow))
  (customize-set-variable 'evil-want-Y-yank-to-eol t) ;; :custom doesn't work

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'shell-mode 'normal)

  (evil-set-undo-system 'undo-fu)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil magit forge
  :custom
  (evil-collection-key-blacklist '("C-y"))
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-operator
  :general
  (:states '(normal visual)
           "gc" #'evilnc-comment-operator))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(eshell-mode shell-mode eat-mode)
        evil-escape-delay 0.25
        evil-escape-key-sequence "kj"))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-enable-delete nil)
  (setq evil-goggles-enable-change nil)
  (setq evil-goggles-enable-nerd-commenter nil)
  (evil-goggles-mode 1))

(use-package avy
  :commands evil-avy-goto-char-2
  :general
  (:states '(normal)
           "s" #'evil-avy-goto-char-2)
  :custom
  (avy-background t))

(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-skip-whitespace nil)
  :hook
  ((prog-mode text-mode conf-mode) . electric-pair-mode)
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  :preface
  (defun +add-pairs (pairs)
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))

(use-package lispyville
  :after evil
  :config
  (setq lispy-safe-paste nil)
  (lispyville-set-key-theme '(operators
                              c-w
                              commentary
                              (atom-motions t)
                              (additional-insert normal insert)
                              additional-wrap
                              slurp/barf-cp
                              (escape insert)))

  ;; configure textobjects here due to conflicts with evil-textobj
  (defvar +lispville-inner-text-objects-map (make-sparse-keymap))
  (defvar +lispville-outer-text-objects-map (make-sparse-keymap))

  (evil-define-key '(visual operator) 'lispyville-mode
    "i" +lispville-inner-text-objects-map
    "a" +lispville-outer-text-objects-map)

  (general-define-key
   :keymaps '+lispville-outer-text-objects-map
   "f" #'lispyville-a-function
   "a" #'lispyville-a-atom
   "l" #'lispyville-a-list
   "x" #'lispyville-a-sexp
   "g" #'lispyville-a-string)

  (general-define-key
   :keymaps '+lispville-inner-text-objects-map
   "f" #'lispyville-inner-function
   "a" #'lispyville-inner-atom
   "l" #'lispyville-inner-list
   "x" #'lispyville-inner-sexp
   "g" #'lispyville-inner-string)

  (general-define-key
   :states '(normal visual)
   :keymaps 'lispyville-mode-map
   ")" 'lispyville-next-closing
   "(" 'lispyville-previous-opening
   "{" 'lispyville-next-opening
   "}" 'lispyville-previous-closing)

  :ghook ('(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode))

(use-package undo-fu
  :custom
  (undo-limit 400000)
  (undo-strong-limit 3000000)
  (undo-outer-limit 48000000))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode)
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package yasnippet
  :after corfu
  :init
  (setq yas-verbosity 2)
  :general
  (+leader-def
    "is" '(yas-insert-snippet :wk "Snippet"))
  :config
  (yas-global-mode +1)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-keymap [(tab)] nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "C-<return>") (yas-filtered-definition 'yas-next-field-or-maybe-expand)))

(use-package yasnippet-capf
  :after (yasnippet cape)
  :vc (:fetcher github :repo elken/yasnippet-capf)
  :config
  (setq completion-at-point-functions
              (list #'yasnippet-capf))
)

;; Hitting TAB behavior
(setq tab-always-indent nil)
;; Remove ispell from default completion
(setq text-mode-ispell-word-completion nil)

(use-package cape)
(use-package corfu
  :hook
  (on-first-buffer . global-corfu-mode)
  (on-first-buffer . corfu-history-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-min-width 25)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match 'show)
  (corfu-cycle t)
  (global-corfu-modes '(prog-mode text-mode conf-mode))
  :config
  (advice-add 'evil-escape-func :after 'corfu-quit)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (general-define-key
   :keymaps 'corfu-map
   [tab] #'corfu-complete)
  )

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package git-commit
  :after magit
  :custom
  (git-commit-summary-max-length 72)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (global-git-commit-mode 1)
  (add-hook 'git-commit-setup-hook
    (lambda ()
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-emacs-state-p))
                 (bobp) (eolp))
        (evil-insert-state)))))

(use-package magit
  :defer .3
  :general
  (+leader-def :infix "g"
    "b" #'magit-branch
    "B" #'magit-blame-addition
    "c" #'magit-init
    "C" #'magit-clone
    "d" #'magit-diff-dwim
    "D" #'dotfiles-magit-status
    "g" #'magit-status
    "S" #'magit-stage-buffer-file
    "U" #'magit-unstage-buffer-file
    "L" #'magit-log-buffer-file)
  :custom
  (magit-auto-revert-mode nil) ;; does not need because global-auto-revert-mode is enabled
  (transient-default-level 5)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-show-gravatars t)
  (magit-revision-insert-related-refs nil)
  (magit-bury-buffer-function #'magit-mode-quit-window)

  :config
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; layout
  (defun +magit-display-buffer-fn (buffer)
    "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
                '(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))

               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode))))
                '(display-buffer-same-window))

               ('(+magit--display-buffer-in-direction))))))

  (defvar +magit-open-windows-in-direction 'right)

  (defun +magit--display-buffer-in-direction (buffer alist)
    "`display-buffer-alist' handler that opens BUFFER in a direction.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
    (let ((direction (or (alist-get 'direction alist)
                         +magit-open-windows-in-direction))
          (origin-window (selected-window)))
      (if-let (window (window-in-direction direction))
          (unless magit-display-buffer-noselect
            (select-window window))
        (if-let (window (and (not (one-window-p))
                             (window-in-direction
                              (pcase direction
                                (`right 'left)
                                (`left 'right)
                                ((or `up `above) 'down)
                                ((or `down `below) 'up)))))
            (unless magit-display-buffer-noselect
              (select-window window))
          (let ((window (split-window nil nil direction)))
            (when (and (not magit-display-buffer-noselect)
                       (memq direction '(right down below)))
              (select-window window))
            (display-buffer-record-window 'reuse window buffer)
            (set-window-buffer window buffer)
            (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
            (set-window-prev-buffers window nil))))
      (unless magit-display-buffer-noselect
        (switch-to-buffer buffer t t)
        (selected-window))))

  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)

  ;; for dotfiles
  (setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.cfg")))
  (setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))
  (defun dotfiles-magit-status ()
    "calls magit status on a git bare repo with set appropriate bare-git-dir and bare-work-tree"
    (interactive)
    (require 'magit-git)
    (let ((magit-git-global-arguments (append magit-git-global-arguments (list dotfiles-git-dir dotfiles-work-tree))))
      (call-interactively 'magit-status)))

  (defun +magit-process-environment (env)
    "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
  https://github.com/magit/magit/issues/460 (@cpitclaudel)."
    (let ((default (file-name-as-directory (expand-file-name default-directory)))
          (home (expand-file-name "~/")))
      (when (string= default home)
        (let ((gitdir (expand-file-name "~/.cfg")))
          (push (format "GIT_WORK_TREE=%s" home) env)
          (push (format "GIT_DIR=%s" gitdir) env))))
    env)

  (advice-add 'magit-process-environment
              :filter-return #'+magit-process-environment)
  )

(use-package forge
  :after magit
  :demand t
  :custom
  (forge-add-default-bindings nil)
  :config
  (transient-append-suffix 'forge-dispatch "c f"
    '("c m" "merge pull request" forge-merge))
  :general
  (+leader-def
    :keymaps '(magit-mode-map)
    "gw" 'forge-browse)
  (general-define-key
    :keymaps 'forge-topic-list-mode-map
    "q" #'kill-current-buffer)
  ;; (general-define-key
  ;;   :keymaps 'forge-pullreq-mode-map
  ;;   "m" '(lambda () (interactive) (forge-merge (forge-current-pullreq) "")))
)

(use-package smerge-mode
  :ensure nil
  :commands +smerge-hydra/body
  :general
  (+leader-def
    "gm" '(+smerge-hydra/body :wk "smerge"))
  :config
  (defhydra +smerge-hydra (:hint nil
                                 :pre (if (not smerge-mode) (smerge-mode 1))
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other         │
  ╭─────────────────────────────────────────────────────────╯
  │  ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
  │  ^_k_ ↑^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
  │  ^_j_ ↓^     [_l_] lower      [_>_] base/lower    [_R_] remove
  │  ^_G_^       [_a_] all        [_H_] hightlight    [_n_] next in project
  │          [_RET_] current  [_E_] ediff
  │                                                   [_q_] quit
  ╰─────────────────────────────────────────────────────╯
"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("j" next-line)
    ("k" previous-line)
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
    ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
    ("q" nil :color blue)))

(use-package browse-at-remote
  :general
  (+leader-def
    :keymaps '(prog-mode-map text-mode-map conf-mode-map)
    "gw" #'browse-at-remote)
)

(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4)
)

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  ;; (treesit-auto-add-to-auto-mode-alist '(go gomod))
  (global-treesit-auto-mode))


(use-package evil-textobj-tree-sitter
  :after (treesit evil)
  :config
  (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist '(tsx-ts-mode . "typescript"))
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
   "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer")
   "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
   "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner")
   "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-install-server)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  :custom
  (lsp-keymap-prefix nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-modeline-code-action-fallback-icon "󰌶")
  (lsp-auto-execute-action nil)
  (lsp-disabled-clients '(rubocop-ls))
  (lsp-kotlin-compiler-jvm-target "2.1")
  (lsp-kotlin-debug-adapter-path "~/.config/emacs/.cache/adapter/kotlin/bin/kotlin-debug-adapter")
  (lsp-clients-typescript-prefer-use-project-ts-server t)
  (lsp-javascript-implicit-project-config-check-js t)
  (lsp-javascript-suggest-complete-js-docs nil)
  (lsp-clients-typescript-preferences '(:includeCompletionsForImportStatements nil))
  :init
  (defun +update-completions-list ()
    (progn
      (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq-local completion-at-point-functions
                  (list (cape-capf-super #'non-greedy-lsp #'yasnippet-capf)))))

  :hook
  (lsp-managed-mode . (lambda () (general-define-key
                                  :states '(normal)
                                  :keymaps 'local
                                  "K" 'lsp-describe-thing-at-point)))
  (lsp-managed-mode . evil-normalize-keymaps)
  (lsp-completion-mode . +update-completions-list)
  (lsp-managed-mode . eldoc-mode)
  :general
  (+leader-def
    :keymaps 'lsp-mode-map
    :infix "c"
    "a" '(lsp-execute-code-action :wk "Code action")
    "i" '(lsp-find-implementation :wk "Find implementation")
    "k" '(lsp-describe-thing-at-point :wk "Show hover doc")
    "l" '(lsp-avy-lens :wk "Click lens")
    "o" '(lsp-organize-imports :wk "Organize imports")
    "q" '(lsp-workspace-shutdown :wk "Shutdown workspace")
    "Q" '(lsp-workspace-restart :wk "Restart workspace")
    "r" '(lsp-rename :wk "Rename")
    "R" '(lsp-find-references :wk "Restart workspace"))
  )

(use-package consult-lsp
  :general
  (+leader-def :keymaps 'lsp-mode-map
    "cj" '(consult-lsp-symbols :wk "Workspace symbols")
    "cx" '(consult-lsp-diagnostics :wk "Workspace diagnostics")))

(use-package editorconfig
  :general
  (+leader-def
    "fc" #'editorconfig-find-current-editorconfig)
  :hook (on-first-file . editorconfig-mode))

(use-package apheleia
  :commands apheleia-mode
  :general
  (+leader-def
    "cf" '(apheleia-format-buffer :wk "Format buffer"))
  :config
  ;; (setf (alist-get 'erb-formatter apheleia-formatters)
  ;;       '("erb-format" "--print-width=140" filepath))
  ;; (add-to-list 'apheleia-mode-alist '(erb-mode . erb-formatter))
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
      '(ruby-standard))
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  )

(use-package flycheck
  :preface
  (defun +flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (flycheck-checkers nil)
  (flycheck-display-errors-function nil)
  (flycheck-help-echo-function nil)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :hook
  (flycheck-mode . (lambda ()
                     (add-hook 'eldoc-documentation-functions #'+flycheck-eldoc 0 t)))
  )

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4)
  :init
  (defun +go-mode-setup ()
    (+add-pairs '((?` . ?`)))
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  :hook
  (go-ts-mode . apheleia-mode)
  (go-ts-mode . +go-mode-setup)
  (go-ts-mode . lsp-deferred)
  )

(use-package gotest
  :general
  (+local-leader-def
    :keymaps 'go-ts-mode-map
    "b" '(:ignore t :wk "build")
    "br" 'go-run
    "t" '(:ignore t :wk "test")
    "ts" 'go-test-current-test
    "tt" 'go-test-current-test-cache
    "tf" 'go-test-current-file
    "ta" 'go-test-current-project
    "tb" 'go-test-current-benchmark))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :ensure nil
  :init
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-server-display-inlay-hints t)
  :hook
  (rust-ts-mode . apheleia-mode)
  (rust-ts-mode . lsp-deferred))

(use-package scala-mode
  :custom
  (scala-indent:align-parameters t)
  (scala-indent:use-javadoc-style t)
  :interpreter ("scala" . scala-mode)
  :mode "\\.scala\\'"
  :mode "\\.sbt\\'")

(use-package sbt-mode
  :general
  (+local-leader-def
    :keymaps '(scala-mode-map)
    "b" '(nil :wk "sbt")
    "bb" #'sbt-command
    "bc" #'sbt-compile
    "br" #'sbt-start
    "b." #'sbt-run-previous-command
    "t" '(nil :wk "test")
    "ta" '(sbt-do-test :wk "Test quick")
    "tf" '(+sbt-test-file :wk "Test current file")
    ;; "tt" '(nil :wk "Test quick")
  )
  :commands sbt-start sbt-command
  :init
  (defun +sbt-get-testonly-file (&optional file)
    "Return FILE formatted in a sbt testOnly command."
    (--> (or file (file-name-base))
         (format "testOnly *%s" it)))

  (defun +sbt-test-file (&optional file)
    (interactive)
    (sbt-command (+sbt-get-testonly-file file)))

  ;; (defun +sbt-get-testcase-name ()
  ;;   "Get Scala test case nearby point."
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (let* ((line (thing-at-point 'line t))
  ;;            (on-testcase-p (and (s-contains? "\"" line)
  ;;                                (s-contains? "{\n" line)))
  ;;            (get-testcase-name (lambda (l)
  ;;                                 (--> l
  ;;                                      (s-split "\"" it)
  ;;                                      reverse
  ;;                                      cl-second))))
  ;;       (if on-testcase-p
  ;;           (funcall get-testcase-name line)
  ;;         (progn
  ;;           (search-backward "{\n")
  ;;           (funcall get-testcase-name (thing-at-point 'line t)))))))

  ;; (defun +sbt-run-testcase-at-point ()
  ;;   "Run Scala test case at point."
  ;;   (interactive)
  ;;   (sbt-command (format "%s -- -z \"%s\"" (+sbt-get-testonly-file) (+sbt-get-testcase-name))))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; (use-package lsp-metals
;;   :general
;;   (+local-leader-def
;;     :keymaps '(scala-mode-map)
;;     "fn" #'lsp-metals-new-scala-file)
;;   :custom
;;   (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
;;   :hook
;;   (scala-mode . lsp-deferred))

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2)
  :hook
  (css-ts-mode . lsp-deferred)
  (css-ts-mode . apheleia-mode))

(use-package jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx?\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :custom
  (js-chain-indent t)
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  :hook
  (jtsx-tsx-mode . lsp-deferred)
  (jtsx-tsx-mode . apheleia-mode)
  (jtsx-jsx-mode . lsp-deferred)
  (jtsx-jsx-mode . apheleia-mode)
  (jtsx-jsx-mode . (lambda ()
                     (yas-activate-extra-mode 'js-mode)
                     (yas-activate-extra-mode '+web-react-mode)))
  (jtsx-tsx-mode . (lambda ()
                     (yas-activate-extra-mode 'typescript-tsx-mode)))
  (jtsx-jsx-mode . (lambda ()
                     (+add-pairs '((?` . ?`)))))
  (jtsx-tsx-mode . (lambda ()
                     (+add-pairs '((?` . ?`)))))
  )

(use-package web-mode
  ;; :defer .5
  ;; :demand t
  :custom
  (web-mode-enable-html-entities-fontification t)
  (web-mode-markup-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-auto-close-style 1)
  (web-mode-comment-style 2)
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
  (define-derived-mode erb-mode web-mode
    "Web[erb]")
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . erb-mode))
  :config
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'"))
  :hook
  (web-mode . apheleia-mode)
)

(use-package auto-rename-tag
  :hook ((jtsx-jsx-mode . auto-rename-tag-mode)
         (html-ts-mode . auto-rename-tag-mode)
         (jtsx-tsx-mode . auto-rename-tag-mode)))

(use-package lsp-pyright
  :hook
  ((python-mode python-ts-mode) . lsp-deferred))

(use-package pytest
  :vc (:fetcher github :repo ionrock/pytest-el)
  :general
  (+local-leader-def
    :keymaps '(python-ts-mode-map)
    "t" '(nil :wk "test")
    "ta" #'pytest-all
    "tf" #'pytest-module
    "t." #'pytest-run
    "tt" #'pytest-again
    "ts" #'pytest-one))

(use-package auto-virtualenv
  :hook
  ((python-mode python-ts-mode) . auto-virtualenv-set-virtualenv))

(use-package pyvenv
  :init
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("venv:" pyvenv-virtual-env-name " ")))
  :hook
  ((python-mode python-ts-mode) . pyvenv-mode))

(use-package ruby-ts-mode
  :ensure nil
  :hook
  (ruby-ts-mode . apheleia-mode)
  (ruby-ts-mode . lsp-deferred))

(use-package inf-ruby
  :hook (compilation-filter . inf-ruby-auto-enter)
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode)
  :general
  (+local-leader-def
    :keymaps 'ruby-ts-mode-map
    "r" '(:ignore t :wk "run")
    "rl" 'ruby-send-line
    "rr" 'ruby-send-region
    "rd" 'ruby-send-definition
    "ro" 'inf-ruby-console-auto))

(use-package ruby-end
  :after (ruby-mode ruby-ts-mode))

(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :general
  (+local-leader-def
    :keymaps '(rspec-mode-map)
    "t" '(nil :wk "test")
    "ta" #'rspec-verify-all
    "tr" #'rspec-rerun
    "tv" #'rspec-verify
    "tc" #'rspec-verify-continue
    "tl" #'rspec-run-last-failed
    "tT" #'rspec-toggle-spec-and-target
    "tt" #'rspec-toggle-spec-and-target-find-example
    "ts" #'rspec-verify-single
    "te" #'rspec-toggle-example-pendingness))

(use-package rake
  :init
  (setq rake-completion-system 'default)
  :general
  (+local-leader-def
    :keymaps '(ruby-ts-mode-map)
    "k" '(nil :wk "rake")
    "kk" #'rake
    "kr" #'rake-rerun
    "kR" #'rake-regenerate-cache
    "kf" #'rake-find-task))

(use-package bundler
  :general
  (+local-leader-def
    :keymaps '(ruby-ts-mode-map)
    "b" '(nil :wk "bundle")
    "bc" #'bundle-check
    "bC" #'bundle-console
    "bi" #'bundle-install
    "bu" #'bundle-update
    "be" #'bundle-exec
    "bo" #'bundle-open))

(use-package kotlin-ts-mode
  :mode "\\.kt\\'"
  :hook
  (kotlin-ts-mode . lsp-deferred)
  :config
  (require 'dap-kotlin)
  (dap-register-debug-template "Kotlin tests with launcher"
                               (list :type "kotlin"
                                     :request "launch"
                                     :mainClass "org.junit.platform.console.ConsoleLauncher --scan-classpath"
                                     :enableJsonLogging nil
                                     :noDebug nil))

  )

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . apheleia-mode)
  :general
  (+local-leader-def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map racket-mode-map scheme-mode-map)
    "p" #'check-parens)
  (+local-leader-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e"   '(nil :wk "eval")
    "eb"  'eval-buffer
    "ed"  'eval-defun
    "ee"  'eval-last-sexp
    "er"  'eval-region
    "eR"  'elisp-eval-region-or-buffer
    "el"  'load-library
    "g"   '(nil :wk "goto/find")
    "gf"  'find-function-at-point
    "gR"  'find-function
    "gv"  'find-variable-at-point
    "gV"  'find-variable
    "gL"  'find-library))

(use-package eros
  :custom
  (eros-eval-result-prefix "⟹ ")
  :hook
  (emacs-lisp-mode . eros-mode))

;; (use-package log4j-mode
;;   :defer t)

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook
  (markdown-mode . variable-pitch-mode)
  :config
  (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
  :custom
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh")))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  :init
  (setq yaml-ts-mode--syntax-table
    (let ((table (make-syntax-table)))
        (modify-syntax-entry ?#  "<"  table)
        (modify-syntax-entry ?\n ">"  table)
        (modify-syntax-entry ?&  "."  table)
        (modify-syntax-entry ?*  "."  table)
        (modify-syntax-entry ?\( "."  table)
        (modify-syntax-entry ?\) "."  table)
        (modify-syntax-entry ?\' "\"" table)
        (modify-syntax-entry ?/  ". 124b" table)
        table))
  )

(use-package json-ts-mode
  :ensure nil
  :init
  (defun +json-mode-setup ()
    (add-hook 'before-save-hook 'json-pretty-print-buffer t t))
  :hook
  (json-ts-mode . +json-mode-setup)
  :mode "\\.prettierrc\\'")

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package git-modes
  :init
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode))

(setq ansi-color-for-comint-mode t)
;; If a shell command never outputs anything, don't show it.
(customize-set-variable 'async-shell-command-display-buffer nil)
(customize-set-variable 'shell-command-prompt-show-cwd t)

;;;###autoload
(defun async-shell-command-region (start end)
  "Send region from START to END to async-shell-command and display the result."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (let ((cmd (string-trim (buffer-substring-no-properties start end))))
    (async-shell-command cmd)))

;;;###autoload
(defun project-or-cwd-async-shell-command ()
  "Run `async-shell-command' in the current project's root directory."
  (declare (interactive-only async-shell-command))
  (interactive)
  (let ((project (project-current)))
    (if project
        (let ((default-directory (project-root (project-current t))))
            (call-interactively #'async-shell-command))
      (call-interactively #'async-shell-command))))

(use-package compile
  :ensure nil
  :custom
  (compile-command "make ")
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(use-package shell-command-x
  :custom
  (shell-command-x-buffer-name-async-format "*shell:%a*")
  (shell-command-x-buffer-name-format "*shell:%a*")
  :bind
  ([remap shell-command] . project-or-cwd-async-shell-command)
  :hook
  (on-first-input . shell-command-x-mode))

;; (use-package bash-completion
;;   :config
;;   (setq bash-completion-use-separate-processes t)
;;   (bash-completion-setup)

;;   (defun eshell-bash-completion-capf-nonexclusive ()
;;     (let ((compl (bash-completion-dynamic-complete-nocomint
;;                   (save-excursion (eshell-bol) (point))
;;                   (point) t)))
;;       (when compl
;;         (append compl '(:exclusive no)))))

;;   (add-hook 'eshell-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions (list #'eshell-bash-completion-capf-nonexclusive))))
;;   )

(use-package eat
  :commands (eat project-eat)
  :config
  (defun project-eat ()
    "Start Eat in the current project's root directory."
    (interactive)
    (defvar eat-buffer-name)
    (let* ((default-directory (project-root (project-current t)))
           (eat-buffer-name (project-prefixed-buffer-name "eat"))
           (eat-buffer (get-buffer eat-buffer-name)))
      (if (and eat-buffer (not current-prefix-arg))
          (pop-to-buffer eat-buffer (bound-and-true-p display-comint-buffer-action))
        (eat))))

  (evil-set-initial-state 'eat-mode 'insert)
  :custom
  (eat-kill-buffer-on-exit t)
  :general
  (+leader-def
    "ot" #'eat
    "pt" #'project-eat)
  (:states '(normal visual)
           :keymaps 'eat-mode-map
           "<return>" #'evil-insert-resume)
  (:states '(insert)
           :keymaps 'eat-mode-map
           "C-y" #'eat-yank)
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(with-eval-after-load 'consult
  (defvar  +consult--source-term
    (list :name     "Terminal buffers"
          :narrow   ?t
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :mode '(shell-mode eshell-mode term-mode eat-mode compilation-mode)
                             :sort 'visibility
                             :as #'buffer-name))))
  (add-to-list 'consult-buffer-sources '+consult--source-term 'append))

(use-package shell
  :ensure nil
  :hook
  (shell-mode . evil-normal-state))

(use-package eshell
  :ensure nil
  :general
  (+leader-def
    "oe"  #'eshell
    "oE"  #'+eshell-new)
  (:states '(normal visual)
           :keymaps 'eshell-mode-map
           "<return>" #'evil-insert-resume)
  (:states '(insert)
           :keymaps 'eshell-mode-map
           "C-y" #'yank)
  (:states '(normal visual insert)
           :keymaps 'eshell-mode-map
           "C-t" #'+interactive-cd)
  (:states '(normal visual insert)
           :keymaps 'eshell-mode-map
           "M-r" #'consult-history)
  :preface
  (defface +eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
    "TODO"
    :group 'eshell)

  (defun +eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (require 'shrink-path)
    (concat (if (bobp) "" "")
            (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name (shrink-path-file pwd)))
                          'face '+eshell-prompt-pwd))
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))
  (defun +interactive-cd (dir)
    "Prompt for a directory and cd to it."
    (interactive "Dcd ")
    (let ((inhibit-read-only t))
      (insert (concat "cd " dir)))
    (pcase major-mode
      ('shell-mode (comint-send-input))
      ('eshell-mode (eshell-send-input))))

  (defun +eshell-new ()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))
  :init
  (setq eshell-banner-message ""
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-default-prompt-fn
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  (add-hook 'eshell-mode-hook
            (defun +eshell-setup ()
              ;; (eshell-cmpl-mode -1)
              ;; (add-to-list 'capf-autosuggest-capf-functions 'pcomplete-completions-at-point)
              ;; remove fringe
              (set-window-fringes nil 0 0)
              (set-window-margins nil 1 nil)
              ;; scrolling
              (setq hscroll-margin 0)
              ;; Text wrapping
              ;; (visual-line-mode +1)
              (set-display-table-slot standard-display-table 0 ?\ )))
  )

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Dropbox/org/")
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-cycle-separator-lines 2)
  (org-fold-core-style 'overlays)
  (imenu-auto-rescan t)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  :config
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-column nil :background nil)
  ;; (set-face-attribute 'org-column-title nil :background nil)

  (define-key org-src-mode-map [remap evil-quit] 'org-edit-src-exit)
  :general
  (+local-leader-def
    :keymaps '(org-mode-map)
    "'" #'org-edit-special
    "." #'consult-org-heading
    "e"   '(nil :wk "eval")
    "ed"  'eval-defun
    "ee"  'eval-last-sexp
    "er"  'eval-region
    "l" #'org-insert-link)
  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
)

(use-package evil-org
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :hook (org-agenda-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-superstar
  :init
  (setq org-superstar-special-todo-items t
        org-superstar-remove-leading-stars t)
  :hook (org-mode . org-superstar-mode))

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                (todo tag-up priority-down category-keep)
                                (tags priority-down category-keep)
                                (search category-keep)))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "[ ](T)" "|" "[X](x!)")))
  (org-refile-use-outline-path 'file)
  (org-refile-targets '(("tasks.org" :maxlevel . 1)
                        ))
  (org-agenda-files `(,(expand-file-name "tasks.org" org-directory)))
  (org-agenda-confirm-kill nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  ;; (org-agenda-custom-commands
  ;;  '(("g" "Groceries" todo ""
  ;;     ((org-agenda-files
  ;;       `(,(expand-file-name "groceries.org" org-directory)))))))
  (org-capture-templates
   `(("t" "Tasks" entry (file "tasks.org")
      "* TODO %?")
     ;; ("g" "Groceries" entry (file+olp "groceries.org" "Groceries")
     ;;  "* [ ] %?")
     ))
  :general
  ;; (+leader-def
  ;;   "ng"  '((lambda () (interactive) (org-agenda nil "g")) :wk "Groceries"))
  (:keymaps 'org-agenda-mode-map
            "q" 'org-agenda-exit)
  :hook
  (org-capture-mode . evil-insert-state)
  (org-agenda-mode . hl-line-mode)
  (org-agenda-mdoe . (lambda ()
                       (interactive) (org-element-cache-reset 'all)))
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
  )

(use-package org-super-agenda
  :after (org-agenda)
  :config
  (setq org-super-agenda-groups
        `(
          (:name "Next" :todo "NEXT")
          (:name "Todo" :todo "TODO")
          ))
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package org-tempo
  :after org
  :ensure nil
  :config
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (shell . t)
      (js . t)
      (verb . t)))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
)

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(setq ediff-diff-options "-w" ; turn off whitespace checking
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defvar +ediff-saved-wconf nil)
(add-hook 'ediff-before-setup-hook
          (lambda ()
            (setq +ediff-saved-wconf (current-window-configuration))))
(defun +ediff-restore-wconf-h ()
  (when (window-configuration-p +ediff-saved-wconf)
    (set-window-configuration +ediff-saved-wconf)))
(add-hook 'ediff-quit-hook '+ediff-restore-wconf-h)
(add-hook 'ediff-suspend-hook '+ediff-restore-wconf-h)

(use-package deadgrep
  :general
  (+leader-def
    "sg" #'deadgrep))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (dolist (var '("KUBECONFIG"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package envrc
  :hook (on-first-file . envrc-global-mode))

(use-package docker
  :init
  (setq docker-show-messages nil)
  (add-to-list
    'display-buffer-alist
     `("\\*docker-"
       (display-buffer-same-window)
      ))
  :general
  (+leader-def
    "od" #'docker)
  )

;; (use-package kubel
;;   :commands kubel
;;   :general
;;   (+leader-def
;;     "ok" #'kubel))

;; (use-package kubel-evil
;;   :after kubel)

(setq dictionary-use-single-buffer t)
(setq dictionary-server "dict.org")

(use-package devdocs
  :commands (devdocs-lookup devdocs-install devdocs-update-all devdocs-delete devdocs-persue)
  :general
  (+leader-def
    "sk" 'devdocs-lookup))

;; (use-package chatgpt-shell
;;   :general
;;   (+leader-def
;;     "og" #'chatgpt-shell)
;;   :config
;;   (setq chatgpt-shell-openai-key
;;         (lambda ()
;;           (auth-source-pick-first-password :host "api.openai.com"))))

(use-package verb
  :init
  (setq verb-auto-kill-response-buffers t
        verb-json-use-mode 'json-ts-mode)
  :config
  (add-to-list 'org-structure-template-alist '("vb" . "src verb :wrap src ob-verb-response :op send get-body"))
  :general
  (+leader-def
   :keymaps 'org-mode-map
   "v" '(:ignore t :wk "verb")
   "vf" '(verb-send-request-on-point-other-window-stay :wk "Send request")
   "vr" '(verb-send-request-on-point-other-window-stay :wk "Send request other window")))

(use-package impostman
  :commands (impostman-import-file impostman-import-string))

(use-package elfeed
  :commands elfeed
  :general
  (+leader-def
    "or" #'elfeed)
  :init
  (setq elfeed-feeds
        '("https://codeopinion.com/feed"
          "https://juacompe.medium.com/feed"
          "https://bitfieldconsulting.com/golang?format=rss"
          "https://go.dev/blog/feed.atom"
          "https://particular.net/feed.xml"
          "https://www.ardanlabs.com/blog/index.xml"
          "https://www.somkiat.cc/feed"
          "https://weerasak.dev/feed.xml"
          "https://engineering.grab.com/feed.xml"
          )))

;; Save custom vars to separate file from init.el.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
