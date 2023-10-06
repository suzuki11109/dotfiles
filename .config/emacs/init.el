;; Some constants
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive.
   native-comp-async-report-warnings-errors nil
   ;; Make native compilation happens asynchronously
   native-comp-jit-compilation t))

;; Set initial buffer to fundamental-mode for faster load
(setq initial-major-mode 'fundamental-mode)

;; Save custom vars to separate file from init.el.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))

;; Inhibits fontification while receiving input
(setq redisplay-skip-fontification-on-input t)

(setq pgtk-wait-for-event-timeout 0.001)

(setq inhibit-x-resources t)

;; Slightly faster re-display
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                    (format "%.2f seconds"
                             (float-time (time-subtract (current-time) before-init-time)))
                     gcs-done)))

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Use imenu with use-package
(setq use-package-enable-imenu-support t)

;; For :bind
(require 'bind-key)

;; Load general first for :general
(use-package general
  :demand t)

(elpaca-wait)

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package general
  :elpaca nil
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
    "!"   #'shell-command
    "|"   #'shell-command-on-region

    "b"   '(nil :wk "buffer")
    "bb"  '(switch-to-buffer :wk "Switch buffer")
    "bd"  '(kill-this-buffer :wk "Kill this buffer")
    "bD"  '(kill-buffer :wk "Kill buffer")
    "bi"  #'ibuffer
    "bo"  '(switch-to-buffer-other-window :wk "Switch buffer other window")
    ;; "bu"  #'+sudo-save-buffer
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

    "f"   '(nil :wk "file")
    "fd"  #'dired
    "fD"  '(+delete-this-file :wk "Delete this file")
    "fe"  '((lambda () (interactive)
              (let ((default-directory "~/.config/emacs/"))
                (call-interactively 'find-file))) :wk "Find in emacs config")
    "ff"  '(find-file :wk "Find file")
    "fg"  '((lambda () (interactive) (find-file "~/.gitconfig")) :wk "Edit .gitconfig")
    "fh"  '((lambda () (interactive)
              (let ((default-directory "~/"))
                (call-interactively 'find-file))) :wk "Find in home")
    "fi"  '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Edit init.org")
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

    "k"  '(nil :wk "bookmark")
    "ks"  #'bookmark-set
    "kk"  #'bookmark-jump
    "kl"  #'list-bookmarks
    "kd"  #'bookmark-delete

    "l"  '(nil :wk "package")
    "lm" #'elpaca-manager
    "ld" #'elpaca-delete
    "ll" #'elpaca-log
    "lt" #'elpaca-status
    "lu" #'elpaca-update
    "lU" #'elpaca-update-all

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

    "tf"  #'toggle-frame-fullscreen
    "th"  '(load-theme :wk "Load theme")
    "tr"  #'read-only-mode
    )
  )

;; Escape once
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :defer 1
  :custom
  (which-key-ellipsis "..")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 5)
  (which-key-add-column-padding 1)
  :config
  (which-key-mode 1))

(setq
 ;; Fluid scrolling
 pixel-scroll-precision-use-momentum t
 ;; Do not adjust window-vscroll to view tall lines. Fixes some lag issues see:
 ;; emacs.stackexchange.com/a/28746
 auto-window-vscroll nil
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 scroll-conservatively 101
 ;; Scroll at a margin of one line
 scroll-margin 3)

;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
(pixel-scroll-precision-mode 1)

;; Enable saving minibuffer history
(use-package savehist
  :elpaca nil
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring register-alist))
  :config
  (savehist-mode 1))

;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1)

;; Enable recursive calls to minibuffer
(setq enable-recursive-minibuffers t)

;; Move stuff to trash
(setq delete-by-moving-to-trash t)

;; Better unique buffer names for files with the same base name.
(setq uniquify-buffer-name-style 'forward)

(setq
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Disable making backup files
 make-backup-files nil)

(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-save/" t)))

;; Auto load files changed on disk
(use-package autorevert
  :elpaca nil
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  :config
  (global-auto-revert-mode 1))

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
  :defer 1
  :elpaca nil
  :config
  (global-so-long-mode 1))

;; Saving multiple files saves only in sub-directories of current project
(setq save-some-buffers-default-predicate #'save-some-buffers-root)

(setq
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t)

;; suppress large file opening confirmation
(setq large-file-warning-threshold nil)

;; recent files
(use-package recentf
  :elpaca nil
  :init
  (setq
   ;; Increase the maximum number of saved items
   recentf-max-saved-items 100
   ;; Ignore case when searching recentf files
   recentf-case-fold-search t
   ;; Exclude some files from being remembered by recentf
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
  :config
  (recentf-mode 1))

(use-package dired
  :elpaca nil
  :defer t
  :commands dired
  :custom
  (dired-listing-switches "-ahl")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :elpaca nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dired-clean-confirm-killing-deleted-buffers nil)
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

;; dired fontlock
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-single
  :after dired
  :config
  (define-key dired-mode-map [remap dired-find-file]
              'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
              'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
              'dired-single-up-directory))

(use-package project
  :elpaca nil
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :config
  (setq project-vc-extra-root-markers '("go.mod"))
  ;; (setq project-switch-commands 'project-dired)
  (project-forget-zombie-projects) ;; really need to this to make tabspaces works

  :general
  (+leader-def
    "p" '(:keymap project-prefix-map :wk "project")
    ))

(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-echo-area-use-multiline-p nil)

(defun bury-or-kill ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil) t))
(add-hook 'kill-buffer-query-functions #'bury-or-kill)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(setq
 ;; Silent mode
 ring-bell-function #'ignore
 ;; Set to non-nil to flash!
 visible-bell nil)

(setq
 ;; Use y or n instead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs #'y-or-n-p)

;; Always prompt in minibuffer (no GUI)
(setq use-dialog-box nil)

;; Use only spaces
(setq-default indent-tabs-mode nil)
;; Tab width 8 is too long
(setq-default tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Use single space between sentences
(setq sentence-end-double-space nil)
;; Don't store duplicated entries
(setq history-delete-duplicates t)
;; Always add final newline
(setq require-final-newline t)

;; Wrap long lines
(global-visual-line-mode 1)

;; Display long lines
(setq truncate-lines nil)

;; Remember cursor position in files
(save-place-mode 1)

  ;;; Why use anything but UTF-8?
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

(use-package elec-pair
  :elpaca nil
  :hook
  ((prog-mode text-mode conf-mode) . electric-pair-mode)
  :hook
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  :preface
  (defun +add-pairs (pairs)
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))

;; Clipboard
(setq
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t)

(use-package evil
  :defer .5
  :custom
  (evil-v$-excludes-newline t)
  (evil-mode-line-format nil)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-respect-visual-line-mode t)
  (evil-symbol-word-search t)
  :preface
  (defun +evil-yank-to-eol ()
    (interactive)
    (evil-yank 0)
    (evil-end-of-line))
  :general
  (+leader-def
    "w" '(:keymap evil-window-map :wk "window"))
  (:states 'motion
    ";" 'evil-ex)
  (:states '(normal visual)
    "$" 'evil-end-of-line)
  :config
  (modify-syntax-entry ?_ "w")
  (defalias 'forward-evil-word 'forward-evil-symbol)
  (setq evil-visual-state-cursor '(hollow))
  (customize-set-variable 'evil-want-Y-yank-to-eol t) ;; :custom doesn't work

  (evil-set-undo-system 'undo-fu)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
)

(use-package evil-collection
  :after evil magit
  :custom
  (evil-collection-key-blacklist '("C-y"))
  :config
  (evil-collection-init)
)

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
        evil-escape-excluded-major-modes '(eshell-mode vterm-mode)
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

(use-package lispyville
  :disabled t
  :config
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

;; Always prompt in minibuffer
(setq use-dialog-box nil)
;; Set default fonts
(set-face-attribute 'default nil :font "monospace" :height 110)
(set-face-attribute 'variable-pitch nil :family "PT Serif" :height 1.1)
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 110)
;; Set thai font
(set-fontset-font t 'thai "SF Thonburi")
(set-fontset-font t 'thai (font-spec :script 'thai) nil 'append)

;; Set line height
(setq-default line-spacing 2)

(use-package default-text-scale
  :custom
  (text-scale-mode-step 1.0625)
  :commands (default-text-scale-increase default-text-scale-decrease)
  :general
  ("M--" 'default-text-scale-decrease)
  ("M-=" 'default-text-scale-increase))

;; Stretch cursor to the glyph width
(setq x-stretch-cursor t)
;; Remove visual indicators from non selected windows
(setq-default cursor-in-non-selected-windows nil)
;; No blinking cursor
(blink-cursor-mode -1)

(use-package display-line-numbers
  :elpaca nil
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-widen t)
  :init
  (dolist (mode '(org-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Show line, columns number in modeline
(size-indication-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq mode-line-percent-position nil)

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'buffer)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-modal nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version nil)
  :init
  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                   '(coding-category-undecided coding-category-utf-8))
                             (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                  t)))

  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
  :hook
  (window-setup . doom-modeline-mode))

;; Show search count in modeline
(use-package anzu
  :after (evil)
  :config
  (global-anzu-mode 1))

(use-package evil-anzu
  :after (evil anzu))

;; Resize a frame by pixel
(setq frame-resize-pixelwise t)
;; Frame title
(setq frame-title-format
      (list
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
       '(:eval
         (let ((project (project-current)))
           (when project
             (format " — %s" (project-name project)))))))

(defun +set-frame-scratch-buffer (frame)
  (with-selected-frame frame
    (switch-to-buffer "*scratch*")))
(add-hook 'after-make-frame-functions #'+set-frame-scratch-buffer)

(use-package tab-bar
  :elpaca nil
  :after (project)
  :custom
  (tab-bar-show 1)
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
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  )

;; Resize window combinations proportionally
(setq window-combination-resize t)

;; Window layout undo/redo
(winner-mode 1)

(use-package windresize
  :init
  (setq windresize-default-increment 5)
  :general
  ("S-C-<return>" 'windresize)
  :commands windresize)

(use-package popper
  :general
  ("C-\\" 'popper-toggle-latest)
  ("C-`"  'popper-cycle)
  ("C-~" 'popper-toggle-type)
  (:keymaps 'vterm-mode-map
      "C-\\" 'popper-toggle-latest)
  :init
  (setq popper-window-height 0.33)
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
      eshell-mode
      "\\*shell\\*"
      shell-mode
      "\\*term\\*"
      term-mode
      "\\*vterm\\*"
      "\\*vterminal\\*"
      "-vterm\\*$"
      "\\* docker vterm:"
      vterm-mode
      "\\*rake-compilation\\*"
      "\\*rspec-compilation\\*"
      "\\*Flymake "
      "\\*Flycheck errors\\*"
      "\\*Org Select\\*"
      help-mode
      lsp-help-mode
      helpful-mode
      docker-container-mode
      docker-network-mode
      docker-volume-mode
      docker-image-mode
      docker-context-mode
      "\\*Org Select\\*"
      "\\*Capture\\*"
      "^CAPTURE-"
      "\\*xref\\*"
      "\\*eldoc\\*"
      ))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package transient
  :elpaca nil
  :defer t
  :config
  ;; Map ESC and q to quit transient
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one))

(use-package nerd-icons
  :general
  (+leader-def
    "in" '(nerd-icons-insert :wk "Nerd icons"))
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font")
  (nerd-icons-scale-factor 1.0))

;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic nil)
;;   (load-theme 'doom-vibrant t)
;;   (doom-themes-org-config))

(use-package catppuccin-theme
  :init
  (load-theme 'catppuccin t))

(use-package hl-todo
  :init
  (global-hl-todo-mode 1))

(use-package orderless
  :demand t
  :custom
  ;; (orderless-matching-styles '(orderless-literal orderless-regexp))
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (partial-completion)))
     ))
  :init
  (defun +orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun +lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (add-hook 'orderless-style-dispatchers #'+orderless-dispatch-flex-first nil 'local))
  :hook
  (lsp-completion-mode . +lsp-mode-setup-completion))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas-reload-all))

(use-package yasnippet-capf
  :after (yasnippet cape)
  :elpaca (:host github :repo "elken/yasnippet-capf"))

;; Hitting TAB behavior
(setq tab-always-indent nil)
(use-package cape)
(use-package corfu
  :elpaca (:host github :repo "minad/corfu"
                 :files (:defaults "extensions/*"))
  :hook
  ((eshell-mode comint-mode) . (lambda ()
                                 (setq-local corfu-auto nil
                                             corfu-preselect 'prompt
                                             corfu-preview-current t
                                             corfu-quit-no-match t
                                             corfu-quit-at-boundary t)
  ))
  (window-setup . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-min-width 25)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1)
  (general-define-key
    :keymaps 'corfu-map
    :predicate '(bound-and-true-p eshell-mode)
    [backtab] #'corfu-previous
    [tab] #'corfu-next)

  (defun corfu-enable-in-minibuffer ()
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  )

(use-package kind-icon
  :after (corfu nerd-icons)
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons nil)
  (kind-icon-mapping
      `(
        (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
        (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
        (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
        (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
        (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
        (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
        (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
        (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
        (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
        (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
        (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
        (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
        (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
        (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
        (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
        (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
        (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
        (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
        (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
        (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
        (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
        (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
        (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
        (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
        (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
        (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
        (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
        (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
        (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
        (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
        (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
        (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
        (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
        (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
        (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
        (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  ("C-s" 'consult-line)
  (+leader-def
    "sb"  #'consult-line
    "sB"  #'consult-line-multi
    "sf"  #'consult-find
    "sh"  #'consult-history
    "sp"  #'consult-ripgrep
    "hI"  #'consult-info)
  :bind
  (:map minibuffer-local-map
    ("M-r" . consult-history))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
)

(use-package embark
  :after vertico
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

  :bind
  ("C-." . embark-dwim)
  ("C-;" . embark-act)
  (:map embark-general-map
    ("D" . xref-find-definitions-other-window))
  )

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
  :elpaca (:host github :repo "minad/vertico"
                 :files (:defaults "extensions/*"))
  :init
  (setq vertico-resize nil
        vertico-count 14)
  :hook
  (window-setup . vertico-mode)
  ;; :config
  ;; (vertico-mode)
  :general
  (+leader-def
    "." '(vertico-repeat :wk "Resume last search")))

(use-package vertico-directory
  :elpaca nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package git-commit
  :after magit
  :custom
  (git-commit-summary-max-length 72)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'git-commit-mode 'insert))
  (global-git-commit-mode 1))

(use-package magit
  :defer .2
  :general
  (+leader-def :infix "g"
    "b" #'magit-branch
    "B" #'magit-blame-addition
    "c" #'magit-init
    "C" #'magit-clone
    "d" #'magit-diff-dwim
    "D" #'dotfiles-magit-status
    "g" #'magit-status
    "S" #'magit-stage-file
    "U" #'magit-unstage-file
    "L" #'magit-log-buffer-file)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-bury-buffer-function #'magit-mode-quit-window)
  :config
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

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

  (defvar +magit-open-windows-in-direction 'right
    "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")

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

  (setq magit-display-buffer-function #'+magit-display-buffer-fn)

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
  :general
  (general-define-key
    :keymaps 'forge-topic-list-mode-map
    "q" #'kill-current-buffer)
  (+local-leader-def
    :keymaps 'forge-topic-mode-map
    "c"  #'forge-create-post
    "e"  '(:ignore t :which-key "edit")
    "ea" #'forge-edit-topic-assignees
    "ed" #'forge-edit-topic-draft
    "ek" #'forge-delete-comment
    "el" #'forge-edit-topic-labels
    "em" #'forge-edit-topic-marks
    "eM" #'forge-merge
    "en" #'forge-edit-topic-note
    "ep" #'forge-edit-post
    "er" #'forge-edit-topic-review-requests
    "es" #'forge-edit-topic-state
    "et" #'forge-edit-topic-title)
)

(use-package smerge-mode
  :elpaca nil
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
    "gw" #'browse-at-remote))

(use-package treesit
  :elpaca nil
  ;; :demand t
  ;; :hook
  ;; (go-ts-mode . (lambda ()
  ;;                 (setq treesit-font-lock-feature-list
  ;;                       `((comment definition)
  ;;                         (keyword string type)
  ;;                         (constant escape-sequence label number)
  ;;                         (bracket delimiter error operator variable)))
  ;;                 (treesit-font-lock-recompute-features)))
  :init
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; remap built-in modes to new ts-modes
  (setq major-mode-remap-alist
        '((html-mode . html-ts-mode)
          (mhtml-mode . html-ts-mode)
          (bash-mode . bash-ts-mode)
          (js-json-mode . json-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (js-jsx-mode . js-ts-mode)
          ))

  (defun +treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75)))))

(use-package evil-textobj-tree-sitter
  :after (evil)
  :elpaca (:host github
                 :repo "meain/evil-textobj-tree-sitter"
                 :files (:defaults "queries" "treesit-queries"))
  :config
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

(use-package eglot
  :disabled t
  :elpaca nil
  :commands eglot eglot-ensure
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))
  :init
  (defvar +eglot--help-buffer nil)
  (defun +eglot-describe-at-point ()
    (interactive)
    "Request documentation for the thing at point."
    (eglot--dbind ((Hover) contents range)
                  (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                   (eglot--TextDocumentPositionParams))
                  (let ((blurb (and (not (seq-empty-p contents))
                                    (eglot--hover-info contents range)))
                        (hint (thing-at-point 'symbol)))
                    (if blurb
                        (with-current-buffer
                            (or (and (buffer-live-p +eglot--help-buffer)
                                     +eglot--help-buffer)
                                (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
                          (with-help-window (current-buffer)
                            (rename-buffer (format "*eglot-help for %s*" hint))
                            (with-current-buffer standard-output (insert blurb))
                            (setq-local nobreak-char-display nil)))
                      (display-local-help))))
    'deferred)
  :hook
  (eglot-managed-mode . (lambda () (general-define-key
                                    :states '(normal)
                                    :keymaps 'local
                                    "K" '+eglot-describe-at-point))))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-install-server)
  :preface
  (setq lsp-use-plists t)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  :custom
  (lsp-keymap-prefix nil)
  (lsp-completion-provider :none)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-insert-final-newline nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)

  :init
  (defun +update-completions-list ()
    (progn
      (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq-local completion-at-point-functions
                  (list (cape-super-capf
                         'non-greedy-lsp
                         #'yasnippet-capf
                         )))))
  :hook
  ;; (lsp-managed-mode . evil-normalize-keymaps)
  (lsp-managed-mode . (lambda () (general-define-key
                                  :states '(normal)
                                  :keymaps 'local
                                  "K" 'lsp-describe-thing-at-point)))
  (lsp-managed-mode . (lambda ()
                         (setq eldoc-documentation-functions
                           '(+flycheck-eldoc
                             t
                             lsp-eldoc-function))
                         (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)))
  (lsp-completion-mode . +update-completions-list)
  :general
  (+leader-def
    :keymaps 'lsp-mode-map
    :infix "c"
    "a" '(lsp-execute-code-action :wk "Code action")
    "D" '(lsp-find-references :wk "Find references")
    "i" '(lsp-find-implementation :wk "Find implementation")
    "k" '(lsp-describe-thing-at-point :wk "Show hover doc")
    "l" '(lsp-avy-lens :wk "Click lens")
    "o" '(lsp-organize-imports :wk "Organize imports")
    "q" '(lsp-workspace-shutdown :wk "Shutdown workspace")
    "r" '(lsp-rename :wk "Rename")
    "R" '(lsp-workspace-restart :wk "Restart workspace"))
  )

(use-package consult-lsp
  :after (lsp-mode consult)
  :general
  (+leader-def :keymaps 'lsp-mode-map
    "cs" '(consult-lsp-file-symbols :wk "Symbols")
    "cj" '(consult-lsp-symbols :wk "Workspace symbols")
    "cx" '(consult-lsp-diagnostics :wk "Workspace diagnostics")))

(use-package editorconfig
  :general
  (+leader-def
    "fc" #'editorconfig-find-current-editorconfig)
  :hook
  ((prog-mode text-mode conf-mode) . editorconfig-mode))

(use-package apheleia
  :commands apheleia-mode
  :general
  (+leader-def
    "cf" '(apheleia-format-buffer :wk "Format buffer"))
  :config
  (setf (alist-get 'erb-formatter apheleia-formatters)
        '("erb-format" "--print-width=140" filepath))
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(erb-mode . erb-formatter)))

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
  (flycheck-display-errors-function nil)
  (flycheck-help-echo-function nil)
  (flycheck-idle-change-delay 0.6)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :hook
  (prog-mode . flycheck-mode))

(use-package go-ts-mode
  :elpaca nil
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4)
  :init
  (defun +go-mode-setup ()
    (+add-pairs '((?` . ?`)))
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  :hook
  (go-ts-mode . +go-mode-setup)
  (go-ts-mode . apheleia-mode)
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
  :elpaca nil
  :init
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-server-display-inlay-hints t)
  :hook
  (rust-ts-mode . apheleia-mode)
  (rust-ts-mode . lsp-deferred))

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode "\\.scala\\'"
  :mode "\\.sbt\\'")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :hook
  (scala-mode . lsp-deferred))

(setq js-chain-indent t)
(setq js-indent-level 2)
(add-hook 'js-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'apheleia-mode)

(use-package css-mode
  :elpaca nil
  :custom
  (css-indent-offset 2)
  :hook
  (css-ts-mode . lsp-deferred)
  (css-ts-mode . apheleia-mode))

(use-package typescript-ts-mode
  :demand t
  :elpaca nil
  :hook
  ((tsx-ts-mode typescript-ts-mode) . apheleia-mode)
  ((tsx-ts-mode typescript-ts-mode) . lsp-deferred)
  )

(use-package web-mode
  :demand t
  :custom
  (web-mode-enable-html-entities-fontification t)
  (web-mode-markup-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-auto-close-style 1)
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
  (define-derived-mode erb-mode web-mode
    "HTML[erb]")
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . erb-mode))
  :hook
  (web-mode . apheleia-mode))

(use-package auto-rename-tag
  :hook ((js-ts-mode . auto-rename-tag-mode)
         (html-ts-mode . auto-rename-tag-mode)
         (typescript-ts-mode . auto-rename-tag-mode)
         (tsx-ts-mode . auto-rename-tag-mode)))

(use-package lsp-pyright
  :hook
  ((python-mode python-ts-mode) . lsp-deferred))

(use-package pytest
  :elpaca (:host github :repo "ionrock/pytest-el")
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
  :elpaca nil
  :hook
  (ruby-ts-mode . apheleia-mode)
  (ruby-ts-mode . lsp-deferred))

(use-package inf-ruby
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

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

(use-package elisp-mode
  :elpaca nil
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

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh")))

(use-package yaml-ts-mode
  :elpaca nil
  :mode "\\.ya?ml\\'")

(use-package json-ts-mode
  :elpaca nil
  :mode "\\.prettierrc\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :general
  (+local-leader-def
    :keymaps '(dockerfile-mode)
    "bb" #'dockerfile-build-buffer))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package git-modes
  :init
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package eat
  :commands eat
  :elpaca (eat :type git
               :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))

  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(use-package shell
  :elpaca nil
  :hook
  (shell-mode . evil-normal-state))

(use-package eshell
  :elpaca nil
  :general
  (+leader-def
    "oe"  #'eshell
    "oE"  #'eshell-new)
  (:states '(normal visual)
           :keymaps 'eshell-mode-map
           "<return>" #'evil-insert-resume)
  (:states '(insert)
   :keymaps 'eshell-mode-map
   "C-y" #'yank)
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
  :init
  (defun eshell-new ()
    "Open a new instance of eshell."
    (interactive)
    (popper-mode 0) ;; temporarily disable popper
    (eshell 'N)
    (popper-mode 1)
  )

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
               ;; remove fringe
               (set-window-fringes nil 0 0)
               (set-window-margins nil 1 nil)
               ;; scrolling
               (setq hscroll-margin 0)
               ;; Text wrapping
               (visual-line-mode +1)
               (set-display-table-slot standard-display-table 0 ?\ )))
  )

(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

;; Term
(use-package vterm
  :general
  (+leader-def
    "ot" #'vterm)
  (:states '(insert)
   :keymaps 'vterm-mode-map
   "C-y" #'vterm-yank)
  (:states '(normal visual)
           :keymaps 'vterm-mode-map
           "<return>" #'evil-insert-resume)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-always-compile-module t)
  (vterm-tramp-shells '(("docker" "/bin/sh")))
  (vterm-timer-delay 0.01)
  :config
  ;; Hide vterm install window
  (add-to-list
   'display-buffer-alist
   `(" \\*Install vterm\\*"
     (display-buffer-no-window)
     (allow-no-window . t)))

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
                               :mode '(shell-mode eshell-mode vterm-mode)
                               :sort 'visibility
                               :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources '+consult--source-term 'append))

  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local confirm-kill-processes nil)
              (setq-local hscroll-margin 0)
              (setq-local evil-insert-state-cursor 'box)
              ))
  )


(use-package multi-vterm
  :commands (multi-vterm multi-vterm-project)
  :general
  (+leader-def
    "oT" #'multi-vterm
    "pt" #'multi-vterm-project))

(use-package org
  :elpaca nil
  :init
  (setq org-directory "~/Dropbox/org/")
  :custom
  (org-adapt-indentation t)
  (org-cycle-separator-lines 2)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-fold-core-style 'overlays)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-edit-src-turn-on-auto-save t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  :config
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face)))

  (require 'org-indent)
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)
  (require 'org-tempo)
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
  (add-to-list 'org-structure-template-alist '("vb" . "src verb :wrap src ob-verb-response :op send get-body"))

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
  (org-mode . variable-pitch-mode))

(use-package evil-org
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :hook (org-agenda-mode . evil-org-mode)
  :general
  (:keymaps 'org-mode-map
            "M-O" 'evil-org-org-insert-subheading-below)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional todo calendar))
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
  :elpaca nil
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
   `(("i" "Inbox" entry (file "inbox.org")
      "* %?")
     ("t" "Tasks" entry (file "tasks.org")
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
          (:name "Next"
                 :todo "NEXT")
          (:name "Todo"
                 :todo "TODO")
          ;; (:name "Groceries"
          ;;        :file-path ,(expand-file-name "groceries.org" org-directory))
          ))
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package deadgrep
  :general
  (+leader-def
    "sg" #'deadgrep))

(setq help-window-select t)
(use-package helpful
  :hook
  (emacs-lisp-mode . (lambda () (setq-local evil-lookup-func 'helpful-at-point)))
  :bind
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-key]      . helpful-key)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command]  . helpful-command)
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

(use-package compile
  :elpaca nil
  :custom
  ;; Always kill current compilation process before starting a new one
  (compilation-always-kill t)
  (compilation-ask-about-save nil)  ; save all buffers on `compile'
  ;; Scroll compilation buffer
  (compilation-scroll-output 'first-error)
  :init
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  ;; (autoload 'comint-truncate-buffer "comint" nil t)
  ;; (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  :config
  (with-eval-after-load 'consult
    (defvar  +consult--source-compilation
      (list :name     "Compilation buffers"
            :narrow   ?c
            :category 'buffer
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :items (lambda () (consult--buffer-query
                               :predicate #'tabspaces--local-buffer-p
                               :mode '(compilation-mode)
                               :sort 'visibility
                               :as #'buffer-name))))

    (add-to-list 'consult-buffer-sources '+consult--source-compilation 'append))
  )

(use-package shell-command-x
  :defer 1
  :config
  ;; (setq shell-command-switch "-ic")
  (shell-command-x-mode 1))

(use-package envrc
  ;; :defer 1
  :init
  (envrc-global-mode 1))

(use-package docker
  :general
  (+leader-def
    "oD" #'docker))

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
  :general
  (+leader-def
   :keymaps 'org-mode-map
   "v" '(:ignore t :wk "verb")
   "vf" '(verb-send-request-on-point-other-window-stay :wk "Send request")
   "vr" '(verb-send-request-on-point-other-window-stay :wk "Send request other window")))

(use-package impostman
  :commands (impostman-import-file impostman-import-string))
