;; ****Performace
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s." (emacs-init-time))))

;; Increase the garbage collection (GC) threshold for faster startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Do not wast time checking the modification time of each file
(setq load-prefer-newer noninteractive)

;; Increase single chunk bytes to read from subprocess (default 4096)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Set initial buffer to fundamental-mode for faster load
(setq initial-major-mode 'fundamental-mode)

;; Inhibits fontification while receiving input
(setq redisplay-skip-fontification-on-input t)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive.
   native-comp-async-report-warnings-errors 'silent
   ;; Make native compilation happens asynchronously
   native-comp-jit-compilation t))

;; Slightly faster re-display
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)


;; ****Package system

;; Do not make installed packages available when Emacs starts
(setq package-enable-at-startup nil)

(setq straight-vc-git-default-clone-depth '(1 single-branch))

;; Use imenu with use-package
(setq use-package-enable-imenu-support t)

;; Bootstraping straight.el
;; See: github.com/radian-software/straight.el#bootstrapping-straightel
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

;; Configure `use-package'
(unless (require 'use-package nil t)
  (straight-use-package 'use-package))

(setq straight-use-package-by-default t)

;; if you use any :bind variant
(require 'bind-key) 

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 32 1024 1024)))



;; *****UI

;; C=ursor
(setq
 ;; Stretch cursor to the glyph width
 x-stretch-cursor t
 ;; Remove visual indicators from non selected windows
 highlight-nonselected-windows nil
 cursor-in-non-selected-windows nil)

;; No blinking cursor
(blink-cursor-mode -1)

;; Always prompt in minibuffer (no GUI)
(setq use-dialog-box nil)

;; Fonts
;; Set fonts
(set-face-attribute 'default nil :font "monospace" :height 105)
(set-face-attribute 'variable-pitch nil :family "PT Serif" :height 1.1)
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
(setq-default line-spacing 2)


;; White space
;; Show trailing whitespaces
(setq show-trailing-whitespace t)
;; Never mix, use only spaces
(setq indent-tabs-mode nil)
;; tab width 8 is too long
(setq-default tab-width 4)

;; Frame
;; Resize a frame by pixel
(setq frame-resize-pixelwise t)
;; Maximize frame when open
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Frame title
;; (setq
;;  frame-title-format (list '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
;;                           '(:eval
;;                             ;;                         (let ((name (projectile-project-name)))
;;                             (let ((name (project-name (project-current))))
;;                               (unless (string= "-" name)
;;                                 (format " — %s" name))))))

;; (use-package beframe
;;   :custom
;;   (beframe-rename-function nil)
;;   (beframe-create-frame-scratch-buffer nil)
;;   (beframe-functions-in-frames '(project-prompt-project-dir))
;;   :config
;;   (beframe-mode 1)
;;   (with-eval-after-load 'consult
;;     (consult-customize consult--source-buffer :hidden t :default nil)
;;     (defface beframe-buffer
;;       '((t :inherit font-lock-string-face))
;;       "Face for `consult' framed buffers.")

;;     (defvar consult--source-beframe
;;       (list :name     "Current frame buffers"
;;             :narrow   ?F
;;             :category 'buffer
;;             :history  'beframe-history
;;             :items    #'beframe-buffer-names
;;             :default   t
;;             :action   #'switch-to-buffer
;;             :state    #'consult--buffer-state))

;;     (add-to-list 'consult-buffer-sources 'consult--source-beframe))
;;   )


;; windows
;; Resize window combinations proportionally
(setq window-combination-resize t)

;; Window layout undo/redo
(winner-mode 1)

;; Line number
;; Show line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Disable line numbers for these modes
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq
 ;; Width for line numbers
 ;; display-line-numbers-width 4
 ;; Relative line numbering
 display-line-numbers-type 'relative)

;; Theme
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

;; Modelines
(use-package nerd-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-major-mode-icon nil
        doom-modeline-env-version nil
        doom-modeline-vcs-max-length 20
        doom-modeline-unicode-fallback t))

;; Show line, columns number in modeline
(line-number-mode 1)
(column-number-mode 1)
(setq mode-line-percent-position nil)

;; Show search count in modeline
(use-package anzu
  :config
  (global-anzu-mode +1))

;; =General UX
;; Escape once
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Save custom vars to separate file from init.el.
(setq-default custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))

;; ====== Scrolling ======
;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
(if (>= emacs-major-version 29)
    (pixel-scroll-precision-mode 1)
  (pixel-scroll-mode 1))
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

;; minibuffer
;; Enable saving minibuffer history
(savehist-mode 1)

;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1)

(setq 
 ;; Enable recursive calls to minibuffer
 enable-recursive-minibuffers t
 ;; Use completion in the minibuffer instead of definitions buffer; already use vertico, needed?
 ;; xref-show-definitions-function #'xref-show-definitions-completing-read)
 )


;; bells
(setq
 ;; Silent mode
 ring-bell-function #'ignore
 ;; Set to non-nil to flash!
 visible-bell nil)

;; Use y or n instead of yes or no
(setq use-short-answers t
      ;; Confirm before quitting
      confirm-kill-emacs #'y-or-n-p)

;; Move stuff to trash
(setq delete-by-moving-to-trash t)

;; ***** Editing
;; Use single space between sentences
(setq sentence-end-double-space nil)
;; Don't store duplicated entries
(setq history-delete-duplicates t)

;; ====== Editing ======
;; Hitting TAB behavior
(setq tab-always-indent 'complete)
;; Always add final newline
(setq require-final-newline t)

;; Wrap long lines
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'conf-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; Display long lines
(setq truncate-lines nil)

;; Remember cursor position in files
(save-place-mode 1)

;;; Why use anything but UTF-8?
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

;; pairs
;; Editor
(use-package elec-pair
  ;; TODO: refactor these
  :straight (:type built-in)
  :hook ((git-commit-mode . git-commit-add-electric-pairs)
         (markdown-mode . markdown-add-electric-pairs)
         (go-ts-mode . go-add-electric-pairs)
         (yaml-ts-mode . yaml-add-electric-pairs))
  :preface
  (defun git-commit-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`) (?= . ?=))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun markdown-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun go-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun yaml-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?\( . ?\)))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  :init
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (electric-pair-mode t))

;; clipboard
(setq 
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t)

;; files
;; Auto load files changed on disk
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

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
Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
            (set-auto-mode))))))

;; Better handling for files with so long lines
(global-so-long-mode 1)

;; Saving multiple files saves only in sub-directories of current project
(setq save-some-buffers-default-predicate #'save-some-buffers-root)

(setq
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t)

;; ====== Backups ======
(setq
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Disable making backup files
 make-backup-files nil)

;; recent files
(use-package recentf
  :straight (:type built-in)
  :init
  (setq
   ;; Increase the maximum number of saved items
   recentf-max-saved-items 500
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
  (recentf-mode 1))

;; (setq
;; Kill the shell buffer after exit
;; shell-kill-buffer-on-exit t)

(use-package compile
  :straight (:type built-in)
  :custom
  ;; Scroll compilation buffer
  (compilation-scroll-output t)
  ;; (compilation-skip-threshold 2)
  (compilation-auto-jump-to-first-error t)
  ;; Always kill current compilation process before starting a new one
  (compilation-always-kill t)
  :config
  ;; colorize compilation buffer
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))


;; key bindings
(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-ellipsis "..")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 5)
  (which-key-add-column-padding 1))

(use-package general
  ;; TODO: remove :after evil
  :config
  (general-auto-unbind-keys)
  ;; Set up some basic equivalents (like `general-nmap') with short named
  (general-evil-setup t)

  (general-create-definer +leader-def
    ;; The order of states matters, the last is prioritized
    :states '(visual normal)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer +local-leader-def
    :states '(visual normal)
    :keymaps 'local
    :prefix "SPC m")

  ;; Define the built-in global keybindings
  (+leader-def
    ;; ====== Top level functions ======
    "SPC" '(execute-extended-command :wk "M-x")
    ;; ">"   '(switch-to-next-buffer :wk "Next buffer")
    ;; "<"   '(switch-to-prev-buffer :wk "Previous buffer")
    ":"   '(pp-eval-expression :wk "Eval expression")
    "X"   #'org-capture
    "u"   '(universal-argument :wk "C-u")
    "!"   #'shell-command
    "|"   #'shell-command-on-region
    "RET" #'bookmark-jump
    ;; RET jump to bookmark
    ;; / search in project
    ;; *
    ;; .
    ;; ,
    ;; `'
    ;; ;
    ;; \

    ;; ====== Buffers ======
    "b"   '(nil :wk "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer
    "bd"  #'kill-this-buffer
    "bD"  #'kill-buffer
    "bi"  #'ibuffer
    ;; bI ibuffer workspafce
    "bo"  #'switch-to-buffer-other-window
    ;; "bu"  #'+sudo-save-buffer
    ;; save-buffer
    "bs"  #'save-buffer
    "bS"  #'save-some-buffers
    ;; "bM"  #'view-echo-area-messages
    "br"  '(revert-buffer :wk "Revert")
    "bR"  '(rename-buffer :wk "Rename")
    "bx"  #'scratch-buffer
    "bz"  #'bury-buffer

    ;; Bookmarks
    "k"  '(nil :wk "bookmark")
    "ki"  #'bookmark-set
    "kj"  #'bookmark-jump
    "kk"  #'list-bookmarks
    "kd"  #'bookmark-delete

    ;; ;; Files / Local variables
    ;; "bv"  '(nil :wk "locals")
    ;; "bvv" '(add-file-local-variable :wk "Add")
    ;; "bvV" '(delete-file-local-variable :wk "Delete")
    ;; "bvp" '(add-file-local-variable-prop-line :wk "Add in prop line")
    ;; "bvP" '(delete-file-local-variable-prop-line :wk "Delete from prop line")
    ;; "bvd" '(add-dir-local-variable :wk "Add to dir-locals")
    ;; "bvD" '(delete-dir-local-variable :wk "Delete from dir-locals")
    ;; "bvr"  '(nil :wk "reload dir-locals for...")
    ;; "bvrr" '(+dir-locals-reload-for-this-buffer :wk "This buffer")
    ;; "bvrd" '(+dir-locals-reload-for-all-buffers-in-this-directory :wk "All buffers in this directory")

    ;; ====== Code ======
    "c"  '(nil :wk "code")
    "cc" #'compile
    "cd" #'xref-find-definitions
    "cD" #'xref-find-references


    ;; ====== Files ======
    "f"   '(nil :wk "file")
    "fd"  #'dired
    "fD"  '(+delete-this-file :wk "Delete this file")
    ;; fe find in emacs.d project?
    "ff"  #'find-file
    "fg"  '((lambda () (interactive) (find-file "~/.gitconfig")) :wk "Edit .gitconfig") ;; use variable
    "fi"  '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit init.el") ;; use variable
    "fl"  #'locate
    "fr"  #'recentf
    "fR"  '(+rename-this-file :wk "Rename/move file")
    "fs"  #'save-buffer
    "fS"  '(write-file :wk "Save as ...")
    "fy"  '((lambda () (interactive) (kill-new (buffer-file-name)) (message "Copied %s to clipboard" (buffer-file-name))) :wk "Yank buffer file name")
    "fz"  '((lambda () (interactive) (find-file "~/.zshrc")) :wk "Edit zsh config") ;; use variable

    ;; ====== Git ======
    "g"   '(nil :wk "git")

    ;; ====== Help ======
    "h" '(nil :wk "help")
	"hb" #'about-emacs
	"he" #'view-echo-area-message
    "hg" #'general-describe-keybindings
	"hi" #'info
	"hI" #'info-display-manual
	"hm" #'describe-mode
	"hp" #'describe-package
    "h'" #'describe-char

    ;; ====== Insert ======
    ;; "i"   '(nil :wk "insert")
    ;; "iu"  '(insert-char :wk "Unicode char")
    ;; "ie"  `(,(when (>= emacs-major-version 29) #'emoji-search) :wk "Emoji")

    ;; ======  Mode specific a.k.a. "local leader" ======
    "m"   '(nil :wk "mode-specific")


    ;; ====== Applications (Open) ======
    "o"   '(nil   :wk "app/open")
    ;; "oa"  #'org-agenda
    ;; "of"  #'make-frame

    ;; ====== Project ======
    "p"   '(nil :wk "project")

    ;; ====== Quit/Session ======
    "q"   '(nil :wk "quit/session")
    "qf"  #'delete-frame
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'kill-emacs
    "qR"  #'restart-emacs
    ;; "ql"  #'desktop-read load last/from file session
    ;; "qL"  #'desktop-read load last/from file session
    ;; "qs"  #'desktop-save save session
    ;; "qS"  #'desktop-save save session to file


    ;; ====== Search ======
    "s"   '(nil :wk "search")
    "si" #'imenu
    ;; "sw"  '+webjump

    ;; ====== Toggle ======
    "t"   '(nil :wk "toggle")
    ;; F fullscreen
    "th"  #'load-theme
    ;; "tl"  #'toggle line number current buffer
    "tr"  #'read-only-mode

    ;; ====== Workspaces ======
    "TAB" '(nil :wk "workspace")
    )
  )


;; Evil
(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-fine-undo t
        evil-want-Y-yank-to-eol t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-kill-on-visual-paste nil
        evil-respect-visual-line-mode t
        evil-ex-interactive-search-highlight 'selected-window
        evil-visual-state-cursor 'hollow)
  :general
  (+leader-def
    "w" '(:keymap evil-window-map :wk "window"))
  :config
  (modify-syntax-entry ?_ "w")
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; TODO: change to general
  (define-key evil-motion-state-map ";" #'evil-ex)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  ;; TODO: change to general
  (evil-define-minor-mode-key 'normal 'flycheck-mode
    (kbd "[e") 'evil-collection-unimpaired-previous-error)
  (evil-define-minor-mode-key 'normal 'flycheck-mode
    (kbd "]e") 'evil-collection-unimpaired-next-error)
  )

(use-package evil-nerd-commenter
  :after (evil general)
  :commands evilnc-comment-operator
  :general
  (general-nvmap "gc" #'evilnc-comment-operator))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(eshell-mode vterm-mode)
        evil-escape-delay 0.25
        evil-escape-key-sequence "kj"))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package avy
  :commands evil-avy-goto-char-2
  :general
  (general-nmap "s" #'evil-avy-goto-char-2)
  :init
  (setq avy-background t))

(use-package evil-anzu
  :after (evil anzu))

;; undo
(use-package undo-fu
  :init
  (setq undo-limit 10000000
        undo-strong-limit 50000000
        undo-outer-limit 150000000)
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu)
    (evil-set-undo-system 'undo-fu)))

(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode 1))

;; help
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
  :config
  (use-package elisp-demos
	:config
	(advice-add 'helpful-update
				:after
				#'elisp-demos-advice-helpful-update))
  :general
  (+leader-def
    :infix "h"
	"k" #'helpful-key
	"c" #'helpful-macro
	"f" #'helpful-callable
	"v" #'helpful-variable
	"o" #'helpful-symbol
	"x" #'helpful-command
    "F" #'helpful-function))


;; Completion
(use-package orderless
  :config
  ;; (defun +orderless-dispatch-flex-first (_pattern index _total)
  ;;   (and (eq index 0) 'orderless-flex))
  ;; ;; Optionally configure the first word as flex filtered.
  ;; (add-hook 'orderless-style-dispatchers #'+orderless-dispatch-flex-first nil 'local)
  :custom
  (orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp))
  (completion-ignore-case t)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (orderless partial-completion basic)))
     )))

(use-package cape)
(use-package company)

(use-package yasnippet
  :commands (yas-minor-mode yas-expand)
  :hook
  (prog-mode . yas-minor-mode)
  :init
  (setq yas-triggers-in-field t)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  )

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :hook
  ((eshell-mode comint-mode) . (lambda ()
                                 (setq-local corfu-quit-at-boundary t
                                             corfu-quit-no-match t
                                             corfu-auto nil)
                                 (corfu-mode)))
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-cycle t)
  (corfu-min-width 25)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil) 
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
  :general
  ("C-s" 'consult-line)
  (+leader-def
    ;; search
    ;; "sa"  #'consult-org-agenda
    "sb"  #'consult-line
    "sB"  #'consult-line-multi
    "sf"  #'consult-find
    "sh"  #'consult-history
    "sp"  #'consult-ripgrep ;; project
    ;; j jumplist
    ;; kK doc/dash
    ;; lL jump link
    ;; m jump book mark
    ;; o search online
    "hI"  #'consult-info)
  :init
  (setq-default completion-in-region-function #'consult-completion-in-region)
  (setq consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (setq register-preview-function #'consult-register-format)
  ;; :config
  ;; (defvar  consult--source-compilation
  ;;   (list :name     "Compilation buffers"
  ;;         :narrow   ?c
  ;;         :category 'buffer
  ;;         :history  'beframe-history
  ;;         :state    #'consult--buffer-state
  ;;         :items
  ;;         (lambda ()
  ;;           (mapcar #'buffer-name
  ;;                   (seq-filter
  ;;                    (lambda (x)
  ;;                      (provided-mode-derived-p (buffer-local-value 'major-mode x) 'compilation-mode))
  ;;                    (beframe-buffer-list))))))

  ;; (add-to-list 'consult-buffer-sources 'consult--source-compilation 'append)
  )

(use-package consult-dir
  :commands consult-dir
  :init
  (setq consult-dir-shadow-filenames nil
        consult-dir-default-command 'consult-ripgrep)
  :general
  (+leader-def
    "sd" '(consult-dir :wk "search in directory"))
  (:keymaps 'minibuffer-local-completion-map
            "C-d" #'consult-dir))

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

  :bind
  (:map minibuffer-local-map
        ("C-." . 'embark-dwim)
        ("C-;" . 'embark-act))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (which-key-use-C-h-commands nil))

(use-package embark-consult
  :after (embark consult)
  :demand t
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
  :straight (:host github :repo "minad/vertico"
                   :files (:defaults "extensions/*")
                   :includes (vertico-directory))
  :hook
  (after-init . vertico-mode)
  :init
  (setq vertico-resize nil
        vertico-count 15)
  :general
  (+leader-def
    "'" '(vertico-repeat :wk "resume last search"))
  )

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

;; Git 
(use-package magit
  :general
  (+leader-def :infix "g"
    "b" #'magit-branch
    "B" #'magit-blame
    "c" #'magit-init
    "C" #'magit-clone
    "d" #'magit-diff-dwim
    "g" #'magit-status
    "l" #'magit-log)
  :init
  (setq magit-diff-refine-hunk t
        magit-revision-show-gravatars t
        magit-save-repository-buffers nil
        magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package diff-hl
  :defer 2
  :commands
  (diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk diff-hl-diff-goto-hunk)
  :hook
  (find-file    . diff-hl-mode)
  ;; (dired-mode   . diff-hl-dired-mode)
  ;; (vc-dir-mode  . diff-hl-dir-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (magit-pre-refresh . 'diff-hl-magit-pre-refresh)
  (magit-post-refresh . 'diff-hl-magit-post-refresh)
  :general
  (+leader-def
    "gs" '(diff-hl-stage-current-hunk :wk "stage hunk")
    "gh" '(diff-hl-diff-goto-hunk :wk "diff hunk")
    "g]" '(diff-hl-next-hunk :wk "next hunk")
    "g[" '(diff-hl-previous-hunk :wk "previous hunk")
    "gr" '(diff-hl-revert-hunk :wk "revert hunk"))
  :init
  (setq vc-git-diff-switches '("--histogram")
        diff-hl-flydiff-delay 0.5
        diff-hl-show-staged-changes nil
        diff-hl-draw-borders nil)
  :config
  (defun +vc-gutter-define-thin-bitmaps ()
    (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
  (advice-add 'diff-hl-define-bitmaps :override #'+vc-gutter-define-thin-bitmaps)
  (defun +vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (advice-add 'diff-hl-fringe-bmp-from-pos  :override #'+vc-gutter-type-at-pos-fn)
  (advice-add 'diff-hl-fringe-bmp-from-type :override #'+vc-gutter-type-at-pos-fn)
  (add-hook 'diff-hl-mode-hook
            (defun +vc-gutter-fix-diff-hl-faces-h ()
              (set-face-background 'diff-hl-insert nil)
              (set-face-background 'diff-hl-delete nil)
              (set-face-background 'diff-hl-change nil)))
  )

(use-package git-modes
  :defer 1
  :mode ("/.dockerignore\\'" . gitignore-mode))

;; programming
(setq eldoc-echo-area-use-multiline-p nil)
(use-package treesit
  :straight nil
  :init
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :hook (after-init . global-treesit-auto-mode)
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))

;; lsp
(use-package lsp-mode
  :hook
  (after-init . +lsp-auto-enable)
  (lsp-completion-mode . +update-completions-list)
  (lsp-managed-mode . (lambda ()
                        (general-nmap :keymaps 'local "K" 'lsp-describe-thing-at-point)))
  :preface
  (setq lsp-use-plists t)
  :commands (+lsp-auto-enable lsp lsp-deferred lsp-install-server)
  :custom
  (lsp-keymap-prefix nil)
  (lsp-completion-provider :none)
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-insert-final-newline nil)
  ;; maybe performace improve
  ;; (lsp-idle-delay 1.0)
  ;; (lsp-signature-auto-activate nil)
  ;; (lsp-trim-trailing-whitespace nil)
  :init
  (defcustom +lsp-auto-enable-modes
	'(python-mode python-ts-mode
                  rust-mode rust-ts-mode go-mode go-ts-mode
                  ruby-mode ruby-ts-mode
                  js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode
                  json-mode json-ts-mode js-json-mode)
	"Modes for which LSP-mode can be automatically enabled by `+lsp-auto-enable'."
	:group 'my-prog
	:type '(repeat symbol))

  (defun +lsp-auto-enable ()
	"Auto-enable LSP-mode in configured modes in `+lsp-auto-enable-modes'."
	(interactive)
	(dolist (mode +lsp-auto-enable-modes)
      (let ((hook (intern (format "%s-hook" mode))))
		(add-hook hook #'lsp-deferred))))

  (defun +update-completions-list ()
	(progn
      (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq-local completion-at-point-functions
                  (list (cape-super-capf
						 'non-greedy-lsp
						 ;; (cape-company-to-capf #'company-yasnippet)
						 )))))

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
	"r" '(lsp-rename :wk "Rename")
	"R" '(lsp-workspace-restart :wk "restart workspace"))
  )

(use-package consult-lsp
  :after consult lsp-mode
  :general
  (+leader-def :keymaps 'lsp-mode-map
    "cs" '(consult-lsp-file-symbols :wk "Symbols")))

;; It's actually annoying
(global-eldoc-mode -1)

;; flycheck
(use-package flycheck
  :preface
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  :custom
  (flycheck-idle-change-delay 1.0)
  (flycheck-display-errors-delay 0.25)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)

  ;; change it enable only
  (setq-default flycheck-disabled-checkers
                `(,@flycheck-disabled-checkers go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck))
  :general
  (+leader-def
    "cx" '(flycheck-list-errors :wk "list errors"))

  :hook
  (lsp-managed-mode . (lambda ()
        				(when (derived-mode-p 'go-ts-mode)
        				  (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))
  (prog-mode . flycheck-mode))

;; go
(use-package flycheck-golangci-lint
  :after (flycheck)
  :config
  (flycheck-add-mode 'golangci-lint 'go-ts-mode)
  :hook
  (go-ts-mode . flycheck-golangci-lint-setup))

(use-package go-ts-mode
  :straight (:type built-in)
  :init
  (setq go-ts-mode-indent-offset 4)
  :config
  (defun +go-mode-setup ()
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  (add-hook 'go-ts-mode-hook #'+go-mode-setup)
  )

(use-package gotest
  :after go-ts-mode
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

;; rust
(use-package rust-ts-mode
  :straight (:type built-in)
  :mode "\\.rs\\'"
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :init
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-server-display-inlay-hints t))

;; web
(setq js-chain-indent t)
(setq js-indent-level 2) ;; has package?
;; (setq css-indent-offset 2)

(use-package typescript-ts-mode
  :straight (:type built-in))

(use-package web-mode
  ;; :mode "\\.erb\\'"
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
  )

;; python
(use-package lsp-pyright
  :hook
  ((python-mode python-ts-mode) . lsp-deferred))

;; ruby
(use-package inf-ruby)
;; :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

(use-package ruby-end)

(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  ;; :init
  ;; (setq rspec-use-spring-when-possible nil)
  ;; (when (modulep! :editor evil)
  ;;   (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))
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

;; emacs lisp
(use-package elisp-mode
  :straight (:type built-in)
  :general
  (+local-leader-def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map racket-mode-map scheme-mode-map)
    "p" #'check-parens)
  :config
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
  :after elisp-mode 
  :custom
  (eros-eval-result-prefix "⟹ ")
  :config
  (eros-mode 1))

;; others
(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :straight nil)

(use-package json-ts-mode
  :straight nil
  :mode "\\.prettierrc\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package terraform-mode
  :mode "\\.tf\\'")

;; formatter
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package apheleia
  :general
  (+leader-def
    "cf" #'apheleia-format-buffer)
  :config
  (setf (alist-get 'erb-formatter apheleia-formatters)
        '("erb-format" filepath))
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(erb-mode . erb-formatter))
  :hook
  ((go-ts-mode rust-ts-mode ruby-ts-mode
               css-ts-mode web-mode erb-mode
               typescript-ts-mode tsx-ts-mode js-ts-mode
               emacs-lisp-mode) . apheleia-mode))

(use-package eshell
  :straight (:type built-in)
  :general
  (+leader-def
    "oe"  #'eshell
	"oE"  #'eshell-new)
  :init
  (defun eshell-new ()
	"Open a new instance of eshell."
	(interactive)
	(eshell 'N))
  )

(use-package eshell-z
  :after eshell
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

;; Term
(use-package vterm
  :general
  (+leader-def
    "ot" #'vterm)
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
  :init
  (setq vterm-timer-delay 0.01
        vterm-kill-buffer-on-exit t
        vterm-always-compile-module t
        vterm-max-scrollback 10000
        vterm-tramp-shells '(("docker" "/bin/bash")))
  :config
  ;; (with-eval-after-load 'consult
  ;;   (defvar  consult--source-term
  ;;     (list :name     "Terminal buffers"
  ;;           :narrow   ?t
  ;;           :category 'buffer
  ;;           :history  'beframe-history
  ;;           :state    #'consult--buffer-state
  ;;           :items
  ;;           (lambda ()
  ;;             (mapcar #'buffer-name
  ;;                     (seq-filter
  ;;                      (lambda (x)
  ;;                        (or
  ;;                         (eq (buffer-local-value 'major-mode x) 'shell-mode)
  ;;                         (eq (buffer-local-value 'major-mode x) 'eshell-mode)
  ;;                         (eq (buffer-local-value 'major-mode x) 'vterm-mode)))
  ;;                      (beframe-buffer-list))))))

  ;;   (add-to-list 'consult-buffer-sources 'consult--source-term 'append))

  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local confirm-kill-processes nil)
              (setq-local hscroll-margin 0)
              (setq-local evil-insert-state-cursor 'box)
              (evil-define-key 'insert vterm-mode-map (kbd "C-y") #'vterm-yank)
              (evil-insert-state)))

  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;; (use-package multi-vterm
;;   :commands (multi-vterm)
;;   :general
;;   (+leader-def
;;     "oT" #'multi-vterm))

(use-package envrc
  :hook
  (after-init . envrc-global-mode))

(use-package docker
  :general
  (+leader-def
    "oD" #'docker))

;; project
(use-package project
  :demand t
  :straight (:type built-in)
  :init
  (setq project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project")
        project-switch-commands 'project-dired)
  :config
  (+leader-def
    "p" '(:keymap project-prefix-map :wk "project")
	"pt" #'project-vterm
	))

;; command runner
(use-package helm-make)
(use-package run-command
  :config
  (require 'subr-x)
  (require 'map)
  (require 'seq)

  (defun run-command-recipe-package-json ()
    "Provide commands to run script from `package.json'.
Automatically detects package manager based on lockfile: npm, yarn, and pnpm."
    (when-let* ((project-dir
                 (locate-dominating-file default-directory "package.json"))
                (project-info
                 (with-temp-buffer
				   (insert-file-contents
                    (concat project-dir "package.json"))
				   (json-parse-buffer)))
                (package-manager
                 (cond
				  ((file-exists-p
                    (concat project-dir "pnpm-lock.yaml"))
				   "pnpm")
				  ((file-exists-p
                    (concat project-dir "yarn.lock"))
				   "yarn")
				  (t
				   "npm")))
                (scripts (map-keys (map-elt project-info "scripts"))))
	  (seq-map
	   (lambda (script)
         (list
		  :command-name script
		  :command-line (concat package-manager " run " script)
		  ;; :runner 'run-command-runner-vterm
		  :display script
		  :working-dir project-dir))
	   scripts)))

  (defun run-command-recipe-make ()
    "Provide commands to run Makefile targets.
Requires `helm-make' (https://github.com/abo-abo/helm-make) to
read Makefile targets, but does not require `helm' and can be
used with any of the selectors supported by `run-command'."

    (when (require 'helm-make nil t)
	  (when-let* ((project-dir
				   (locate-dominating-file default-directory "Makefile"))
				  (makefile (concat project-dir "Makefile"))
				  (targets (helm--make-cached-targets makefile)))
        (seq-map
         (lambda (target)
		   (list
            :command-name target
            :command-line (concat "make " target)
            :display target
            :working-dir project-dir))
         targets))))

  (setq run-command-default-runner 'run-command-runner-compile)
  (setq run-command-recipes '(run-command-recipe-make run-command-recipe-package-json))

  :general
  (+leader-def
    "rc" #'run-command))

;; org
(use-package org
  :straight (:type built-in)
  :custom
  (org-startup-folded 'content)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 2)
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-edit-src-content-indentation 2)
  :config
  (require 'org-indent)
  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
				  (org-level-2 . 1.1)
				  (org-level-3 . 1.05)
				  (org-level-4 . 1.0)
				  (org-level-5 . 1.1)
				  (org-level-6 . 1.1)
				  (org-level-7 . 1.1)
				  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; Get rid of the background on column views
  ;; (set-face-attribute 'org-column nil :background nil)
  ;; (set-face-attribute 'org-column-title nil :background nil)

  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode))

;; (defun +org-mode-visual-fill ()
;;   (setq visual-fill-column-width 110
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :defer t
;;   :hook (org-mode . +org-mode-visual-fill))

(use-package evil-org
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :general
  (:keymaps 'org-mode-map
			;; "M-o" 'evil-org-org-insert-heading-below
			"M-O" 'evil-org-org-insert-subheading-below)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-superstar
  :init
  (setq org-superstar-special-todo-items t
        org-superstar-remove-leading-stars t)
  :hook (org-mode . org-superstar-mode))

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


;; help/helpful window placement
(add-to-list
 'display-buffer-alist
 '((lambda (buffer _) (with-current-buffer buffer
                        (seq-some (lambda (mode)
                                    (derived-mode-p mode))
								  '(help-mode helpful-mode))))
   (display-buffer-reuse-mode-window display-buffer-in-direction)
   (direction . bottom)
   (dedicated . t) 
   (mode . (help-mode helpful-mode))
   (window-height . 0.5)
   ))

(use-package popper
  :general
  ("C-\\" 'popper-toggle-latest)
  ("C-`"  'popper-cycle)
  ("C-~" 'popper-toggle-type)
  (:keymaps 'vterm-mode-map
            "C-\\" 'popper-toggle-latest)
  :init
  (setq popper-window-height 0.35)
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
		  vterm-mode  
		  "\\*rake-compilation\\*$"
		  "\\*rspec-compilation\\*$"
		  ))
  (popper-mode +1)
  (popper-echo-mode +1))    

(use-package windresize
  :init
  (setq windresize-default-increment 5)
  :general
  ("S-C-<return>" 'windresize)
  :commands windresize)

