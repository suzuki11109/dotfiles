;; emacs dot file
;; youtube how to use bookmarks
;; github forge clone, pull request

;; needed learn tools
;; try react development/ run server/ run json-api server/setup editorconfig
;; verb restclient calls
;; docker pull/manage containers/build/tramp exec
;; k8s need minikube or k3s learn/run built image/tramp exec
;; projectile ruby-on-rails needed?

;; frog-jump-buffer jump to compilation/tests/terminal/files

;; lispy
;; clojure
;; elixir

;; server-mode systemd?
;; eshell in new frame command/key-shortcut
;; verb
;; sql

;; python project runner
;; web-mode
;; angular
;; rails erb
;; comint?

;; rails project runner
;; editorconfig
;; org
;; minimal modeline
;; dashboard
;; detache?

;; dot files magit?
;; obsidian
;; literate config
;; elpaca or build-in
;; calendar
;; dired & image
;; emoji/unicode
;; slides
;; web generator
;; ftp/tramp/docker tramp
;; open doom snippets pr
;; mail
;; cape-yasnippet

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive.
   native-comp-async-report-warnings-errors 'silent
   ;; Make native compilation happens asynchronously
   native-comp-deferred-compilation t))

;; Add lisp directories to `load-path'.

;; Package system
(setq
 ;; straight-repository-branch "develop"
 ;; Do not clone all project history, just the last worktree (--depth 1)
 straight-vc-git-default-clone-depth '(1 single-branch))

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

(setq
 ;;
 straight-use-package-by-default t
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t)

;; if you use any :bind variant
(require 'bind-key) 

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s." (emacs-init-time))))

;; Garbage collect
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 32 1024 1024)))

;; Defaults maybe in emacs
;; Inhibit startup message in echo area the brutal way!
;; The `inhibit-startup-echo-area-message' variable is very restrictive, there
;; is only one unique way of setting it right!
;; See: reddit.com/r/emacs/comments/6e9o4o/comment/di8q1t5
(fset 'display-startup-echo-area-message #'ignore)

;;; Why use anything but UTF-8?
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

;; No blinking cursor
(blink-cursor-mode -1)

(setq
 ;; ====== Default behavior ======
 ;; Inhibit startup message
 inhibit-startup-message t
 ;; Do not ring
 ring-bell-function #'ignore
 ;; Set to non-nil to flash!
 visible-bell nil
 ;; Increase the large file threshold to 50 MiB
 large-file-warning-threshold (* 50 1024 1024)
 ;; Set initial buffer to fundamental-mode for faster load
 initial-major-mode 'fundamental-mode
 ;; Always prompt in minibuffer (no GUI)
 use-dialog-box nil
 ;; Use y or n instead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs #'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Save files only in sub-directories of current project
 save-some-buffers-default-predicate #'save-some-buffers-root
 ;; Use single space between sentences
 sentence-end-double-space nil
 ;; Move stuff to trash
 delete-by-moving-to-trash t
 ;; Select help window for faster quit!
 help-window-select t
 ;; More info on completions
 ;; completions-detailed t
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t
 ;; Use completion in the minibuffer instead of definitions buffer
 xref-show-definitions-function #'xref-show-definitions-completing-read
 ;; Enable recursive calls to minibuffer
 enable-recursive-minibuffers t
 ;; Kill the shell buffer after exit
 shell-kill-buffer-on-exit t

 ;; ====== Performances ======
 ;; Don’t compact font caches during GC
 inhibit-compacting-font-caches t
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (* 1024 1024) ;; 1mb
 ;; Inhibits fontification while receiving input
 redisplay-skip-fontification-on-input t

 ;; ====== Aesthetics and UI ======
 ;; Do force frame size to be a multiple of char size
 frame-resize-pixelwise t
 ;; Stretch cursor to the glyph width
 x-stretch-cursor t
 ;; Remove visual indicators from non selected windows
 highlight-nonselected-windows nil
 cursor-in-non-selected-windows nil
 ;; Show trailing whitespaces
 show-trailing-whitespace t
 ;; Resize window combinations proportionally
 window-combination-resize t
 ;; Enable time in the mode-line
 display-time-string-forms '((propertize (concat 24-hours ":" minutes)))
 ;; Relative line numbering
 display-line-numbers-type 'relative
 ;; No ugly button for widgets
 widget-image-enable nil
 ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
 prettify-symbols-unprettify-at-point t
 ;; Make tooltips last a bit longer (default 10s)
 tooltip-hide-delay 20
 ;; Use small frames to display tooltips instead of the default OS tooltips
 use-system-tooltips nil
 ;; Frame title
 frame-title-format (list '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
                          '(:eval
 ;;                         (let ((name (projectile-project-name)))
                            (let ((name (project-name (project-current))))
                              (unless (string= "-" name)
                                (format " — %s" name)))))

 ;; ====== Undo ======
 ;; 10MB (default is 160kB)
 undo-limit 10000000
 ;; 50MB (default is 240kB)
 undo-strong-limit 50000000
 ;; 150MB (default is 24MB)
 undo-outer-limit 150000000

 ;; ====== Editing ======
 ;; Hitting TAB behavior
 tab-always-indent nil
 ;; Default behavior for `whitespace-cleanup'
 ;; whitespace-action '(cleanup auto-cleanup)
 ;; End files with newline
 require-final-newline t
 ;; Enable Drag-and-Drop of regions
 mouse-drag-and-drop-region t
 ;; Enable Drag-and-Drop of regions from Emacs to external programs
 mouse-drag-and-drop-region-cross-program t

 ;; ====== Backups ======
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Disable making backup files
 make-backup-files nil

 ;; ====== Scrolling ======
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
 scroll-margin 3
 ;; Better scrolling on Emacs29+, specially on a touchpad
 pixel-scroll-precision-use-momentum t

 ;; ====== Recent files ======
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
     (* any)))

 ;; ====== Timestamps ======
 ;; Do enable time-stamps
 ;; time-stamp-active t
 ;; ;; Check the first 12 buffer lines for Time-stamp: <>
 ;; time-stamp-line-limit 12
 ;; ;; Timestamp format
 ;; time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S"

 ;; ====== Auto-Saving ======
 ;; Enable auto-save (use `recover-file' or `recover-session' to recover)
 auto-save-default t
 ;; Include big deletions
 auto-save-include-big-deletions t
 ;; Set file naming transform
 auto-save-file-name-transforms (list
                                 ;; Prefix tramp autosaves with "tramp-"
                                 (list
                                  "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                  (concat auto-save-list-file-prefix "tramp-\\2") t)
                                 ;; Local autosaves
                                 (list ".*" auto-save-list-file-prefix t))

 ;; ====== Compilation ======
 ;; Scroll compilation buffer
 compilation-scroll-output t ; 'first-error can be a good option
 ;; Always kill current compilation process before starting a new one
 compilation-always-kill t
 ;; Skip visited messages on compilation motion commands
 compilation-skip-visited t
 ;; Keep it readable
 compilation-window-height 12)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Slightly faster re-display
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq-default
 ;; ====== Buffer-local variables ======
 ;; Display long lines
 truncate-lines nil
 ;; Default fill column width
 fill-column 80
 ;; Never mix, use only spaces
 indent-tabs-mode nil
 ;; Width for line numbers
 display-line-numbers-width 4
 ;; Small tab is enough!
 tab-width 4
 ;; Save buffer status
 desktop-save-buffer t)

;; Escape once
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Kill `term' buffer on exit (reproduce a similar behavior to `shell's
;; `shell-kill-buffer-on-exit').
(advice-add
 'term-sentinel :around
 (defun +term--kill-after-exit-a (orig-fn proc msg)
   (if (memq (process-status proc) '(signal exit))
       (let ((buffer (process-buffer proc)))
         (apply orig-fn (list proc msg))
         (kill-buffer buffer))
     (apply orig-fn (list proc msg)))))

;; Kill the minibuffer when switching by mouse to another window.
;; Adapted from: trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html

;; ====== Tweaks on file save ======
;; Update time stamp (if available) before saving a file.
;; (add-hook 'before-save-hook 'time-stamp)

;; Auto-remove trailing white spaces before saving for modes defined in

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

;; ====== Modes enabled locally, mainly for `prog-mode', `conf-mode' and `text-mode' ======
;; Show line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Wrap long lines
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'conf-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; Window layout undo/redo (`winner-undo' / `winner-redo')
(winner-mode 1)

;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
(if (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1)
  (pixel-scroll-mode 1))

;; Replace selection after start typing
(delete-selection-mode 1)

;; Enable `recentf-mode' to remember recent files
(recentf-mode 1)

;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1)

;; Save place in files
(save-place-mode 1)

;; Enable saving minibuffer history
(savehist-mode 1)

;; Auto load files changed on disk
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Show line, columns number in mode-line
(line-number-mode 1)
(column-number-mode 1)
(setq mode-line-percent-position nil)

;; Better handling for files with so long lines
(global-so-long-mode 1)


;; Save Emacs state from one session to another
;; (desktop-save-mode 1)

;; Global SubWord mode
(global-subword-mode 1)

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

;; key bindings
(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-ellipsis "..")
  (which-key-idle-delay 1.0)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 5)
  (which-key-add-column-padding 1)
  ;; (which-key-max-display-columns nil)
  ;; Allow a key binding to be modified by multiple rules in
  ;; `which-key-replacement-alist'
  ;; (which-key-allow-multiple-replacements t)
  :config
  (which-key-setup-side-window-bottom))

(use-package general
  ;; PERF: Loading `general' early make Emacs very slow on startup.
  ;; :after evil
  :demand t
  :config
  ;; Advise `define-key' to automatically unbind keys when necessary.
  (general-auto-unbind-keys)
  ;; Set up some basic equivalents (like `general-nmap') with short named
  ;; aliases (like `nmap') for VIM mapping functions.
  (general-evil-setup t)

  ;; Global leader
  (general-create-definer +leader-def
    ;; The order of states matters, the last is prioritized
    :states '(visual normal)
    :keymaps 'override
    :prefix "SPC")

  ;; Local leader
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
    ;; "bm"  '(nil :wk "bookmark")
    ;; "bmm"  #'bookmark-set
    ;; "bmd"  #'bookmark-delete
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
    "fi"  '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit init.el") ;; use variable
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
    "h" '(:keymap help-map :wk "help")
    "h'" #'describe-char
    "hg" #'general-describe-keybindings

    ;; ====== Insert ======
    ;; "i"   '(nil :wk "insert")
    ;; "iu"  '(insert-char :wk "Unicode char")
    ;; "ie"  `(,(when (>= emacs-major-version 29) #'emoji-search) :wk "Emoji")

    ;; ======  Mode specific a.k.a. "local leader" ======
    "m"   '(nil :wk "mode-specific")


    ;; ====== Applications (Open) ======
    "o"   '(nil   :wk "app/open")
    ;; "oa"  #'org-agenda
    "of"  #'make-frame
    "oe"  #'eshell

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
  :preface
  (setq evil-want-keybinding nil)
  :custom
  (evil-visual-state-cursor 'hollow)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-respect-visual-line-mode t)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  (+leader-def
    "w" '(:keymap evil-window-map :wk "window"))

  ;; Use `evil-search' instead of `isearch'
  (evil-select-search-module 'evil-search-module 'evil-search)
  
  (define-key evil-motion-state-map ";" #'evil-ex)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-operator
  :init
  (nvmap "gc" #'evilnc-comment-operator))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (evil-escape-excluded-major-modes '(treemacs-mode eshell-mode vterm-mode))
  (evil-escape-delay 0.25)
  ;; The default "fd" interfere with the "f" (bound to `evil-snipe-f') binding.
  (evil-escape-key-sequence "kj"))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package avy
  :commands evil-avy-goto-char-2
  :init
  (nmap "s" #'evil-avy-goto-char-2)
  :custom
  (avy-background t))

;; UI
(setq-default line-spacing 2)

(use-package all-the-icons
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; (doom-modeline-time-icon nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-env-version nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-unicode-fallback t))

(use-package catppuccin-theme
  :demand t
  :config
  (load-theme 'catppuccin t))

(use-package anzu
  :defer 2
  :config
  (global-anzu-mode +1))


;; Completion

(use-package orderless
  :demand t
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

(use-package cape
  :demand t)

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :hook ((eshell-mode comint-mode) . (lambda ()
                         (setq-local corfu-quit-at-boundary t
                                     corfu-quit-no-match t
                                     corfu-auto nil)
                         (corfu-mode)))
  :custom
  (corfu-auto t) 
  (corfu-cycle t) 
  (corfu-min-width 25)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-on-exact-match nil)
  (corfu-preselect 'first)
  )

(use-package kind-icon
  :after corfu
  :demand t
  :custom
  ;; (kind-icon-default-style '(:padding 0
  ;;                            :stroke 0
  ;;                            :margin 0
  ;;                            :radius 0
  ;;                            :height 0.8
  ;;                            :scale 1.05)) ; Fix the scaling/height
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :custom
  ;; Use `consult-xref' for `xref-find-references'
  (xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (register-preview-function #'consult-register-format)
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
  ;; :init
  ;; (global-set-key (kbd "C-s") #'consult-line)
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
    ;; o seardch online
    ;; s search with thing at point?

    ;; code
    "cx"  #'consult-flymake
    ;; help
    "hI"  #'consult-info)
  :init
  (setq consult-narrow-key "<")
  (setq-default completion-in-region-function #'consult-completion-in-region)
  :config
  (defvar  consult--source-compilation
    (list :name     "Compilation buffers"
          :narrow   ?c
          :category 'buffer
          :history  'beframe-history
          :state    #'consult--buffer-state
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (provided-mode-derived-p (buffer-local-value 'major-mode x) 'compilation-mode))
                     (beframe-buffer-list))))))

  (add-to-list 'consult-buffer-sources 'consult--source-compilation 'append)
  )

(use-package consult-dir
  :commands consult-dir
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-default-command 'consult-ripgrep)
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

  :bind
  (:map minibuffer-local-map
        ("C-." . 'embark-dwim)
        ("C-;" . 'embark-act))
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package vertico
  :straight (:host github :repo "minad/vertico"
                   :files (:defaults "extensions/*")
                   :includes (vertico-directory))
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 15)
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

;; Editor

(use-package smartparens
  :hook ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Tempel

;; Undo
(use-package undo-fu
  :demand t
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu)
    (evil-set-undo-system 'undo-fu)))

(use-package undo-fu-session
  :after undo-fu
  :demand t
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode 1))


;; Git comeback diff gitcommit
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
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package forge
  :after magit
  :custom
  (auth-sources '("~/.authinfo")))

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
  :custom
  (vc-git-diff-switches '("--histogram"))
  (diff-hl-flydiff-delay 0.5)
  (diff-hl-show-staged-changes nil)
  (diff-hl-draw-borders nil)
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

;; Programming
;; Use built-in `treesit' when available
(use-package treesit
  :straight (:type built-in)
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Install all languages when calling `treesit-auto-install-all'
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))

(use-package lsp-mode
  :hook (after-init . +lsp-auto-enable)
  :preface
  (setq lsp-use-plists t)
  :commands (+lsp-auto-enable lsp lsp-deferred lsp-install-server)
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix nil)
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil)
  (lsp-idle-delay 1.0)
  ;; Less intrusive UI
  (lsp-eldoc-render-all nil) ; clangd docs looks ugly on eldoc-box!
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-modeline-diagnostics-enable nil)
  ;; Maybe set to nil and enable modes manually (`lsp-completion-mode',
  ;; `lsp-modeline-diagnostics-mode', ...)
  ;; (lsp-auto-configure t)
  ;; Those stuff should be managed by Emacs's builtins (whitespace-cleanup, treesit, ...)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-trim-trailing-whitespace nil)
  (lsp-insert-final-newline nil)
  (lsp-trim-final-newlines nil)
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

  :config
  (+leader-def
    :keymaps 'lsp-mode-map
    :infix "c"
    "a" '(lsp-execute-code-action :wk "Code action")
    "i" '(lsp-find-implementation :wk "Find implementation")
    "k" '(lsp-describe-thing-at-point :wk "Show hover doc")
    "l" '(lsp-avy-lens :wk "Click lens")
    "o" '(lsp-organize-imports :wk "Organize imports")
    "q" '(lsp-workspace-shutdown :wk "Shutdown workspace")
    "r" '(lsp-rename :wk "Fename")
    "R" '(lsp-workspace-restart :wk "restart workspace"))
  ;; x flycheck error
  (evil-define-minor-mode-key 'normal 'lsp-mode
	(kbd "K") 'lsp-describe-thing-at-point)
  )

(use-package consult-lsp
  :after consult lsp-mode
  :general
  (+leader-def :keymaps 'lsp-mode-map
    "cs" '(consult-lsp-file-symbols :wk "Symbols")))

(use-package go-ts-mode
  :demand t
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

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode ("\\.rs\\'" . rust-ts-mode)
  :init
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-server-display-inlay-hints t))

(setq js-chain-indent t)
(setq js-indent-level 2)
(setq css-indent-offset 2)

(use-package typescript-ts-mode
  :demand t
  :straight (:type built-in))

(use-package lsp-pyright
  :hook
  ((python-mode python-ts-mode) . lsp-deferred))

(use-package inf-ruby)

  ;; :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))
(use-package ruby-end
  :demand t)

;; Emacs Lisp
(use-package elisp-mode
  :straight (:type built-in)
  :hook (emacs-lisp-mode . (lambda () (setq-local tab-width 8))) ;; to view built-in packages correctly
  :init
  (+local-leader-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map racket-mode-map scheme-mode-map)
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
  :demand t
  :custom
  (eros-eval-result-prefix "⟹ ")
  :config
  (eros-mode 1))


;; ==========

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :straight (:type built-in))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package terraform-mode
  :mode "\\.tf\\'")

;; Formatter
(use-package apheleia
  :general
  (+leader-def
    "cf" #'apheleia-format-buffer)
  :hook
  ((go-ts-mode rust-ts-mode
    typescript-ts-mode tsx-ts-mode js-ts-mode) . apheleia-mode)
  )

(use-package helpful
  ;; :demand t
  :hook (emacs-lisp-mode . (lambda () (setq-local evil-lookup-func 'helpful-at-point)))
  :bind
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-key]      . helpful-key)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command]  . helpful-command)
  :general
  (+leader-def
    :infix "h"
    "F" #'helpful-function))

;; Term
(use-package vterm
  :commands vterm
  :general
  (+leader-def
    "oT" #'vterm)
  :custom
  (vterm-timer-delay 0.01)
  (vterm-kill-buffer-on-exit t)
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000)
  (vterm-tramp-shells '(("docker" "/bin/bash")))
  :config

  (with-eval-after-load 'consult
    (defvar  consult--source-vterm
      (list :name     "Vterm buffers"
            :narrow   ?v
            :category 'buffer
            :history  'beframe-history
            :state    #'consult--buffer-state
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (seq-filter
                       (lambda (x)
                         (eq (buffer-local-value 'major-mode x) 'vterm-mode))
                       (beframe-buffer-list))))))

    (add-to-list 'consult-buffer-sources 'consult--source-vterm 'append))

  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local confirm-kill-processes nil)
              (setq-local hscroll-margin 0)
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package envrc
  :hook
  (after-init . envrc-global-mode))

(use-package docker
  :general
  (+leader-def
    "oD" #'docker))

;; Project
(use-package project
  :straight (:type built-in)
  :demand t
  :custom
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project"))
  (project-switch-commands 'project-dired)
  :config
  (+leader-def
    "p" '(:keymap project-prefix-map :wk "project"))
  )


;; Windows

;; Show *Warnings* at bottom
(add-to-list
 'display-buffer-alist
 `("*Warnings*"
   (display-buffer-reuse-window display-buffer-in-direction)
   (direction . bottom) ;; bottom (above below...)
   (dedicated . t) ;; Close when finished
   (reusable-frames . visible) ;;
   (window-height . 0.3)))

;;  bottom terminals
(add-to-list
 'display-buffer-alist
 `(,(rx (seq "*" (or "eshell" "vterm" "terminal") "*"))
   ;; (display-buffer-reuse-window display-buffer-at-bottom)
   (display-buffer-reuse-window display-buffer-in-direction)
   (direction . bottom) ;; bottom (above below...)
   (dedicated . t) ;; Close when finished
   (reusable-frames . visible)
   (window-height . 0.3)
   ))

;; Frames
(use-package beframe
  :demand t
  :custom
  (beframe-rename-function nil)
  (beframe-create-frame-scratch-buffer nil)
  (beframe-functions-in-frames '(project-prompt-project-dir))
  :config
  (beframe-mode 1)

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; (defface beframe-buffer
    ;;   '((t :inherit font-lock-string-face))
    ;;   "Face for `consult' framed buffers.")

    (defvar consult--source-beframe
      (list :name     "Current frame buffers"
            :narrow   ?F
            :category 'buffer
            :history  'beframe-history
            :items    #'beframe-buffer-names
            :default   t
            :action   #'switch-to-buffer
            :state    #'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'consult--source-beframe))
  )

(use-package helm-make
  :demand t)

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

  (setq run-command-recipes '(run-command-recipe-make run-command-recipe-package-json))

  :custom
  (run-command-default-runner 'run-command-runner-compile)
  :general
  (+leader-def
    "rc" #'run-command))
