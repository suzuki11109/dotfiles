;;; init.el --- init file -*- lexical-binding: t; no-byte-compile: t; -*-

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(setq package-install-upgrade-built-in nil)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;; Set exec-path
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Load general for :general
(use-package general
  :config
  (general-create-definer +leader-def
    :states '(visual normal motion)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer +local-leader-def
    :states '(visual normal motion)
    :keymaps 'local
    :prefix "SPC m")
  )

;; For on-first-* hooks
(use-package on
  :vc (:url "https://github.com/ajgrf/on.el" :branch "master"))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if init-file-debug
       (progn ,@forms)
     (let ((message-log-max nil))
       (with-temp-message (or (current-message) "") ,@forms))))

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max) t)))

;; Save custom vars to separate file from init.el.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

(use-package gcmh
  :defer 1
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 32 1024 1024))
  :config
  (gcmh-mode 1))

(setq mac-command-modifier 'meta)

(use-package general
  :ensure nil
  :after evil
  :config
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (+leader-def
    "SPC" '(execute-extended-command :wk "M-x")
    ":"   '(pp-eval-expression :wk "Eval expression")
    "X"   #'org-capture
    "u"   '(universal-argument :wk "C-u")

    "<tab>"   '(nil :wk "workspaces")

    "b"   '(nil :wk "buffer")
    "bb"  '(switch-to-buffer :wk "Switch buffer")
    "bd"  '(kill-current-buffer :wk "Kill this buffer")
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
    "cd" '(xref-find-definitions :wk "Go to definitions")
    "cD" '(xref-find-definitions-other-window :wk "Go to definitions other window")
    "cR" '(xref-find-references :wk "Find references")

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
    "fW"  '((lambda () (interactive) (dired "~/Downloads")) :wk "Go to download directory")
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

    "l"  '(nil :wk "package")
    "ll"  #'list-packages
    "lu"  #'package-upgrade
    "lU"  #'package-upgrade-all

    "m"  '(nil :wk "mode-specific")

    "n"  '(nil :wk "notes")
    "na" #'org-agenda
    "nf" '((lambda () (interactive)
              (let ((default-directory org-directory))
                (call-interactively 'find-file))) :wk "Find notes")
    "nm" #'org-tags-view
    "nt" #'org-todo-list

    "o"   '(nil   :wk "app/open")
    "oa"  #'org-agenda
    "of"  #'select-frame-by-name
    "oF"  #'make-frame
    "ol"  #'browse-url
    "ow"  #'download-file
    "o-"  #'dired-jump

    "p"  '(nil :wk "project")
    "pp" #'project-switch-project

    "q"  '(nil :wk "quit/session")
    "qf" '(delete-frame :wk "Delete this frame")
    "qq" '(save-buffers-kill-terminal :wk "Quit emacs")
    "qR" '(restart-emacs :wk "Restart emacs")

    ;;; <leader> r --- remote

    "s"  '(nil :wk "search")
    "si" #'imenu
    "st" #'dictionary-lookup-definition
    "sT" #'dictionary

    "t"  '(nil :wk "toggle")
    "tc" '(global-display-fill-column-indicator-mode :wk "Fill column indicator")
    "tf" '(toggle-frame-fullscreen :wk "Frame fullscreen")
    "th" '(load-theme :wk "Load theme")
    "tr" '(read-only-mode :wk "Read-only mode")
    )
  )

(use-package which-key
  :ensure nil
  :custom
  (which-key-ellipsis "..")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-side-window-slot -10)
  (which-key-min-display-lines 5)
  :hook
  (on-first-input . which-key-mode)
  )

;; Confirm before quitting
(setq confirm-kill-emacs #'y-or-n-p)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

;; Better unique buffer names for files with the same base name.
(setq uniquify-buffer-name-style 'forward)

;; No beep or blink
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Disable GUIs because they are inconsistent across systems
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Always prompt in minibuffer (no GUI)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

(setq auto-window-vscroll nil)

(setq hscroll-margin 2
      hscroll-step 1)

(setq scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t)

(use-package pixel-scroll
  :ensure nil
  :hook
  ((prog-mode text-mode conf-mode) . pixel-scroll-precision-mode))

;; Don't stretch the cursor to fit wide characters, it is disorienting,
(setq x-stretch-cursor nil)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; No blinking cursor
(blink-cursor-mode -1)

;; Remember cursor position in files
(use-package saveplace
  :ensure nil
  :hook
  (on-first-file . save-place-mode))

;; Frame title
(setq frame-title-format
      (list
       '(:eval
         (let ((project (project-current)))
           (when project
             (format "%s — " (project-name project)))))
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
       ))

;; Resize a frame by pixel
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;; Window layout undo/redo
(use-package winner
  :ensure nil
  :hook
  (on-first-buffer . winner-mode))

(use-package ace-window
  :defer t
  :custom-face
  (aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0))))
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (aw-dispatch-always t)
  )

(use-package popper
  :general-config
  ("C-`" 'popper-toggle)
  ("C-\\"  'popper-cycle)
  ("C-~" 'popper-toggle-type)
  :config
  (defun +popup/quit-window ()
    (interactive)
    (if (eq popper-popup-status 'popup)
        (popper-kill-latest-popup)
      (quit-window)))

  (global-set-key [remap quit-window] #'+popup/quit-window)

  (setq popper-window-height 0.40)
  (setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          ("\\*Compile-Log\\*" . hide)
          "\\*Async Shell Command\\*$"
          compilation-mode
          comint-mode
          "^\\*term.*\\*$" term-mode
          "^\\*shell.*\\*$" shell-mode shell-command-mode
          "^\\*eshell" eshell-mode "-eshell\\*$"
          "^\\*eat" eat-mode "-eat\\*$"
          "^\\*vterm" vterm-mode "-vterm\\*$"
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
          "\\*xref\\*"
          "\\*eldoc\\*"
          "\\magit-process:"
          inf-ruby-mode
          sbt-mode
          forge-post-mode
          "\\*Embark Export:"
          "\\*Embark Collect:"
          flutter-mode
          "\\*LSP Dart tests\\*"
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

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :hook ((org-mode markdown-mode) . (lambda () (display-line-numbers-mode 0)))
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start t))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package catppuccin-theme
  :init
  (setq catppuccin-height-title-3 1.1)
  (load-theme 'catppuccin t))

(add-hook 'after-init-hook (lambda ()
    (set-face-attribute 'default nil :family "JetBrains Mono" :height 130)
    (set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0)
    (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 1.0)

    (set-face-attribute 'mode-line-inactive nil :family (face-attribute 'variable-pitch :family) :height 1.0)
    (set-face-attribute 'mode-line-active nil :family (face-attribute 'variable-pitch :family) :height 1.0)
    (set-face-attribute 'mode-line nil :family (face-attribute 'variable-pitch :family))

    (set-face-attribute 'tab-bar nil :family (face-attribute 'variable-pitch :family))
    ))

(setq-default line-spacing 0.3)

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease)
  :general
  ("M--" 'default-text-scale-decrease)
  ("M-=" 'default-text-scale-increase))

(use-package nerd-icons
  :demand t
  :general-config
  (+leader-def
    "in" '(nerd-icons-insert :wk "Nerd icons"))
  :custom
  (nerd-icons-scale-factor 1.0))

(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 0)
  (doom-modeline-height 36)
  (doom-modeline-buffer-file-name-style 'buffer)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-modal nil)
  (doom-modeline-check-simple-format t)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-indent-info t)
  :config
  (doom-modeline-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)

  (doom-modeline-def-modeline 'main
    '(matches eldoc bar workspace-name window-number modals follow buffer-info remote-host buffer-position selection-info word-count parrot)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process check time " "))

  ;; (doom-modeline-def-modeline 'vcs
  ;;   '(matches bar window-number modals buffer-info remote-host selection-info parrot)
  ;;   '(compilation misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process time " "))

;;   (defun +modeline-flymake-counter (type)
;;     "Compute number of diagnostics in buffer with TYPE's severity.
;; TYPE is usually keyword `:error', `:warning' or `:note'."
;;     (let ((count 0))
;;       (dolist (d (flymake--project-diagnostics))
;;         (when (= (flymake--severity type)
;;                  (flymake--severity (flymake-diagnostic-type d)))
;;           (cl-incf count)))
;;       (when (cl-plusp count)
;;         (number-to-string count))))

;;   (defvar +modeline-flymake-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map [mode-line down-mouse-1] 'flymake-show-project-diagnostics)
;;       map)
;;     "Keymap to display on Flymake indicator.")

;;   (defmacro +modeline-flymake-type (type &optional face)
;;     "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
;;     `(defun ,(intern (format "+modeline-flymake-%s" type)) ()
;;        (when-let ((count (+modeline-flymake-counter
;;                           ,(intern (format ":%s" type)))))
;;          (concat
;;           (propertize count
;;                       'face ',(or face type)
;;                       'mouse-face 'mode-line-highlight
;;                       ;; FIXME 2023-07-03: Clicking on the text with
;;                       ;; this buffer and a single warning present, the
;;                       ;; diagnostics take up the entire frame.  Why?
;;                       'local-map +modeline-flymake-map
;;                       'help-echo "mouse-1: projects diagnostics")))))

;;   (+modeline-flymake-type error)
;;   (+modeline-flymake-type warning)
;;   (+modeline-flymake-type note success)

;;   (defvar-local +modeline-flymake
;;       `(:eval
;;         (when (and (bound-and-true-p flymake-mode)
;;                    (mode-line-window-selected-p))
;;           ;; See the calls to the macro `+modeline-flymake-type'
;;           '(:eval (s-join (propertize "/" 'face 'shadow)
;;                           (remove nil (list (+modeline-flymake-error)
;;                                             (+modeline-flymake-warning)
;;                                             (+modeline-flymake-note)))))
;;           ))
;;     "Mode line construct displaying `flymake-mode-line-format'.
;; Specific to the current window's mode line.")
;;   (add-to-list 'mode-line-misc-info +modeline-flymake)
  :hook
  (after-init . doom-modeline-mode))

;; Show search count in modeline
(use-package anzu
  :after (evil)
  :config
  (global-anzu-mode 1))

(use-package evil-anzu
  :after (evil anzu))

(use-package project
  :ensure nil
  :custom
  (project-switch-commands 'project-dired)
  :general-config
  (+leader-def
    "p" '(:ignore t :wk "project")
    "pp" #'project-switch-project
    "pb" #'project-switch-to-buffer
    "pd" #'project-dired
    "pD" #'project-forget-project
    "pe" #'project-eshell
    "pf" #'project-find-file
    "pF" #'project-or-external-find-file
    "pk" #'project-kill-buffers
    ))

(use-package tab-bar
  :ensure nil
  :commands (tab-bar-mode)
  :general-config
  (+leader-def
    "<tab><tab>" #'tab-bar-switch-to-tab
    "<tab>l" #'tab-bar-switch-to-recent-tab
    "<tab>n" #'tab-bar-switch-to-next-tab
    "<tab>p" #'tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-close-tab-select 'recent)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-format '(tab-bar-format-tabs #'+tab-bar-suffix))
  (tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)
  :config
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
  )

;; (use-package tabspaces
;;   :custom
;;   (tab-bar-new-tab-choice "*scratch*")
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "scratch")
;;   (tabspaces-include-buffers '("*dashboard*" "*Messages*"))
;;   (tabspaces-initialize-project-with-todo t)
;;   :general-config
;;   (+leader-def
;;     "<tab>1" #'tab-bar-switch-to-default-tab
;;     "<tab>b" #'tabspaces-switch-to-buffer
;;     "<tab>k" #'tabspaces-kill-buffers-close-workspace
;;     "<tab><tab>" #'tab-bar-switch-to-tab
;;     "<tab>s" #'tabspaces-switch-or-create-workspace
;;     "<tab>t" #'tabspaces-switch-buffer-and-tab
;;     "<tab>n" #'tab-bar-switch-to-next-tab
;;     "<tab>p" #'tab-bar-switch-to-prev-tab)
;;   (+leader-def
;;     "pp" #'tabspaces-open-or-create-project-and-workspace)
;;   :config
;;   (tabspaces-mode 1)
;;   (tab-bar-mode 1)
;;   (tab-bar-rename-tab tabspaces-default-tab) ;; Rename intial tab to default tab

;;   ;; tab-name not exists
;;   ;;  add to map, use simple name
;;   ;; tab-name exists & same project path
;;   ;;  use simple name
;;   ;; tab-name exists & diff project path
;;   ;;  rename existing tab, use complex name
;;   (defun tabspaces-generate-descriptive-tab-name (project-path existing-tab-names)
;;     "Generate a unique tab name from the PROJECT-PATH checking against EXISTING-TAB-NAMES."
;;     (let* ((parts (reverse (split-string (directory-file-name project-path) "/")))
;;            (base-name (car parts))
;;            (parent-dir (nth 1 parts))
;;            (grandparent-dir (nth 2 parts))
;;            (simple-tab-name base-name)
;;            (complex-tab-name (if parent-dir
;;                                  (format "%s (%s/%s)" base-name (or grandparent-dir "") parent-dir)
;;                                base-name)))
;;       (if (member simple-tab-name existing-tab-names)
;;           (let ((existing-path (rassoc simple-tab-name tabspaces-project-tab-map)))
;;             (when (not (string= (car existing-path) project-path))
;;               ;; Generate a new complex name for the existing conflict
;;               (let ((new-name-for-existing (tabspaces-generate-complex-name (car existing-path))))
;;                 ;; Rename the existing tab
;;                 (tabspaces-rename-existing-tab simple-tab-name new-name-for-existing)
;;                 ;; Update the map with the new name for the existing path
;;                 (setcdr existing-path new-name-for-existing))
;;               ;; Use the complex name for the new tab to avoid future conflicts
;;               complex-tab-name)
;;             simple-tab-name)
;;         ;; No conflict, add to map and use the simple name
;;         (progn
;;           (add-to-list 'tabspaces-project-tab-map (cons project-path simple-tab-name))
;;           simple-tab-name))))


;;   (with-eval-after-load 'consult
;;     (consult-customize consult--source-buffer :hidden t :default nil)

;;     (defvar consult--source-workspace
;;       (list :name     "Workspace Buffers"
;;             :narrow   ?w
;;             :history  'buffer-name-history
;;             :category 'buffer
;;             :state    #'consult--buffer-state
;;             :default  t
;;             :items    (lambda () (consult--buffer-query
;;                                   :predicate (lambda (x) (and (tabspaces--local-buffer-p x) (not (popper-popup-p x))))
;;                                   :sort 'visibility
;;                                   :as #'buffer-name))))
;;     (add-to-list 'consult-buffer-sources 'consult--source-workspace))

;;   (defun tab-bar-switch-to-default-tab ()
;;     (interactive)
;;     (tab-bar-switch-to-tab tabspaces-default-tab))
;;   )

(use-package perspective
  :custom
  (persp-show-modestring nil)
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :general-config
  (+leader-def
    "<tab><tab>" #'persp-switch
    "<tab>b" #'persp-switch-to-buffer*
    "<tab>k" #'persp-kill-current
    "pp" #'persp-switch-project)
  :preface
  (defun persp-switch-project (directory)
    "Switch to project DIRECTORY.
If DIRECTORY exists in a pespective, select it.  Otherwise switch to
the project in DIRECTORY."
    (interactive (list (funcall project-prompter)))
    (project--remember-dir directory)
    (let ((name (file-name-nondirectory (directory-file-name directory))))
      (if (not (member name (persp-names)))
          (progn
            (persp-switch name)
            (project-switch-project directory))
        (persp-switch name))))

  (defun +persp-names-sorted-by-created ()
    "Always sort persps by created time from left to right."
    (let ((persps (hash-table-values (perspectives-hash))))
      (mapcar 'persp-name
                     (sort persps (lambda (a b)
                                    (time-less-p (persp-created-time a)
                                                 (persp-created-time b)))))))
  (defun persp-kill-current ()
    "Kill current perspecitve."
    (interactive)
    (persp-kill (persp-current-name)))
  :config
  (advice-add 'persp-names :override #'+persp-names-sorted-by-created)
  (persp-mode 1)

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
                                  :predicate (lambda (x) (and (persp-is-current-buffer x) (not (popper-popup-p x))))
                                  :sort 'visibility
                                  :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  )

(use-package perspective-tabs
  :after perspective
  :vc (:url "https://git.sr.ht/~woozong/perspective-tabs")
  :config
  (perspective-tabs-mode 1))

;; Move stuff to trash
(setq delete-by-moving-to-trash t)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
(use-package files
  :ensure nil
  :init
  (setq create-lockfiles nil
        make-backup-files nil)

  (setq auto-save-default t
        auto-save-include-big-deletions t
        auto-save-list-file-prefix (expand-file-name "auto-save/" user-emacs-directory)
        tramp-auto-save-directory  (expand-file-name "tramp-auto-save/" user-emacs-directory)
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t)))
  )

;; Auto load files changed on disk
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 2)
  :hook
  (on-first-file . global-auto-revert-mode))

;;;###autoload
(defun +delete-this-file (&optional forever)
  "Delete the file associated with `current-buffer'.
If FOREVER is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let ((file (or (buffer-file-name)
                       (user-error "Current buffer is not visiting a file")))
             ((y-or-n-p "Delete this file? ")))
    (delete-file file (not forever))
    (kill-buffer (current-buffer))))

;;;###autoload
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

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Suppress large file opening confirmation
(setq large-file-warning-threshold nil)

;; Persistent scratch
(setq remember-notes-buffer-name "*scratch*"
      initial-buffer-choice (lambda ()
                              (kill-buffer remember-notes-buffer-name)
                              (remember-notes)))

(use-package recentf
  :ensure nil
  :defer 1
  :commands recentf-open-files
  :config
  (setq
   recentf-filename-handlers '(abbreviate-file-name)
   recentf-max-saved-items 200
   recentf-auto-cleanup 300)

  ;; Anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))
  (quiet! (recentf-mode 1))

  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  )

(when IS-MAC
  (setq dired-use-ls-dired nil))

(use-package dired
  :ensure nil
  :commands dired
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-listing-switches "-ahl")
  (dired-kill-when-opening-new-dired-buffer t))

;; Dired fontlock
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

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
  (dired-do-revert-buffer t)
  (dired-vc-rename-file t)
  :config
  (setf (alist-get "\\.tar\\.gz\\'" dired-compress-file-suffixes)
        '("" "tar -xzf %i --one-top-level")))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :hook
  (on-first-file . global-hl-todo-mode))

(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Wrap long lines
(use-package visual-line-mode
  :ensure nil
  :hook
  (on-first-buffer . global-visual-line-mode))

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring easier to peruse
(setq kill-do-not-save-duplicates t)

 ;; Save existing clipboard text into the kill ring before replacing it.
(setq save-interprogram-paste-before-kill t)

(use-package evil
  :defer .2
  :custom
  (evil-want-keybinding nil)
  (evil-v$-excludes-newline t)
  (evil-mode-line-format nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-symbol-word-search t)
  (evil-goto-definition-functions '(evil-goto-definition-xref
                                    evil-goto-definition-imenu
                                    evil-goto-definition-semantic
                                    evil-goto-definition-search))
  :general-config
  (+leader-def
    "bN"  '(evil-buffer-new :wk "New empty buffer")
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
  (evil-set-initial-state 'shell-command-mode 'normal)

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
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "q" nil)
  )

(use-package evil-nerd-commenter
  :after evil
  :general-config
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

(use-package evil-matchit
  :after evil
  :config
  (defun evilmi-jsx-get-tag ()
    (evilmi-html-get-tag))

  (defun evilmi-jsx-jump (info num)
    (jtsx-jump-jsx-element-tag-dwim))

  (evilmi-load-plugin-rules '(html-ts-mode) '(template simple html))
  (evilmi-load-plugin-rules '(jtsx-tsx-mode jtsx-jsx-mode) '(simple javascript jsx))
  (global-evil-matchit-mode 1))

(use-package avy
  :after evil
  :general-config
  (:states '(normal)
           "s" #'evil-avy-goto-char-2)
  :custom
  (avy-background t))

(delete-selection-mode 1)

(use-package electric-pair-mode
  :ensure nil
  :custom
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

(use-package paren
  :ensure nil
  :hook
  (on-first-buffer . show-paren-mode)
  :init
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package undo-fu
  :custom
  (undo-limit 400000)
  (undo-strong-limit 3000000)
  (undo-outer-limit 48000000))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode)
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package minibuffer
  :ensure nil
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :config
  ;; Use y or n instead of yes or no
  (setq use-short-answers t)

  ;; Show current key-sequence in minibuffer
  (setq echo-keystrokes 0.02)

  ;; Show recursion depth in minibuffer
  (minibuffer-depth-indicate-mode 1)

  ;; Enable recursive calls to minibuffer
  (setq enable-recursive-minibuffers t)

  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  )

(use-package savehist
  :ensure nil
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval nil)
  (savehist-additional-variables '(kill-ring register-alist search-ring regexp-search-ring comint-input-ring))
  (history-delete-duplicates t)
  :hook
  (on-first-input . savehist-mode)
)

(use-package orderless
  :demand t
  :preface
  (defun +orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun +lsp-mode-setup-completion ()
    ;; (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
    ;;      '(orderless))
    (add-hook 'orderless-style-dispatchers #'+orderless-dispatch-flex-first nil 'local)
    ;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
    )
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)

  ;; :config
  ;; (add-to-list
  ;;  'completion-styles-alist
  ;;  '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  ;; (setq completion-styles '(orderless basic))
  ;; (setq completion-category-defaults nil)
  ;; (setq completion-category-overrides '((file (styles basic-remote orderless partial-completion))
  ;;                                       ))
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  :hook
  (lsp-completion-mode . +lsp-mode-setup-completion)
  )

(use-package vertico
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p) ;; hide commands that does not work
  (vertico-resize nil)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :general-config
  (+leader-def
    "." '(vertico-repeat-select :wk "Resume previous search"))
  :hook
  (on-first-input . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode 1))

(use-package consult
  :after vertico
  :demand t
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap evil-show-marks]               . consult-mark)
  ([remap imenu]                         . consult-imenu)
  ([remap Info-search]                   . consult-info)
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
  :preface
  (defun consult-ripgrep-in-dir ()
    "Search with `rg' for files in DIR selected from prompt"
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'consult-ripgrep))
  :general-config
  (+leader-def
    "sb"  #'consult-line
    "sB"  #'consult-line-multi
    "sd"  #'consult-ripgrep-in-dir
    "sf"  #'consult-find
    "sI"  #'consult-imenu-multi
    "sp"  #'consult-ripgrep
    "hI"  #'consult-info)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; narrow key without delay
  (defun immediate-which-key-for-narrow (fun &rest args)
    (let* ((refresh t)
           (timer (and consult-narrow-key
                       (memq :narrow args)
                       (run-at-time 0.05 0.05
                                    (lambda ()
                                      (if (eq last-input-event (elt consult-narrow-key 0))
                                          (when refresh
                                            (setq refresh nil)
                                            (which-key--update))
                                        (setq refresh t)))))))
      (unwind-protect
          (apply fun args)
        (when timer
          (cancel-timer timer)))))
  (advice-add #'consult--read :around #'immediate-which-key-for-narrow)
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell
                                          consult-dir-sources)))
          (eshell/cd (substring-no-properties
                      (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs)))))))
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
                (x (user-error "embark category %S doesn't support writable export" x)))))
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

  (general-define-key
   :keymaps 'embark-file-map
   "o" (+embark-ace-action find-file))
  (general-define-key
   :keymaps 'embark-buffer-map
   "o" (+embark-ace-action switch-to-buffer))
  (general-define-key
   :keymaps 'embark-general-map
   "D" #'xref-find-definitions-other-window)
  :general
  (:keymaps 'minibuffer-local-map
            "C-c C-e" #'+embark-export-write)
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
  ;; (on-first-input . global-corfu-mode)
  ((prog-mode text-mode conf-mode) . corfu-mode)
  (eshell-mode . corfu-enable-in-shell)
  (minibuffer-setup . corfu-enable-in-shell)
  :preface
  ;; (defun corfu-enable-in-minibuffer ()
  ;;   "Enable Corfu in the minibuffer."
  ;;   (when (local-variable-p 'completion-at-point-functions)
  ;;     (setq-local corfu-auto nil) ;; Enable/disable auto completion
  ;;     (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
  ;;                 corfu-popupinfo-delay nil)
  ;;     (corfu-mode 1)))

  (defun corfu-enable-in-shell ()
    (setq-local corfu-auto nil)
    (corfu-mode 1))
  :custom
  (text-mode-ispell-word-completion nil)
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-count 14)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match 'show)
  ;; (global-corfu-modes
  ;;  '((not help-mode eat-mode) t))
  ;; :general-config
  ;; (:keymaps 'corfu-map
  ;;           (kbd "TAB") 'corfu-insert
  ;;           [(tab)] 'corfu-insert)
  :config
  (set-face-attribute 'corfu-default nil :family (face-attribute 'default :family))
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  ;; (setq global-corfu-minibuffer
  ;;       (lambda ()
  ;;         (not (or (bound-and-true-p mct--active)
  ;;                  (bound-and-true-p vertico--input)
  ;;                  (eq (current-local-map) read-passwd-map)))))
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
  :general-config
  (+leader-def
    "is" '(yas-insert-snippet :wk "Snippet"))
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

(use-package magit
  :defer .3
  :general-config
  (+leader-def :infix "g"
    "b" #'magit-branch-checkout
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
  (git-commit-summary-max-length 72)
  (git-commit-style-convention-checks '(overlong-summary-line))

  (magit-auto-revert-mode nil) ;; does not need because global-auto-revert-mode is enabled
  (transient-default-level 5)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-show-gravatars t)
  (magit-revision-insert-related-refs nil)
  (magit-bury-buffer-function #'magit-mode-quit-window)

  :config
  (global-git-commit-mode 1)
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (when (and (bound-and-true-p evil-mode)
                         (bobp) (eolp))
                (evil-insert-state))))

  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

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
  :custom
  (forge-add-default-bindings nil)
  :config
  (transient-append-suffix 'forge-dispatch "c f"
    '("c m" "merge pull request" forge-merge))
  :general-config
  ;; (+leader-def
  ;;   :keymaps '(magit-mode-map)
  ;;   "gw" 'forge-browse)
  (general-define-key
    :keymaps 'forge-topic-list-mode-map
    "q" #'kill-current-buffer)
  )

(use-package smerge-mode
  :ensure nil
  :after magit
  :general-config
  (+leader-def
    "gm" 'hydra-smerge/body)
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
    ;; ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
    ("q" nil :color blue))
  :hook
  (find-file . (lambda ()
                 (unless (bound-and-true-p smerge-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward "^<<<<<<< " nil t)
                       (smerge-mode 1)))))))

(use-package browse-at-remote
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^git\\.xspringas\\.com$" :type "gitlab"))
  :general
  (+leader-def
    :keymaps '(prog-mode-map text-mode-map conf-mode-map)
    "gw" #'browse-at-remote)
)

(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0.6)

(use-package treesit
  :ensure nil
  :preface
  (defun treesit-install-all-language-grammers ()
    "Build and install the tree-sitter language grammar libraries

for all languages configured in `treesit-language-source-alist'."
    (interactive)
    (dolist (source treesit-language-source-alist)
      (unless (treesit-ready-p (car source))
        (treesit-install-language-grammar (car source)))))
  :init
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          ;; (dart "https://github.com/UserNobody14/tree-sitter-dart")
          (go "https://github.com/tree-sitter/tree-sitter-go" "master")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (php "https://github.com/tree-sitter/tree-sitter-php")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
  )

;; Use only spaces
(setq-default indent-tabs-mode nil)
;; Tab width 8 is too long
(setq-default tab-width 2)
;; Hitting TAB behavior
(setq tab-always-indent nil)
;; Delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Use single space between sentences
(setq sentence-end-double-space nil)
;; Always add final newline
(setq require-final-newline t)

(use-package apheleia
  :commands apheleia-mode
  :general-config
  (+leader-def
    "cf" '(apheleia-format-buffer :wk "Format buffer"))
  :config
  (setf (alist-get 'erb-formatter apheleia-formatters)
        '("erb-format" "--print-width=140" filepath))
  (add-to-list 'apheleia-mode-alist '(erb-mode . erb-formatter))
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
      '(ruby-standard))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier-markdown))
  )

(use-package editorconfig
  :general-config
  (+leader-def
    "fc" '(editorconfig-find-current-editorconfig :wk "Open project editorconfig"))
  :hook (on-first-file . editorconfig-mode))

(setq xref-prompt-for-identifier nil)

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-install-server)
  :preface
  (defun +update-completions-list ()
    (progn
      (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq-local completion-at-point-functions
                  (list (cape-capf-super #'non-greedy-lsp #'yasnippet-capf)))))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ))

  ;; (lsp-register-client (make-lsp-client
  ;;                       :new-connection (lsp-stdio-connection '("dart" "language-server" "--client-id" "emacs.lsp-dart"))
  ;;                       :activation-fn (lsp-activate-on "dart")
  ;;                       :priority 1
  ;;                       :server-id 'dart-analysis-server))

  :custom
  (lsp-keymap-prefix nil)
  (lsp-completion-provider :none)
  ;; (lsp-diagnostics-provider :flymake)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-folding nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-auto-execute-action nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-disabled-clients '(rubocop-ls))
  (lsp-pylsp-plugins-ruff-enabled t)
  (lsp-clients-typescript-prefer-use-project-ts-server t)
  (lsp-clients-typescript-preferences '(:importModuleSpecifierPreference "non-relative" :includeCompletionsForImportStatements nil))
  (lsp-typescript-suggest-complete-js-docs nil)
  ;; :jsxAttributeCompletionStyle "none"
  (lsp-javascript-implicit-project-config-check-js t)
  (lsp-javascript-suggest-complete-js-docs nil)
  :hook
  (lsp-managed-mode . (lambda () (general-define-key
                                  :states '(normal visual)
                                  :keymaps 'local
                                  "K" 'lsp-describe-thing-at-point)))
  (lsp-completion-mode . +update-completions-list)
  :general-config
  (+leader-def
    "ca" '(lsp-execute-code-action :wk "Code action")
    "ci" '(lsp-find-implementation :wk "Find implementation")
    "ck" '(lsp-describe-thing-at-point :wk "Show hover doc")
    "cl" '(lsp-avy-lens :wk "Click lens")
    "co" '(lsp-organize-imports :wk "Organize imports")
    "cQ" '(lsp-workspace-restart :wk "Restart workspace")
    "cq" '(lsp-workspace-shutdown :wk "Shutdown workspace")
    "cr" '(lsp-rename :wk "Rename")
    )
  )

(use-package consult-lsp
  :after (consult lsp-mode)
  :general-config
  (+leader-def
    "cj" '(consult-lsp-symbols :wk "Workspace symbols")
    "cx" '(consult-lsp-diagnostics :wk "Workspace diagnostics")))

(use-package flycheck
  :config
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
  (flycheck-checkers nil)
  (flycheck-idle-change-delay 1.0)
  ;; (flycheck-display-errors-delay 0.25)
  (flycheck-display-errors-function nil)
  ;; (flycheck-help-echo-function nil)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :hook
  (flycheck-mode . (lambda ()
                     (add-hook 'eldoc-documentation-functions #'+flycheck-eldoc 0 t)))
  )

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
  (go-ts-mode . apheleia-mode)
  (go-ts-mode . +go-mode-setup)
  (go-ts-mode . lsp-deferred)
  )

(use-package gotest
  :after go-ts-mode
  :custom
  (go-test-verbose t)
  :general-config
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

(use-package dart-mode
  :vc (:url "https://github.com/emacsorphanage/dart-mode")
  :mode "\\.dart\\'"
  :hook
  (dart-mode . apheleia-mode)
  (dart-mode . (lambda ()
                 (setq-local syntax-propertize-function nil)))
  :config
  (add-hook 'dart-mode-hook 'lsp-deferred)
  )

(use-package flutter
  :init
  (put 'flutter-run-args 'safe-local-variable #'stringp)
  :general
  (+local-leader-def
    :keymaps '(dart-mode-map flutter-mode-map)
    "f" '(:ignore t :wk "flutter")
    "ff" #'flutter-run-or-hot-reload
    "fq" #'flutter-quit
    "fr" #'flutter-hot-reload
    "fR" #'flutter-hot-restart
    "t" '(:ignore t :wk "test")
    "ts" #'flutter-test-at-point
    "tf" #'flutter-test-current-file
    "ta" #'flutter-test-all
    )
  :preface
  (defun +flutter-hot-reload ()
    "Run `flutter-hot-reload' only if flutter-mode is running."
    (when (and (fboundp 'flutter--running-p) (flutter--running-p))
      (flutter-hot-reload)))
  (defun +flutter-mode-setup ()
    (add-hook 'after-save-hook '+flutter-hot-reload nil t))
  :hook
  (dart-mode . +flutter-mode-setup)
  )

(use-package lsp-dart
  :hook
  (dart-mode . lsp-deferred)
  :custom
  (lsp-dart-test-tree-on-run nil)
  (lsp-dart-test-pop-to-buffer-on-run t)
  (lsp-dart-line-length 120)
  (lsp-dart-main-code-lens nil)
  (lsp-dart-test-code-lens nil)
  :config
  ;; workaround for dart not returning completions after "."
  (advice-add 'lsp-completion--looking-back-trigger-characterp :around
              (defun lsp-completion--looking-back-trigger-characterp@fix-dart-trigger-characters (orig-fn trigger-characters)
                (funcall orig-fn
                         (if (and (derived-mode-p 'dart-mode) (not trigger-characters))
                             ["." "(" "$"]
                           trigger-characters))))

  ;; switch to evil-mode after using Wrap with widget actions
  (advice-add 'lsp--execute-code-action :around
              (defun +lsp-dart-wrap-code-action-insert-mode (orig-fn &rest args)
                (let* ((first-arg (nth 0 args))
                       (result (apply orig-fn args))
                       (action-name (plist-get first-arg :title)))
                  (when (and (derived-mode-p 'dart-mode)
                             (bound-and-true-p evil-mode)
                             (string= "Wrap with widget..." action-name))
                    (evil-insert-state))
                  result)))

  )

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :ensure nil
  :hook
  (rust-ts-mode . lsp-deferred)
  (rust-ts-mode . apheleia-mode))

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2)
  :hook
  (css-ts-mode . lsp-deferred)
  (css-ts-mode . apheleia-mode))

(use-package emmet-mode
  :custom
  (emmet-indentation 2)
  :config
  (add-to-list 'emmet-jsx-major-modes 'jtsx-tsx-mode)
  (add-to-list 'emmet-jsx-major-modes 'jtsx-jsx-mode)
  :hook
  ((jtsx-tsx-mode jtsx-jsx-mode) . emmet-mode)
  (html-ts-mode . emmet-mode)
  (web-mode . emmet-mode))

(use-package jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :bind
  ([remap comment-dwim] . jtsx-comment-dwim)
  :custom
  (js-chain-indent t)
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  :preface
  (defun +jsx-comment-or-uncomment-region (beg end)
    (cond
     ((jtsx-jsx-attribute-context-p)
      (let* ((comment-start "/* ")
             (comment-end " */")
             (comment-use-syntax nil)
             (comment-start-skip "\\(?:/\\*+\\)\\s-*")
             (comment-end-skip "\\s-*\\(\\*+/\\)"))
        (evilnc-comment-or-uncomment-region-internal beg end)))
     ((jtsx-jsx-context-p)
      (let* ((comment-start "{/* ")
             (comment-end " */}")
             (comment-use-syntax nil)
             (comment-start-skip "\\(?:{?/\\*+\\)\\s-*")
             (comment-end-skip "\\s-*\\(\\*+/}?\\)"))
        (evilnc-comment-or-uncomment-region-internal beg end)))
     (t (evilnc-comment-or-uncomment-region-internal beg end))))

  :general-config
  (:states '(normal visual motion)
           :keymaps '(jtsx-tsx-mode)
           "M-r" #'consult-history)
  :hook
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . (lambda ()
                                                          (setq-local evilnc-comment-or-uncomment-region-function '+jsx-comment-or-uncomment-region)))
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . (lambda ()
                                                          (+add-pairs '((?` . ?`)))))
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . lsp-deferred)
  ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode) . apheleia-mode)
  )

(use-package web-mode
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

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :preface
  (defun +python-mode-setup ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))
  :hook
  (python-ts-mode . lsp-deferred)
  (python-ts-mode . +python-mode-setup))

(use-package pythontest
  :general
  (+local-leader-def
    :keymaps '(python-ts-mode-map)
    "t" '(nil :wk "test")
    "ta" #'pythontest-test-all
    "tf" #'pythontest-test-file
    "ts" #'pythontest-test-at-point))

(use-package auto-virtualenv
  :hook
  ((python-mode python-ts-mode) . auto-virtualenv-set-virtualenv))

(use-package pyvenv
  :init
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("venv:" pyvenv-virtual-env-name " ")))
  :hook
  ((python-mode python-ts-mode) . pyvenv-mode))

(use-package inf-ruby
  :hook (compilation-filter . inf-ruby-auto-enter)
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode)
  :custom
  (inf-ruby-console-environment "development")
  :general-config
  (:states '(normal visual insert)
           :keymaps 'inf-ruby-mode-map
           "M-r" #'consult-history)
  (+local-leader-def
    :keymaps 'ruby-ts-mode-map
    "s" '(:ignore t :wk "send")
    "sl" #'ruby-send-line
    "sr" #'ruby-send-region
    "sR" #'ruby-send-region-and-go
    "sd" #'ruby-send-definition
    "sD" #'ruby-send-definition-and-go
    "si" #'ruby-switch-to-inf
    "so" #'inf-ruby-console-auto))

(use-package ruby-end
  :after (ruby-mode ruby-ts-mode))

(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :general-config
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

(use-package bundler
  :after ruby-ts-mode
  :general-config
  (+local-leader-def
    :keymaps '(ruby-ts-mode-map)
    "b" '(nil :wk "bundle")
    "bc" #'bundle-check
    "bC" #'bundle-console
    "bi" #'bundle-install
    "bu" #'bundle-update
    "be" #'bundle-exec
    "bo" #'bundle-open))

(use-package rake
  :after ruby-ts-mode
  :custom
  (rake-completion-system 'default)
  :general-config
  (+local-leader-def
    :keymaps '(ruby-ts-mode-map)
    "rk" #'rake))

(use-package ruby-ts-mode
  :ensure nil
  :hook
  (ruby-ts-mode . apheleia-mode)
  (ruby-ts-mode . lsp-deferred)
  :general-config
  (+local-leader-def
    :keymaps '(ruby-ts-mode-map inf-ruby-mode-map erb-mode-map)
    "r" '(:keymap rails-command-map :wk "rails"))
  )

(defvar rails-command-prefix "bundle exec rails")

(defvar rails-generators
  '(("assets" (("app/assets/"
                "app/assets/\\(?:stylesheets\\|javascripts\\)/\\(.+?\\)\\..+$")))
    ("controller" (("app/controllers/" "app/controllers/\\(.+\\)_controller\\.rb$")))
    ("generator" (("lib/generator/" "lib/generators/\\(.+\\)$")))
    ("helper" (("app/helpers/" "app/helpers/\\(.+\\)_helper.rb$")))
    ("integration_test" (("test/integration/" "test/integration/\\(.+\\)_test\\.rb$")))
    ("job" (("app/jobs/" "app/jobs/\\(.+\\)_job\\.rb$")))
    ("mailer" (("app/mailers/" "app/mailers/\\(.+\\)\\.rb$")))
    ("migration" (("db/migrate/" "db/migrate/[0-9]+_\\(.+\\)\\.rb$")))
    ("model" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
    ("resource" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
    ("scaffold" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
    ("task" (("lib/tasks/" "lib/tasks/\\(.+\\)\\.rake$")))))

(defun rails-generate ()
  "Execute Rails generate COMMAND with input completion."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (async-shell-command (rails-command-with-completion " generate "))))

(defun rails-destroy ()
  "Execute Rails destroy COMMAND with input completion."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (async-shell-command (rails-command-with-completion " destroy "))))

(defun rails-command-with-completion (command)
  "Build Rails command from COMMAND with input completion."
  (let ((keymap (copy-keymap minibuffer-local-map))
        (command-prefix (concat rails-command-prefix command)))
    (define-key keymap (kbd "<tab>") 'rails--completion-in-region)
    (concat command-prefix (read-from-minibuffer command-prefix nil keymap))))

(defun rails--completion-in-region ()
  "Apply Rails generators for text completion in region."
  (interactive)
  (let ((generators (--map (concat (car it) " ") rails-generators)))
    (when (<= (minibuffer-prompt-end) (point))
      (completion-in-region (minibuffer-prompt-end) (point-max)
                            generators))))

(defun rails-server ()
  "Run rails server command."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (async-shell-command (concat rails-command-prefix " server"))))

(defun rails-console ()
  "Start a rails console at project root."
  (interactive)
  (inf-ruby-console-rails (project-root (project-current t))))

(defun project-find-file-in-dir (dir)
  "Visit a file (with completion) in the current project.
The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\"."
  (interactive)
  (let* ((pr (project-current t))
        (dirs (list (expand-file-name dir (project-root pr)))))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

(defun rails-find-controller ()
  (interactive)
  (project-find-file-in-dir "app/controllers/"))

;; refactor to macro?
(defun rails-find-model ()
  (interactive)
  (project-find-file-in-dir "app/models/"))

(defun rails-find-view ()
  (interactive)
  (project-find-file-in-dir "app/views/"))

(defun rails-find-helper ()
  (interactive)
  (project-find-file-in-dir "app/helpers/"))

(defun rails-find-test ()
  (interactive)
  (project-find-file-in-dir "app/tests/"))

(defun rails-find-javascript ()
  (interactive)
  (project-find-file-in-dir "app/javascript/"))

(defun rails-find-job ()
  (interactive)
  (project-find-file-in-dir "app/jobs/"))

(defun rails-find-mailer ()
  (interactive)
  (project-find-file-in-dir "app/mailers/"))

;; non macro
(defun rails-find-spec ()
  (interactive)
  (project-find-file-in-dir "app/spec/"))

(defun rails-find-migration ()
  (interactive)
  (project-find-file-in-dir "db/migrate/"))

(defun rails-find-stylesheet ()
  (interactive)
  (project-find-file-in-dir "app/assets/stylesheets/"))

(defun rails-find-initializer ()
  (interactive)
  (project-find-file-in-dir "config/initializers/"))

(defun rails-find-locale ()
  (interactive)
  (project-find-file-in-dir "config/locales/"))

(defvar rails-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'rails-find-locale)
    (define-key map (kbd "b") 'rails-find-job)
    (define-key map (kbd "c") 'rails-find-controller)
    (define-key map (kbd "d") 'rails-destroy)
    (define-key map (kbd "g") 'rails-generate)
    (define-key map (kbd "h") 'rails-find-helper)
    (define-key map (kbd "i") 'rails-find-initializer)
    (define-key map (kbd "j") 'rails-find-javascript)
    (define-key map (kbd "m") 'rails-find-model)
    (define-key map (kbd "n") 'rails-find-migration)
    (define-key map (kbd "p") 'rails-find-spec)
    (define-key map (kbd "r") 'rails-console)
    (define-key map (kbd "R") 'rails-server)
    (define-key map (kbd "s") 'rails-find-stylesheet)
    (define-key map (kbd "t") 'rails-find-test)
    (define-key map (kbd "u") 'rails-find-fixture)
    (define-key map (kbd "v") 'rails-find-view)
    (define-key map (kbd "w") 'rails-find-component)
    (define-key map (kbd "@") 'rails-find-mailer)
    map)
  "Keymap after `rails-keymap-prefix'.")
(fset 'rails-command-map rails-command-map)

(use-package feature-mode
  :mode ("/\\.feature\\'" . feature-mode)
  :config
  (require 'org-table))

(use-package lua-ts-mode
  :mode "\\.lua\\'")

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . apheleia-mode)
  :general-config
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

;; (use-package edit-indirect)
;; (use-package markdown-mode
;;   :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
;;   :hook
;;   (markdown-mode . variable-pitch-mode)
;;   (markdown-mode . apheleia-mode)
;;   :config
;;   ;; (add-to-list 'org-src-lang-modes '("md" . markdown))

;;   (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'markdown-inline-code-face nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'markdown-table-face nil :inherit 'org-table)
;;   (set-face-attribute 'markdown-code-face nil :inherit 'org-block)

;;   (set-face-attribute 'markdown-html-tag-delimiter-face nil :family (face-attribute 'fixed-pitch :family))
;;   (set-face-attribute 'markdown-html-tag-name-face nil :family (face-attribute 'fixed-pitch :family))
;;   (set-face-attribute 'markdown-html-entity-face nil :family (face-attribute 'fixed-pitch :family))
;;   (set-face-attribute 'markdown-html-attr-name-face nil :family (face-attribute 'fixed-pitch :family))
;;   (set-face-attribute 'markdown-html-attr-value-face nil :family (face-attribute 'fixed-pitch :family))
;;   :general-config
;;   (+local-leader-def
;;     :keymaps '(markdown-mode-map)
;;     "'" #'markdown-edit-code-block
;;     "o" #'markdown-open
;;     "p" #'markdown-preview
;;     "e" #'markdown-export
;;     )
;;   :custom
;;   (markdown-command "multimarkdown")
;;   (markdown-asymmetric-header t)
;;   (markdown-header-scaling t)
;;   (markdown-enable-highlighting-syntax t)
;;   (markdown-enable-math t)
;;   (markdown-fontify-whole-heading-line t)
;;   (markdown-fontify-code-blocks-natively t)
;;   (markdown-gfm-additional-languages '("sh"))
;;   (markdown-italic-underscore t)
;;   (markdown-hide-urls t)
;;   (markdown-make-gfm-checkboxes-buttons t)
;;   (markdown-content-type "application/xhtml+xml")
;;   (markdown-css-paths
;;         '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
;;           "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
;;   (markdown-xhtml-header-content
;;         (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
;;                 "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
;;                 "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
;;                 "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
;;                 "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
;;   )

(add-to-list 'auto-mode-alist
             (cons "rc\\'" 'conf-mode))

(use-package jenkinsfile-mode
  :mode "\\Jenkinsfile\\'")

(use-package dockerfile-ts-mode
  :ensure nil
  :mode "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'"
  :hook
  (dockerfile-ts-mode . lsp-deferred))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

(use-package json-ts-mode
  :ensure nil
  :preface
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

(use-package compile
  :ensure nil
  :preface
  (defun +project-compilation-buffer-name (compilation-mode)
    "Meant to be used for `compilation-buffer-name-function`.
Argument COMPILATION-MODE is the name of the major mode used for the
compilation buffer."
    (concat (+compilation-buffer-name-function compilation-mode)
            (if (project-current) (concat "<" (project-name (project-current)) ">") "")))

  (defun +compilation-buffer-name (arg)
    "Rename buffer to whatever command was used.
eg. *python main.py*"
    (concat "*" compile-command "*"))
  :custom
  (compile-command "make ")
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compilation-buffer-name-function '+compilation-buffer-name)
  (project-compilation-buffer-name-function '+project-compilation-buffer-name)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package comint
  :ensure nil
  :custom
  (ansi-color-for-comint-mode t)
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048))

(use-package shell-command-pro
  :load-path "~/code/shell-command-pro"
  :commands (async-shell-command-in-dir async-shell-command-from-history
             project-or-cwd-async-shell-command project-or-cwd-async-shell-command-from-history
             project-run-project project-or-cwd-compile project-or-cwd-compile-from-history)
  :preface
  (defun project-or-cwd-compile ()
    "Run `compile' in the current project's root directory."
    (declare (interactive-only compile))
    (interactive)
    (let ((project (project-current)))
      (if project
          (let ((default-directory (project-root (project-current t))))
            (call-interactively #'project-compile))
        (call-interactively #'compile))))

  (defun project-or-cwd-compile-from-history ()
    "Run `compile' with a choice from its command history in
current project's root directory."
    (interactive)
    (let ((project (project-current)))
      (if project
          (let ((default-directory (project-root (project-current t))))
            (call-interactively #'compile-from-history))
        (call-interactively #'compile-from-history))))

  (defun +project-or-cwd-default-directory ()
    (let ((project (project-current)))
      (if project
          (project-root (project-current t))
        default-directory)))

  (defun project-or-cwd-async-shell-command (&optional command)
    "Run `async-shell-command' in the current project's root directory or in the current directory."
    (declare (interactive-only async-shell-command))
    (interactive)
    (async-shell-command-in-dir (+project-or-cwd-default-directory) command))

  (defun project-or-cwd-async-shell-command-from-history ()
    "Run `async-shell-command' with a choice from its command history in
current project's root directory."
    (interactive)
    (let ((default-directory (+project-or-cwd-default-directory)))
      (call-interactively #'async-shell-command-from-history)))

  (defun project-run-project ()
    "Run project's run command with `async-shell-command'."
    (interactive)
    (project-or-cwd-async-shell-command project-commands-run-command))
  :init
  (put 'project-commands-run-command 'safe-local-variable #'stringp)
  :bind
  ([remap shell-command] . project-or-cwd-async-shell-command)
  ("M-r" . project-or-cwd-async-shell-command-from-history)
  ("M-o" . project-or-cwd-compile-from-history)
  :general
  (+leader-def
    "cc" #'project-or-cwd-compile
    "pc" #'project-or-cwd-compile
    "!"  #'project-or-cwd-async-shell-command
    "p!" #'project-or-cwd-async-shell-command
    "pR" #'project-run-project
    "@"  #'async-shell-command-in-dir))

(use-package shell
  :ensure nil
  :general
  (+leader-def
    "|"  #'async-shell-command-region)
  :custom
  (async-shell-command-display-buffer nil) ;; If a shell command never outputs anything, don't show it.
  (shell-command-prompt-show-cwd t)
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
  :custom
  (shell-command-x-buffer-name-async-format "*shell %p:%a*")
  (shell-command-x-buffer-name-format "*shell %p:%a*")
  :config
  (shell-command-x-mode 1))

;; (use-package bash-completion
;;   :custom
;;   (bash-completion-use-separate-processes t)
;;   :config
;;   ;; (defun eshell-bash-completion-capf-nonexclusive ()
;;   ;;   (let ((compl (bash-completion-dynamic-complete-nocomint
;;   ;;                 (save-excursion (eshell-bol) (point))
;;   ;;                 (point) t)))
;;   ;;     (when compl
;;   ;;       (append compl '(:exclusive no)))))
;;   :hook
;;   (elpaca-after-init . bash-completion-setup) ;; shell-command completion setup
;;   ;; (eshell-mode . (lambda ()
;;   ;;                  (setq-local completion-at-point-functions (list #'bash-completion-capf-nonexclusive))))
;;   (eshell-mode .
;;                (lambda ()
;;                  (add-hook 'completion-at-point-functions
;;                            'bash-completion-capf-nonexclusive nil t)))
;;   )

(use-package fish-completion
  :if (executable-find "fish")
  :hook
  (minibuffer-setup . fish-completion-mode)
  (eshell-mode . fish-completion-mode))

(use-package eat
  :commands (eat eat-project)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-name "xterm-256color")
  :config
  (evil-set-initial-state 'eat-mode 'insert)
  :general
  (+leader-def
    "os" #'eat
    "ps" #'eat-project)
  :general-config
  (:states '(normal visual)
           :keymaps 'eat-mode-map
           "<return>" #'evil-insert-resume)
  (:states '(insert)
           :keymaps 'eat-mode-map
           "C-y" #'eat-yank)
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(use-package eshell
  :ensure nil
  :general
  (+leader-def
    "oe"  #'eshell
    "oE"  #'+eshell-new)
  :general-config
  (:states '(normal visual)
           :keymaps 'eshell-mode-map
           "<return>" #'evil-insert-resume)
  (:states '(insert)
           :keymaps 'eshell-mode-map
           "C-y" #'yank)
  (:states '(normal insert visual)
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
      ('eshell-mode (eshell-send-input)))
    )

  (defun +eshell-new ()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))

  (defun +eshell-setup ()
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 nil)
    (setq-local hscroll-margin 0)
    (set-display-table-slot standard-display-table 0 ?\ ))

  :custom
  (eshell-banner-message "")
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 4096)
  (eshell-prompt-regexp "^.* λ ")
  (eshell-prompt-function #'+eshell-default-prompt-fn)
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  :hook
  (eshell-mode . +eshell-setup))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package ielm
  :commands (ielm)
  :ensure nil
  :general-config
  (:states '(insert)
           :keymaps 'inferior-emacs-lisp-mode-map
           "C-y" #'yank)
  )

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/") ;; don't know why custom doesn not work
  :custom
  (org-src-window-setup 'current-window)
  ;; (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  ;; (org-hide-emphasis-markers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  ;; (org-cycle-separator-lines 2)
  ;; (org-fold-core-style 'overlays)
  (org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . success)))
  (org-use-sub-superscripts '{})
  (org-tags-column 0)
  (org-startup-indented t)
  ;; (org-special-ctrl-a/e t)
  ;; (imenu-auto-rescan t)
  (org-confirm-babel-evaluate nil)
  :config
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (dolist (face '(org-table org-tag org-verbatim org-list-dt org-hide
                            org-date org-todo org-done org-formula
                            org-checkbox org-special-keyword))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  (set-face-attribute 'org-block nil :foreground (catppuccin-get-color 'text) :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))

  :general-config
  (+local-leader-def
    :keymaps '(org-mode-map)
    "'" #'org-edit-special
    "." #'consult-org-heading
    "x" #'org-toggle-checkbox
    "i" #'org-toggle-item
    "l" #'org-insert-link
    "b" '(:ignore t :wk "table")
    "bc" #'org-table-create-or-convert-from-region
    "bs" #'org-table-sort-lines
    "b-" #'org-table-insert-hline
    )
  :hook
  (org-mode . variable-pitch-mode))

(use-package org-indent
  :ensure nil
  :hook
  (org-mode . org-indent-mode))

(use-package evil-org
  :after (org evil)
  :init
  (setf evil-org-key-theme '(textobjects insert navigation additional todo))
  :config
  (evil-define-key '(normal insert) 'evil-org-mode
    (kbd "<C-return>") (evil-org-define-eol-command org-insert-heading-after-current)
    (kbd "<C-S-return>") (evil-org-define-bol-command org-insert-heading))
  :hook
  (org-mode . evil-org-mode)
  (org-agenda-mode . (lambda ()
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-superstar
  :custom
  (org-superstar-remove-leading-stars t)
  :hook
  (org-mode . org-superstar-mode))

(use-package org-tempo
  :after org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (js . t)))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  )

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
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-inhibit-startup t)
  (org-capture-templates
   `(("t" "Task" entry (file "tasks.org")
      "* TODO %?")
     ("e" "Emacs todo" entry (file+olp "~/.config/emacs/init.org" "Todos")
      "* %?")
     ("v" "Vocalubary" entry (file "vocab.org")
      "* %?")
     ))

  :general-config
  (:keymaps 'org-agenda-mode-map
            "q" 'org-agenda-exit)
  :hook
  (org-agenda-mode . hl-line-mode)
  (org-agenda-mdoe . (lambda ()
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

(use-package org-roam
  :hook (org-load . org-roam-db-autosync-mode)
  :config
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-list-files-commands '(fd fdfind rg find))
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  )

(use-package bazel
  :mode ("\\Tiltfile\\'" . bazel-starlark-mode))

(setq help-window-select t)
(setq echo-keystrokes-help nil)
(use-package helpful
  :hook
  (emacs-lisp-mode . (lambda () (setq-local evil-lookup-func 'helpful-at-point)))
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
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

(use-package wgrep
  :defer t
  :commands (wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t))

(use-package link-hint
  :general
  (+leader-def
    "oL" #'link-hint-open-link))

(use-package envrc
  :commands (envrc-global-mode)
  :hook (on-first-buffer . envrc-global-mode))

(use-package docker
  :config
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

(use-package kubel
  :commands (kubel)
  :general
  (+leader-def
    "ok" 'kubel))

(use-package kubel-evil
  :after kubel)

;; (setq dictionary-use-single-buffer t)
;; (setq dictionary-server "dict.org")

(use-package devdocs
  :commands (devdocs-lookup devdocs-install devdocs-update-all devdocs-delete devdocs-persue)
  :general
  (+leader-def
    "sk" 'devdocs-lookup))

(use-package verb
  :after org
  :custom
  (verb-auto-kill-response-buffers t)
  (verb-json-use-mode 'json-ts-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((verb     . t))))
  (add-to-list 'org-structure-template-alist '("vb" . "src verb :wrap src ob-verb-response :op send get-body"))
  :general-config
  (+leader-def
    :keymaps 'org-mode-map
    "v" '(:ignore t :wk "verb")
    "vv" '(verb-send-request-on-point-other-window-stay :wk "Send request")
    "vs" '(verb-set-var :wk "Set variable")
    "vu" '(verb-unset-var :wk "Unset variable")
    "vg" '(verb-show-vars :wk "Show variables"))
    )

;;;###autoload
(defun download-file (url)
  "Download file from URL."
  (interactive "sEnter URL: ")
  (url-copy-file url (read-file-name "Save as: ")))

(use-package makefile-executor
  :general
  (+local-leader-def
    :keymaps 'makefile-mode-map
    "m" 'makefile-executor-execute-target)
  (+leader-def
    "pm" 'makefile-executor-execute-project-target)
  :hook
  (makefile-mode . makefile-executor-mode))

(use-package elcord
  :defer 2
  :custom
  (elcord-quiet t)
  :config
  (elcord-mode 1))

(use-package calc
  :ensure nil
  :general
  (+leader-def
    "oc" #'quick-calc
    ))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))
