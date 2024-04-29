;;; General Settings
(menu-bar-mode -1)
(unless my/is-termux
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(tooltip-mode -1)
(column-number-mode 1)

(global-auto-revert-mode 1) ; revert buffers when file change on disk
(setq global-auto-revert-non-file-buffers t)

(setq inhibit-startup-message t
      visible-bell t
      use-dialog-box nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'modus-operandi)
(add-to-list 'default-frame-alist '(font . "Monospace-11"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq scroll-margin 8
      scroll-conservatively 1)

;; indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ; spaces instead of tabs
(setq-default standard-indent 4)
(setq-default electric-indent-inhibit t) ; no global auto indent

;; ;; autopair
;; (setq electric-pair-pairs
;;       '((?\{ . ?\})
;;         (?\( . ?\))
;;         (?\[ . ?\])
;;         (?\" . ?\")))
;; (electric-pair-mode 1)

;; keep unwant thing out of `.emacs.d'
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; ;; Set the right directory to store the native comp cache
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; suppress `(comp)' warnings
(setq native-comp-async-report-warnings-errors nil)

;; prevent emacs customize `init.el' file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq auto-save-default nil ; no `#filename#'
      make-backup-files nil ; no `filename~'
      create-lockfiles nil) ; no `.#filename'

;;; Packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
       (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default 1)
(straight-use-package 'use-package)
(setq use-package-always-defer 1
      use-package-always-ensure 1)

;;; Clean
;; Clean Directory
(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups)
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,backup-dir))))
  (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,(concat temporary-file-directory "\\2") t)
            ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
            ("." ,auto-save-dir t)))))

;; Clean mode-line
(use-package delight
  :delight
  (eldoc-mode))

;;; QOL
;; minibuffer history
(use-package savehist
  :defer nil
  :after no-littering
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil)
  (savehist-mode t))

;; example: run `M-x' on `M-x' again and again
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;; open recent files
(use-package recentf
  :defer nil
  :after no-littering
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)
  (recentf-mode t))

;; Repeatable key `describe-repeat-maps'
(use-package repeat
  :init (repeat-mode t))

(use-package editorconfig
  :delight
  :config (editorconfig-mode 1))

;;; Undo Tree
(use-package undo-tree
  :delight
  :config (global-undo-tree-mode 1))

;;; Evil
(use-package evil
  :ensure t
  :defer nil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t ;; use `\ C-u' instead
        evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)

  (evil-define-operator my/evil-comment (beg end)
    "Comment text"
    (comment-or-uncomment-region beg end))
  (define-key evil-normal-state-map (kbd "gc") 'my/evil-comment)

  ;; TODO: reduce hardcode
  (defvar my/win-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "w") #'evil-window-next)
      (define-key map (kbd "W") #'evil-window-prev)
      (define-key map (kbd "+") #'evil-window-increase-height)
      (define-key map (kbd "-") #'evil-window-decrease-height)
      ;; (define-key map (kbd "<down>") #'evil-window-down)
      ;; (define-key map (kbd "<left>") #'evil-window-left)
      ;; (define-key map (kbd "<right>") #'evil-window-right)
      ;; (define-key map (kbd "<up>") #'evil-window-up)        
      map))
  (dolist (cmd '(evil-window-next evil-window-prev evil-window-increase-height evil-window-decrease-height evil-window-down evil-window-left evil-window-right evil-window-up))
    (put cmd 'repeat-map 'my/win-repeat-map))

  (defun my/set-terminal-cursor ()
    (let* ((box-blink "\e[1 q")
           (box "\e[2 q")
           (hbar-blink "\e[3 q")
           (hbar "\e[4 q")
           (bar-blink "\e[5 q")
           (bar "\e[6 q")
           (shape (cond ((symbolp cursor-type) cursor-type)
                        ((listp cursor-type) (car cursor-type))))
           (seq box-blink))
      (unless (member shape '(box bar hbar))
        (setq shape 'box))
      (cond ((eq shape 'box)
             (setq seq (if blink-cursor-mode box-blink box)))
            ((eq shape 'bar)
             (setq seq (if blink-cursor-mode bar-blink bar)))
            ((eq shape 'hbar)
             (setq seq (if blink-cursor-mode hbar-blink hbar))))
      (send-string-to-terminal seq)))

  (unless (display-graphic-p)
    (add-hook 'pre-command-hook #'my/set-terminal-cursor)
    (add-hook 'post-command-hook #'my/set-terminal-cursor)
    (add-hook 'evil-operator-state-entry-hook #'my/set-terminal-cursor)))

(use-package goto-chg)

(use-package evil-collection
  :after evil
  :delight evil-collection-unimpaired-mode
  :config
  (evil-collection-init)
  ;; `repeat-mode' integration
  (setq evil-collection-unimpaired-want-repeat-mode-integration t))

;;; Keymaps
(use-package general
  :after evil
  :config
  (general-create-definer my/leader-def
    :states '(normal visual motion insert emacs)
    :keymaps '(override)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer my/local-leader-def
    :states '(normal visual motion insert emacs)
    :keymaps '(override)
    :prefix "DEL" ; NOTE: DEL is <backspace> not <delete>
    :non-normal-prefix "M-DEL")

  (my/leader-def
    "" nil
    "u" 'universal-argument-more
    ;; "h" (general-simulate-key "C-h") ; this seen in `C-h k' but not seen in `C-h b'
    "h" (general-key "C-h")
    "x" (general-key "C-x")
    "c" (general-key "C-c")
    "hh" 'describe-symbol
    "hH" 'describe-face
    "ff" 'find-file
    "fc" '(lambda ()
            (interactive)
            (let ((project-current-directory-override "~/.config/emacs"))
              (call-interactively #'project-find-file)))
    "gg" 'magit-status
    "gb" 'magit-blame
    "dd" 'dired
    "bb" 'switch-to-buffer
    "bm" 'bookmark-set
    "bl" 'list-bookmarks
    "jb" 'bookmark-jump
    "tl" 'display-line-numbers-mode
    "p" (general-key "C-x p"))

  (my/local-leader-def
    "" nil
    "g" 'vc-dir)

  (general-def 'normal
    "]d" 'next-error
    "[d" 'previous-error))

(use-package god-mode
  :general (my/leader-def ";" 'god-execute-with-current-bindings)
  :config
  ;; (global-set-key (kbd "<escape>") #'god-local-mode)
  (setq god-exempt-major-modes nil ; list of mode to disable
        god-exempt-predicates nil)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  ;; FIXME: this seem to not right
  (define-key god-local-mode-map (kbd "<escape>") #'(god-local-mode nil)))

;;; Help
(use-package helpful
  ;; :commands helpful--read-symbol
  :config
  (setq apropos-do-all t)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

;;; Git
(use-package magit
  :ensure t)

(use-package diff-hl
  :init (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode)
  (setq diff-hl-show-staged-changes nil)
  :general
  (general-def
    :states 'normal
    "]c" 'diff-hl-next-hunk
    "[c" 'diff-hl-previous-hunk)
  (my/leader-def
    "gs" 'diff-hl-stage-dwim
    "gr" 'diff-hl-revert-hunk
    "gp" 'diff-hl-show-hunk
    "gU" 'diff-hl-unstage-file))

;;; Todo comment
(use-package hl-todo
  :init (global-hl-todo-mode))

(use-package magit-todos
  :after magit hl-todo
  :config (magit-todos-mode))

;;; Completion
;; Mini-buffer completion
(use-package vertico
  :init
  (vertico-mode)
  ;; Show more candidates
  (setq vertico-count 15)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :general
  (my/leader-def
    "tr" 'vertico-repeat
    "tR" 'vertico-repeat-select)
  (general-def
    "M-P" 'vertico-suspend) ; push completion into completion stack
  (general-def vertico-map
    "C-M-n" 'vertico-repeat-next
    "C-M-p" 'vertico-repeat-previous
    "M-j" 'vertico-quick-jump
    "M-i" 'vertico-quick-insert
    "M-q" 'vertico-quick-exit))

(unless (default-value vertico-mode) (fido-vertical-mode 1)) ; fuzzy find

;; Unorder querying.
(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; More precise completion.
(use-package prescient
  :after (corfu vertico orderless)
  :custom (completion-styles '(prescient orderless basic)
           prescient-sort-full-matches-first t)
  :config (prescient-persist-mode))

;; More completion meta-data
(use-package marginalia
  :general
  (general-def minibuffer-local-map
   "M-C" 'marginalia-cycle)
  :init
  (marginalia-mode))

;; More useful command
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :general
  ;; See https://github.com/minad/consult#use-package-example
  (general-def
    [remap switch-to-buffer] 'consult-buffer
    [remap imenu] 'consult-imenu
    [remap goto-line] 'consult-goto-line
    [remap bookmark-jump] 'consult-bookmark
    [remap project-list-buffers] 'consult-project-buffer
    [remap Info-search] 'consult-info)
  (general-def minibuffer-local-map
    "M-s" 'consult-history
    "M-r" 'consult-history)
  (my/leader-def
    "fg" (if (executable-find "rg") 'consult-ripgrep 'consult-grep)
    "fd" (if (executable-find "fd") 'consult-fd 'consult-find)
    "fG" 'consult-git-grep
    "fr" 'consult-recent-file
    "ol" 'consult-outline)
  (my/local-leader-def
    "M-x" 'consult-mode-command))

(use-package embark
  :config
  (setq prefix-help-command #'embark-prefix-help-command) ; `<prefix> ?'
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly) ; show multiple-line eldoc item in minibuffer
  ;; (setq embark-prompter 'embark-completing-read-prompter) ; show completion available when perform action

  (defun my/embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let ((embark-prompter 'embark-completing-read-prompter))
      (embark-act arg)))

  :general
  (general-def "C-h B" 'embark-bindings) ; alternative for `describe-bindings'(`C-h b')
  ;; FIXME: this keymap still also show `embark-keymap-prompter' and leave behind `Act' indicator on vertico completion
  (general-def vertico-map "M-?" 'my/embark-act-with-completing-read)
  (general-def minibuffer-local-map
    "M-a" 'embark-act
    "M-A" 'embark-act-all
    "M-." 'embark-dwim
    "M-B" 'embark-become
    "C-q" 'embark-collect
    "M-S" 'embark-collect
    "M-E" 'embark-export)
  (my/leader-def
    "." 'embark-dwim
    "a" 'embark-act
    "S" 'embark-collect
    "L" 'embark-live
    "E" 'embark-export))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Completion at point
(use-package corfu
  :init (global-corfu-mode)
  :general
  (general-def corfu-map
    :states 'insert
    "C-y" 'corfu-insert
    "C-e" 'corfu-quit
    "C-k" 'corfu-info-documentation ; popup info
    "C-j" 'corfu-info-location) ; peek definition
  :config
  (setq corfu-auto t
        corfu-cycle t
        corfu-quit-no-match nil
        corfu-echo-delay nil
        corfu-preselect 'valid
        corfu-popupinfo-delay nil)
  (corfu-popupinfo-mode t))

(use-package corfu-terminal
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode t)))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev))
  ;; (add-to-list 'completion-at-point-functions #'cape-dict))
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-emoji))

;;; Terminal
;; (use-package vterm
;;   :config
;;   (setq vterm-shell "/usr/bin/zsh"))

;;; Zen mode
(use-package olivetti
  :delight " zen"
  :general
  (my/leader-def "tz" 'olivetti-mode))

;;; Elisp
(use-package parinfer-rust-mode
  :if (not my/is-termux)
  :hook
  emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t)
  :general (my/leader-def "tp" 'parinfer-rust-mode))

;;; Org Mode
(use-package org
  :ensure nil
  :defer nil
  :straight nil
  :config
  (setq org-directory "~/notes/org")
  (my/leader-def
    "fo" '(lambda ()
            (interactive)
            (let ((project-current-directory-override org-directory))
              (project-find-file))))
  (setq org-agenda-files '("inbox.org" ; for capture
                           "project.org" ; main project file
                           "someday.org"
                           "tickler.org"))
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-auto-align-tags nil)
  (setq org-refile-targets
    '((nil :maxlevel . 9)
      ("project.org" :maxlevel . 9)
      ("someday.org" :maxlevel . 9)
      ("archive.org" :maxlevel . 9)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  (setq org-hide-leading-stars t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@/!)")))
      ;; (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-capture-templates
    '(("q" "Quick Capture" entry (file+olp "inbox.org" "Inbox")
        "* TODO %?\n  %U\n  %a\n  %i" :prepend t)
      ("c" "Clipboard" entry (file+olp "inbox.org" "Inbox")
        "* TODO %?\n  %U\n  %x\n  %i" :prepend t)))

  (setq org-agenda-custom-commands
    '(("n" "Next action" todo "NEXT")
      ("N" "Next action tree" todo-tree "NEXT")
      ("h" . "Home and Hobbies")
      ("hh" "Hobby" tags-todo "hobby|@home")
      ("hn" "Hobby next"
        ((tags-todo "hobby+@home"))
        (todo "NEXT"))
      ("p" "Project Agenda" agenda "" ((org-agenda-files '("project.org"))))
      ("r" "Recurring Agenda" agenda "" ((org-agenda-files '("tickler.org"))))
      ("A" "Appointment" agenda*)))

  (my/leader-def
    "ol" 'org-store-link
    "oc" 'org-capture
    "oa" 'org-agenda
    "joc" 'org-capture-goto-last-stored
    "jor" 'org-refile-goto-last-stored)
  (my/local-leader-def
    :keymaps 'org-mode-map
    "R" 'org-refile
    "A" 'org-archive-subtree
    "tL" 'org-toggle-link-display
    "il" 'org-insert-last-stored-link
    "is" 'org-schedule
    "id" 'org-deadline))

  ;; (setq-default initial-major-mode 'org-mode
  ;;               initial-scratch-message
  ;;   "#+title: Scratch Buffer\n
  ;; #+begin_src elisp\n
  ;; #+end_src"))

;;; Markdown
(use-package markdown-mode
  :mode ("\\(README\\|readme\\)\\.md\\'" . gfm-mode)
  :config
  (setq markdown-enable-wiki-links t))
  ;; :init (setq markdown-command "multimarkdown"))

;;; Feeds
(use-package elfeed
  :ensure nil
  :general
  (my/leader-def "nf" #'elfeed))

;;; Mails
(use-package notmuch
  :ensure nil
  :init
  (setq notmuch-search-oldest-first nil)
  :general
  (my/leader-def "nm" #'notmuch))

;; ;;; Exwm
;; (add-to-list 'load-path "~/.local/src/xelb/")
;; (add-to-list 'load-path "~/.local/src/exwm/")
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)
