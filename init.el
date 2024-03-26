;;; General Settings
(menu-bar-mode -1)
(when (not (getenv "TERMUX_VERSION"))
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

;;; Clean Directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory
          (expand-file-name ".local/etc" user-emacs-directory)
        no-littering-var-directory
          (expand-file-name ".local/var" user-emacs-directory))
  :config
  (no-littering-theme-backups))

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
  :config (editorconfig-mode 1))

;;; Undo Tree
(use-package undo-tree
  :config (global-undo-tree-mode 1))

;;; Evil
(use-package evil
  :ensure t
  :defer nil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t) ;; use `\ C-u' instead
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

  (defun my/set-terminal-cursor-block-blink () (unless (display-graphic-p) (send-string-to-terminal "\e[1 q")))
  (defun my/set-terminal-cursor-block () (unless (display-graphic-p) (send-string-to-terminal "\e[2 q")))
  (defun my/set-terminal-cursor-underline-blink () (unless (display-graphic-p) (send-string-to-terminal "\e[3 q")))
  (defun my/set-terminal-cursor-underline () (unless (display-graphic-p) (send-string-to-terminal "\e[4 q")))
  (defun my/set-terminal-cursor-beam-blink () (unless (display-graphic-p) (send-string-to-terminal "\e[5 q")))
  (defun my/set-terminal-cursor-beam () (unless (display-graphic-p) (send-string-to-terminal "\e[6 q")))

  (add-hook 'evil-normal-state-entry-hook #'my/set-terminal-cursor-block-blink)
  (add-hook 'evil-visual-state-entry-hook #'my/set-terminal-cursor-block-blink)
  (add-hook 'evil-insert-state-entry-hook #'my/set-terminal-cursor-beam-blink) ; FIXME: sometime it become block while in insert state
  (add-hook 'evil-operator-state-entry-hook #'my/set-terminal-cursor-underline-blink)
  (add-hook 'evil-replace-state-entry-hook #'my/set-terminal-cursor-underline)
  (add-hook 'evil-emacs-state-entry-hook #'my/set-terminal-cursor-block))

(use-package goto-chg)

(use-package evil-collection
  :after evil
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
            (let ((project-current-directory-override user-emacs-directory))
              (project-find-file)))
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
    "g" 'vc-dir))

(use-package god-mode
  :general (my/leader-def ";" 'god-execute-with-current-bindings)
  :config
  ;; (global-set-key (kbd "<escape>") #'god-local-mode)
  (setq god-exempt-major-modes nil ; list of mode to disable
        god-exempt-predicates nil)
  (define-key god-local-mode-map (kbd ".") #'repeat)
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

;;; Todo comment
(use-package magit-todos
  :after magit
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
  (my/leader-def
    "tr" 'vertico-repeat)
  (general-def
    "M-T" 'vertico-suspend)) ; push completion into completion stack

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
  :bind (:map minibuffer-local-map
         ("M-C" . marginalia-cycle))
  :init
  (marginalia-mode))

;; More useful command
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :bind (:map minibuffer-mode-map
          ("M-s" . consult-history)
          ("M-r" . consult-history))

  :general
  ;; https://github.com/minad/consult#use-package-example
  (my/leader-def
    "fg" (if (executable-find "rg") 'consult-ripgrep 'consult-grep)
    "fd" (if (executable-find "fd") 'consult-fd 'consult-find)
    "fG" 'consult-git-grep
    "fr" 'consult-recent-file
    "ol" 'consult-outline)
  (my/local-leader-def
    "M-x" 'consult-mode-command)
  :config
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap imenu] #'consult-imenu)
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap bookmark-jump] #'consult-bookmark)
  (global-set-key [remap project-list-buffers] #'consult-project-buffer))

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
  ("C-h B" 'embark-bindings) ; alternative for `describe-bindings'(`C-h b')
  ;; FIXME: this keymap still also show `embark-keymap-prompter' and leave behind `Act' indicator on vertico completion
  (vertico-map "M-?" 'my/embark-act-with-completing-read)
  (minibuffer-local-map
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
  (corfu-map
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
  :general
  (my/leader-def "tz" 'olivetti-mode))

;;; Elisp
(use-package parinfer-rust-mode
  :if (not (getenv "TERMUX_VERSION"))
  :hook
  emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t)
  :general (my/leader-def "tp" 'parinfer-rust-mode))

;;; Org Mode
(setq org-directory "~/notes/org")
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
      ((tags-todo "hobby+@home")
       (todo "NEXT")))
    ("p" "Project Agenda" agenda "" ((org-agenda-files '("project.org"))))
    ("r" "Recurring Agenda" agenda "" ((org-agenda-files '("tickler.org"))))
    ("A" "Appointment" agenda*)))

(my/leader-def
  "SPC l" 'org-store-link
  "SPC c" 'org-capture
  "SPC a" 'org-agenda
  "oc" 'org-capture-goto-last-stored
  "or" 'org-refile-goto-last-stored)
(my/leader-def
  :keymaps 'org-mode-map
  "R" 'org-refile
  "A" 'org-archive-subtree
  "tL" 'org-toggle-link-display
  "il" 'org-insert-last-stored-link
  "is" 'org-schedule
  "id" 'org-deadline)

;; (setq-default initial-major-mode 'org-mode
;;               initial-scratch-message
;;   "#+title: Scratch Buffer\n
;; #+begin_src elisp\n
;; #+end_src")

;; ;;; Exwm
;; (add-to-list 'load-path "~/.local/src/xelb/")
;; (add-to-list 'load-path "~/.local/src/exwm/")
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)
