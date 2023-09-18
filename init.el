;;; Fix
;; fix emacs transparent when there is alpha in xresources
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;;; General Settings
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(use-package savehist
  :defer nil
  :init
  (savehist-mode))

 ; minibuffer history
(global-auto-revert-mode 1) ; revert buffers when file change on disk
(setq global-auto-revert-non-file-buffers t)

(setq inhibit-startup-message t
      visible-bell t
      use-dialog-box nil)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq scroll-margin 8)
(setq scroll-conservatively 1)

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

;;; Undo Tree
(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

;;; Evil
(use-package evil
  :defer nil
  :init
  (setq evil-want-keybinding nil
   evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line))

(use-package goto-chg)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; Keymaps
(use-package general
  :after evil
  :config
  (general-create-definer my-leader-def
    :keymaps '(normal visual motion insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer my-local-leader-def
    :keymaps '(normal visual motion insert emacs)
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")

  (my-leader-def
    "" nil
    "gg" 'magit-status
    "dd" 'dired
    "h" (general-simulate-key "C-h")
    "x" (general-simulate-key "C-x")
    "c" (general-simulate-key "C-c"))

  (my-local-leader-def
   "" nil
   "g" 'vc-dir))

;;; Git
(use-package magit)

;;; Completion

(use-package vertico
  :init
  (vertico-mode)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(unless (default-value vertico-mode) (fido-vertical-mode 1)) ; fuzzy find

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;; Terminal
(use-package vterm
  :config
  (setq vterm-shell "/usr/bin/zsh"))

;;; Org Mode
(use-package org
  :custom
  (org-directory "~/notes/org")
  (org-agenda-files '("tasks.org" "jobs.org"))
  (org-capture-templates
    `(("t" "Task" entry (file+olp "tasks.org" "Inbox")
       "* TODO %?\n  %U\n  %a\n  %i"))))

;;; Elisp
(use-package parinfer-rust-mode
  :hook
  emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t)) 
