;;; Fix
;; fix emacs transparent when there is alpha in xresources
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;;; General Settings
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(savehist-mode 1) ; minibuffer history
(global-auto-revert-mode 1) ; revert buffers when file change on disk
(setq global-auto-revert-non-file-buffers t)

(setq inhibit-startup-message t
      visible-bell t
      use-dialog-box nil)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
  :init
  (setq evil-want-keybinding nil
   evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line))

;;; Git
(use-package magit)

;;; Completion
(fido-vertical-mode 1) ; fuzzy find

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
