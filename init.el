;;; General Settings
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

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

;; open recent files
(use-package recentf
  :defer nil
  :after no-littering
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)
  (recentf-mode t))

;;; Undo Tree
(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

;;; Evil
(use-package evil
  :ensure t
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
    :states '(normal visual motion insert emacs)
    :keymaps '(override)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer my-local-leader-def
    :states '(normal visual motion insert emacs)
    :keymaps '(override)
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

;;; Help
(use-package helpful
  ;; :commands helpful--read-symbol
  :config
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

;;; Git
(use-package magit
  :ensure t)

;;; Completion

(use-package vertico
  :init
  (vertico-mode)
  ;; Show more candidates
  (setq vertico-count 15)
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

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator))

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

(setq-default initial-major-mode 'org-mode
              initial-scratch-message "#+title: Scratch Buffer\n\n")
;;; Elisp
(use-package parinfer-rust-mode
  :hook
  emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t)) 

