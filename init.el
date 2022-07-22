;; fix emacs transparent when there is alpha in xresources
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

(setq inhibit-startup-message t
      visible-bell t)

(setq use-dialog-box nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (set-fringe-mode 10)

(savehist-mode 1)
;; revert buffers when file changed
(global-auto-revert-mode 1)
;; including Dired and orther buffers
(setq global-auto-revert-non-file-buffers t)

;; mouse even in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)

(set-default-coding-systems 'utf-8)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;;; Clean emacs directory
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; don't create lock file
(setq create-lockfiles nil)

;; no `#filename#` https://www.victorquinn.com/emacs-prevent-autosave-mess
(setq auto-save-default nil)

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-soft t))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;; for `g;` ahd `g,` motions
(use-package goto-chg)

(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))
