;;; Fix
;; fix emacs transparent when there is alpha in xresources
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;;; General Settings
(setq inhibit-startup-message t
      visible-bell t)

;; no gui dialog box
(setq use-dialog-box nil)

;; no graphic interfaces
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (set-fringe-mode 10)

;; history in minibuffer
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

;; ;;; Straight.el Settings
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")
;;                          ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")))

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

;;; Color theme
(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-light-soft t))

;;; Evil
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

;;; Which key
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;;;; Completions
;;; Minibuffer Completions
(use-package vertico
  :init
  (vertico-mode)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult)
(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-S-a" . embark-act)))


;;; Completion in region
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))
