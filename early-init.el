;;; Fix
;; fix emacs transparent when there is alpha in xresources
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq package-enable-at-startup nil)
(setq inhibit-x-resources t)
(add-hook 'tty-setup-hook
  (lambda ()
    (message "In TYY")))
