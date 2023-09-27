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

(defun my/tty-hook ()
  "tty setup hook"
  (interactive)
  (xterm-mouse-mode 1)
  (setq xterm-box-blink-seq "\e[1 q")
  (send-string-to-terminal xterm-box-blink-seq))

(add-hook 'tty-setup-hook 'my/tty-hook)
