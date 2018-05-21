;;; init-whitespace --- Nothing.
;;; Commentary:
;;; Code:
;;; 一行超过80列,则变色

(require 'diminish)

(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)
;;; 尾部的空格
(setq-default show-trailing-whitespace t)
;;; Whitespace
(defun  init-whitespace-no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook
                eshell-mode-hook))
  (add-hook hook #' init-whitespace-no-trailing-whitespace))

(require 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(diminish 'whitespace-cleanup-mode)
(global-set-key [remap just-one-space] 'cycle-spacing)

(provide 'init-whitespace)
;;; init-whitespace ends here
