;;; init-flycheck.el --- Nothing
;;; Commentary:
;;; Code:
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
(setq-default flycheck-emacs-lisp-load-path 'inherit)


(require 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; Flycheck使用的搜索目录.



(provide 'init-flycheck)
;;; init-flycheck ends here
