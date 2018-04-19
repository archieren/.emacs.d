;;; init-flycheck.el --- Nothing
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)


(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; Flycheck使用的搜索目录.
(setq-default flycheck-emacs-lisp-load-path 'inherit)


(provide 'init-flycheck)
;;; init-flycheck ends here
