;;; init-flycheck.el --- Nothing
;;; Commentary:
;;; Code:
(require 'flycheck)
(global-flycheck-mode t)
(setq flycheck-display-errors-function
      #'flycheck-display-error-messages-unless-error-list)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(custom-set-variables '(flycheck-mode-line-prefix ""))

(require 'flycheck-package)
(flycheck-package-setup)

(require 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook
          #'flycheck-color-mode-line-mode)
(custom-set-variables '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id ))
                      '(powerline-default-separator (quote slant)))
(custom-set-faces
 '(flycheck-color-mode-line-error-face ((t (:background "#073642" :foreground "#dc322f" :weight bold :slant italic))))
 '(flycheck-color-mode-line-warning-face ((t (:background "#073642" :foreground "#cb4b16" :weight bold :slant italic))))
 '(flycheck-color-mode-line-info-face ((t (:background "#073642" :foreground "#2aa198" :weight bold :slant italic)))))
;;
;; Flycheck使用的搜索目录.



(provide 'init-flycheck)
;;; init-flycheck ends here
