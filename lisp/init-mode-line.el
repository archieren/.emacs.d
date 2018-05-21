;;; init-mode-line --- Nothing.
;;; Commentary:
;;; Code:

;;; Powerline
(require 'powerline)
(powerline-default-theme)
(custom-set-faces
 '(powerline-active1 ((t (:background "#783e57" :foreground "#ffffff"))))
 '(powerline-active2 ((t (:background "grey20"  :foreground "#ffffff"))))
 '(flycheck-color-mode-line-error-face ((t (:background "#073642" :foreground "#dc322f" :weight bold :slant italic))))
 '(flycheck-color-mode-line-warning-face ((t (:background "#073642" :foreground "#cb4b16" :weight bold :slant italic))))
 '(flycheck-color-mode-line-info-face ((t (:background "#073642" :foreground "#2aa198" :weight bold :slant italic)))))

(provide 'init-modeline)
;;; init-mode-line ends here
