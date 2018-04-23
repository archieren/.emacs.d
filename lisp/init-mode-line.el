;;; init-mode-line --- Nothing.
;;; Commentary:
;;; Code:

;;; Powerline
(require 'powerline)
(powerline-default-theme)
(custom-set-faces
 '(powerline-active1 ((t (:background "#783e57" :foreground "#fffff"))))
 '(powerline-active2 ((t (:background "grey20" :foreground "#ffffff")))))

;; These three lines you really need.
;; 不费那个劲，smart-mode-line 配起来费劲.

;;(require 'smart-mode-line)
;;(require 'smart-mode-line-powerline-theme)
;;(setq sml/no-confirm-load-theme t)
;;(setq sml/theme 'respectful)
;;(sml/setup)

;;; airline-themes
;;(require 'airline-themes)
;;(load-theme 'airline-solarized-alternate-gui)

(provide 'init-modeline)
;;; init-mode-line ends here
