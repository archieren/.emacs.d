;;; init-css --- Nothing.
;;; Commentary:
;;; Code:
;;; Colourise CSS colour literals

(require 'init-utils)
(require 'rainbow-mode)
(require 'mmm-mode)
(require 'sass-mode)
(require 'skewer-mode)
(require 'skewer-less)
(require 'css-eldoc)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))


;;; Embedding in html

(with-eval-after-load 'mmm-vars
  (mmm-add-group
   'html-css
   '((css-cdata
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
      :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
      :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t]*\n?"
      :back "[ \t]*</style>"
      :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css-inline
      :submode css-mode
      :face mmm-code-submode-face
      :front "style=\""
      :back "\"")))
  (dolist (mode (list 'html-mode 'nxml-mode))
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))




;;; SASS and SCSS
;; Prefer the scss-mode built into Emacs
;; Emacs 自己启用了 scss-mode.
;;(unless (fboundp 'scss-mode)  (require-package 'scss-mode))
(setq-default scss-compile-at-save nil)

;;; LESS
;; Prefer the scss-mode built into Emacs
;; Emacs自己启用了less-css-mode.
;;(unless (fboundp 'less-css-mode)   (require-package 'less-css-mode))
(add-hook 'less-css-mode-hook 'skewer-less-mode)

;; Skewer CSS
(add-hook 'css-mode-hook 'skewer-css-mode)

;;; Use eldoc for syntax hints
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)


(provide 'init-css)
;;; init-css ends here
