;;; init-html --- Nothing.
;;; Commentary:
;;; Code:
(require 'init-utils)
(require 'tagedit)
(with-eval-after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (define-key tagedit-mode-map (kbd "M-?") nil)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")


(provide 'init-html)
;;; init-html ends here
