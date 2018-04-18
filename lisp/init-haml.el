;;; init-haml --- Nothing.
;;; Commentary:
;;; Code:
(require-package 'haml-mode)

(after-load 'haml-mode
  (define-key haml-mode-map (kbd "C-o") 'open-line))

(provide 'init-haml)
;;; init-haml ends here
