;;; init-markdown --- Nothing.
;;; Commentary:
;;; Code:
(require 'init-utils)
(require 'markdown-mode)
(require 'whitespace-cleanup-mode)
(add-auto-mode 'markdown-mode "\\.md\\.html\\'")
(with-eval-after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


(provide 'init-markdown)
;;; init-markdown ends here
