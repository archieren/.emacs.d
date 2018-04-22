;;; init-haml --- Nothing.
;;; Commentary:
;; haml 是什么？ 好像被sass-mode给加载上来了.
;;; Code:
(require 'haml-mode)
(with-eval-after-load 'haml-mode
  (define-key haml-mode-map (kbd "C-o") 'open-line))

(provide 'init-haml)
;;; init-haml ends here
