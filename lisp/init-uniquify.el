;;; init-uniquify --- Nothing.
;;; Commentary:
;; 同名文件的buffer命名
;;; Code:
;; Nicer naming of buffers for files with identical names

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


(provide 'init-uniquify)

;;; init-uniquify ends here
