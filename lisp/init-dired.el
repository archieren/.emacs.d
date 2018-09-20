;;; init-dired --- Nothing
;;; Commentary:
;;; Code:
(require 'diredfl)
(require 'diff-hl)
(require 'guide-key)
(setq-default dired-dwim-target t)

(with-eval-after-load 'dired
  (diredfl-global-mode)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
  ;; 只在一个buffer 里打开 dired-mode!
  (put 'dired-find-alternate-file 'disabled nil)
  ;;Dired里显示一些版本信息.
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  )

(provide 'init-dired)
;;; init-dired ends here
