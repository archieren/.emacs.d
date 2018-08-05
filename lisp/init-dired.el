;;; init-dired --- Nothing
;;; Commentary:
;;; Code:
(require 'diredfl)
(require 'diff-hl)
(require 'guide-key)
(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))


(with-eval-after-load 'dired
  (diredfl-global-mode))

;; 只在一个buffer 里打开 dired-mode!
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
  )


(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))


(provide 'init-dired)
;;; init-dired ends here
