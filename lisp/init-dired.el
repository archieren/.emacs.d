;;; init-dired --- Nothing
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))


(after-load 'dired
  (diredfl-global-mode))

;; 只在一个buffer 里打开 dired-mode!
(put 'dired-find-alternate-file 'disabled nil)
(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))


(after-load 'dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))


(provide 'init-dired)
;;; init-dired ends here
