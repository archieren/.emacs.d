;;; init-smex --- Nothing
;;; Commentary:
;;; Code:
;;; 设置ido-mode 但这好像是内置模式，可能没必要！
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere t)
(ido-mode 1)


;; Use smex to handle M-x
(when (maybe-require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  ;;(global-set-key [remap execute-extended-command] 'smex)
  ;;; 设置Smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )


(provide 'init-smex)
;;; init-smex ends here
