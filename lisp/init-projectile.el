;;; init-projectile --- Nothing.

;;; Commentary:

;;; Code:

(require 'projectile)


;; Shorter-mode-line
;;以前的: (setq-default projectile-mode-line '(:eval (if (file-remote-p default-directory) "" (format " [%s]" (projectile-project-name)))))
;; 新版本里,好像是个bug,我就重新定义一个.
;; 但他好像还在修改,我这个估计也就用几天.
(defun init-projectile-update-mode-line ()
  "Report project in mode-line."
  (let* ((project-name (projectile-project-name))
         (project-type (projectile-project-type))
         (message (format " [%s:%s]" project-name project-type)))
    ;;(make-local-variable 'projectile-mode-line)
    (setq projectile-mode-line message))
  (force-mode-line-update))

(setq-default projectile-mode-line " ")
(make-variable-buffer-local 'projectile-mode-line)
(with-eval-after-load 'projectile
  (add-hook 'after-init-hook 'projectile-mode)
  (define-key projectile-mode-map (kbd "s-x C-p") 'projectile-command-map)
  ;;(remove-hook 'find-file-hook #'projectile-update-mode-line)
  (add-hook    'find-file-hook #'init-projectile-update-mode-line)

  )
(provide 'init-projectile)
;;; init-projectile ends here
