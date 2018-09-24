;;; init-projectile --- Nothing.

;;; Commentary:

;;; Code:

(require 'projectile)


;; Shorter-mode-line
(defun init-projectile-update-mode-line ()
  "Report project in mode-line."
  (let* ((project-name (projectile-project-name))
         (project-type (projectile-project-type)))
    (format " [%s:%s]" project-name project-type))
  )

(with-eval-after-load 'projectile
  (projectile-mode t)
  (setq projectile-mode-line-fn 'init-projectile-update-mode-line)
  (define-key projectile-mode-map (kbd "s-x C-p") 'projectile-command-map)
  )
(provide 'init-projectile)
;;; init-projectile ends here
