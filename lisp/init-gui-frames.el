;;; init-gui-frames --- Nothing
;;; Commentary:
;;; Code:

;;; Stop the C-z command.
(defun init-gui-frames/doNothing ()
  "Nothing."
  (interactive)
  )
(global-set-key (kbd "C-z") 'init-gui-frames/doNothing)
;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))




(provide 'init-gui-frames)
;;; init-gui-frames ends here