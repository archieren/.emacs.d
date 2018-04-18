;;; init-frame-hooks.el --- Nothing
;;; Commentary:
;;; Code:
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window system frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst init-frame-hooks/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when init-frame-hooks/initial-frame
                  (run-after-make-frame-hooks init-frame-hooks/initial-frame))))


(provide 'init-frame-hooks)
;;; init-frame-hooks ends here