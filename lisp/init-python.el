;;; init-python --- Nothing.
;;; Commentary:
;;; Code:

(require 'elpy)
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

;; run command `pip install rope jedi flake8 autopep8 yapf` in shell,
;; or just check https://github.com/jorgenschaefer/elpy
(elpy-enable)

(defun python-mode-hook-setup ()
  "Nothing."
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  )

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;(elpy-use-ipython) ;;deprecated!

;; Just for fun.
(add-hook 'python-mode-hook (lambda () (setq mode-name "")))
;;ein
;;See https://github.com/millejoh/emacs-ipython-notebook
(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)
(require 'dash)
(require 'websocket)
(require 'request)
(require 'request-deferred)
(require 's)
(require 'auto-complete)
(require 'skewer-mode)

(custom-set-variables '(ein:jupyter-default-server-command "jupyter")
                                        ;'(ein:jupyter-server-args "notebook")
                      '(ein:jupyter-default-notebook-directory "~/Projects/NoteBook/"))
;; Choose the comletion-backend
;; Look up and See the code in ein-subpackages.el.
;; At least,it has four completion-backend.
;;(setq ein:use-auto-complete-superpack t) is suggested on web.
(with-eval-after-load 'company
  (with-eval-after-load 'ein
    (setq ein:completion-backend 'ein:use-company-backend)
    (setq ein:use-smartrep nil)))

;;; See http://millejoh.github.io/emacs-ipython-notebook/#
;;; {{Key-Binding Reference
;; Keybindings - Notebook
;; key             binding
;; ---             -------
;;
;; C-c             Prefix Command
;; C-x             Prefix Command
;; ESC             Prefix Command
;; .               ein:notebook-complete-dot
;; C-:             ein:shared-output-eval-string
;; <C-down>        ein:worksheet-goto-next-input
;; <C-up>          ein:worksheet-goto-prev-input
;; <M-S-return>    ein:worksheet-execute-cell-and-insert-below
;; <M-down>        ein:worksheet-move-cell-down
;; <M-up>          ein:worksheet-move-cell-up
;;
;; C-x C-s         ein:notebook-save-notebook-command
;; C-x C-w         ein:notebook-rename-command
;;
;; M-RET           ein:worksheet-execute-cell-and-goto-next
;; M-,             ein:pytools-jump-back-command
;; M-.             ein:pytools-jump-to-source-command
;; M-n             ein:worksheet-next-input-history
;; M-p             ein:worksheet-previous-input-history
;;
;; C-c C-a         ein:worksheet-insert-cell-above
;; C-c C-b         ein:worksheet-insert-cell-below
;; C-c C-c         ein:worksheet-execute-cell
;; C-c C-e         ein:worksheet-toggle-output
;; C-c C-f         ein:pytools-request-tooltip-or-help
;; C-c TAB         ein:completer-complete
;; C-c C-k         ein:worksheet-kill-cell
;; C-c C-l         ein:worksheet-clear-output
;; C-c RET         ein:worksheet-merge-cell
;; C-c C-n         ein:worksheet-goto-next-input
;; C-c C-o         ein:console-open
;; C-c C-p         ein:worksheet-goto-prev-input
;; C-c C-q         ein:notebook-kill-kernel-then-close-command
;; C-c C-r         ein:notebook-restart-kernel-command
;; C-c C-s         ein:worksheet-split-cell-at-point
;; C-c C-t         ein:worksheet-toggle-cell-type
;; C-c C-u         ein:worksheet-change-cell-type
;; C-c C-v         ein:worksheet-set-output-visibility-all
;; C-c C-w         ein:worksheet-copy-cell
;; C-c C-x         ein:tb-show
;; C-c C-y         ein:worksheet-yank-cell
;; C-c C-z         ein:notebook-kernel-interrupt-command
;; C-c ESC         Prefix Command
;; C-c !           ein:worksheet-rename-sheet
;; C-c +           ein:notebook-worksheet-insert-next
;; C-c -           ein:notebook-worksheet-delete
;; C-c 1           ein:notebook-worksheet-open-1th
;; C-c 2           ein:notebook-worksheet-open-2th
;; C-c 3           ein:notebook-worksheet-open-3th
;; C-c 4           ein:notebook-worksheet-open-4th
;; C-c 5           ein:notebook-worksheet-open-5th
;; C-c 6           ein:notebook-worksheet-open-6th
;; C-c 7           ein:notebook-worksheet-open-7th
;; C-c 8           ein:notebook-worksheet-open-8th
;; C-c 9           ein:notebook-worksheet-open-last
;; C-c {           ein:notebook-worksheet-open-prev-or-last
;; C-c }           ein:notebook-worksheet-open-next-or-first
;; C-c C-S-l       ein:worksheet-clear-all-output
;; C-c C-#         ein:notebook-close
;; C-c C-'         ein:worksheet-turn-on-autoexec
;; C-c C-,         ein:pytools-jump-back-command
;; C-c C-.         ein:pytools-jump-to-source-command
;; C-c C-/         ein:notebook-scratchsheet-open
;; C-c C-;         ein:shared-output-show-code-cell-at-point
;; C-c <down>      ein:worksheet-move-cell-down
;; C-c <up>        ein:worksheet-move-cell-up
;;
;; C-c M-+         ein:notebook-worksheet-insert-prev
;; C-c M-w         ein:worksheet-copy-cell
;; C-c M-{         ein:notebook-worksheet-move-prev
;; C-c M-}         ein:notebook-worksheet-move-next
;; Keybindings - Connect
;; In Python (or any other) buffer, you can connect to any open notebook by
;;            M-x ein:connect-to-notebook then choose appropriate notebook.
;; After connecting to the notebook (and hence its kernel), the following
;; commands are available.

;; key             binding
;; ---             -------
;;
;; C-c             Prefix Command
;; ESC             Prefix Command
;; .               ein:completer-dot-complete
;; C-:             ein:shared-output-eval-string
;;
;; M-,             ein:pytools-jump-back-command
;; M-.             ein:pytools-jump-to-source-command
;;
;; C-c C-a         ein:connect-toggle-autoexec
;; C-c C-c         ein:connect-run-or-eval-buffer
;; C-c C-f         ein:pytools-request-tooltip-or-help
;; C-c TAB         ein:completer-complete
;; C-c C-l         ein:connect-reload-buffer
;; C-c C-o         ein:console-open
;; C-c C-r         ein:connect-eval-region
;; C-c C-x         ein:tb-show
;; C-c C-z         ein:connect-pop-to-notebook
;; C-c C-,         ein:pytools-jump-back-command
;; C-c C-.         ein:pytools-jump-to-source-command
;; C-c C-/         ein:notebook-scratchsheet-open
;;; }}
(provide 'init-python)
;;; init-python ends here
