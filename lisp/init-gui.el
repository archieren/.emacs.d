;;; init-gui --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names.同名文件的buffer命名.
;;----------------------------------------------------------------------------
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;----------------------------------------------------------------------------
;; Recentf.
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------

(require 'winner)

(add-hook 'after-init-hook 'winner-mode)

;; Make "C-x o" prompt for a target window when there are more than 2
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)



;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  "SPLIT-FUNCTION."
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun init-window-toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'init-window-toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

;;; 设置 window-number
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)


(provide 'init-gui)
;;; init-gui ends here
