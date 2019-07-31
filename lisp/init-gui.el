;;; init-gui --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
;; Some basic preferences
(setq-default cursor-type 'bar)
(setq-default blink-cursor-interval 0.4)
(setq-default bookmark-default-file
              (expand-file-name ".bookmarks.el" user-emacs-directory))
(setq-default buffers-menu-max-size 30)
(setq-default case-fold-search t)
;;; Linum-mode has some with multicursor.
;;(global-linum-mode t)
;;(add-hook 'after-init-hook 'global-linum-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'delete-selection-mode)
;;(setq-default ediff-split-window-function 'split-window-horizontally)
;;(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;;; No tabs in source code
(setq-default indent-tabs-mode nil)
;;; No backup files in the backend
(setq-default make-backup-files nil)
(setq-default mouse-yank-at-point t)
(setq-default save-interprogram-paste-before-kill t)
(setq-default scroll-preserve-screen-position 'always)
(setq-default set-mark-command-repeat-pop t)
(setq-default tooltip-delay 0.5)
;;; No wraping.
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)
;;; Beacon
(require 'beacon)
(with-eval-after-load 'beacon
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  (add-hook 'after-init-hook 'beacon-mode))

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

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

(defun init-gui-toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'init-gui-toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

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

;;----------------------------------------------------------------------------
;;; 设置 window-number
;;----------------------------------------------------------------------------
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;;----------------------------------------------------------------------------
;;; Multi major mode
;;----------------------------------------------------------------------------
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

;;----------------------------------------------------------------------------
;;; 字体设置 - 用等宽字体，比较好对齐中英文!
;;----------------------------------------------------------------------------
(set-face-attribute  'default
                     nil
                     :font  (font-spec :family "DejaVu Sans Mono"
                                       :size 14))
(dolist (script '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font  t  ;;(frame-parameter nil 'font)
                     script
                     (font-spec :family "Noto Sans Mono CJK SC"
                                :size 14)))
;;; Fall Back!
(set-fontset-font t
                  '(#xf000 . #xf8ff)
                  (font-spec
                   :family "Font Awesome 5 Free Solid"
                   :style  "Solid"
                   :size 12) )
;;;  Fall Back!
;;But now,it doesn't work!So,quote it.
;;Can I merge "Font Awesome 5 Free Solid" with "Font Awesome 5 Brands Regular?"
(set-fontset-font t
                  '(#xf000 . #xf8ff)
                  (font-spec
                   :family "Font Awesome 5 Brands Regular"
                   :style  "Regular"
                   :size 12)
                  nil
                  'append)

(setq face-font-rescale-alist '(("DejaVu Sans Mono" . 1.0)
                                ("Noto Sans Mono CJK SC" . 1.0)))
;;;
(add-hook 'after-init-hook 'default-text-scale-mode)
(require 'visual-fill-column)
(defun  init-font-maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  ;; TODO: submit as patch
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook
                'visual-fill-column--adjust-window
                nil
                t)
    (remove-hook 'after-setting-font-hook
                 'visual-fill-column--adjust-window
                 t)))
(add-hook 'visual-fill-column-mode-hook
          ' init-font-maybe-adjust-visual-fill-column)

;;----------------------------------------------------------------------------
;;;  Whitespace
;;----------------------------------------------------------------------------
(require 'diminish)
(require 'whitespace)
(require 'whitespace-cleanup-mode)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 256)
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)
(diminish 'whitespace-cleanup-mode)
;;; 尾部的空格
(setq-default show-trailing-whitespace t)

;;; Whitespace
(defun  init-gui-no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace nil))
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook
                eshell-mode-hook
                cmake-mode-hook))
  (add-hook hook #'init-gui-no-trailing-whitespace))
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)

;;; ??
(global-set-key [remap just-one-space] 'cycle-spacing)

;;----------------------------------------------------------------------------
;;;  Modeline
;;----------------------------------------------------------------------------
(require 'powerline)
(powerline-default-theme)
(custom-set-faces
 '(powerline-active1 ((t (:background "#783e57" :foreground "#ffffff"))))
 '(powerline-active2 ((t (:background "grey20"  :foreground "#ffffff"))))
 )


(provide 'init-gui)
;;; init-gui ends here
