;;; init-local.el --- do some customizations

;; Copyright (C) 2018-2028 renjianpeng

;; Author: renjianpeng <renjp_r@hotmail.com>
;; Version:0.1
;; Keywords:faces ghc-mod
;; Maintainer: renjianpeng <renjp_r@hotmail.com>
;; Created: 1 Jan 2018
;; Homepage: http://no
;;; Commentary:

;;  Install:
;;  put the 'init-local.el' to your load path
;;  add follow sentences to your .emacs
;;  (require 'local-local)
;;

;;; Code:
;; Some Settings

;; 杂项
(delete-selection-mode t) ;; 选择一片区域后，用键入字符代替
(global-linum-mode t) ;; 显示行号
(setq-default cursor-type 'bar ) ;;;; Set cursor-type
(setq make-backup-files nil)  ;;;; 编辑时,不需要备份文件！
(desktop-save-mode nil)



;; The init-local.el 将经常被修改，所以定义一个快捷键 s-f .

(defun init-local-open-init-local ()
  "The init-local.el is used for customization."
  (interactive)
  (find-file (expand-file-name "lisp/init-local.el" user-emacs-directory)))

(global-set-key (kbd "s-f") 'init-local-open-init-local)

;; 只在一个buffer 里打开 dired-mode!
(put 'dired-find-alternate-file 'disabled nil)
(after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file))

;; Fonts settings
(defun init-local-set-font (English-font Chinese-font English-font-size Chinese-font-size)
  "Set ENGLISH-FONT CHINESE-FONT ENGLISH-FONT-SIZE CHINESE-FONT-SIZE."
  (set-face-attribute  'default   nil  :font  (font-spec :family English-font :size English-font-size))
  (dolist (script '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font  (frame-parameter nil 'font) script (font-spec :family Chinese-font :size Chinese-font-size))))

(init-local-set-font    "DejaVu Sans Mono" "Noto Sans Mono CJK SC" 14 14)
;;Let the Latin and han 's font the same,it is not good idea.
;;(init-local-set-font "Noto Sans Mono CJK SC" "Noto Sans Mono CJK SC" 14 14)
;;xft:-GOOG-Noto Sans CJK SC-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1
;;xft:-PfEd-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1
(setq face-font-rescale-alist '(("DejaVu Sans Mono" . 1.0) ("Noto Sans Mono CJK SC" . 1.2)))

;;; 设置 shell
(setq shell-file-name "/usr/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
;;; 设置ido-mode 但这好像是内置模式，可能没必要！
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere t)
(ido-mode 1)

;;; 设置 window-number
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;;; 设置Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; 设置 multiple-cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "s-;") 'mc/mark-next-like-this)
(global-set-key (kbd "s-'") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c s-;") 'mc/mark-all-like-this)

;;;Haskell 编程 : ghc-mode is a submode of haskell mode!!! I want it!

(require-package 'ghc)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))



(provide 'init-local)
;;; init-local.el ends here
