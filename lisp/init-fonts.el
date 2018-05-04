;;; init-fonts.el --- Changing font sizes
;;; Commentary:
;; 目前不清楚想干什么！留给中文字体调整用！
;;; Code:

;; 用等宽字体，比较好对齐中英文！
;; Fonts settings
(defun init-fonts-set-font (English-font
                            Chinese-font
                            English-font-size
                            Chinese-font-size)
  "Set ENGLISH-FONT CHINESE-FONT ENGLISH-FONT-SIZE CHINESE-FONT-SIZE."
  (set-face-attribute  'default
                       nil
                       :font  (font-spec :family English-font
                                         :size English-font-size))
  (dolist (script '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font  t  ;;(frame-parameter nil 'font)
                       script
                       (font-spec :family Chinese-font
                                  :size Chinese-font-size))))

(init-fonts-set-font    "DejaVu Sans Mono" "Noto Sans Mono CJK SC" 14 12)
;;Let the Latin and han 's font the same,it is not good idea.
;;(init-local-set-font "Noto Sans Mono CJK SC" "Noto Sans Mono CJK SC" 14 14)
;;xft:-GOOG-Noto Sans CJK SC-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1
;;xft:-PfEd-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1
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



(provide 'init-fonts)
;;; init-fonts ends here
