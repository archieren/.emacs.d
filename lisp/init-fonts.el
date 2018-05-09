;;; init-fonts.el --- Changing font sizes
;;; Commentary:
;; 目前不清楚想干什么！留给中文字体调整用!
;;; Code:

;; 用等宽字体，比较好对齐中英文!
;; Fonts settings


(set-face-attribute  'default
                     nil
                     :font  (font-spec :family "DejaVu Sans Mono"
                                       :size 14))

(dolist (script '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font  t  ;;(frame-parameter nil 'font)
                     script
                     (font-spec :family "Noto Sans Mono CJK SC"
                                :size 12)))
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

(require 'cl-lib)
(require 'init-fontawesome-data)
(require 'init-yi-data)
(declare-function ivy-read "ivy")

(defun glyph-propertize (glyph)
  "GLYPH is the unicode of a font."
  (propertize glyph ))

(defun init-fonts-construct-candidates (fonts-alist)
  "FONTS-ALIST."
  (mapcar (lambda (fontawesome)
            (cons (concat
                   (car fontawesome)
                   "->"
                   (glyph-propertize (cdr fontawesome)))
                  (cdr fontawesome)))
          fonts-alist))



;;;###autoload
(defun counsel-fontawesome ()
  "Nothing."
  (interactive)
  (require 'ivy)
  (ivy-read "Font awesome> " (init-fonts-construct-candidates fontawesome-alist)
            :action (lambda (font)
                      (insert (cdr font)))))

(defun counsel-yi()
  "Nothing."
  (interactive)
  (require 'ivy)
  (ivy-read "Yi > " (init-fonts-construct-candidates yi-alist)
            :action (lambda (font)
                      (insert (cdr font)))))


(provide 'init-fonts)
;;; init-fonts ends here
