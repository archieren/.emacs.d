;;; init-fonts.el --- Changing font sizes
;;; Commentary :

;;; Code:
(require 'cl)


(defun font-name-replace-size (font-name new-size)
  "Replace the size part in the FONT-NAME with NEW-SIZE."
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
The pixel size of the frame is kept (approximately) the same.
DELTA should be a multiple of 10, in the units used by the
:height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (font-name-replace-size (face-font 'default)
                                                new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  "For Chinese."
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  "For Chinese."
  (interactive)
  (increment-default-font-height -10))

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

(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)


(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  ;; TODO: submit as patch
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)



(provide 'init-fonts)
;;; init-fonts ends here
