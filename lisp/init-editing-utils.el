;;; init-editing-utils --- Nothing.
;;; Commentary:
;;; Code:

;; FCI will cause some strange actions.
;;(require 'fill-column-indicator)
;;(setq fci-rule-column 80)
;;(setq fci-rule-color "#783e57")
;;(setq fci-rule-width 1)
;;(add-hook 'prog-mode-hook 'fci-mode)

;;
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)
;; Some basic preferences
(setq-default cursor-type 'bar)
(setq-default blink-cursor-interval 0.4)
(setq-default bookmark-default-file
              (expand-file-name ".bookmarks.el" user-emacs-directory))
(setq-default buffers-menu-max-size 30)
(setq-default case-fold-search t)
;;(global-linum-mode t)
;;(add-hook 'after-init-hook 'global-linum-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'delete-selection-mode)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;;; No tabs in source code
(setq-default indent-tabs-mode nil)
;;; No backup files in the backend
(setq-default make-backup-files nil)
(setq-default mouse-yank-at-point t)
(setq-default save-interprogram-paste-before-kill t)
(setq-default scroll-preserve-screen-position 'always)
(setq-default set-mark-command-repeat-pop t)
(setq-default tooltip-delay 1.5)
;;; No wraping.
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)


(require 'autorevert);;build-in
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

;;; Very large file.
;;; 此处定义了一个交互式命令，没有和键绑定.
(require 'ffap)
(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;;; A simple visible bell which works in all terminal types.
(add-hook 'after-init-hook 'mode-line-bell-mode)
;; Beacon
(setq-default beacon-lighter "")
(setq-default beacon-size 5)
(add-hook 'after-init-hook 'beacon-mode)
;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun init-editing-utils-newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key
 (kbd "S-<return>") 'init-editing-utils-newline-at-end-of-line)
;;; Subword
(with-eval-after-load 'subword
  (diminish 'subword-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))
;;;
(add-hook 'after-init-hook 'global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode))
;;; symbol-overlay
(require 'symbol-overlay)
(dolist (hook '(prog-mode-hook
                html-mode-hook
                css-mode-hook
                yaml-mode-hook
                conf-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(with-eval-after-load 'symbol-overlay
  (diminish 'symbol-overlay-mode)
  (define-key
    symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key
    symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key
    symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))
;; Zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)
;;; browse-kill-ring
(require 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key
    browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key
    browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key
    browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)
;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Rectangle selections, and overwrite text when the selection is active
(cua-selection-mode t)                  ; for rectangles, CUA is nice
;; Handy key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
;;; avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)


;; multiple-cursors
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; 重新定义为
(global-set-key (kbd "s-;") 'mc/mark-next-like-this)
(global-set-key (kbd "s-'") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c s-;") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])



(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'global-page-break-lines-mode)
(with-eval-after-load 'page-break-lines
  (diminish 'page-break-lines-mode))

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------

(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;; whole-line-or-region :Cut/copy the current line if no region is active
(add-hook 'after-init-hook 'whole-line-or-region-mode)
(with-eval-after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-mode))

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar
         (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name
         (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)


(defun init-editing-utils-open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'init-editing-utils-open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly from BEG to ENDs."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;;; highlight-escape-sequence
(add-hook 'after-init-hook 'hes-mode)
;;;
(require 'guide-key)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/guide-key-sequence
      '("C-x"
        "C-c"
        "C-x 4"
        "C-x 5"
        "C-c ;"
        "C-c ; f"
        "C-c ' f"
        "C-x n"
        "C-x C-r"
        "C-x r"
        "M-s"
        "C-h"
        "C-c C-a"))
(add-hook 'after-init-hook 'guide-key-mode)
(with-eval-after-load 'guide-key
  (diminish 'guide-key-mode))


(provide 'init-editing-utils)
;;; init-editing-utils ends here
