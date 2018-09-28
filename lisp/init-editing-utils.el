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
(when (fboundp 'electric-indent-mode)
  (add-hook 'after-init-hook 'electric-indent-mode))

(require 'autorevert);;build-in
(add-hook 'after-init-hook 'global-auto-revert-mode)
(diminish 'auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun init-editing-utils-newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'init-editing-utils-newline-at-end-of-line)

;;; Subword
(with-eval-after-load 'subword
  (diminish 'subword-mode))

;;; RainBow
(require 'rainbow-delimiters)
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; Prettify-Symbols
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

;;;
(require 'undo-tree)
(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode)
  (add-hook 'after-init-hook 'global-undo-tree-mode))

;;; symbol-overlay
(require 'symbol-overlay)
(with-eval-after-load 'symbol-overlay
  (diminish 'symbol-overlay-mode)
  (dolist (hook '(prog-mode-hook
                  html-mode-hook
                  css-mode-hook
                  yaml-mode-hook
                  conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (define-key
    symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key
    symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key
    symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

;;; 杂技类命令
;; "M-z" zap-to-char , "M-Z" zap-up-to-char
;; Zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)
;;; Kill from point back to the first non-whitespace character on the line.
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
;; Open-line
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
;;; 这更像键盘侠的杂技
;; Avy is a GNU Emacs package for jumping to visible text using a char-based decision tree.
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char-timer)



;;; browse-kill-ring
(require 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (setq browse-kill-ring-separator "\f")
  (global-set-key (kbd "M-Y") 'browse-kill-ring)
  (define-key
    browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key
    browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key
    browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

;;----------------------------------------------------------------------------
;; Page break lines
;; C-q C-l插入 ^L ，C-x {],[}导航
;;----------------------------------------------------------------------------
(require 'page-break-lines)
(with-eval-after-load 'page-break-lines
  (diminish 'page-break-lines-mode)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (push 'browse-kill-ring-mode page-break-lines-modes)
  )

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Rectangle selections, and overwrite text when the selection is active
;; Builtins.I just turn it on.
(cua-selection-mode t)                  ; for rectangles, CUA is nice




;; Handy key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)


;; multiple-cursors
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; 用windows键,重新定义为
(global-set-key (kbd "s-x n") 'mc/mark-next-like-this)
(global-set-key (kbd "s-x p") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-x m") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])




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
(require 'whole-line-or-region)
(with-eval-after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode "")
  (add-hook 'after-init-hook 'whole-line-or-region-mode))

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
(require 'highlight-escape-sequences)
(with-eval-after-load 'highlight-escape-sequences
  (add-hook 'after-init-hook 'hes-mode))
;;;
(require 'guide-key)
(with-eval-after-load 'guide-key
  (diminish 'guide-key-mode)
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/guide-key-sequence t)
  (add-hook 'after-init-hook 'guide-key-mode))

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(require 'yasnippet-snippets)

(provide 'init-editing-utils)
;;; init-editing-utils ends here
