;;; init-ivy ---  -*- lexical-binding: t -*-

;;; Commentary:
;; Ivy relies on nothing.
;; Swiper relies on Ivy,
;; and Counsel relies on both Swiper and Ivy.
;; Ivy helps you narrow down results when searching for something by typing.
;; Swiper is a replacement for isearch.
;; Counsel gives you extra functions that aren’t normally available.

;;; Code:

(add-hook 'after-init-hook 'ivy-mode)
(add-hook 'after-init-hook (lambda () (ivy-historian-mode t)))
(add-hook 'after-init-hook 'counsel-mode)



(after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-initial-inputs-alist
                '((man . "^")
                  (woman . "^")))

  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (dolist (k '("C-j" "C-RET")) (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (diminish 'ivy-mode)
  )


(setq-default counsel-mode-override-describe-bindings t)
(after-load 'counsel
  (diminish 'counsel-mode))

;; With respect to swiper
(after-load 'ivy
  (defun init-ivy-swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))

  (define-key ivy-mode-map (kbd "M-s /") 'init-ivy-swiper-at-point))

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; gives C-x C-f counsel features
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h f") 'counsel-describe-function)

(setq xref-show-xrefs-function 'ivy-xref-show-xrefs)

(provide 'init-ivy)
;;; init-ivy ends here
