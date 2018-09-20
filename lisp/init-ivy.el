;;; init-ivy ---  -*- lexical-binding: t -*-

;;; Commentary:
;; Ivy relies on nothing.
;; Swiper relies on Ivy,
;; and Counsel relies on both Swiper and Ivy.
;; Ivy helps you narrow down results when searching for something by typing.
;; Swiper is a replacement for isearch.
;; Counsel gives you extra functions that aren’t normally available.

;;; Code:
(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'counsel-etags)

(add-hook 'after-init-hook 'ivy-mode)
(add-hook 'after-init-hook (lambda () (ivy-historian-mode t)))
(add-hook 'after-init-hook 'counsel-mode)



(with-eval-after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format "%d/%d "
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
(with-eval-after-load 'counsel
  (diminish 'counsel-mode)
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s ."
            )
    (warn "\nWARNING: Could not find the ripgrep executable.")))

;; With respect to swiper
(with-eval-after-load 'ivy
  (defun init-ivy-swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))

  (define-key ivy-mode-map (kbd "M-s /") 'init-ivy-swiper-at-point))

(with-eval-after-load 'counsel-etags
  (add-to-list 'counsel-etags-ignore-directories "build_clang")
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-directories ".stack-work")
  (add-to-list 'counsel-etags-ignore-directories ".vscode")
  ;; counsel-etags-ignore-filenames supports wildcast
  ;; haskell
  (add-to-list 'counsel-etags-ignore-filenames "*.yaml")
  (add-to-list 'counsel-etags-ignore-filenames "*.cabal")
  (add-to-list 'counsel-etags-ignore-filenames "*.hi")
  ;;rust
  (add-to-list 'counsel-etags-ignore-filenames "*.lock")
  (add-to-list 'counsel-etags-ignore-filenames "*.toml")
  ;;clang
  (add-to-list 'counsel-etags-ignore-filenames "*.clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local))))

(global-set-key (kbd "C-c C-r")   'ivy-resume)
(global-set-key (kbd "C-x b")     'ivy-switch-buffer)
(global-set-key (kbd "C-s")       'swiper)  ;; replaces i-search with swiper
;; Leave the kbd "M-x" for smex.
(global-set-key (kbd "s-x x")     'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f")   'counsel-find-file) ;; gives C-x C-f counsel features
(global-set-key (kbd "C-h v")     'counsel-describe-variable)
(global-set-key (kbd "C-h f")     'counsel-describe-function)
(global-set-key (kbd "M-y")       'counsel-yank-pop)

(global-set-key (kbd "s-x .")     'counsel-etags-find-tag-at-point)
(global-set-key (kbd "s-x t")     'counsel-etags-grep-symbol-at-point)
(global-set-key (kbd "s-x s")     'counsel-etags-find-tag)
;; Here s-x {x,.,t,s} ,s-x {a,r} in init-grep, s-x {n,p,a} in init-editing-utils
;;

(require 'xref)
(setq xref-show-xrefs-function 'ivy-xref-show-xrefs)

(provide 'init-ivy)
;;; init-ivy ends here
