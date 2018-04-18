;;; init-ruby --- Nothing.
;;; Commentary:
;;; Code:
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)
(require-package 'rspec-mode)
(require-package 'inf-ruby)
(require-package 'ruby-compilation)
(require-package 'robe)
(require-package 'goto-gem)
(require-package 'bundler)
(require-package 'yari)
(require-package 'yard-mode)
(require-package 'mmm-mode)


(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(after-load 'ruby-mode
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(add-hook 'ruby-mode-hook 'subword-mode)

(after-load 'page-break-lines
  (push 'ruby-mode page-break-lines-modes))
;;;
(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")

;;;

(after-load 'ruby-mode
  (let ((m ruby-mode-map))
    (define-key m [S-f7] 'ruby-compilation-this-buffer)
    (define-key m [f7] 'ruby-compilation-this-test)))

(after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))


;;; Robe
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))
(after-load 'company
  (dolist (hook (mapcar 'derived-mode-hook-name '(ruby-mode inf-ruby-mode html-erb-mode haml-mode)))
    (add-hook hook
              (lambda () (sanityinc/local-push-company-backend 'company-robe)))))


;;; ri
(defalias 'ri 'yari)
;;; yard

(add-hook 'ruby-mode-hook 'yard-mode)
(after-load 'yard-mode
  (diminish 'yard-mode))

;;; ERB

(defun sanityinc/ensure-mmm-erb-loaded ()
  "NoSpecification."
  (require 'mmm-erb))

(require 'derived)

(defun sanityinc/set-up-mode-for-erb (mode)
  "MODE."
  (add-hook (derived-mode-hook-name mode) 'sanityinc/ensure-mmm-erb-loaded)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(let ((html-erb-modes '(html-mode html-erb-mode nxml-mode)))
  (dolist (mode html-erb-modes)
    (sanityinc/set-up-mode-for-erb mode)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))

(mapc 'sanityinc/set-up-mode-for-erb
      '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
(sanityinc/set-up-mode-for-erb 'yaml-mode)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))

(provide 'init-ruby)
;;; init-ruby ends here
