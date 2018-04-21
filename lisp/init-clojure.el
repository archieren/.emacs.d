;;; init-clojure --- Nothing.
;;; Commentary:

;;; Code:
(require 'clojure-mode)
(require 'cider)
(require 'init-lisp);; init-lisp-lisp-setup defined in init-lisp.el
(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'init-lisp-lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))


(with-eval-after-load 'cider
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook 'init-whitespace-no-trailing-whitespace))

(with-eval-after-load 'clojure-mode
  (with-eval-after-load 'cider
    (with-eval-after-load 'flycheck
      (flycheck-clojure-setup))))

(provide 'init-clojure)
;;; init-clojure ends here
