;;; init-clojure --- Nothing.
;;; Commentary:
;;  (require 'init-lisp)
;;; Code:
(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))


(setq nrepl-popup-stacktraces nil)

(after-load 'cider
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook 'init-whitespace-no-trailing-whitespace))

(after-load 'clojure-mode
  (after-load 'cider
    (after-load 'flycheck
      (flycheck-clojure-setup))))

(provide 'init-clojure)
;;; init-clojure ends here
