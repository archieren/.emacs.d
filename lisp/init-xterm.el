;;; init-xterm --- Nothing
;;; Commentary:
;;; Code:

(require 'init-frame-hooks)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun init-xterm/console-frame-setup ()
  "Nothing."
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))



(add-hook 'after-make-console-frame-hooks 'init-xterm/console-frame-setup)

(provide 'init-xterm)
;;; init-xterm ends here
