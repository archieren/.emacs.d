;;; init-http --- Nothing.
;;; Commentary:
;;; Code:
(require 'init-utils)
(require 'restclient)
(add-auto-mode 'restclient-mode "\\.rest\\'")

(defun sanityinc/restclient ()
  "Nothing."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))


(provide 'init-http)
;;; init-http ends here
