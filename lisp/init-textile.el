;;; init-textile --- Nothing.
;;; Commentary:
;;; Code:
(require-package 'textile-mode)

(setq auto-mode-alist
      (cons '("\\.textile\\'" . textile-mode) auto-mode-alist))


(provide 'init-textile)
;;; init-textile ends here
