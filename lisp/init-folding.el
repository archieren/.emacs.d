;;; init-folding --- Nothing.
;;; Commentary:
;; 折叠、折纸
;;; Code:

(add-hook 'prog-mode-hook 'origami-mode)
(after-load 'origami
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))


(provide 'init-folding)
;;; init-folding ends here
