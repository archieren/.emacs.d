;;; init-grep --- Nothing
;;; Commentary:
;;; Code:


(setq-default grep-highlight-matches t
              grep-scroll-output t)


;;; ag 和 rg 是什么?
;; ag is the_silver_searcher.
;; ag>ack>grep
(when (executable-find "ag")
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

;; rg is the ripgrep
;; See https://github.com/BurntSushi/ripgrep
(when (executable-find "rg")
  (global-set-key (kbd "M-?") 'rg-project))


(provide 'init-grep)
;;; init-grep ends here
