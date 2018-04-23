;;; init-grep --- Nothing
;;; Commentary:
;;; Code:
(require 'ag)
(require 'rg)
(require 'wgrep)
(require 'wgrep-ag)

(setq-default grep-highlight-matches t
              grep-scroll-output t)


;;; ag 和 rg 是什么?
;; ag is the_silver_searcher.
;; See https://github.com/ggreer/the_silver_searcher
;; And See https://github.com/Wilfred/ag.el
;; ag>ack>grep
(when (executable-find "ag")
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "s-a") 'ag-project))

;; rg is the ripgrep
;; See https://github.com/BurntSushi/ripgrep
(when (executable-find "rg")
  ;; Set the <prefix>
  (rg-enable-default-bindings (kbd "s-s"))
  ;;<prefix> d  rg-dwim
  ;;<prefix> k  rg-kill-saved-searches
  ;;<prefix> l  rg-list-searches
  ;;<prefix> p  rg-project
  ;;<prefix> r  rg
  ;;<prefix> s  rg-save-search
  ;;<prefix> S  rg-save-search-as-name
  ;;<prefix> t  rg-literal
  (add-hook 'rg-mode-hook 'wgrep-ag-setup))


(provide 'init-grep)
;;; init-grep ends here
