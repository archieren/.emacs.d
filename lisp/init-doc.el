;;; init-doc --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-utils)

;;; Textile
(require 'textile-mode)
(setq auto-mode-alist
      (cons '("\\.textile\\'" . textile-mode) auto-mode-alist))

;;; Markdown
(require 'markdown-mode)
(require 'whitespace-cleanup-mode)
(add-auto-mode 'markdown-mode "\\.md\\.html\\'")
(push 'markdown-mode whitespace-cleanup-mode-ignore-modes)

;;; CSV
(require 'csv-mode)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(setq csv-separators '("," ";" "|" " "))

;;; nXML mode is an Emacs major-mode for editing XML documents.
(require 'nxml-mode)
(add-auto-mode
 'nxml-mode
 (concat "\\."
         (regexp-opt
          '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
            "gpx" "tcx" "plist"))
         "\\'"))
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)

;; 提供两个小功能.
;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun sanityinc/pp-xml-region (beg end)
  "Pretty format XML markup in region from BEG to ENDs.
It inserts linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's indentation rules."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  ;; Use markers because our changes will move END
  (setq beg (set-marker (make-marker) beg)
        end (set-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "\>[ \\t]*\<" end t)
      (backward-char) (insert "\n"))
    (nxml-mode)
    (indent-region beg end)))

;; Integration with tidy for html + xml
;; tidy should be installed on system.
(defun sanityinc/tidy-buffer-xml (beg end)
  "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))

;;; Html:Html用build-in里的sgml-mode来实现.
(require 'tagedit)
(require 'sgml-mode)
(tagedit-add-paredit-like-keybindings)
(define-key tagedit-mode-map (kbd "M-?") nil)
(add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1)))
(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;;; Haml 是什么？ 好像被sass-mode给加载上来了.
;; 原来sass-mode是它的派生mode.
(require 'haml-mode)
(define-key haml-mode-map (kbd "C-o") 'open-line)

;;;----------------------------------------------
;;; CSS / SASS / SCSS
;;;----------------------------------------------
(require 'rainbow-mode)
(require 'mmm-mode)
(require 'sass-mode) ;; sass 是 css的一个扩展.
(require 'skewer-mode)
(require 'skewer-less)
(require 'css-eldoc)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))
;;Embedding in html
(require 'mmm-vars)
(mmm-add-group
 'html-css
 '((css-cdata
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css-inline
    :submode css-mode
    :face mmm-code-submode-face
    :front "style=\""
    :back "\"")))
(dolist (mode (list 'html-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))
;; SASS and SCSS
;; Prefer the scss-mode built into Emacs
;; Emacs 自己启用了 scss-mode.定义在css-model.el中.
;;(unless (fboundp 'scss-mode)  (require-package 'scss-mode))
(setq-default scss-compile-at-save nil)

;;; Skewer CSS
(add-hook 'css-mode-hook 'skewer-css-mode)
;;; Use eldoc for syntax hint
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;;;----------------------------------------------
;;;Toml
;;;----------------------------------------------
;;Toml-mode 好像好久没开发了.
(require 'toml-mode)
(add-hook 'toml-mode-hook 'goto-address-prog-mode)

;;;----------------------------------------------
;;;Yaml
;;;----------------------------------------------
(require 'yaml-mode)
(add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
(add-hook 'yaml-mode-hook 'goto-address-prog-mode)

;;; Terraform
(require 'terraform-mode)
(require 'company-terraform)
(company-terraform-init)


(provide 'init-doc)
;;; init-doc ends here
