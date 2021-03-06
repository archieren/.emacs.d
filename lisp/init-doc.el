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
;; Conflict with Web-Mode/Html-mode when the html is xhtml/html5.
;; (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
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
