;;; init-csv --- Nothing
;;; Commentary:
;;; Code:
(require-package 'csv-mode)

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
;;; init-csv ends here
