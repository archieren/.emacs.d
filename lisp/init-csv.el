;;; init-csv --- Nothing
;;; Commentary:
;;; Code:
(require 'csv-mode)
(require 'init-utils)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
;;; init-csv ends here
