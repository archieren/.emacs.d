
import yaml

f = open("icons.yml")
data = yaml.load(f)

fo = open("init-fontawesome-data.el", "w")

fo.write(";;; init-fontawesome-data --- Nothing\n")
fo.write(";;; Commentary:\n")
fo.write(";;; Code:\n")
fo.write("(defvar fontawesome-alist\n")
fo.write("  '(\n")

for key in data.keys():
    fo.write("    (\"%s\" . \"\\x%s\")\n" % (key, data[key]['unicode']))

fo.write("    ))\n")
fo.write("(provide 'init-fontawesome-data)\n")
fo.write(";;; init-fontawesome-data ends here\n")


fo.close()
