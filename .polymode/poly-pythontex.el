(require 'polymode)
(defcustom  pm-inner/pythontex
  (pm-hbtchunkmode "pythontex"
                   :mode 'python-mode
                   :head-reg  ".begin.pycode.*"
                   :tail-reg  ".end.pycode."
                   :head-mode 'latex-mode
                   :tail-mode 'latex-mode
                   )
  "pycode typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/pythontex
  (pm-polymode-one "pythontex"
                   :hostmode 'pm-host/latex
                   :innermode 'pm-inner/pythontex)
  "Noweb typical polymode."
  :group 'polymodes
  :type 'object)
(define-polymode poly-pythontex-mode pm-poly/pythontex)
(provide 'poly-pythontex)
