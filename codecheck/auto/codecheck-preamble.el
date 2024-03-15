(TeX-add-style-hook
 "codecheck-preamble"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "font=Large" "labelfont=bf" "textfont=bf")))
   (TeX-run-style-hooks
    "mathpazo"
    "booktabs"
    "rotating"
    "makecell"
    "hyperref"
    "caption"
    "scalerel"
    "tikz")
   (TeX-add-symbols
    '("orcidicon" 1)
    "orcidlogo")
   (LaTeX-add-xcolor-definecolors
    "orcidlogocol")
   (LaTeX-add-caption-DeclareCaptions
    '("\\DeclareCaptionLabelFormat{addC}" "LabelFormat" "addC")))
 :latex)

