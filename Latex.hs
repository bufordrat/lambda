-- Matt's Typechecker: LaTeX helper functions and constants
-- (C) Matt Teichman, 2018

module Latex where

-- my modules
import LTerm

-- prelude for a simple LaTeX document
prelude :: String
prelude = "\\documentclass[10pt]{article}\n\n\\title{Sample Lambda Calculus Typesetting}\n\\author{}\n\\date{}\n\\thispagestyle{empty}\n\\pagestyle{empty}\n\\usepackage{linguex}\n\\usepackage{amssymb,amsmath}\n\\usepackage{stmaryrd}\n\\usepackage[left=2cm,top=1cm,bottom=2cm,right=2cm,nohead,nofoot]{geometry}\n\\setlength{\\parindent}{0in}\n\n\\begin{document}\n\\maketitle"

-- closing for a simple LaTeX document
closing = "\\end{document}"

-- LaTeX string representation of a type
latexType :: LType -> String
latexType E = "e"
latexType K = "k"
latexType S = "s"
latexType T = "t"
latexType (Arrow ty1 ty2) = "(" ++ (latexType ty1) ++ " \\-\\ " ++
                            " \\rightarrow " ++ " \\-\\ " ++ (latexType ty2) ++
                            ")"

-- LaTeX string representation of a lambda term
latexTerm :: LTerm -> [Char]
latexTerm (Vr string) = "\\mathit{" ++ string ++ "}"
latexTerm (App trm1 trm2) = "(" ++ (latexTerm trm1) ++ " \\-\\ " ++
                            (latexTerm trm2) ++ ")"
latexTerm (Lambda str typ trm) = "(\\lambda " ++ "\\mathit{" ++ str ++ "}" ++
                                 "_{" ++ (latexType typ) ++ "} \\-\\ . \\-\\ " ++
                                 (latexTerm trm) ++ ")"

-- LaTeX string representation of a typed expression
latexTE :: TypedExpression -> [Char]
latexTE (Typed trm typ) = (latexTerm trm) ++ " \\-\\ : \\-\\ " ++
                          (latexType typ)

