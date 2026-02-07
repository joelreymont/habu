(load "lib/stdlib.habu")
(prin1 (macroexpand-1 '(loop for type in '(short-float single-float double-float long-float)
                                  for c across "SFDL"
                                  when (subtypep 'short-float type)
                                  nconc (list c (char-downcase c)))))
(terpri)
