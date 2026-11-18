split-str
\ repl.f - the REPL's reader (src/habu/interpreter.f): REPL-READ, which defaults
\ to the terminal line editor (src/lib/line-editor.f) and genio may replace, so
\ the REPL calls it again before each line.
require src/lib/line-editor.f

defer REPL-READ ( -- str )

: INSTALL ( -- )
   [: LINE-EDITOR:READ-LINE ;] is REPL-READ ;
INSTALL
