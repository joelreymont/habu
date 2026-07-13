\ maki/eval-tokest-test.f - the deterministic model-token estimator.
\
\ GEN-TOK-EST = alphanumeric runs + non-whitespace punctuation bytes; every
\ case is hand-counted so a classification drift fails loudly.

require lib/test.f
require maki/eval-tokest.f

T-RESET

s" est empty" T-LABEL         s" " EVAL:GEN-TOK-EST 0 T=
s" est ws only" T-LABEL       s"    " EVAL:GEN-TOK-EST 0 T=
s" est one word" T-LABEL      s" dup" EVAL:GEN-TOK-EST 1 T=
s" est alnum run" T-LABEL     s" f32" EVAL:GEN-TOK-EST 1 T=
s" est hyphen split" T-LABEL  s" GRID-CTX" EVAL:GEN-TOK-EST 3 T=
s" est ws separates" T-LABEL  s"   a   b  " EVAL:GEN-TOK-EST 2 T=
s" est typed arg" T-LABEL     s" span<space-global,f32,extent-n>" EVAL:GEN-TOK-EST 12 T=
s" est locals" T-LABEL        s" {: x y :}" EVAL:GEN-TOK-EST 6 T=
s" est kernel line" T-LABEL   s" K ( n -- n ) 1+" EVAL:GEN-TOK-EST 9 T=

T-REPORT
