\ operand-rows.f - the `parses:`, `parses-through:` and `names:` words.

package CHECK

global
\ `parses: W n` and `parses-through: W n ( E1 E2 )`, at top level after W's
\ definition, read as src/habu/verify-source.f PARSES-ROW reads them, checked
\ by the checker and kept in it by W's symbol and in W's facts text, for the
\ source pre-verifier, which never reads an engine-provided file. The keyword is the
\ last token the engine read when the word starts (TKA-CELL). UNSAFE-TOK? bars
\ both from checked bodies. They load after the hook so this checker records them
\ and INTRINSIC tags each (CTL-PARSES, INTRINSIC-CTL): a word checker.f defines is
\ recorded by the build's checker, which may know no such word.
TRUSTED: parses: ( -- )
   data-base TKA-CELL + @  data-base TKL-CELL + @
   parse-name parse-name NULL-PTR 0 RES-FALSE PARSES-KEEP ;
INTRINSIC-PARSES INTRINSIC

TRUSTED: parses-through: ( -- )
   data-base TKA-CELL + @  data-base TKL-CELL + @
   parse-name parse-name PARSES-LIST-LOAD PARSES-KEEP ;
INTRINSIC-PARSES-THROUGH INTRINSIC

\ `names: W`, at top level after W's definition: W looks its string operand up
\ as a name, so a string literal before a call of W is a use of the word it
\ names. Read as the source pre-verifier reads it (src/habu/verify-source.f
\ NAMES-ROW), checked by the checker, which marks W's symbol,
\ barred from checked bodies and
\ tagged as the two above are.
TRUSTED: names: ( -- )
   data-base TKA-CELL + @  data-base TKL-CELL + @
   parse-name CHECKER-NAMES-ROW ;
INTRINSIC-NAMES INTRINSIC
;package
