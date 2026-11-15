\ cell-effects.f - the `parses:`, `parses-through:` and `names:` words and the
\ checked call-window store.

\ `parses: W n` and `parses-through: W n ( E1 E2 )`, at top level after W's
\ definition, read as src/habu/verify-source.f PARSES-ROW reads them and checked
\ by the checker; nothing is kept. The keyword is the
\ last token the engine read when the word starts (TKA-CELL). UNSAFE-TOK? bars
\ both from checked bodies. They load after the hook so this checker records them
\ and INTRINSIC tags each (CTL-PARSES, INTRINSIC-CTL): a word checker.f defines is
\ recorded by the build's checker, which may know no such word.
TRUSTED: parses: ( -- )
   data-base TKA-CELL + @  data-base TKL-CELL + @
   parse-name parse-name RES-FALSE CHECKER-PARSES-ROW drop drop drop ;
INTRINSIC-PARSES INTRINSIC

TRUSTED: parses-through: ( -- )
   data-base TKA-CELL + @  data-base TKL-CELL + @
   parse-name parse-name PARSES-LIST-LOAD CHECKER-PARSES-ROW drop drop drop ;
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

\ The checked call-window store, which checker.f reads through CWIN-STATE;
\ this package only installs it.
package CHECKER-CALLS

PTR-VARIABLE STATE  variable STATE-N  variable STATE-CAP

\ The owner can record calls before this checked header is loaded. Transfer
\ that allocation with its rows, then clear the retired header so capture has
\ only one live store to prepare. Reinstalling the current header is harmless.
: INSTALL ( -- )
   CWIN-STATE {: prior:ptr prior-n:ptr prior-cap:ptr :}
   prior STATE = if exit then
   prior @ STATE !
   prior-n @ STATE-N !
   prior-cap @ STATE-CAP !
   NULL-PTR prior !
   0 prior-n !
   0 prior-cap !
   [: STATE STATE-N STATE-CAP ;] is CWIN-STATE ;
INSTALL

seal-package
;package
