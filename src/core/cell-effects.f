\ cell-effects.f - effects for words needed before the checker starts.

s" CELL" s" -- n" TRUST

\ Public constants of files that load before the checker: without these rows a
\ body naming one is E-UNDEFINED on a from-source prefix boot.
PRIM: PATH-CAP PE-N PE-OUT PRIM;
PRIM: E-PATH-RANGE PE-N PE-OUT PRIM;
PRIM: E-PATH-NUL PE-N PE-OUT PRIM;
PRIM: SCOPE-FIND-AMBIGUOUS PE-N PE-OUT PRIM;
PRIM: E-XREF-NULL PE-N PE-OUT PRIM;

\ intern-table.f's words load before the checker; a row records no throw, so the
\ checker misses TEXT's and FIND's E-INTERN-REWIND.
PPRIM: INTERN CLEANUP PPRIM;
PPRIM: INTERN EACH-STRING PE-Q PE-N PE-QIN ;PE-Q PE-IN PPRIM;
PPRIM: INTERN TEXT PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-OUT PPRIM;
PPRIM: INTERN FIND PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-OUT PPRIM;
PPRIM: INTERN KEEP-BELOW PE-N PE-IN PPRIM;

\ The checker's control-effect flags and verdict word src/habu/verify-source.f
\ calls: without a row each is E-UNDEFINED on a from-source prefix boot.
PRIM: CTL-RENDERS PE-N PE-OUT PRIM;
PRIM: CTL-CREATES PE-N PE-OUT PRIM;
PRIM: CTL-NOMINAL PE-N PE-OUT PRIM;
PRIM: REPORT-UNCHECKABLE PE-N PE-IN PE-N PE-OUT PRIM;

\ The checker's error code the constructor tests read at top level.
PRIM: E-CTOR-PROTECTED PE-N PE-OUT PRIM;

s" SCRATCH-ALLOT" s" n --" TRUST
s" SCRATCH-VARIABLE" s" --" TRUST
s" PTR-VARIABLE" s" --" TRUST
s" PERSISTED-PTR-VARIABLE" s" --" TRUST
s" PTR-U8-TABLE" s" n --" TRUST
s" PERSISTED-PTR-U8-TABLE" s" n --" TRUST
s" PERSISTED-PTR-U8-TABLE-VARIABLE" s" --" TRUST
s" PTR-U8-TABLE-VARIABLE" s" --" TRUST
s" RESERVED-PTR-U8-CELL" s" n --" TRUST

\ The refusal count and the words that open and close a collection, from
\ src/core/checker.f, which loads before the checker starts. Checked bodies
\ count a recovered refusal (src/core/arena.f, src/habu/verify-source.f); tests
\ open and close a collection (test/native-multi-error-recovery.f).
PRIM: MULTI-ERR-N PE-PTR-N PE-OUT PRIM;
PRIM: MULTI-ERR-BEGIN PRIM;
PRIM: MULTI-ERR-END PE-N PE-OUT PRIM;

\ src/habu/verify-source.f counts its learned definers and its `parses:` rows
\ in these checker cells, which the rollback frame rewinds (src/core/checker.f),
\ and names them in checked bodies loaded after the hook.
PRIM: VERIFY-DEFINER-N PE-PTR-N PE-OUT PRIM;
PRIM: VERIFY-PARSES-N PE-PTR-N PE-OUT PRIM;

s" SIG-RAW-MODE" s" -- ptr n" TRUST

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

\ The checked call-window store. checker.f reads its call facts through its own
\ readers (the owner record's CALL-*-OFF rows); this package only installs it.
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
