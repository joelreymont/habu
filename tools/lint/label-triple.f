\ label-triple.f — package LINT-LABEL-TRIPLE: the label table and the
\ `NAME LABEL@ <closer>` shape that the two AOT reach lints read off the source.
\ tools/aot-section-reach-lint.f and tools/aot-startup-reach-lint.f each keep
\ their own model — membership of the baked payload section there, binding
\ inside the same definition here — their report lines and their entry. What
\ they share is exactly what three adjacent tokens say, and it is stated once,
\ here: a fix to the shape or to the qualified-name tail is made in one place.
\
\ THE THREE-TOKEN SHAPE. A binding and a reference are both `NAME LABEL@
\ <closer>`, closed by `LBL,` where a label is bound and by `ADR,` where one is
\ reached. TRIPLE? pins all three roles, so the same three tokens in any other
\ order are neither, and `TEXT-ADR,` / `TADR,` are not `ADR,`. The scan runs on
\ the shared string-aware lexer (tools/lint/source-lex.f), so the same tokens
\ inside a comment or inside a string literal are not a triple at all; each
\ lint's test pins those cases.
\
\ THE TAIL RULE. TAIL is the bytes after the last INTERIOR colon of a token. A
\ token that starts or ends with ':' is an ordinary word (docs/forth.md
\ § Naming), and the bare `:` definer must not read as an empty tail, so both
\ keep the whole token. A site may spell a label bare inside its own package
\ (`LROWS`) or qualified from outside (`AOT-XTSITE:LROWS`, `BP-CALLER:LBPLH`);
\ comparing tails is what makes those one name.
\
\ MEASURED UNREACHABLE, KEPT ON PURPOSE. Dropping WORD?'s kind test changes no
\ verdict in any fixture of either lint, and it cannot: the lexer never turns the
\ inside of a comment or a string into tokens at all, and the tokens it does emit
\ for them carry their own delimiters in the span — a paren comment's token text
\ is `( ... )` and a primitive-axiom row's is the whole row — so none of them can
\ equal a bare label name. What keeps the hidden-text fixtures honest is
\ therefore the lexer, not this line. It stays because a lint's business is to
\ fail closed on a shape it did not expect, and the set of token kinds is
\ source-lex.f's to change, not this file's to assume.
\
\ Run: the lints; this file defines no entry of its own.

require tools/lint/text.f
require tools/lint/source-lex.f

package LINT-LABEL-TRIPLE

\ One table serves both lints: a scan empties it with RESET, fills it with
\ ADD-LABEL and reads it back with LABEL? inside the one call, so two lints
\ loaded into the same image never interleave on it.
\
\ The section lint binds 33 names today and the largest definition the startup
\ lint scans binds eight; the cap is generous and its overflow is a refusal by
\ the caller rather than a silent truncation, because a dropped name changes a
\ verdict either way — a member the section lint stops checking, a clean site
\ the startup lint turns into a finding.
$40 constant LMAX
$400 constant NAMES-CAP
create NAMES NAMES-CAP allot   variable NEND
create NOFF LMAX cells allot   create NLEN LMAX cells allot   variable N#

public

: RESET ( -- )
   0 N# !  0 NEND ! ;

\ LINT-FALSE when the name does not fit — the row cap or the byte pool is full.
\ The caller refuses in its own words: each lint names itself and the limit its
\ own model hits, and one message stored here would be the wrong one for
\ whichever lint was loaded second.
: ADD-LABEL ( ptr u8 n -- bool ) {: a:ptr u:n :}
   N# @ LMAX >=  NEND @ u + NAMES-CAP >  or IF LINT-FALSE exit THEN
   a  NAMES NEND @ +  u LINT-BMOVE
   NEND @ NOFF N# @ cells + !   u NLEN N# @ cells + !
   NEND @ u + NEND !   N# @ 1+ N# !
   LINT-TRUE ;

: LABELS ( -- n ) N# @ ;

: LABEL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup N# @ < while
      dup cells NOFF + @ NAMES +  over cells NLEN + @  a u LINT-STR=CI
      IF drop LINT-TRUE exit THEN
      1+
   repeat drop LINT-FALSE ;

: TAIL ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 2 < IF a u exit THEN
   u 1- begin dup 1 > while
      dup a + c@ 58 = IF  dup 1+ a +  u rot -  1-  exit THEN
      1-
   repeat drop a u ;

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: LEX-WORD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? LINT-NOT IF LINT-FALSE exit THEN
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ Token k is a WORD and k+1 / k+2 are `LABEL@` and the closer given.
: TRIPLE? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k 2 + LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k WORD? LINT-NOT IF LINT-FALSE exit THEN
   k 1+ s" LABEL@" LEX-WORD= LINT-NOT IF LINT-FALSE exit THEN
   k 2 + a u LEX-WORD= ;

;package
