\ label-triple.f — package LINT-LABEL-TRIPLE: the label table and the
\ `NAME LABEL@ <closer>` shape that tools/aot-section-reach-lint.f and
\ tools/aot-startup-reach-lint.f both read off the source; each keeps its own
\ model, report lines and entry. A binding closes with `LBL,`, a reference with
\ `ADR,`. Tokens come from src/lib/source-lex.f, so a triple inside a comment or
\ a string is none; each lint's test pins those cases. No entry of its own.

require tools/lint/text.f
require src/lib/source-lex.f

package LINT-LABEL-TRIPLE

\ A scan empties the table with RESET, fills it and reads it back inside one
\ call, so two lints in one image never interleave on it. The section lint binds
\ 33 names and the largest startup definition eight; overflow is the caller's
\ refusal, never a truncation: a dropped name changes a verdict either way.
$40 constant LMAX
$400 constant NAMES-CAP
create NAMES NAMES-CAP allot   variable NEND
create NOFF LMAX cells allot   create NLEN LMAX cells allot   variable N#

public

: RESET ( -- )
   0 N# !  0 NEND ! ;

\ LINT-FALSE when the name does not fit (row cap or byte pool full). The caller
\ refuses in its own words: a message stored here would name the wrong lint.
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

\ The bytes after the last interior colon, so `LROWS` and `AOT-XTSITE:LROWS`
\ are one name. A token that starts or ends with ':' is an ordinary word
\ (docs/forth.md § Naming), and the bare `:` must not read as an empty tail:
\ both keep the whole token.
: TAIL ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 2 < IF a u exit THEN
   u 1- begin dup 1 > while
      dup a + c@ 58 = IF  dup 1+ a +  u rot -  1-  exit THEN
      1-
   repeat drop a u ;

\ No fixture needs this kind test: the lexer's comment and string tokens carry
\ their delimiters and can never equal a bare label. It stays to fail closed on
\ a token kind source-lex.f may add.
: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: LEX-WORD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? LINT-NOT IF LINT-FALSE exit THEN
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ Token k is a WORD and k+1 / k+2 are `LABEL@` and the closer given, in that
\ order only; `TEXT-ADR,` and `TADR,` are not `ADR,`.
: TRIPLE? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k 2 + LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k WORD? LINT-NOT IF LINT-FALSE exit THEN
   k 1+ s" LABEL@" LEX-WORD= LINT-NOT IF LINT-FALSE exit THEN
   k 2 + a u LEX-WORD= ;

;package
