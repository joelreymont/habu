\ aot-startup-reach-lint.f - an emitter may only ADR, a label it binds itself.
\ `adr` reaches ±1 MiB (src/arch/arm64/icode.f ?ADR) and the stripped startup's
\ labels bind after the copied code band, whose size is the application's; any
\ other label goes through src/habu/aot-link-arm.f TEXT-ADR,. The triple shape
\ is in tools/lint/label-triple.f, shared with tools/aot-section-reach-lint.f.
\ Run: bin/hb --load tools/aot-startup-reach-lint.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require tools/lint/label-triple.f

package AOT-STARTUP-REACH-LINT

\ The lint's own findings code, in the unclaimed gap the sibling lints use.
-4822 constant E-AOT-STARTUP-REACH-LEX

create AS-SLAB LINT-SLAB:CELLS cells allot

variable BAD  variable LI  variable DEND

\ The largest scanned definition binds eight labels; overflow is a named
\ refusal rather than a spurious finding.
: CAP-FAIL ( -- )
   s" aot-startup-reach-lint: more labels in one definition than LMAX" 1 die ;

: ADD-LABEL ( ptr u8 n -- )
   LINT-LABEL-TRIPLE:ADD-LABEL LINT-NOT IF CAP-FAIL THEN ;

\ ---- definition boundaries ---------------------------------------------------
\ `:` and `;` are whole WORD tokens, so prose cannot forge them, and `;package`
\ is another word.
: DEF-OPEN? ( n -- bool ) {: k:n :}
   k 1+ LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k s" :" LINT-LABEL-TRIPLE:LEX-WORD= LINT-NOT IF LINT-FALSE exit THEN
   k 1+ LINT-LABEL-TRIPLE:WORD? ;

: DEF-END ( n -- n ) {: k:n :}
   k 1+ begin dup LINT-LEX:COUNT < while
      dup s" ;" LINT-LABEL-TRIPLE:LEX-WORD= IF exit THEN
      1+
   repeat ;

\ ---- the two passes, run per definition --------------------------------------
\ A `<X> LABEL@ ADR,` triple is a finding unless X's tail is LTEXT (text offset
\ zero, behind every startup site) or a `<X> LABEL@ LBL,` triple binds X in the
\ same definition. A site outside any definition is a finding.
\ Unseen: a label passed as a value (crash.f C-CRASH-GUARD-REPORT `msg:label`)
\ carries no LABEL@; its caller C-CRASH-STACK-GUARDS binds it in its own body.
: COLLECT-RANGE ( n n -- ) {: s:n e:n :}
   e s ?do
      i s" LBL," LINT-LABEL-TRIPLE:TRIPLE? IF
         i LINT-LEX:TOKEN LINT-LABEL-TRIPLE:TAIL ADD-LABEL
      THEN
   loop ;

: REPORT ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   s" AOT-STARTUP-REACH " type pa pu type s" :" type k LINT-LEX:LINE@ LINT-MAIN-N$ type
   s" : `" type k LINT-LEX:TOKEN type
   s" ` is bound elsewhere: use TEXT-ADR,, not ADR," type cr
   BAD @ 1+ BAD ! ;

: CHECK-TOKEN ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   k s" ADR," LINT-LABEL-TRIPLE:TRIPLE? LINT-NOT IF exit THEN
   k LINT-LEX:TOKEN LINT-LABEL-TRIPLE:TAIL s" LTEXT" LINT-STR=CI IF exit THEN
   k LINT-LEX:TOKEN LINT-LABEL-TRIPLE:TAIL LINT-LABEL-TRIPLE:LABEL? IF exit THEN
   pa pu k REPORT ;

: CHECK-RANGE ( ptr u8 n n n -- ) {: pa:ptr pu:n s:n e:n :}
   e s ?do  pa pu i CHECK-TOKEN  loop ;

\ A lexer diagnostic hides every later token, the ADR, included: fail closed and
\ name the file.
: LEX-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" aot-startup-reach-lint: source did not lex: " type pa pu type
   s"  (line " type LINT-LEX:ERROR-LINE@ LINT-MAIN-N$ type s" )" type cr
   E-AOT-STARTUP-REACH-LEX throw ;

public

: SCAN-SOURCE ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF pa pu LEX-FAIL THEN
   0 LI !
   begin LI @ LINT-LEX:COUNT < while
      LI @ DEF-OPEN? IF
         LI @ DEF-END DEND !
         LINT-LABEL-TRIPLE:RESET
         LI @ DEND @ COLLECT-RANGE
         pa pu LI @ DEND @ CHECK-RANGE
         DEND @ LI !
      ELSE
         LINT-LABEL-TRIPLE:RESET
         pa pu LI @ CHECK-TOKEN
      THEN
      LI @ 1+ LI !
   repeat ;

: SCAN-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu AS-SLAB LINT-SLAB:LOAD
   pa pu  AS-SLAB LINT-SLAB:TEXT  SCAN-SOURCE ;

: FINDINGS ( -- n ) BAD @ ;
: FINDINGS-RESET ( -- ) 0 BAD ! ;

private

\ src/habu/habu2.f is not scanned: it emits the engine's startup, where every
\ label is baked beside it, and tools/aot-section-reach-lint.f holds it to the
\ payload-section rule.
: MAIN ( -- )
   FINDINGS-RESET
   s" src/habu/aot-lib.f" SCAN-FILE
   s" src/habu/crash.f" SCAN-FILE
   s" src/habu/rt.f" SCAN-FILE
   BAD @ 0 > IF
      s" aot-startup-reach-lint: " type BAD @ LINT-MAIN-N$ type s"  finding(s)" type cr
      s" aot-startup-reach-lint: ADR, to a label the emitter does not bind" 1 die
   THEN
   s" aot-startup-reach-lint: clean" type cr ;
MAIN

;package
