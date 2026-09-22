\ aot-startup-reach-lint.f — an emitter may only ADR, a label it binds itself.
\
\ WHY A LINT AND NOT A MEASUREMENT. `adr` carries a signed 21-bit byte delta
\ (src/arch/arm64/icode.f ?ADR): ±1 MiB. The fixed startup src/habu/aot-lib.f
\ EMIT-ENTRY emits sits at text offset zero, and the labels it has to address —
\ the entry word, the crash handler, the signal stub, the sparse data blob — bind
\ after the whole copied code band, whose size is the application's. Every image
\ anyone builds by hand is far under 1 MiB, so an `ADR,` there works until the
\ program that matters: Tender's standalone closure is 1.36 MB of code and its
\ build died `icode: adr out of reach site=844 target=1363312`, at scale only and
\ far from the line that caused it. So the rule is about the SHAPE and not about
\ any measured distance: an emitter reaches a label it did not bind itself
\ through src/habu/aot-lib.f TEXT-ADR,, whatever that label's offset happens to
\ be today. This is the sibling of tools/aot-section-reach-lint.f, which keeps
\ the same rule for the baked AOT payload section.
\
\ THE MODEL. A `<X> LABEL@ ADR,` triple is a finding unless
\   - X's tail is LTEXT, the label at text offset zero that TEXT-ADR, itself
\     reaches — it is behind every startup site, so an ADR to it is in reach by
\     construction for any startup under 1 MiB; or
\   - a `<X> LABEL@ LBL,` triple occurs inside the SAME definition, before or
\     after the site. A definition's body is bounded — the emitted run is the one
\     the word writes — so a label bound there is at most that body away, and
\     every such pair in the scanned files is a message string or a local jump
\     target a few dozen bytes off.
\ A site outside any definition has no such binding and is a finding: the lint
\ fails closed on a shape it was not taught.
\
\ WHAT THE MODEL DOES NOT SEE. A label passed to an emitter as a value —
\ src/habu/crash.f C-CRASH-GUARD-REPORT takes `msg:label` and writes
\ `1 msg ADR,` — carries no `LABEL@` and is no triple. That is acceptable here
\ because such a label is bound by the caller inside its own emitted run
\ (C-CRASH-STACK-GUARDS binds all three message labels in its own body, a few
\ instructions from the reads), and because the alternative — following a value
\ through a call — is a dataflow the lexer cannot do. The rule this lint enforces
\ is the one that can be read off three adjacent tokens.
\
\ Structure, not text: the three-token shape, the qualified-name tail rule and
\ the label table live in tools/lint/label-triple.f, shared with the sibling
\ section lint and stated there. The scan runs on the shared string-aware lexer
\ (tools/lint/source-lex.f), so the same three tokens inside a comment, inside a
\ string literal, or in the wrong order are not a binding and not a reference.
\ tools/aot-startup-reach-lint-test.f pins each of those.
\
\ Run: bin/hb --load tools/aot-startup-reach-lint.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/label-triple.f

package AOT-STARTUP-REACH-LINT

\ The lint's own findings code, in the unclaimed gap the sibling lints use.
-4822 constant E-AOT-STARTUP-REACH-LEX

create AS-SLAB LINT-SLAB:CELLS cells allot

variable BAD  variable LI  variable DEND

\ The largest definition in the scanned files binds eight labels, well inside
\ the shared table's cap. Its overflow is this named refusal rather than a
\ silent truncation, because a dropped binding turns a clean site into a finding
\ and the reverse can never happen.
: CAP-FAIL ( -- )
   s" aot-startup-reach-lint: more labels in one definition than LMAX" 1 die ;

: ADD-LABEL ( ptr u8 n -- )
   LINT-LABEL-TRIPLE:ADD-LABEL LINT-NOT IF CAP-FAIL THEN ;

\ ---- definition boundaries ---------------------------------------------------
\ A definition opens on the `:` definer followed by a name and closes on the next
\ bare `;`. Both are whole WORD tokens, so neither can be forged from prose, and
\ `;package` / `;using` / `;SUITE` are other words entirely.
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

\ A lexer diagnostic means the scan stopped early and every later token is
\ invisible - including, possibly, the ADR, this lint exists to find. Fail closed
\ and name the file rather than report a clean scan of half of it.
: LEX-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" aot-startup-reach-lint: source did not lex: " type pa pu type
   s"  (line " type LINT-LEX:ERROR-LINE@ LINT-MAIN-N$ type s" )" type cr
   E-AOT-STARTUP-REACH-LEX throw ;

public

\ Scan one already-loaded source. This is the whole lint; the file entry below
\ and every fixture in the test go through it.
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
         LINT-LABEL-TRIPLE:RESET                 \ no enclosing definition, no bindings
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

\ The three files that emit a stripped image's fixed startup and the code it
\ installs. src/habu/habu2.f is not here: it emits the ENGINE's startup, where
\ every label is baked beside it, and tools/aot-section-reach-lint.f already
\ holds it to the payload-section rule.
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
