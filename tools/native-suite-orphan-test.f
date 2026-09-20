\ native-suite-orphan-test.f - keep compiler test coverage closed.
\
\ Every test/compiler source that is a suite must occur in the gate registry.
\ The compiler directory also contains source helpers (proof schemas, generated
\ cases, fixture subjects and the Rocq runner); those are structural children of
\ their registered parent and are the only allowlisted exceptions. A new file
\ outside that small child set fails by its path, so an orphan cannot be added
\ silently.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/test.f

package NATIVE-SUITE-ORPHAN

$40000 constant GNO-REG-CAP
create GNO-REG GNO-REG-CAP allot
variable GNO-REG-U
variable GNO-SEEN

: GNO-REG$ ( -- ptr u8 n ) GNO-REG GNO-REG-U @ ;

: GNO-ALLOW? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -cases.f" ENDS-WITH? if true exit then
   a u s" -obligations.f" ENDS-WITH? if true exit then
   a u s" -schema.f" ENDS-WITH? if true exit then
   a u s" -subject.f" ENDS-WITH? if true exit then
   a u s" -fixture.f" ENDS-WITH? if true exit then
   a u s" -replay.f" ENDS-WITH? if true exit then
   a u s" -concurrency.f" ENDS-WITH? if true exit then
   a u s" proof-manifest.f" ENDS-WITH? if true exit then
   a u s" rocq-run.f" ENDS-WITH? if true exit then
   a u s" reloc-vm.f" ENDS-WITH? if true exit then
   a u s" native-trap-noret.f" ENDS-WITH? if true exit then
   a u s" native-prefix-rollback.f" ENDS-WITH? if true exit then
   a u s" native-match-forge.f" ENDS-WITH? if true exit then
   a u s" ir-id-source.f" ENDS-WITH? if true exit then
   a u s" native-checker-storage.f" ENDS-WITH? if true exit then
   a u s" native-match-layout.f" ENDS-WITH? if true exit then
   false ;

: GNO-REGISTERED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   \ Registry entries are indented three spaces and end at the line break; search
   \ that row shape rather than accepting a mention in a comment.
   SB-RESET
   s"    " SB-APPEND
   a u SB-APPEND
   10 SB-APPEND-C
   GNO-REG$ SB$ CONTAINS? ;

: GNO-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" .f" ENDS-WITH? 0= if exit then
   GNO-SEEN @ 1+ GNO-SEEN !
   a u GNO-ALLOW? if exit then
   a u T-LABEL
   a u GNO-REGISTERED? TTRUE ;

: GNO-REGISTRY ( -- )
   s" test/gate-stdlib-cases.f" FILE-SIZE dup GNO-REG-CAP > if
      drop E-TBL-BOUNDS throw
   then
   drop
   s" test/gate-stdlib-cases.f" GNO-REG GNO-REG-CAP READ-ALL GNO-REG-U ! ;

public
: RUN ( -- )
   T-RESET
   0 GNO-SEEN !
   GNO-REGISTRY
   s" test/compiler" [: GNO-CHECK ;] WALK-FILES
   s" native-colon is covered by the orphan sweep" T-LABEL
   s" test/compiler/native-colon.f" GNO-REGISTERED? TTRUE
   s" compiler sources were discovered" T-LABEL
   GNO-SEEN @ 100 > TTRUE
   T-REPORT ;

;package

NATIVE-SUITE-ORPHAN:RUN
