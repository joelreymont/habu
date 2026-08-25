\ ir-id-manifest.f - focused test for the frozen compiler identity schema.
\
\ It proves four things about `test/compiler/ir-id-schema.f` and the production
\ identity authority `src/compiler/ir/id.f` it describes:
\
\   - the canonical bytes and their SHA-256 digest are deterministic, and the
\     digest still equals the committed frozen value;
\   - the thirteen identity families are present once each, in declaration
\     order, with the names the schema froze;
\   - every numeric vector row, valid and hostile, produces exactly the outcome
\     the schema recorded when it is run through the public `IR-ID` words, and
\     the scalar families keep the whole value they were handed even when that
\     value is shaped like a packed identity;
\   - guard coverage matches the manifest: every guard the manifest marks as
\     vector-reachable is actually reached by a row, and every guard it marks as
\     unreachable is never reached.
\
\ Wrong-family rejection and require replay are deliberately not tested here.
\ They belong to `test/compiler/ir-id.f`, and the schema only names them.

require lib/test.f
require lib/fs.f
require test/compiler/ir-id-schema.f

package IR-ID-MANIFEST-TEST
using COMPILER-ID-PROOF
private

13 constant FROZEN-FAMILIES

create SNAP BYTES-CAP allot
create HEX-SNAP DIGEST-HEX-LEN allot

variable SNAP-U
variable FAM-I
variable FAM-J
variable GUARD-I

\ ---- rebuild determinism -----------------------------------------------------

: SNAP-TAKE ( -- )
   BYTES$ {: a:ptr u:n :}
   u BYTES-CAP > if E-STR-CAPACITY throw then
   a SNAP u BYTE-COPY
   u SNAP-U ! ;

: SNAP$ ( -- ptr u8 n )
   SNAP SNAP-U @ ;

: HEX-SNAP-TAKE ( -- )
   DIGEST-HEX$ {: a:ptr u:n :}
   u DIGEST-HEX-LEN <> if E-STR-CAPACITY throw then
   a HEX-SNAP u BYTE-COPY ;

: HEX-SNAP$ ( -- ptr u8 n )
   HEX-SNAP DIGEST-HEX-LEN ;

: DETERMINISM ( -- )
   BUILD
   SNAP-TAKE
   HEX-SNAP-TAKE
   BUILD
   s" canonical bytes are identical on a rebuild" T-LABEL
   BYTES$ SNAP$ T$=
   s" digest is identical on a rebuild" T-LABEL
   DIGEST-HEX$ HEX-SNAP$ T$=
   s" digest equals the committed freeze" T-LABEL
   DIGEST-HEX$ EXPECTED-DIGEST$ T$= ;

\ ---- family coverage ---------------------------------------------------------

: FAMILY-TAIL-DISTINCT ( n -- ) {: idx:n :}
   idx 1+ FAM-J !
   begin FAM-J @ FAMILY-COUNT < while
      s" identity family names are distinct" T-LABEL
      idx FAMILY-NAME-AT FAM-J @ FAMILY-NAME-AT T-STR= TFALSE
      FAM-J @ 1+ FAM-J !
   repeat ;

: FAMILY-COVERAGE ( -- )
   s" the design freezes thirteen identity families" T-LABEL
   FAMILY-COUNT FROZEN-FAMILIES T=
   0 FAM-I !
   begin FAM-I @ FAMILY-COUNT < while
      s" identity family name is not empty" T-LABEL
      FAM-I @ FAMILY-NAME-AT nip 0 > TTRUE
      FAM-I @ FAMILY-TAIL-DISTINCT
      FAM-I @ 1+ FAM-I !
   repeat ;

\ ---- vector rows through the production surface ------------------------------

: PACK-ROW-RUN ( n -- ) {: idx:n :}
   idx PACK-LOCAL {: local:n :}
   idx PACK-CLASS {: class:n :}
   s" pack vector reaches its recorded outcome" T-LABEL
   local PACK-OUTCOME class T=
   class 0 <> if exit then
   s" accepted local index round-trips out of the packed identity" T-LABEL
   local PACK-LOCAL-BACK local T=
   s" accepted packed identity keeps its minting module" T-LABEL
   local PACK-OWNER-KEPT? TTRUE ;

: PACK-VECTOR-RUN ( -- )
   PACK-ROWS 0 ?do i PACK-ROW-RUN loop ;

: CHECK-ROW-RUN ( n -- ) {: idx:n :}
   s" check vector reaches its recorded outcome" T-LABEL
   idx CHECK-LOCAL idx CHECK-BOUND idx CHECK-OWNER CHECK-OUTCOME
      idx CHECK-CLASS T= ;

: CHECK-VECTOR-RUN ( -- )
   CHECK-ROWS 0 ?do i CHECK-ROW-RUN loop ;

\ An accepted scalar must come back whole. The high-bit rows are the ones that
\ tell a plain number apart from a packed identity: if either scalar family had
\ masked, shifted, or picked up a module serial, the value would come back as
\ one of the two projections instead of itself.
: NO-PROJECTION ( n n -- ) {: value:n back:n :}
   s" scalar is not the local projection of itself" T-LABEL
   back value LOCAL-MASK and T<>
   s" scalar is not the owner projection of itself" T-LABEL
   back value LOCAL-SHIFT rshift T<> ;

: SCALAR-IDENTITY ( n -- ) {: value:n :}
   s" count keeps the whole value" T-LABEL
   value COUNT-BACK value T=
   s" pool offset keeps the whole value" T-LABEL
   value POOL-BACK value T=
   s" count picks up no module serial" T-LABEL
   value COUNT-BACK LOCAL-SHIFT rshift  value LOCAL-SHIFT rshift T=
   s" pool offset picks up no module serial" T-LABEL
   value POOL-BACK LOCAL-SHIFT rshift  value LOCAL-SHIFT rshift T=
   value LOCAL-SHIFT rshift 0= if exit then
   value value COUNT-BACK NO-PROJECTION
   value value POOL-BACK NO-PROJECTION ;

: SCALAR-ROW-RUN ( n -- ) {: idx:n :}
   idx SCALAR-VALUE {: value:n :}
   idx SCALAR-CLASS {: class:n :}
   s" count vector reaches its recorded outcome" T-LABEL
   value COUNT-OUTCOME class T=
   s" pool-offset vector reaches its recorded outcome" T-LABEL
   value POOL-OUTCOME class T=
   class 0 <> if exit then
   value SCALAR-IDENTITY ;

: SCALAR-VECTOR-RUN ( -- )
   SCALAR-ROWS 0 ?do i SCALAR-ROW-RUN loop ;

: HIGH-SCALAR-ROWS ( -- n )
   0 SCALAR-ROWS 0 ?do
      i SCALAR-CLASS 0=
      i SCALAR-VALUE LOCAL-SHIFT rshift 0 <> and if 1+ then
   loop ;

: SCALAR-DISCRIMINATION ( -- )
   s" accepted scalar rows carry bits above the local field" T-LABEL
   HIGH-SCALAR-ROWS 0 > TTRUE ;

\ ---- guard coverage ----------------------------------------------------------

: PACK-CODE-HITS ( n -- n ) {: code:n :}
   0 PACK-ROWS 0 ?do
      i PACK-LOCAL PACK-OUTCOME code = if 1+ then
   loop ;

: CHECK-CODE-HITS ( n -- n ) {: code:n :}
   0 CHECK-ROWS 0 ?do
      i CHECK-LOCAL i CHECK-BOUND i CHECK-OWNER CHECK-OUTCOME code = if 1+ then
   loop ;

: SCALAR-CODE-HITS ( n -- n ) {: code:n :}
   0 SCALAR-ROWS 0 ?do
      i SCALAR-VALUE COUNT-OUTCOME code = if 1+ then
      i SCALAR-VALUE POOL-OUTCOME code = if 1+ then
   loop ;

: CODE-HITS ( n -- n ) {: code:n :}
   code PACK-CODE-HITS
   code CHECK-CODE-HITS +
   code SCALAR-CODE-HITS + ;

: GUARD-ROW-COVERAGE ( n -- ) {: idx:n :}
   idx GUARD-CODE-AT CODE-HITS {: hits:n :}
   idx GUARD-BY-VECTOR? if
      s" guard the manifest marks reachable is exercised by a row" T-LABEL
      hits 0 > TTRUE
      exit
   then
   s" guard the manifest marks unreachable is never reached" T-LABEL
   hits 0 T= ;

: GUARD-COVERAGE ( -- )
   0 GUARD-I !
   begin GUARD-I @ GUARD-COUNT < while
      GUARD-I @ GUARD-ROW-COVERAGE
      GUARD-I @ 1+ GUARD-I !
   repeat ;

\ ---- fixture references ------------------------------------------------------

: FIXTURE-COVERAGE ( -- )
   FIXTURE-COUNT 0 ?do
      s" referenced separate fixture file exists" T-LABEL
      i FIXTURE-PATH$ FILE? TTRUE
   loop ;

public

: RUN ( -- )
   T-RESET
   DETERMINISM
   FAMILY-COVERAGE
   PACK-VECTOR-RUN
   CHECK-VECTOR-RUN
   SCALAR-VECTOR-RUN
   SCALAR-DISCRIMINATION
   GUARD-COVERAGE
   FIXTURE-COVERAGE
   T-REPORT ;

;using
;package

IR-ID-MANIFEST-TEST:RUN
