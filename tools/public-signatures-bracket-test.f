\ public-signatures-bracket-test.f - the manifest's load-time bracket: rows stamped
\ with one file's path speak only for families THIS load declared, above
\ PS:FAM-BASE, so the first family below the mark that PS:PS-ENUM-PUBLIC? or
\ PS:PS-DRV-PUBLIC? would publish must not be named. Seeded product: FAM-BASE 119;
\ unfiltered, 13 definitions became 386. An unseeded host has none below the mark
\ and says so. Run: bin/hb --load tools/public-signatures-bracket-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/public-signatures-core.f

package PS-BRACKET
private

$40000 constant OUT-CAP                 \ larger than an unfiltered seeded manifest, so
create OUT OUT-CAP allot                \ the provenance assert answers before any capacity
create ERR OUT-CAP allot                \ refusal can answer for it
variable FOUND

\ The subject: a file this process does NOT load. Its own words are all this
\ manifest may carry.
: SUBJECT$ ( -- ptr u8 n )
   s" src/examples/llm/good.f" ;

: SCAN ( -- )
   OUT OUT-CAP PS:PS-OUT-BUFFER!
   ERR OUT-CAP PS:PS-ERR-BUFFER!
   0 PS:PS-TRUST !
   PS:PS-JSON-DOC-START
   SUBJECT$ PS:PS-SCAN-FILE
   PS:PS-JSON-DOC-END ;

: OUT$ ( -- ptr u8 n )
   OUT PS:PS-OUT$ nip ;

\ Would the walk publish anything for this family? Asked of the tool, so a change
\ to what a manifest may contain moves this case with it.
: PUBLISHABLE? ( n -- bool ) {: fam:n :}
   fam PS:PS-ENUM-PUBLIC? fam PS:PS-DRV-PUBLIC? or ;

: BELOW-BASE-FAM ( -- n )               \ first publishable family below the mark, or -1
   -1 FOUND !
   PS:FAM-BASE 0 ?do
      FOUND @ 0 < if
         i PUBLISHABLE? if i FOUND ! then
      then
   loop
   FOUND @ ;

: RUN-CASE ( -- )
   SCAN
   PS:PS-ERR$ nip 0 T=
   OUT$ nip 0 > T-ASSERT

   s" the subject's own words are in the manifest" T-LABEL
   OUT$ s\" \"word\":\"SQUARE\"" CONTAINS? T-ASSERT

   BELOW-BASE-FAM {: fam:n :}
   fam 0 < if
      s" this engine holds no publishable family below the mark" T-LABEL
      PS:FAM-BASE 0 >= T-ASSERT
      PS:PS-BUFFERS-OFF
      exit
   then

   s" a family the engine arrived with is not named in a scanned file's manifest"
   T-LABEL   OUT$ fam TFAM-NAME$ CONTAINS? 0= T-ASSERT
   PS:PS-BUFFERS-OFF ;

public

: RUN ( -- )
   T-RESET
   RUN-CASE
   T-REPORT
   s" public-signatures-bracket-test: ok" type cr ;

;package

PS-BRACKET:RUN
