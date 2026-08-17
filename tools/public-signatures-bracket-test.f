\ public-signatures-bracket-test.f - the manifest's load-time bracket.
\
\ THE CLAIM UNDER TEST is provenance, not arithmetic. A manifest names one file
\ and every row in it is stamped with that file's path, so the synthesized
\ registry rows may only speak for families THIS LOAD declared. PS:FAM-BASE is
\ the mark that separates them from the ones the engine arrived holding.
\
\ WHY THIS IS NOT A RESTATEMENT. Asserting that the manifest holds exactly the
\ rows between FAM-BASE and TFAM-N@ would repeat the walk's own bounds and could
\ not fail. What is asserted instead is the consequence: a family the engine
\ came with must not be named in the manifest of a file that never mentioned it.
\ The witness is FOUND rather than spelled - the first family below the mark that
\ the tool's own PS:PS-ENUM-PUBLIC? / PS:PS-DRV-PUBLIC? say it would publish -
\ so the case asks the walk's question rather than keeping a second copy of it.
\ Measured on the seeded product: FAM-BASE is 119 and the unfiltered walk turned
\ a 13-definition manifest into 386 definitions.
\
\ AN ENGINE WITH NOTHING BELOW THE MARK says so. An unseeded host carries no
\ public enum or derived family at all before this load, so there is nothing for
\ it to exclude; the case records that rather than passing quietly on an
\ assertion that could not fail. The gate runs this on the product, where it is
\ live.
\
\ Run: bin/hb --load tools/public-signatures-bracket-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
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
   s" examples/llm/good.f" ;

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
