\ codegen-census-test.f - tools/codegen-census.f through its command line.
\
\ The ways the census can fail, and what reaches each here:
\   the image is not an ARM64 Habu executable       a text file: refused 74 by name
\   the image is truncated                          a 64 KiB prefix of bin/hb: refused 74
\   the argument count is wrong                     refused 64 with the usage line
\   a pattern's row is missing or malformed         each of the eleven P rows is found and
\                                                   holds only decimal fields, as many as its form
\   a row claims to save more than the shape holds  each P row's saving is at most its bytes
\   the report names the wrong image or commit      the E line opens with bin/hb's own
\                                                   SHA-256 and the commit given
\ Whether a detector still matches the emitter's shapes is read from the report
\ artifact, not asserted: the code-size fixes drive rows such as signed-maximum
\ and remainder to zero on a correct engine. Not reachable from a product: a
\ malformed payload or missing aot/code-spans rows (the payload walk refuses
\ them), a declared site outside the blob or not a carrier, a br in the blob,
\ an engine without throw, die or ! (the census refuses those by name), and a
\ stripped or snapshot image, which needs an application build. The tool reads
\ its argv, so it is spawned.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package CODEGEN-CENSUS-TEST
private

4096 constant CAP
65536 constant CUT-BYTES
create OUT CAP allot
create ERR CAP allot
variable ERR-U
create ROOT FS-PATH-CAP allot
variable ROOT-U
create REPORT FS-PATH-CAP allot
variable REPORT-U
create CUT FS-PATH-CAP allot
variable CUT-U
DYNAMIC-BUFFER TEXT u8
variable TEXT-U
create FCTX SHA256-FILE-CTX-BYTES allot
create HEX 64 allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: REPORT$ ( -- ptr u8 n ) REPORT REPORT-U @ ;
: CUT$ ( -- ptr u8 n ) CUT CUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: TEXT$ ( -- ptr u8 n ) 0 TEXT TEXT-U @ ;

: PREP ( -- )
   CLEANUP-RESET
   s" habu-codegen-census" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" codegen-census.txt" REPORT JOIN-PATH REPORT-U !
   ROOT$ s" truncated" CUT JOIN-PATH CUT-U ! ;

\ One census of img, with the commit and report arguments when whole.
: RUN ( ptr u8 n bool -- n ) {: img:ptr imgu:n whole:bool :}
   PROC-ARGV-RESET PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+ s" tools/codegen-census.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ img imgu >LEN PROC-ARGV+
   whole if s" census-test-commit" >LEN PROC-ARGV+ REPORT$ >LEN PROC-ARGV+ then
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN 60000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   erru LEN>N ERR-U !
   rc ;

: SLURP ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size 1+ TEXT-RESERVE
   path pathu 0 TEXT size READ-ALL size T=
   size TEXT-U ! ;

3 constant ROW-FIELDS                  \ sites, bytes, estimated saving
5 constant DATA-FIELDS                 \ ... and distinct targets per record and per island
DATA-FIELDS TYPED-BUFFER FIELD n
variable FIELDS

: NUMBER ( ptr u8 n -- n )
   STR>NUMBER? MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH ;

\ The space-separated fields of a row's tail, each of which must be decimal.
: TAIL-FIELDS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 FIELDS !
   0 begin
      {: start:n :}
      a u 32 start SPLIT-NEXT 0= if drop 2drop exit then
      {: f:ptr fu:n next:n :}
      f fu STR-DIGITS? TTRUE
      FIELDS @ DATA-FIELDS < if f fu NUMBER FIELDS @ FIELD ! then
      1 FIELDS +!
      next
   again ;

\ The pattern's row is in the report, holds want decimal fields, and its
\ estimated saving is at most the bytes its shapes occupy.
: P-ROW ( ptr u8 n n -- ) {: name:ptr u:n want:n :}
   SB-RESET S\" \nP " SB-APPEND name u SB-APPEND s"  " SB-APPEND
   TEXT$ SB$ FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N SB$ nip + ENDOF
   ;MATCH {: at:n :}
   name u T-LABEL  at 0 >= TTRUE
   at 0 < if exit then
   at TEXT  TEXT-U @ at - {: row:ptr rest:n :}
   row rest 10 INDEX-OF MATCH option none OF rest ENDOF some OF IDX>N ENDOF ;MATCH
   {: rowu:n :}
   row rowu TAIL-FIELDS
   name u T-LABEL  FIELDS @ want T=
   FIELDS @ want <> if exit then
   name u T-LABEL  2 FIELD @ 1 FIELD @ <= TTRUE ;

: ON-PRODUCT ( -- )
   s" the product's census runs" T-LABEL
   s" bin/hb" 0 0= RUN 0 T=
   REPORT$ SLURP
   FCTX s" bin/hb" HEX SHA256-FILE-HEX-IN 0 T=
   s" the E line names bin/hb's SHA-256 and the commit" T-LABEL
   SB-RESET s" E " SB-APPEND HEX 64 SB-APPEND s"  census-test-commit " SB-APPEND
   TEXT$ SB$ STARTS-WITH? TTRUE
   s" guarded-division" ROW-FIELDS P-ROW  s" terminal-only-frame" ROW-FIELDS P-ROW
   s" mask-chain" ROW-FIELDS P-ROW  s" constant-shift" ROW-FIELDS P-ROW
   s" scaled-index" ROW-FIELDS P-ROW  s" remainder" ROW-FIELDS P-ROW
   s" signed-maximum" ROW-FIELDS P-ROW  s" call-crossing-spill" ROW-FIELDS P-ROW
   s" data-carrier" DATA-FIELDS P-ROW  s" no-return-fallback" ROW-FIELDS P-ROW
   s" wide-store-run" ROW-FIELDS P-ROW ;

: REFUSALS ( -- )
   s" a non-image is refused by name" T-LABEL
   s" tools/codegen-census.f" 0 0= RUN 74 T=
   ERR$ s" not an ARM64 Habu executable" CONTAINS? TTRUE
   s" a truncated product is refused" T-LABEL
   s" bin/hb" SLURP
   CUT$ 0 TEXT CUT-BYTES WRITE-ALL
   CUT$ 0 0= RUN 74 T=
   \ The container reader refuses it on macOS, the payload walk on Linux.
   ERR$ s" macho-read: " CONTAINS? ERR$ s" image-size: " CONTAINS? or TTRUE
   s" a short command line gets the usage line" T-LABEL
   s" bin/hb" 0 0= 0= RUN 64 T=
   ERR$ s" usage: " CONTAINS? TTRUE ;

: CENSUS-TEST ( -- )
   T-RESET PREP ON-PRODUCT REFUSALS
   TEXT-RELEASE CLEANUP-RUN T-REPORT ;

CENSUS-TEST
;package
