\ Construct native support from the bytes the child actually consumed.
require src/lib/error.f
require src/lib/fs.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/engine-id.f
require src/lib/process-argv.f
require src/lib/content-key.f
require tools/build-target.f
require tools/keyed-image.f
require tools/native-file-inputs.f

package NATIVE-BUILD-IMAGE

8192 constant PROGRAM-CAP
80 constant RESULT-HEADER-U
create PROGRAM-BUF PROGRAM-CAP allot
create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create DONOR-BUF 64 allot
create ROOT-BUF PATH-CAP allot
create ENGINE-BUF PATH-CAP allot
create PATH-BUF PATH-CAP allot
create RESULT-PATH PATH-CAP allot
create ROW-SHA SHA256-CTX-BYTES allot
create ROW-DIGEST 32 allot
DYNAMIC-BUFFER RESULT-RECORD u8
DYNAMIC-BUFFER SUPPORT-ROWS u8
variable PROGRAM-U
variable ROOT-U
variable ENGINE-U
variable PATH-U
variable RESULT-PATH-U
variable SUPPORT-ROWS-U

: FAMILY$ ( -- ptr u8 n ) s" native-build-support" ;
: PROGRAM+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   PROGRAM-U @ u + PROGRAM-CAP > if E-FS-CAPACITY throw then
   a PROGRAM-BUF PROGRAM-U @ + u BYTE-COPY
   u PROGRAM-U +! ;

\ The child first loads this tree's startup files (tools/cold-load.f) at the
\ default tier, as the engine compiles them; tier 1 applies to the files after
\ it. TOOL-ROLE keeps the cold load's reads and records the support load's, and
\ NATIVE-BUILD-RESULT hands both on as the support inputs.
: PROGRAM! ( -- )
   S\" require tools/cold-load.f\nCOLD-LOAD:RUN\n1 set-tier\nrequire tools/tool-role.f\n' TOOL-ROLE:COLD-ROW COLD-SOURCES:DRAIN\n" PROGRAM+
   S\" require tools/native-build-support.f\nNATIVE-BUILD-SUPPORT:SAVE-ROOT\nNATIVE-BUILD-RESULT:WRITE\nNFILE-INPUTS:CLOSE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" PROGRAM+ ;

: PROGRAM$ ( -- ptr u8 n )
   PROGRAM-U @ 0= if PROGRAM! then
   PROGRAM-BUF PROGRAM-U @ ;

: COPY-PATH! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY u up ! ;

: ROWS! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u SUPPORT-ROWS-RESERVE
   a 0 SUPPORT-ROWS u BYTE-COPY
   u SUPPORT-ROWS-U ! ;

: RESULT-OF ( ptr u8 n -- ptr u8 n )
   {: image:ptr imageu:n :}
   imageu 7 + PATH-CAP > if E-FS-CAPACITY throw then
   image RESULT-PATH imageu BYTE-COPY
   s" .result" drop RESULT-PATH imageu + 7 BYTE-COPY
   imageu 7 + RESULT-PATH-U !
   RESULT-PATH RESULT-PATH-U @ ;

: SUPPORT-KEY-BODY ( CONTENT-KEY:fold -- )
   s" native-build-support-image-v2" CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   DONOR-BUF 64 CONTENT-KEY:TEXT+
   ROOT-BUF ROOT-U @ CONTENT-KEY:TEXT+
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ CONTENT-KEY:TEXT+
   ROW-DIGEST CONTENT-KEY:DIGEST+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: SUPPORT-KEY! ( -- )
   ROW-SHA 0 SUPPORT-ROWS SUPPORT-ROWS-U @ ROW-DIGEST SHA256-IN
   [: SUPPORT-KEY-BODY ;] CONTENT-KEY:WITH-FOLD
   FAMILY$ DONOR-BUF PATH-BUF PATH-U KEYED-IMAGE:PATH! ;

\ The key the image at this path publishes under, from the result record its
\ child wrote beside it.
: ACCEPT-RESULT ( ptr u8 n -- ptr u8 )
   RESULT-OF FS:FILE-SIZE {: size:n :}
   size RESULT-HEADER-U < if E-NUNIT-PROFILE throw then
   size RESULT-RECORD-RESERVE
   RESULT-PATH RESULT-PATH-U @ 0 RESULT-RECORD size FS:READ-ALL size <>
      if E-FS-IO throw then
   0 RESULT-RECORD {: rec:ptr :}
   rec 64 + LE:U64@ {: rootu:n :}
   rec 72 + LE:U64@ {: rowsu:n :}
   rootu 0 <= rootu PATH-CAP > or if E-NUNIT-PROFILE throw then
   rootu size RESULT-HEADER-U - > if E-NUNIT-PROFILE throw then
   rowsu size RESULT-HEADER-U rootu + - <> if E-NUNIT-PROFILE throw then
   rec RESULT-HEADER-U + rootu + rowsu NFILE-INPUTS:FORMAT
   rec DONOR-BUF 64 BYTE-COPY
   rec RESULT-HEADER-U + ROOT-BUF rootu BYTE-COPY
   rootu ROOT-U !
   rec RESULT-HEADER-U + rootu + rowsu ROWS!
   RESULT-RECORD-RELEASE
   SUPPORT-KEY!
   KEY-HEX ;

: OUT-ARG ( ptr u8 n -- )
   2dup >LEN PROC-ARGV+
   RESULT-OF >LEN PROC-ARGV+ ;

public

: CONTEXT! ( -- )
   ENGINE-ID:KEY$ drop DONOR-BUF 64 BYTE-COPY
   ENGINE-ID:PATH$ ENGINE-BUF ENGINE-U COPY-PATH!
   ENGINE-ID:PATH$ SOURCE-ROOT:ENGINE-FOR$
      ROOT-BUF ROOT-U COPY-PATH! ;

: DONOR$ ( -- ptr u8 n ) DONOR-BUF 64 ;
: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: PROGRAM-BYTES$ ( -- ptr u8 n ) PROGRAM$ ;
: KEY$ ( -- ptr u8 n ) KEY-HEX KEYED-IMAGE:KEY-HEX-LEN ;
: ROWS$ ( -- ptr u8 n ) 0 SUPPORT-ROWS SUPPORT-ROWS-U @ ;
: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

: ADOPT ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n rows:ptr rowsu:n :}
   keyu 64 <> if E-NUNIT-PROFILE throw then
   rows rowsu NFILE-INPUTS:FORMAT
   key KEY-HEX 64 BYTE-COPY
   rows rowsu ROWS!
   FAMILY$ DONOR-BUF PATH-BUF PATH-U KEYED-IMAGE:PATH! ;

: REUSE? ( -- bool )
   KEY-HEX FAMILY$ DONOR-BUF KEYED-IMAGE:FRESH? ;

\ DONOR-BUF is the engine key KEYED-IMAGE reads, and ACCEPT-RESULT replaces it
\ with the key the child reports: the image publishes under the engine that
\ built it, even when that file changed after CONTEXT! read it.
: BUILD$ ( bool -- ptr u8 n )
   {: quiet:bool :}
   FAMILY$ DONOR-BUF ENGINE-BUF ENGINE-U @ PROGRAM$ ['] OUT-ARG 180000 quiet
      ['] ACCEPT-RESULT KEYED-IMAGE:BUILD-WITH-RESULT-QUIET ;

;package
