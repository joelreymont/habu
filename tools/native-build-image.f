\ Construct native support from the bytes the child actually consumed.
require src/lib/errors.f
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
create ROOT-BUF FS-PATH-CAP allot
create ENGINE-BUF FS-PATH-CAP allot
create PATH-BUF FS-PATH-CAP allot
create RESULT-PATH FS-PATH-CAP allot
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

\ These definitions run from stdin before the first require. The first imports
\ include the ordinary input recorder itself, so its dependency reads are
\ staged here, then drained into the same persistent row format.
: PROGRAM! ( -- )
   S\" 1 set-tier\npackage NB-START private\nDYNAMIC-BUFFER EVENTS u8\nvariable USED\nvariable CURSOR\ncreate SHA SHA256-CTX-BYTES allot\n" PROGRAM+
   S\" : ROOM ( n -- n ) {: u:n :} USED @ u + 16777216 > if E-FS-CAPACITY throw then USED @ u + EVENTS-RESERVE USED @ dup u + USED ! ;\n" PROGRAM+
   S\" : ROW ( n ptr u8 n ptr u8 n bool -- ptr u8 ) {: kind:n path:ptr pu:n answer:ptr au:n exists:bool :} 64 pu + au + ROOM EVENTS {: dst:ptr :} kind dst LE:U64! pu dst 8 + LE:U64! au dst 16 + LE:U64! exists if 1 else 0 then dst 24 + LE:U64! 32 0 ?do 0 dst 32 + i + c! loop path dst 64 + pu BYTE-COPY answer dst 64 + pu + au BYTE-COPY dst ;\n" PROGRAM+
   S\" : CANON ( ptr u8 n -- ptr u8 n bool ) {: path:ptr pu:n :} path pu SOURCE-ROOT:CANON-OS {: ans:ptr au:n yes:bool :} 1 path pu ans au yes ROW drop ans au yes ;\n" PROGRAM+
   S\" : READ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: path:ptr pu:n root:ptr ru:n :} path pu root ru SOURCE-ROOT:READ-OS {: src:ptr su:n :} 2 path pu root ru true ROW 32 + {: digest:ptr :} SHA src su digest SHA256-IN src su ;\n" PROGRAM+
   S\" public : ON ( -- ) ['] CANON ['] READ SOURCE-INPUT:USE ;\n;package\nNB-START:ON\nrequire tools/native-file-inputs.f\npackage NB-START private\n" PROGRAM+
   S\" : NEXT-CANON ( ptr u8 n -- ptr u8 n bool ) {: path:ptr pu:n :} path pu SOURCE-ROOT:CANON-OS {: ans:ptr au:n yes:bool :} path pu ans au yes NFILE-INPUTS:ADD-CANON ans au yes ;\n" PROGRAM+
   S\" : NEXT-READ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: path:ptr pu:n root:ptr ru:n :} path pu root ru SOURCE-ROOT:READ-OS {: src:ptr su:n :} path pu root ru src su NFILE-INPUTS:ADD-ROOT src su ;\n" PROGRAM+
   S\" public : DRAIN ( -- ) 0 CURSOR ! begin CURSOR @ USED @ < while CURSOR @ EVENTS {: row:ptr :} row 8 + LE:U64@ {: pu:n :} row 16 + LE:U64@ {: au:n :} row 64 + {: path:ptr :} path pu + {: ans:ptr :} row LE:U64@ 1 = if path pu ans au row 24 + LE:U64@ 0<> NFILE-INPUTS:ADD-CANON else path pu ans au row 32 + NFILE-INPUTS:ADD-HASH then 64 pu + au + CURSOR +! repeat EVENTS-RELEASE 0 USED ! ['] NEXT-CANON ['] NEXT-READ SOURCE-INPUT:USE ;\n" PROGRAM+
   S\" : OFF ( -- ) SOURCE-INPUT:RESET EVENTS-RELEASE 0 USED ! ;\n;package\nNB-START:DRAIN\nrequire tools/native-build-support.f\nNATIVE-BUILD-SUPPORT:SAVE-ROOT\nrequire tools/native-build-result.f\nNATIVE-BUILD-RESULT:WRITE\nNB-START:OFF\nNFILE-INPUTS:CLOSE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" PROGRAM+ ;

: PROGRAM$ ( -- ptr u8 n )
   PROGRAM-U @ 0= if PROGRAM! then
   PROGRAM-BUF PROGRAM-U @ ;

: COPY-PATH! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY u up ! ;

: ROWS! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u SUPPORT-ROWS-RESERVE
   a 0 SUPPORT-ROWS u BYTE-COPY
   u SUPPORT-ROWS-U ! ;

: RESULT-OF ( ptr u8 n -- ptr u8 n )
   {: image:ptr imageu:n :}
   imageu 7 + FS-PATH-CAP > if E-FS-CAPACITY throw then
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
   RESULT-OF FILE-SIZE {: size:n :}
   size RESULT-HEADER-U < if E-NUNIT-PROFILE throw then
   size RESULT-RECORD-RESERVE
   RESULT-PATH RESULT-PATH-U @ 0 RESULT-RECORD size READ-ALL size <>
      if E-FS-IO throw then
   0 RESULT-RECORD {: rec:ptr :}
   rec 64 + LE:U64@ {: rootu:n :}
   rec 72 + LE:U64@ {: rowsu:n :}
   rootu 0 <= rootu FS-PATH-CAP > or if E-NUNIT-PROFILE throw then
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
