\ aot-shadow-capture.f - a capture taken while an x86-64 shadow is open carries the
\ shadow's routines (src/habu/aot-shadow.f into src/habu/aot-decl.f AOT-SHADOW),
\ and src/habu/aot-file.f carries them through an artifact unchanged.
\
\ WHAT IS DRIVEN. A small window compiled at tier 1 by the engine's own driver
\ with the shadow open, so each definition is also an x86-64 routine in
\ NSHADOW's map, then the real capture and the shadow reader over it, then WRITE
\ and READ. The window holds one of each thing the shadow tables carry: a leaf, a
\ call to a window word, a `does>` definer, a quotation, a code literal naming a
\ window word, a window DATA literal, a call to a word of the engine's own prefix,
\ and a declared code cell holding a window word's entry. It also holds four
\ shadowed words that ship no record: a dead private word and two dead retired
\ ones, the second calling the first, between two shadowed records - the capture
\ strips the first and does not record the retired ones at all, so past them a
\ record's window index, its capture index and its shipped row all differ - and
\ a private `does>` definer whose shipped companion carries its routine, and a
\ live private callee whose x86 routine is carried anonymously.
\
\ WHAT IS ASKED. Every record is found by the name it carries in the capture's
\ shipped record table, the row the shadow keys it by: the records and their
\ spans, the call site's target, the companion's entry against the definer's own
\ `codeaddr`, the function, code and DATA literals as the capture rewrote them,
\ the prefix call by name, the code cell, the stripped and retired words'
\ absence, the carried definer, the anonymous routine and its call site, and
\ the four tables after a READ of what
\ WRITE wrote, then the same file from a second WRITE. Refusing an artifact of
\ the version before this one is test/aot-chain-capture-suite.f's old-version
\ row case; test/x86-64-link-records.f covers link refusals.
\
\ WHAT IS REFUSED. Artifacts this WRITE produced with one table field changed: a
\ record's routine one byte past the shadow code, a site and a code cell naming
\ the record one past the last shipped one, each refused by READ by name; and the
\ unchanged artifact, refused by MERGE into a host written without its shadow. A
\ refusal ends the process that reads, so a child - this file, handed the paths
\ - does the reading, and the parent asks for the reader's exit code and its
\ sentence. Capture children exercise retired, alias and private cases: a
\ retired body reached only through tier-0 code needs no x86 routine; a public
\ alias carries the body it shares; a dead retired body's calls retain none of
\ their private callees; a public EXPORT alias carries its private source's
\ routine without publishing that source's name.
\
\ THE MARKS COME FIRST, then the window, then the artifact writer, in the order
\ test/aot-artifact-roundtrip.f gives its reasons for.
\
\ Registered as `SUITE aot-shadow-capture`. Run standalone from the repository
\ root: bin/hb --load test/aot-shadow-capture.f
\ The reader child: bin/hb --load test/aot-shadow-capture.f -- read <artifact>
\              or: bin/hb --load test/aot-shadow-capture.f -- merge <host> <artifact>
\ The capturing child: bin/hb --load test/aot-shadow-capture.f -- retired|alias|bridge|helper|export

package AOTSH
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
;package

require lib/string.f

package AOTSH
public
: MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   false ;
\ What a capturing child's window adds before GONE retires. Live code reaches
\ GONE through a live tier-0 word calling GONER - it has no x86-64 routine, so
\ no shipped routine names either retired word - or through an alias of GONE in
\ another public package. Only dead code reaches it through VIA, retired too,
\ calling a private tier-0 word that calls GONE. The last two leave GONE dead
\ and strip a private word: HELP, which only GONEST calls, retired, and SHARED,
\ whose body the public alias EXPORT makes shares.
: MODE$ ( -- ptr u8 n )
   s" retired" MODE? if s" 0 set-tier : KEEP ( -- n ) GONER 1+ ; 1 set-tier" exit then
   s" alias" MODE? if
      s" ;package package AOTSH-ALIAS public EXPORT AOTSH-WINDOW:GONE ;package package AOTSH-WINDOW public"
      exit
   then
   s" bridge" MODE? if
      s" private 0 set-tier : BRIDGE ( -- n ) GONE 1+ ; public 1 set-tier : VIA ( -- n ) BRIDGE 1+ ; undefine VIA"
      exit
   then
   s" helper" MODE? if
      s" private : HELP ( -- n ) 7 ; public : GONEST ( -- n ) HELP 1+ ; undefine GONEST"
      exit
   then
   s" export" MODE? if s" private : SHARED ( -- n ) 7 ; public EXPORT SHARED" exit then
   s" " ;
;package

require lib/le.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/compiler/native/string.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f

\ A binding is a multi-cell value, which only a compiled body may hold.
package AOTSH
public
: OPEN-X64 ( -- ) X64ABI:BINDING NSHADOW:OPEN ;
;package
AOTSH:OPEN-X64
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package AOTSH-WINDOW
public

create WCELL 8 allot
7 WCELL !

1 set-tier
: PEEK ( -- n ) WCELL @ ;
private : HIDDEN ( n -- ) . ; public
: GONE ( -- n ) PEEK 1+ ;
: GONER ( -- n ) GONE 1+ ;
AOTSH:MODE$ evaluate
undefine GONE
undefine GONER
: LEAF ( n n -- n ) + ;
: CALLER ( n -- n ) dup LEAF 1 + ;
: CONST ( n -- ) create , does> ( -- n ) @ ;
private : MAKER ( n -- ) create , does> ( -- n ) @ 1+ ; public
5 MAKER MADE
: QUOT ( -- [ n -- n ] ) [: 1 + ;] ;
: TICK ( -- [ n n -- n ] ) ['] LEAF ;
: SHOW ( n -- ) . ;
private : SECRET ( n -- n ) 1+ ; public
: CALL-SECRET ( n -- n ) SECRET ;
CAST: >CAP-IDX ( n -- idx )
CAST: CAP-IDX>N ( idx -- n )
: CAST-USER ( n -- n ) >CAP-IDX CAP-IDX>N 1+ ;
0 set-tier

defer HOOK ( n n -- n )
: ARM-HOOK ( -- ) ['] LEAF is HOOK ;
ARM-HOOK

;package

AOT-ARM:WINDOW-CLOSE

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/address-carrier.f
require src/habu/aot-file.f

package AOTSH
using AOT-BUF

32 constant SHA-BYTES
create KEY SHA-BYTES allot           \ the producer key: any 32 bytes READ is handed back
create DIR FS-PATH-CAP allot    variable DIR-U       \ where the artifacts go
create ART FS-PATH-CAP allot    variable ART-U       \ the capture's own
create FORGED FS-PATH-CAP allot    variable FORGED-U \ one with a field changed
create HOST FS-PATH-CAP allot    variable HOST-U     \ the capture without its shadow
create SHA-CTX SHA256-CTX-BYTES allot
create DIGESTS SHA-BYTES 4 * allot   \ the four tables' digests before the round trip
create REREAD SHA-BYTES allot
create FIRST SHA-BYTES allot         \ the first WRITE's file digest

: ART$ ( -- ptr u8 n ) ART ART-U @ ;

\ ---- the records, as the capture ships them -------------------------------------
\ The row of the capture's compact record table that carries `name`, or -1 when
\ the capture strips the record: the table holds only the records it ships, and
\ the shadow keys every routine, site and code cell by that row.
: CREC ( n -- ptr u8 ) {: k:n :} AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;
: SHIPPED ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   AOT-REC-N @ 0 ?do
      AOT-NAMES-BUF@ i CREC 8 + LE:U32@ + {: e:ptr :}
      e 1+ e c@ a u STR= if drop i leave then
   loop ;
: W-PEEK ( -- n ) s" PEEK" SHIPPED ;
: W-LEAF ( -- n ) s" LEAF" SHIPPED ;
: W-CALLER ( -- n ) s" CALLER" SHIPPED ;
: W-CONST ( -- n ) s" CONST" SHIPPED ;
: W-DOES ( -- n ) s" CONST;does" SHIPPED ;
: W-MAKER-DOES ( -- n ) s" MAKER;does" SHIPPED ;
: W-MADE ( -- n ) s" MADE" SHIPPED ;
: W-QUOT ( -- n ) s" QUOT" SHIPPED ;
: W-TICK ( -- n ) s" TICK" SHIPPED ;
: W-SHOW ( -- n ) s" SHOW" SHIPPED ;
: W-CALL-SECRET ( -- n ) s" CALL-SECRET" SHIPPED ;
: W-CAST-IN ( -- n ) s" >CAP-IDX" SHIPPED ;
: W-CAST-OUT ( -- n ) s" CAP-IDX>N" SHIPPED ;
: W-CAST-USER ( -- n ) s" CAST-USER" SHIPPED ;

\ ---- the tables ---------------------------------------------------------------
: REC-AT ( n n -- ptr u8 ) {: r:n f:n :}
   AOT-SHADOW:REC-BUF@ r AOT-SHADOW:REC-ROW * + f + ;
: SITE-AT ( n n -- ptr u8 ) {: s:n f:n :}
   AOT-SHADOW:SITE-BUF@ s AOT-SHADOW:SITE-ROW * + f + ;
: XT-AT ( n n -- ptr u8 ) {: x:n f:n :}
   AOT-SHADOW:XT-BUF@ x AOT-SHADOW:XT-ROW * + f + ;
: REC@ ( n n -- n ) REC-AT LE:U32@ ;
: SITE@ ( n n -- n ) SITE-AT LE:U32@ ;
: XT@ ( n n -- n ) XT-AT LE:U32@ ;
: CODE-BYTE@ ( n -- n ) AOT-SHADOW:CODE-BUF@ + c@ ;
: IMM@ ( n -- n ) AOT-SHADOW:CODE-BUF@ + ADDRESS-CARRIER:MOVABSV ;

\ The shadow's record row keyed by shipped record w, or -1.
: ROW-OF ( n -- n ) {: w:n :}
   -1
   AOT-SHADOW:REC-N @ 0 ?do
      i 0 REC@ w = if drop i leave then
   loop ;

: AT-OF ( n -- n ) ROW-OF 4 REC@ ;
: LEN-OF ( n -- n ) ROW-OF 8 REC@ ;

\ The first site of `kind` inside shipped record w's routine, or -1.
: SITE-IN ( n n -- n ) {: w:n kind:n :}
   w AT-OF {: at:n :}
   w LEN-OF {: len:n :}
   -1
   AOT-SHADOW:SITE-N @ 0 ?do
      i 0 SITE@ {: s:n :}
      s at >=  s at len + < and  i 4 SITE@ kind = and if drop i leave then
   loop ;

: SITES-IN ( n -- n ) {: w:n :}
   w AT-OF {: at:n :}
   w LEN-OF {: len:n :}
   0
   AOT-SHADOW:SITE-N @ 0 ?do
      i 0 SITE@ {: s:n :}
      s at >= s at len + < and if 1+ then
   loop ;

: REC-TARGET ( n -- n ) SITE-REC-TAG or ;

\ The window offset of WCELL, as the DATA literal must now hold it.
: WCELL-OFF ( -- n )
   AOTSH-WINDOW:WCELL BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N +  AOT-ARM:D0 @ - ;

\ ---- the capture -------------------------------------------------------------
: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-CAPTURE:SHADOW-CAPTURE ;

: RECORDS-CASE ( -- )
   s" named routines and an anonymous callee follow publication order" T-LABEL
   W-PEEK ROW-OF 0 >= TTRUE
   W-LEAF ROW-OF W-PEEK ROW-OF 1+ T=
   W-CALLER ROW-OF W-LEAF ROW-OF 1+ T=
   W-CONST ROW-OF W-CALLER ROW-OF 1+ T=
   W-DOES ROW-OF W-CONST ROW-OF 1+ T=
   W-MAKER-DOES ROW-OF W-DOES ROW-OF 1+ T=
   W-MADE ROW-OF W-MAKER-DOES ROW-OF 1+ T=
   W-QUOT ROW-OF W-MADE ROW-OF 1+ T=
   W-TICK ROW-OF W-QUOT ROW-OF 1+ T=
   W-SHOW ROW-OF W-TICK ROW-OF 1+ T=
   W-CALL-SECRET ROW-OF W-SHOW ROW-OF > TTRUE
   W-CAST-IN ROW-OF W-CALL-SECRET ROW-OF > TTRUE
   W-CAST-OUT ROW-OF W-CAST-IN ROW-OF 1+ T=
   W-CAST-USER ROW-OF W-CAST-OUT ROW-OF 1+ T= ;

: CAST-CASE ( -- )
   s" checked cast declarations have recorded x86 identity routines" T-LABEL
   W-CAST-IN ROW-OF 0 >= TTRUE
   W-CAST-OUT ROW-OF 0 >= TTRUE
   W-CAST-IN LEN-OF 0 > TTRUE
   W-CAST-OUT LEN-OF 0 > TTRUE ;

: ANON-CASE ( -- )
   s" a live private callee has an x86 routine but no shipped dictionary name" T-LABEL
   s" SECRET" SHIPPED -1 T=
   W-CALL-SECRET AOT-SHADOW:TAIL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ {: target:n :}
   target SITE-TARGET-MASK invert and SITE-SHADOW-TAG T=
   target SITE-TARGET-MASK and {: row:n :}
   row AOT-SHADOW:REC-N @ < TTRUE
   row 0 REC@ AOT-SHADOW:ANON-REC and AOT-SHADOW:ANON-REC T= ;

: STRIP-CASE ( -- )
   s" a dead private word and two dead retired ones between two shadowed records ship no record and no routine, and the record after them ships one row past PEEK, four window records on" T-LABEL
   s" HIDDEN" SHIPPED -1 T=
   s" GONE" SHIPPED -1 T=
   s" GONER" SHIPPED -1 T=
   W-LEAF W-PEEK 1+ T=
   s" AOTSH-WINDOW:LEAF" XREF-FIND-INDEX  s" AOTSH-WINDOW:PEEK" XREF-FIND-INDEX 4 +  T= ;

: CARRIED-CASE ( -- )
   s" a live private definer ships no record, and its shipped does> companion carries its routine from the definer's start" T-LABEL
   s" MAKER" SHIPPED -1 T=
   W-MAKER-DOES ROW-OF {: r:n :}
   r 0 >= TTRUE
   r 12 REC@ {: entry:n :}
   entry 0 > TTRUE
   W-MAKER-DOES AOT-SHADOW:FUN SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 0 SITE@ IMM@ entry T=
   W-MADE AOT-SHADOW:TAIL SITE-IN {: patch:n :}
   patch 0 >= TTRUE
   patch 8 SITE@ W-MAKER-DOES REC-TARGET T=
   patch 0 SITE@ CODE-BYTE@ $E9 T= ;

: SPAN-CASE ( -- )
   s" a leaf's span is one x86-64 routine entered at its start, ending in ret, with no site" T-LABEL
   W-LEAF ROW-OF 12 REC@ 0 T=
   W-LEAF LEN-OF 0 > TTRUE
   W-LEAF AT-OF W-LEAF LEN-OF + 1- CODE-BYTE@ $C3 T=
   W-LEAF SITES-IN 0 T= ;

: CALL-CASE ( -- )
   s" a call to a window word is a rel32 call row naming that word's shipped record, and its field stays zero" T-LABEL
   W-CALLER AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ W-LEAF REC-TARGET T=
   s 0 SITE@ {: at:n :}
   at CODE-BYTE@ $E8 T=
   at 1+ CODE-BYTE@ at 2 + CODE-BYTE@ or at 3 + CODE-BYTE@ or at 4 + CODE-BYTE@ or 0 T= ;

: DOES-CASE ( -- )
   s" a does> companion shares its definer's routine and enters at the clause, the offset the definer's own codeaddr holds" T-LABEL
   W-CONST ROW-OF {: r:n :}
   W-DOES ROW-OF {: c:n :}
   c 4 REC@ r 4 REC@ T=
   c 8 REC@ r 8 REC@ T=
   r 12 REC@ 0 T=
   c 12 REC@ {: entry:n :}
   entry 0 > TTRUE
   W-CONST AOT-SHADOW:FUN SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 0 SITE@ IMM@ entry T= ;

: LITERAL-CASE ( -- )
   s" a quotation keeps its function offset, a code literal names its word's record over a zeroed field, a DATA literal holds its window offset" T-LABEL
   W-QUOT AOT-SHADOW:FUN SITE-IN {: q:n :}
   q 0 >= TTRUE
   q 0 SITE@ IMM@ 0 > TTRUE
   q 0 SITE@ IMM@ W-QUOT LEN-OF < TTRUE
   W-TICK AOT-SHADOW:CODE SITE-IN {: t:n :}
   t 0 >= TTRUE
   t 8 SITE@ W-LEAF REC-TARGET T=
   t 0 SITE@ IMM@ 0 T=
   W-PEEK AOT-SHADOW:DATA SITE-IN {: d:n :}
   d 0 >= TTRUE
   d 0 SITE@ IMM@ WCELL-OFF T= ;

: PREFIX-CASE ( -- )
   s" a call to a word of the engine's own prefix travels by name" T-LABEL
   W-SHOW AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ {: t:n :}
   t SITE-TARGET-MASK invert and SITE-NAME-TAG T=
   AOT-NAMES-BUF@ t SITE-TARGET-MASK and + {: e:ptr :}
   e 1+ e c@ s" ." T$= ;

: CELL-CASE ( -- )
   s" a declared code cell holding a window word's entry is keyed by that word's record" T-LABEL
   AOT-SHADOW:XT-N @ 1 T=
   0 4 XT@ W-LEAF T=
   0 0 XT@ AOT-WINDOW:XTOFF-N @ < TTRUE ;

\ ---- the round trip ---------------------------------------------------------
: TABLE$ ( n -- ptr u8 n ) {: k:n :}
   k 0 = if AOT-SHADOW:REC-BUF@ AOT-SHADOW:REC-N @ AOT-SHADOW:REC-ROW * exit then
   k 1 = if AOT-SHADOW:CODE-BUF@ AOT-SHADOW:CODE-LEN @ exit then
   k 2 = if AOT-SHADOW:SITE-BUF@ AOT-SHADOW:SITE-N @ AOT-SHADOW:SITE-ROW * exit then
   AOT-SHADOW:XT-BUF@ AOT-SHADOW:XT-N @ AOT-SHADOW:XT-ROW * ;

: DIGEST ( n ptr u8 -- ) {: k:n out:ptr :}
   SHA-CTX SHA256-BEGIN
   SHA-CTX k TABLE$ SHA256-FEED
   SHA-CTX out SHA256-END ;

\ What the writer and every reader child agree on: the producer key, and the
\ chain sources the header digests.
: IDENT! ( -- )
   SHA-BYTES 0 ?do 0 KEY i + c! loop
   AOT-IDENT:RESET
   s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   s" src/habu/aot-shadow.f" AOT-IDENT:PATH+ ;

\ The path of `name` in the artifacts' directory, into `dst`.
: IN-DIR ( ptr u8 n ptr u8 -- n ) {: na:ptr nu:n dst:ptr :}
   DIR DIR-U @ na nu dst JOIN-PATH ;

: ARTIFACT! ( -- )
   s" habu-aot-shadow-capture" HB-TMP-MKDIR {: a:ptr u:n :}
   a u CLEANUP-TREE+
   a DIR u BYTE-COPY  u DIR-U !
   s" shadow.aot" ART IN-DIR ART-U !
   IDENT! ;

: ROUND-CASE ( -- )
   s" WRITE and READ carry the four shadow tables byte for byte, and a second WRITE is the same file" T-LABEL
   ARTIFACT!
   4 0 ?do i DIGESTS i SHA-BYTES * + DIGEST loop
   KEY ART$ AOT-FILE:WRITE
   AOT-FILE:SHA$ drop FIRST SHA-BYTES BYTE-COPY
   AOT-SHADOW:RESET
   KEY ART$ AOT-FILE:READ
   4 0 ?do
      i REREAD DIGEST
      REREAD SHA-BYTES DIGESTS i SHA-BYTES * + SHA-BYTES T$=
   loop
   KEY ART$ AOT-FILE:WRITE
   AOT-FILE:SHA$ FIRST SHA-BYTES T$= ;

\ ---- the reader child ---------------------------------------------------------
\ src/habu/aot-file.f's DIE: this exit code, its sentence on stderr.
$4B constant REFUSE-RC
64 constant USAGE-RC                \ sysexits EX_USAGE
$8000 constant CAP
60000 constant CHILD-MS
create OUT CAP allot    variable OUT-U
create ERR CAP allot    variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

\ Stage `bin/hb --load test/aot-shadow-capture.f -- <mode>`; the paths follow.
: CHILD-ARGS ( ptr u8 n -- ) {: m:ptr mu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-shadow-capture.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   m mu >LEN PROC-ARGV+ ;

: RUN-CHILD ( -- )
   s" bin/hb" >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: SHOW-CHILD ( -- )
   s" aot-shadow-capture: child rc=" type RC @ . cr
   s" aot-shadow-capture: child stdout:" type cr OUT OUT-U @ type cr
   s" aot-shadow-capture: child stderr:" type cr ERR ERR-U @ type cr ;

\ The staged child exits with the reader's refusal code, saying `m` on stderr.
: REFUSED ( ptr u8 n -- ) {: m:ptr mu:n :}
   RUN-CHILD
   ERR ERR-U @ m mu CONTAINS? {: said:bool :}
   RC @ REFUSE-RC <> said 0= or if SHOW-CHILD then
   RC @ REFUSE-RC T=
   said TTRUE ;

\ The capturing child in `mode` completes its capture.
: CAPTURED ( ptr u8 n -- )
   CHILD-ARGS
   RUN-CHILD
   OUT OUT-U @ s" aot-shadow-capture: captured" CONTAINS? {: said:bool :}
   RC @ 0<> said 0= or if SHOW-CHILD then
   RC @ 0 T=
   said TTRUE ;

\ In the retired child KEEP, a public tier-0 word, calls GONER, which calls GONE:
\ both retired, so the capture ships neither, and no shipped routine names either.
\ Live code reaches GONE only through GONER, a retired body reached itself, and
\ GONE's map row comes first, so the refusal names GONE.
: LIVE-RETIRED-CASE ( -- )
   s" a retired word reached only from tier-0 code needs no x86 routine" T-LABEL
   s" retired" CAPTURED ;

\ In the alias child AOTSH-ALIAS:GONE, a public name no map row files, enters
\ GONE's body; nothing calls either. A shipped name is live, and the body it
\ shares is GONE's, so GONE's routine is live and no shipped row carries it.
: ALIAS-CASE ( -- )
   s" a public alias of a retired word carries the shared x86 routine" T-LABEL
   s" alias" CAPTURED ;

\ In the bridge child VIA, retired, calls BRIDGE, an ordinary private word, which
\ calls GONE. Nothing live reaches VIA, so nothing live reaches BRIDGE or GONE.
: BRIDGE-CASE ( -- )
   s" a retired word reached only through an ordinary word that only dead retired code reaches is dead, and the capture completes" T-LABEL
   s" bridge" CAPTURED ;

\ In the helper child GONEST, retired, calls HELP, a private word the capture
\ strips. ARM64 keeps GONEST's body as a gap and HELP's code with it, but
\ nothing live reaches GONEST, so nothing live reaches HELP.
: HELPER-CASE ( -- )
   s" a stripped private word only dead retired code calls is dead, and the capture completes" T-LABEL
   s" helper" CAPTURED ;

\ In the export child the public alias SHARED shares the body of the private
\ SHARED the capture strips. A shipped name is live, so the stripped routine is
\ live, and no shipped row carries it: the alias would ship with no routine.
: EXPORT-CASE ( -- )
   s" a public alias carries its private source's x86 routine without its name" T-LABEL
   s" export" CAPTURED ;

\ ---- forged artifacts -----------------------------------------------------------
\ One field of the live tables changed for one WRITE and changed back: the file is
\ the writer's own, with honest lengths and digests, so the field is the only
\ thing wrong with it.
: FORGE ( ptr u8 n ptr u8 n -- ) {: p:ptr v:n na:ptr nu:n :}
   p LE:U32@ {: was:n :}
   v p LE:U32!
   na nu FORGED IN-DIR FORGED-U !
   KEY FORGED FORGED-U @ AOT-FILE:WRITE
   was p LE:U32! ;

: READ-REFUSED ( ptr u8 n -- ) {: m:ptr mu:n :}
   s" read" CHILD-ARGS  FORGED FORGED-U @ >LEN PROC-ARGV+
   m mu REFUSED ;

\ The last routine ends where the shadow code does.
: SPAN-FORGED-CASE ( -- )
   s" READ refuses a shadow record whose routine runs one byte past the shadow code" T-LABEL
   W-SHOW ROW-OF {: r:n :}
   r 8 REC-AT  AOT-SHADOW:CODE-LEN @ r 4 REC@ - 1+  s" span.aot" FORGE
   s" aot-file: a shadow record's routine lies outside the shadow code" READ-REFUSED ;

\ AOT-REC-N is the count of shipped records, as the READ above restored it.
: SITE-FORGED-CASE ( -- )
   s" READ refuses a shadow call naming shipped record N of N shipped records" T-LABEL
   W-CALLER AOT-SHADOW:CALL SITE-IN 8 SITE-AT  AOT-REC-N @ REC-TARGET  s" site.aot" FORGE
   s" aot-file: a shadow site names neither a window record nor a pool entry" READ-REFUSED ;

: CELL-FORGED-CASE ( -- )
   s" READ refuses a shadow code cell keyed by shipped record N of N shipped records" T-LABEL
   0 4 XT-AT  AOT-REC-N @  s" cell.aot" FORGE
   s" aot-file: a shadow code cell names no window record" READ-REFUSED ;

\ The host is this capture written again with its shadow dropped, so the shadow
\ MERGE meets is the artifact's alone. It drops the tables, so it runs last.
: MERGE-CASE ( -- )
   s" MERGE refuses an artifact that carries a shadow, into a host that carries none" T-LABEL
   AOT-SHADOW:RESET
   s" host.aot" HOST IN-DIR HOST-U !
   KEY HOST HOST-U @ AOT-FILE:WRITE
   s" merge" CHILD-ARGS  HOST HOST-U @ >LEN PROC-ARGV+  ART$ >LEN PROC-ARGV+
   s" aot-file: a merge carries no shadow target's routines" REFUSED ;

\ The child: READ an artifact, or READ a host and MERGE an artifact into it.
: CHILD ( -- )
   IDENT!
   0 SCRIPT-ARGV$ s" read" STR=  SCRIPT-ARGC 2 = and if
      KEY 1 SCRIPT-ARGV$ AOT-FILE:READ
      s" aot-shadow-capture: read=ok" type cr exit
   then
   0 SCRIPT-ARGV$ s" merge" STR=  SCRIPT-ARGC 3 = and if
      KEY 1 SCRIPT-ARGV$ AOT-FILE:READ
      KEY 2 SCRIPT-ARGV$ AOT-FILE:MERGE
      s" aot-shadow-capture: merge=ok" type cr exit
   then
   s" aot-shadow-capture: expected no arguments, retired, alias, bridge, helper, export, read <artifact>, or merge <host> <artifact>"
   USAGE-RC die ;

\ What the capture carried, for a reader comparing runs.
: REPORT ( -- )
   s" aot-shadow-capture: recs=" type AOT-SHADOW:REC-N @ .
   s" code=" type AOT-SHADOW:CODE-LEN @ .
   s" sites=" type AOT-SHADOW:SITE-N @ .
   s" cells=" type AOT-SHADOW:XT-N @ . cr ;

public

: RUN ( -- )
   MODE$ nip 0<> if
      CAPTURE NSHADOW:CLOSE  s" aot-shadow-capture: captured" type cr exit
   then
   SCRIPT-ARGC 0 > if NSHADOW:CLOSE CHILD exit then
   CAPTURE
   NSHADOW:CLOSE
   T-RESET
   RECORDS-CASE
   CAST-CASE
   ANON-CASE
   STRIP-CASE
   LIVE-RETIRED-CASE
   ALIAS-CASE
   BRIDGE-CASE
   HELPER-CASE
   EXPORT-CASE
   CARRIED-CASE
   SPAN-CASE
   CALL-CASE
   DOES-CASE
   LITERAL-CASE
   PREFIX-CASE
   CELL-CASE
   ROUND-CASE
   SPAN-FORGED-CASE
   SITE-FORGED-CASE
   CELL-FORGED-CASE
   REPORT
   MERGE-CASE
   T-REPORT ;

;using
;package

AOTSH:RUN
