\ aot-shadow-capture.f - a capture taken with an x86-64 shadow open carries the
\ shadow's routines (src/habu/aot-shadow.f into src/habu/aot-decl.f AOT-SHADOW),
\ and src/habu/aot-file.f carries them through an artifact unchanged.
\
\ Registered as `SUITE aot-shadow-capture`.

\ The marks come first, then the window, then the artifact writer, for the
\ reasons test/aot-artifact-roundtrip.f gives.
package AOTSH
public
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
variable PRIVATE-WID
;package

require src/lib/string.f

package AOTSH
public
\ The capturing child: bin/hb --load test/aot-shadow-capture.f --
\   retired|alias|bridge|helper|export|closed
: MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   false ;
\ What a capturing child's window adds before GONE retires; each mode's reach is
\ stated at its case below.
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

require src/lib/le.f
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

\ The window holds one of each thing the shadow tables carry; created words,
\ `does>` children, casts and a defer carry routines too
\ (src/compiler/native/compiler.f SHADOW-FIXED compiles every fixed word at any
\ tier). A stripped private word and two retired ones sit between shadowed
\ records, so past them a record's window index, capture index and shipped row
\ all differ.
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
private get-current AOTSH:PRIVATE-WID !
: MAKER ( n -- ) create , does> ( -- n ) @ 1+ ; public
5 MAKER MADE
: QUOT ( -- [ n -- n ] ) [: 1 + ;] ;
: TICK ( -- [ n n -- n ] ) ['] LEAF ;
: SHOW ( n -- ) . ;
CAST: >CAP-IDX ( n -- idx )
CAST: CAP-IDX>N ( idx -- n )
: CAST-USER ( n -- n ) >CAP-IDX CAP-IDX>N 1+ ;
0 set-tier

defer HOOK ( n n -- n )
: ARM-HOOK ( -- ) ['] LEAF is HOOK ;
ARM-HOOK

;package

AOT-ARM:WINDOW-CLOSE

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/habu/fdio.f
require src/habu/address-carrier.f
require src/habu/aot-file.f
require test/aot-shadow-key.f
require test/pcap-result.f

package AOTSH
using AOT-BUF

32 constant SHA-BYTES
create DIR PATH-CAP allot    variable DIR-U       \ where the artifacts go
create ART PATH-CAP allot    variable ART-U       \ the capture's own
create FORGED PATH-CAP allot    variable FORGED-U \ one with a field changed
create HOST PATH-CAP allot    variable HOST-U     \ the capture without its shadow
create SHA-CTX SHA256-CTX-BYTES allot
create DIGESTS SHA-BYTES 4 * allot   \ the four tables' digests before the round trip
create REREAD SHA-BYTES allot
create FIRST SHA-BYTES allot         \ the first WRITE's file digest

: ART$ ( -- ptr u8 n ) ART ART-U @ ;

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
: WID-OF ( n -- n ) CREC 16 + LE:U32@ ;
\ One name may ship from two wordlists.
: SHIPPED-IN ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   -1
   AOT-REC-N @ 0 ?do
      AOT-NAMES-BUF@ i CREC 8 + LE:U32@ + {: e:ptr :}
      e 1+ e c@ a u STR=  i WID-OF wid = and if drop i leave then
   loop ;
: W-CELL ( -- n ) s" WCELL" SHIPPED ;
: W-PEEK ( -- n ) s" PEEK" SHIPPED ;
: W-LEAF ( -- n ) s" LEAF" SHIPPED ;
: W-CALLER ( -- n ) s" CALLER" SHIPPED ;
: W-CONST ( -- n ) s" CONST" SHIPPED ;
: W-DOES ( -- n ) s" CONST;does" SHIPPED ;
: W-MAKER ( -- n ) s" MAKER" SHIPPED ;
: W-MAKER-DOES ( -- n ) s" MAKER;does" SHIPPED ;
: W-MADE ( -- n ) s" MADE" SHIPPED ;
: W-QUOT ( -- n ) s" QUOT" SHIPPED ;
: W-TICK ( -- n ) s" TICK" SHIPPED ;
: W-SHOW ( -- n ) s" SHOW" SHIPPED ;
: W-CAST-IN ( -- n ) s" >CAP-IDX" SHIPPED ;
: W-CAST-OUT ( -- n ) s" CAP-IDX>N" SHIPPED ;
: W-CAST-USER ( -- n ) s" CAST-USER" SHIPPED ;
: W-HOOK ( -- n ) s" HOOK" SHIPPED ;

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

: ROW-OF ( n -- n ) {: w:n :}
   -1
   AOT-SHADOW:REC-N @ 0 ?do
      i 0 REC@ w = if drop i leave then
   loop ;

: AT-OF ( n -- n ) ROW-OF 4 REC@ ;
: LEN-OF ( n -- n ) ROW-OF 8 REC@ ;

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

: WCELL-OFF ( -- n )
   AOTSH-WINDOW:WCELL BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N +  AOT-ARM:D0 @ - ;

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:TARGET-CAPTURE ;

: RECORDS-CASE ( -- )
   s" every shipped tier-1 definition and fixed word is one shadow record in window order, a does> companion beside its definer" T-LABEL
   AOT-SHADOW:REC-N @ 16 T=
   W-CELL ROW-OF 0 T=
   W-PEEK ROW-OF 1 T=
   W-LEAF ROW-OF 2 T=
   W-CALLER ROW-OF 3 T=
   W-CONST ROW-OF 4 T=
   W-DOES ROW-OF 5 T=
   W-MAKER ROW-OF 6 T=
   W-MAKER-DOES ROW-OF 7 T=
   W-MADE ROW-OF 8 T=
   W-QUOT ROW-OF 9 T=
   W-TICK ROW-OF 10 T=
   W-SHOW ROW-OF 11 T=
   W-CAST-IN ROW-OF 12 T=
   W-CAST-OUT ROW-OF 13 T=
   W-CAST-USER ROW-OF 14 T=
   W-HOOK ROW-OF 15 T= ;

: CAST-CASE ( -- )
   s" checked cast declarations have recorded x86 identity routines" T-LABEL
   W-CAST-IN LEN-OF 0 > TTRUE
   W-CAST-OUT LEN-OF 0 > TTRUE ;

: STRIP-CASE ( -- )
   s" a dead private word and two dead retired ones between two shadowed records ship no record and no routine, and the record after them ships one row past PEEK, four window records on" T-LABEL
   s" HIDDEN" SHIPPED -1 T=
   s" GONE" SHIPPED -1 T=
   s" GONER" SHIPPED -1 T=
   W-LEAF W-PEEK 1+ T=
   s" AOTSH-WINDOW:LEAF" XREF-FIND-INDEX  s" AOTSH-WINDOW:PEEK" XREF-FIND-INDEX 4 +  T= ;

: CARRIED-CASE ( -- )
   s" a live private definer and its does> companion keep private records over one emission, which its child's routine enters" T-LABEL
   W-MAKER ROW-OF {: parent:n :}
   parent 0 >= TTRUE
   W-MAKER CREC 16 + LE:U32@  PRIVATE-WID @ AOT-ARM:W0 @ - 1+ T=
   W-MAKER CREC 16 + LE:U32@  W-MAKER-DOES CREC 16 + LE:U32@ T=
   W-MAKER-DOES ROW-OF {: r:n :}
   r 0 >= TTRUE
   r 4 REC@ parent 4 REC@ T=
   r 8 REC@ parent 8 REC@ T=
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
   W-LEAF SITES-IN 0 T=
   AOT-SHADOW:CODE-LEN @ W-HOOK AT-OF W-HOOK LEN-OF + 2 cells + T= ;

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

: TABLE$ ( n -- ptr u8 n ) {: k:n :}
   k 0 = if AOT-SHADOW:REC-BUF@ AOT-SHADOW:REC-N @ AOT-SHADOW:REC-ROW * exit then
   k 1 = if AOT-SHADOW:CODE-BUF@ AOT-SHADOW:CODE-LEN @ exit then
   k 2 = if AOT-SHADOW:SITE-BUF@ AOT-SHADOW:SITE-N @ AOT-SHADOW:SITE-ROW * exit then
   AOT-SHADOW:XT-BUF@ AOT-SHADOW:XT-N @ AOT-SHADOW:XT-ROW * ;

: DIGEST ( n ptr u8 -- ) {: k:n out:ptr :}
   SHA-CTX SHA256-BEGIN
   SHA-CTX k TABLE$ SHA256-FEED
   SHA-CTX out SHA256-END ;

: IN-DIR ( ptr u8 n ptr u8 -- n ) {: na:ptr nu:n dst:ptr :}
   DIR DIR-U @ na nu dst FS:JOIN-PATH ;

: ARTIFACT! ( -- )
   s" habu-aot-shadow-capture" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u FS:CLEANUP-TREE+
   a DIR u BYTE-COPY  u DIR-U !
   s" shadow.aot" ART IN-DIR ART-U !
   AOTSH-KEY:IDENT! ;

: ROUND-CASE ( -- )
   s" WRITE and READ carry the four shadow tables byte for byte, and a second WRITE is the same file" T-LABEL
   ARTIFACT!
   4 0 ?do i DIGESTS i SHA-BYTES * + DIGEST loop
   AOTSH-KEY:KEY ART$ AOT-FILE:WRITE
   AOT-FILE:SHA$ drop FIRST SHA-BYTES BYTE-COPY
   AOT-SHADOW:RESET
   AOTSH-KEY:KEY ART$ AOT-FILE:READ
   AOT-SHADOW:REC-N @ 16 T=
   4 0 ?do
      i REREAD DIGEST
      REREAD SHA-BYTES DIGESTS i SHA-BYTES * + SHA-BYTES T$=
   loop
   AOTSH-KEY:KEY ART$ AOT-FILE:WRITE
   AOT-FILE:SHA$ FIRST SHA-BYTES T$= ;

: CLOSED-ROUND-CASE ( -- )
   s" capture without a shadow writes and reads empty shadow sections" T-LABEL
   ARTIFACT!
   AOTSH-KEY:KEY ART$ AOT-FILE:WRITE
   AOT-SHADOW:RESET
   AOTSH-KEY:KEY ART$ AOT-FILE:READ
   4 0 ?do i TABLE$ nip 0 T= loop ;

\ src/habu/aot-file.f's DIE: this exit code, its sentence on stderr.
$4B constant REFUSE-RC
64 constant USAGE-RC                \ sysexits EX_USAGE
$8000 constant CAP
60000 constant CHILD-MS
create OUT CAP allot    variable OUT-U
create ERR CAP allot    variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

: CHILD-ARGS ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n m:ptr mu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   f fu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   m mu >LEN PROC-ARGV+ ;

\ The child inherits this process's environment, so the artifact it makes
\ lies under the TMPDIR this run was given.
: RUN-CHILD ( -- )
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: SHOW-CHILD ( -- )
   s" aot-shadow-capture: child rc=" type RC @ . cr
   s" aot-shadow-capture: child stdout:" type cr OUT OUT-U @ type cr
   s" aot-shadow-capture: child stderr:" type cr ERR ERR-U @ type cr ;

: REFUSED ( ptr u8 n -- ) {: m:ptr mu:n :}
   RUN-CHILD
   ERR ERR-U @ m mu CONTAINS? {: said:bool :}
   RC @ REFUSE-RC <> said 0= or if SHOW-CHILD then
   RC @ REFUSE-RC T=
   said TTRUE ;

: CAPTURED ( ptr u8 n -- )
   {: m:ptr mu:n :}
   s" test/aot-shadow-capture.f" m mu CHILD-ARGS
   RUN-CHILD
   OUT OUT-U @ s" aot-shadow-capture: captured" CONTAINS? {: said:bool :}
   RC @ 0<> said 0= or if SHOW-CHILD then
   RC @ 0 T=
   said TTRUE ;

\ In the retired child KEEP, a public tier-0 word, calls GONER, which calls GONE:
\ both retired, so the capture ships neither. KEEP has no x86 routine, so no x86
\ edge reaches GONER or GONE.
: LIVE-RETIRED-CASE ( -- )
   s" a retired word reached only from tier-0 code needs no x86 routine" T-LABEL
   s" retired" CAPTURED ;

\ In the alias child AOTSH-ALIAS:GONE, a public name no map row files, enters
\ GONE's body; nothing calls either. A shipped name is live, and the body it
\ shares is GONE's, so the alias's shipped row carries GONE's routine.
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

\ In the export child the public alias SHARED enters the private SHARED's body.
\ Both names are live, so both ship, and their shadow rows carry the one
\ emission. The child asserts it in EXPORT-SHIPPED-CASE.
: EXPORT-CASE ( -- )
   s" a private body with a public EXPORT alias ships both names over one routine" T-LABEL
   s" export" CAPTURED ;

\ PEEK is public in the window, so its wordlist is the alias's.
: EXPORT-SHIPPED-CASE ( -- )
   s" the private SHARED and its public EXPORT alias ship under their own wordlists, their shadow rows over one routine" T-LABEL
   s" SHARED" PRIVATE-WID @ AOT-ARM:W0 @ - 1+ SHIPPED-IN {: priv:n :}
   s" SHARED" W-PEEK WID-OF SHIPPED-IN {: pub:n :}
   priv 0 >= TTRUE
   pub 0 >= TTRUE
   priv ROW-OF 0 >= TTRUE
   pub ROW-OF 0 >= TTRUE
   priv LEN-OF 0 > TTRUE
   priv AT-OF pub AT-OF T=
   priv LEN-OF pub LEN-OF T= ;

\ One field of the live tables changed for one WRITE and changed back: the file is
\ the writer's own, with honest lengths and digests, so the field is the only
\ thing wrong with it.
: FORGE ( ptr u8 n ptr u8 n -- ) {: p:ptr v:n na:ptr nu:n :}
   p LE:U32@ {: was:n :}
   v p LE:U32!
   na nu FORGED IN-DIR FORGED-U !
   AOTSH-KEY:KEY FORGED FORGED-U @ AOT-FILE:WRITE
   was p LE:U32! ;

\ A READ refusal ends the process, so test/aot-shadow-reader.f reads in a child.
\ The old-version refusal is test/aot-chain-capture-suite.f's; a callee with no
\ shipped record is test/x86-64-link-records.f's callee child.
: READ-REFUSED ( ptr u8 n -- ) {: m:ptr mu:n :}
   s" test/aot-shadow-reader.f" s" read" CHILD-ARGS  FORGED FORGED-U @ >LEN PROC-ARGV+
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
   AOTSH-KEY:KEY HOST HOST-U @ AOT-FILE:WRITE
   s" test/aot-shadow-reader.f" s" merge" CHILD-ARGS  HOST HOST-U @ >LEN PROC-ARGV+  ART$ >LEN PROC-ARGV+
   s" aot-file: a merge carries no shadow target's routines" REFUSED ;

: REPORT ( -- )
   s" aot-shadow-capture: recs=" type AOT-SHADOW:REC-N @ .
   s" code=" type AOT-SHADOW:CODE-LEN @ .
   s" sites=" type AOT-SHADOW:SITE-N @ .
   s" cells=" type AOT-SHADOW:XT-N @ . cr ;

public

: RUN ( -- )
   s" closed" MODE? if
      NSHADOW:CLOSE
      CAPTURE
      T-RESET
      CLOSED-ROUND-CASE
      T-REPORT
      s" aot-shadow-capture: captured" type cr exit
   then
   s" export" MODE? if
      CAPTURE
      NSHADOW:CLOSE
      T-RESET
      EXPORT-SHIPPED-CASE
      T-REPORT
      s" aot-shadow-capture: captured" type cr exit
   then
   MODE$ nip 0<> if
      CAPTURE NSHADOW:CLOSE  s" aot-shadow-capture: captured" type cr exit
   then
   SCRIPT-ARGC 0 > if
      s" aot-shadow-capture: expected no arguments, retired, alias, bridge, helper, export or closed"
      USAGE-RC die
   then
   CAPTURE
   NSHADOW:CLOSE
   T-RESET
   RECORDS-CASE
   CAST-CASE
   STRIP-CASE
   LIVE-RETIRED-CASE
   ALIAS-CASE
   BRIDGE-CASE
   HELPER-CASE
   EXPORT-CASE
   s" closed" CAPTURED
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
