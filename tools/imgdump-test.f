\ imgdump-test.f - checked fixture coverage for tools/imgdump.f compare mode.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/imgdump.f
\ tools/imgdump-test.f

$4000 constant IDT-CAP
5000 constant IDT-TIMEOUT-MS

create IDT-OUT IDT-CAP allot
create IDT-ERR IDT-CAP allot
create IDT-IMG DREC allot
create IDT-ROOT FS-PATH-CAP allot
create IDT-A FS-PATH-CAP allot
create IDT-SAME FS-PATH-CAP allot
create IDT-SHIFT FS-PATH-CAP allot
create IDT-DIFF FS-PATH-CAP allot

variable IDT-ROOT-U
variable IDT-A-U
variable IDT-SAME-U
variable IDT-SHIFT-U
variable IDT-DIFF-U

: IDT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: IDT-ROOT$ ( -- ptr u8 n )
   IDT-ROOT IDT-ROOT-U @ ;

: IDT-A$ ( -- ptr u8 n )
   IDT-A IDT-A-U @ ;

: IDT-SAME$ ( -- ptr u8 n )
   IDT-SAME IDT-SAME-U @ ;

: IDT-SHIFT$ ( -- ptr u8 n )
   IDT-SHIFT IDT-SHIFT-U @ ;

: IDT-DIFF$ ( -- ptr u8 n )
   IDT-DIFF IDT-DIFF-U @ ;

: IDT-ZERO ( -- )
   DREC 0 ?do
      0 IDT-IMG i + c!
   loop ;

: IDT-WRITE-IMG ( ptr u8 n n n n -- ) {: path:ptr pathu start len ch :}
   IDT-ZERO
   start IDT-IMG !
   len IDT-IMG 8 + !
   1 IDT-IMG 16 + !
   ch IDT-IMG 24 + c!
   path pathu IDT-IMG DREC WRITE-ALL ;

: IDT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-imgdump-test" TMPDIR-MKDIR IDT-ROOT IDT-ROOT-U IDT-COPY!
   IDT-ROOT$ CLEANUP-TREE+
   IDT-ROOT$ s" a.img" IDT-A JOIN-PATH IDT-A-U !
   IDT-ROOT$ s" same.img" IDT-SAME JOIN-PATH IDT-SAME-U !
   IDT-ROOT$ s" shift.img" IDT-SHIFT JOIN-PATH IDT-SHIFT-U !
   IDT-ROOT$ s" diff.img" IDT-DIFF JOIN-PATH IDT-DIFF-U !
   IDT-A$ $100 $0c 65 IDT-WRITE-IMG
   IDT-SAME$ $100 $0c 65 IDT-WRITE-IMG
   IDT-SHIFT$ $120 $0c 65 IDT-WRITE-IMG
   IDT-DIFF$ $100 $10 66 IDT-WRITE-IMG ;

: IDT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: IDT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" --load" IDT-ARG+
   s" tools/imgdump.f" IDT-ARG+
   s" --" IDT-ARG+ ;

: IDT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: IDT-RUN-1 ( ptr u8 n -- n n n ) {: a:ptr u :}
   IDT-ARGV-BASE
   a u IDT-ARG+
   s" bin/hb"  >LEN IDT-OUT IDT-CAP >LEN IDT-ERR IDT-CAP >LEN
   IDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IDT-CAPTURE>N ;

: IDT-RUN-2 ( ptr u8 n ptr u8 n -- n n n ) {: a:ptr au b:ptr bu :}
   IDT-ARGV-BASE
   a au IDT-ARG+
   b bu IDT-ARG+
   s" bin/hb"  >LEN IDT-OUT IDT-CAP >LEN IDT-ERR IDT-CAP >LEN
   IDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IDT-CAPTURE>N ;

: IDT-TEST-DUMP ( -- )
   IDT-A$ IDT-RUN-1 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" A $100 $c" CONTAINS? TTRUE ;

: IDT-TEST-IDENTICAL ( -- )
   IDT-A$ IDT-SAME$ IDT-RUN-2 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" identical dicts" CONTAINS? TTRUE ;

: IDT-TEST-SHIFT ( -- )
   IDT-A$ IDT-SHIFT$ IDT-RUN-2 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" word sizes identical; offsets shifted" CONTAINS? TTRUE
   IDT-OUT outu s" A $100 $c" CONTAINS? TTRUE
   IDT-OUT outu s" A $120 $c" CONTAINS? TTRUE ;

: IDT-TEST-DIFF ( -- )
   IDT-A$ IDT-DIFF$ IDT-RUN-2 1 T=
   {: outu erru :}
   IDT-OUT outu s" word size/name differences" CONTAINS? TTRUE
   IDT-OUT outu s" < A $c" CONTAINS? TTRUE
   IDT-OUT outu s" > B $10" CONTAINS? TTRUE
   IDT-ERR erru s" imgdump: dictionaries differ" CONTAINS? TTRUE ;

: IDT-MAIN ( -- )
   T-RESET
   IDT-PREPARE
   IDT-TEST-DUMP
   IDT-TEST-IDENTICAL
   IDT-TEST-SHIFT
   IDT-TEST-DIFF
   CLEANUP-RUN
   T-REPORT
   s" imgdump-test: ok" type cr ;

IDT-MAIN
