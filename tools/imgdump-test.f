\ imgdump-test.f - checked fixture coverage for tools/imgdump.f compare mode.
\ Run: bin/hb --load tools/imgdump-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/fmt.f
require tools/imgdump.f

package IMAGE-DUMP-TEST

$4000 constant IDT-CAP
60000 constant IDT-TIMEOUT-MS       \ includes checked compilation of imgdump
\ FIND-DICT's longest-run scan over a real multi-MB baked engine (IDT-RUN-PC-SELF)
\ does far more work than a scan of the tiny synthetic images above, on top of
\ the same checked compilation; give it its own, larger budget.
240000 constant IDT-SELF-TIMEOUT-MS

\ Padding standing in for the ELF header a real image has ahead of its dict
\ table (tools/imgdump.f ELF-TYPE-OFF/ELF-ENTRY-OFF), so these synthetic
\ single-record images exercise the same NO-SNAP-XTBASE path a baked engine
\ does instead of colliding with it: the record used to start at file offset
\ 0, which is also where imgdump now reads e_type/e_entry.
128 constant IDT-HDR-BYTES

create IDT-OUT IDT-CAP allot
create IDT-ERR IDT-CAP allot
create IDT-IMG IDT-HDR-BYTES DREC + allot
create IDT-ROOT FS-PATH-CAP allot
create IDT-A FS-PATH-CAP allot
create IDT-SAME FS-PATH-CAP allot
create IDT-SHIFT FS-PATH-CAP allot
create IDT-DIFF FS-PATH-CAP allot
create IDT-PIE FS-PATH-CAP allot

variable IDT-ROOT-U
variable IDT-A-U
variable IDT-SAME-U
variable IDT-SHIFT-U
variable IDT-DIFF-U
variable IDT-PIE-U

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

: IDT-PIE$ ( -- ptr u8 n )
   IDT-PIE IDT-PIE-U @ ;

: IDT-ZERO ( -- )
   IDT-HDR-BYTES DREC + 0 ?do
      0 IDT-IMG i + c!
   loop ;

\ The one dict record every fixture below carries, now DREC bytes past the
\ fake header instead of at file offset 0.
: IDT-WRITE-RECORD ( ptr u8 n n n n -- ) {: path:ptr pathu start len ch :}
   start IDT-IMG IDT-HDR-BYTES + !
   len   IDT-IMG IDT-HDR-BYTES + 8 + !
   1     IDT-IMG IDT-HDR-BYTES + 16 + !
   ch    IDT-IMG IDT-HDR-BYTES + 24 + c!
   path pathu IDT-IMG IDT-HDR-BYTES DREC + WRITE-ALL ;

\ A fixed-base (ET_EXEC) fake header with entry 0: NO-SNAP-XTBASE reads a real
\ base and imgdump adds it to every raw xt field, so entry 0 is a no-op and
\ every existing offset assertion below still reads the value it wrote.
: IDT-WRITE-IMG ( ptr u8 n n n n -- )
   IDT-ZERO
   IMAGE-DUMP:ELF-ET-EXEC IDT-IMG IMAGE-DUMP:ELF-TYPE-OFF + !
   IDT-WRITE-RECORD ;

\ e_type left 0 (not ET_EXEC): stands in for a PIE image or a non-ELF file,
\ where the real xt base is not in the file at all.
: IDT-WRITE-PIE-IMG ( ptr u8 n n n n -- )
   IDT-ZERO
   IDT-WRITE-RECORD ;

: IDT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-imgdump-test" TMPDIR-MKDIR IDT-ROOT IDT-ROOT-U IDT-COPY!
   IDT-ROOT$ CLEANUP-TREE+
   IDT-ROOT$ s" a.img" IDT-A JOIN-PATH IDT-A-U !
   IDT-ROOT$ s" same.img" IDT-SAME JOIN-PATH IDT-SAME-U !
   IDT-ROOT$ s" shift.img" IDT-SHIFT JOIN-PATH IDT-SHIFT-U !
   IDT-ROOT$ s" diff.img" IDT-DIFF JOIN-PATH IDT-DIFF-U !
   IDT-ROOT$ s" pie.img" IDT-PIE JOIN-PATH IDT-PIE-U !
   IDT-A$ $100 $0c 65 IDT-WRITE-IMG
   IDT-SAME$ $100 $0c 65 IDT-WRITE-IMG
   IDT-SHIFT$ $120 $0c 65 IDT-WRITE-IMG
   IDT-DIFF$ $100 $10 66 IDT-WRITE-IMG
   IDT-PIE$ $100 $0c 65 IDT-WRITE-PIE-IMG ;

: IDT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: IDT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" --load" IDT-ARG+
   s" tools/imgdump.f" IDT-ARG+
   s" --" IDT-ARG+ ;

: IDT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

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

: IDT-RUN-PC ( ptr u8 n -- n n n ) {: a:ptr u:n :}
   IDT-ARGV-BASE
   s" --pc" IDT-ARG+
   IDT-A$ IDT-ARG+
   a u IDT-ARG+
   s" bin/hb" >LEN IDT-OUT IDT-CAP >LEN IDT-ERR IDT-CAP >LEN
   IDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IDT-CAPTURE>N ;

\ Asks imgdump about bin/hb itself: a real baked engine with no snapshot
\ trailer, so this is the only fixture above that exercises NO-SNAP-XTBASE
\ against a genuine image rather than a synthetic one.
: IDT-RUN-PC-SELF ( ptr u8 n -- n n n ) {: a:ptr u:n :}
   IDT-ARGV-BASE
   s" --pc" IDT-ARG+
   s" bin/hb" IDT-ARG+
   a u IDT-ARG+
   s" bin/hb" >LEN IDT-OUT IDT-CAP >LEN IDT-ERR IDT-CAP >LEN
   IDT-SELF-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IDT-CAPTURE>N ;

\ The live xt of a word every engine carries, as decimal text for a --pc
\ argument: `+` is baked directly into __text (EM-SEED-DICT), so its address
\ is stable across runs of a fixed-base engine and is exactly what
\ NO-SNAP-XTBASE is meant to reconstruct offline.
: IDT-PLUS-XT$ ( -- ptr u8 n )
   s" +" XREF-FIND {: r:ptr :}
   r XREF-FOUND? 0= if
      s" imgdump-test: + missing from own dictionary" 70 die
   then
   r XREF-START
   SB-RESET FMT:SB-U SB$ ;

: IDT-TEST-DUMP ( -- )
   s" imgdump single image" T-LABEL
   IDT-A$ IDT-RUN-1 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" A $100 $c" CONTAINS? TTRUE ;

: IDT-TEST-IDENTICAL ( -- )
   s" imgdump identical images" T-LABEL
   IDT-A$ IDT-SAME$ IDT-RUN-2 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" identical dicts" CONTAINS? TTRUE ;

: IDT-TEST-SHIFT ( -- )
   s" imgdump shifted offsets" T-LABEL
   IDT-A$ IDT-SHIFT$ IDT-RUN-2 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" word sizes identical; offsets shifted" CONTAINS? TTRUE
   IDT-OUT outu s" A $100 $c" CONTAINS? TTRUE
   IDT-OUT outu s" A $120 $c" CONTAINS? TTRUE ;

: IDT-TEST-DIFF ( -- )
   s" imgdump name/size diff" T-LABEL
   IDT-A$ IDT-DIFF$ IDT-RUN-2 1 T=
   {: outu erru :}
   IDT-OUT outu s" word size/name differences" CONTAINS? TTRUE
   IDT-OUT outu s" < A $c" CONTAINS? TTRUE
   IDT-OUT outu s" > B $10" CONTAINS? TTRUE
   IDT-ERR erru s" imgdump: dictionaries differ" CONTAINS? TTRUE ;

\ The forged entry holds one instruction: a code length has to be whole
\ instructions (CODE-SPAN:CHECK), and the pc asked for is the entry's start.
: IDT-PC-OK ( ptr u8 n n -- ) {: a:ptr u:n value:n :}
   IDT-A$ value CODE-SPAN:INSN-BYTES 65 IDT-WRITE-IMG
   a u IDT-RUN-PC 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   IDT-OUT outu s" A " CONTAINS? TTRUE ;

: IDT-PC-BAD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u IDT-RUN-PC 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   IDT-ERR erru s" usage:" CONTAINS? TTRUE ;

: IDT-TEST-PARSERS ( -- )
   s" imgdump parses $hex" T-LABEL
   s" $ff" 255 IDT-PC-OK
   s" imgdump parses 0x hex" T-LABEL
   s" 0x10" 16 IDT-PC-OK
   s" imgdump parses decimal" T-LABEL
   s" 42" 42 IDT-PC-OK
   s" imgdump rejects malformed hex" T-LABEL
   s" $zz" IDT-PC-BAD
   s" imgdump rejects mixed decimal" T-LABEL
   s" 4x2" IDT-PC-BAD
   s" imgdump rejects empty number" T-LABEL
   s" " IDT-PC-BAD ;

\ Pins the fix for habu-make-imgdump-fail-f6797954: without this, --pc against
\ a baked engine with no snapshot trailer answered from FIND-DICT's raw,
\ un-rebased __text offsets instead of the live address `' +`/XREF-START
\ reports, so a real pc either missed or hit the wrong word.
: IDT-TEST-BAKED-PC ( -- )
   s" imgdump --pc on bin/hb itself (baked, no snapshot)" T-LABEL
   IDT-PLUS-XT$ IDT-RUN-PC-SELF 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" + " CONTAINS? TTRUE ;

\ The other half of the same fix: a no-trailer image whose base is not
\ file-derivable (here, e_type left unequal to ET_EXEC, standing in for a PIE
\ image) must refuse by name instead of guessing one.
: IDT-TEST-NO-BASE-REFUSES ( -- )
   s" imgdump refuses a no-trailer image with no fixed base" T-LABEL
   IDT-PIE$ IDT-RUN-1 74 T=
   {: outu erru :}
   outu 0 T=
   IDT-ERR erru s" refusing to guess" CONTAINS? TTRUE ;

: IDT-MAIN ( -- )
   T-RESET
   IDT-PREPARE
   IDT-TEST-DUMP
   IDT-TEST-IDENTICAL
   IDT-TEST-SHIFT
   IDT-TEST-DIFF
   IDT-TEST-PARSERS
   IDT-TEST-BAKED-PC
   IDT-TEST-NO-BASE-REFUSES
   CLEANUP-RUN
   T-REPORT
   s" imgdump-test: ok" type cr ;

IDT-MAIN

;package
