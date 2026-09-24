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
\ FIND-DICT's longest-run scan over a real multi-MB baked engine (IDT-TEST-BAKED-PC)
\ does far more work than a scan of the tiny synthetic images above, on top of
\ the same checked compilation; give it its own, larger budget.
240000 constant IDT-SELF-TIMEOUT-MS

\ Room for the ELF header a real image has ahead of its dict table
\ (tools/imgdump.f ELF-EHDR-BYTES), so these synthetic single-record images
\ exercise the same NO-SNAP-XTBASE path a baked engine does instead of
\ colliding with it.
128 constant IDT-HDR-BYTES
3 constant IDT-ET-DYN               \ e_type: ET_DYN, a PIE image
IDT-HDR-BYTES DREC + constant IDT-IMG-BYTES

\ The decoy fixture (IDT-WRITE-DECOY-IMG) puts a second, refused record ahead
\ of the real one. Its name length field declares far more bytes than the
\ record holds, and the file carries a tail long enough that ENT?'s in-file
\ bound still accepts it - that is what makes the scan reach PRN? at all - so
\ the only thing that can refuse the decoy is its first name byte.
$1000 constant IDT-DECOY-TAIL-BYTES
IDT-IMG-BYTES DREC + IDT-DECOY-TAIL-BYTES + constant IDT-DECOY-IMG-BYTES
4000 constant IDT-DECOY-NAME-LEN
1 constant IDT-DECOY-NAME-C         \ not printable ascii: PRN? refuses at byte 0

create IDT-OUT IDT-CAP allot
create IDT-ERR IDT-CAP allot
create IDT-IMG IDT-DECOY-IMG-BYTES allot
create IDT-PRN-BUF 3 allot
create IDT-ROOT FS-PATH-CAP allot
create IDT-A FS-PATH-CAP allot
create IDT-SAME FS-PATH-CAP allot
create IDT-SHIFT FS-PATH-CAP allot
create IDT-DIFF FS-PATH-CAP allot
create IDT-PIE FS-PATH-CAP allot
create IDT-NONELF FS-PATH-CAP allot
create IDT-DECOY FS-PATH-CAP allot

variable IDT-ROOT-U
variable IDT-A-U
variable IDT-SAME-U
variable IDT-SHIFT-U
variable IDT-DIFF-U
variable IDT-PIE-U
variable IDT-NONELF-U
variable IDT-DECOY-U

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

: IDT-NONELF$ ( -- ptr u8 n )
   IDT-NONELF IDT-NONELF-U @ ;

: IDT-DECOY$ ( -- ptr u8 n )
   IDT-DECOY IDT-DECOY-U @ ;

: IDT-ZERO ( n -- ) {: bytes :}
   bytes 0 ?do
      0 IDT-IMG i + c!
   loop ;

\ One dict record at `off`, with a single-byte inline name. A no-trailer image
\ stores slot 1 as the span's __text-relative END (tools/imgdump.f E-CODE-END),
\ so a (start, len) fixture writes start + len.
: IDT-REC-AT ( n n n n n -- ) {: off start len namelen ch :}
   start IDT-IMG off + !
   start len + IDT-IMG off + 8 + !
   namelen IDT-IMG off + 16 + !
   ch      IDT-IMG off + 24 + c! ;

\ The one dict record every single-record fixture below carries, DREC bytes
\ past the fake header.
: IDT-WRITE-RECORD ( ptr u8 n n n n -- ) {: path:ptr pathu start len ch :}
   IDT-HDR-BYTES start len 1 ch IDT-REC-AT
   path pathu IDT-IMG IDT-IMG-BYTES WRITE-ALL ;

\ An arm64 ELF64 header of the given e_type, with entry 0: the base
\ NO-SNAP-XTBASE reads from an accepted one is 0, so every offset assertion
\ below still reads back the value the fixture wrote. The magic goes down
\ first because it lands as a whole cell over e_ident; e_type and e_machine are
\ little-endian u16 whose values fit in the low byte.
: IDT-WRITE-ELF-HDR ( n -- ) {: etype :}
   IMAGE-DUMP:ELF-MAG        IDT-IMG IMAGE-DUMP:ELF-MAG-OFF + !
   IMAGE-DUMP:ELF-CLASS-64   IDT-IMG IMAGE-DUMP:ELF-CLASS-OFF + c!
   IMAGE-DUMP:ELF-EM-AARCH64 IDT-IMG IMAGE-DUMP:ELF-MACHINE-OFF + c!
   etype                     IDT-IMG IMAGE-DUMP:ELF-TYPE-OFF + c! ;

: IDT-WRITE-IMG ( ptr u8 n n n n -- )
   IDT-IMG-BYTES IDT-ZERO
   IMAGE-DUMP:ELF-ET-EXEC IDT-WRITE-ELF-HDR
   IDT-WRITE-RECORD ;

\ A PIE image: the loader picks its load base at exec time, so the file does
\ not carry the xt base at all.
: IDT-WRITE-PIE-IMG ( ptr u8 n n n n -- )
   IDT-IMG-BYTES IDT-ZERO
   IDT-ET-DYN IDT-WRITE-ELF-HDR
   IDT-WRITE-RECORD ;

\ Not an ELF, but carrying ET_EXEC's byte at e_type's offset: the one field
\ the check used to trust on its own.
: IDT-WRITE-NON-ELF-IMG ( ptr u8 n n n n -- )
   IDT-IMG-BYTES IDT-ZERO
   IMAGE-DUMP:ELF-ET-EXEC IDT-IMG IMAGE-DUMP:ELF-TYPE-OFF + c!
   IDT-WRITE-RECORD ;

\ A decoy record ahead of the real one, and nothing but its first name byte to
\ refuse it with: the length field declares IDT-DECOY-NAME-LEN bytes of name,
\ which the tail keeps inside the file, so ENT? runs the whole plausibility
\ check and reaches PRN?. FIND-DICT must anchor its run on the real record and
\ the dump must be the one a.img produces.
: IDT-WRITE-DECOY-IMG ( ptr u8 n -- ) {: path:ptr pathu :}
   IDT-DECOY-IMG-BYTES IDT-ZERO
   IMAGE-DUMP:ELF-ET-EXEC IDT-WRITE-ELF-HDR
   IDT-HDR-BYTES $100 $0c IDT-DECOY-NAME-LEN IDT-DECOY-NAME-C IDT-REC-AT
   IDT-HDR-BYTES DREC + $100 $0c 1 65 IDT-REC-AT
   path pathu IDT-IMG IDT-DECOY-IMG-BYTES WRITE-ALL ;

: IDT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-imgdump-test" HB-TMP-MKDIR IDT-ROOT IDT-ROOT-U IDT-COPY!
   IDT-ROOT$ CLEANUP-TREE+
   IDT-ROOT$ s" a.img" IDT-A JOIN-PATH IDT-A-U !
   IDT-ROOT$ s" same.img" IDT-SAME JOIN-PATH IDT-SAME-U !
   IDT-ROOT$ s" shift.img" IDT-SHIFT JOIN-PATH IDT-SHIFT-U !
   IDT-ROOT$ s" diff.img" IDT-DIFF JOIN-PATH IDT-DIFF-U !
   IDT-ROOT$ s" pie.img" IDT-PIE JOIN-PATH IDT-PIE-U !
   IDT-ROOT$ s" nonelf.img" IDT-NONELF JOIN-PATH IDT-NONELF-U !
   IDT-ROOT$ s" decoy.img" IDT-DECOY JOIN-PATH IDT-DECOY-U !
   IDT-A$ $100 $0c 65 IDT-WRITE-IMG
   IDT-SAME$ $100 $0c 65 IDT-WRITE-IMG
   IDT-SHIFT$ $120 $0c 65 IDT-WRITE-IMG
   IDT-DIFF$ $100 $10 66 IDT-WRITE-IMG
   IDT-PIE$ $100 $0c 65 IDT-WRITE-PIE-IMG
   IDT-NONELF$ $100 $0c 65 IDT-WRITE-NON-ELF-IMG
   IDT-DECOY$ IDT-WRITE-DECOY-IMG ;

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

\ Source for the child that pins PRN?'s read bound. MEM-ALLOC-GUARDED keeps an
\ inaccessible page past the capacity it hands back, so a span that starts on
\ the last readable byte and declares a whole page of name has exactly one byte
\ the scan may touch. That byte is not printable, so PRN? has its answer at
\ once; a scan that keeps going reads the guard page and the child dies on the
\ fault. Nothing here is timed - the pin is which bytes the scan may touch.
: IDT-PRN-GUARD-SRC$ ( -- ptr u8 n )
   S\" require tools/imgdump.f\nrequire lib/memory.f\n: PRN-GUARD ( -- )\n   STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED {: base:ptr cap:n :}\n   1 base cap 1 - BYTE+ c!\n   base cap 1 - BYTE+ cap IMAGE-DUMP:PRN? if s\q imgdump-test: PRN? read past the first non-printable byte\q 70 die then\n   s\q prn-guard ok\q type cr ;\nPRN-GUARD\n" ;

\ hb evaluates stdin only when no --load names a file, so this child compiles
\ imgdump from the source it is fed rather than through IDT-ARGV-BASE.
: IDT-RUN-STDIN ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   PROC-ARGV-RESET
   s" bin/hb" >LEN src srcu >LEN IDT-OUT IDT-CAP >LEN IDT-ERR IDT-CAP >LEN
   IDT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE IDT-CAPTURE>N ;

\ A word this engine carries live. Its record says both what to ask --pc and
\ what the answer has to be: a word baked into __text (EM-SEED-DICT) keeps a
\ stable address across runs of a fixed-base engine, and that address is
\ exactly what NO-SNAP-XTBASE reconstructs offline.
: IDT-REC ( ptr u8 n -- ptr n ) {: a:ptr u :}
   a u XREF-FIND {: r:ptr :}
   r XREF-FOUND? 0= if
      s" imgdump-test: word missing from own dictionary" 70 die
   then
   r ;

variable IDT-SLIDE
DYNAMIC-BUFFER IDT-SELF u8

: IDT-IMAGE-BASE ( -- )
   0 IDT-SLIDE !
   HB-TARGET-MACOS? 0= if exit then
   s" bin/hb" FILE-SIZE {: size:n :}
   size IDT-SELF-RESERVE
   s" bin/hb" 0 IDT-SELF size READ-ALL size T=
   0 IDT-SELF size MACHO-READ:OPEN
   rbase MACHO-READ:TEXT-VA - IDT-SLIDE !
   IDT-SELF-RELEASE ;

: IDT-XT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}   \ file-coordinate pc
   a u IDT-REC XREF-START IDT-SLIDE @ -
   SB-RESET FMT:SB-U SB$ ;

\ The whole line --pc must print for that word, in imgdump's own $hex spelling:
\ the start column is the xt that was asked for and the third is the code span
\ length, not the span's end address.
: IDT-PC-LINE+ ( ptr u8 n -- ) {: a:ptr u :}
   a u IDT-REC {: r:ptr :}
   a u SB-APPEND
   32 SB-APPEND-C
   r XREF-START IDT-SLIDE @ - IMAGE-DUMP:HEX$ SB-APPEND
   32 SB-APPEND-C
   r XREF-CODE-BYTES IMAGE-DUMP:HEX$ SB-APPEND
   10 SB-APPEND-C ;

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

\ The forged entry spans one instruction and the pc asked for is its start.
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

\ Pins both no-trailer fixes: --pc used to answer from FIND-DICT's raw,
\ un-rebased __text offsets, and then from a span end read as a length, which
\ made every record's window reach to the end of __text so the first one
\ answered nearly any pc. `+` is that first record; `evaluate` is not.
: IDT-TEST-BAKED-PC ( -- )
   s" imgdump --pc batches addresses on bin/hb itself" T-LABEL
   IDT-IMAGE-BASE
   IDT-ARGV-BASE
   s" --pc" IDT-ARG+ s" bin/hb" IDT-ARG+
   s" +" IDT-XT$ IDT-ARG+
   s" evaluate" IDT-XT$ IDT-ARG+
   s" bin/hb" >LEN IDT-OUT IDT-CAP >LEN IDT-ERR IDT-CAP >LEN
   IDT-SELF-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IDT-CAPTURE>N 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   SB-RESET s" +" IDT-PC-LINE+ s" evaluate" IDT-PC-LINE+
   IDT-OUT outu SB$ T$= ;

: IDT-TEST-NAMESPACE ( -- )
   s" imgdump namespace WIDs are not code addresses" T-LABEL
   IDT-IMG-BYTES IDT-ZERO
   IMAGE-DUMP:ELF-ET-EXEC IDT-WRITE-ELF-HDR
   $100000000 IDT-IMG IMAGE-DUMP:ELF-ENTRY-OFF + !
   IDT-HDR-BYTES 17 6 1 78 IDT-REC-AT
   XREF-NAMESPACE-WL IDT-IMG IDT-HDR-BYTES + 40 + !
   IDT-A$ IDT-IMG IDT-IMG-BYTES WRITE-ALL
   IDT-A$ IDT-RUN-1 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   IDT-OUT outu S\" N $11 $17\n" T$=
   \ Zero roles remain valid records in the same run as a following word.
   IDT-HDR-BYTES 0 0 1 78 IDT-REC-AT
   IDT-HDR-BYTES DREC + $100 $0c 1 65 IDT-REC-AT
   IDT-A$ IDT-IMG IDT-IMG-BYTES DREC + WRITE-ALL
   IDT-A$ IDT-RUN-1 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   IDT-OUT outu S\" N $0 $0\nA $100000100 $c\n" T$= ;

\ A no-trailer image whose base is not file-derivable must refuse by name
\ instead of guessing one.
: IDT-TEST-NO-BASE-REFUSES ( -- )
   s" imgdump refuses a no-trailer image with no fixed base" T-LABEL
   IDT-PIE$ IDT-RUN-1 74 T=
   {: outu erru :}
   outu 0 T=
   IDT-ERR erru s" refusing to guess" CONTAINS? TTRUE ;

\ e_type is not evidence on its own: a file that is no ELF at all can carry
\ ET_EXEC's byte at that offset, and the entry point read from it is fiction.
: IDT-TEST-NON-ELF-REFUSES ( -- )
   s" imgdump refuses a non-ELF image carrying ET_EXEC's byte" T-LABEL
   IDT-NONELF$ IDT-RUN-1 74 T=
   {: outu erru :}
   outu 0 T=
   IDT-ERR erru s" refusing to guess" CONTAINS? TTRUE ;

\ PRN?'s answer, which the read bound below must not change.
: IDT-TEST-PRN ( -- )
   s" imgdump PRN? accepts a printable span" T-LABEL
   s" Aword" IMAGE-DUMP:PRN? TTRUE
   s" imgdump PRN? accepts an empty span" T-LABEL
   IDT-PRN-BUF 0 IMAGE-DUMP:PRN? TTRUE
   s" imgdump PRN? refuses a non-printable first byte" T-LABEL
   IDT-DECOY-NAME-C IDT-PRN-BUF c!
   65 IDT-PRN-BUF 1 + c!
   65 IDT-PRN-BUF 2 + c!
   IDT-PRN-BUF 3 IMAGE-DUMP:PRN? TFALSE
   s" imgdump PRN? refuses a non-printable byte behind a printable one" T-LABEL
   65 IDT-PRN-BUF c!
   IDT-DECOY-NAME-C IDT-PRN-BUF 1 + c!
   65 IDT-PRN-BUF 2 + c!
   IDT-PRN-BUF 3 IMAGE-DUMP:PRN? TFALSE ;

\ FIND-DICT asks PRN? about every 4-byte offset of the image and E-L is a
\ 46-bit field, so a scan that does not stop at the first refused byte costs a
\ pass over the rest of the file per candidate: 130 s of CPU for one --pc of
\ bin/hb, twice per run, which is what overran this suite's own child deadline
\ under a loaded pool. The guarded span pins the bound by memory, not by clock.
: IDT-TEST-PRN-BOUND ( -- )
   s" imgdump PRN? reads no byte past the first non-printable one" T-LABEL
   IDT-PRN-GUARD-SRC$ IDT-RUN-STDIN 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu s" prn-guard ok" CONTAINS? TTRUE ;

\ The same refusal through the real load path: a record-shaped decoy whose
\ declared name is long and whose first name byte is not printable is no
\ dictionary anchor, and the dump is the one a.img's single record produces.
: IDT-TEST-DECOY ( -- )
   s" imgdump skips a record whose name is not printable" T-LABEL
   IDT-DECOY$ IDT-RUN-1 0 T=
   {: outu erru :}
   erru 0 T=
   IDT-OUT outu S\" A $100 $c\n" T$= ;

: IDT-MAIN ( -- )
   T-RESET
   IDT-PREPARE
   IDT-TEST-PRN
   IDT-TEST-PRN-BOUND
   IDT-TEST-DECOY
   IDT-TEST-DUMP
   IDT-TEST-IDENTICAL
   IDT-TEST-SHIFT
   IDT-TEST-DIFF
   IDT-TEST-PARSERS
   IDT-TEST-BAKED-PC
   IDT-TEST-NO-BASE-REFUSES
   IDT-TEST-NON-ELF-REFUSES
   IDT-TEST-NAMESPACE
   CLEANUP-RUN
   T-REPORT
   s" imgdump-test: ok" type cr ;

IDT-MAIN

;package
