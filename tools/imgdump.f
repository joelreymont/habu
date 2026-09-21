\ imgdump.f — habu image inspector, in habu.
\ Run: bin/hb --load tools/imgdump.f -- <image> [image2]
\ Reads the image path argument, locates the snapshot trailer, maps the live region
\ payload, and prints one line per word: name $start $len (namespace rows
\ carry public/private WIDs instead). --pc accepts one or more addresses.
\ With two images, compares name+length first, then reports offset-only shifts.
\ Loads the target executable layout on demand; common dictionary layout is
\ already present in the native cold prefix.

require lib/adt/option.f                 \ option<n> for the number parsers (switchover wave A)
require src/habu/code-span.f

: IMG-FALSE ( -- bool )
   0 0= 0= ;

: IMG-LOAD-TARGET-LAYOUT ( -- )
   s" DATA-SIZE" XREF-FIND 0= if
      HB-TARGET-LINUX? if s" src/os/linux/layout.f" included exit then
      HB-TARGET-MACOS? if s" src/os/macos/layout.f" included exit then
      HB-TARGET-LINUX-X86-64? if
         s" src/os/linux-x86-64/layout.f" included exit then
      s" imgdump: unknown target" 74 die
   then ;

IMG-LOAD-TARGET-LAYOUT
undefine IMG-LOAD-TARGET-LAYOUT

package IMAGE-DUMP

TYPED-VARIABLE IB ptr u8                     \ image buffer
variable IL                                  \ image length
variable IFD
1024 constant IPATH-CAP
create IPATH IPATH-CAP 1 + allot
create ISTAT 144 allot
variable TOFF  variable IMG-TBASE  variable TNDICT  variable TREG  variable TDATA
variable ROFF  variable HAS-SNAP
variable RUNV  variable BESTO  variable BESTN
\ No-trailer only: the base added to a raw dict-record xt field. A snapshot's
\ xt fields are already canonical/absolute (SNAP-CORE?, PTR>OFF); a baked
\ image with no trailer stores the boot-seeded dictionary's xt fields
\ __text-relative instead (src/habu/habu2.f EM-SEED-DICT adds XREG-RBASE at
\ boot), so imgdump must add the same base back before treating them as
\ addresses. See NO-SNAP-XTBASE below.
variable XTBASE
variable HN  variable ISZ
variable A-N  variable CMP-NAME-LEN-DIFF  variable CMP-OFF-DIFF  variable CMP-IDX
TYPED-VARIABLE CMP-B0-P ptr u8
variable CMP-B0-U  variable CMP-B0-S  variable CMP-B0-L
TYPED-VARIABLE CMP-BAD-P ptr u8
variable CMP-BAD-IDX  variable CMP-BAD-U  variable CMP-BAD-L
variable PCV
variable NUM-I  variable NUM-ACC  variable NUM-DIG

\ TYPED-BUFFER's count is source-checked as a decimal literal.  DICT-CAP is
\ the same fixed image ABI value at runtime; keep the allocation and bounds
\ readers on that named constant and pin the declaration's numeric extent here.
65536 TYPED-BUFFER A-NAME-P-SLOT ptr u8   \ one dictionary name start per row
create A-NAME-U DICT-CAP cells allot
create A-START DICT-CAP cells allot
create A-LEN DICT-CAP cells allot

: IB@ ( -- ptr u8 )
   IB @ ;

: IB! ( ptr u8 -- )
   IB ! ;

\ Checked -1 validation refines the file mmap result; retire with
\ habu-builder-trust-rows-c5d41af6 when syscall-result refinement is typed.
TRUSTED: IMG-MMAP-PTR ( n -- ptr u8 )
   dup 0 < IF IFD @ close s" imgdump: mmap failed" 74 die THEN ;

: IMG-USAGE ( -- )
   s" usage: bin/hb --load tools/imgdump.f -- image [image2] | --pc image pc [pc ...] | --wid image name | --data image off" 64 die ;

: IMG-PATH$ ( -- ptr u8 n )
   SCRIPT-ARGC 1 < if IMG-USAGE then
   SCRIPT-ARGC 2 > if IMG-USAGE then
   0 SCRIPT-ARGV$ ;

: ZPATH {: a:ptr u d:ptr cap :} ( ptr u8 ptr n -- )
   u cap > if s" imgdump: path too long" 74 die then
   0 begin dup u < while  dup a + c@  over d + c!  1 + repeat drop  0 d u + c! ;

: READ-IMG-PATH ( ptr u8 n -- )
   IPATH IPATH-CAP ZPATH
   IPATH ISTAT stat64 0 < IF s" imgdump: stat failed" 74 die THEN
   ISTAT 96 + @ ISZ !
   ISZ @ 0 > 0= IF s" imgdump: empty image" 74 die THEN
   IPATH 0 0 open IFD !
   IFD @ 0 < IF s" imgdump: open failed" 74 die THEN
   0 ISZ @ 1 2 IFD @ 0 mmap
   IMG-MMAP-PTR IB!
   IFD @ close
   ISZ @ IL ! ;

: READ-IMG
   IMG-PATH$ READ-IMG-PATH ;

\ ---- hex printing ($-prefixed, lowercase) and char output ----
create EB 4 allot
: EMITC {: c :}  c EB c!  EB 1 type ;
: NIB {: n :}  n 10 < if n 48 + else n 87 + then ;
create HB 24 allot
variable HV  variable HP
public
\ The view lives in HB and dies at the next call. Public so
\ tools/imgdump-test.f can build the exact line the dump prints.
: HEX$ {: u :} ( n -- ptr u8 n )
   u HV !  20 HP !  0 HN !
   begin
     HP @ 1 - HP !
     HV @ 15 and NIB HB HP @ + c!
     HN @ 1 + HN !
     HV @ 16 / HV !
     HV @ 0 =
   until
   HP @ 1 - HP !  36 HB HP @ + c!
   HN @ 1 + HN !
   HB HP @ +  HN @ ;
private
: h. ( n -- )
   HEX$ type ;

\ ---- snapshot and dict entry fields ----
: I@ ( n -- n ) {: o :}
   IB@ o + {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or
   p 4 + c@ 32 lshift or
   p 5 + c@ 40 lshift or
   p 6 + c@ 48 lshift or
   p 7 + c@ 56 lshift or ;
: E-E {: o :} ( n -- n )
   o 8 + I@ ;
: E-F {: o :} ( n -- n )
   o 16 + I@ ;
: E-L {: o :} ( n -- n )
   o E-F DNAME-LEN-MASK and ;
: E-WID ( n -- n ) 40 + I@ ;
\ A namespace record's slots 0 and 1 are the package's public and private WID
\ roles, not a code span (src/habu/xref.f XREF-LEN), so no span is computed for
\ one and its slot 1 is reported as stored.
: E-CODE? {: o :} ( n -- bool )
   o E-WID XREF-NAMESPACE-WL <> ;

: E-S {: o :} ( n -- n )
   o I@  HAS-SNAP @ 0= o E-CODE? and if XTBASE @ + then ;

\ Dictionary slot 1 carries a different quantity in each image class: a
\ snapshot stores the live CODE-SPAN raw length, a baked no-trailer image the
\ seed record's __text-relative span end, which src/habu/habu2.f EM-SEED-DICT
\ turns into that length at boot (src/habu/xref.f XREF-LEN-SLOT). Both classes
\ answer the same span here; XREF-CODE-BYTES is the live counterpart.
: E-CODE-END {: o :} ( n -- n )                     \ code records only
   HAS-SNAP @ if o E-S o E-E CODE-SPAN:BYTES + exit then
   XTBASE @ o E-E + ;

: E-CODE-BYTES {: o :} ( n -- n )
   o E-CODE? 0= if o E-E exit then
   o E-CODE-END o E-S - ;

variable OKV
public
\ FIND-DICT asks this of every 4-byte offset in the image and E-L is a 46-bit
\ field (src/habu/layout.f DNAME-LEN-MASK), so a candidate whose length field
\ is large but still inside the file made this scan megabytes of bytes even
\ when byte 0 had already answered: `--pc` over the 5 MB bin/hb cost 130 s of
\ CPU, and two of those overran tools/imgdump-test.f's own child deadline
\ under a loaded test pool. The loop therefore stops at the first byte that is
\ not printable ASCII and reads no byte past it; the answer is the same one.
\ Public so tools/imgdump-test.f can pin that bound structurally, with a span
\ whose declared length runs off the end of a guarded mapping.
: PRN? {: a:ptr u :} ( ptr u8 n -- bool )     \ a..a+u all printable ascii?
   1 OKV !
   0 begin dup u < OKV @ 0<> and while
     dup a + c@ 32 >  OKV @ and
     over a + c@ 127 <  and  OKV !
     1 +
   repeat drop
   OKV @ ;
private
\ Validate the six 8-byte trailer fields (src/habu/layout.f owns the geometry).
: SNAP-CORE? {: o :} ( n -- bool )
   o SNAP-TRL-BYTES + IL @ > if IMG-FALSE exit then
   o I@ SNAP-MAGIC = 0= if IMG-FALSE exit then
   o SNAP-TRL-VERSION + I@ SNAP-FORMAT-VERSION = 0= if IMG-FALSE exit then
   o SNAP-TRL-NDICT + I@ 1 < if IMG-FALSE exit then
   o SNAP-TRL-NDICT + I@ DICT-CAP > if IMG-FALSE exit then
   o SNAP-TRL-REGLEN + I@ 0 <= if IMG-FALSE exit then
   o SNAP-TRL-REGLEN + I@ REGION > if IMG-FALSE exit then
   o SNAP-TRL-DATALEN + I@ 0 <= if IMG-FALSE exit then
   o SNAP-TRL-DATALEN + I@ DATA-SIZE > if IMG-FALSE exit then
   o SNAP-TRL-NDICT + I@ DREC *  o SNAP-TRL-REGLEN + I@ > if IMG-FALSE exit then
   0 0= ;

: SNAP? {: o :} ( n -- bool )
   o SNAP-CORE? 0= if IMG-FALSE exit then
   o SNAP-TRL-REGLEN + I@ o SNAP-TRL-DATALEN + I@ +  o > if IMG-FALSE exit then
   0 0= ;

: FIND-SNAPSHOT ( -- bool )
   -1 TOFF !
   IMAGE-TEXT-SIZE-OFF I@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - {: off:n :}
   off 0 < if IMG-FALSE exit then
   off SNAP? 0= if IMG-FALSE exit then
   off TOFF !
   0 0= ;
: LOAD-SNAPSHOT ( -- )
   FIND-SNAPSHOT 0= if 0 HAS-SNAP ! exit then
   -1 HAS-SNAP !
   TOFF @ SNAP-TRL-TBASE + I@ IMG-TBASE !
   TOFF @ SNAP-TRL-NDICT + I@ TNDICT !
   TOFF @ SNAP-TRL-REGLEN + I@ TREG !
   TOFF @ SNAP-TRL-DATALEN + I@ TDATA !
   TOFF @ TDATA @ - TREG @ - ROFF ! ;

: PTR>OFF {: p :} ( n -- n )
   HAS-SNAP @ 0= if -1 exit then
   p RBASE-VA >=  p RBASE-VA TREG @ + < and if p RBASE-VA - ROFF @ + exit then
   p IMG-TBASE @ >=  p IMG-TBASE @ ROFF @ CODE-OFF - + < and if p IMG-TBASE @ - CODE-OFF + exit then
   -1 ;
: E-NAME-OFF {: o :} ( n -- n )
   o E-F DNAME-EXT and 0= if o 24 + else o 24 + I@ PTR>OFF then ;
: E-NAME {: o :} ( n -- ptr u8 )
   o E-NAME-OFF dup 0 < if s" imgdump: bad external name pointer" 74 die then
   dup o E-L + IL @ > if s" imgdump: truncated name" 74 die then
   IB@ +  o E-L ;
\ AN EMPTY NAME IS A RECORD, NOT A CORRUPTION. habu-ship-no-dictionary-2fee2dea
\ shipped a record with an empty pool entry for every word nothing can ask for
\ by name, and habu-carry-code-spans-0db56c19 stopped shipping the row at all -
\ its code span travels in the payload's AOT-SPAN table instead. A current image
\ therefore holds no empty-named record, and every engine built between those
\ two does. So the length floor here stays 0: it is what a dump of one of those
\ engines walks over, and refusing it reported a shipped format as a broken
\ file. A name length is a record field, not a plausibility test.
: ENT? {: o :} ( n -- bool )
   o E-CODE? if
      o E-S 0 <= if IMG-FALSE exit then
      HAS-SNAP @ if o E-S PTR>OFF 0 < if IMG-FALSE exit then then
   else
      \ Namespace roles may be zero and are never code pointers.
      o E-S 0 < if IMG-FALSE exit then
   then
   o E-E 0 < if IMG-FALSE exit then                 \ the raw field, never the rebased span
   o E-L 0 < if IMG-FALSE exit then
   o E-NAME-OFF dup 0 < if drop 0 0= 0= exit then
   dup o E-L + IL @ > if drop 0 0= 0= exit then
   IB@ +  o E-L PRN? ;

\ The SCAN still needs a name. An image with no snapshot trailer has no header
\ saying where its dictionary is, so FIND-DICT looks for the longest run of
\ plausible records - and a printable name is most of what makes a record
\ plausible. A run may CONTAIN stripped rows, which is why RUN# counts with
\ ENT? above, but it has to be ANCHORED on a named one or a region of
\ span-shaped numbers could pass for a dictionary.
: ENT-NAMED? ( n -- bool ) {: o :}
   o ENT? 0= if IMG-FALSE exit then
   o E-L 1 >= ;

: RUN# {: o :} ( n -- n )
   0 RUNV !
   o begin dup IL @ DREC - <= while
      dup ENT? 0= if drop RUNV @ exit then
      RUNV @ 1 + RUNV !  DREC +
   repeat drop
   RUNV @ ;
: FIND-DICT ( -- )
   0 BESTO !  0 BESTN !
   0 begin dup IL @ DREC - <= while
      dup ENT-NAMED? if
         dup RUN# RUNV !
         RUNV @ BESTN @ > if dup BESTO ! RUNV @ BESTN ! then
      then
      4 +
   repeat drop
   BESTN @ 0= if s" imgdump: no dict found" 74 die then ;

\ ---- dump ----
: .ENT {: o :}
   o E-NAME type  32 EMITC
   o E-S h.  32 EMITC
   o E-CODE-BYTES h.  10 EMITC ;
: DICT-START ( -- n )
   HAS-SNAP @ if ROFF @ else BESTO @ then ;

: DICT-END ( -- n )
   HAS-SNAP @ if ROFF @ TNDICT @ DREC * + else BESTO @ BESTN @ DREC * + then ;

: DUMP-DICT
   DICT-START
   begin dup DICT-END < while
      dup ENT? 0= if s" imgdump: corrupt dict entry" 74 die then
      dup .ENT  DREC +
   repeat drop ;

: PC-HIT? ( n -- bool ) {: o :}
   o E-CODE? 0= if IMG-FALSE exit then
   o E-S PCV @ <=  PCV @ o E-CODE-END < and ;

: PC>DICT ( n -- )
   PCV !
   DICT-START
   begin dup DICT-END < while
      dup ENT? 0= if s" imgdump: corrupt dict entry" 74 die then
      dup PC-HIT? if .ENT exit then
      DREC +
   repeat drop
   s" imgdump: pc not found" 74 die ;

\ ---- no-trailer xt base ----
\ Elf64_Ehdr fields. The header of the image in hand is the only evidence of
\ its class; this tool's own build target is not a property of that file.
\ Public so tools/imgdump-test.f's synthetic no-trailer fixtures can build a
\ header that satisfies (or deliberately fails) this same check.
public
$00 constant ELF-MAG-OFF
$464C457F constant ELF-MAG    \ e_ident[0..4) = 7f 45 4c 46, little-endian
$04 constant ELF-CLASS-OFF
2  constant ELF-CLASS-64      \ EI_CLASS: ELFCLASS64
$10 constant ELF-TYPE-OFF
2  constant ELF-ET-EXEC       \ e_type: ET_EXEC, a fixed-base (non-PIE) image
$12 constant ELF-MACHINE-OFF
\ e_machine: EM_AARCH64. The entry-point-is-XREG-RBASE rule below is the ARM64
\ startup's, so this stays the only machine admitted; an x86_64 image is refused
\ by name until that startup exists (dot habu-cross-build-the-d25a959d).
183 constant ELF-EM-AARCH64
$18 constant ELF-ENTRY-OFF    \ e_entry: XREG-RBASE for a fixed-base image (see below)
$40 constant ELF-EHDR-BYTES
private

\ Without a snapshot trailer, the only part of the dictionary imgdump can
\ locate offline is the boot-seeded table src/habu/habu2.f EM-SEED-DICT reads
\ from the baked LDICT blob (found by FIND-DICT's longest-run scan): every
\ other word is JIT-compiled into a region the kernel places at boot and is
\ unrecoverable from a static file. EM-SEED-DICT rebases each seed record's
\ xt by XREG-RBASE, a register loaded by `ADR x20, LANCHOR` at the image's
\ entry (src/habu/habu2.f EM-RUNTIME-STACK) -- PC-relative, so for a
\ fixed-base (ET_EXEC, non-PIE) arm64 executable it is a build-time constant
\ equal to the image's own ELF entry point, readable straight from the file.
\ A PIE image's load base is chosen by the loader at exec time and is not in
\ the file at all, and a file that is not this kind of ELF has no such field to
\ read, so refuse rather than guess one.
: ELF-FIXED-BASE? ( -- bool )
   IL @ ELF-EHDR-BYTES < if IMG-FALSE exit then
   ELF-MAG-OFF I@ $FFFFFFFF and ELF-MAG <> if IMG-FALSE exit then
   ELF-CLASS-OFF I@ $FF and ELF-CLASS-64 <> if IMG-FALSE exit then
   ELF-MACHINE-OFF I@ $FFFF and ELF-EM-AARCH64 <> if IMG-FALSE exit then
   ELF-TYPE-OFF I@ $FFFF and ELF-ET-EXEC = ;

: NO-SNAP-XTBASE ( -- n )
   ELF-FIXED-BASE? 0= if
      s" imgdump: no snapshot trailer and image is not a fixed-base arm64 ELF executable; refusing to guess the dictionary base" 74 die
   then
   ELF-ENTRY-OFF I@ ;

: PREP-IMG ( -- )
   LOAD-SNAPSHOT
   HAS-SNAP @ 0= if NO-SNAP-XTBASE XTBASE !  FIND-DICT then ;

: A-NAME-P! ( ptr u8 n -- ) A-NAME-P-SLOT ! ;
: A-NAME-U! ( n n -- ) cells A-NAME-U + ! ;
: A-START! ( n n -- ) cells A-START + ! ;
: A-LEN! ( n n -- ) cells A-LEN + ! ;
: A-NAME-P@ ( n -- ptr u8 ) A-NAME-P-SLOT @ ;
: A-NAME-U@ ( n -- n ) cells A-NAME-U + @ ;
: A-START@ ( n -- n ) cells A-START + @ ;
: A-LEN@ ( n -- n ) cells A-LEN + @ ;

: IMG-HEX-DIGIT? ( n -- option<n> ) {: c:n :}   \ SOME hex digit value, else NONE
   c 48 >= c 57 <= and if c 48 - OPTION:SOME exit then
   c 65 >= c 70 <= and if c 55 - OPTION:SOME exit then
   c 97 >= c 102 <= and if c 87 - OPTION:SOME exit then
   OPTION:NONE ;

: DEC-DIGIT? ( n -- option<n> ) {: c:n :}   \ SOME decimal digit value, else NONE
   c 48 >= c 57 <= and if c 48 - OPTION:SOME exit then
   OPTION:NONE ;

: HEX-BODY ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u :}
   u 1 > if a c@ 36 = if a 1 + u 1 - 0 0= exit then then
   u 2 > if a c@ 48 = a 1 + c@ 120 = and if a 2 + u 2 - 0 0= exit then then
   a u 0 0= 0= ;

: PARSE-HEX ( ptr u8 n -- option<n> ) {: a:ptr u:n :}   \ SOME parsed hex value, else NONE
   u 0= if OPTION:NONE exit then
   0 NUM-I !
   0 NUM-ACC !
   begin NUM-I @ u < while
      a NUM-I @ + c@ IMG-HEX-DIGIT? MATCH option
        none OF OPTION:NONE exit ENDOF
        some OF NUM-DIG ! ENDOF
      ;MATCH
      NUM-ACC @ 16 * NUM-DIG @ + NUM-ACC !
      NUM-I @ 1 + NUM-I !
   repeat
   NUM-ACC @ OPTION:SOME ;

: PARSE-DEC ( ptr u8 n -- option<n> ) {: a:ptr u:n :}   \ SOME parsed decimal value, else NONE
   u 0= if OPTION:NONE exit then
   0 NUM-I !
   0 NUM-ACC !
   begin NUM-I @ u < while
      a NUM-I @ + c@ DEC-DIGIT? MATCH option
        none OF OPTION:NONE exit ENDOF
        some OF NUM-DIG ! ENDOF
      ;MATCH
      NUM-ACC @ 10 * NUM-DIG @ + NUM-ACC !
      NUM-I @ 1 + NUM-I !
   repeat
   NUM-ACC @ OPTION:SOME ;

: IMG>NUMBER? ( ptr u8 n -- option<n> )   \ SOME parsed $hex/0xhex/decimal, else NONE
   HEX-BODY if PARSE-HEX exit then
   PARSE-DEC ;

: CAPTURE-A-ENTRY {: o idx :} ( n n -- )
   idx DICT-CAP >= if s" imgdump: too many dict entries" 74 die then
   o E-NAME idx A-NAME-U! idx A-NAME-P!
   o E-S idx A-START!
   o E-CODE-BYTES idx A-LEN! ;

: CAPTURE-A ( -- )
   0 A-N !
   DICT-START
   begin dup DICT-END < while
      dup ENT? 0= if s" imgdump: corrupt dict entry" 74 die then
      dup A-N @ CAPTURE-A-ENTRY
      A-N @ 1 + A-N !
      DREC +
   repeat drop ;

: PRINT-A-ENTRY {: idx :} ( n -- )
   idx A-NAME-P@ idx A-NAME-U@ type 32 EMITC
   idx A-START@ h. 32 EMITC
   idx A-LEN@ h. 10 EMITC ;

: PRINT-B0 ( -- )
   CMP-B0-P @ CMP-B0-U @ type 32 EMITC
   CMP-B0-S @ h. 32 EMITC
   CMP-B0-L @ h. 10 EMITC ;

: PRINT-A-NL {: idx :} ( n -- )
   idx A-NAME-P@ idx A-NAME-U@ type 32 EMITC idx A-LEN@ h. 10 EMITC ;

: PRINT-B-NL {: o :} ( n -- )
   o E-NAME type 32 EMITC o E-CODE-BYTES h. 10 EMITC ;

: PRINT-BAD-NL ( -- )
   CMP-BAD-P @ CMP-BAD-U @ type 32 EMITC CMP-BAD-L @ h. 10 EMITC ;

: CMP-NAME-LEN? {: o idx :} ( n n -- bool )
   idx A-NAME-P@ idx A-NAME-U@ o E-NAME CORE-STR=
   idx A-LEN@ o E-CODE-BYTES = and ;

: CMP-ENTRY {: o idx :} ( n n -- )
   idx 0= if
      o E-NAME CMP-B0-U ! CMP-B0-P !
      o E-S CMP-B0-S !
      o E-CODE-BYTES CMP-B0-L !
   then
   CMP-NAME-LEN-DIFF @ if exit then
   idx A-N @ >= if
      o E-NAME CMP-BAD-U ! CMP-BAD-P !
      o E-CODE-BYTES CMP-BAD-L !
      idx CMP-BAD-IDX !
      -1 CMP-NAME-LEN-DIFF !
      exit
   then
   o idx CMP-NAME-LEN? 0= if
      o E-NAME CMP-BAD-U ! CMP-BAD-P !
      o E-CODE-BYTES CMP-BAD-L !
      -1 CMP-NAME-LEN-DIFF !
      idx CMP-BAD-IDX !
      exit
   then
   idx A-START@ o E-S <> if -1 CMP-OFF-DIFF ! then ;

: COMPARE-B ( -- )
   0 CMP-NAME-LEN-DIFF !
   0 CMP-OFF-DIFF !
   0 CMP-IDX !
   DICT-START
   begin dup DICT-END < while
      dup ENT? 0= if s" imgdump: corrupt dict entry" 74 die then
      dup CMP-IDX @ CMP-ENTRY
      CMP-IDX @ 1 + CMP-IDX !
      DREC +
   repeat drop
   CMP-NAME-LEN-DIFF @ 0= if
      CMP-IDX @ A-N @ <> if
         CMP-IDX @ CMP-BAD-IDX !
         NULL-PTR CMP-BAD-P !
         0 CMP-BAD-U !
         0 CMP-BAD-L !
         -1 CMP-NAME-LEN-DIFF !
      then
   then ;

: REPORT-COMPARE ( -- )
   CMP-NAME-LEN-DIFF @ if
      s" word size/name differences (name len):" type cr
      CMP-BAD-IDX @ A-N @ < if 60 EMITC 32 EMITC CMP-BAD-IDX @ PRINT-A-NL then
      CMP-BAD-P @ 0= 0= if 62 EMITC 32 EMITC PRINT-BAD-NL then
      s" imgdump: dictionaries differ" 1 die
   then
   CMP-OFF-DIFF @ if
      s" word sizes identical; offsets shifted (first entry):" type cr
      0 PRINT-A-ENTRY
      PRINT-B0
      exit
   then
   s" identical dicts" type cr ;

: COMPARE-IMG ( -- )
   0 SCRIPT-ARGV$ READ-IMG-PATH PREP-IMG CAPTURE-A
   1 SCRIPT-ARGV$ READ-IMG-PATH PREP-IMG COMPARE-B REPORT-COMPARE ;

: PC-ARG ( n -- n )
   SCRIPT-ARGV$ IMG>NUMBER? MATCH option
     none OF IMG-USAGE ENDOF
     some OF ENDOF
   ;MATCH ;

: PC-IMG ( -- )
   1 SCRIPT-ARGV$ READ-IMG-PATH PREP-IMG
   SCRIPT-ARGC 2 ?do i PC-ARG PC>DICT loop ;

: WID-IMG ( -- )
   1 SCRIPT-ARGV$ READ-IMG-PATH PREP-IMG
   DICT-START
   begin dup DICT-END < while
      dup ENT? 0= if drop s" imgdump: corrupt dict entry" 74 die then
      dup E-NAME 2 SCRIPT-ARGV$ CORE-STR= if
         dup E-NAME type s"  wid " type E-WID . cr exit
      then
      DREC +
   repeat drop
   s" imgdump: word not found" 74 die ;

: DATA-IMG ( -- )
   1 SCRIPT-ARGV$ READ-IMG-PATH LOAD-SNAPSHOT
   HAS-SNAP @ 0= if s" imgdump: no snapshot" 74 die then
   2 SCRIPT-ARGV$ IMG>NUMBER? MATCH option
     none OF IMG-USAGE ENDOF
     some OF ENDOF
   ;MATCH {: off:n :}
   off 0 < off 8 + TDATA @ > or if s" imgdump: data offset out of range" 74 die then
   TOFF @ TDATA @ - off + I@ . ;

\ Print snapshot dictionary and payload sizes.
: SNAP-INFO ( -- )
   1 SCRIPT-ARGV$ READ-IMG-PATH LOAD-SNAPSHOT
   HAS-SNAP @ 0= if s" no-snapshot" type cr exit then
   s" ndict " type TNDICT @ . cr
   s" region " type TREG @ h. cr
   s" data " type TDATA @ h. cr ;

: MAIN ( -- )
   SCRIPT-ARGC 3 >= if 0 SCRIPT-ARGV$ s" --pc" CORE-STR= if PC-IMG exit then then
   SCRIPT-ARGC 3 = if 0 SCRIPT-ARGV$ s" --wid" CORE-STR= if WID-IMG exit then then
   SCRIPT-ARGC 3 = if 0 SCRIPT-ARGV$ s" --data" CORE-STR= if DATA-IMG exit then then
   SCRIPT-ARGC 2 = if 0 SCRIPT-ARGV$ s" --snap" CORE-STR= if SNAP-INFO exit then then
   SCRIPT-ARGC 2 = if COMPARE-IMG exit then
   READ-IMG  PREP-IMG  DUMP-DICT ;

: RUN-MAIN? ( -- )
   SCRIPT-ARGC 0 > if MAIN then ;
RUN-MAIN?

;package
