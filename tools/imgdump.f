\ imgdump.f — habu image inspector, in habu.
\ Run: bin/hb --load tools/imgdump.f -- <image> [image2]
\ Reads the image path argument, locates the snapshot trailer, maps the live region
\ payload, and prints one line per word: name $start $len. With two images,
\ compares name+length first, then reports offset-only shifts.
\ Loads the target executable layout on demand; common dictionary layout is
\ already present in the native cold prefix.

s" src/core/result.f" required

: IMG-FALSE ( -- bool )
   0 0= 0= ;

: IMG-LOAD-TARGET-LAYOUT ( -- )
   s" DATA-SIZE" XREF-FIND 0= if
      HB-TARGET-LINUX? if s" src/os/linux/layout.f" included exit then
      HB-TARGET-MACOS? if s" src/os/macos/layout.f" included exit then
      s" imgdump: unknown target" 74 die
   then ;

IMG-LOAD-TARGET-LAYOUT

variable IB   variable IL                    \ image buffer, length
variable IFD
1024 constant IPATH-CAP
create IPATH IPATH-CAP 1 + allot
create ISTAT 144 allot
variable TOFF  variable IMG-TBASE  variable TNDICT  variable TREG  variable TDATA
variable ROFF  variable SCAN-OFF  variable HAS-SNAP
variable RUNV  variable BESTO  variable BESTN
variable SNAP-DIRECT-OFF
variable HN  variable ISZ
variable A-N  variable CMP-NAME-LEN-DIFF  variable CMP-OFF-DIFF  variable CMP-IDX
variable CMP-B0-P  variable CMP-B0-U  variable CMP-B0-S  variable CMP-B0-L
variable CMP-BAD-IDX  variable CMP-BAD-P  variable CMP-BAD-U  variable CMP-BAD-L
variable PCV
variable NUM-I  variable NUM-ACC  variable NUM-DIG

create A-NAME-P DICT-CAP cells allot
create A-NAME-U DICT-CAP cells allot
create A-START DICT-CAP cells allot
create A-LEN DICT-CAP cells allot

: IB-FIELD ( -- ptr ptr u8 )
   IB 0 ptr-field ;

: IB@ ( -- ptr u8 )
   IB-FIELD @ ;

: IB! ( ptr u8 -- )
   IB-FIELD ! ;

: IMG-MMAP-RESULT ( -- result<ptr u8,n> )
   0 ISZ @ 1 2 IFD @ 0 mmap RESULT:MMAP>BYTES ;

: IMG-MMAP-ERR ( n -- )
   drop IFD @ close s" imgdump: mmap failed" 74 die ;

: IMG-MMAP ( -- ptr u8 )
   IMG-MMAP-RESULT
   [: ;] [: IMG-MMAP-ERR ;] RESULT:CASE ;

: IMG-USAGE ( -- )
   s" usage: bin/hb --load tools/imgdump.f -- image [image2] | --pc image pc" 64 die ;

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
   IMG-MMAP IB!
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
: h. {: u :}
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
   HB HP @ +  HN @  type ;

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
: E-S {: o :} ( n -- n )
   o I@ ;
: E-E {: o :} ( n -- n )
   o 8 + I@ ;
: E-F {: o :} ( n -- n )
   o 16 + I@ ;
: E-L {: o :} ( n -- n )
   o E-F DNAME-LEN-MASK and ;
variable OKV
: PRN? {: a:ptr u :} ( ptr u8 n -- bool )     \ a..a+u all printable ascii?
   1 OKV !
   0 begin dup u < while
     dup a + c@ 32 >  OKV @ and
     over a + c@ 127 <  and  OKV !
     1 +
   repeat drop
   OKV @ ;
: SNAP-CORE? {: o :} ( n -- bool )
   o 40 + IL @ > if IMG-FALSE exit then
   o I@ SNAP-MAGIC = 0= if IMG-FALSE exit then
   o 16 + I@ 1 < if IMG-FALSE exit then
   o 16 + I@ DICT-CAP > if IMG-FALSE exit then
   o 24 + I@ 0 <= if IMG-FALSE exit then
   o 24 + I@ REGION > if IMG-FALSE exit then
   o 32 + I@ 0 <= if IMG-FALSE exit then
   o 32 + I@ DATA-SIZE > if IMG-FALSE exit then
   o 16 + I@ DREC *  o 24 + I@ > if IMG-FALSE exit then
   0 0= ;

: SNAP? {: o :} ( n -- bool )
   o SNAP-CORE? 0= if IMG-FALSE exit then
   o 24 + I@ o 32 + I@ +  o <> if IMG-FALSE exit then
   0 0= ;

: SNAP-DIRECT? {: o :} ( n -- bool )
   o SNAP-CORE? 0= if IMG-FALSE exit then
   o 24 + I@ o 32 + I@ +  o > if IMG-FALSE exit then
   0 0= ;
: FIND-SNAPSHOT ( -- bool )
   -1 TOFF !
   IMAGE-TEXT-SIZE-OFF I@ IMAGE-TEXT-TRAILER-ADJ + 40 -
   SNAP-DIRECT-OFF !
   SNAP-DIRECT-OFF @ 0 >= IF
      SNAP-DIRECT-OFF @ SNAP-DIRECT? IF SNAP-DIRECT-OFF @ TOFF ! 0 0= EXIT THEN
   THEN
   IL @ 40 - dup 7 and - SCAN-OFF !
   begin SCAN-OFF @ 0 >= while
      SCAN-OFF @ SNAP? if SCAN-OFF @ TOFF ! 0 0= exit then
      SCAN-OFF @ 8 - SCAN-OFF !
   repeat
   0 0= 0= ;
: LOAD-SNAPSHOT ( -- )
   FIND-SNAPSHOT 0= if 0 HAS-SNAP ! exit then
   -1 HAS-SNAP !
   TOFF @ 8 + I@ IMG-TBASE !
   TOFF @ 16 + I@ TNDICT !
   TOFF @ 24 + I@ TREG !
   TOFF @ 32 + I@ TDATA !
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
: ENT? {: o :} ( n -- bool )
   o E-S 0 <= if IMG-FALSE exit then
   o E-E 0 < if IMG-FALSE exit then
   HAS-SNAP @ if o E-S PTR>OFF 0 < if IMG-FALSE exit then then
   o E-L 1 < if IMG-FALSE exit then
   o E-NAME-OFF dup 0 < if drop 0 0= 0= exit then
   dup o E-L + IL @ > if drop 0 0= 0= exit then
   IB@ +  o E-L PRN? ;

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
      dup ENT? if
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
   o E-E h.  10 EMITC ;
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
   o E-S PCV @ <=  PCV @ o E-S o E-E + < and ;

: PC>DICT ( n -- )
   PCV !
   DICT-START
   begin dup DICT-END < while
      dup ENT? 0= if s" imgdump: corrupt dict entry" 74 die then
      dup PC-HIT? if .ENT exit then
      DREC +
   repeat drop
   s" imgdump: pc not found" 74 die ;

: PREP-IMG ( -- )
   LOAD-SNAPSHOT
   HAS-SNAP @ 0= if FIND-DICT then ;

: A-NAME-P! ( ptr u8 n -- ) cells A-NAME-P + ! ;
: A-NAME-U! ( n n -- ) cells A-NAME-U + ! ;
: A-START! ( n n -- ) cells A-START + ! ;
: A-LEN! ( n n -- ) cells A-LEN + ! ;
: A-NAME-P@ ( n -- ptr u8 ) cells A-NAME-P + @ ;
: A-NAME-U@ ( n -- n ) cells A-NAME-U + @ ;
: A-START@ ( n -- n ) cells A-START + @ ;
: A-LEN@ ( n -- n ) cells A-LEN + @ ;

: IMG-HEX-DIGIT? ( n -- n bool ) {: c :}
   c 48 >= c 57 <= and if c 48 - 0 0= exit then
   c 65 >= c 70 <= and if c 55 - 0 0= exit then
   c 97 >= c 102 <= and if c 87 - 0 0= exit then
   0 0 0= 0= ;

: DEC-DIGIT? ( n -- n bool ) {: c :}
   c 48 >= c 57 <= and if c 48 - 0 0= exit then
   0 0 0= 0= ;

: HEX-BODY ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u :}
   u 1 > if a c@ 36 = if a 1 + u 1 - 0 0= exit then then
   u 2 > if a c@ 48 = a 1 + c@ 120 = and if a 2 + u 2 - 0 0= exit then then
   a u 0 0= 0= ;

: PARSE-HEX ( ptr u8 n -- n bool ) {: a:ptr u :}
   u 0= if 0 0 0= 0= exit then
   0 NUM-I !
   0 NUM-ACC !
   begin NUM-I @ u < while
      a NUM-I @ + c@ IMG-HEX-DIGIT? 0= if drop 0 0 0= 0= exit then
      NUM-DIG !
      NUM-ACC @ 16 * NUM-DIG @ + NUM-ACC !
      NUM-I @ 1 + NUM-I !
   repeat
   NUM-ACC @ 0 0= ;

: PARSE-DEC ( ptr u8 n -- n bool ) {: a:ptr u :}
   u 0= if 0 0 0= 0= exit then
   0 NUM-I !
   0 NUM-ACC !
   begin NUM-I @ u < while
      a NUM-I @ + c@ DEC-DIGIT? 0= if drop 0 0 0= 0= exit then
      NUM-DIG !
      NUM-ACC @ 10 * NUM-DIG @ + NUM-ACC !
      NUM-I @ 1 + NUM-I !
   repeat
   NUM-ACC @ 0 0= ;

: IMG>NUMBER? ( ptr u8 n -- n bool )
   HEX-BODY if PARSE-HEX exit then
   PARSE-DEC ;

: CAPTURE-A-ENTRY {: o idx :} ( n n -- )
   idx DICT-CAP >= if s" imgdump: too many dict entries" 74 die then
   o E-NAME idx A-NAME-U! idx A-NAME-P!
   o E-S idx A-START!
   o E-E idx A-LEN! ;

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
   o E-NAME type 32 EMITC o E-E h. 10 EMITC ;

: PRINT-BAD-NL ( -- )
   CMP-BAD-P @ CMP-BAD-U @ type 32 EMITC CMP-BAD-L @ h. 10 EMITC ;

: CMP-NAME-LEN? {: o idx :} ( n n -- bool )
   idx A-NAME-P@ idx A-NAME-U@ o E-NAME CORE-STR=
   idx A-LEN@ o E-E = and ;

: CMP-ENTRY {: o idx :} ( n n -- )
   idx 0= if
      o E-NAME CMP-B0-U ! CMP-B0-P !
      o E-S CMP-B0-S !
      o E-E CMP-B0-L !
   then
   CMP-NAME-LEN-DIFF @ if exit then
   idx A-N @ >= if
      o E-NAME CMP-BAD-U ! CMP-BAD-P !
      o E-E CMP-BAD-L !
      idx CMP-BAD-IDX !
      -1 CMP-NAME-LEN-DIFF !
      exit
   then
   o idx CMP-NAME-LEN? 0= if
      o E-NAME CMP-BAD-U ! CMP-BAD-P !
      o E-E CMP-BAD-L !
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
         0 CMP-BAD-P !
         0 CMP-BAD-U !
         0 CMP-BAD-L !
         -1 CMP-NAME-LEN-DIFF !
      then
   then ;

: REPORT-COMPARE ( -- )
   CMP-NAME-LEN-DIFF @ if
      s" word size/name differences (name len):" type cr
      CMP-BAD-IDX @ A-N @ < if 60 EMITC 32 EMITC CMP-BAD-IDX @ PRINT-A-NL then
      CMP-BAD-P @ 0 <> if 62 EMITC 32 EMITC PRINT-BAD-NL then
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

: PC-ARG ( -- n )
   2 SCRIPT-ARGV$ IMG>NUMBER? 0= if drop IMG-USAGE then ;

: PC-IMG ( -- )
   1 SCRIPT-ARGV$ READ-IMG-PATH PREP-IMG
   PC-ARG PC>DICT ;

: MAIN ( -- )
   SCRIPT-ARGC 3 = if 0 SCRIPT-ARGV$ s" --pc" CORE-STR= if PC-IMG exit then then
   SCRIPT-ARGC 2 = if COMPARE-IMG exit then
   READ-IMG  PREP-IMG  DUMP-DICT ;

: RUN-MAIN? ( -- )
   SCRIPT-ARGC 0 > if MAIN then ;
RUN-MAIN?
