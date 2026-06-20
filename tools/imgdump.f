\ imgdump.f — habu image inspector, in habu. Run: bin/hb tools/imgdump.f <image>
\ Reads the image path argument, locates the snapshot trailer, maps the live region
\ payload, and prints one line per word: name $start $len.
\ Self-contained: runs on bin/hb with nothing prepended.

variable IB   variable IL                    \ image buffer, length
variable IFD
1024 constant IPATH-CAP
create IPATH IPATH-CAP 1 + allot
create ISTAT 144 allot
variable TOFF  variable TBASE  variable TNDICT  variable TREG  variable TDATA
variable ROFF  variable SCAN-OFF  variable HAS-SNAP
variable RUNV  variable BESTO  variable BESTN
variable HN  variable ISZ

: IB@ IB @ ;
s" IB@" s" -- ptr u8" TRUST

: IMG-USAGE ( -- )
   s" usage: bin/hb tools/imgdump.f image" 64 die ;

: IMG-PATH$ ( -- ptr u8 n )
   SCRIPT-ARGC 1 <> if IMG-USAGE then
   0 SCRIPT-ARGV$ ;

: ZPATH {: a:ptr u d:ptr cap :} ( ptr u8 ptr n -- )
   u cap > if s" imgdump: path too long" 74 die then
   0 begin dup u < while  dup a + c@  over d + c!  1 + repeat drop  0 d u + c! ;

: READ-IMG
   IMG-PATH$ IPATH IPATH-CAP ZPATH
   IPATH ISTAT stat64 0 < IF s" imgdump: stat failed" 74 die THEN
   ISTAT 96 + @ ISZ !
   ISZ @ 0 > 0= IF s" imgdump: empty image" 74 die THEN
   IPATH 0 0 open IFD !
   IFD @ 0 < IF s" imgdump: open failed" 74 die THEN
   0 ISZ @ 1 2 IFD @ 0 mmap
   dup 0 < IF IFD @ close s" imgdump: mmap failed" 74 die THEN
   IB !
   IFD @ close
   ISZ @ IL ! ;

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
: I@ {: o :} ( n -- n )
   IB@ o + @ ;
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
: SNAP? {: o :} ( n -- bool )
   o 40 + IL @ > if 0 0= 0= exit then
   o I@ SNAP-MAGIC = 0= if 0 0= 0= exit then
   o 16 + I@ 1 < if 0 0= 0= exit then
   o 16 + I@ DICT-CAP > if 0 0= 0= exit then
   o 24 + I@ 0 <= if 0 0= 0= exit then
   o 24 + I@ REGION > if 0 0= 0= exit then
   o 32 + I@ 0 <= if 0 0= 0= exit then
   o 32 + I@ DATA-SIZE > if 0 0= 0= exit then
   o 16 + I@ DREC *  o 24 + I@ > if 0 0= 0= exit then
   o 24 + I@ o 32 + I@ +  o <> if 0 0= 0= exit then
   0 0= ;
: FIND-SNAPSHOT ( -- bool )
   -1 TOFF !
   IL @ 40 - SCAN-OFF !
   begin SCAN-OFF @ 0 >= while
      SCAN-OFF @ SNAP? if SCAN-OFF @ TOFF ! 0 0= exit then
      SCAN-OFF @ 1 - SCAN-OFF !
   repeat
   0 0= 0= ;
: LOAD-SNAPSHOT ( -- )
   FIND-SNAPSHOT 0= if 0 HAS-SNAP ! exit then
   -1 HAS-SNAP !
   TOFF @ 8 + I@ TBASE !
   TOFF @ 16 + I@ TNDICT !
   TOFF @ 24 + I@ TREG !
   TOFF @ 32 + I@ TDATA !
   TOFF @ TDATA @ - TREG @ - ROFF ! ;

: PTR>OFF {: p :} ( n -- n )
   HAS-SNAP @ 0= if -1 exit then
   p RBASE-VA >=  p RBASE-VA TREG @ + < and if p RBASE-VA - ROFF @ + exit then
   p TBASE @ >=  p TBASE @ ROFF @ CODE-OFF - + < and if p TBASE @ - CODE-OFF + exit then
   -1 ;
: E-NAME-OFF {: o :} ( n -- n )
   o E-F DNAME-EXT and 0= if o 24 + else o 24 + I@ PTR>OFF then ;
: E-NAME {: o :} ( n -- ptr u8 )
   o E-NAME-OFF dup 0 < if s" imgdump: bad external name pointer" 74 die then
   dup o E-L + IL @ > if s" imgdump: truncated name" 74 die then
   IB@ +  o E-L ;
: ENT? {: o :} ( n -- bool )
   o E-S 0 <= if 0 0= 0= exit then
   o E-E 0 < if 0 0= 0= exit then
   HAS-SNAP @ if o E-S PTR>OFF 0 < if 0 0= 0= exit then then
   o E-L 1 < if 0 0= 0= exit then
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
: DUMP-DICT
   HAS-SNAP @ if ROFF @ else BESTO @ then
   begin dup HAS-SNAP @ if ROFF @ TNDICT @ DREC * + else BESTO @ BESTN @ DREC * + then < while
      dup ENT? 0= if s" imgdump: corrupt dict entry" 74 die then
      dup .ENT  DREC +
   repeat drop ;

: MAIN ( -- )
   READ-IMG  LOAD-SNAPSHOT  HAS-SNAP @ 0= if FIND-DICT then  DUMP-DICT ;
MAIN
