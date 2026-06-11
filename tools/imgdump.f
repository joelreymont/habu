\ imgdump.f — habu image inspector, in habu. Run: tools/imgdump.sh <image>
\ Reads /tmp/imgdump-in, locates the embedded dict (48-byte entries:
\ start/end/len/name[16]/wid), prints one line per word: name $start $len.
\ Self-contained: runs on bin/hbi with nothing prepended.

variable IB   variable IL                    \ image buffer, length
variable IFD  variable IRD
$40000 constant IMAX
create IPATH 32 allot

: zpath {: a u d :}
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;

: read-img
   s" /tmp/imgdump-in" IPATH zpath
   IPATH 0 0 open IFD !
   here IB !  IMAX allot  0 IL !
   BEGIN
     IFD @  IB @ IL @ +  IMAX IL @ -  read IRD !
     IRD @ 0 >
   WHILE  IL @ IRD @ + IL !  REPEAT
   IFD @ close
   IL @ 0 > 0= IF s" imgdump: empty image" 74 die THEN
   IL @ IMAX = IF s" imgdump: image exceeds buffer" 74 die THEN ;

\ ---- hex printing ($-prefixed, lowercase) and char output ----
create EB 4 allot
: emitc {: c :}  c EB c!  EB 1 type ;
: nib {: n :}  n 10 < IF n 48 + ELSE n 87 + THEN ;
create HB 24 allot
variable HV  variable HP
: h. {: u :}
   u HV !  HB 20 + HP !
   BEGIN
     HP @ 1 - HP !
     HV @ 15 and nib HP @ c!
     HV @ 16 / HV !
     HV @ 0 =
   UNTIL
   HP @ 1 - HP !  36 HP @ c!
   HP @  HB 20 + HP @ -  type ;

\ ---- dict entry fields and validation ----
: e-s ( o -- x )  IB @ + @ ;
: e-e ( o -- x )  8 + IB @ + @ ;
: e-l ( o -- x )  16 + IB @ + @ ;
variable OKV
: prn? {: a u :}                              \ a..a+u all printable ascii?
   1 OKV !
   0 BEGIN dup u < WHILE
     dup a + c@ 32 >  OKV @ and
     over a + c@ 127 <  and  OKV !
     1 +
   REPEAT drop
   OKV @ ;
: ent? {: o :}                                \ plausible entry at o?
   o e-s 0 >
   o e-e o e-s >  and
   o e-e IL @ <=  and
   o e-l 1 >=  and
   o e-l 16 <=  and
   dup IF  drop  o 24 + IB @ +  o e-l  prn?  THEN ;

\ ---- find the dict: longest run of consecutive valid entries ----
variable RUNV  variable BESTO  variable BESTN
: run# {: o :}
   0 RUNV !
   o BEGIN  dup IL @ 48 - <=  over ent? and  WHILE
     RUNV @ 1 + RUNV !  48 +
   REPEAT drop
   RUNV @ ;
: find-dict
   0 BESTO !  0 BESTN !
   0 BEGIN dup IL @ 48 - <= WHILE
     dup ent? IF
       dup run# RUNV !
       RUNV @ BESTN @ > IF  dup BESTO !  RUNV @ BESTN !  THEN
     THEN
     4 +
   REPEAT drop
   BESTN @ 0 = IF s" imgdump: no dict found" 74 die THEN ;

\ ---- dump ----
: .ent {: o :}
   o 24 + IB @ +  o e-l  type  32 emitc
   o e-s h.  32 emitc
   o e-e o e-s - h.  10 emitc ;
: dump-dict
   BESTO @
   BEGIN dup BESTO @ BESTN @ 48 * + < WHILE  dup .ent  48 + REPEAT drop ;

read-img  find-dict  dump-dict
