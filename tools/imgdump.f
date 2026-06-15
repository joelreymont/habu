\ imgdump.f — habu image inspector, in habu. Run: tools/imgdump.sh <image>
\ Reads /tmp/imgdump-in, locates the embedded dict (48-byte entries:
\ start/end/len/name[16]/wid), prints one line per word: name $start $len.
\ Self-contained: runs on bin/hb with nothing prepended.

variable IB   variable IL                    \ image buffer, length
variable IFD  variable IRD
$40000 constant IMAX
create IPATH 32 allot

: ZPATH {: a u d :}
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;

: READ-IMG
   s" /tmp/imgdump-in" IPATH ZPATH
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
: EMITC {: c :}  c EB c!  EB 1 type ;
: NIB {: n :}  n 10 < IF n 48 + ELSE n 87 + THEN ;
create HB 24 allot
variable HV  variable HP
: h. {: u :}
   u HV !  HB 20 + HP !
   BEGIN
     HP @ 1 - HP !
     HV @ 15 and NIB HP @ c!
     HV @ 16 / HV !
     HV @ 0 =
   UNTIL
   HP @ 1 - HP !  36 HP @ c!
   HP @  HB 20 + HP @ -  type ;

\ ---- dict entry fields and validation ----
: E-S ( o -- x )  IB @ + @ ;
: E-E ( o -- x )  8 + IB @ + @ ;
: E-L ( o -- x )  16 + IB @ + @ ;
variable OKV
: PRN? {: a u :}                              \ a..a+u all printable ascii?
   1 OKV !
   0 BEGIN dup u < WHILE
     dup a + c@ 32 >  OKV @ and
     over a + c@ 127 <  and  OKV !
     1 +
   REPEAT drop
   OKV @ ;
: ENT? {: o :}                                \ plausible entry at o?
   o E-S 0 >
   o E-E o E-S >  and
   o E-E IL @ <=  and
   o E-L 1 >=  and
   o E-L 16 <=  and
   dup IF  drop  o 24 + IB @ +  o E-L  PRN?  THEN ;

\ ---- find the dict: longest run of consecutive valid entries ----
variable RUNV  variable BESTO  variable BESTN
: RUN# {: o :}
   0 RUNV !
   o BEGIN  dup IL @ 48 - <=  over ENT? and  WHILE
     RUNV @ 1 + RUNV !  48 +
   REPEAT drop
   RUNV @ ;
: FIND-DICT
   0 BESTO !  0 BESTN !
   0 BEGIN dup IL @ 48 - <= WHILE
     dup ENT? IF
       dup RUN# RUNV !
       RUNV @ BESTN @ > IF  dup BESTO !  RUNV @ BESTN !  THEN
     THEN
     4 +
   REPEAT drop
   BESTN @ 0 = IF s" imgdump: no dict found" 74 die THEN ;

\ ---- dump ----
: .ENT {: o :}
   o 24 + IB @ +  o E-L  type  32 EMITC
   o E-S h.  32 EMITC
   o E-E o E-S - h.  10 EMITC ;
: DUMP-DICT
   BESTO @
   BEGIN dup BESTO @ BESTN @ 48 * + < WHILE  dup .ENT  48 + REPEAT drop ;

READ-IMG  FIND-DICT  DUMP-DICT
