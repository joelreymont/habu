\ string.f - checked byte-string helpers.

9 constant STR-TAB
10 constant STR-LF
13 constant STR-CR
32 constant STR-SPACE
64 constant STR-BEFORE-A
91 constant STR-AFTER-Z
96 constant STR-BEFORE-LOWER-A
123 constant STR-AFTER-LOWER-Z

: BYTE-COPY ( ptr u8 ptr u8 n -- ) {: src:ptr dst:ptr u :}
   0 begin dup u < while
      dup src + c@ over dst + c!
      1+
   repeat drop ;

: ASCII-LOWER ( n -- n )
   dup STR-BEFORE-A > over STR-AFTER-Z < and if STR-SPACE + then ;

: ASCII-UPPER ( n -- n )
   dup STR-BEFORE-LOWER-A > over STR-AFTER-LOWER-Z < and if STR-SPACE - then ;

: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ ASCII-LOWER over b + c@ ASCII-LOWER <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: STARTS-WITH? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if 0 0= 0= exit then
   a v b v STR= ;

: ENDS-WITH? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if 0 0= 0= exit then
   a u v - + v b v STR= ;

: FIND-SUB ( ptr u8 n ptr u8 n -- n ) {: a:ptr u b:ptr v :}
   v 0= if 0 exit then
   u v < if -1 exit then
   0 begin dup u v - <= while
      dup a + v b v STR= if exit then
      1+
   repeat drop -1 ;

: CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   FIND-SUB 0 < 0= ;

: INDEX-OF ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat drop -1 ;

: COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 0 begin dup u < while
      dup a + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

: LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      0= if dup a + u rot - exit then
      1+
   repeat drop a 0 ;

: RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u begin dup 0 > while
      a over 1- + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      if 1- else a swap exit then
   repeat drop a 0 ;

: TRIM ( ptr u8 n -- ptr u8 n )
   LTRIM RTRIM ;
