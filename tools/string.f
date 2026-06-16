\ string.f - checked byte-string helpers.

9 constant STR-TAB
10 constant STR-LF
13 constant STR-CR
32 constant STR-SPACE
64 constant STR-BEFORE-A
91 constant STR-AFTER-Z
96 constant STR-BEFORE-LOWER-A
123 constant STR-AFTER-LOWER-Z

variable STR-I
variable STR-N

: BYTE-COPY {: src:ptr dst:ptr u :} ( ptr u8 ptr u8 n -- )
   0 STR-I !
   begin STR-I @ u < while
      src STR-I @ + c@ dst STR-I @ + c!
      STR-I @ 1+ STR-I !
   repeat ;

: ASCII-LOWER ( n -- n )
   dup STR-BEFORE-A > over STR-AFTER-Z < and IF STR-SPACE + THEN ;

: ASCII-UPPER ( n -- n )
   dup STR-BEFORE-LOWER-A > over STR-AFTER-LOWER-Z < and IF STR-SPACE - THEN ;

: STR= {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v <> IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: STR=CI {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v <> IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a + c@ ASCII-LOWER over b + c@ ASCII-LOWER <> IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: STARTS-WITH? {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v < IF 0 0= 0= exit THEN
   a v b v STR= ;

: ENDS-WITH? {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v < IF 0 0= 0= exit THEN
   a u v - + v b v STR= ;

: FIND-SUB {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- n )
   v 0= IF 0 exit THEN
   u v < IF -1 exit THEN
   0 begin dup u v - <= while
      dup a + v b v STR= IF exit THEN
      1+
   repeat drop -1 ;

: CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   FIND-SUB 0 < 0= ;

: INDEX-OF {: a:ptr u c :} ( ptr u8 n n -- n )
   0 begin dup u < while
      dup a + c@ c = IF exit THEN
      1+
   repeat drop -1 ;

: COUNT-CHAR {: a:ptr u c :} ( ptr u8 n n -- n )
   0 STR-N !
   0 begin dup u < while
      dup a + c@ c = IF STR-N @ 1+ STR-N ! THEN
      1+
   repeat drop STR-N @ ;

: WS? ( n -- bool )
   dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or ;

: LTRIM {: a:ptr u :} ( ptr u8 n -- ptr u8 n )
   0 begin dup u < while
      dup a + c@ WS? 0= IF dup a + u rot - exit THEN
      1+
   repeat drop a 0 ;

: RTRIM {: a:ptr u :} ( ptr u8 n -- ptr u8 n )
   u STR-I !
   begin STR-I @ 0 > while
      a STR-I @ 1- + c@ WS? IF STR-I @ 1- STR-I ! ELSE a STR-I @ exit THEN
   repeat a 0 ;

: TRIM ( ptr u8 n -- ptr u8 n )
   LTRIM RTRIM ;
