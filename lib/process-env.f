\ process-env.f - checked argv/env process helpers and PATH lookup.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/process.f, and
\ lib/process-argv.f. Kept separate from process-argv so old native seeds can
\ still run tools/build.sh before this newer primitive exists.

256 constant PROC-ENV-MAX
131072 constant PROC-ENV-BUF-CAP
61 constant PROC-ENV-EQUAL
58 constant PROC-PATH-SEP
47 constant PROC-PATH-SLASH
1 constant PROC-X-OK

create PROC-ENV-TABLE PROC-ENV-MAX 1 + cells allot
create PROC-ENV-BUF PROC-ENV-BUF-CAP allot

variable PROC-ENV-N
variable PROC-ENV-OFF
variable PROC-ENV-I
variable PROC-PATH-I

: PROC-ENV-TRUE ( -- bool )
   0 0= ;

: PROC-ENV-FALSE ( -- bool )
   0 0= 0= ;

: PROC-SPAWN-ARGV-ENV-RAW ( ptr u8 ptr a ptr a n n n -- n )
   spawn-argv-env-io ;

: PROC-ENV-RESET ( -- )
   0 PROC-ENV-N !
   0 PROC-ENV-OFF ! ;

: PROC-ENV-SLOT ( n -- ptr a ) {: idx :}
   idx 0 < if E-PROC-ENV throw then
   idx PROC-ENV-MAX > if E-PROC-ENV throw then
   idx cells PROC-ENV-TABLE + ;

: PROC-ENV-CHECK-EXTRA ( -- )
   PROC-ENV-N @ PROC-ENV-MAX >= if E-PROC-ENV throw then ;

: PROC-ENV-HAS-EQUAL? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ PROC-ENV-EQUAL = if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;

: PROC-ENV-CHECK-NAME ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-PROC-ENV throw then
   0 begin dup u < while
      dup a + c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
      1+
   repeat drop ;

: PROC-ENV-CHECK-ENTRY ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-PROC-ENV throw then
   a c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
   a u PROC-ENV-HAS-EQUAL? 0= if E-PROC-ENV throw then ;

: PROC-ENV-STORE-Z ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   u 0 < if E-PROC-ENV throw then
   PROC-ENV-OFF @ {: off :}
   off u 1 + + PROC-ENV-BUF-CAP > if E-PROC-ENV throw then
   a PROC-ENV-BUF off + u BYTE-COPY
   0 PROC-ENV-BUF off + u + c!
   off u 1 + + PROC-ENV-OFF !
   PROC-ENV-BUF off + ;

: PROC-ENV-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-N @ PROC-ENV-SLOT !
   PROC-ENV-N @ 1+ PROC-ENV-N ! ;

: PROC-ENV-ENTRY+ ( ptr u8 n -- ) {: a:ptr u :}
   a u PROC-ENV-CHECK-ENTRY
   PROC-ENV-CHECK-EXTRA
   a u PROC-ENV-STORE-Z PROC-ENV-INSTALL-Z ;

: PROC-ENV-NAME-LEN ( ptr u8 n -- n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ PROC-ENV-EQUAL = if exit then
      1+
   repeat ;

: PROC-ENV-SAME-NAME? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   a u PROC-ENV-NAME-LEN {: au :}
   b v PROC-ENV-NAME-LEN {: bv :}
   au bv <> if PROC-ENV-FALSE exit then
   a au b bv STR= ;

: PROC-ENV-SLOT-NAME? ( ptr u8 n n -- bool ) {: a:ptr u idx :}
   idx PROC-ENV-SLOT @ {: z:ptr :}
   a u z z ZLEN PROC-ENV-SAME-NAME? ;

: PROC-ENV-HAS-NAME? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 begin dup PROC-ENV-N @ < while
      dup PROC-ENV-I !
      a u PROC-ENV-I @ PROC-ENV-SLOT-NAME? if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;

: PROC-ENV+ ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu val:ptr valu :}
   name nameu PROC-ENV-CHECK-NAME
   valu 0 < if E-PROC-ENV throw then
   PROC-ENV-CHECK-EXTRA
   PROC-ENV-OFF @ {: off :}
   off nameu valu + 2 + + PROC-ENV-BUF-CAP > if E-PROC-ENV throw then
   name PROC-ENV-BUF off + nameu BYTE-COPY
   PROC-ENV-EQUAL PROC-ENV-BUF off + nameu + c!
   val PROC-ENV-BUF off + nameu + 1 + valu BYTE-COPY
   0 PROC-ENV-BUF off + nameu + 1 + valu + c!
   PROC-ENV-BUF off + PROC-ENV-INSTALL-Z
   off nameu valu + 2 + + PROC-ENV-OFF ! ;

: PROC-ENV-PREPARE ( -- ptr a )
   0 PROC-ENV-N @ PROC-ENV-SLOT !
   PROC-ENV-TABLE ;

: PROC-ENV-INHERIT-ONE ( n -- n ) {: idx :}
   idx ENVP dup ZLEN {: z:ptr u :}
   z u PROC-ENV-CHECK-ENTRY
   z u PROC-ENV-HAS-NAME? 0= if z u PROC-ENV-ENTRY+ then
   idx 1+ ;

: PROC-ENV-INHERIT-MISSING ( -- )
   0 begin dup ENVP 0= 0= while
      PROC-ENV-INHERIT-ONE
   repeat drop ;

: PROC-ARGV-ENV-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: SPAWN-ARGV-ENV-IO ( ptr u8 n n n n -- n ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE PROC-ENV-PREPARE infd outfd errfd
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-ARGV-ENV-IO-RC ( ptr u8 n n n n -- n )
   SPAWN-ARGV-ENV-IO WAIT-RC ;

: PROC-SPAWN-ARGV-ENV-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp -1 PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp PROC-ARGV-IN-R @ PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid 0 < if E-PROC-SPAWN PROC-ARGV-THROW-CAPTURE then
   pid PROC-PID !
   PROC-ARGV-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-ARGV-ENV-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap 0 < if E-PROC-OUTPUT throw then
   errcap 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: RUN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   outcap 0 < if E-PROC-OUTPUT throw then
   errcap 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-ARGV-SETUP-STDIN-FDS
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-ARGV-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: PROC-HAS-SLASH? ( ptr u8 n -- bool )
   PROC-PATH-SLASH INDEX-OF 0 >= ;

: PROC-EXECUTABLE? ( ptr u8 n -- bool )
   FS-PATHZ PROC-X-OK access 0= ;

: PROC-COPY-PATH ( ptr u8 n ptr u8 -- n ) {: a:ptr u dst:ptr :}
   u 0 < if E-PROC-PATH throw then
   u FS-PATH-CAP > if E-PROC-PATH throw then
   a dst u BYTE-COPY
   u ;

: PROC-JOIN-PATH-SEG ( ptr u8 n ptr u8 n ptr u8 -- n )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   segu 0= if
      s" ." cmd cmdu dst JOIN-PATH
   else
      seg segu cmd cmdu dst JOIN-PATH
   then ;

: PROC-TRY-PATH-SEG ( ptr u8 n ptr u8 n ptr u8 -- n bool )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   seg segu cmd cmdu dst PROC-JOIN-PATH-SEG {: gotu :}
   dst gotu PROC-EXECUTABLE? if gotu PROC-ENV-TRUE exit then
   gotu PROC-ENV-FALSE ;

: FIND-EXECUTABLE-IN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n bool )
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu PROC-HAS-SLASH? if
      cmd cmdu PROC-EXECUTABLE? if
         cmd cmdu dst PROC-COPY-PATH PROC-ENV-TRUE exit
      then
      0 PROC-ENV-FALSE exit
   then
   0 PROC-PATH-I !
   begin path pathu PROC-PATH-SEP PROC-PATH-I @ SPLIT-NEXT while
      PROC-PATH-I !
      cmd cmdu dst PROC-TRY-PATH-SEG if PROC-ENV-TRUE exit then
      drop
   repeat
   drop 2drop
   0 PROC-ENV-FALSE ;

: FIND-EXECUTABLE ( ptr u8 n ptr u8 -- n bool ) {: cmd:ptr cmdu dst:ptr :}
   s" PATH" GETENV {: path:ptr pathu :}
   pathu 0= if 0 PROC-ENV-FALSE exit then
   cmd cmdu path pathu dst FIND-EXECUTABLE-IN-PATH ;

: RESOLVE-EXECUTABLE ( ptr u8 n ptr u8 -- n )
   FIND-EXECUTABLE if exit then
   E-PROC-PATH throw ;
