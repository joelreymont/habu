\ process-env.f - checked argv/env process helpers and PATH lookup.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/process.f,
\ and lib/process-argv.f. Kept separate from process-argv so old native seeds can
\ still run the build-fixpoint installer before this newer primitive exists.

256 constant PROC-ENV-MAX
131072 constant PROC-ENV-BUF-CAP
61 constant PROC-ENV-EQUAL
58 constant PROC-PATH-SEP
47 constant PROC-PATH-SLASH
variable PROC-ENV-N
variable PROC-ENV-OFF
variable PROC-ENV-I
variable PROC-PATH-I
variable PROC-ENV-TABLE-A
variable PROC-ENV-BUF-A
variable PROC-ENV-DEF-N
variable PROC-ENV-DEF-OFF
variable PROC-ENV-DEF-TABLE-A
variable PROC-ENV-DEF-BUF-A

: PROC-ENV-TABLE-A-FIELD ( -- ptr ptr a )
   PROC-ENV-TABLE-A 0 ptr-field ;

: PROC-ENV-TABLE@ ( -- ptr a )
   PROC-ENV-TABLE-A-FIELD @ ;

: PROC-ENV-TABLE! ( ptr a -- )
   PROC-ENV-TABLE-A-FIELD ! ;

: PROC-ENV-TABLE ( -- ptr a )
   PROC-ENV-TABLE@ 0= if
      PROC-ENV-MAX 1 + >COUNT MEM-ALLOC-CELLS PROC-ENV-TABLE!
   then
   PROC-ENV-TABLE@ ;

: PROC-ENV-BUF-A-FIELD ( -- ptr ptr u8 )
   PROC-ENV-BUF-A 0 ptr-field ;

: PROC-ENV-BUF@ ( -- ptr u8 )
   PROC-ENV-BUF-A-FIELD @ ;

: PROC-ENV-BUF! ( ptr u8 -- )
   PROC-ENV-BUF-A-FIELD ! ;

: PROC-ENV-BUF ( -- ptr u8 )
   PROC-ENV-BUF@ 0= if
      PROC-ENV-BUF-CAP MEM-ALLOC-BYTES drop PROC-ENV-BUF!
   then
   PROC-ENV-BUF@ ;

: PROC-ENV-DEF-TABLE-A-FIELD ( -- ptr ptr a )
   PROC-ENV-DEF-TABLE-A 0 ptr-field ;

: PROC-ENV-DEF-TABLE@ ( -- ptr a )
   PROC-ENV-DEF-TABLE-A-FIELD @ ;

: PROC-ENV-DEF-TABLE! ( ptr a -- )
   PROC-ENV-DEF-TABLE-A-FIELD ! ;

: PROC-ENV-DEF-TABLE ( -- ptr a )
   PROC-ENV-DEF-TABLE@ 0= if
      PROC-ENV-MAX 1 + >COUNT MEM-ALLOC-CELLS PROC-ENV-DEF-TABLE!
   then
   PROC-ENV-DEF-TABLE@ ;

: PROC-ENV-DEF-BUF-A-FIELD ( -- ptr ptr u8 )
   PROC-ENV-DEF-BUF-A 0 ptr-field ;

: PROC-ENV-DEF-BUF@ ( -- ptr u8 )
   PROC-ENV-DEF-BUF-A-FIELD @ ;

: PROC-ENV-DEF-BUF! ( ptr u8 -- )
   PROC-ENV-DEF-BUF-A-FIELD ! ;

: PROC-ENV-DEF-BUF ( -- ptr u8 )
   PROC-ENV-DEF-BUF@ 0= if
      PROC-ENV-BUF-CAP MEM-ALLOC-BYTES drop PROC-ENV-DEF-BUF!
   then
   PROC-ENV-DEF-BUF@ ;

: PROC-ENV-TRUE ( -- bool )
   0 0= ;

: PROC-ENV-FALSE ( -- bool )
   0 0= 0= ;

: PROC-SPAWN-ARGV-ENV-RAW ( ptr u8 ptr a ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr envp:ptr infd outfd errfd :}
   pathz argv envp infd FD>N outfd FD>N errfd FD>N spawn-argv-env-io >PID ;

: PROC-ENV-RESET ( -- )
   0 >COUNT PROC-ENV-N !
   0 >OFF PROC-ENV-OFF ! ;

: PROC-ENV-DEFAULT-RESET ( -- )
   0 >COUNT PROC-ENV-DEF-N !
   0 >OFF PROC-ENV-DEF-OFF ! ;

: PROC-ENV-SLOT ( idx -- ptr a ) {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N PROC-ENV-MAX > if E-PROC-ENV throw then
   idx IDX>N cells PROC-ENV-TABLE + ;

: PROC-ENV-CHECK-EXTRA ( -- )
   PROC-ENV-N @ COUNT>N PROC-ENV-MAX >= if E-PROC-ENV throw then ;

: PROC-ENV-DEF-CHECK-EXTRA ( -- )
   PROC-ENV-DEF-N @ COUNT>N PROC-ENV-MAX >= if E-PROC-ENV throw then ;

: PROC-ENV-HAS-EQUAL? ( ptr u8 len -- bool ) {: a:ptr u :}
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;

: PROC-ENV-CHECK-NAME ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-ENV throw then
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
      1+
   repeat drop ;

: PROC-ENV-CHECK-ENTRY ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-ENV throw then
   a c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
   a u PROC-ENV-HAS-EQUAL? 0= if E-PROC-ENV throw then ;

: PROC-ENV-STORE-Z ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-ENV throw then
   PROC-ENV-OFF @ {: off :}
   off OFF>N u LEN>N 1 + + PROC-ENV-BUF-CAP > if E-PROC-ENV throw then
   a PROC-ENV-BUF off OFF>N + u LEN>N BYTE-COPY
   0 PROC-ENV-BUF off OFF>N + u LEN>N + c!
   off OFF>N u LEN>N 1 + + >OFF PROC-ENV-OFF !
   PROC-ENV-BUF off OFF>N + ;

: PROC-ENV-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   PROC-ENV-N @ COUNT>N 1+ >COUNT PROC-ENV-N ! ;

: PROC-ENV-DEF-SLOT ( idx -- ptr a ) {: idx:idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N PROC-ENV-MAX > if E-PROC-ENV throw then
   idx IDX>N cells PROC-ENV-DEF-TABLE + ;

: PROC-ENV-DEF-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-DEF-N @ COUNT>N >IDX PROC-ENV-DEF-SLOT !
   PROC-ENV-DEF-N @ COUNT>N 1+ >COUNT PROC-ENV-DEF-N ! ;

: PROC-ENV-ENTRY+ ( ptr u8 len -- ) {: a:ptr u :}
   a u PROC-ENV-CHECK-ENTRY
   PROC-ENV-CHECK-EXTRA
   a u PROC-ENV-STORE-Z PROC-ENV-INSTALL-Z ;

: PROC-ENV-NAME-LEN ( ptr u8 len -- len ) {: a:ptr u :}
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if >LEN exit then
      1+
   repeat >LEN ;

: PROC-ENV-SAME-NAME? ( ptr u8 len ptr u8 len -- bool ) {: a:ptr u b:ptr v :}
   a u PROC-ENV-NAME-LEN {: au :}
   b v PROC-ENV-NAME-LEN {: bv :}
   au LEN>N bv LEN>N <> if PROC-ENV-FALSE exit then
   a au LEN>N b bv LEN>N STR= ;

: PROC-ENV-SLOT-NAME? ( ptr u8 len idx -- bool ) {: a:ptr u idx :}
   idx PROC-ENV-SLOT @ {: z:ptr :}
   a u z z ZLEN >LEN PROC-ENV-SAME-NAME? ;

: PROC-ENV-NAME-IDX ( ptr u8 len -- n ) {: a:ptr u:len :}
   0 begin dup PROC-ENV-N @ COUNT>N < while
      dup >IDX PROC-ENV-I !
      a u PROC-ENV-I @ PROC-ENV-SLOT-NAME? if exit then
      1+
   repeat drop -1 ;

: PROC-ENV-HAS-NAME? ( ptr u8 len -- bool )
   PROC-ENV-NAME-IDX 0 >= ;

: PROC-ENV-DEFAULT$? ( ptr u8 len -- ptr u8 len bool ) {: a:ptr u:len :}
   0 >IDX begin dup IDX>N PROC-ENV-DEF-N @ COUNT>N < while
      dup PROC-ENV-I !
      PROC-ENV-I @ PROC-ENV-DEF-SLOT @ {: z:ptr :}
      a u z z ZLEN >LEN PROC-ENV-SAME-NAME? if
         drop
         z ZLEN >LEN {: zu:len :}
         z zu PROC-ENV-NAME-LEN {: nameu:len :}
         z nameu LEN>N + 1 + zu LEN>N nameu LEN>N - 1 - >LEN
         PROC-ENV-TRUE
         exit
      then
      IDX>N 1+ >IDX
   repeat drop
   s" " >LEN PROC-ENV-FALSE ;

: PROC-ENV-ROW-Z ( ptr u8 len ptr u8 len -- ptr u8 ) {: name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-CHECK-NAME
   valu LEN>N 0 < if E-PROC-ENV throw then
   PROC-ENV-OFF @ {: off:off :}
   off OFF>N nameu LEN>N valu LEN>N + 2 + + PROC-ENV-BUF-CAP > if E-PROC-ENV throw then
   name PROC-ENV-BUF off OFF>N + nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL PROC-ENV-BUF off OFF>N + nameu LEN>N + c!
   val PROC-ENV-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 PROC-ENV-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N + c!
   off OFF>N nameu LEN>N valu LEN>N + 2 + + >OFF PROC-ENV-OFF !
   PROC-ENV-BUF off OFF>N + ;

: PROC-ENV+ ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   PROC-ENV-CHECK-EXTRA
   name nameu val valu PROC-ENV-ROW-Z PROC-ENV-INSTALL-Z ;

\ Replace-or-add: exactly one row for the name survives. Unlike PROC-ENV+,
\ which appends and leaves duplicate-key resolution to the child's getenv,
\ this overwrites an existing row in place (e.g. one copied from the parent's
\ own environment by PROC-ENV-INHERIT-MISSING) and appends only when absent.
: PROC-ENV-SET ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-NAME-IDX {: i:n :}
   i 0 < if name nameu val valu PROC-ENV+ exit then
   name nameu val valu PROC-ENV-ROW-Z i >IDX PROC-ENV-SLOT ! ;

: PROC-ENV-DEFAULT+ ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-CHECK-NAME
   valu LEN>N 0 < if E-PROC-ENV throw then
   PROC-ENV-DEF-CHECK-EXTRA
   PROC-ENV-DEF-OFF @ {: off:off :}
   off OFF>N nameu LEN>N valu LEN>N + 2 + + PROC-ENV-BUF-CAP > if E-PROC-ENV throw then
   name PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N + c!
   val PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N + c!
   PROC-ENV-DEF-BUF off OFF>N + PROC-ENV-DEF-INSTALL-Z
   off OFF>N nameu LEN>N valu LEN>N + 2 + + >OFF PROC-ENV-DEF-OFF ! ;

: PROC-ENV-PREPARE ( -- ptr a )
   0 PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   PROC-ENV-TABLE ;

: PROC-ENV-INHERIT-ONE ( idx -- idx ) {: idx :}
   idx IDX>N ENVP dup ZLEN {: z:ptr u :}
   z u >LEN PROC-ENV-CHECK-ENTRY
   z u >LEN PROC-ENV-HAS-NAME? 0= if z u >LEN PROC-ENV-ENTRY+ then
   idx IDX>N 1+ >IDX ;

: PROC-ENV-INHERIT-DEFAULT-ONE ( idx -- idx ) {: idx:idx :}
   idx PROC-ENV-DEF-SLOT @ {: z:ptr :}
   z z ZLEN >LEN PROC-ENV-CHECK-ENTRY
   z z ZLEN >LEN PROC-ENV-HAS-NAME? 0= if z z ZLEN >LEN PROC-ENV-ENTRY+ then
   idx IDX>N 1+ >IDX ;

: PROC-ENV-INHERIT-DEFAULTS ( -- )
   0 >IDX begin dup IDX>N PROC-ENV-DEF-N @ COUNT>N < while
      PROC-ENV-INHERIT-DEFAULT-ONE
   repeat drop ;

: PROC-ENV-INHERIT-MISSING ( -- )
   PROC-ENV-INHERIT-DEFAULTS
   0 >IDX begin dup IDX>N ENVP 0= 0= while
      PROC-ENV-INHERIT-ONE
   repeat drop ;

: PROC-ARGV-ENV-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PROC-SPAWN-ARGV-ENV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE PROC-ENV-PREPARE infd outfd errfd
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-ENV-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )   \ ok = clean exit (0), err = nonzero completion rc
   PROC-SPAWN-ARGV-ENV-IO PROC-WAIT-RC ;

: PROC-SPAWN-ARGV-ENV-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp -1 >FD PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp PROC-IN-R @ PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-ARGV-ENV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: RUN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: PROC-HAS-SLASH? ( ptr u8 len -- bool )
   LEN>N PROC-PATH-SLASH INDEX-OF 0 >= ;

: PROC-EXECUTABLE? ( ptr u8 len -- bool )
   LEN>N EXECUTABLE? ;

: PROC-COPY-PATH ( ptr u8 len ptr u8 -- len ) {: a:ptr u dst:ptr :}
   u LEN>N 0 < if E-PROC-PATH throw then
   u LEN>N FS-PATH-CAP > if E-PROC-PATH throw then
   a dst u LEN>N BYTE-COPY
   u ;

: PROC-JOIN-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- len )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   segu LEN>N 0= if
      s" ." cmd cmdu LEN>N dst JOIN-PATH >LEN
   else
      seg segu LEN>N cmd cmdu LEN>N dst JOIN-PATH >LEN
   then ;

: PROC-TRY-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- len bool )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   seg segu cmd cmdu dst PROC-JOIN-PATH-SEG {: gotu :}
   dst gotu PROC-EXECUTABLE? if gotu PROC-ENV-TRUE exit then
   gotu PROC-ENV-FALSE ;

: FIND-EXECUTABLE-IN-PATH ( ptr u8 len ptr u8 len ptr u8 -- len bool )
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu PROC-HAS-SLASH? if
      cmd cmdu PROC-EXECUTABLE? if
         cmd cmdu dst PROC-COPY-PATH PROC-ENV-TRUE exit
      then
      0 >LEN PROC-ENV-FALSE exit
   then
   0 >OFF PROC-PATH-I !
   begin path pathu LEN>N PROC-PATH-SEP PROC-PATH-I @ OFF>N SPLIT-NEXT while
      >OFF PROC-PATH-I !
      >LEN cmd cmdu dst PROC-TRY-PATH-SEG if PROC-ENV-TRUE exit then
      drop
   repeat
   drop 2drop
   0 >LEN PROC-ENV-FALSE ;

: FIND-EXECUTABLE ( ptr u8 len ptr u8 -- len bool ) {: cmd:ptr cmdu dst:ptr :}
   s" PATH" GETENV {: path:ptr pathu :}
   pathu 0= if 0 >LEN PROC-ENV-FALSE exit then
   cmd cmdu path pathu >LEN dst FIND-EXECUTABLE-IN-PATH ;

: RESOLVE-EXECUTABLE ( ptr u8 len ptr u8 -- len )
   FIND-EXECUTABLE if exit then
   E-PROC-PATH throw ;
