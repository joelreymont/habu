split-str
\ process-env.f - checked argv/env process helpers and PATH lookup. The prepared
\ environment and the inherited defaults are tables of row addresses plus row
\ bytes in chunks of max(row size, $1000), grown without a ceiling.
\ STORAGE CLASS. PROCESS-WIDE: both tables are one set for the image, held by
\ the task preparing a spawn until the spawn resets them; a reset keeps the
\ storage. It is scratch, so a build leaves it out. An owned CMD run reads only
\ the defaults.

\ A row's bytes never move once added: process-command.f INHERIT-Z holds them
\ from PREPARE to spawn.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/adt/option.f

61 constant PROC-ENV-EQUAL
58 constant PROC-PATH-SEP
47 constant PROC-PATH-SLASH
SCRATCH-VARIABLE PROC-ENV-N
variable PROC-ENV-I
variable PROC-PATH-I
SCRATCH-VARIABLE PROC-ENV-DEF-N

package PROC-ENV-ROWS
private

DYNAMIC-BUFFER TABLE ^u8
DYNAMIC-BUFFER CHUNKS SPAN:span<u8>
create CONTROL PROC-CHUNKS:CONTROL-CELLS cells SCRATCH-ALLOT
DYNAMIC-BUFFER DEF-TABLE ^u8
DYNAMIC-BUFFER DEF-CHUNKS SPAN:span<u8>
create DEF-CONTROL PROC-CHUNKS:CONTROL-CELLS cells SCRATCH-ALLOT

public

: SLOT ( idx -- ^^u8 )
   {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N 1+ TABLE-RESERVE
   idx IDX>N TABLE ;

: DEF-SLOT ( idx -- ^^u8 )
   {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N 1+ DEF-TABLE-RESERVE
   idx IDX>N DEF-TABLE ;

: BYTES ( n -- ^u8 )
   CONTROL [: drop CHUNKS ;] [: drop CHUNKS-RESERVE ;] PROC-CHUNKS:ROOM ;

: DEF-BYTES ( n -- ^u8 )
   DEF-CONTROL [: drop DEF-CHUNKS ;] [: drop DEF-CHUNKS-RESERVE ;] PROC-CHUNKS:ROOM ;

: REWIND ( -- )
   CONTROL PROC-CHUNKS:REWIND ;

: DEF-REWIND ( -- )
   DEF-CONTROL PROC-CHUNKS:REWIND ;

;package

: PROC-ENV-TRUE ( -- bool )
   0 0= ;

: PROC-ENV-FALSE ( -- bool )
   0 0= 0= ;

package PROC-RAW
public

: SPAWN-ARGV-ENV ( ^u8 ^a ^a fd fd fd -- ^u8 ^a ^a fd fd fd )
   {: pathz:^ argv:^ envp:^ infd outfd errfd :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv ROWS PROCESS:ARGV-ROWS
   envp ROWS PROCESS:ENV-ROWS
   infd outfd errfd LAUNCH
   pathz argv envp infd outfd errfd ;

: SPAWN-ARGV-ENV-CWD ( ^u8 ^a ^a ^u8 fd fd fd -- ^u8 ^a ^a ^u8 fd fd fd )
   {: pathz:^ argv:^ envp:^ cwdz:^ infd outfd errfd :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv ROWS PROCESS:ARGV-ROWS
   envp ROWS PROCESS:ENV-ROWS
   cwdz dup ZLEN PROCESS:CWD
   infd outfd errfd LAUNCH
   pathz argv envp cwdz infd outfd errfd ;

;package

: PROC-SPAWN-ARGV-ENV-RAW ( ^u8 ^a ^a fd fd fd -- pid )
   [: PROC-RAW:SPAWN-ARGV-ENV ;] catch {: rc :}
   drop drop drop drop drop drop
   rc PROC-RAW:ANSWER ;

: PROC-ENV-RESET ( -- )
   0 >COUNT PROC-ENV-N !
   PROC-ENV-ROWS:REWIND ;

: PROC-ENV-DEFAULT-RESET ( -- )
   0 >COUNT PROC-ENV-DEF-N !
   PROC-ENV-ROWS:DEF-REWIND ;

: PROC-ENV-SLOT ( idx -- ^^u8 )
   PROC-ENV-ROWS:SLOT ;

: PROC-ENV-DEF-SLOT ( idx -- ^^u8 )
   PROC-ENV-ROWS:DEF-SLOT ;

\ Held at the maximum cell instead of wrapping: nothing bounds a string's
\ length, and a plain sum near that maximum would come back small. Both
\ operands are nonnegative.
: PROC-ENV-NEED ( n n -- n ) {: used:n u :}
   u MEM-MAX-N used - >= if MEM-MAX-N exit then
   used u + 1 + ;

\ The size of one name=value row and its terminator, each length refused when
\ negative before it is summed and the sum refused when it would not fit a
\ cell. Every row writer checks it before a byte of the name is read.
: PROC-ENV-ROW-NEED ( n n -- n ) {: nameu valu :}
   nameu 0 < if E-PROC-ENV throw then
   valu 0 < if E-PROC-ENV throw then
   0 nameu PROC-ENV-NEED valu PROC-ENV-NEED {: need:n :}
   need MEM-MAX-N >= if E-PROC-ENV throw then
   need ;

: PROC-ENV-HAS-EQUAL? ( str -- bool ) {: a:^ u :}
   0 begin dup u < while
      dup a + c@ PROC-ENV-EQUAL = if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;


\ A name is refused empty or holding `=` or NUL: every reader of a row stops at
\ its first NUL, so a NUL would make a shorter name of it.
: PROC-ENV-CHECK-NAME ( str -- )
   {: a:^ u :}
   u 0 <= if E-PROC-ENV throw then
   0 begin dup u < while
      dup a + c@ dup PROC-ENV-EQUAL = swap 0= or if E-PROC-ENV throw then
      1+
   repeat drop ;


: PROC-ENV-CHECK-ENTRY ( str -- ) {: a:^ u :}
   u 0 <= if E-PROC-ENV throw then
   a c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
   a u PROC-ENV-HAS-EQUAL? 0= if E-PROC-ENV throw then ;

\ The size is checked before the entry is read: a length past it is refused
\ unread, never scanned for its `=`.
: PROC-ENV-STORE-Z ( str -- ^u8 ) {: a:^ u :}
   u 0 < if E-PROC-ENV throw then
   0 u PROC-ENV-NEED {: need:n :}
   need MEM-MAX-N >= if E-PROC-ENV throw then
   a u PROC-ENV-CHECK-ENTRY
   a u need PROC-ENV-ROWS:BYTES need >LEN PROCESS:ZCOPY ;

: PROC-ENV-INSTALL-Z ( ^u8 -- )
   PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   PROC-ENV-N @ COUNT>N 1+ >COUNT PROC-ENV-N ! ;

: PROC-ENV-DEF-INSTALL-Z ( ^u8 -- )
   PROC-ENV-DEF-N @ COUNT>N >IDX PROC-ENV-DEF-SLOT !
   PROC-ENV-DEF-N @ COUNT>N 1+ >COUNT PROC-ENV-DEF-N ! ;

: PROC-ENV-ENTRY+ ( str -- )
   PROC-ENV-STORE-Z PROC-ENV-INSTALL-Z ;

: PROC-ENV-NAME-LEN ( str -- n ) {: a:^ u :}
   0 begin dup u < while
      dup a + c@ PROC-ENV-EQUAL = if exit then
      1+
   repeat ;

: PROC-ENV-SAME-NAME? ( str str -- bool ) {: a:^ u b:^ v :}
   a u PROC-ENV-NAME-LEN {: au :}
   b v PROC-ENV-NAME-LEN {: bv :}
   au bv <> if PROC-ENV-FALSE exit then
   a au b bv STR:= ;

: PROC-ENV-VEC-NAME? ( n str ^^u8 -- bool ) {: i:n a:^ u:n tab:^ :}
   i cells tab + @ {: z:^ :}
   a u z z ZLEN PROC-ENV-SAME-NAME? ;

: PROC-ENV-VEC-NAME-IDX ( str ^^u8 n -- n ) {: a:^ u:n tab:^ rows:n :}
   0 begin dup rows < while
      dup a u tab PROC-ENV-VEC-NAME? if exit then
      1+
   repeat drop -1 ;

: PROC-ENV-VEC-HAS-NAME? ( str ^^u8 n -- bool )
   PROC-ENV-VEC-NAME-IDX 0 >= ;

: PROC-ENV-NAME-IDX ( str -- n ) {: a:^ u:n :}
   a u 0 >IDX PROC-ENV-SLOT PROC-ENV-N @ COUNT>N PROC-ENV-VEC-NAME-IDX ;

: PROC-ENV-HAS-NAME? ( str -- bool )
   PROC-ENV-NAME-IDX 0 >= ;

: PROC-ENV-DEFAULT$? ( str -- str bool ) {: a:^ u:n :}
   0 >IDX begin dup IDX>N PROC-ENV-DEF-N @ COUNT>N < while
      dup PROC-ENV-I !
      PROC-ENV-I @ PROC-ENV-DEF-SLOT @ {: z:^ :}
      a u z z ZLEN PROC-ENV-SAME-NAME? if
         drop
         z ZLEN {: zu:n :}
         z zu PROC-ENV-NAME-LEN {: nameu:n :}
         z nameu + 1 + zu nameu - 1 -
         PROC-ENV-TRUE
         exit
      then
      IDX>N 1+ >IDX
   repeat drop
   s" " PROC-ENV-FALSE ;

\ One name=value row in the bytes BYTES answers for its size.
: PROC-ENV-ROW ( str str [ n -- ^u8 ] -- ^u8 )
   {: name:^ nameu:n val:^ valu:n bytes :}
   nameu valu PROC-ENV-ROW-NEED {: need:n :}
   name nameu PROC-ENV-CHECK-NAME
   need bytes execute {: dst:^ :}
   val valu dst nameu + 1 +
   valu FFI:CSTR-ROOM SPAN:MAKE FFI:CSTR drop
   name dst nameu BYTE-COPY
   PROC-ENV-EQUAL dst nameu + c!
   dst ;

: PROC-ENV-ROW-Z ( str str -- ^u8 )
   [: PROC-ENV-ROWS:BYTES ;] PROC-ENV-ROW ;

: PROC-ENV+ ( str str -- )
   PROC-ENV-ROW-Z PROC-ENV-INSTALL-Z ;

\ Exactly one row for the name survives: an existing row (one copied by
\ PROC-ENV-INHERIT-MISSING, say) is overwritten in place. PROC-ENV+ appends and
\ leaves duplicate keys to the child's getenv.
: PROC-ENV-SET ( str str -- ) {: name:^ nameu:n val:^ valu:n :}
   nameu valu PROC-ENV-ROW-NEED drop
   name nameu PROC-ENV-NAME-IDX {: i:n :}
   i 0 < if name nameu val valu PROC-ENV+ exit then
   name nameu val valu PROC-ENV-ROW-Z i >IDX PROC-ENV-SLOT ! ;

: PROC-ENV-DEFAULT+ ( str str -- )
   [: PROC-ENV-ROWS:DEF-BYTES ;] PROC-ENV-ROW PROC-ENV-DEF-INSTALL-Z ;

\ The envp array the child receives ends at a null entry; the declared null
\ pointer spells it, because the table holds `^u8` and not a bare cell. The
\ base is fetched after the terminator is stored, which may move the table.
: PROC-ENV-PREPARE ( -- ^^u8 )
   NULL-PTR PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   0 >IDX PROC-ENV-SLOT ;

: PROC-ENV-INHERIT-ONE ( idx -- idx ) {: idx :}
   idx IDX>N ENVP dup ZLEN {: z:^ u :}
   z u PROC-ENV-CHECK-ENTRY
   z u PROC-ENV-HAS-NAME? 0= if z u PROC-ENV-ENTRY+ then
   idx IDX>N 1+ >IDX ;

: PROC-ENV-INHERIT-DEFAULT-ONE ( idx -- idx ) {: idx:idx :}
   idx PROC-ENV-DEF-SLOT @ {: z:^ :}
   z z ZLEN PROC-ENV-CHECK-ENTRY
   z z ZLEN PROC-ENV-HAS-NAME? 0= if z z ZLEN PROC-ENV-ENTRY+ then
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

: PROC-SPAWN-ARGV-ENV-IO ( str fd fd fd -- pid ) {: a:^ u infd outfd errfd :}
   [: a u PROC-ARGV-PREPARE PROC-ENV-PREPARE infd outfd errfd
      PROC-SPAWN-ARGV-ENV-RAW ;]
   [: PROC-ARGV-ENV-RESET ;] finally {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-ENV-IO-RC ( str fd fd fd -- result<n,n> )
   PROC-SPAWN-ARGV-ENV-IO PROCESS:WAIT PROCESS:STATUS>RC PROCESS:RC>RESULT ;

: PROC-SPAWN-ARGV-ENV-CAPTURE ( ^u8 ^a ^a -- ) {: pathz:^ argv:^ envp:^ :}
   [: [: pathz argv envp PROCESS:CAPTURE-NULL-INPUT
         PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD PROC-SPAWN-ARGV-ENV-RAW ;]
      [: PROC-ARGV-ENV-RESET ;] finally ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ( ^u8 ^a ^a -- )
   {: pathz:^ argv:^ envp:^ :}
   [: [: pathz argv envp PROCESS:IN-R @ >FD
         PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD PROC-SPAWN-ARGV-ENV-RAW ;]
      [: PROC-ARGV-ENV-RESET ;] finally ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

: RUN-ARGV-ENV-CAPTURE ( str ^u8 len ^u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:^ pathu out:^ outcap err:^ errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      timeout PROCESS:CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CAPTURE ;]
   [: PROC-ARGV-ENV-RESET ;] finally
   out outcap err errcap PROCESS:RUN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CAPTURE-OUTCOME ( str ^u8 len ^u8 len ms -- len len PROCESS:ending )
   {: path:^ pathu out:^ outcap err:^ errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      timeout PROCESS:CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CAPTURE ;]
   [: PROC-ARGV-ENV-RESET ;] finally
   out outcap err errcap PROCESS:RUN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

: RUN-ARGV-ENV-STDIN-CAPTURE ( str str ^u8 len ^u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:^ pathu in:^ inu out:^ outcap err:^ errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      inu PROCESS:CAPTURE-CHECK-STDIN
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ;]
   [: PROC-ARGV-ENV-RESET ;] finally
   in inu out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ( str str ^u8 len ^u8 len ms -- len len PROCESS:ending )
   {: path:^ pathu in:^ inu out:^ outcap err:^ errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      inu PROCESS:CAPTURE-CHECK-STDIN
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ;]
   [: PROC-ARGV-ENV-RESET ;] finally
   in inu out outcap err errcap
   PROCESS:RUN-STDIN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

\ The child's stdout goes to storage that grows to what it writes: SINK makes
\ room and answers the first byte (src/lib/process.f). The length answered is
\ what the child wrote.
: RUN-ARGV-ENV-STDIN-GROWING-CAPTURE-OUTCOME ( str str [ n -- ^u8 ] ^u8 len ms -- len len PROCESS:ending )
   {: path:^ pathu in:^ inu sink err:^ errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      inu PROCESS:CAPTURE-CHECK-STDIN
      errcap PROCESS:CAPTURE-CHECK-CAP
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ;]
   [: PROC-ARGV-ENV-RESET ;] finally
   in inu sink err errcap PROCESS:RUN-STDIN-GROWING-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

\ Both streams use independent caller-owned growing storage. Sinks have the
\ same contract as the stdout sink above; their buffers stay with the caller
\ on every outcome, including a timeout or a nonzero child completion.
: RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME ( str str [ n -- ^u8 ] [ n -- ^u8 ] ms -- len len PROCESS:ending )
   {: path:^ pathu in:^ inu out err timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      inu PROCESS:CAPTURE-CHECK-STDIN
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ;]
   [: PROC-ARGV-ENV-RESET ;] finally
   in inu out err PROCESS:RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

: PROC-HAS-SLASH? ( str -- bool )
   PROC-PATH-SLASH STR:INDEX-OF MATCH option
     none OF PROC-ENV-FALSE ENDOF
     some OF drop PROC-ENV-TRUE ENDOF
   ;MATCH ;

: PROC-EXECUTABLE? ( str -- bool )
   FS:EXECUTABLE? ;

: PROC-COPY-PATH ( str ^u8 -- n ) {: a:^ u dst:^ :}
   u 0 < if E-PROC-PATH throw then
   u PATH-CAP > if E-PROC-PATH throw then
   a dst u BYTE-COPY
   u ;

: PROC-JOIN-PATH-SEG ( str str ^u8 -- n )
   {: seg:^ segu cmd:^ cmdu dst:^ :}
   segu 0= if
      s" ." cmd cmdu dst FS:JOIN-PATH
   else
      seg segu cmd cmdu dst FS:JOIN-PATH
   then ;

: PROC-TRY-PATH-SEG ( str str ^u8 -- option<n> )
   {: seg:^ segu cmd:^ cmdu dst:^ :}
   seg segu cmd cmdu dst PROC-JOIN-PATH-SEG {: gotu :}
   dst gotu PROC-EXECUTABLE? if gotu OPTION:SOME exit then
   OPTION:NONE ;

: FIND-EXECUTABLE-IN-PATH ( str str ^u8 -- option<n> )   \ SOME resolved length in dst, else NONE
   {: cmd:^ cmdu path:^ pathu dst:^ :}
   cmd cmdu PROC-HAS-SLASH? if
      cmd cmdu PROC-EXECUTABLE? if
         cmd cmdu dst PROC-COPY-PATH OPTION:SOME exit
      then
      OPTION:NONE exit
   then
   0 >OFF PROC-PATH-I !
   begin path pathu PROC-PATH-SEP PROC-PATH-I @ OFF>N STR:SPLIT-NEXT
   while
      >OFF PROC-PATH-I !
      cmd cmdu dst PROC-TRY-PATH-SEG MATCH option
        none OF ENDOF
        some OF OPTION:SOME exit ENDOF
      ;MATCH
   repeat
   drop 2drop OPTION:NONE ;

: FIND-EXECUTABLE ( str ^u8 -- option<n> ) {: cmd:^ cmdu:n dst:^ :}
   s" PATH" GETENV {: path:^ pathu :}
   pathu 0= if OPTION:NONE exit then
   cmd cmdu path pathu dst FIND-EXECUTABLE-IN-PATH ;

: RESOLVE-EXECUTABLE ( str ^u8 -- n )
   FIND-EXECUTABLE MATCH option
     none OF E-PROC-PATH throw ENDOF
     some OF ENDOF
   ;MATCH ;
