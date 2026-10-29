\ process-env.f - checked argv/env process helpers and PATH lookup. The prepared
\ environment and the inherited defaults are tables of row addresses plus row
\ bytes in chunks of max(row size, $1000), grown without a ceiling.
\ STORAGE CLASS. PROCESS-WIDE: both tables are one set for the image, held by
\ the task preparing a spawn until the spawn resets them; an owned CMD run reads
\ only the defaults.

\ A row's bytes never move once added: process-command.f INHERIT-Z holds them
\ from PREPARE to spawn.

require src/lib/errors.f
require src/lib/string.f
require src/lib/string-roles.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/adt/option.f

61 constant PROC-ENV-EQUAL
58 constant PROC-PATH-SEP
47 constant PROC-PATH-SLASH
variable PROC-ENV-N
variable PROC-ENV-I
variable PROC-PATH-I
variable PROC-ENV-DEF-N

package PROC-ENV-ROWS
private

DYNAMIC-BUFFER TABLE ptr u8
DYNAMIC-BUFFER CHUNKS SPAN:span<u8>
PROC-CHUNKS:CONTROL-CELLS TYPED-BUFFER CONTROL n
DYNAMIC-BUFFER DEF-TABLE ptr u8
DYNAMIC-BUFFER DEF-CHUNKS SPAN:span<u8>
PROC-CHUNKS:CONTROL-CELLS TYPED-BUFFER DEF-CONTROL n
TYPED-VARIABLE REGISTERED bool
false REGISTERED !

: RELEASE ( -- )
   0 CONTROL [: drop CHUNKS ;] PROC-CHUNKS:FREE
   CHUNKS-RELEASE
   TABLE-RELEASE
   0 DEF-CONTROL [: drop DEF-CHUNKS ;] PROC-CHUNKS:FREE
   DEF-CHUNKS-RELEASE
   DEF-TABLE-RELEASE
   0 >COUNT PROC-ENV-N !
   0 >COUNT PROC-ENV-DEF-N !
   false REGISTERED ! ;

\ Register before allocation: refusal cannot leave an untracked mapping.
\ IMAGE-LIFECYCLE releases the storage; a reset keeps it for the next spawn.
: REGISTER ( -- )
   REGISTERED @ if exit then
   [: RELEASE ;] IMAGE-LIFECYCLE:REGISTER
   true REGISTERED ! ;

public

: SLOT ( idx -- ptr ptr u8 )
   {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   REGISTER
   idx IDX>N 1+ TABLE-RESERVE
   idx IDX>N TABLE ;

: DEF-SLOT ( idx -- ptr ptr u8 )
   {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   REGISTER
   idx IDX>N 1+ DEF-TABLE-RESERVE
   idx IDX>N DEF-TABLE ;

: BYTES ( n -- ptr u8 )
   REGISTER
   0 CONTROL [: drop CHUNKS ;] [: drop CHUNKS-RESERVE ;] PROC-CHUNKS:ROOM ;

: DEF-BYTES ( n -- ptr u8 )
   REGISTER
   0 DEF-CONTROL [: drop DEF-CHUNKS ;] [: drop DEF-CHUNKS-RESERVE ;] PROC-CHUNKS:ROOM ;

: REWIND ( -- )
   0 CONTROL PROC-CHUNKS:REWIND ;

: DEF-REWIND ( -- )
   0 DEF-CONTROL PROC-CHUNKS:REWIND ;

;package

\ STR:SPLIT-NEXT answers NUM:byte-len and NUM:byte-off; PROC-TRY-PATH-SEG,
\ FS:JOIN-PATH and the PROC-PATH-I cell take len and off, so two checked
\ role-to-role casts cross between them and NUM is not reopened.
CAST: PROC-SEG>LEN ( NUM:byte-len -- len )
CAST: PROC-CURSOR>OFF ( NUM:byte-off -- off )

: PROC-ENV-TRUE ( -- bool )
   0 0= ;

: PROC-ENV-FALSE ( -- bool )
   0 0= 0= ;

package PROC-RAW
public

: SPAWN-ARGV-ENV ( ptr u8 ptr a ptr a fd fd fd -- ptr u8 ptr a ptr a fd fd fd )
   {: pathz:ptr argv:ptr envp:ptr infd outfd errfd :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv ROWS PROCESS:ARGV-ROWS
   envp ROWS PROCESS:ENV-ROWS
   infd outfd errfd LAUNCH
   pathz argv envp infd outfd errfd ;

;package

: PROC-SPAWN-ARGV-ENV-RAW ( ptr u8 ptr a ptr a fd fd fd -- pid )
   [: PROC-RAW:SPAWN-ARGV-ENV ;] catch {: rc :}
   drop drop drop drop drop drop
   rc PROC-RAW:ANSWER ;

: PROC-ENV-RESET ( -- )
   0 >COUNT PROC-ENV-N !
   PROC-ENV-ROWS:REWIND ;

: PROC-ENV-DEFAULT-RESET ( -- )
   0 >COUNT PROC-ENV-DEF-N !
   PROC-ENV-ROWS:DEF-REWIND ;

: PROC-ENV-SLOT ( idx -- ptr ptr u8 )
   PROC-ENV-ROWS:SLOT ;

: PROC-ENV-DEF-SLOT ( idx -- ptr ptr u8 )
   PROC-ENV-ROWS:DEF-SLOT ;

\ Held at the maximum cell instead of wrapping: `>LEN` is an unchecked cast, and
\ a plain sum near that maximum would come back small. Both operands are nonnegative.
: PROC-ENV-NEED ( n len -- n ) {: used:n u :}
   u LEN>N MEM-MAX-N used - >= if MEM-MAX-N exit then
   used u LEN>N + 1 + ;

\ The size of one name=value row and its terminator, each length refused when
\ negative before it is summed and the sum refused when it would not fit a
\ cell. Every row writer checks it before a byte of the name is read.
: PROC-ENV-ROW-NEED ( len len -- n ) {: nameu valu :}
   nameu LEN>N 0 < if E-PROC-ENV throw then
   valu LEN>N 0 < if E-PROC-ENV throw then
   0 nameu PROC-ENV-NEED valu PROC-ENV-NEED {: need:n :}
   need MEM-MAX-N >= if E-PROC-ENV throw then
   need ;

: PROC-ENV-HAS-EQUAL? ( ptr u8 len -- bool ) {: a:ptr u :}
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;


\ A name is refused empty or holding `=` or NUL: every reader of a row stops at
\ its first NUL, so a NUL would make a shorter name of it.
: PROC-ENV-CHECK-NAME ( ptr u8 len -- )
   {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-ENV throw then
   0 begin dup u LEN>N < while
      dup a + c@ dup PROC-ENV-EQUAL = swap 0= or if E-PROC-ENV throw then
      1+
   repeat drop ;


: PROC-ENV-CHECK-ENTRY ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-ENV throw then
   a c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
   a u PROC-ENV-HAS-EQUAL? 0= if E-PROC-ENV throw then ;

\ The size is checked before the entry is read: a length past it is refused
\ unread, never scanned for its `=`.
: PROC-ENV-STORE-Z ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-ENV throw then
   0 u PROC-ENV-NEED {: need:n :}
   need MEM-MAX-N >= if E-PROC-ENV throw then
   a u PROC-ENV-CHECK-ENTRY
   a u need PROC-ENV-ROWS:BYTES need >LEN PROC-ZCOPY ;

: PROC-ENV-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   PROC-ENV-N @ COUNT>N 1+ >COUNT PROC-ENV-N ! ;

: PROC-ENV-DEF-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-DEF-N @ COUNT>N >IDX PROC-ENV-DEF-SLOT !
   PROC-ENV-DEF-N @ COUNT>N 1+ >COUNT PROC-ENV-DEF-N ! ;

: PROC-ENV-ENTRY+ ( ptr u8 len -- )
   PROC-ENV-STORE-Z PROC-ENV-INSTALL-Z ;

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

\ Reads any envp vector: this file's table and process-command.f's caller-owned
\ contexts each pass their own base and count.
: PROC-ENV-VEC-NAME? ( n ptr u8 len ptr ptr u8 -- bool ) {: i:n a:ptr u:len tab:ptr :}
   i cells tab + @ {: z:ptr :}
   a u z z ZLEN >LEN PROC-ENV-SAME-NAME? ;

: PROC-ENV-VEC-NAME-IDX ( ptr u8 len ptr ptr u8 n -- n ) {: a:ptr u:len tab:ptr rows:n :}
   0 begin dup rows < while
      dup a u tab PROC-ENV-VEC-NAME? if exit then
      1+
   repeat drop -1 ;

: PROC-ENV-VEC-HAS-NAME? ( ptr u8 len ptr ptr u8 n -- bool )
   PROC-ENV-VEC-NAME-IDX 0 >= ;

: PROC-ENV-NAME-IDX ( ptr u8 len -- n ) {: a:ptr u:len :}
   a u 0 >IDX PROC-ENV-SLOT PROC-ENV-N @ COUNT>N PROC-ENV-VEC-NAME-IDX ;

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

\ One name=value row in the bytes BYTES answers for its size.
: PROC-ENV-ROW ( ptr u8 len ptr u8 len [ n -- ptr u8 ] -- ptr u8 )
   {: name:ptr nameu:len val:ptr valu:len bytes :}
   nameu valu PROC-ENV-ROW-NEED {: need:n :}
   name nameu PROC-ENV-CHECK-NAME
   need bytes execute {: dst:ptr :}
   name dst nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL dst nameu LEN>N + c!
   val dst nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 dst nameu LEN>N + 1 + valu LEN>N + c!
   dst ;

: PROC-ENV-ROW-Z ( ptr u8 len ptr u8 len -- ptr u8 )
   [: PROC-ENV-ROWS:BYTES ;] PROC-ENV-ROW ;

: PROC-ENV+ ( ptr u8 len ptr u8 len -- )
   PROC-ENV-ROW-Z PROC-ENV-INSTALL-Z ;

\ Exactly one row for the name survives: an existing row (one copied by
\ PROC-ENV-INHERIT-MISSING, say) is overwritten in place. PROC-ENV+ appends and
\ leaves duplicate keys to the child's getenv.
: PROC-ENV-SET ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   nameu valu PROC-ENV-ROW-NEED drop
   name nameu PROC-ENV-NAME-IDX {: i:n :}
   i 0 < if name nameu val valu PROC-ENV+ exit then
   name nameu val valu PROC-ENV-ROW-Z i >IDX PROC-ENV-SLOT ! ;

: PROC-ENV-DEFAULT+ ( ptr u8 len ptr u8 len -- )
   [: PROC-ENV-ROWS:DEF-BYTES ;] PROC-ENV-ROW PROC-ENV-DEF-INSTALL-Z ;

\ The envp array the child receives ends at a null entry; the declared null
\ pointer spells it, because the table holds `ptr u8` and not a bare cell. The
\ base is fetched after the terminator is stored, which may move the table.
: PROC-ENV-PREPARE ( -- ptr ptr u8 )
   NULL-PTR PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   0 >IDX PROC-ENV-SLOT ;

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
   pathz argv envp PROC-CAPTURE-NULL-INPUT PROC-OUT-W @ >FD PROC-ERR-W @ >FD PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

\ The staging is reset ahead of the failed spawn's throw, so a refused spawn
\ cannot leave rows for the next one.
: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 ptr a ptr a -- )
   {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp PROC-IN-R @ >FD PROC-OUT-W @ >FD PROC-ERR-W @ >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PROC-CAPTURE-ADOPT-SPAWN ;

: RUN-ARGV-ENV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: RUN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
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

: RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
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

\ The child's stdout goes to storage that grows to what it writes: SINK makes
\ room and answers the first byte (src/lib/process.f). The length answered is
\ what the child wrote.
: RUN-ARGV-ENV-STDIN-GROWING-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len [ n -- ptr u8 ] ptr u8 len ms -- len len outcome )
   {: path:ptr pathu in:ptr inu sink err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   errcap PROC-CAPTURE-CHECK-CAP
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu sink err errcap PROC-RUN-STDIN-GROWING-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

\ Both streams use independent caller-owned growing storage. Sinks have the
\ same contract as the stdout sink above; their buffers stay with the caller
\ on every outcome, including a timeout or a nonzero child completion.
: RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len [ n -- ptr u8 ] [ n -- ptr u8 ] ms -- len len outcome )
   {: path:ptr pathu in:ptr inu out err timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu out err PROC-RUN-STDIN-GROWING-BOTH-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: PROC-HAS-SLASH? ( ptr u8 len -- bool )
   LEN>N STR:LENGTH PROC-PATH-SLASH STR:INDEX-OF MATCH option
     none OF PROC-ENV-FALSE ENDOF
     some OF drop PROC-ENV-TRUE ENDOF
   ;MATCH ;

: PROC-EXECUTABLE? ( ptr u8 len -- bool )
   LEN>N FS:EXECUTABLE? ;

: PROC-COPY-PATH ( ptr u8 len ptr u8 -- len ) {: a:ptr u dst:ptr :}
   u LEN>N 0 < if E-PROC-PATH throw then
   u LEN>N PATH-CAP > if E-PROC-PATH throw then
   a dst u LEN>N BYTE-COPY
   u ;

: PROC-JOIN-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- len )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   segu LEN>N 0= if
      s" ." cmd cmdu LEN>N dst FS:JOIN-PATH >LEN
   else
      seg segu LEN>N cmd cmdu LEN>N dst FS:JOIN-PATH >LEN
   then ;

: PROC-TRY-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- option<len> )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   seg segu cmd cmdu dst PROC-JOIN-PATH-SEG {: gotu :}
   dst gotu PROC-EXECUTABLE? if gotu OPTION:SOME exit then
   OPTION:NONE ;

: FIND-EXECUTABLE-IN-PATH ( ptr u8 len ptr u8 len ptr u8 -- option<len> )   \ SOME resolved length in dst, else NONE
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu PROC-HAS-SLASH? if
      cmd cmdu PROC-EXECUTABLE? if
         cmd cmdu dst PROC-COPY-PATH OPTION:SOME exit
      then
      OPTION:NONE exit
   then
   0 >OFF PROC-PATH-I !
   begin path pathu LEN>N STR:LENGTH PROC-PATH-SEP PROC-PATH-I @ OFF>N STR:OFFSET STR:SPLIT-NEXT MATCH option
     none OF STR-FALSE ENDOF
     some OF STR-SPLIT:UNMAKE {: seg:ptr segl:NUM:byte-len nx:NUM:byte-off :}
        nx PROC-CURSOR>OFF PROC-PATH-I !
        seg segl PROC-SEG>LEN cmd cmdu dst PROC-TRY-PATH-SEG MATCH option
          none OF ENDOF
          some OF OPTION:SOME exit ENDOF
        ;MATCH
        STR-TRUE ENDOF
     ;MATCH
   while repeat
   OPTION:NONE ;

: FIND-EXECUTABLE ( ptr u8 len ptr u8 -- option<len> ) {: cmd:ptr cmdu:len dst:ptr :}
   s" PATH" GETENV {: path:ptr pathu :}
   pathu 0= if OPTION:NONE exit then
   cmd cmdu path pathu >LEN dst FIND-EXECUTABLE-IN-PATH ;

: RESOLVE-EXECUTABLE ( ptr u8 len ptr u8 -- len )
   FIND-EXECUTABLE MATCH option
     none OF E-PROC-PATH throw ENDOF
     some OF ENDOF
   ;MATCH ;
