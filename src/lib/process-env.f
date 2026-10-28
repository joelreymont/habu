\ process-env.f - checked argv/env process helpers and PATH lookup.
\
\ Kept separate from process-argv so old native seeds can still run the
\ build-fixpoint installer before this newer primitive exists.
\
\ STORAGE. The prepared environment and the inherited defaults are each a
\ table of row addresses and the row bytes. Both grow with the rows added, so
\ neither the parent's envp nor the caller's additions have a ceiling: a row
\ is refused only when a length is negative or its terminated size would not
\ fit a cell.
\
\ COST: one cell per row plus the terminating NULL per table, and the row
\ bytes in chunks of max(row size, $1000). Nothing is allocated until a
\ builder is used; a reset keeps the storage for the next spawn and
\ IMAGE-LIFECYCLE releases all of it.
\
\ ONCE ADDED, a row's bytes never move: chunks are never reallocated, only
\ added. An owned CMD run (src/lib/process-command.f INHERIT-Z) keeps the
\ default rows' addresses from its PREPARE to its spawn while another task
\ adds defaults.
\
\ STORAGE CLASS. PROCESS-WIDE: the environment table and its rows and the
\ inherited-default table and its rows are one set for the image, held by the
\ task that prepares a spawn until the spawn resets them; an owned CMD run uses
\ only the inherited defaults, read-only, when it inherits.

require src/lib/errors.f
require src/lib/string.f
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/fd-lock.f
require src/lib/process-argv.f
require src/lib/adt/option.f                 \ option<len> for the FIND-EXECUTABLE cluster (switchover wave A)

61 constant PROC-ENV-EQUAL
58 constant PROC-PATH-SEP
47 constant PROC-PATH-SLASH
variable PROC-ENV-N
variable PROC-ENV-I
variable PROC-PATH-I
variable PROC-ENV-DEF-N

\ Both tables hold row addresses, so they are declared pointer storage.
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

\ The PATH scan reads a split field as STR:SPLIT-NEXT states it: a NUM:byte-len
\ field length and a NUM:byte-off cursor. Both have to cross into the engine's
\ own len/off roles, because PROC-TRY-PATH-SEG, JOIN-PATH and the PROC-PATH-I
\ cursor cell are stated in those and that chain reaches this file's public entry.
\ Two checked role-to-role casts at this file's scope do the crossing, so no raw
\ cell exists in between and NUM is not reopened. Raw storage rejects nominal
\ roles; the casts preserve the engine's declared length and offset roles.
CAST: PROC-SEG>LEN ( NUM:byte-len -- len )
CAST: PROC-CURSOR>OFF ( NUM:byte-off -- off )

: PROC-ENV-TRUE ( -- bool )
   0 0= ;

: PROC-ENV-FALSE ( -- bool )
   0 0= 0= ;

: PROC-SPAWN-ARGV-ENV-RAW-HELD ( ptr u8 ptr a ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr envp:ptr infd outfd errfd :}
   pathz argv envp infd FD>N outfd FD>N errfd FD>N spawn-argv-env-io >PID ;

: PROC-SPAWN-ARGV-ENV-RAW ( ptr u8 ptr a ptr a fd fd fd -- pid )
   [: PROC-SPAWN-ARGV-ENV-RAW-HELD ;] FD-LOCK:WITH ;

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

\ The size after one more run of u bytes and its terminator, held at the
\ maximum cell instead of wrapping: `>LEN` is an unchecked cast, and a plain sum
\ of a length near that maximum would come back small. Both operands are
\ nonnegative.
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

\ The name scan reads ANY envp vector: the table below passes its own base and
\ count, and src/lib/process-command.f's caller-owned contexts pass theirs, so one
\ scan serves both and neither reaches into the other's rows.
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

\ Replace-or-add: exactly one row for the name survives. Unlike PROC-ENV+,
\ which appends and leaves duplicate-key resolution to the child's getenv,
\ this overwrites an existing row in place (e.g. one copied from the parent's
\ own environment by PROC-ENV-INHERIT-MISSING) and appends only when absent.
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

\ The spawn alone, resetting no process-wide staging: a caller that owns its own
\ argv and envp vectors (src/lib/process-command.f) passes them here and adopts the
\ child itself. The wrapper below keeps the staging reset for the callers that
\ built their vectors in this module's tables, and keeps it ahead of the failed
\ spawn's throw so a refused spawn cannot leave rows for the next one.
: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE-CORE ( ptr u8 ptr a ptr a -- pid )
   {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp PROC-IN-R @ >FD PROC-OUT-W @ >FD PROC-ERR-W @ >FD
   PROC-SPAWN-ARGV-ENV-RAW ;

: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 ptr a ptr a -- )
   PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE-CORE {: pid :}
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

\ RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME with the child's stdout in storage that
\ grows to what it writes: SINK makes room and answers the first byte
\ (src/lib/process.f, a capture whose stdout grows). The stdout length answered is
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

: PROC-TRY-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- option<len> )   \ SOME resolved length if executable, else NONE
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
     none OF STR-FALSE ENDOF                        \ no more PATH segments: end the scan
     some OF STR-SPLIT:UNMAKE {: seg:ptr segl:NUM:byte-len nx:NUM:byte-off :}
        nx PROC-CURSOR>OFF PROC-PATH-I !
        seg segl PROC-SEG>LEN cmd cmdu dst PROC-TRY-PATH-SEG MATCH option
          none OF ENDOF                             \ segment miss: try the next one
          some OF OPTION:SOME exit ENDOF            \ resolved: re-wrap and return
        ;MATCH
        STR-TRUE ENDOF
     ;MATCH
   while repeat
   OPTION:NONE ;

: FIND-EXECUTABLE ( ptr u8 len ptr u8 -- option<len> ) {: cmd:ptr cmdu:len dst:ptr :}   \ SOME resolved length via $PATH, else NONE
   s" PATH" GETENV {: path:ptr pathu :}
   pathu 0= if OPTION:NONE exit then
   cmd cmdu path pathu >LEN dst FIND-EXECUTABLE-IN-PATH ;

: RESOLVE-EXECUTABLE ( ptr u8 len ptr u8 -- len )
   FIND-EXECUTABLE MATCH option
     none OF E-PROC-PATH throw ENDOF
     some OF ENDOF
   ;MATCH ;
