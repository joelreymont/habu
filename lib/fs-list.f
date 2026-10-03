\ fs-list.f - the entry names of one directory.
\
\ EACH hands every name except . and .. to a quotation in directory order.
\ NAMES writes the names into a caller buffer, newline-separated and in byte
\ order, so two listings of the same directory compare equal. Only the raw
\ open-rd, getdirentries64 and close primitives are involved; the record
\ decoding is shared with the walker in lib/fs.f.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state. A listing - its
\ descriptor, the base cookie getdirentries64 keeps, its cursor and its dirent
\ block - is one mapping that EACH or NAMES makes and gives back, the
\ descriptor closed, however the call ends. Nothing outlives a call, so any
\ number of tasks list at once and a quotation may list again inside EACH.
\ NAMES writes only into the caller's buffer.

require lib/errors.f
require lib/memory.f
require lib/fs.f

package FS-LIST
public

E-FS-LIST-OPEN constant E-OPEN
E-FS-LIST-READ constant E-READ
E-FS-LIST-ENTRY constant E-ENTRY
E-FS-LIST-CAPACITY constant E-CAPACITY

private

10 constant LF
4096 constant BLOCK-CAP

\ A listing's mapping: four cells, then the dirent block. FILLED is what the
\ last block read answered and OFFSET the next record in that block.
0 constant FD-CELL
1 constant BASE-CELL
2 constant FILLED-CELL
3 constant OFFSET-CELL
4 cells constant BLOCK-OFF
BLOCK-OFF BLOCK-CAP + constant LISTING-BYTES


: SLOT ( ptr u8 n -- ptr n ) {: ls:ptr idx:n :}
   ls CELL-VIEW idx cells + ;


: BLOCK ( ptr u8 -- ptr u8 )
   BLOCK-OFF + ;


\ A fresh mapping is zeroed, and descriptor 0 is stdin: the cell says no
\ descriptor until the open succeeds.
: MAP ( -- ptr u8 NUM:alloc-byte-len )
   LISTING-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: ls:ptr lsu :}
   -1 ls FD-CELL SLOT !
   ls lsu ;


: FINISH ( ptr u8 NUM:alloc-byte-len -- ) {: ls:ptr lsu :}
   ls FD-CELL SLOT @ {: fd:n :}
   fd 0 >= if fd close then
   ls lsu MEM:RELEASE-BYTES ;


: OPEN ( ptr u8 ptr u8 n -- ) {: ls:ptr path:ptr size:n :}
   path size FS-PATHZ open-rd {: handle:n :}
   handle 0 < if E-OPEN throw then
   handle ls FD-CELL SLOT ! ;


: READ-BLOCK ( ptr u8 -- bool ) {: ls:ptr :}
   ls FD-CELL SLOT @ ls BLOCK BLOCK-CAP ls BASE-CELL SLOT getdirentries64 {: got:n :}
   got 0 < if E-READ throw then
   got ls FILLED-CELL SLOT !
   0 ls OFFSET-CELL SLOT !
   got 0 > ;


\ The name of the record at OFFSET, which then moves past the record.
: RECORD ( ptr u8 -- ptr u8 n ) {: ls:ptr :}
   ls OFFSET-CELL SLOT @ {: at:n :}
   ls BLOCK at + {: ent:ptr :}
   ent FS-DIRENT-RECLEN {: rec:n :}
   rec 0 <= at rec + ls FILLED-CELL SLOT @ > or if E-ENTRY throw then
   ent FS-DIRENT-NAME-END rec > if E-ENTRY throw then
   at rec + ls OFFSET-CELL SLOT !
   ent FS-DIRENT-NAME ;


\ The next name except . and .., read from a new block when this one is spent;
\ FALSE under an empty name once the directory has no more.
: NEXT ( ptr u8 -- ptr u8 n bool ) {: ls:ptr :}
   begin
      ls OFFSET-CELL SLOT @ ls FILLED-CELL SLOT @ < if
         ls RECORD 2dup FS-SKIP-SELF-ENTRY? 0= if true exit then
         2drop
      else
         ls READ-BLOCK 0= if s" " false exit then
      then
   again ;


\ TRUE when a sorts before b in byte order.
: BEFORE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v min {: shared:n :}
   shared 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;


\ The end of the line that starts at offset at among the len bytes in dst.
: LINE-END ( ptr u8 n n -- n ) {: dst:ptr len:n at:n :}
   at begin dup len < if dup dst + c@ LF <> else false then while 1+ repeat ;


\ The offset in dst where name belongs among the len bytes of names written so
\ far.
: INSERT-AT ( ptr u8 n ptr u8 n -- n ) {: name:ptr size:n dst:ptr len:n :}
   0 begin dup len < while
      {: at:n :}
      dst len at LINE-END {: stop:n :}
      name size dst at + stop at - BEFORE? if at exit then
      stop 1+
   repeat
   len min ;


\ Writes name among the len bytes of names in dst and answers the new length.
: INSERT ( n ptr u8 n ptr u8 n -- n ) {: len:n name:ptr size:n dst:ptr cap:n :}
   size 0 ?do name i + c@ LF = if E-ENTRY throw then loop
   len 0 > if 1 else 0 then {: separator:n :}
   len size + separator + cap > if E-CAPACITY throw then
   name size dst len INSERT-AT {: at:n :}
   at len = if
      separator 0 <> if LF dst len + c! then
      name dst len + separator + size BYTE-COPY
   else
      len at - {: tail:n :}
      tail 0 ?do
         dst len 1- i - + c@ dst len 1- i - size + 1+ + c!
      loop
      name dst at + size BYTE-COPY
      LF dst at + size + c!
   then
   len size + separator + ;


\ The listing runs on the stack into these preserving words, so the catch
\ around each gives the mapping back however it leaves - a quotation-typed
\ local cannot be caught (lib/fs.f FS-WALK-RUN).
: EACH-RUN ( ptr u8 ptr u8 n [ ptr u8 n -- ] -- ptr u8 ptr u8 n [ ptr u8 n -- ] )
   {: ls:ptr path:ptr size:n q :}
   ls path size OPEN
   begin ls NEXT while q execute repeat 2drop
   ls path size q ;


: NAMES-RUN ( ptr u8 ptr u8 n ptr u8 n n -- ptr u8 ptr u8 n ptr u8 n n )
   {: ls:ptr path:ptr size:n dst:ptr cap:n len:n :}
   ls path size OPEN
   ls path size dst cap
   len begin ls NEXT while dst cap INSERT repeat 2drop ;

public

\ Hands each name except . and .. to the quotation, in directory order.
: EACH ( ptr u8 n [ ptr u8 n -- ] -- ) {: path:ptr size:n q :}
   MAP {: ls:ptr lsu :}
   ls path size q [: EACH-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls lsu FINISH
   code 0<> if code throw then ;


\ Writes the names except . and .. into the caller's buffer, newline-separated
\ and in byte order, and returns the length; names holding a newline are refused.
: NAMES ( ptr u8 n ptr u8 n -- n ) {: path:ptr size:n dst:ptr cap:n :}
   cap 0 < if E-CAPACITY throw then
   MAP {: ls:ptr lsu :}
   ls path size dst cap 0 [: NAMES-RUN ;] catch {: code:n :}
   ls lsu FINISH
   code 0<> if 2drop 2drop 2drop code throw then
   >r 2drop 2drop drop r> ;

;package
