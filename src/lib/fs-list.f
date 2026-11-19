split-str
\ fs-list.f - the entries of one directory, through raw open-rd or an openat of
\ "." under the caller's descriptor, getdirentries64 and close; record decoding
\ is shared with the walker in src/lib/fs.f.
\ STORAGE CLASS. CALLER-OWNED: INIT binds an aligned caller span to one opaque
\ context, one listing at a time (re-entry or sharing mid-listing is
\ E-FS-LIST-ACTIVE, the first listing undisturbed); distinct contexts nest. The
\ convenience words release a private context after one call.

require src/lib/error.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/ffi-abi.f
require src/core/compile-reads.f

package FS-LIST
public

\ Semantic refusals clear FS:ERRNO. On Darwin a regular file opens for listing
\ and getdirentries64 then fails EINVAL: E-READ keeps that as its cause through
\ descriptor cleanup.
E-FS-LIST-OPEN constant E-OPEN
E-FS-LIST-READ constant E-READ
E-FS-LIST-ENTRY constant E-ENTRY
E-FS-LIST-CAPACITY constant E-CAPACITY
E-FS-LIST-ACTIVE constant E-ACTIVE

0 constant DT-UNKNOWN
4 constant DT-DIR
8 constant DT-REG
10 constant DT-LNK

private
4096 constant BLOCK-CAP

public
NEWTYPE context ^u8

2 cells BLOCK-CAP + constant STORAGE-BYTES

private

10 constant LF
-1 constant IDLE
-2 constant CLAIMED

CAST: >CONTEXT ( ^u8 -- context )
CAST: CONTEXT>PTR ( context -- ^u8 )

: FD-CELL ( ^u8 -- ^n ) CELL-VIEW ;
: BASE-CELL ( ^u8 -- ^n ) CELL-VIEW cell+ ;


: BLOCK ( ^u8 -- ^u8 )
   2 cells + ;


: MAP ( -- ^u8 NUM:alloc-byte-len )
   STORAGE-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES ;


: FINISH ( ^u8 -- ) {: ls:^ :}
   ls FD-CELL @ {: fd:n :}
   fd 0 >= if fd close then
   IDLE ls FD-CELL atomic! ;

: ACQUIRE ( ^u8 -- ) {: ls:^ :}
   IDLE CLAIMED ls FD-CELL atomic-cas IDLE <> if
      FS:CAUSE-CLEAR E-ACTIVE throw
   then
   0 ls BASE-CELL ! ;


: OPEN ( ^u8 ^u8 n -- ) {: ls:^ path:^ size:n :}
   path size FS:>PATHZ {: z:^ :}
   path size z COMPILE-READS:FILE+
   z open-rd {: handle:n :}
   handle 0 < if E-OPEN throw then
   handle ls FD-CELL ! ;

PROCESS-SYMBOLS
FUNCTION: OPEN-AT-CALL openat ( n ^u8 n -- i32 ) ;FUNCTION

\ "." under the caller's directory: an open file description of its own, so
\ the listing's offset is not the caller's.
: OPEN-AT ( ^u8 fd -- ) {: ls:^ dir:fd :}
   COMPILE-READS:MISS
   dir FD>N S\" .\z" drop FS:OPEN-DIR-FLAGS OPEN-AT-CALL {: handle:n :}
   handle 0 < if FFI:ERRNO FS:CAUSE! E-OPEN throw then
   handle ls FD-CELL ! ;


: READ-BLOCK ( ^u8 -- n ) {: ls:^ :}
   ls FD-CELL @ ls BLOCK BLOCK-CAP ls BASE-CELL getdirentries64 {: got:n :}
   got 0 < if E-READ throw then
   got ;


\ Validate the header before field reads, then validate the name inside rec.
: RECORD ( n n ^u8 -- n n ^u8 n u8 )
   {: filled:n at:n ls:^ :}
   filled at - FS:DIRENT-NAME-OFFSET < if
      FS:CAUSE-CLEAR E-ENTRY throw
   then
   ls BLOCK at + {: ent:^ :}
   ent FS:DIRENT-RECLEN {: rec:n :}
   rec FS:DIRENT-NAME-OFFSET <= rec filled at - > or if
      FS:CAUSE-CLEAR E-ENTRY throw
   then
   ent FS:DIRENT-NAME {: name:^ size:n :}
   size rec FS:DIRENT-NAME-OFFSET - > if FS:CAUSE-CLEAR E-ENTRY throw then
   filled at rec + name size ent FS:DIRENT-TYPE ;


\ The next name except . and .., read from a new block when this one is spent;
\ FALSE under an empty name once the directory has no more.
: NEXT ( n n ^u8 -- n n ^u8 n u8 bool ) {: ls:^ :}
   begin
      2dup > if
         ls RECORD >r 2dup FS:SKIP-SELF-ENTRY? 0= if r> true exit then
         2drop r> drop
      else
         2drop ls READ-BLOCK dup 0= if 0 s" " 0 false exit then 0
      then
   again ;


: BEFORE? ( str str -- bool ) {: a:^ u:n b:^ v:n :}
   u v min {: shared:n :}
   shared 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;


: LINE-END ( str n -- n ) {: dst:^ len:n at:n :}
   at begin dup len < if dup dst + c@ LF <> else false then while 1+ repeat ;


: INSERT-AT ( str str -- n ) {: name:^ size:n dst:^ len:n :}
   0 begin dup len < while
      {: at:n :}
      dst len at LINE-END {: stop:n :}
      name size dst at + stop at - BEFORE? if at exit then
      stop 1+
   repeat
   len min ;


: INSERT ( n ^u8 n ^u8 n -- n ) {: len:n name:^ size:n dst:^ cap:n :}
   size 0 ?do name i + c@ LF = if FS:CAUSE-CLEAR E-ENTRY throw then loop
   len 0 > if 1 else 0 then {: separator:n :}
   len size + separator + cap > if FS:CAUSE-CLEAR E-CAPACITY throw then
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


: TYPED-LOOP ( ^u8 [ str u8 -- ] -- ) {: ls:^ q :}
   0 0 begin ls NEXT while q execute repeat drop 2drop 2drop ;

\ The listing runs on the stack into these preserving words, so the catch
\ around each gives the mapping back however it leaves - a quotation-typed
\ local cannot be caught (src/lib/fs.f WALK-RUN).
: EACH-RUN ( ^u8 str [ str -- ] -- ^u8 str [ str -- ] )
   {: ls:^ path:^ size:n q :}
   ls path size OPEN
   0 0 begin ls NEXT while drop q execute repeat drop 2drop 2drop
   ls path size q ;

: EACH-TYPED-RUN ( ^u8 str [ str u8 -- ] -- ^u8 str [ str u8 -- ] )
   {: ls:^ path:^ size:n q :}
   ls path size OPEN
   ls q TYPED-LOOP
   ls path size q ;

: EACH-TYPED-AT-RUN ( ^u8 fd [ str u8 -- ] -- ^u8 fd [ str u8 -- ] )
   {: ls:^ dir:fd q :}
   ls dir OPEN-AT
   ls q TYPED-LOOP
   ls dir q ;

: NAMES-STEP ( n n n ^u8 ^u8 n -- n n n bool )
   {: len:n filled:n at:n ls:^ dst:^ cap:n :}
   filled at ls NEXT if
      drop len -rot dst cap INSERT -rot true
   else
      drop 2drop len -rot false
   then ;

: NAMES-RUN ( ^u8 str ^u8 n n -- ^u8 str str n )
   {: ls:^ path:^ size:n dst:^ cap:n len:n :}
   ls path size OPEN
   len 0 0 begin ls dst cap NAMES-STEP while repeat 2drop
   >r ls path size dst cap r> ;

public

\ Refuses a span shorter than STORAGE-BYTES or not cell-aligned with
\ E-FS-LIST-CAPACITY. The caller keeps the span alive for the context's
\ lifetime and never INITs one that is listing. A context is reusable after
\ success, an open or read failure, or a callback throw.
: INIT ( SPAN:span<u8> -- context )
   SPAN:$ {: ls:^ cap:n :}
   cap STORAGE-BYTES < ls NULL$ drop - 1 cells mod 0<> or if
      FS:CAUSE-CLEAR E-CAPACITY throw
   then
   IDLE ls FD-CELL !
   0 ls BASE-CELL !
   ls >CONTEXT ;

: EACH-IN ( context str [ str -- ] -- )
   {: ctx path:^ size:n q :}
   ctx CONTEXT>PTR {: ls:^ :}
   ls ACQUIRE
   ls path size q [: EACH-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   code 0<> if code throw then ;

: EACH-TYPED-IN ( context str [ str u8 -- ] -- )
   {: ctx path:^ size:n q :}
   ctx CONTEXT>PTR {: ls:^ :}
   ls ACQUIRE
   ls path size q [: EACH-TYPED-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   code 0<> if code throw then ;

\ EACH-TYPED-IN over a directory the caller opened and may search; the
\ caller's descriptor and offset are untouched. A refused "." is E-FS-LIST-OPEN.
: EACH-TYPED-AT ( context fd [ str u8 -- ] -- )
   {: ctx dir:fd q :}
   ctx CONTEXT>PTR {: ls:^ :}
   ls ACQUIRE
   ls dir q [: EACH-TYPED-AT-RUN ;] catch {: code:n :}
   2drop drop
   ls FINISH
   code 0<> if code throw then ;

: NAMES-IN ( context str ^u8 n -- n )
   {: ctx path:^ size:n dst:^ cap:n :}
   cap 0 < if FS:CAUSE-CLEAR E-CAPACITY throw then
   ctx CONTEXT>PTR {: ls:^ :}
   ls ACQUIRE
   ls path size dst cap 0 [: NAMES-RUN ;] catch {: code:n :}
   ls FINISH
   code 0<> if 2drop 2drop 2drop code throw then
   >r 2drop 2drop drop r> ;

\ Hands each name except . and .. to the quotation, in directory order.
: EACH ( ^u8 n [ ^u8 n -- ] -- ) {: path:^ size:n q :}
   MAP {: ls:^ lsu :}
   CLAIMED ls FD-CELL ! 0 ls BASE-CELL !
   ls path size q [: EACH-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   ls lsu MEM:RELEASE-BYTES
   code 0<> if code throw then ;

\ d_type passes unchanged: a filesystem may omit it, so a caller handles
\ DT-UNKNOWN. The name is borrowed from the listing's block only for the
\ callback.
: EACH-TYPED ( str [ str u8 -- ] -- ) {: path:^ size:n q :}
   MAP {: ls:^ lsu :}
   CLAIMED ls FD-CELL ! 0 ls BASE-CELL !
   ls path size q [: EACH-TYPED-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   ls lsu MEM:RELEASE-BYTES
   code 0<> if code throw then ;


\ The names except . and .. into the caller's buffer, newline-separated and in
\ byte order, so two listings of one directory compare equal; answers the
\ length. A name holding a newline is refused; a short buffer is E-FS-LIST-CAPACITY.
: NAMES ( ^u8 n ^u8 n -- n ) {: path:^ size:n dst:^ cap:n :}
   cap 0 < if FS:CAUSE-CLEAR E-CAPACITY throw then
   MAP {: ls:^ lsu :}
   CLAIMED ls FD-CELL ! 0 ls BASE-CELL !
   ls path size dst cap 0 [: NAMES-RUN ;] catch {: code:n :}
   ls FINISH
   ls lsu MEM:RELEASE-BYTES
   code 0<> if 2drop 2drop 2drop code throw then
   >r 2drop 2drop drop r> ;

;package
