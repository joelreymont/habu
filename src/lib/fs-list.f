\ fs-list.f - the entries of one directory, through raw open-rd, getdirentries64
\ and close; record decoding is shared with the walker in src/lib/fs.f.
\ STORAGE CLASS. CALLER-OWNED: INIT binds an aligned caller span to one opaque
\ context, one listing at a time (re-entry or sharing mid-listing is
\ E-FS-LIST-ACTIVE, the first listing undisturbed); distinct contexts nest. The
\ convenience words release a private context after one call.

require src/lib/errors.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/fs.f

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
NEWTYPE context

2 cells BLOCK-CAP + constant STORAGE-BYTES

private

10 constant LF
-1 constant IDLE
-2 constant CLAIMED

CAST: >CONTEXT ( ptr u8 -- context )
CAST: CONTEXT>PTR ( context -- ptr u8 )

: FD-CELL ( ptr u8 -- ptr n ) CELL-VIEW ;
: BASE-CELL ( ptr u8 -- ptr n ) CELL-VIEW cell+ ;


: BLOCK ( ptr u8 -- ptr u8 )
   2 cells + ;


: MAP ( -- ptr u8 NUM:alloc-byte-len )
   STORAGE-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES ;


: FINISH ( ptr u8 -- ) {: ls:ptr :}
   ls FD-CELL @ {: fd:n :}
   fd 0 >= if fd close then
   IDLE ls FD-CELL atomic! ;

: ACQUIRE ( ptr u8 -- ) {: ls:ptr :}
   IDLE CLAIMED ls FD-CELL atomic-cas IDLE <> if
      FS:CAUSE-CLEAR E-ACTIVE throw
   then
   0 ls BASE-CELL ! ;


: OPEN ( ptr u8 ptr u8 n -- ) {: ls:ptr path:ptr size:n :}
   path size FS:>PATHZ open-rd {: handle:n :}
   handle 0 < if E-OPEN throw then
   handle ls FD-CELL ! ;


: READ-BLOCK ( ptr u8 -- n ) {: ls:ptr :}
   ls FD-CELL @ ls BLOCK BLOCK-CAP ls BASE-CELL getdirentries64 {: got:n :}
   got 0 < if E-READ throw then
   got ;


\ Validate the header before field reads, then validate the name inside rec.
: RECORD ( n n ptr u8 -- n n ptr u8 n u8 )
   {: filled:n at:n ls:ptr :}
   filled at - FS:DIRENT-NAME-OFFSET < if
      FS:CAUSE-CLEAR E-ENTRY throw
   then
   ls BLOCK at + {: ent:ptr :}
   ent FS:DIRENT-RECLEN {: rec:n :}
   rec FS:DIRENT-NAME-OFFSET <= rec filled at - > or if
      FS:CAUSE-CLEAR E-ENTRY throw
   then
   ent FS:DIRENT-NAME {: name:ptr size:n :}
   size rec FS:DIRENT-NAME-OFFSET - > if FS:CAUSE-CLEAR E-ENTRY throw then
   filled at rec + name size ent FS:DIRENT-TYPE ;


\ The next name except . and .., read from a new block when this one is spent;
\ FALSE under an empty name once the directory has no more.
: NEXT ( n n ptr u8 -- n n ptr u8 n u8 bool ) {: ls:ptr :}
   begin
      2dup > if
         ls RECORD >r 2dup FS:SKIP-SELF-ENTRY? 0= if r> true exit then
         2drop r> drop
      else
         2drop ls READ-BLOCK dup 0= if 0 s" " 0 false exit then 0
      then
   again ;


: BEFORE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v min {: shared:n :}
   shared 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;


: LINE-END ( ptr u8 n n -- n ) {: dst:ptr len:n at:n :}
   at begin dup len < if dup dst + c@ LF <> else false then while 1+ repeat ;


: INSERT-AT ( ptr u8 n ptr u8 n -- n ) {: name:ptr size:n dst:ptr len:n :}
   0 begin dup len < while
      {: at:n :}
      dst len at LINE-END {: stop:n :}
      name size dst at + stop at - BEFORE? if at exit then
      stop 1+
   repeat
   len min ;


: INSERT ( n ptr u8 n ptr u8 n -- n ) {: len:n name:ptr size:n dst:ptr cap:n :}
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


\ The listing runs on the stack into these preserving words, so the catch
\ around each gives the mapping back however it leaves - a quotation-typed
\ local cannot be caught (src/lib/fs.f WALK-RUN).
: EACH-RUN ( ptr u8 ptr u8 n [ ptr u8 n -- ] -- ptr u8 ptr u8 n [ ptr u8 n -- ] )
   {: ls:ptr path:ptr size:n q :}
   ls path size OPEN
   0 0 begin ls NEXT while drop q execute repeat drop 2drop 2drop
   ls path size q ;

: EACH-TYPED-RUN ( ptr u8 ptr u8 n [ ptr u8 n u8 -- ] -- ptr u8 ptr u8 n [ ptr u8 n u8 -- ] )
   {: ls:ptr path:ptr size:n q :}
   ls path size OPEN
   0 0 begin ls NEXT while q execute repeat drop 2drop 2drop
   ls path size q ;

: NAMES-STEP ( n n n ptr u8 ptr u8 n -- n n n bool )
   {: len:n filled:n at:n ls:ptr dst:ptr cap:n :}
   filled at ls NEXT if
      drop len -rot dst cap INSERT -rot true
   else
      drop 2drop len -rot false
   then ;

: NAMES-RUN ( ptr u8 ptr u8 n ptr u8 n n -- ptr u8 ptr u8 n ptr u8 n n )
   {: ls:ptr path:ptr size:n dst:ptr cap:n len:n :}
   ls path size OPEN
   len 0 0 begin ls dst cap NAMES-STEP while repeat 2drop
   >r ls path size dst cap r> ;

public

\ Refuses a span shorter than STORAGE-BYTES or not cell-aligned with
\ E-FS-LIST-CAPACITY. The caller keeps the span alive for the context's
\ lifetime and never INITs one that is listing. A context is reusable after
\ success, an open or read failure, or a callback throw.
: INIT ( SPAN:span<u8> -- context )
   SPAN:$ {: ls:ptr cap:n :}
   cap STORAGE-BYTES < ls NULL$ drop - 1 cells mod 0<> or if
      FS:CAUSE-CLEAR E-CAPACITY throw
   then
   IDLE ls FD-CELL !
   0 ls BASE-CELL !
   ls >CONTEXT ;

: EACH-IN ( context ptr u8 n [ ptr u8 n -- ] -- )
   {: ctx path:ptr size:n q :}
   ctx CONTEXT>PTR {: ls:ptr :}
   ls ACQUIRE
   ls path size q [: EACH-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   code 0<> if code throw then ;

: EACH-TYPED-IN ( context ptr u8 n [ ptr u8 n u8 -- ] -- )
   {: ctx path:ptr size:n q :}
   ctx CONTEXT>PTR {: ls:ptr :}
   ls ACQUIRE
   ls path size q [: EACH-TYPED-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   code 0<> if code throw then ;

: NAMES-IN ( context ptr u8 n ptr u8 n -- n )
   {: ctx path:ptr size:n dst:ptr cap:n :}
   cap 0 < if FS:CAUSE-CLEAR E-CAPACITY throw then
   ctx CONTEXT>PTR {: ls:ptr :}
   ls ACQUIRE
   ls path size dst cap 0 [: NAMES-RUN ;] catch {: code:n :}
   ls FINISH
   code 0<> if 2drop 2drop 2drop code throw then
   >r 2drop 2drop drop r> ;

\ Hands each name except . and .. to the quotation, in directory order.
: EACH ( ptr u8 n [ ptr u8 n -- ] -- ) {: path:ptr size:n q :}
   MAP {: ls:ptr lsu :}
   CLAIMED ls FD-CELL ! 0 ls BASE-CELL !
   ls path size q [: EACH-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   ls lsu MEM:RELEASE-BYTES
   code 0<> if code throw then ;

\ d_type passes unchanged: a filesystem may omit it, so a caller handles
\ DT-UNKNOWN. The name is borrowed from the listing's block only for the
\ callback.
: EACH-TYPED ( ptr u8 n [ ptr u8 n u8 -- ] -- ) {: path:ptr size:n q :}
   MAP {: ls:ptr lsu :}
   CLAIMED ls FD-CELL ! 0 ls BASE-CELL !
   ls path size q [: EACH-TYPED-RUN ;] catch {: code:n :}
   drop 2drop drop
   ls FINISH
   ls lsu MEM:RELEASE-BYTES
   code 0<> if code throw then ;


\ The names except . and .. into the caller's buffer, newline-separated and in
\ byte order, so two listings of one directory compare equal; answers the
\ length. A name holding a newline is refused; a short buffer is E-FS-LIST-CAPACITY.
: NAMES ( ptr u8 n ptr u8 n -- n ) {: path:ptr size:n dst:ptr cap:n :}
   cap 0 < if FS:CAUSE-CLEAR E-CAPACITY throw then
   MAP {: ls:ptr lsu :}
   CLAIMED ls FD-CELL ! 0 ls BASE-CELL !
   ls path size dst cap 0 [: NAMES-RUN ;] catch {: code:n :}
   ls FINISH
   ls lsu MEM:RELEASE-BYTES
   code 0<> if 2drop 2drop 2drop code throw then
   >r 2drop 2drop drop r> ;

;package
