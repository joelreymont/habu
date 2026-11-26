\ fs-list.f - the entries of one directory, through raw open-rd or an openat of
\ "." under the caller's descriptor, getdirentries64 and close; record decoding
\ is shared with the walker in src/lib/fs.f.
\ STORAGE CLASS. Each word lends a private listing record through
\ MEM:WITH-RECORD for the one call.

require src/lib/error.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/ffi-abi.f
require src/core/compile-reads.f

package FS-LIST
public

\ Semantic rejections clear FS:ERRNO. On Darwin a regular file opens for listing
\ and getdirentries64 then fails EINVAL: E-READ keeps that as its cause through
\ descriptor cleanup.
E-FS-LIST-OPEN constant E-OPEN
E-FS-LIST-READ constant E-READ
E-FS-LIST-ENTRY constant E-ENTRY
E-FS-LIST-CAPACITY constant E-CAPACITY

0 constant DT-UNKNOWN
4 constant DT-DIR
8 constant DT-REG
10 constant DT-LNK

private
\ The kernel fills block with getdirentries64's records and base
\ with its cookie.
STRUCTURE listing DERIVE addr
   FIELD fd n
   FIELD base n
   BUFFER block 4096
;STRUCTURE

10 constant LF
-2 constant CLAIMED


: FINISH ( ^listing -- ) {: ls:^ :}
   ls LISTING-FD @ {: fd:n :}
   fd 0 >= if fd close then ;


: OPEN ( ^listing str -- ) {: ls:^ path:str :}
   path FS:>PATHZ {: z:^ :}
   path z COMPILE-READS:FILE+
   z open-rd {: handle:n :}
   handle 0 < if E-OPEN throw then
   handle ls LISTING-FD ! ;

PROCESS-SYMBOLS
FUNCTION: OPEN-AT-CALL openat ( n ^u8 n -- i32 ) ;FUNCTION

\ "." under the caller's directory: an open file description of its own, so
\ the listing's offset is not the caller's.
: OPEN-AT ( ^listing fd -- ) {: ls:^ dir:fd :}
   COMPILE-READS:MISS
   dir FD>N S\" .\z" STR:DATA FS:OPEN-DIR-FLAGS OPEN-AT-CALL
   {: handle:n :}
   handle 0 < if FFI:ERRNO FS:CAUSE! E-OPEN throw then
   handle ls LISTING-FD ! ;


: READ-BLOCK ( ^listing -- n ) {: ls:^ :}
   ls LISTING-FD @ ls LISTING-BLOCK ls LISTING-BASE getdirentries64 {: got:n :}
   got 0 < if E-READ throw then
   got ;


\ Validate the header before field reads, then validate the name inside rec.
: RECORD ( n n ^listing -- n n str u8 )
   {: filled:n at:n ls:^ :}
   filled at - FS:DIRENT-NAME-OFFSET < if
      FS:CAUSE-CLEAR E-ENTRY throw
   then
   ls LISTING-BLOCK SPAN:$ STR:DATA at + {: ent:^ :}
   ent FS:DIRENT-RECLEN {: rec:n :}
   rec FS:DIRENT-NAME-OFFSET <= rec filled at - > or if
      FS:CAUSE-CLEAR E-ENTRY throw
   then
   ent FS:DIRENT-NAME {: name:str :}
   name STR:LEN rec FS:DIRENT-NAME-OFFSET - > if FS:CAUSE-CLEAR E-ENTRY throw then
   filled at rec + name ent FS:DIRENT-TYPE ;


\ The next name except . and .., read from a new block when this one is spent;
\ FALSE under an empty name once the directory has no more.
: NEXT ( n n ^listing -- n n str u8 bool ) {: ls:^ :}
   begin
      2dup > if
         ls RECORD >r dup FS:SKIP-SELF-ENTRY? 0= if r> true exit then
         drop r> drop
      else
         2drop ls READ-BLOCK dup 0= if 0 s" " 0 false exit then 0
      then
   again ;


: BEFORE? ( str str -- bool ) {: a:str b:str :}
   a STR:DATA {: p:^ :}
   b STR:DATA {: q:^ :}
   a STR:LEN b STR:LEN min {: shared:n :}
   shared 0 ?do
      p i + c@ q i + c@ <> if p i + c@ q i + c@ < unloop exit then
   loop
   a STR:LEN b STR:LEN < ;


: LINE-END ( str n -- n ) {: dst:str at:n :}
   at begin dup dst STR:LEN < if dup dst STR:DATA + c@ LF <> else false then while 1+ repeat ;


: INSERT-AT ( str str -- n ) {: name:str dst:str :}
   0 begin dup dst STR:LEN < while
      {: at:n :}
      dst at LINE-END {: stop:n :}
      name dst STR:DATA at + stop at - >str BEFORE? if at exit then
      stop 1+
   repeat
   dst STR:LEN min ;


: INSERT ( n str ^u8 n -- n ) {: len:n name:str dst:^ cap:n :}
   name STR:LEN 0 ?do name STR:DATA i + c@ LF = if FS:CAUSE-CLEAR E-ENTRY throw then loop
   len 0 > if 1 else 0 then {: separator:n :}
   len name STR:LEN + separator + cap > if FS:CAUSE-CLEAR E-CAPACITY throw then
   name dst len >str INSERT-AT {: at:n :}
   at len = if
      separator 0 <> if LF dst len + c! then
      name STR:DATA dst len + separator + name STR:LEN BYTE-COPY
   else
      len at - {: tail:n :}
      tail 0 ?do
         dst len 1- i - + c@ dst len 1- i - name STR:LEN + 1+ + c!
      loop
      name STR:DATA dst at + name STR:LEN BYTE-COPY
      LF dst at + name STR:LEN + c!
   then
   len name STR:LEN + separator + ;


: TYPED-LOOP ( ^listing [ str u8 -- ] -- ) {: ls:^ q :}
   0 0 begin ls NEXT while q execute repeat drop drop 2drop ;

\ The listing runs on the stack into these preserving words, so the catch
\ around each closes the descriptor however it leaves - a quotation-typed
\ local cannot be caught (src/lib/fs.f WALK-RUN).
: EACH-RUN ( ^listing str [ str -- ] -- ^listing str [ str -- ] )
   {: ls:^ path:str q :}
   ls path OPEN
   0 0 begin ls NEXT while drop q execute repeat drop drop 2drop
   ls path q ;

: EACH-TYPED-RUN
   ( ^listing str [ str u8 -- ] -- ^listing str [ str u8 -- ] )
   {: ls:^ path:str q :}
   ls path OPEN
   ls q TYPED-LOOP
   ls path q ;

: EACH-TYPED-AT-RUN
   ( ^listing fd [ str u8 -- ] -- ^listing fd [ str u8 -- ] )
   {: ls:^ dir:fd q :}
   ls dir OPEN-AT
   ls q TYPED-LOOP
   ls dir q ;

: NAMES-STEP ( n n n ^listing ^u8 n -- n n n bool )
   {: len:n filled:n at:n ls:^ dst:^ cap:n :}
   filled at ls NEXT if
      drop len swap dst cap INSERT -rot true
   else
      drop drop len -rot false
   then ;

: NAMES-RUN ( ^listing str ^u8 n n -- ^listing str str n )
   {: ls:^ path:str dst:^ cap:n len:n :}
   ls path OPEN
   len 0 0 begin ls dst cap NAMES-STEP while repeat 2drop
   >r ls path dst cap >str r> ;

: EACH-WITH ( str [ str -- ] ^listing -- )
   {: path:str q ls:^ :}
   CLAIMED ls LISTING-FD !
   ls path q [: EACH-RUN ;] catch {: code:n :}
   drop drop drop
   ls FINISH
   code 0<> if code throw then ;

: EACH-TYPED-WITH ( str [ str u8 -- ] ^listing -- )
   {: path:str q ls:^ :}
   CLAIMED ls LISTING-FD !
   ls path q [: EACH-TYPED-RUN ;] catch {: code:n :}
   drop drop drop
   ls FINISH
   code 0<> if code throw then ;

: EACH-TYPED-AT-WITH ( fd [ str u8 -- ] ^listing -- )
   {: dir:fd q ls:^ :}
   CLAIMED ls LISTING-FD !
   ls dir q [: EACH-TYPED-AT-RUN ;] catch {: code:n :}
   2drop drop
   ls FINISH
   code 0<> if code throw then ;

: NAMES-WITH ( str ^u8 n ^listing -- n )
   {: path:str dst:^ cap:n ls:^ :}
   CLAIMED ls LISTING-FD !
   ls path dst cap 0 [: NAMES-RUN ;] catch {: code:n :}
   ls FINISH
   code 0<> if drop drop drop drop code throw then
   >r drop drop drop r> ;

public

\ Hands each name except . and .. to the quotation, in directory order.
: EACH ( str [ str -- ] -- )
   [type] listing [: EACH-WITH ;] MEM:WITH-RECORD ;

\ d_type passes unchanged: a filesystem may omit it, so a caller handles
\ DT-UNKNOWN. The name is borrowed from the listing's block only for the
\ callback.
: EACH-TYPED ( str [ str u8 -- ] -- )
   [type] listing [: EACH-TYPED-WITH ;] MEM:WITH-RECORD ;

\ EACH-TYPED over a directory the caller opened and may read and search; the
\ caller's descriptor and offset are untouched. A rejected "." is E-FS-LIST-OPEN.
: EACH-TYPED-AT ( fd [ str u8 -- ] -- )
   [type] listing [: EACH-TYPED-AT-WITH ;] MEM:WITH-RECORD ;


\ The names except . and .. into the caller's buffer, newline-separated and in
\ byte order, so two listings of one directory compare equal; answers the
\ length. A name holding a newline is rejected; a short buffer is E-FS-LIST-CAPACITY.
: NAMES ( str ^u8 n -- n ) {: path:str dst:^ cap:n :}
   cap 0 < if FS:CAUSE-CLEAR E-CAPACITY throw then
   path dst cap [type] listing [: NAMES-WITH ;] MEM:WITH-RECORD ;

;package
