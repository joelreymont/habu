\ secret/dir.f - SECRET:DIR-BIND, a read-only store over a directory holding
\ one file per item, as systemd's CREDENTIALS_DIRECTORY holds a unit's
\ credentials. Which store SECRET reads is the host's choice (STORE!); this
\ one serves the one service it was bound for, an item being the file the
\ account names, reads no environment variable, and throws E-ACCESS for
\ WRITE and ERASE. The directory is the host's and holds regular files: a
\ symlink item is followed, and an item that is not a regular file, a FIFO
\ included, is E-ACCESS. Contract: src/lib/secret.f.
\ STORAGE CLASS. PROCESS-WIDE: the bound directory and service, written by
\ DIR-BIND before use; one read's item and staged bytes, under DIR-LOCK.
require src/lib/secret.f
require src/lib/fs.f
require src/lib/log.f
require src/lib/span.f
require src/lib/string.f
require src/lib/task.f

package SECRET
private

PATH-CAP SPAN-BUFFER: DIR-PATH
variable DIR-PATH-U
DYNAMIC-BUFFER DIR-SERVICE u8
variable DIR-SERVICE-U
PATH-CAP SPAN-BUFFER: DIR-ITEM
variable DIR-ITEM-U
variable DIR-FD
variable DIR-SIZE
variable DIR-FOUND
\ The item's bytes, wiped before DIR-LOCK is released; the destination's
\ size bounds them.
DYNAMIC-BUFFER DIR-STAGED u8
variable DIR-STAGED-U
TASK:FACILITY DIR-LOCK
\ access(2) R_OK | X_OK: the directory can be listed and searched.
5 constant DIR-ACCESS

: DIR-PATH$ ( -- ptr u8 n ) DIR-PATH SPAN:$ drop DIR-PATH-U @ ;
: DIR-SERVICE$ ( -- ptr u8 n ) 0 DIR-SERVICE DIR-SERVICE-U @ ;
: DIR-ITEM$ ( -- ptr u8 n ) DIR-ITEM SPAN:$ drop DIR-ITEM-U @ ;


\ A file name in the directory: no slash, neither . nor ..
: DIR-PLAIN? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u FS:SKIP-SELF-ENTRY? if false exit then
   u 0 ?do a i + c@ FS:SLASH = if unloop false exit then loop
   true ;


\ The item path's length for an account of n bytes: FS:JOIN-PATH-INTO adds
\ a slash unless the directory ends in one.
: DIR-ITEM-LEN ( n -- n )
   DIR-PATH-U @ + DIR-PATH$ FS:JOIN-SEPARATED? 0= if 1+ then ;


: DIR-OPEN ( -- )
   DIR-ITEM$ FS:OPEN-SIZED bool>n DIR-FOUND ! DIR-SIZE ! FD>N DIR-FD ! ;


: DIR-READ-ITEM ( -- )
   DIR-FD @ >FD 0 DIR-STAGED DIR-STAGED-U @ FS:READ-SIZED ;


\ The cause's errno text; with none, what FS refused: a file not regular
\ (E-FS-STAT) or a size that changed while read (E-FS-IO).
: DIR-CAUSE$ ( n n -- ptr u8 n )
   {: code:n cause:n :}
   cause 0<> if cause FS:ERRNO$ exit then
   code E-FS-STAT = if s" not a regular file" exit then
   s" its size changed while it was read" ;


: DIR-LOG ( n n -- )
   {: code:n cause:n :}
   LOG:ERROR s" secret" LOG:LINE s" cannot read " LOG:TEXT+ DIR-ITEM$ LOG:TEXT+
   s" : " LOG:TEXT+ code cause DIR-CAUSE$ LOG:TEXT+ LOG:;LINE ;


\ Logs an FS fault on the item and throws it as the store's: ENAMETOOLONG is
\ E-CONFIG, E-FS-OPEN, E-FS-STAT and E-FS-IO are E-ACCESS, any other its own.
: DIR-REJECT ( n n -- )
   {: code:n cause:n :}
   code 0= if exit then
   code E-FS-OPEN = cause FS:ENAMETOOLONG = and if
      code cause DIR-LOG E-CONFIG throw
   then
   code E-FS-OPEN = code E-FS-STAT = or code E-FS-IO = or if
      code cause DIR-LOG E-ACCESS throw
   then
   code throw ;


\ Stage the opened item's bytes; a failed reservation closes the item.
\ DIR-STAGED holds at least one element, so index 0 is valid for an empty
\ item.
: DIR-STAGE ( -- )
   [: DIR-SIZE @ 1 max DIR-STAGED-RESERVE ;] catch {: code:n :}
   code 0<> if DIR-FD @ close code throw then
   DIR-SIZE @ DIR-STAGED-U !
   [: DIR-READ-ITEM ;] catch FS:ERRNO DIR-REJECT ;


: DIR-READ-STAGED ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: sa:ptr su:n aa:ptr au:n dst:SPAN:span<u8> :}
   sa su DIR-SERVICE$ STR= 0= if E-CONFIG throw then
   aa au DIR-PLAIN? 0= if E-CONFIG throw then
   au DIR-ITEM-LEN PATH-CAP > if E-CONFIG throw then
   DIR-PATH$ aa au DIR-ITEM FS:JOIN-PATH-INTO DIR-ITEM-U !
   [: DIR-OPEN ;] catch FS:ERRNO DIR-REJECT
   DIR-FOUND @ 0= if 0 false exit then
   DIR-SIZE @ dst SPAN:LEN > if DIR-FD @ close E-CAPACITY throw then
   DIR-STAGE
   0 DIR-STAGED DIR-STAGED-U @ dst SPAN:COPY
   DIR-STAGED-U @ true ;


: DIR-WIPE ( -- )
   DIR-STAGED-U @ 0= if exit then
   0 0 DIR-STAGED DIR-STAGED-U @ SPAN:MAKE SPAN:FILL
   0 DIR-STAGED-U ! ;


: DIR-UNLOCK ( -- ) DIR-WIPE DIR-LOCK TASK:RELEASE ;


: DIR-READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   DIR-LOCK TASK:GET
   [: DIR-READ-STAGED ;] [: DIR-UNLOCK ;] finally ;


: DIR-WRITE ( ptr u8 n ptr u8 n ptr u8 n -- )
   2drop 2drop 2drop E-ACCESS throw ;


: DIR-ERASE ( ptr u8 n ptr u8 n -- bool )
   2drop 2drop E-ACCESS throw ;


\ A directory past PATH-CAP or holding a NUL, or one this process cannot list
\ and search, is E-CONFIG.
: DIR-CHECK ( ptr u8 n -- )
   {: dir:ptr dir-u:n :}
   dir-u 0 < dir-u PATH-CAP > or if E-CONFIG throw then
   dir-u 0 ?do dir i + c@ 0= if E-CONFIG throw then loop
   dir dir-u FS:DIR? 0= if E-CONFIG throw then
   dir dir-u FS:>PATHZ DIR-ACCESS access 0<> if E-CONFIG throw then ;

public

\ Binds with STORE! the store over the directory, for the service given; a
\ directory DIR-CHECK refuses, or a service SECRET refuses, is E-CONFIG and
\ binds nothing. Run once, before anything reads a secret.
: DIR-BIND ( ptr u8 n ptr u8 n -- )
   {: dir:ptr dir-u:n svc:ptr svc-u:n :}
   svc svc-u INPUT
   dir dir-u DIR-CHECK
   svc-u 1 max DIR-SERVICE-RESERVE
   svc 0 DIR-SERVICE svc-u BYTE-COPY svc-u DIR-SERVICE-U !
   dir dir-u DIR-PATH SPAN:COPY dir-u DIR-PATH-U !
   DIR-LOCK TASK:FACILITY-INIT
   [: DIR-READ ;] [: DIR-WRITE ;] [: DIR-ERASE ;] STORE! ;

;package
