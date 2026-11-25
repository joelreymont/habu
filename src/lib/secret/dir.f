\ secret/dir.f - SECRET:DIR-BIND, a read-only store over a directory holding
\ one file per item, as systemd's CREDENTIALS_DIRECTORY holds a unit's
\ credentials. Which store SECRET reads is the host's choice (STORE!); this
\ one serves the one service it was bound for, an item being the file the
\ account names, reads no environment variable, and throws E-ACCESS for
\ WRITE and ERASE. The directory is the host's and holds regular files: a
\ symlink item is followed, and an item that is not a regular file, a FIFO
\ included, is E-ACCESS. Contract: src/lib/secret.f.
\ STORAGE CLASS. PROCESS-WIDE: the bound directory and service, written by
\ DIR-BIND before use; one read's item path, descriptor, size and found flag,
\ under DIR-LOCK.
require src/lib/secret.f
require src/lib/fs.f
require src/lib/log.f
require src/lib/span.f
require src/lib/string.f
require src/lib/task.f
require src/lib/vector.f

package SECRET
private

create DIR-PATH-ROW PATH-CAP SCRATCH-ALLOT
: DIR-PATH ( -- SPAN:span<u8> ) DIR-PATH-ROW PATH-CAP SPAN:MAKE ;
SCRATCH-VARIABLE DIR-PATH-U
DYNAMIC-BUFFER DIR-SERVICE u8
SCRATCH-VARIABLE DIR-SERVICE-U
create DIR-ITEM-ROW PATH-CAP SCRATCH-ALLOT
: DIR-ITEM ( -- SPAN:span<u8> ) DIR-ITEM-ROW PATH-CAP SPAN:MAKE ;
SCRATCH-VARIABLE DIR-ITEM-U
SCRATCH-VARIABLE DIR-FD
SCRATCH-VARIABLE DIR-SIZE
SCRATCH-VARIABLE DIR-FOUND
TASK:FACILITY DIR-LOCK
\ access(2) R_OK | X_OK: the directory can be listed and searched.
5 constant DIR-ACCESS

: DIR-PATH$ ( -- str ) DIR-PATH SPAN:$ STR:DATA DIR-PATH-U @ >str ;
: DIR-SERVICE$ ( -- str ) 0 DIR-SERVICE DIR-SERVICE-U @ >str ;
: DIR-ITEM$ ( -- str ) DIR-ITEM SPAN:$ STR:DATA DIR-ITEM-U @ >str ;


\ A file name in the directory: no slash, neither . nor ..
: DIR-PLAIN? ( str -- bool )
   {: s:str :}
   s FS:SKIP-SELF-ENTRY? if false exit then
   s STR:LEN 0 ?do s STR:DATA i + c@ FS:SLASH = if unloop false exit then loop
   true ;


\ The item path's length for an account of n bytes: FS:JOIN-PATH-INTO adds
\ a slash unless the directory ends in one.
: DIR-ITEM-LEN ( n -- n )
   DIR-PATH-U @ + DIR-PATH$ FS:JOIN-SEPARATED? 0= if 1+ then ;


: DIR-OPEN ( -- )
   DIR-ITEM$ FS:OPEN-SIZED bool>n DIR-FOUND ! DIR-SIZE ! FD>N DIR-FD ! ;


\ The cause's errno text; with none, what FS refused: a file not regular
\ (E-FS-STAT) or a size that changed while read (E-FS-IO).
: DIR-CAUSE$ ( n n -- str )
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


\ Appends the opened item straight into the vector, its room reserved before the
\ first byte is read; a failed reservation closes the item.
: DIR-APPEND ( ^VEC:vector<u8> -- )
   {: dst:^ :}
   dst VEC:COUNT {: old:n :}
   [: dst old DIR-SIZE @ + VEC:ENSURE ;] catch {: code:n :}
   code 0<> if DIR-FD @ close code throw then
   dst VEC:BYTES STR:DATA old + {: at:^ :}
   [: DIR-FD @ >FD at DIR-SIZE @ FS:READ-SIZED ;] catch
   FS:ERRNO DIR-REJECT ;


: DIR-READ-LOCKED ( str str ^VEC:vector<u8> -- bool )
   {: ss:str as:str dst:^ :}
   ss DIR-SERVICE$ STR:= 0= if E-CONFIG throw then
   as DIR-PLAIN? 0= if E-CONFIG throw then
   as STR:LEN DIR-ITEM-LEN PATH-CAP > if E-CONFIG throw then
   DIR-PATH$ as DIR-ITEM FS:JOIN-PATH-INTO DIR-ITEM-U !
   [: DIR-OPEN ;] catch FS:ERRNO DIR-REJECT
   DIR-FOUND @ 0= if false exit then
   dst DIR-APPEND
   true ;


: DIR-READ ( str str ^VEC:vector<u8> -- bool )
   DIR-LOCK TASK:GET
   [: DIR-READ-LOCKED ;] [: DIR-LOCK TASK:RELEASE ;] finally ;


: DIR-WRITE ( str str str -- )
   drop drop drop E-ACCESS throw ;


: DIR-ERASE ( str str -- bool )
   drop drop E-ACCESS throw ;


\ Within PATH-CAP and holding no NUL: a path the host takes and a line shows.
: DIR-TEXT? ( str -- bool )
   {: dir:str :}
   dir STR:LEN 0 < dir STR:LEN PATH-CAP > or if false exit then
   dir STR:LEN 0 ?do dir STR:DATA i + c@ 0= if unloop false exit then loop
   true ;


\ Opens a refused bind's ERROR line up to its reason, which DIR-REFUSED ends.
: DIR-REFUSING ( str -- )
   {: dir:str :}
   LOG:ERROR s" secret" LOG:LINE
   s" cannot bind the directory store" LOG:TEXT+
   dir DIR-TEXT? if s"  at " LOG:TEXT+ dir LOG:TEXT+ then
   s" : " LOG:TEXT+ ;


: DIR-REFUSED ( str -- ) LOG:TEXT+ LOG:;LINE E-CONFIG throw ;


\ A directory past PATH-CAP or holding a NUL, or one this process cannot list
\ and search, is E-CONFIG, logged as one ERROR line tagged secret.
: DIR-CHECK ( str -- )
   {: dir:str :}
   dir STR:LEN 0 < dir STR:LEN PATH-CAP > or if
      dir DIR-REFUSING s" a path of " LOG:TEXT+
      dir STR:LEN LOG:INT+ s"  bytes" DIR-REFUSED
   then
   dir DIR-TEXT? 0= if
      dir DIR-REFUSING s" its path holds a NUL byte" DIR-REFUSED
   then
   dir FS:DIR? 0= if
      dir DIR-REFUSING s" no directory is there" DIR-REFUSED
   then
   dir FS:>PATHZ DIR-ACCESS access 0<> if
      FS:ERRNO dir DIR-REFUSING FS:ERRNO$ DIR-REFUSED
   then ;

public

\ Binds with STORE! the store over the directory for the service, once, before
\ any read; a refusal binds nothing: E-CONFIG, one ERROR line tagged secret.
: DIR-BIND ( str str -- )
   {: dir:str svc:str :}
   [: svc INPUT ;] catch {: code:n :}
   code E-CONFIG = if
      dir DIR-REFUSING
      s" the service is empty or not text" DIR-REFUSED
   then
   code 0<> if code throw then
   dir DIR-CHECK
   svc STR:LEN 1 max DIR-SERVICE-RESERVE
   svc STR:DATA 0 DIR-SERVICE svc STR:LEN BYTE-COPY svc STR:LEN DIR-SERVICE-U !
   dir DIR-PATH SPAN:COPY dir STR:LEN DIR-PATH-U !
   DIR-LOCK TASK:FACILITY-INIT
   [: DIR-READ ;] [: DIR-WRITE ;] [: DIR-ERASE ;] STORE! ;

;package
