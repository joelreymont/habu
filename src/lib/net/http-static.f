\ http-static.f - one directory tree served from memory. STATIC-ROOT reads the
\ whole tree before the server starts, so the tables are written while no task
\ is live and a request costs no file system call; only a path wider than
\ PATH-CAP is refused. A file's Cache-Control and a missing path's fallback come
\ from the caller's rules, installed before START (src/lib/net/http.f
\ CACHE-RULE! and FALLBACK-RULE!); the defaults revalidate and never fall back.
require src/lib/net/http-router.f
require src/lib/fs.f
require src/lib/fs-identity.f
require src/lib/fs-list.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f

package HTTP
using MEM

private

$40 constant DIGEST-HEX-BYTES
$20 constant DIGEST-BYTES
$2E constant DOT-BYTE

\ A row per file and per directory, reserved as each is found. Growth may move
\ a row, so an entry keeps offsets into TEXT-ROW and a pointer into it is taken
\ again after the next append.
DYNAMIC-BUFFER FILE-PATH-AT n
DYNAMIC-BUFFER FILE-PATH-N n
DYNAMIC-BUFFER FILE-ETAG-AT n
DYNAMIC-BUFFER FILE-ETAG-N n
DYNAMIC-BUFFER FILE-BODY SPAN:span<u8>   \ the bytes one file was read into
DYNAMIC-BUFFER FILE-BODY-N n
DYNAMIC-BUFFER DIR-AT n
DYNAMIC-BUFFER DIR-N n
DYNAMIC-BUFFER DIR-UP n                  \ the parent's row; the root has none
DYNAMIC-BUFFER DIR-DEVICE n
DYNAMIC-BUFFER DIR-INODE n
DYNAMIC-BUFFER TEXT-ROW u8               \ the served paths and their ETags

PATH-CAP SPAN-BUFFER: PATH-BUF
PATH-CAP SPAN-BUFFER: URL-BUF
PATH-CAP SPAN-BUFFER: LIST-BUF        \ the directory being listed
\ SHA256-IN and SHA256>HEX write a digest of their own fixed size and take no
\ capacity, so these two are the library's bound and not a span's.
create DIGEST-RAW DIGEST-BYTES allot
create DIGEST-HEX DIGEST-HEX-BYTES allot

variable TEXT-U
variable FILE-COUNT
variable DIR-HEAD
variable DIR-TAIL
variable ROOT-U
PATH-CAP SPAN-BUFFER: ROOT-BUF


: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ SPAN:TAKE SPAN:$ ;


: TEXT+ ( ptr u8 n -- n ) {: bytes:ptr len:n :}
   TEXT-U @ {: at:n :}
   len 0= if at exit then
   at len + TEXT-ROW-RESERVE
   bytes len at TEXT-ROW len SPAN:MAKE SPAN:COPY
   at len + TEXT-U !
   at ;


\ The root directory's url is the one empty text, and it has no byte to point at.
: TEXT$ ( n n -- ptr u8 n ) {: at:n len:n :}
   len 0= if s" " exit then
   at TEXT-ROW len ;


: FILE-PATH$ ( n -- ptr u8 n ) {: at:n :}
   at FILE-PATH-AT @ at FILE-PATH-N @ TEXT$ ;


: FILE-ETAG$ ( n -- ptr u8 n ) {: at:n :}
   at FILE-ETAG-AT @ at FILE-ETAG-N @ TEXT$ ;


: FILE-BODY$ ( n -- ptr u8 n ) {: at:n :}
   at FILE-BODY @ at FILE-BODY-N @ SPAN:TAKE SPAN:$ ;



: CONTENT-TYPE$ ( ptr u8 n -- ptr u8 n ) {: path:ptr len:n :}
   path len s" .html" ENDS-WITH? if s" text/html; charset=utf-8" exit then
   path len s" .js" ENDS-WITH? if s" text/javascript; charset=utf-8" exit then
   path len s" .css" ENDS-WITH? if s" text/css; charset=utf-8" exit then
   path len s" .json" ENDS-WITH? if s" application/json" exit then
   path len s" .map" ENDS-WITH? if s" application/json" exit then
   path len s" .svg" ENDS-WITH? if s" image/svg+xml" exit then
   path len s" .png" ENDS-WITH? if s" image/png" exit then
   path len s" .jpg" ENDS-WITH? if s" image/jpeg" exit then
   path len s" .ico" ENDS-WITH? if s" image/vnd.microsoft.icon" exit then
   path len s" .woff2" ENDS-WITH? if s" font/woff2" exit then
   path len s" .txt" ENDS-WITH? if s" text/plain; charset=utf-8" exit then
   s" application/octet-stream" ;


\ The rows hold quotations, the checker's proven-quotation store
\ (src/lib/task.f): an xt in a plain cell would lose the effect.
1 TYPED-BUFFER CACHE-HOOK [ ptr u8 n -- ptr u8 n ]
1 TYPED-BUFFER FALLBACK-HOOK [ ptr u8 n -- ptr u8 n ]


: NO-CACHE ( ptr u8 n -- ptr u8 n )
   2drop s" no-cache" ;


: NO-FALLBACK ( ptr u8 n -- ptr u8 n )
   drop 0 ;


: DEFAULT-RULES ( -- )
   [: NO-CACHE ;] 0 CACHE-HOOK !
   [: NO-FALLBACK ;] 0 FALLBACK-HOOK ! ;

DEFAULT-RULES


: CACHE$ ( ptr u8 n -- ptr u8 n )
   0 CACHE-HOOK @ execute ;



: UNDER-ROOT ( ptr u8 n -- n ) {: rel:ptr len:n :}
   len 0= if ROOT$ {: base:ptr baselen:n :} base baselen PATH-BUF SPAN:COPY baselen exit then
   \ FS:JOIN-PATH checks its own PATH-CAP and takes no capacity: the bound is
   \ src/lib/fs.f's, and PATH-BUF is that wide.
   ROOT$ rel 1+ len 1- PATH-BUF SPAN:$ drop FS:JOIN-PATH ;


: ETAG-HASH-IN ( ptr u8 n ptr u8 -- ptr u8 n ptr u8 )
   {: bytes len:n hash :}
   hash bytes len DIGEST-RAW SHA256-IN
   DIGEST-RAW DIGEST-HEX SHA256>HEX
   bytes len hash ;


: ETAG-HASH ( ptr u8 n -- )
   {: bytes len:n :}
   SHA256-CTX-BYTES BYTES-ALLOC-LEN ALLOC-BYTES drop {: hash :}
   bytes len hash [: ETAG-HASH-IN ;] catch {: code:n :}
   2drop drop
   hash SHA256-CTX-BYTES BYTES-ALLOC-LEN RELEASE-BYTES
   code 0<> if code throw then ;


: ETAG-FOR ( ptr u8 n -- n n ) {: bytes:ptr len:n :}
   bytes len ETAG-HASH
   S\" \q" TEXT+ {: at:n :}
   DIGEST-HEX DIGEST-HEX-BYTES TEXT+ drop
   S\" \q" TEXT+ drop
   at DIGEST-HEX-BYTES 2 + ;


: FILE-ROWS ( n -- ) {: count:n :}
   count FILE-PATH-AT-RESERVE
   count FILE-PATH-N-RESERVE
   count FILE-ETAG-AT-RESERVE
   count FILE-ETAG-N-RESERVE
   count FILE-BODY-RESERVE
   count FILE-BODY-N-RESERVE ;


\ Reads the file into body and records all of entry at but the body itself.
\ The stack is kept, so the catch around it can give the body back.
: FILE-ENTRY ( ptr u8 n n SPAN:span<u8> n -- ptr u8 n n SPAN:span<u8> n )
   {: url:ptr urllen:n pathlen:n body at:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ body SPAN:$ FS:READ-ALL {: got:n :}
   url urllen TEXT+ at FILE-PATH-AT !
   urllen at FILE-PATH-N !
   got at FILE-BODY-N !
   body SPAN:$ drop got ETAG-FOR {: etag:n etaglen:n :}
   etag at FILE-ETAG-AT !
   etaglen at FILE-ETAG-N !
   url urllen pathlen body at ;


\ The entry counts only once it owns its body, so STATIC-CLOSE frees each body
\ once and a failed read frees its own.
: LOAD-FILE ( ptr u8 n -- ) {: url:ptr urllen:n :}
   FILE-COUNT @ {: at:n :}
   at 1+ FILE-ROWS
   url urllen UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ FS:FILE-SIZE 1 max
      BYTES-ALLOC-LEN ALLOC-SPAN {: body :}
   url urllen pathlen body at [: FILE-ENTRY ;] catch {: code:n :}
   2drop 2drop drop
   code 0<> if body FREE-SPAN code throw then
   body at FILE-BODY !
   at 1+ FILE-COUNT ! ;


: ANCESTOR? ( n n n -- bool ) {: at:n device:n inode:n :}
   at 0< if false exit then
   at DIR-DEVICE @ device = at DIR-INODE @ inode = and if true exit then
   at DIR-UP @ device inode recurse ;


\ A directory under row up. Symlinks are followed, so a linked directory is
\ served under its link's path. One whose device and inode match an ancestor's
\ is a cycle and refuses the root before it is walked; two links to one
\ directory that is not an ancestor are both served.
: DIR+ ( ptr u8 n n -- ) {: url:ptr urllen:n up:n :}
   url urllen UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ FS:PATH-IDENTITY {: device:n inode:n :}
   up device inode ANCESTOR? if E-STATIC throw then
   DIR-TAIL @ {: at:n :}
   at 1+ DIR-AT-RESERVE
   at 1+ DIR-N-RESERVE
   at 1+ DIR-UP-RESERVE
   at 1+ DIR-DEVICE-RESERVE
   at 1+ DIR-INODE-RESERVE
   url urllen TEXT+ at DIR-AT !
   urllen at DIR-N !
   up at DIR-UP !
   device at DIR-DEVICE !
   inode at DIR-INODE !
   at 1+ DIR-TAIL ! ;


: DIR$ ( n -- ptr u8 n ) {: at:n :}
   at DIR-AT @ at DIR-N @ TEXT$ ;


: URL-JOIN ( ptr u8 n ptr u8 n -- n ) {: base:ptr baselen:n name:ptr namelen:n :}
   baselen namelen + 1+ PATH-CAP > if E-STATIC throw then
   base baselen URL-BUF SPAN:COPY
   SLASH URL-BUF baselen SPAN:U8!
   name namelen URL-BUF baselen 1+ SPAN:SKIP SPAN:COPY
   baselen namelen + 1+ ;


: ENTRY ( ptr u8 n ptr u8 n -- ) {: base:ptr baselen:n name:ptr namelen:n :}
   base baselen name namelen URL-JOIN {: urllen:n :}
   URL-BUF urllen SPAN:TAKE SPAN:$ UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ FS:DIR? if
      URL-BUF urllen SPAN:TAKE SPAN:$ DIR-HEAD @ DIR+ exit
   then
   PATH-BUF pathlen SPAN:TAKE SPAN:$ FS:FILE? if URL-BUF urllen SPAN:TAKE SPAN:$ LOAD-FILE then ;


\ One name in the directory DIR-HEAD names. Its url is taken again for every
\ name, because the entry before may have grown TEXT-ROW and moved it.
: NAME-ENTRY ( ptr u8 n -- ) {: name:ptr namelen:n :}
   DIR-HEAD @ DIR$ name namelen ENTRY ;


\ The listing's path sits in LIST-BUF, which no entry writes.
: WALK-DIR ( -- )
   DIR-HEAD @ DIR$ UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ LIST-BUF SPAN:COPY
   LIST-BUF pathlen SPAN:TAKE SPAN:$ [: NAME-ENTRY ;] FS-LIST:EACH ;


: WALK ( -- )
   begin
      DIR-HEAD @ DIR-TAIL @ >= if exit then
      WALK-DIR
      DIR-HEAD @ 1+ DIR-HEAD !
   again ;



: DOT-DOT-AT? ( ptr u8 n n -- bool ) {: path:ptr len:n at:n :}
   at 2 + len > if false exit then
   path at + c@ DOT-BYTE <> if false exit then
   path at 1+ + c@ DOT-BYTE <> if false exit then
   at 0 > if path at 1- + c@ SLASH <> if false exit then then
   at 2 + len = if true exit then
   path at 2 + + c@ SLASH = ;


: TRAVERSAL? ( ptr u8 n -- bool ) {: path:ptr len:n :}
   len 0 ?do
      path len i DOT-DOT-AT? if true unloop exit then
   loop
   false ;


: SERVED-PATH$ ( n -- ptr u8 n ) {: idx:n :}
   idx SLOT-PATH$ {: path:ptr len:n :}
   path len s" /" STR= if s" /index.html" exit then
   path len ;


: FIND-FILE ( ptr u8 n -- n ) {: path:ptr len:n :}
   FILE-COUNT @ 0 ?do
      i FILE-PATH$ path len STR= if i unloop exit then
   loop
   -1 ;


\ The file that answers this request: the one the path names, or the one the
\ fallback rule names for it. An empty answer is no fallback.
: SERVED-FILE ( n -- n ) {: idx:n :}
   idx SERVED-PATH$ FIND-FILE {: at:n :}
   at 0 >= if at exit then
   idx SLOT-PATH$ 0 FALLBACK-HOOK @ execute {: fa:ptr fu:n :}
   fu 0= if -1 exit then
   fa fu FIND-FILE ;


\ If-None-Match may list several tags, and * matches any file.
: MATCHES-ETAG? ( n n -- bool ) {: idx:n at:n :}
   idx s" if-none-match" HEADER$ {: va:ptr vu:n found:bool :}
   found 0= if false exit then
   va vu TRIM s" *" STR= if true exit then
   va vu at FILE-ETAG$ CONTAINS? ;


: STATIC-HEADERS ( n n -- ) {: idx:n at:n :}
   idx RESPONSE-OF s" ETag" at FILE-ETAG$ HEADER!
   idx RESPONSE-OF s" Cache-Control" at FILE-PATH$ CACHE$ HEADER! ;


\ True when the static root answered the request, including the refusal of a
\ path that climbs: the table is keyed on whole paths, so a dot-dot segment
\ names no file, and answering it plainly beats letting it look like a typo.
: SERVE-STATIC ( n -- bool ) {: idx:n :}
   FILE-COUNT @ 0= if false exit then
   idx SLOT-METHOD$ s" GET" STR= idx SLOT-METHOD$ s" HEAD" STR= or 0= if false exit then
   idx SLOT-PATH$ TRAVERSAL? if
      idx RESPONSE-OF 400 s" bad_path"
      s" the path of a static file may not climb out of the root" ERROR!
      true exit
   then
   idx SERVED-FILE {: at:n :}
   at 0 < if false exit then
   idx at STATIC-HEADERS
   idx at MATCHES-ETAG? if
      idx RESPONSE-OF 304 STATUS!
      true exit
   then
   idx RESPONSE-OF 200 STATUS!
   idx RESPONSE-OF s" Content-Type" at FILE-PATH$ CONTENT-TYPE$ HEADER!
   idx RESPONSE-OF at FILE-BODY$ BODY!
   true ;


public

: STATIC-CLOSE ( -- )
   FILE-COUNT @ 0 ?do
      i FILE-BODY @ FREE-SPAN
   loop
   FILE-PATH-AT-RELEASE
   FILE-PATH-N-RELEASE
   FILE-ETAG-AT-RELEASE
   FILE-ETAG-N-RELEASE
   FILE-BODY-RELEASE
   FILE-BODY-N-RELEASE
   DIR-AT-RELEASE
   DIR-N-RELEASE
   DIR-UP-RELEASE
   DIR-DEVICE-RELEASE
   DIR-INODE-RELEASE
   TEXT-ROW-RELEASE
   0 FILE-COUNT !
   0 TEXT-U !
   0 ROOT-U ! ;


\ Reads the whole tree under a directory into memory, before the server
\ starts. A failure while reading it gives back what was read and is rethrown.
\ STOP and failures opening worker resources or starting tasks empty the
\ table. Rejected arguments/context and listener-open failures retain it.
: STATIC-ROOT ( ptr u8 n -- ) {: root:ptr len:n :}
   len 0 <= if E-STATIC throw then
   len PATH-CAP >= if E-STATIC throw then
   FILE-COUNT @ 0 <> if E-STATE throw then
   root len FS:DIR? 0= if E-STATIC throw then
   root len ROOT-BUF SPAN:COPY
   len ROOT-U !
   0 TEXT-U !
   0 FILE-COUNT !
   0 DIR-HEAD !
   0 DIR-TAIL !
   [: s" " -1 DIR+ WALK ;] catch {: code:n :}
   code 0<> if STATIC-CLOSE code throw then ;


: STATIC-COUNT ( -- n )
   FILE-COUNT @ ;


;using
;package
