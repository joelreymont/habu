\ http-static.f - one directory tree served from memory. STATIC-ROOT reads the
\ whole tree before the server starts, so the tables are written while no task
\ is live and a request costs no file system call.
\
\ Each file carries the content type its extension names, an ETag over its
\ bytes, and the Cache-Control value the installed cache rule gives its path.
\ A request whose path names no file may fall back to one that does, as the
\ installed fallback rule decides. The rules are the caller's policy and are
\ installed before START (lib/net/http.f CACHE-RULE! and FALLBACK-RULE!);
\ until then every file is revalidated and nothing falls back.
require lib/net/http-router.f
require lib/fs.f
require lib/fs-list.f
require lib/memory.f
require lib/span.f
require lib/string.f

package HTTP
using MEM

private

$40 constant MAX-FILES
$20 constant MAX-DIRS
$4000 constant TEXT-CAP           \ the served paths and their ETags
$1000 constant NAMES-CAP
$40 constant DIGEST-HEX-BYTES
$20 constant DIGEST-BYTES
$400000 constant MAX-FILE-BYTES   \ one served file
$2E constant DOT-BYTE

MAX-FILES TYPED-BUFFER FILE-PATH-AT n
MAX-FILES TYPED-BUFFER FILE-PATH-N n
MAX-FILES TYPED-BUFFER FILE-ETAG-AT n
MAX-FILES TYPED-BUFFER FILE-ETAG-N n
MAX-FILES TYPED-BUFFER FILE-BODY SPAN:span<u8>   \ the bytes one file was read into
MAX-FILES TYPED-BUFFER FILE-BODY-N n
MAX-DIRS TYPED-BUFFER DIR-AT n
MAX-DIRS TYPED-BUFFER DIR-N n

TEXT-CAP SPAN-BUFFER: TEXT-BUF
NAMES-CAP SPAN-BUFFER: NAMES-BUF
FS-PATH-CAP SPAN-BUFFER: PATH-BUF
FS-PATH-CAP SPAN-BUFFER: URL-BUF
\ SHA256-IN and SHA256>HEX write a digest of their own fixed size and take no
\ capacity, so these two are the library's bound and not a span's.
create DIGEST-RAW DIGEST-BYTES allot
create DIGEST-HEX DIGEST-HEX-BYTES allot

variable TEXT-U
variable FILE-COUNT
variable DIR-HEAD
variable DIR-TAIL
variable ROOT-U
FS-PATH-CAP SPAN-BUFFER: ROOT-BUF


: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ SPAN:TAKE SPAN:$ ;


: TEXT+ ( ptr u8 n -- n ) {: bytes:ptr len:n :}
   TEXT-U @ len + TEXT-CAP > if E-STATIC throw then
   TEXT-U @ {: at:n :}
   bytes len TEXT-BUF at SPAN:SKIP SPAN:COPY
   TEXT-U @ len + TEXT-U !
   at ;


: TEXT$ ( n n -- ptr u8 n ) {: at:n len:n :}
   TEXT-BUF at len SPAN:SUB SPAN:$ ;


: FILE-PATH$ ( n -- ptr u8 n ) {: at:n :}
   at FILE-PATH-AT @ at FILE-PATH-N @ TEXT$ ;


: FILE-ETAG$ ( n -- ptr u8 n ) {: at:n :}
   at FILE-ETAG-AT @ at FILE-ETAG-N @ TEXT$ ;


: FILE-BODY$ ( n -- ptr u8 n ) {: at:n :}
   at FILE-BODY @ at FILE-BODY-N @ SPAN:TAKE SPAN:$ ;


\ ---- content types and cache policy -----------------------------------------

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
\ (docs/threads.md): an xt in a plain cell would lose the effect.
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


\ ---- loading the tree -------------------------------------------------------

: UNDER-ROOT ( ptr u8 n -- n ) {: rel:ptr len:n :}
   len 0= if ROOT$ {: base:ptr baselen:n :} base baselen PATH-BUF SPAN:COPY baselen exit then
   \ JOIN-PATH checks its own FS-PATH-CAP and takes no capacity: the bound is
   \ lib/fs.f's, and PATH-BUF is that wide.
   ROOT$ rel 1+ len 1- PATH-BUF SPAN:$ drop JOIN-PATH ;


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


: LOAD-FILE ( ptr u8 n -- ) {: url:ptr urllen:n :}
   FILE-COUNT @ MAX-FILES >= if E-STATIC throw then
   url urllen UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ FILE-SIZE {: size:n :}
   size MAX-FILE-BYTES > if E-STATIC throw then
   size 1 max {: capacity:n :}
   capacity BYTES-ALLOC-LEN ALLOC-SPAN {: body :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ body SPAN:$ READ-ALL {: got:n :}
   FILE-COUNT @ {: at:n :}
   url urllen TEXT+ at FILE-PATH-AT !
   urllen at FILE-PATH-N !
   body at FILE-BODY !
   got at FILE-BODY-N !
   body SPAN:$ drop got ETAG-FOR {: etag:n etaglen:n :}
   etag at FILE-ETAG-AT !
   etaglen at FILE-ETAG-N !
   at 1+ FILE-COUNT ! ;


: DIR+ ( ptr u8 n -- ) {: url:ptr urllen:n :}
   DIR-TAIL @ MAX-DIRS >= if E-STATIC throw then
   url urllen TEXT+ DIR-TAIL @ DIR-AT !
   urllen DIR-TAIL @ DIR-N !
   DIR-TAIL @ 1+ DIR-TAIL ! ;


: URL-JOIN ( ptr u8 n ptr u8 n -- n ) {: base:ptr baselen:n name:ptr namelen:n :}
   baselen namelen + 1+ FS-PATH-CAP > if E-STATIC throw then
   base baselen URL-BUF SPAN:COPY
   SLASH URL-BUF baselen SPAN:U8!
   name namelen URL-BUF baselen 1+ SPAN:SKIP SPAN:COPY
   baselen namelen + 1+ ;


: ENTRY ( ptr u8 n ptr u8 n -- ) {: base:ptr baselen:n name:ptr namelen:n :}
   base baselen name namelen URL-JOIN {: urllen:n :}
   URL-BUF urllen SPAN:TAKE SPAN:$ UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ DIR? if URL-BUF urllen SPAN:TAKE SPAN:$ DIR+ exit then
   PATH-BUF pathlen SPAN:TAKE SPAN:$ FILE? if URL-BUF urllen SPAN:TAKE SPAN:$ LOAD-FILE then ;


\ FS-LIST answers the whole listing into this buffer before anything opens the
\ next directory, so one buffer serves every level of the walk.
: LIST-NAMES ( ptr u8 n -- n ) {: url:ptr urllen:n :}
   url urllen UNDER-ROOT {: pathlen:n :}
   PATH-BUF pathlen SPAN:TAKE SPAN:$ NAMES-BUF SPAN:$ FS-LIST:NAMES ;


\ FS-LIST separates the names with one newline and no terminator.
: LINE-STOP ( n n -- n ) {: total:n at:n :}
   total at ?do
      NAMES-BUF i SPAN:U8@ LF = if i unloop exit then
   loop
   total ;


: WALK-NAMES ( ptr u8 n n -- ) {: url:ptr urllen:n total:n :}
   0
   begin
      {: at:n :}
      at total >= if exit then
      total at LINE-STOP {: stop:n :}
      stop at > if url urllen NAMES-BUF at stop at - SPAN:SUB SPAN:$ ENTRY then
      stop 1+
   again ;


: WALK-DIR ( n -- ) {: at:n :}
   at DIR-AT @ at DIR-N @ TEXT$ {: url:ptr urllen:n :}
   url urllen LIST-NAMES {: total:n :}
   url urllen total WALK-NAMES ;


: WALK ( -- )
   begin
      DIR-HEAD @ DIR-TAIL @ >= if exit then
      DIR-HEAD @ WALK-DIR
      DIR-HEAD @ 1+ DIR-HEAD !
   again ;


\ ---- serving ----------------------------------------------------------------

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


\ If-None-Match may list several tags, and * matches whatever we hold.
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

\ Reads the whole tree under a directory into memory, before the server
\ starts. The table is emptied again by STOP, so each start reads it afresh.
: STATIC-ROOT ( ptr u8 n -- ) {: root:ptr len:n :}
   len 0 <= if E-STATIC throw then
   len FS-PATH-CAP >= if E-STATIC throw then
   FILE-COUNT @ 0 <> if E-STATE throw then
   root len DIR? 0= if E-STATIC throw then
   root len ROOT-BUF SPAN:COPY
   len ROOT-U !
   0 TEXT-U !
   0 FILE-COUNT !
   0 DIR-HEAD !
   0 DIR-TAIL !
   s" " DIR+
   WALK ;


: STATIC-COUNT ( -- n )
   FILE-COUNT @ ;


\ Every span the tree took, given back. The table is emptied so a second start
\ reads the tree again.
: STATIC-CLOSE ( -- )
   FILE-COUNT @ 0 ?do
      i FILE-BODY @ FREE-SPAN
   loop
   0 FILE-COUNT !
   0 TEXT-U !
   0 ROOT-U ! ;


;using
;package
