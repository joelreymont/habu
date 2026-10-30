\ curl.f - HTTPS client over libcurl's easy interface: one request and one
\ response, with no retry policy, authentication, payload format or URL
\ construction; TLS, redirects, proxies and the CA bundle are libcurl's. Every
\ setter copies its span before it returns. PERFORM holds its task; START/AWAIT
\ and STREAM-START/NEXT run on one CURL loop task (the multi interface below).
\ Only HABU_NET_TESTS=1 runs the case that leaves the machine (TLS, CA bundle).
require src/lib/errors.f
require src/lib/string.f                      \ BUFFER:, the transfer table's byte rows
require src/lib/le.f                          \ the foreign cells are little-endian
require src/lib/ffi-abi.f
require src/lib/type/deftype.f
require src/lib/image-lifecycle.f
require src/lib/memory.f                      \ the grown record chunks, the text copies
require src/lib/span.f                        \ a text copy's reach
require src/lib/num-types.f                   \ the alloc-byte-len a chunk carries
require src/lib/task.f
require src/lib/aio.f                         \ the loop waits on AIO tickets
require src/lib/adt/result.f                  \ TASK:JOIN's answer, read by LOOP-STOP

package CURL
public

DEFTYPE HANDLE
DEFTYPE CODE
DEFTYPE HTTP-STATUS

SUMTYPE init-result
   VARIANT ready handle ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

SUMTYPE fetch-result
   VARIANT response http-status len ;VARIANT
   VARIANT truncated http-status len ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

SUMTYPE chunk-result
   VARIANT chunk http-status len ;VARIANT
   VARIANT response http-status ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

SUMTYPE header-result
   VARIANT complete len ;VARIANT
   VARIANT truncated len ;VARIANT
;SUMTYPE

E-CURL-OPERAND constant E-OPERAND
E-CURL-PLATFORM constant E-PLATFORM
E-CURL-STATE constant E-STATE
E-CURL-RESULT constant E-RESULT

private

\ curl.h writes every option as its type's base plus the option's own number, so
\ each line below is checkable against /usr/include/curl/curl.h on sight.
0 constant OPT-LONG                       \ CURLOPTTYPE_LONG
10000 constant OPT-OBJECT                 \ CURLOPTTYPE_OBJECTPOINT
$100000 constant INFO-STRING              \ CURLINFO_STRING
$200000 constant INFO-LONG                \ CURLINFO_LONG

OPT-OBJECT 1 + constant OPT-WRITE-DATA    \ CURLOPT_WRITEDATA
OPT-OBJECT 29 + constant OPT-HEADER-DATA  \ CURLOPT_HEADERDATA
OPT-OBJECT 2 + constant OPT-URL           \ CURLOPT_URL
OPT-OBJECT 4 + constant OPT-PROXY         \ CURLOPT_PROXY
OPT-OBJECT 23 + constant OPT-HTTP-HEADER  \ CURLOPT_HTTPHEADER
OPT-OBJECT 31 + constant OPT-COOKIE-FILE  \ CURLOPT_COOKIEFILE
OPT-OBJECT 36 + constant OPT-METHOD       \ CURLOPT_CUSTOMREQUEST
OPT-OBJECT 82 + constant OPT-COOKIE-JAR   \ CURLOPT_COOKIEJAR
OPT-OBJECT 103 + constant OPT-PRIVATE     \ CURLOPT_PRIVATE
OPT-OBJECT 165 + constant OPT-COPY-BODY   \ CURLOPT_COPYPOSTFIELDS
OPT-LONG 19 + constant OPT-LOW-SPEED-LIMIT \ CURLOPT_LOW_SPEED_LIMIT
OPT-LONG 20 + constant OPT-LOW-SPEED-TIME  \ CURLOPT_LOW_SPEED_TIME
OPT-LONG 52 + constant OPT-FOLLOW         \ CURLOPT_FOLLOWLOCATION
OPT-LONG 60 + constant OPT-BODY-SIZE      \ CURLOPT_POSTFIELDSIZE
OPT-LONG 74 + constant OPT-FRESH-CONNECT  \ CURLOPT_FRESH_CONNECT
OPT-LONG 80 + constant OPT-HTTPGET        \ CURLOPT_HTTPGET
OPT-LONG 99 + constant OPT-NO-SIGNAL      \ CURLOPT_NOSIGNAL
OPT-LONG 155 + constant OPT-TIMEOUT-MS    \ CURLOPT_TIMEOUT_MS
OPT-LONG 181 + constant OPT-PROTOCOLS     \ CURLOPT_PROTOCOLS
OPT-LONG 182 + constant OPT-REDIR-PROTO   \ CURLOPT_REDIR_PROTOCOLS
INFO-LONG 2 + constant INFO-STATUS        \ CURLINFO_RESPONSE_CODE
INFO-STRING 21 + constant INFO-PRIVATE    \ CURLINFO_PRIVATE

3 constant GLOBAL-DEFAULT                 \ CURL_GLOBAL_DEFAULT
0 constant CURLE-OK
2 constant CURLE-FAILED-INIT
27 constant CURLE-OUT-OF-MEMORY
23 constant CURLE-WRITE-ERROR
2 constant UNBUFFERED                     \ _IONBF, on both libcs

1 constant PROTO-HTTP                     \ CURLPROTO_HTTP
2 constant PROTO-HTTPS                    \ CURLPROTO_HTTPS
PROTO-HTTP PROTO-HTTPS or constant PROTO-WEB

$7FFFFFFF constant MAX-TIMEOUT            \ a C long option's own ceiling
999 constant MAX-STATUS
$08 constant CELL-BYTES

\ Foreign out-parameter cells, per task like FFI's own argument staging.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $28 TASK:+USER IO-STORAGE drop
: INFO-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: BUF-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $08 + ;
: LEN-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $10 + ;
: HBUF-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $18 + ;
: HLEN-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $20 + ;


\ libcurl's easy interface. CURL* and FILE* are OPAQUE here - nothing in this
\ package dereferences one - and so is curl_slist* outside SLIST-RELEASE, which
\ reads its documented layout to wipe it; all three are declared `n`: both host
\ ABIs pass a pointer and an integer in the same register, and a cell is what the
\ nominal handle types wrap. Only a span Habu or the callee really reads or
\ writes is declared `ptr u8`.
\
\ The two fixed arguments precede a variadic value. VARIADIC selects Apple's
\ stack convention or Linux's register convention and preserves pointer extents.
VERSIONED-LIBRARY curl 4

FUNCTION: GLOBAL-INIT curl_global_init ( n -- i32 ) ;FUNCTION
FUNCTION: EASY-INIT curl_easy_init ( -- n ) ;FUNCTION
FUNCTION: EASY-CLEANUP curl_easy_cleanup ( n -- ) ;FUNCTION
FUNCTION: EASY-PERFORM curl_easy_perform ( n -- i32 ) ;FUNCTION
FUNCTION: SETOPT-NUM curl_easy_setopt ( n n n -- i32 ) 2 VARIADIC ;FUNCTION
FUNCTION: SETOPT-SPAN curl_easy_setopt ( n n ptr u8 -- i32 ) 2 VARIADIC ;FUNCTION
FUNCTION: SLIST-APPEND curl_slist_append ( n n -- n ) ;FUNCTION
FUNCTION: SLIST-FREE curl_slist_free_all ( n -- ) ;FUNCTION

FUNCTION: GETINFO-CELL curl_easy_getinfo ( n n ptr u8 -- i32 )
   2 VARIADIC
   2 $08 WRITES-BYTES                     \ one C long, or one char*
;FUNCTION


\ libcurl's multi interface, the transfer driver the loop at the end of this
\ file runs on. CURLM* is opaque like CURL*, every answer is a CURLMcode, and
\ each out-parameter is a C int, so each line is checkable against
\ /usr/include/curl/multi.h on sight. curl_multi_info_read answers a CURLMsg*
\ or zero, which is foreign memory this package copies before it reads, and
\ curl_multi_fdset fills three fd_sets the CALLER cleared, which is why each of
\ them is declared as the whole FD_SETSIZE bitmap it writes into.
FUNCTION: MULTI-INIT curl_multi_init ( -- n ) ;FUNCTION
FUNCTION: MULTI-CLEANUP curl_multi_cleanup ( n -- i32 ) ;FUNCTION
FUNCTION: MULTI-ADD curl_multi_add_handle ( n n -- i32 ) ;FUNCTION
FUNCTION: MULTI-REMOVE curl_multi_remove_handle ( n n -- i32 ) ;FUNCTION

FUNCTION: MULTI-PERFORM curl_multi_perform ( n ptr u8 -- i32 )
   1 $04 WRITES-BYTES                     \ int *running_handles
;FUNCTION

FUNCTION: MULTI-INFO-READ curl_multi_info_read ( n ptr u8 -- n )
   1 $04 WRITES-BYTES                     \ int *msgs_in_queue
;FUNCTION

FUNCTION: MULTI-FDSET curl_multi_fdset ( n ptr u8 ptr u8 ptr u8 ptr u8 -- i32 )
   1 $80 WRITES-BYTES                     \ fd_set *read_fd_set
   2 $80 WRITES-BYTES                     \ fd_set *write_fd_set
   3 $80 WRITES-BYTES                     \ fd_set *exc_fd_set
   4 $04 WRITES-BYTES                     \ int *max_fd
;FUNCTION

FUNCTION: MULTI-TIMEOUT curl_multi_timeout ( n ptr u8 -- i32 )
   1 $08 WRITES-BYTES                     \ long *milliseconds
;FUNCTION


\ The process's own libc. Habu cannot hand libcurl a callback into checked code,
\ and none is needed: libcurl's default write callback is fwrite. A whole
\ body's CURLOPT_WRITEDATA is an fmemopen stream over the caller's span; the
\ header capture and a streamed body are open_memstream streams, and memcpy
\ moves their bytes into caller spans. strlen measures the request list's
\ lines for their wipe.
PROCESS-SYMBOLS

FUNCTION: STREAM-CLOSE fclose ( n -- i32 ) ;FUNCTION
FUNCTION: STREAM-FLUSH fflush ( n -- i32 ) ;FUNCTION
FUNCTION: STREAM-POSITION ftell ( n -- n ) ;FUNCTION
FUNCTION: STREAM-ERROR ferror ( n -- i32 ) ;FUNCTION
FUNCTION: STREAM-BUFFERING setvbuf ( n n n n -- i32 ) ;FUNCTION
FUNCTION: FILE-OPEN fopen ( ptr u8 ptr u8 -- n ) ;FUNCTION
FUNCTION: TEXT-LENGTH strlen ( n -- n ) ;FUNCTION
FUNCTION: RELEASE free ( n -- ) ;FUNCTION
FUNCTION: NEW-STATE calloc ( n n -- n ) ;FUNCTION

FUNCTION: MEMSTREAM open_memstream ( ptr u8 ptr u8 -- n )
   0 $08 WRITES-BYTES                     \ char **bufp
   1 $08 WRITES-BYTES                     \ size_t *sizep
;FUNCTION

FUNCTION: SPAN-STREAM fmemopen ( ptr u8 n ptr u8 -- n )
   0 1 WRITES-ARG                         \ the caller's span, length from arg 1
;FUNCTION

FUNCTION: COPY-OUT memcpy ( ptr u8 n n -- n )
   0 2 WRITES-ARG                         \ the caller's span, length from arg 2
;FUNCTION


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


\ A request body, a destination span, a string option and a header line are the
\ caller's own storage, bounded only by what the caller allocated: a whole body
\ past its span is `truncated`, the header capture grows in a memstream, and
\ libcurl refuses a string option past its own input limit with a code, which
\ answers `failed`.
: AT-LEAST ( n n -- ) {: value:n minimum:n :}
   value minimum < if E-OPERAND throw then ;


\ Every foreign cell here is little-endian and LE: reads it: LE:U64@ for a
\ pointer or a C long, LE:U32@ for the C int every multi out-parameter and both
\ CURLMsg enums are.
: CELL-CLEAR ( ptr u8 -- ) {: target :}
   CELL-BYTES 0 do 0 target i + c! loop ;


: HANDLE-CELL ( handle -- n ) {: subject:handle :}
   subject HANDLE>N dup 0= if E-STATE throw then ;


: CODE>STATUS ( n -- status ) {: rc:n :}
   rc CURLE-OK = if CURL-STATUS:ok exit then
   rc >CODE CURL-STATUS:failed ;


: OUT-OF-MEMORY ( -- status )
   CURLE-OUT-OF-MEMORY >CODE CURL-STATUS:failed ;


CAST: >BYTES ( n -- ptr u8 )


\ A URL, a header line or a response can carry a credential, so every text
\ copy, request list and response buffer this file frees is zeroed first.
\ libcurl's own receive and TLS buffers, and the copies open_memstream leaves
\ behind as it grows a capture, are outside that guarantee.
: BYTES-WIPE ( n n -- )
   {: source:n u:n :}
   source >BYTES {: bytes :}
   u 0 ?do 0 bytes i + c! loop ;


: WIPE ( n -- )
   dup TEXT-LENGTH BYTES-WIPE ;


\ A capture whose stream never published a buffer is zero, which owns nothing.
: BYTES-RELEASE ( n n -- )
   {: source:n u:n :}
   source 0= if exit then
   source u BYTES-WIPE
   source RELEASE ;


\ libcurl reads a NUL-terminated copy and duplicates it for itself, so the copy
\ is a MEM:WITH-BYTES mapping that lasts only for the call. FFI:CSTR refuses an
\ embedded NUL (E-FFI-NUL), which would silently shorten a URL or header line,
\ before it writes a byte, so a refused mapping is unmapped still zero.
: TEXT-ROOM ( n -- NUM:alloc-byte-len )
   {: u:n :}
   u 0 AT-LEAST
   u FFI:CSTR-ROOM MEM:BYTES-ALLOC-LEN ;


: TEXT-WIPE ( SPAN:span<u8> -- ) 0 swap SPAN:FILL ;


\ A request list is curl_slist { char *data; struct curl_slist *next; }, and
\ its header lines can carry a credential, so they are wiped before it is freed.
: SLIST-RELEASE ( n -- )
   {: list:n :}
   list begin dup 0 <> while
      dup >BYTES LE:U64@ WIPE
      >BYTES CELL-BYTES + LE:U64@
   repeat drop
   list SLIST-FREE ;


: SET-NUM-RAW ( handle n n -- n ) {: subject:handle option:n value:n :}
   subject HANDLE-CELL option value SETOPT-NUM ;


: TEXT-SETOPT ( n n ptr u8 n SPAN:span<u8> -- n )
   {: easy:n option:n text u:n copy :}
   easy option text u copy FFI:CSTR FFI:>CELL SETOPT-NUM
   copy TEXT-WIPE ;


: SET-TEXT-RAW ( handle n ptr u8 n -- n )
   {: subject:handle option:n text u:n :}
   subject HANDLE-CELL option text u u TEXT-ROOM
   [: MEM:ALLOCATION>SPAN TEXT-SETOPT ;] MEM:WITH-BYTES ;


: SET-TEXT ( handle n ptr u8 n -- status )
   SET-TEXT-RAW CODE>STATUS ;


\ The private slot owns one record: request list, captured response pointer,
\ response length and success flag. The record lasts exactly as long as CURL*.
0 constant S.LIST
8 constant S.RESPONSE
16 constant S.LENGTH
24 constant S.VALID
4 constant STATE-CELLS

: STATE-GET ( n -- n ) {: easy:n :}
   INFO-CELL CELL-CLEAR
   easy INFO-PRIVATE INFO-CELL GETINFO-CELL CURLE-OK <> if E-RESULT throw then
   INFO-CELL LE:U64@ dup 0= if E-STATE throw then ;

: STATE-FIELD ( n n -- ptr u8 )
   {: state:n offset:n :}
   state >BYTES offset + ;

: STATE-LIST@ ( n -- n ) S.LIST STATE-FIELD LE:U64@ ;
: STATE-RESP@ ( n -- n ) S.RESPONSE STATE-FIELD LE:U64@ ;
: STATE-LEN@ ( n -- n ) S.LENGTH STATE-FIELD LE:U64@ ;
: STATE-VALID@ ( n -- n ) S.VALID STATE-FIELD LE:U64@ ;

: STATE-CLEAR-RESPONSE ( n -- ) {: state:n :}
   state STATE-RESP@ state STATE-LEN@ BYTES-RELEASE
   0 state S.RESPONSE STATE-FIELD LE:U64!
   0 state S.LENGTH STATE-FIELD LE:U64!
   0 state S.VALID STATE-FIELD LE:U64! ;

: STATE-PUBLISH ( n n n -- ) {: easy:n source:n u:n :}
   easy STATE-GET {: state:n :}
   state STATE-CLEAR-RESPONSE
   source state S.RESPONSE STATE-FIELD LE:U64!
   u state S.LENGTH STATE-FIELD LE:U64!
   1 state S.VALID STATE-FIELD LE:U64! ;

: STATE-DESTROY ( n -- )
   {: state:n :}
   state STATE-CLEAR-RESPONSE
   state STATE-LIST@ dup 0 <> if SLIST-RELEASE else drop then
   state RELEASE ;


\ A list this call created and could not publish is freed here; an appended-to
\ list is already the head the private slot holds, so it stays owned either way.
: HEADER-STORE ( n n n n -- status )
   {: easy:n state:n old:n list:n :}
   easy OPT-HTTP-HEADER list SETOPT-NUM dup CURLE-OK <> if
      old 0= if list SLIST-RELEASE then
      CODE>STATUS exit
   then drop
   list state S.LIST STATE-FIELD LE:U64!
   CURL-STATUS:ok ;


\ curl_slist_append duplicates the line into the node it adds.
: TEXT-APPEND ( n ptr u8 n SPAN:span<u8> -- n )
   {: old:n text u:n copy :}
   old text u copy FFI:CSTR FFI:>CELL SLIST-APPEND
   copy TEXT-WIPE ;


: HEADER-APPEND ( n ptr u8 n -- status )
   {: easy:n text u:n :}
   easy STATE-GET {: state:n :}
   state STATE-LIST@ {: old:n :}
   old text u u TEXT-ROOM
   [: MEM:ALLOCATION>SPAN TEXT-APPEND ;] MEM:WITH-BYTES {: list:n :}
   list 0= if OUT-OF-MEMORY exit then
   easy state old list HEADER-STORE ;


\ libcurl sends a header line as it is: a CR or LF inside it would end the line
\ early and send the rest as a line of its own.
: ONE-LINE ( ptr u8 n -- )
   {: text u:n :}
   u 0 ?do
      text i + c@ dup $0D = swap $0A = or if E-OPERAND throw then
   loop ;


\ The two cells open_memstream publishes the finished buffer and its length in
\ are the CALLER'S, not this word's: PERFORM passes its own task's pair, while a
\ multiplexed transfer passes its record's, because the task that closes that
\ stream is the loop and not the task that opened it.
: STREAM-OPEN ( ptr u8 ptr u8 -- n ) {: buf len :}
   buf CELL-CLEAR
   len CELL-CLEAR
   buf len MEMSTREAM ;


\ open_memstream publishes its buffer and length when the stream closes; the
\ buffer is the caller's to free from then on.
: STREAM-TAKE ( n ptr u8 ptr u8 -- n n n ) {: stream:n buf len :}
   stream STREAM-CLOSE 0= if CURLE-OK else CURLE-WRITE-ERROR then
   buf LE:U64@ len LE:U64@ rot ;


\ The caller's span as the stream libcurl writes a whole body into. fmemopen
\ "r+b" writes no NUL and nothing past the span on either libc, where the "w"
\ modes put a NUL in the span's last byte on glibc. macOS refuses a zero-byte
\ fmemopen, so an empty span is /dev/null opened read-only, which refuses every
\ write on both. Unbuffered, a byte libcurl hands fwrite is in the span or
\ refused at that call. Zero answers a stream that could not be opened.
: SPAN-OPEN ( ptr u8 n -- n ) {: target capacity:n :}
   capacity 0= if
      S\" /dev/null\z" drop S\" rb\z" drop FILE-OPEN
   else
      target capacity S\" r+b\z" drop SPAN-STREAM
   then
   dup 0= if exit then
   dup 0 UNBUFFERED 0 STREAM-BUFFERING 0 <> if STREAM-CLOSE drop 0 then ;


\ The body bytes the span holds, whether the span refused a write for want of
\ room, and the close's code. A full span sets the stream's error flag on both
\ libcs while fclose still answers 0.
: SPAN-TAKE ( n -- n bool n ) {: stream:n :}
   stream STREAM-POSITION {: u:n :}
   stream STREAM-ERROR 0 <> {: refused:bool :}
   stream STREAM-CLOSE 0= if CURLE-OK else CURLE-WRITE-ERROR then {: rc:n :}
   u 0 < if E-RESULT throw then
   u refused rc ;


\ The body outran the span: the span stream refused a write, which is
\ `truncated`, not a failure.
: OVERFLOW? ( n bool -- bool ) {: rc:n refused:bool :}
   rc CURLE-WRITE-ERROR = refused and ;


\ A transfer's code once an overflow has been answered as `truncated`.
: BODY-RC ( n bool -- n ) {: rc:n full:bool :}
   full if CURLE-OK else rc then ;


: FIRST-ERROR ( n n -- n ) {: prior:n next:n :}
   prior CURLE-OK <> if prior else next then ;


: DATA-CLEAR ( n -- n ) {: easy:n :}
   easy OPT-WRITE-DATA 0 SETOPT-NUM
   easy OPT-HEADER-DATA 0 SETOPT-NUM FIRST-ERROR ;


: TRANSFER ( n n n -- n ) {: easy:n body:n headers:n :}
   easy OPT-WRITE-DATA body SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy OPT-HEADER-DATA headers SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy EASY-PERFORM ;


: STATUS-TRY ( n -- n bool ) {: easy:n :}
   INFO-CELL CELL-CLEAR
   easy INFO-STATUS INFO-CELL GETINFO-CELL CURLE-OK <> if 0 false exit then
   INFO-CELL LE:U64@ dup 0 < over MAX-STATUS > or if drop 0 false exit then
   true ;


: BODY-COPY ( ptr u8 n n -- ) {: target source:n u:n :}
   u 0= if exit then
   source 0= if E-RESULT throw then
   target source u COPY-OUT drop ;


: FETCH-RESULT ( n n bool -- fetch-result ) {: status:n u:n full:bool :}
   full if
      status >HTTP-STATUS u >LEN CURL-FETCH--RESULT:truncated exit
   then
   status >HTTP-STATUS u >LEN CURL-FETCH--RESULT:response ;


: FETCH-FAILED ( n -- fetch-result ) {: rc:n :}
   rc >CODE CURL-FETCH--RESULT:failed ;


: FLAG>N ( bool -- n )
   if 1 else 0 then ;


\ Process-wide, as curl_global_init requires.
variable GLOBAL-DONE
variable GLOBAL-REGISTERED


\ A captured image is restored into another process, where libcurl is not
\ initialised; the flag is cleared then, exactly as FFI clears its symbol cache.
: FORGET-GLOBAL ( -- )
   0 GLOBAL-DONE !
   0 GLOBAL-REGISTERED ! ;


\ The registered flag is set only after REGISTER completes, so a throwing
\ registration leaves flag and hook consistent for a retry.
: REGISTER-CLEANUP ( -- )
   GLOBAL-REGISTERED @ 0= if
      [: FORGET-GLOBAL ;] IMAGE-LIFECYCLE:REGISTER
      1 GLOBAL-REGISTERED !
   then ;


: GLOBAL-READY ( -- n )
   GLOBAL-DONE @ 0 <> if CURLE-OK exit then
   REGISTER-CLEANUP
   GLOBAL-DEFAULT GLOBAL-INIT dup CURLE-OK <> if exit then
   1 GLOBAL-DONE ! ;


\ Only HTTP and HTTPS, for the first URL and for whatever a redirect names, so a
\ file:// or ftp:// URL out of a scraped page fails with CURLE_UNSUPPORTED_PROTOCOL
\ before anything opens. The bitmask options are deprecated in favour of
\ CURLOPT_PROTOCOLS_STR (318) and CURLOPT_REDIR_PROTOCOLS_STR (319), which need
\ libcurl 7.85; the bitmasks are still honoured and need no version floor.
: SCHEMES ( n -- n ) {: easy:n :}
   easy OPT-PROTOCOLS PROTO-WEB SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy OPT-REDIR-PROTO PROTO-WEB SETOPT-NUM ;


\ libcurl must not reach for signals inside a tasked engine.
: DEFAULTS ( n -- n ) {: easy:n :}
   easy OPT-NO-SIGNAL 1 SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy SCHEMES ;

public

\ A handle that cannot be initialised or configured is cleaned up before INIT
\ answers, so a failure never leaves one allocated.
: INIT ( -- init-result )
   GLOBAL-READY dup CURLE-OK <> if >CODE CURL-INIT--RESULT:failed exit then drop
   EASY-INIT dup 0= if
      drop CURLE-FAILED-INIT >CODE CURL-INIT--RESULT:failed exit
   then
   {: easy:n :}
   easy DEFAULTS dup CURLE-OK <> if
      easy EASY-CLEANUP >CODE CURL-INIT--RESULT:failed exit
   then drop
   STATE-CELLS CELL-BYTES NEW-STATE dup 0= if
      drop easy EASY-CLEANUP CURLE-OUT-OF-MEMORY >CODE CURL-INIT--RESULT:failed exit
   then
   {: state:n :}
   easy OPT-PRIVATE state SETOPT-NUM dup CURLE-OK <> if
      state RELEASE easy EASY-CLEANUP >CODE CURL-INIT--RESULT:failed exit
   then drop
   easy >HANDLE CURL-INIT--RESULT:ready ;


: URL! ( handle ptr u8 n -- status ) {: subject:handle text u:n :}
   u 1 AT-LEAST
   subject OPT-URL text u SET-TEXT ;


\ An empty proxy disables proxies, including those named by the environment.
: PROXY! ( handle ptr u8 n -- status )
   {: subject:handle text u:n :}
   subject OPT-PROXY text u SET-TEXT ;


\ libcurl picks GET by default and POST once BODY! has been used. METHOD!
\ overrides the verb without changing anything else about the request, and the
\ override is sent on every request, redirected ones included: METHOD! "POST"
\ stays POST across a 302 or 303. A POST that should follow 302 or 303 as GET
\ uses BODY! alone. GET! returns a reused handle to plain GET.
: METHOD! ( handle ptr u8 n -- status ) {: subject:handle text u:n :}
   u 1 AT-LEAST
   subject OPT-METHOD text u SET-TEXT ;


\ Plain GET again: no body, no Content-Length or form Content-Type, and no
\ METHOD! override; a null CURLOPT_CUSTOMREQUEST is libcurl's way to drop one.
\ The URL, headers, cookies and every other option stay.
: GET! ( handle -- status ) {: subject:handle :}
   subject OPT-HTTPGET 1 SET-NUM-RAW dup CURLE-OK <> if CODE>STATUS exit then drop
   subject OPT-METHOD 0 SET-NUM-RAW CODE>STATUS ;


: HEADER+ ( handle ptr u8 n -- status ) {: subject:handle text u:n :}
   u 1 AT-LEAST
   text u ONE-LINE
   subject HANDLE-CELL text u HEADER-APPEND ;


\ The span is borrowed for this call: libcurl copies the body out of it, and its
\ declared length is set first because the copy reads that length. Any length
\ libcurl can hold is taken; a copy it cannot allocate is its out-of-memory status.
: BODY! ( handle ptr u8 n -- status ) {: subject:handle bytes u:n :}
   u 0 AT-LEAST
   subject OPT-BODY-SIZE u SET-NUM-RAW dup CURLE-OK <> if CODE>STATUS exit then drop
   subject HANDLE-CELL OPT-COPY-BODY bytes SETOPT-SPAN CODE>STATUS ;


\ Cookies read at request time. An empty path starts the cookie engine with no
\ stored cookies, which is libcurl's own spelling for "keep session cookies".
: COOKIE-FILE! ( handle ptr u8 n -- status ) {: subject:handle path u:n :}
   subject OPT-COOKIE-FILE path u SET-TEXT ;


\ Cookies written back when the handle is cleaned up.
: COOKIE-JAR! ( handle ptr u8 n -- status ) {: subject:handle path u:n :}
   u 1 AT-LEAST
   subject OPT-COOKIE-JAR path u SET-TEXT ;


\ The whole transfer's limit, not one read's. Zero removes it.
: TIMEOUT! ( handle ms -- status ) {: subject:handle limit:ms :}
   limit MS>N 0 MAX-TIMEOUT WITHIN-RANGE
   subject OPT-TIMEOUT-MS limit MS>N SET-NUM-RAW CODE>STATUS ;


\ A stall test, not a ceiling: the transfer is ended only while it stays below
\ `rate` bytes per second for `seconds` seconds, so a document that arrives
\ slowly but never stops still finishes. TIMEOUT!'s limit on the whole transfer
\ is separate and unchanged. Zero in either value removes the test. The rate is
\ set first, and a failure there is answered before the window is touched.
: LOW-SPEED! ( handle n n -- status ) {: subject:handle rate:n seconds:n :}
   rate 0 MAX-TIMEOUT WITHIN-RANGE
   seconds 0 MAX-TIMEOUT WITHIN-RANGE
   subject OPT-LOW-SPEED-LIMIT rate SET-NUM-RAW dup CURLE-OK <> if
      CODE>STATUS exit
   then drop
   subject OPT-LOW-SPEED-TIME seconds SET-NUM-RAW CODE>STATUS ;


: FOLLOW! ( handle bool -- status ) {: subject:handle on:bool :}
   subject OPT-FOLLOW on FLAG>N SET-NUM-RAW CODE>STATUS ;


\ Force a new connection for the next transfer when enabled. Zero restores
\ libcurl's default attempt to reuse a connection from its pool.
: FRESH-CONNECT! ( handle bool -- status ) {: subject:handle on:bool :}
   subject OPT-FRESH-CONNECT on FLAG>N SET-NUM-RAW CODE>STATUS ;


private

\ PERFORM holds its task for a whole transfer, so ten transfers that way cost
\ ten threads parked in libcurl. The words below are the other shape: ONE
\ package-owned task drives libcurl's multi interface and a task that starts a
\ transfer is free until it asks for the answer. That loop task parks in package
\ AIO and not in libcurl: after every curl_multi_perform it asks
\ curl_multi_fdset which descriptors libcurl is waiting on and curl_multi_timeout
\ how long it may wait, turns those into AIO tickets, and takes the first one
\ that ends. No CURLMOPT_SOCKETFUNCTION and no CURLMOPT_TIMERFUNCTION: those are
\ callbacks, and Habu hands libcurl no callback - the same rule PERFORM's
\ memstream answers.

$400 constant FD-SETSIZE                  \ an fd_set holds bits 0..1023
$80 constant FD-SET-BYTES                 \ those bits, eight to the byte
$40 constant WAKE-CHUNK                   \ bytes taken off the wake pipe at once
100 constant IDLE-MS                      \ the turn to take when libcurl names none
1000 constant LONGEST-MS                  \ the longest turn to take at all
1 constant SHORTEST-MS                    \ and the shortest, so zero is not a spin

0 constant CURLM-OK                       \ CURLM_OK
1 constant MSG-DONE                       \ CURLMSG_DONE
42 constant CURLE-ABORTED                 \ CURLE_ABORTED_BY_CALLBACK

\ struct CURLMsg {int msg; CURL *easy_handle; union {void *whatever; CURLcode
\ result;} data;}, /usr/include/curl/multi.h, on AArch64 with its int padded.
$00 constant MSG.KIND
$08 constant MSG.EASY
$10 constant MSG.RESULT
$18 constant MSG-BYTES

\ A record's life. Only the submitter moves FREE -> CLAIMING, and only with one
\ atomic-cas; every other move is made under the facility below.
0 constant STATE-FREE                     \ nobody owns this record
1 constant STATE-CLAIMING                 \ a submitter owns it and is filling it
2 constant STATE-CLAIMED                  \ filled; the loop has not added it yet
3 constant STATE-RUNNING                  \ the loop added its handle to the multi handle
4 constant STATE-CANCEL                   \ the owner asked for it to end early
5 constant STATE-ABANDONED                \ the owner ended; the loop frees it
6 constant STATE-DONE                     \ the result is stored and the owner woken
7 constant STATE-LEAVING                  \ the halted owner waits for the loop to let go
8 constant STATE-LEFT                     \ the loop let go; the woken owner frees it

0 constant KIND-RESPONSE                  \ which fetch-result AWAIT builds
1 constant KIND-TRUNCATED
2 constant KIND-FAILED

0 constant MODE-WHOLE
1 constant MODE-STREAM
0 constant REQUEST-NONE
1 constant REQUEST-WAITING
2 constant REQUEST-CHUNK
3 constant REQUEST-RESPONSE
4 constant REQUEST-FAILED

BEGIN-STRUCTURE REC-BYTES
   CELL +FIELD R.STATE
   CELL +FIELD R.EASY
   CELL +FIELD R.CAP
   CELL +FIELD R.STREAM
   CELL +FIELD R.HSTREAM
   CELL +FIELD R.ADDED                    \ the handle is in the multi handle
   CELL +FIELD R.KIND
   CELL +FIELD R.STATUS
   CELL +FIELD R.LEN
   CELL +FIELD R.CODE
   CELL +FIELD R.MODE
   CELL +FIELD R.REQUEST
   CELL +FIELD R.OFFSET
   CELL +FIELD R.BODY
   CELL +FIELD R.DELIVERED
   CELL +FIELD R.OWNER                    \ the TCB pointer TASK:WAKE takes
   CELL +FIELD R.TARGET                   \ the span's address
   \ open_memstream's buffer and length, for the body and for the header
   \ capture, because the loop is what closes the streams and the opening
   \ task's row is not where it can read. The FFI writes them as bytes.
   CELL +FIELD R.BUF
   CELL +FIELD R.BUF-LEN
   CELL +FIELD R.HBUF
   CELL +FIELD R.HLEN
END-STRUCTURE

\ One record per transfer, in cells the loop reads with atomic@ and atomic!.
\ The records live in a chain of chunks, as AIO's do: a link cell, then the
\ records. The first chunk is static and each grown one holds as many records
\ as all before it plus RECS-FIRST, so REC passes at most log2 links. A chunk
\ never moves and goes back only at a capture, so a record's index is stable
\ for the loop and for the owners that read a record without the facility.
$20 constant RECS-FIRST
CELL constant CHUNK-HEAD

\ BUFFER: allots zeroed storage on a cell-rounded address and a grown chunk is
\ a mapping, page-aligned, which is the alignment every row below wants - the
\ atomic record fields, and the fd_sets, the CURLMsg copy and the
\ out-parameter cells libcurl reads and writes as aligned C objects.
CHUNK-HEAD RECS-FIRST REC-BYTES * + BUFFER: CURL-RECS

\ The records the published chunks hold. Grown under the facility, after the
\ chunk's link; read with atomic@ by an owner checking an index.
variable REC-COUNT
RECS-FIRST REC-COUNT !
variable MAPS-ARMED                       \ the capture sweep below is registered

FD-SET-BYTES BUFFER: SET-READ
FD-SET-BYTES BUFFER: SET-WRITE
FD-SET-BYTES BUFFER: SET-EXC
FD-SETSIZE BUFFER: SET-KEPT
$20 BUFFER: MULTI-STAGE
MSG-BYTES BUFFER: MSG-BUF
WAKE-CHUNK BUFFER: WAKE-BUF
create WAKE-BYTE $01 c,

\ One ticket per descriptor libcurl wants, in slots a zero mask says are free.
\ A slot holds the descriptor itself, or -1 for none, so the row is plain cells.
\ Every descriptor comes from the fd_sets, so FD-SETSIZE slots hold them all.
FD-SETSIZE TYPED-BUFFER FD-FDS n
FD-SETSIZE TYPED-BUFFER FD-MASKS n
FD-SETSIZE TYPED-BUFFER FD-TICKETS AIO:ticket

variable MULTI-CELL                       \ the CURLM* the loop drives
variable LOOP-LIVE                        \ atomic: a loop is running
variable STOP-FLAG                        \ atomic: the loop takes no new record and ends
variable LOCK-READY                       \ the facility has been initialized once
variable LOOP-THROW                       \ what ended the loop, reported by LOOP-STOP
variable LOOP-MRC                         \ the CURLMcode the loop's own body ended on
variable WAKE-R
variable WAKE-W
variable TIMER-MS                         \ the ms the pending timer holds, -1 for none

TASK:FACILITY CURL-LOCK
TASK:MIN-STACK TASK:TASK LOOP-TASK
AIO:GROUP LOOP-GROUP
TYPED-VARIABLE WAKE-TICKET AIO:ticket
TYPED-VARIABLE TIMER-TICKET AIO:ticket

\ A ticket is matched against the ones this loop holds, never minted here: the
\ projection out of AIO's nominal is what a comparison needs and all it needs.
CAST: TICKET>N ( AIO:ticket -- n )

: SAME-TICKET? ( AIO:ticket AIO:ticket -- bool ) {: a:AIO:ticket b:AIO:ticket :}
   a TICKET>N b TICKET>N = ;

: COUNT-CELL ( -- ptr u8 )    MULTI-STAGE ;
: MAXFD-CELL ( -- ptr u8 )    MULTI-STAGE $08 + ;
: TIMEOUT-CELL ( -- ptr u8 )  MULTI-STAGE $10 + ;


\ A record chunk, which this module names by address as n: 0 ends the chain.
CAST: CURL-N>PTR ( n -- ptr n )

\ A record's two pointer cells, as the slots they are.
CAST: OWNER-SLOT ( ptr n -- ptr ptr n )
CAST: TARGET-SLOT ( ptr n -- ptr ptr u8 )

: CHUNK-FIRST ( -- n )
   CURL-RECS CELL-VIEW FFI:>CELL ;

: CHUNK-NEXT ( n -- n )
   CURL-N>PTR atomic@ ;

: CHUNK-REC ( n n -- ptr n ) {: c:n k:n :}
   c CHUNK-HEAD + k REC-BYTES * + CURL-N>PTR ;

: CHUNK-LEN ( n -- NUM:alloc-byte-len )
   REC-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

: CHUNK-LAST ( -- n )
   CHUNK-FIRST begin dup CHUNK-NEXT dup 0<> while nip repeat drop ;

\ REC-CHECK bounds every index by the published chunks, so an index past them
\ here is this module's own defect.
: CHUNK-AFTER ( n -- n )
   CHUNK-NEXT dup 0= if s" curl: record index" E-STATE die then ;


: REC-CHECK ( n -- n ) {: idx:n :}
   idx 0 < idx REC-COUNT atomic@ >= or if E-OPERAND throw then
   idx ;


\ The walk carries the chunk, the index left and the chunk's record count.
: REC ( n -- ptr n ) REC-CHECK
   CHUNK-FIRST swap RECS-FIRST
   begin 2dup >= while
      swap over - swap 2 *
      rot CHUNK-AFTER -rot
   repeat
   drop CHUNK-REC ;


: REC-BUF-CELL ( n -- ptr u8 )  REC R.BUF BYTE-VIEW ;
: REC-LEN-CELL ( n -- ptr u8 )  REC R.BUF-LEN BYTE-VIEW ;
: REC-HBUF-CELL ( n -- ptr u8 ) REC R.HBUF BYTE-VIEW ;
: REC-HLEN-CELL ( n -- ptr u8 ) REC R.HLEN BYTE-VIEW ;


: REC-STATE@ ( n -- n )     REC R.STATE atomic@ ;
: REC-EASY@ ( n -- n )      REC R.EASY @ ;
: REC-CAP@ ( n -- n )       REC R.CAP @ ;
: REC-STREAM@ ( n -- n )    REC R.STREAM @ ;
: REC-HSTREAM@ ( n -- n )   REC R.HSTREAM @ ;
: REC-MODE@ ( n -- n )      REC R.MODE @ ;
: REC-TARGET@ ( n -- ptr u8 ) REC R.TARGET TARGET-SLOT @ ;
: REC-OWNER@ ( n -- ptr n ) REC R.OWNER OWNER-SLOT @ ;


: REC-TARGET! ( ptr u8 n -- ) {: target idx:n :}
   target idx REC R.TARGET TARGET-SLOT ! ;


: REC-STATE! ( n n -- ) {: state:n idx:n :}
   state idx REC R.STATE atomic! ;


\ A grown chunk is this process's: a capture gives it back so the image carries
\ only the static first chunk. A record in flight in one is the program's
\ defect, named here: await every transfer before a capture.
: CHUNK-RETIRE ( n n -- ) {: c:n len:n :}
   len 0 ?do
      c i CHUNK-REC R.STATE atomic@ STATE-FREE <> if
         s" curl: transfer in flight at capture - await it before the build or snapshot captures"
         E-STATE die
      then
   loop
   c CURL-N>PTR BYTE-VIEW len CHUNK-LEN MEM:RELEASE-BYTES ;


\ The walk carries the next chunk and the record count of the one after. The
\ facility keeps a loop turn from walking the chain while it is cut.
: RECS-SWEEP ( -- )
   CURL-LOCK TASK:GET
   CHUNK-FIRST CHUNK-NEXT RECS-FIRST 2 *
   0 CHUNK-FIRST CURL-N>PTR atomic!
   begin over 0<> while
      over CHUNK-NEXT -rot
      swap over CHUNK-RETIRE 2 *
   repeat
   2drop
   RECS-FIRST REC-COUNT atomic!
   0 MAPS-ARMED !
   CURL-LOCK TASK:RELEASE ;


\ Armed before a chunk is mapped, so no mapping exists without its cleanup;
\ the flag is set only after REGISTER completes, as AIO's MAPS-ARM.
: MAPS-ARM ( -- )
   MAPS-ARMED @ 0<> if exit then
   [: RECS-SWEEP ;] IMAGE-LIFECYCLE:REGISTER
   1 MAPS-ARMED ! ;


\ A fresh mapping is zero: every record free. The chunk is linked once it is
\ whole, and REC-COUNT grows after the link, so a reader that sees the count
\ finds the chunk.
: REC-GROW ( -- )
   MAPS-ARM
   REC-COUNT @ RECS-FIRST + {: len:n :}
   len CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   c CHUNK-LAST CURL-N>PTR atomic!
   REC-COUNT @ len + REC-COUNT atomic! ;


\ The first free record of the chunk at c, whose records start at index base,
\ claimed, or -1. One claim wins: the state cell moves FREE -> CLAIMING in one
\ step, so two tasks starting a transfer at the same moment are handed
\ different records. CLAIMING is a state the loop passes over, so a record is
\ never added to the multi handle before the submitter has finished filling it.
: CHUNK-CLAIM ( n n n -- n ) {: c:n base:n len:n :}
   len 0 ?do
      STATE-FREE STATE-CLAIMING c i CHUNK-REC R.STATE atomic-cas STATE-FREE = if
         base i + unloop exit
      then
   loop
   -1 ;


\ The walk carries the chunk, its first index and its record count.
: FREE-CLAIM ( -- n )
   CHUNK-FIRST 0 RECS-FIRST
   begin
      >r 2dup r@ CHUNK-CLAIM r> swap
      dup 0 >= if nip nip nip exit then drop
      >r r@ + r> 2 *
      rot CHUNK-NEXT dup 0= if drop 2drop -1 exit then -rot
   again ;


\ A free record, grown into when every one is taken. Every claim runs under
\ CURL-LOCK, which is what lets REC-GROW append without a race. Answers a code
\ and the index: 0 and the record, or the code the growth threw and -1, which
\ the claim answers because it holds the facility.
: REC-CLAIM ( -- n n )
   FREE-CLAIM dup 0 < if
      drop [: REC-GROW ;] catch dup 0<> if -1 exit then drop
      FREE-CLAIM
   then
   0 swap ;


\ A record is free again only after its handle is cleared, so a scan can never
\ match a record whose transfer is over.
: REC-RELEASE ( n -- ) {: idx:n :}
   0 idx REC R.EASY !
   0 idx REC R.STREAM !
   0 idx REC R.HSTREAM !
   0 idx REC R.BODY !
   MODE-WHOLE idx REC R.MODE !
   REQUEST-NONE idx REC R.REQUEST atomic!
   0 idx REC R.ADDED !
   STATE-FREE idx REC-STATE! ;


\ An owner that left never collects the response, even if FINISH already
\ closed both streams and published it before the halt reached AWAIT. Runs
\ under CURL-LOCK once the handle is out of the multi handle. A halted owner
\ waiting in ABANDON is woken to free the record itself; the lock it takes to
\ do so keeps it alive until this wake has landed. Any other record is free.
: LEAVE-DONE ( n -- ) {: idx:n :}
   idx REC-EASY@ STATE-GET STATE-CLEAR-RESPONSE
   idx REC R.BODY @ idx REC R.LEN @ BYTES-RELEASE
   0 idx REC R.BODY !
   idx REC-STATE@ STATE-LEAVING = if
      STATE-LEFT idx REC-STATE!
      idx REC-OWNER@ TASK:WAKE
      exit
   then
   idx REC-RELEASE ;


: LEAVING? ( n -- bool ) {: idx:n :}
   idx REC-STATE@ dup STATE-ABANDONED = swap STATE-LEAVING = or ;


: TABLE-CLEAR ( -- )
   REC-COUNT @ 0 ?do i REC-RELEASE loop ;


\ The record a handle is in, or -1. A handle is in at most one record at a time
\ - START refuses a second one - so the first match is the only match. This is
\ how a task finds its own transfer: asking libcurl about a handle the loop may
\ be driving is exactly what it must not do.
: REC-FIND ( n -- n ) {: easy:n :}
   REC-COUNT @ 0 ?do
      i REC-STATE@ STATE-FREE <> if
         i REC-EASY@ easy = if i unloop exit then
      then
   loop
   -1 ;


: ANY-BUSY? ( -- bool )
   REC-COUNT @ 0 ?do
      i REC-STATE@ STATE-FREE <> if true unloop exit then
   loop
   false ;


: REC-FILL ( n n ptr u8 n n n n -- ) {: idx:n easy:n target capacity:n stream:n headers:n mode:n :}
   easy idx REC R.EASY !
   capacity idx REC R.CAP !
   stream idx REC R.STREAM !
   headers idx REC R.HSTREAM !
   0 idx REC R.ADDED !
   target idx REC-TARGET!
   TASK:SELF idx REC R.OWNER OWNER-SLOT !
   KIND-FAILED idx REC R.KIND !
   0 idx REC R.STATUS !
   0 idx REC R.LEN !
   CURLE-ABORTED idx REC R.CODE !
   mode idx REC R.MODE !
   REQUEST-NONE idx REC R.REQUEST atomic!
   0 idx REC R.OFFSET !
   0 idx REC R.BODY !
   0 idx REC R.DELIVERED ! ;


\ One byte says "a record changed": a START, a CANCEL, an abandoned record or
\ the stop. The loop holds one AIO POLL ticket on the read end, and after it
\ reads whatever is there it scans the table, so a byte that arrives between the
\ read and the scan costs one extra turn and never a missed record.

: WAKE-POKE ( -- )
   WAKE-W @ WAKE-BYTE 1 write drop ;


: WAKE-DRAIN ( -- )
   WAKE-R @ WAKE-BUF WAKE-CHUNK read drop ;


: PIPE-OPEN ( -- n )
   pipe {: r:n w:n rc:n :}
   rc 0 <> if rc exit then
   r WAKE-R !
   w WAKE-W !
   0 ;


: PIPE-CLOSE ( -- )
   WAKE-R @ close-rc drop
   WAKE-W @ close-rc drop
   0 WAKE-R !
   0 WAKE-W ! ;


: REC-STREAM-TAKE ( n -- n n n ) {: idx:n :}
   idx REC-STREAM@ idx REC-BUF-CELL idx REC-LEN-CELL STREAM-TAKE ;

: REC-HEADER-TAKE ( n -- n n n ) {: idx:n :}
   idx REC-HSTREAM@ idx REC-HBUF-CELL idx REC-HLEN-CELL STREAM-TAKE ;


\ The header capture is closed and the handle forgets both streams, so it keeps
\ no pointer to a closed one. The capture is the caller's to free.
: CAPTURE-CLOSE ( n -- n n n ) {: idx:n :}  \ ( idx -- headers headeru rc )
   idx REC-HSTREAM@ 0= if
      0 0 CURLE-OK
   else idx REC-HEADER-TAKE then
   {: headers:n headeru:n headrc:n :}
   idx REC-EASY@ DATA-CLEAR {: clear-rc:n :}
   0 idx REC R.STREAM !
   0 idx REC R.HSTREAM !
   headers headeru headrc clear-rc FIRST-ERROR ;


\ A whole body's span keeps what it was given; a streamed body is the loop's
\ own buffer and is freed.
: BODY-DROP ( n -- n ) {: idx:n :}
   idx REC-MODE@ MODE-WHOLE = if
      idx REC-STREAM@ SPAN-TAKE nip nip exit
   then
   idx REC-STREAM-TAKE {: body:n bodyu:n bodyrc:n :}
   body bodyu BYTES-RELEASE
   bodyrc ;


: STREAM-DROP ( n -- n ) {: idx:n :}
   idx REC-STREAM@ 0= if CURLE-OK exit then
   idx BODY-DROP {: bodyrc:n :}
   idx CAPTURE-CLOSE {: headers:n headeru:n headrc:n :}
   headers headeru BYTES-RELEASE
   bodyrc headrc FIRST-ERROR ;


: FAILED-STORE ( n n -- ) {: idx:n rc:n :}
   KIND-FAILED idx REC R.KIND !
   rc idx REC R.CODE ! ;


\ FETCH-RESULT's decision, stored as fields because the value belongs to the
\ task that waits.
: RESULT-STORE ( n n n bool -- ) {: idx:n status:n u:n full:bool :}
   status idx REC R.STATUS !
   u idx REC R.LEN !
   full if KIND-TRUNCATED else KIND-RESPONSE then idx REC R.KIND ! ;


\ The state store is the barrier: every field above is written before it, and
\ AWAIT reads them after its wait.
: SETTLE ( n -- ) {: idx:n :}
   STATE-DONE idx REC-STATE!
   idx REC-MODE@ MODE-WHOLE = if idx REC-OWNER@ TASK:WAKE then ;


\ Failure paths that drop an active stream have no later loop turn to deliver
\ buffered bytes. Answer a pending NEXT before waking its owner.
: SETTLE-FAILED ( n -- ) {: idx:n :}
   idx REC-MODE@ MODE-STREAM = if
      idx REC R.REQUEST atomic@ REQUEST-WAITING = if
         REQUEST-FAILED idx REC R.REQUEST atomic!
         idx SETTLE
         idx REC-OWNER@ TASK:WAKE
         exit
      then
   then
   idx SETTLE ;


: ADD-ONE ( n -- n ) {: idx:n :}
   MULTI-CELL atomic@ idx REC-EASY@ MULTI-ADD dup CURLM-OK <> if exit then drop
   1 idx REC R.ADDED !
   STATE-RUNNING idx REC-STATE!
   CURLM-OK ;


: REMOVE-ONE ( n -- n ) {: idx:n :}
   idx REC R.ADDED @ 0= if CURLM-OK exit then
   MULTI-CELL atomic@ idx REC-EASY@ MULTI-REMOVE dup CURLM-OK <> if exit then drop
   0 idx REC R.ADDED !
   CURLM-OK ;


\ A finished streamed transfer keeps its body for NEXT to deliver. An owner
\ that left gets no result: LEAVE-DONE wakes only a halted owner still waiting
\ in ABANDON, which keeps this loop from waking a TCB the join has released.
: STREAM-FINISH ( n n -- n ) {: idx:n rc:n :}
   idx REC-STREAM-TAKE {: source:n u:n bodyrc:n :}
   idx CAPTURE-CLOSE {: headers:n headeru:n headrc:n :}
   idx LEAVING? if
      source u BYTES-RELEASE headers headeru BYTES-RELEASE
      idx LEAVE-DONE CURLM-OK exit
   then
   rc bodyrc FIRST-ERROR headrc FIRST-ERROR {: outcome:n :}
   source idx REC R.BODY !
   u idx REC R.LEN !
   idx REC-EASY@ STATUS-TRY {: status:n valid:bool :}
   status idx REC R.STATUS !
   outcome CURLE-OK <> if
      headers headeru BYTES-RELEASE
      idx outcome FAILED-STORE
   else
      valid 0= if headers headeru BYTES-RELEASE E-RESULT throw then
      KIND-RESPONSE idx REC R.KIND !
      idx REC-EASY@ headers headeru STATE-PUBLISH
   then
   idx SETTLE
   CURLM-OK ;


\ A finished whole transfer: libcurl already wrote the body into the owner's
\ span, exactly as PERFORM's. An owner that left gets no result.
: WHOLE-FINISH ( n n -- n ) {: idx:n rc:n :}
   idx REC-STREAM@ SPAN-TAKE {: u:n refused:bool bodyrc:n :}
   idx CAPTURE-CLOSE {: headers:n headeru:n headrc:n :}
   idx LEAVING? if
      headers headeru BYTES-RELEASE idx LEAVE-DONE CURLM-OK exit
   then
   rc refused OVERFLOW? {: full:bool :}
   rc full BODY-RC bodyrc FIRST-ERROR headrc FIRST-ERROR {: outcome:n :}
   outcome CURLE-OK <> if
      headers headeru BYTES-RELEASE
      idx outcome FAILED-STORE
      idx SETTLE
      CURLM-OK exit
   then
   idx REC-EASY@ STATUS-TRY {: status:n valid:bool :}
   valid 0= if headers headeru BYTES-RELEASE E-RESULT throw then
   idx status u full RESULT-STORE
   idx REC-EASY@ headers headeru STATE-PUBLISH
   idx SETTLE
   CURLM-OK ;


: FINISH ( n n -- n ) {: idx:n rc:n :}
   idx REMOVE-ONE dup CURLM-OK <> if exit then drop
   idx REC-MODE@ MODE-STREAM = if idx rc STREAM-FINISH exit then
   idx rc WHOLE-FINISH ;


\ Only the loop calls fflush or reads the foreign memstream cells. Its copy
\ completes under CURL-LOCK before it publishes REQUEST-CHUNK to the owner.
\ The owner's destination is untouched again until that owner calls NEXT.
: STREAM-DELIVER ( n -- n ) {: idx:n :}
   idx REC-MODE@ MODE-STREAM <> if CURLM-OK exit then
   idx REC R.REQUEST atomic@ REQUEST-WAITING <> if CURLM-OK exit then
   idx REC-STATE@ STATE-DONE = if
      idx REC R.BODY @ idx REC R.LEN @ CURLM-OK
   else
      idx REC-STREAM@ STREAM-FLUSH 0= if
         idx REC-BUF-CELL LE:U64@ idx REC-LEN-CELL LE:U64@ CURLM-OK
      else 0 0 CURLE-WRITE-ERROR then
   then
   {: source:n u:n frc:n :}
   frc CURLE-OK <> if
      idx REMOVE-ONE dup CURLM-OK <> if exit then drop
      idx STREAM-DROP drop
      idx frc FAILED-STORE
      idx SETTLE-FAILED
      CURLM-OK exit
   then
   u idx REC R.OFFSET @ > if
      idx REC-CAP@ u idx REC R.OFFSET @ - min {: count:n :}
      idx REC-STATE@ STATE-DONE = if
         idx REC R.STATUS @
      else
         idx REC-EASY@ STATUS-TRY 0= if drop E-RESULT throw then
      then
      idx REC R.STATUS !
      idx REC-TARGET@ source idx REC R.OFFSET @ + count BODY-COPY
      idx REC R.OFFSET @ count + idx REC R.OFFSET !
      count idx REC R.DELIVERED !
      REQUEST-CHUNK idx REC R.REQUEST atomic!
      idx REC-OWNER@ TASK:WAKE
      CURLM-OK exit
   then
   idx REC-STATE@ STATE-DONE = if
      idx REC R.KIND @ KIND-FAILED = if REQUEST-FAILED
      else REQUEST-RESPONSE then
      idx REC R.REQUEST atomic!
      idx REC-OWNER@ TASK:WAKE
   then
   CURLM-OK ;


\ The owner asked for this transfer to end: libcurl's own code for a transfer an
\ application ended is the answer it gets.
: CANCEL-RUN ( n -- n ) {: idx:n :}
   idx REMOVE-ONE dup CURLM-OK <> if exit then drop
   idx STREAM-DROP drop
   idx CURLE-ABORTED FAILED-STORE
   idx SETTLE-FAILED
   CURLM-OK ;


: LEAVE-RUN ( n -- n ) {: idx:n :}
   idx REMOVE-ONE dup CURLM-OK <> if exit then drop
   idx STREAM-DROP drop
   idx LEAVE-DONE
   CURLM-OK ;


: SERVICE-ONE ( n -- n ) {: idx:n :}
   idx REC-STATE@ {: st:n :}
   st STATE-FREE = st STATE-CLAIMING = or st STATE-LEFT = or if CURLM-OK exit then
   st STATE-CLAIMED = if idx ADD-ONE exit then
   st STATE-CANCEL = if idx CANCEL-RUN exit then
   idx LEAVING? if idx LEAVE-RUN exit then
   idx STREAM-DELIVER ;


: SERVICE-RECORDS ( -- n )
   REC-COUNT @ 0 ?do
      i SERVICE-ONE dup CURLM-OK <> if unloop exit then drop
   loop
   CURLM-OK ;


\ CURLMsg is libcurl's memory, so the struct is copied into this loop's own
\ bytes with the same bounded memcpy the body copy uses and read from the copy.
: MSG-TAKE ( n -- n ) {: msg:n :}
   MSG-BUF msg MSG-BYTES COPY-OUT drop
   MSG-BUF MSG.KIND + LE:U32@ MSG-DONE <> if CURLM-OK exit then
   MSG-BUF MSG.EASY + LE:U64@ {: easy:n :}
   MSG-BUF MSG.RESULT + LE:U32@ {: rc:n :}
   easy REC-FIND dup 0 < if drop CURLM-OK exit then
   rc FINISH ;


: REAP ( -- n )
   begin
      MULTI-CELL atomic@ COUNT-CELL MULTI-INFO-READ dup 0= if drop CURLM-OK exit then
      MSG-TAKE dup CURLM-OK <> if exit then drop
   again ;


: TRANSFERS-RUN ( -- n )
   MULTI-CELL atomic@ COUNT-CELL MULTI-PERFORM dup CURLM-OK <> if exit then drop
   REAP dup CURLM-OK <> if exit then drop
   SERVICE-RECORDS ;


\ Every record access but the claim runs under this one facility, and the two
\ locked bodies answer a CURLMcode rather than throwing, so a refusal never
\ leaves it held.
: LOCKED-SERVICE ( -- n )
   CURL-LOCK TASK:GET
   SERVICE-RECORDS
   CURL-LOCK TASK:RELEASE ;


: LOCKED-TRANSFERS ( -- n )
   CURL-LOCK TASK:GET
   TRANSFERS-RUN
   CURL-LOCK TASK:RELEASE ;


\ An fd_set is a bitmap of FD_SETSIZE bits, bit i in byte i/8, which is the same
\ byte whatever the word size the C library reads it in. An fd at or above
\ FD_SETSIZE cannot appear in one at all, so the sets bound this loop at 1024
\ descriptors and that ceiling is libcurl's own.

: SET-CLEAR ( ptr u8 -- ) {: target :}
   FD-SET-BYTES 0 do 0 target i + c! loop ;


: SET-BIT? ( ptr u8 n -- bool ) {: target fd:n :}
   fd 0 < fd FD-SETSIZE >= or if false exit then
   target fd 8 / + c@ 1 fd 7 and lshift and 0 <> ;


: KEPT-CLEAR ( n -- ) {: maxfd:n :}
   maxfd 1 + 0 ?do 0 SET-KEPT i + c! loop ;


: KEPT! ( n -- ) {: fd:n :}
   1 SET-KEPT fd + c! ;


: KEPT? ( n -- bool ) {: fd:n :}
   SET-KEPT fd + c@ 0 <> ;


\ The mask an fd is wanted with. An exception is neither readable nor writable
\ to the kernel this loop polls with, so it is asked for as both.
: WANT-MASK ( n -- n ) {: fd:n :}
   SET-EXC fd SET-BIT? if AIO:READABLE AIO:WRITABLE or exit then
   0
   SET-READ fd SET-BIT? if AIO:READABLE or then
   SET-WRITE fd SET-BIT? if AIO:WRITABLE or then ;


: FD-FD@ ( n -- n ) FD-FDS @ ;


: FD-FD! ( n n -- ) {: fd:n slot:n :}
   fd slot FD-FDS ! ;


: FD-MASK@ ( n -- n ) FD-MASKS @ ;


: FD-MASK! ( n n -- ) {: mask:n slot:n :}
   mask slot FD-MASKS ! ;


: FD-TABLE-CLEAR ( -- )
   FD-SETSIZE 0 ?do 0 i FD-MASK! -1 i FD-FD! loop ;


\ An fd FD-REVIEW arms holds no slot, and every fd that does is another one
\ below FD-SETSIZE, so a slot is always free: none is this loop's defect.
: FD-FREE-SLOT ( -- n )
   FD-SETSIZE 0 ?do i FD-MASK@ 0= if i unloop exit then loop
   s" curl: no free descriptor slot" E-STATE die ;


: FD-SLOT-OF ( AIO:ticket -- n ) {: t:AIO:ticket :}
   FD-SETSIZE 0 ?do
      i FD-MASK@ 0 <> if
         i FD-TICKETS @ t SAME-TICKET? if i unloop exit then
      then
   loop
   -1 ;


: FD-RELEASE ( n -- ) {: slot:n :}
   0 slot FD-MASK!
   -1 slot FD-FD! ;


\ A ticket this loop no longer wants. The cancel leaves it in the group until
\ AWAIT-ANY hands it back, where it matches no slot and the turn ignores it.
: FD-DROP ( n -- ) {: slot:n :}
   slot FD-TICKETS @ AIO:CANCEL
   slot FD-RELEASE ;


: FD-ARM ( n n -- ) {: fd:n mask:n :}
   FD-FREE-SLOT {: slot:n :}
   fd >FD mask -1 >MS AIO:POLL {: t:AIO:ticket :}
   t LOOP-GROUP AIO:GROUP+
   t slot FD-TICKETS !
   fd slot FD-FD!
   mask slot FD-MASK! ;


\ max_fd is the one foreign int read signed: curl_multi_fdset reports an empty
\ set as -1.
: SETS-READ ( -- n n )                    \ ( -- CURLMcode max-fd )
   SET-READ SET-CLEAR
   SET-WRITE SET-CLEAR
   SET-EXC SET-CLEAR
   MAXFD-CELL CELL-CLEAR
   MULTI-CELL atomic@ SET-READ SET-WRITE SET-EXC MAXFD-CELL MULTI-FDSET
   dup CURLM-OK <> if -1 exit then drop
   CURLM-OK MAXFD-CELL LE:S32@ ;


\ An fd still wanted with the mask its ticket holds keeps that ticket; one whose
\ mask changed, or that left the sets, gives it up.
: SLOT-REVIEW ( n -- ) {: slot:n :}
   slot FD-MASK@ dup 0= if drop exit then {: mask:n :}
   slot FD-FD@ {: fd:n :}
   fd WANT-MASK mask <> if slot FD-DROP exit then
   fd KEPT! ;


: FD-REVIEW ( n -- ) {: fd:n :}
   fd KEPT? if exit then
   fd WANT-MASK dup 0= if drop exit then
   fd swap FD-ARM ;


: SETS-REBUILD ( -- n )
   SETS-READ {: mrc:n maxfd:n :}
   mrc CURLM-OK <> if mrc exit then
   maxfd KEPT-CLEAR
   FD-SETSIZE 0 ?do i SLOT-REVIEW loop
   maxfd 1 + 0 ?do i FD-REVIEW loop
   CURLM-OK ;


: WANT-MS ( -- n n )                      \ ( -- CURLMcode ms )
   TIMEOUT-CELL CELL-CLEAR
   MULTI-CELL atomic@ TIMEOUT-CELL MULTI-TIMEOUT dup CURLM-OK <> if IDLE-MS exit then
   drop
   TIMEOUT-CELL LE:U64@ {: ms:n :}
   ms 0 < if CURLM-OK IDLE-MS exit then
   CURLM-OK ms SHORTEST-MS max LONGEST-MS min ;


: TIMER-ARM ( n -- ) {: ms:n :}
   ms >MS AIO:TIMEOUT {: t:AIO:ticket :}
   t LOOP-GROUP AIO:GROUP+
   t TIMER-TICKET !
   ms TIMER-MS ! ;


\ One timer ticket at a time. A shorter answer than the pending one replaces it;
\ a longer one waits, because the pending timer fires first and asks again.
: TIMER-REVIEW ( -- n )
   WANT-MS {: mrc:n ms:n :}
   mrc CURLM-OK <> if mrc exit then
   TIMER-MS @ 0 < if ms TIMER-ARM CURLM-OK exit then
   ms TIMER-MS @ >= if CURLM-OK exit then
   TIMER-TICKET @ AIO:CANCEL
   -1 TIMER-MS !
   ms TIMER-ARM
   CURLM-OK ;


: WAKE-ARM ( -- )
   WAKE-R @ >FD AIO:READABLE -1 >MS AIO:POLL {: t:AIO:ticket :}
   t LOOP-GROUP AIO:GROUP+
   t WAKE-TICKET ! ;


: OUTCOME-DROP ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;


\ A ticket no slot owns is one this loop cancelled and is done with.
: TURN-ANSWER ( AIO:ticket -- n ) {: t:AIO:ticket :}
   t WAKE-TICKET @ SAME-TICKET? if WAKE-DRAIN WAKE-ARM LOCKED-SERVICE exit then
   t TIMER-TICKET @ SAME-TICKET? if -1 TIMER-MS ! CURLM-OK exit then
   t FD-SLOT-OF dup 0 >= if FD-RELEASE CURLM-OK exit then
   drop CURLM-OK ;


: TURN ( -- n )
   LOOP-GROUP AIO:AWAIT-ANY OUTCOME-DROP
   TURN-ANSWER dup CURLM-OK <> if exit then drop
   LOCKED-TRANSFERS dup CURLM-OK <> if exit then drop
   SETS-REBUILD dup CURLM-OK <> if exit then drop
   TIMER-REVIEW ;


: LOOP-RUN ( -- n )
   WAKE-ARM
   IDLE-MS TIMER-ARM
   LOCKED-TRANSFERS dup CURLM-OK <> if exit then drop
   SETS-REBUILD dup CURLM-OK <> if exit then drop
   begin
      STOP-FLAG atomic@ 0 <> if CURLM-OK exit then
      TURN dup CURLM-OK <> if exit then drop
   again ;


\ A multi call that refuses the handle it was given is a contract violation, and
\ the transfers in flight are ended here rather than left with an owner parked
\ on a loop that is gone.
: FAIL-ONE ( n -- ) {: idx:n :}
   idx REC-STATE@ {: st:n :}
   st STATE-FREE = st STATE-CLAIMING = or st STATE-LEFT = or if exit then
   st STATE-DONE = if
      idx REC-MODE@ MODE-WHOLE = if exit then
      idx REC R.BODY @ idx REC R.LEN @ BYTES-RELEASE
      0 idx REC R.BODY !
      idx REC R.OFFSET @ idx REC R.LEN !
      idx CURLE-ABORTED FAILED-STORE
      idx REC R.REQUEST atomic@ REQUEST-WAITING = if
         REQUEST-FAILED idx REC R.REQUEST atomic!
         idx REC-OWNER@ TASK:WAKE
      then
      exit
   then
   idx REMOVE-ONE drop
   idx STREAM-DROP drop
   idx LEAVING? if idx LEAVE-DONE exit then
   idx CURLE-ABORTED FAILED-STORE
   idx SETTLE-FAILED ;


: FAIL-ALL ( -- )
   CURL-LOCK TASK:GET
   1 STOP-FLAG atomic!
   REC-COUNT @ 0 ?do i FAIL-ONE loop
   CURL-LOCK TASK:RELEASE ;


\ Every ticket this loop submitted is ended and handed back before the task
\ returns: a record still in flight is what AIO:STOP refuses, and a ticket
\ nobody awaits is a record nobody frees.
: GROUP-DRAIN ( -- )
   FD-SETSIZE 0 ?do i FD-MASK@ 0 <> if i FD-DROP then loop
   WAKE-TICKET @ AIO:CANCEL
   TIMER-MS @ 0 >= if TIMER-TICKET @ AIO:CANCEL -1 TIMER-MS ! then
   begin
      LOOP-GROUP AIO:GROUP-COUNT 0= if exit then
      LOOP-GROUP AIO:AWAIT-ANY OUTCOME-DROP drop
   again ;


: LOOP-RUN-STORE ( -- )
   LOOP-RUN LOOP-MRC ! ;


\ However the loop ends - a multi call that refused, or a throw out of AIO - no
\ owner is left parked on a loop that is gone: every transfer in flight is ended
\ first and what ended the loop is the task's answer, which LOOP-STOP rethrows.
\ A loop that ended by a throw leaves its tickets to AIO's own per-task cleanup,
\ because the ring is what just refused.
: LOOP-BODY ( -- )
   CURLM-OK LOOP-MRC !
   [: LOOP-RUN-STORE ;] catch {: rc:n :}
   LOOP-MRC @ {: mrc:n :}
   rc 0 <> mrc CURLM-OK <> or if FAIL-ALL then
   rc 0 <> if rc throw then
   GROUP-DRAIN
   mrc CURLM-OK <> if E-RESULT throw then
   0 TASK:RETURN ;


\ The handle is written into the record before the facility is given back, so a
\ second START on it - from this task or another - already finds it in flight.
: CLAIM-BODY ( n -- n n ) {: easy:n :}    \ ( easy -- rc idx )
   easy REC-FIND 0 >= if E-STATE -1 exit then
   REC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   easy idx REC R.EASY !
   0 idx ;


: START-CLAIM ( n -- n )
   CURL-LOCK TASK:GET
   CLAIM-BODY
   CURL-LOCK TASK:RELEASE
   {: rc:n idx:n :}
   rc 0 <> if rc throw then
   idx ;


: OWNED-INDEX ( n -- n ) {: easy:n :}
   CURL-LOCK TASK:GET
   easy REC-FIND
   CURL-LOCK TASK:RELEASE
   dup 0 < if drop E-STATE throw then
   {: idx:n :}
   idx REC-OWNER@ FFI:>CELL TASK:SELF-N <> if E-STATE throw then
   idx ;


: RESULT-BUILD ( n n n n -- fetch-result ) {: kind:n status:n u:n rc:n :}
   kind KIND-FAILED = if rc >CODE CURL-FETCH--RESULT:failed exit then
   kind KIND-TRUNCATED = if
      status >HTTP-STATUS u >LEN CURL-FETCH--RESULT:truncated exit
   then
   status >HTTP-STATUS u >LEN CURL-FETCH--RESULT:response ;


: RESULT-TAKE ( n -- fetch-result ) {: idx:n :}
   CURL-LOCK TASK:GET
   idx REC R.KIND @ idx REC R.STATUS @ idx REC R.LEN @ idx REC R.CODE @
   idx REC-RELEASE
   CURL-LOCK TASK:RELEASE
   RESULT-BUILD ;


\ A halted owner gives its record up and returns only once the loop holds no
\ part of it: the loop takes the handle out of the multi handle, cleans it up
\ and marks it LEFT before the span is the owner's to free or reuse. The wait
\ is a TASK:STOP loop, which parks normally once the halt's hint is taken, and
\ the owner frees the record itself. A DONE record is already out of the multi
\ handle with nothing left to write, and is freed here without the loop, which
\ after a FAIL-ALL is no longer there to answer.
: ABANDON ( n -- ) {: idx:n :}
   CURL-LOCK TASK:GET
   idx REC-STATE@ STATE-DONE = if
      idx LEAVE-DONE
      CURL-LOCK TASK:RELEASE
      exit
   then
   STATE-LEAVING idx REC-STATE!
   CURL-LOCK TASK:RELEASE
   WAKE-POKE
   begin idx REC-STATE@ STATE-LEFT <> while TASK:STOP repeat
   CURL-LOCK TASK:GET
   idx REC-RELEASE
   CURL-LOCK TASK:RELEASE ;


: OWNER-MARK ( ptr n n -- bool ) {: owner:ptr idx:n :}
   idx REC-STATE@ dup STATE-FREE = swap STATE-CLAIMING = or if false exit then
   idx REC-OWNER@ FFI:>CELL owner FFI:>CELL <> if false exit then
   STATE-ABANDONED idx REC-STATE!
   true ;

\ Every task exit precedes release of its TCB. Marking its records under the
\ loop's lock stops a later copy or wake, but this runs after the task's
\ cleanups, so it gives no span back: a span must outlive its record.
: OWNER-END ( -- )
   TASK:SELF {: owner:ptr :}
   CURL-LOCK TASK:GET
   false REC-COUNT @ 0 ?do owner i OWNER-MARK or loop
   CURL-LOCK TASK:RELEASE
   if WAKE-POKE then ;


: CANCEL-MARK ( n -- ) {: idx:n :}
   idx REC-STATE@ {: st:n :}
   st STATE-CLAIMED = st STATE-RUNNING = or 0= if exit then
   STATE-CANCEL idx REC-STATE! ;

public

\ Creates the multi handle, the wake pipe and the one task that drives every
\ multiplexed transfer. AIO's own loop must already be running - starting it is
\ the program's job, and a LOOP-START without it answers AIO's refusal by name -
\ and a second start is E-STATE. A host that refuses the wake pipe is answered
\ the way libcurl answers exhaustion, with CURLE_OUT_OF_MEMORY.
: LOOP-START ( -- status )
   LOOP-LIVE atomic@ 0 <> if E-STATE throw then
   0 >MS AIO:TIMEOUT AIO:AWAIT OUTCOME-DROP
   GLOBAL-READY dup CURLE-OK <> if CODE>STATUS exit then drop
   LOCK-READY @ 0= if CURL-LOCK TASK:FACILITY-INIT 1 LOCK-READY ! then
   PIPE-OPEN 0 <> if OUT-OF-MEMORY exit then
   MULTI-INIT dup 0= if drop PIPE-CLOSE OUT-OF-MEMORY exit then
   MULTI-CELL atomic!
   TABLE-CLEAR
   FD-TABLE-CLEAR
   -1 TIMER-MS !
   0 LOOP-THROW !
   0 STOP-FLAG atomic!
   ['] LOOP-BODY LOOP-TASK TASK:ACTIVATE
   1 LOOP-LIVE atomic!
   CURL-STATUS:ok ;


\ Ends the loop and gives the multi handle and the pipe back. Every transfer
\ must have been awaited first: a record that is not free is E-STATE, exactly as
\ AIO:STOP refuses a ring the kernel still owns. What ended the loop is
\ rethrown here. After a stop the loop can be started again.
: LOOP-STOP ( -- )
   LOOP-LIVE atomic@ 0= if E-STATE throw then
   CURL-LOCK TASK:GET
   ANY-BUSY? dup 0= if 1 STOP-FLAG atomic! then
   CURL-LOCK TASK:RELEASE
   if E-STATE throw then
   0 LOOP-LIVE atomic!
   WAKE-POKE
   LOOP-TASK TASK:JOIN MATCH result
      ok OF drop ENDOF
      err OF LOOP-THROW ! ENDOF
   ;MATCH
   MULTI-CELL atomic@ MULTI-CLEANUP {: mrc:n :}
   0 MULTI-CELL atomic!
   PIPE-CLOSE
   LOOP-THROW @ {: rc:n :}
   rc 0 <> if rc throw then
   mrc CURLM-OK <> if E-RESULT throw then ;


\ Hands one prepared handle and one writable span to the loop and answers as
\ soon as the record is queued. Every per-handle option - URL!, METHOD!, GET!,
\ PROXY!, HEADER+, BODY!, COOKIE-FILE!, COOKIE-JAR!, TIMEOUT!, LOW-SPEED!, FOLLOW!,
\ FRESH-CONNECT! and the scheme restriction - is set before this and holds for
\ that transfer alone.
\ From here until AWAIT answers, the handle and the span belong to the loop: no
\ other task may touch either. A START with no loop running, and a second START
\ on a handle already in flight, are E-STATE. The records grow with the
\ transfers in flight, so no count of them is refused.
private

\ A whole body goes straight into the owner's span; a streamed one grows in the
\ record's memstream until NEXT copies it out.
: BODY-OPEN ( n ptr u8 n n -- n ) {: idx:n target capacity:n mode:n :}
   mode MODE-WHOLE = if target capacity SPAN-OPEN exit then
   idx REC-BUF-CELL idx REC-LEN-CELL STREAM-OPEN ;


: START-WITH-BODY ( handle ptr u8 len n -- status ) {: subject:handle target capacity:len mode:n :}
   subject HANDLE-CELL {: easy:n :}
   LOOP-LIVE atomic@ 0= if E-STATE throw then
   capacity LEN>N 0 AT-LEAST
   TASK:SELF dup FFI:>CELL 0<> if ['] OWNER-END swap TASK:AT-EXIT else drop then
   easy START-CLAIM {: idx:n :}
   easy STATE-GET STATE-CLEAR-RESPONSE
   idx target capacity LEN>N mode BODY-OPEN dup 0= if
      drop idx REC-RELEASE OUT-OF-MEMORY exit
   then
   {: stream:n :}
   idx easy target capacity LEN>N stream 0 mode REC-FILL
   idx REC-HBUF-CELL idx REC-HLEN-CELL STREAM-OPEN dup 0= if
      drop
      idx STREAM-DROP drop
      idx REC-RELEASE OUT-OF-MEMORY exit
   then
   {: headers:n :}
   headers idx REC R.HSTREAM !
   easy OPT-WRITE-DATA stream SETOPT-NUM dup CURLE-OK <> if
      idx STREAM-DROP drop
      idx REC-RELEASE
      CODE>STATUS exit
   then drop
   easy OPT-HEADER-DATA headers SETOPT-NUM dup CURLE-OK <> if
      idx STREAM-DROP drop
      idx REC-RELEASE
      CODE>STATUS exit
   then drop
   CURL-LOCK TASK:GET
   STOP-FLAG atomic@ 0= {: live:bool :}
   live if STATE-CLAIMED idx REC-STATE! then
   CURL-LOCK TASK:RELEASE
   live 0= if idx STREAM-DROP drop idx REC-RELEASE E-STATE throw then
   WAKE-POKE
   CURL-STATUS:ok ;


\ No halt may leave a half-initialized transfer behind. Once published, the
\ task's exit cleanup owns abandonment until a terminal result is collected.
: START-WITH ( handle ptr u8 len n -- status )
   TASK:DEFER-ENTER
   [: START-WITH-BODY ;] [: TASK:DEFER-LEAVE ;] finally ;


: STREAM-RESULT-BUILD ( n n n n -- chunk-result ) {: kind:n status:n u:n rc:n :}
   kind REQUEST-FAILED = if rc >CODE CURL-CHUNK--RESULT:failed exit then
   kind REQUEST-RESPONSE = if
      status >HTTP-STATUS CURL-CHUNK--RESULT:response exit
   then
   status >HTTP-STATUS u >LEN CURL-CHUNK--RESULT:chunk ;


: STREAM-RESULT-TAKE ( n -- chunk-result ) {: idx:n :}
   CURL-LOCK TASK:GET
   idx REC R.REQUEST atomic@ {: kind:n :}
   idx REC R.STATUS @ idx REC R.DELIVERED @ idx REC R.CODE @
      {: status:n u:n rc:n :}
   kind REQUEST-CHUNK = if
      REQUEST-NONE idx REC R.REQUEST atomic!
   else
      idx REC R.BODY @ idx REC R.LEN @ BYTES-RELEASE
      idx REC-RELEASE
   then
   CURL-LOCK TASK:RELEASE
   kind status u rc STREAM-RESULT-BUILD ;

public

: START ( handle ptr u8 len -- status ) {: subject:handle target capacity:len :}
   subject target capacity MODE-WHOLE START-WITH ;


\ Queues the same prepared handle for incremental receive. NEXT supplies each
\ destination; no caller span is held while the transfer is in flight.
: STREAM-START ( handle -- status ) {: subject:handle :}
   subject BUF-CELL 0 >LEN MODE-STREAM START-WITH ;


: NEXT ( handle ptr u8 len -- chunk-result ) {: subject:handle target capacity:len :}
   subject HANDLE-CELL OWNED-INDEX {: idx:n :}
   idx REC-MODE@ MODE-STREAM <> if E-STATE throw then
   capacity LEN>N 1 AT-LEAST
   CURL-LOCK TASK:GET
   idx REC R.REQUEST atomic@ REQUEST-NONE <> if
      CURL-LOCK TASK:RELEASE E-STATE throw
   then
   target idx REC-TARGET!
   capacity LEN>N idx REC R.CAP !
   idx REC-STATE@ STATE-DONE =
      idx REC R.OFFSET @ idx REC R.LEN @ >= and if
      idx REC R.KIND @ KIND-FAILED = if REQUEST-FAILED else REQUEST-RESPONSE then
      true
   else REQUEST-WAITING false then
   {: request:n ready:bool :}
   request idx REC R.REQUEST atomic!
   CURL-LOCK TASK:RELEASE
   ready if idx STREAM-RESULT-TAKE exit then
   WAKE-POKE
   begin
      TASK:STOP
      TASK:HALTED? if
         idx ABANDON
         TASK:PAUSE
         \ PAUSE returns when termination is deferred. The record is already
         \ given back, so never read or park on it again.
         CURLE-ABORTED >CODE CURL-CHUNK--RESULT:failed exit
      then
      idx REC R.REQUEST atomic@ REQUEST-WAITING <> if
         idx STREAM-RESULT-TAKE exit
      then
   again ;


\ Waits for that handle's transfer and answers the same fetch-result PERFORM
\ would have. It is what gives the record back, so every started transfer is
\ awaited exactly once, cancelled ones included. A handle with no transfer in
\ flight, and one another task started, are E-STATE.
\
\ The wait is a TASK:STOP loop, so it costs no CPU and the main thread may wait
\ too. A TASK:HALT while it waits ends the calling task at the PAUSE below, and
\ the record is abandoned first: ABANDON returns once the loop has let go of the
\ handle and the span, and the loop wakes nobody after that, which is what keeps
\ it from waking a TCB the join has released. The wait has no PAUSE of its own:
\ HALT posts the park this STOP waits on, so every halt is read at HALTED? and
\ ends the task only at the PAUSE after ABANDON.
: AWAIT ( handle -- fetch-result ) {: subject:handle :}
   subject HANDLE-CELL OWNED-INDEX {: idx:n :}
   idx REC-MODE@ MODE-WHOLE <> if E-STATE throw then
   begin
      idx REC-STATE@ STATE-DONE = if idx RESULT-TAKE exit then
      TASK:STOP
      TASK:HALTED? if
         idx ABANDON
         TASK:PAUSE
         CURLE-ABORTED FETCH-FAILED exit
      then
   again ;


\ Ends a transfer early: the loop takes the handle out, drops whatever body it
\ had and answers its owner with CURLE_ABORTED_BY_CALLBACK, the code libcurl
\ uses for a transfer an application ended. A transfer that has already finished
\ keeps its result, so a cancel that loses that race changes nothing. AWAIT is
\ still what collects the answer and frees the record. A handle with no transfer
\ in flight is E-STATE.
: CANCEL ( handle -- ) {: subject:handle :}
   subject HANDLE-CELL {: easy:n :}
   CURL-LOCK TASK:GET
   easy REC-FIND dup 0 >= if dup CANCEL-MARK then
   CURL-LOCK TASK:RELEASE
   0 < if E-STATE throw then
   WAKE-POKE ;


private

: HANDLE-BUSY? ( n -- bool ) {: easy:n :}
   LOOP-LIVE atomic@ 0= if false exit then
   CURL-LOCK TASK:GET
   easy REC-FIND 0 >=
   CURL-LOCK TASK:RELEASE ;


: HEADER-STATUS? ( ptr u8 n n -- bool ) {: source at:n u:n :}
   at 5 + u > if false exit then
   source at + 5 s" HTTP/" STR= ;


: HEADER-LF ( ptr u8 n n -- n ) {: source at:n u:n :}
   at begin dup u < while
      source over + c@ $0A = if exit then
      1+
   repeat drop -1 ;


: HEADER-BLANK? ( ptr u8 n n -- bool ) {: source at:n lf:n :}
   lf at = if true exit then
   lf at 1+ = if source at + c@ $0D = exit then
   false ;


: HEADER-CANDIDATE ( n -- ) {: at:n :}
   at LEN-CELL LE:U64!
   0 HBUF-CELL LE:U64!
   1 HLEN-CELL LE:U64! ;


\ Scratch offsets are task-local; every status line invalidates the previous
\ complete candidate, and trailer lines after a blank line cannot extend it.
: FINAL-BLOCK ( ptr u8 n -- n n ) {: source u:n :}
   0 BUF-CELL LE:U64!
   0 LEN-CELL LE:U64!
   0 HBUF-CELL LE:U64!
   0 HLEN-CELL LE:U64!
   begin BUF-CELL LE:U64@ u < while
      BUF-CELL LE:U64@ {: at:n :}
      source at u HEADER-LF dup 0 < if
         source at u HEADER-STATUS? if at HEADER-CANDIDATE then
         drop u BUF-CELL LE:U64!
      else
         {: lf:n :}
         source at lf HEADER-STATUS? if at HEADER-CANDIDATE then
         HLEN-CELL LE:U64@ 0 <> HBUF-CELL LE:U64@ 0= and if
            source at lf HEADER-BLANK? if lf 1+ HBUF-CELL LE:U64! then
         then
         lf 1+ BUF-CELL LE:U64!
      then
   repeat
   HBUF-CELL LE:U64@ dup 0= if drop E-RESULT throw then
   LEN-CELL LE:U64@ swap ;

public

\ The destination is borrowed only for this copy. It is untouched on an
\ unavailable result or malformed capture, and the result names the whole
\ final block even when only a prefix fits.
: HEADERS ( handle ptr u8 len -- header-result )
   {: subject:handle target capacity:len :}
   subject HANDLE-CELL {: easy:n :}
   capacity LEN>N 0 < if E-OPERAND throw then
   easy HANDLE-BUSY? if E-STATE throw then
   easy STATE-GET {: state:n :}
   state STATE-VALID@ 0= if E-STATE throw then
   state STATE-RESP@ >BYTES state STATE-LEN@ FINAL-BLOCK
      {: at:n finish:n :}
   finish at - {: u:n :}
   u capacity LEN>N min {: copied:n :}
   copied 0 > if
      target state STATE-RESP@ >BYTES at + FFI:>CELL copied COPY-OUT drop
   then
   u >LEN
   u capacity LEN>N > if CURL-HEADER--RESULT:truncated
   else CURL-HEADER--RESULT:complete then ;


\ libcurl writes the body into the span as it arrives, never past it. An HTTP
\ error status is a `response`, not `failed`. A body longer than the span is
\ `truncated`, its length the body bytes the span holds; on the write that
\ overflows, glibc may hold fewer than the span. A followed redirect's own body
\ never reaches the span; a `failed` or cancelled transfer may leave part of
\ its body there. The first libcurl error wins; only an otherwise successful
\ transfer maps a close failure to 23.
: PERFORM ( handle ptr u8 len -- fetch-result ) {: subject:handle target capacity:len :}
   subject HANDLE-CELL {: easy:n :}
   capacity LEN>N 0 AT-LEAST
   easy HANDLE-BUSY? if E-STATE throw then
   easy STATE-GET STATE-CLEAR-RESPONSE
   target capacity LEN>N SPAN-OPEN dup 0= if
      drop CURLE-OUT-OF-MEMORY FETCH-FAILED exit
   then
   {: body:n :}
   HBUF-CELL HLEN-CELL STREAM-OPEN dup 0= if
      drop
      body SPAN-TAKE drop drop drop
      CURLE-OUT-OF-MEMORY FETCH-FAILED exit
   then
   {: headers:n :}
   easy body headers TRANSFER {: rc:n :}
   body SPAN-TAKE {: u:n refused:bool bodyrc:n :}
   headers HBUF-CELL HLEN-CELL STREAM-TAKE {: captured:n capturedu:n headrc:n :}
   easy DATA-CLEAR {: clear-rc:n :}
   rc refused OVERFLOW? {: full:bool :}
   rc full BODY-RC bodyrc FIRST-ERROR headrc FIRST-ERROR clear-rc FIRST-ERROR
      {: outcome:n :}
   outcome CURLE-OK <> if
      captured capturedu BYTES-RELEASE
      outcome FETCH-FAILED exit
   then
   easy STATUS-TRY {: status:n valid:bool :}
   valid 0= if captured capturedu BYTES-RELEASE E-RESULT throw then
   easy captured capturedu STATE-PUBLISH
   status u full FETCH-RESULT ;


\ A later use of the handle is a use-after-free the nominal type does not catch.
: CLEANUP ( handle -- ) {: subject:handle :}
   subject HANDLE-CELL {: easy:n :}
   easy HANDLE-BUSY? if E-STATE throw then
   easy STATE-GET {: state:n :}
   easy EASY-CLEANUP
   state STATE-DESTROY ;

;package
