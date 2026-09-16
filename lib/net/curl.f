\ Linux AArch64 HTTPS client over libcurl's easy interface.
require lib/errors.f
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/image-lifecycle.f
require lib/task.f

package CURL
public

DEFTYPE HANDLE
DEFTYPE CODE
DEFTYPE HTTP-STATUS

SUMTYPE init-result 0
   VARIANT ready handle ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

SUMTYPE status 0
   VARIANT ok ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

SUMTYPE fetch-result 0
   VARIANT response http-status len ;VARIANT
   VARIANT truncated http-status len ;VARIANT
   VARIANT failed code ;VARIANT
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
OPT-OBJECT 2 + constant OPT-URL           \ CURLOPT_URL
OPT-OBJECT 23 + constant OPT-HTTP-HEADER  \ CURLOPT_HTTPHEADER
OPT-OBJECT 31 + constant OPT-COOKIE-FILE  \ CURLOPT_COOKIEFILE
OPT-OBJECT 36 + constant OPT-METHOD       \ CURLOPT_CUSTOMREQUEST
OPT-OBJECT 82 + constant OPT-COOKIE-JAR   \ CURLOPT_COOKIEJAR
OPT-OBJECT 103 + constant OPT-PRIVATE     \ CURLOPT_PRIVATE
OPT-OBJECT 165 + constant OPT-COPY-BODY   \ CURLOPT_COPYPOSTFIELDS
OPT-LONG 52 + constant OPT-FOLLOW         \ CURLOPT_FOLLOWLOCATION
OPT-LONG 60 + constant OPT-BODY-SIZE      \ CURLOPT_POSTFIELDSIZE
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

1 constant PROTO-HTTP                     \ CURLPROTO_HTTP
2 constant PROTO-HTTPS                    \ CURLPROTO_HTTPS
PROTO-HTTP PROTO-HTTPS or constant PROTO-WEB

$4000000 constant MAX-BYTES               \ request body and response capacity
$10000 constant MAX-TEXT                  \ one URL, method, path or header line
$7FFFFFFF constant MAX-TIMEOUT            \ CURLOPT_TIMEOUT_MS takes a C long
999 constant MAX-STATUS
$08 constant CELL-BYTES

\ Foreign out-parameter cells, per task like FFI's own argument staging.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $18 TASK:+USER IO-STORAGE drop
: INFO-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: BUF-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $08 + ;
: LEN-CELL ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $10 + ;


\ libcurl's easy interface. CURL*, curl_slist* and FILE* are OPAQUE here -
\ nothing in this package dereferences one - so they are declared `n`: AAPCS64
\ passes a pointer and an integer in the same register, and a cell is what the
\ nominal handle types wrap. Only a span Habu or the callee really reads or
\ writes is declared `ptr u8`.
\
\ curl_easy_setopt and curl_easy_getinfo are variadic. On Linux AAPCS64 a
\ variadic integer or pointer argument uses the same register as a fixed one, so
\ one declaration per ARGUMENT SHAPE is exact, and the two shapes share a symbol
\ under different Habu names. INIT's platform gate is what keeps that true:
\ Apple's ARM64 variant passes variadic arguments on the stack instead.
LIBRARY libcurl.so.4

FUNCTION: GLOBAL-INIT curl_global_init ( n -- n ) ;FUNCTION
FUNCTION: EASY-INIT curl_easy_init ( -- n ) ;FUNCTION
FUNCTION: EASY-CLEANUP curl_easy_cleanup ( n -- ) ;FUNCTION
FUNCTION: EASY-PERFORM curl_easy_perform ( n -- n ) ;FUNCTION
FUNCTION: SETOPT-NUM curl_easy_setopt ( n n n -- n ) ;FUNCTION
FUNCTION: SETOPT-SPAN curl_easy_setopt ( n n ptr u8 -- n ) ;FUNCTION
FUNCTION: SLIST-APPEND curl_slist_append ( n n -- n ) ;FUNCTION
FUNCTION: SLIST-FREE curl_slist_free_all ( n -- ) ;FUNCTION

FUNCTION: GETINFO-CELL curl_easy_getinfo ( n n ptr u8 -- n )
   2 $08 WRITES-BYTES                     \ one C long, or one char*
;FUNCTION


\ The process's own libc. Habu cannot hand libcurl a callback into checked code,
\ and none is needed: libcurl's default write callback is fwrite, so
\ CURLOPT_WRITEDATA takes an open_memstream stream and memcpy moves the finished
\ bytes into the caller's span. strndup/strlen/free build the NUL-terminated
\ copies libcurl's string options read.
PROCESS-SYMBOLS

FUNCTION: STREAM-CLOSE fclose ( n -- n ) ;FUNCTION
FUNCTION: DUP-TEXT strndup ( ptr u8 n -- n ) ;FUNCTION
FUNCTION: TEXT-LENGTH strlen ( n -- n ) ;FUNCTION
FUNCTION: RELEASE free ( n -- ) ;FUNCTION

FUNCTION: MEMSTREAM open_memstream ( ptr u8 ptr u8 -- n )
   0 $08 WRITES-BYTES                     \ char **bufp
   1 $08 WRITES-BYTES                     \ size_t *sizep
;FUNCTION

FUNCTION: COPY-OUT memcpy ( ptr u8 n n -- n )
   0 2 WRITES-ARG                         \ the caller's span, length from arg 2
;FUNCTION


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LE64@ ( ptr u8 -- n ) {: source :}
   0 CELL-BYTES 0 do source 7 i - + c@ swap 8 lshift or loop ;


: CELL-CLEAR ( ptr u8 -- ) {: target :}
   CELL-BYTES 0 do 0 target i + c! loop ;


: PLATFORM ( -- )
   HB-TARGET-LINUX? 0= if E-PLATFORM throw then ;


: HANDLE-CELL ( handle -- n ) {: subject:handle :}
   subject HANDLE>N dup 0= if E-STATE throw then ;


: CODE>STATUS ( n -- status ) {: rc:n :}
   rc CURLE-OK = if CURL-STATUS:ok exit then
   rc >CODE CURL-STATUS:failed ;


: OUT-OF-MEMORY ( -- status )
   CURLE-OUT-OF-MEMORY >CODE CURL-STATUS:failed ;


\ A NUL-terminated copy libcurl reads and duplicates for itself, released as soon
\ as the call returns. strndup stops at an embedded NUL, and a silently shortened
\ URL or header line is exactly what must not happen, so the copy is measured.
\ Zero answers an allocation failure; every other refusal throws.
: TEXT-DUP ( ptr u8 n -- n ) {: text u:n :}
   u 0 MAX-TEXT WITHIN-RANGE
   text u DUP-TEXT dup 0= if exit then
   dup TEXT-LENGTH u <> if RELEASE E-OPERAND throw then ;


: SET-NUM-RAW ( handle n n -- n ) {: subject:handle option:n value:n :}
   subject HANDLE-CELL option value SETOPT-NUM ;


: SET-TEXT-RAW ( handle n ptr u8 n -- n ) {: subject:handle option:n text u:n :}
   subject HANDLE-CELL {: easy:n :}
   text u TEXT-DUP dup 0= if drop CURLE-OUT-OF-MEMORY exit then
   {: copy:n :}
   easy option copy SETOPT-NUM {: rc:n :}
   copy RELEASE
   rc ;


: SET-TEXT ( handle n ptr u8 n -- status )
   SET-TEXT-RAW CODE>STATUS ;


\ The handle's own private slot holds the header list, so CLEANUP frees exactly
\ the list that handle owns and no package-side table has to track it.
: HEADERS-CELL ( n -- n ) {: easy:n :}
   INFO-CELL CELL-CLEAR
   easy INFO-PRIVATE INFO-CELL GETINFO-CELL CURLE-OK <> if E-RESULT throw then
   INFO-CELL LE64@ ;


\ A list this call created and could not publish is freed here; an appended-to
\ list is already the head the private slot holds, so it stays owned either way.
: HEADER-STORE ( n n n -- status ) {: easy:n old:n list:n :}
   easy OPT-PRIVATE list SETOPT-NUM dup CURLE-OK <> if
      old 0= if list SLIST-FREE then
      CODE>STATUS exit
   then drop
   easy OPT-HTTP-HEADER list SETOPT-NUM CODE>STATUS ;


: HEADER-APPEND ( n ptr u8 n -- status ) {: easy:n text u:n :}
   text u TEXT-DUP dup 0= if drop OUT-OF-MEMORY exit then
   {: line:n :}
   easy HEADERS-CELL {: old:n :}
   old line SLIST-APPEND {: list:n :}
   line RELEASE
   list 0= if OUT-OF-MEMORY exit then
   easy old list HEADER-STORE ;


: STREAM-OPEN ( -- n )
   BUF-CELL CELL-CLEAR
   LEN-CELL CELL-CLEAR
   BUF-CELL LEN-CELL MEMSTREAM ;


\ open_memstream publishes its buffer and length when the stream closes; the
\ buffer is the caller's to free from then on.
: STREAM-TAKE ( n -- n n ) {: stream:n :}
   stream STREAM-CLOSE drop
   BUF-CELL LE64@ LEN-CELL LE64@ ;


: WRITE-DATA-CLEAR ( n -- ) {: easy:n :}
   easy OPT-WRITE-DATA 0 SETOPT-NUM drop ;


: TRANSFER ( n n -- n ) {: easy:n stream:n :}
   easy OPT-WRITE-DATA stream SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy EASY-PERFORM ;


: STATUS-READ ( n -- http-status ) {: easy:n :}
   INFO-CELL CELL-CLEAR
   easy INFO-STATUS INFO-CELL GETINFO-CELL CURLE-OK <> if E-RESULT throw then
   INFO-CELL LE64@ dup 0 MAX-STATUS WITHIN-RANGE >HTTP-STATUS ;


: BODY-COPY ( ptr u8 n n -- ) {: target source:n u:n :}
   u 0= if exit then
   source 0= if E-RESULT throw then
   target source u COPY-OUT drop ;


: BODY-TAKE ( ptr u8 len n n -- ) {: target capacity:len source:n u:n :}
   u 0 < if E-RESULT throw then
   u capacity LEN>N > if target source capacity LEN>N BODY-COPY exit then
   target source u BODY-COPY ;


: FETCH-RESULT ( n len n -- fetch-result ) {: easy:n capacity:len u:n :}
   u capacity LEN>N > if
      easy STATUS-READ u >LEN CURL-FETCH--RESULT:truncated exit
   then
   easy STATUS-READ u >LEN CURL-FETCH--RESULT:response ;


: FETCH-FAILED ( n -- fetch-result ) {: rc:n :}
   rc >CODE CURL-FETCH--RESULT:failed ;


: FLAG>N ( bool -- n )
   if 1 else 0 then ;


variable GLOBAL-DONE
variable GLOBAL-REGISTERED


\ A captured image is restored into another process, where libcurl is not
\ initialised; the flag is cleared then, exactly as FFI clears its symbol cache.
: FORGET-GLOBAL ( -- )
   0 GLOBAL-DONE ! ;


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
\ file:// or ftp:// URL out of a scraped page cannot reach the filesystem or
\ another service through this handle. The bitmask options are deprecated in
\ favour of CURLOPT_PROTOCOLS_STR (318) and CURLOPT_REDIR_PROTOCOLS_STR (319) but
\ still honoured, and they carry no version floor; move to the string form once
\ the libcurl floor is 7.85.
: SCHEMES ( n -- n ) {: easy:n :}
   easy OPT-PROTOCOLS PROTO-WEB SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy OPT-REDIR-PROTO PROTO-WEB SETOPT-NUM ;


\ libcurl must not reach for signals inside a tasked engine, and the private slot
\ starts empty so HEADERS-CELL reads a real absent list.
: DEFAULTS ( n -- n ) {: easy:n :}
   easy OPT-NO-SIGNAL 1 SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy SCHEMES dup CURLE-OK <> if exit then drop
   easy OPT-PRIVATE 0 SETOPT-NUM ;

public

\ A handle that cannot be initialised or configured is cleaned up before INIT
\ answers, so a failure never leaves one allocated.
: INIT ( -- init-result )
   PLATFORM
   GLOBAL-READY dup CURLE-OK <> if >CODE CURL-INIT--RESULT:failed exit then drop
   EASY-INIT dup 0= if
      drop CURLE-FAILED-INIT >CODE CURL-INIT--RESULT:failed exit
   then
   {: easy:n :}
   easy DEFAULTS dup CURLE-OK <> if
      easy EASY-CLEANUP >CODE CURL-INIT--RESULT:failed exit
   then drop
   easy >HANDLE CURL-INIT--RESULT:ready ;


: URL! ( handle ptr u8 n -- status ) {: subject:handle text u:n :}
   u 1 MAX-TEXT WITHIN-RANGE
   subject OPT-URL text u SET-TEXT ;


\ libcurl picks GET by default and POST once BODY! has been used; this overrides
\ the verb without changing anything else about the request.
: METHOD! ( handle ptr u8 n -- status ) {: subject:handle text u:n :}
   u 1 MAX-TEXT WITHIN-RANGE
   subject OPT-METHOD text u SET-TEXT ;


\ One "Name: value" line, appended to the list this handle already owns.
: HEADER+ ( handle ptr u8 n -- status ) {: subject:handle text u:n :}
   u 1 MAX-TEXT WITHIN-RANGE
   subject HANDLE-CELL text u HEADER-APPEND ;


\ The span is borrowed for this call: libcurl copies the body out of it, and its
\ declared length is set first because the copy reads that length.
: BODY! ( handle ptr u8 n -- status ) {: subject:handle bytes u:n :}
   u 0 MAX-BYTES WITHIN-RANGE
   subject OPT-BODY-SIZE u SET-NUM-RAW dup CURLE-OK <> if CODE>STATUS exit then drop
   subject HANDLE-CELL OPT-COPY-BODY bytes SETOPT-SPAN CODE>STATUS ;


\ Cookies read at request time. An empty path starts the cookie engine with no
\ stored cookies, which is libcurl's own spelling for "keep session cookies".
: COOKIE-FILE! ( handle ptr u8 n -- status ) {: subject:handle path u:n :}
   subject OPT-COOKIE-FILE path u SET-TEXT ;


\ Cookies written back when the handle is cleaned up.
: COOKIE-JAR! ( handle ptr u8 n -- status ) {: subject:handle path u:n :}
   u 1 MAX-TEXT WITHIN-RANGE
   subject OPT-COOKIE-JAR path u SET-TEXT ;


\ The whole transfer's limit, not one read's. Zero removes it.
: TIMEOUT! ( handle ms -- status ) {: subject:handle limit:ms :}
   limit MS>N 0 MAX-TIMEOUT WITHIN-RANGE
   subject OPT-TIMEOUT-MS limit MS>N SET-NUM-RAW CODE>STATUS ;


: FOLLOW! ( handle bool -- status ) {: subject:handle on:bool :}
   subject OPT-FOLLOW on FLAG>N SET-NUM-RAW CODE>STATUS ;


\ The body arrives without a Habu callback: libcurl's default write callback is
\ fwrite into CURLOPT_WRITEDATA, an open_memstream stream, and the finished bytes
\ are copied into the caller's span. A body larger than the span is TRUNCATED and
\ carries the WHOLE body's length while capacity bytes were copied, so a short
\ read is always visible. The stream and its buffer are released on every branch.
: PERFORM ( handle ptr u8 len -- fetch-result ) {: subject:handle target capacity:len :}
   subject HANDLE-CELL {: easy:n :}
   capacity LEN>N 1 MAX-BYTES WITHIN-RANGE
   STREAM-OPEN dup 0= if drop CURLE-OUT-OF-MEMORY FETCH-FAILED exit then
   {: stream:n :}
   easy stream TRANSFER {: rc:n :}
   stream STREAM-TAKE {: source:n u:n :}
   easy WRITE-DATA-CLEAR
   rc CURLE-OK <> if source RELEASE rc FETCH-FAILED exit then
   target capacity source u BODY-TAKE
   source RELEASE
   easy capacity u FETCH-RESULT ;


\ The handle is dead afterwards and its header list goes with it; neither is ever
\ left allocated.
: CLEANUP ( handle -- ) {: subject:handle :}
   subject HANDLE-CELL {: easy:n :}
   easy HEADERS-CELL {: list:n :}
   easy EASY-CLEANUP
   list 0= if exit then
   list SLIST-FREE ;

;package
