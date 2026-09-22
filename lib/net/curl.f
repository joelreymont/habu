\ Linux AArch64 HTTPS client over libcurl's easy interface.
\
\ STORAGE CLASS. TASK-LOCAL for everything a call runs through: the three
\ foreign out-parameter cells are one $18 TASK:+USER row, so each task reads
\ back its own info, buffer and length, and the request and response spans are
\ caller-owned. The one-time global init flags (GLOBAL-DONE,
\ GLOBAL-REGISTERED) are PROCESS-WIDE, which is what curl_global_init requires.
\ So is the multiplexed transfer table at the end of this file: one loop task
\ owns the multi handle and the records, and a record is claimed atomically by
\ whichever task starts a transfer. See docs/threads.md.
require lib/errors.f
require lib/string.f                      \ BUFFER:, the transfer table's byte rows
require lib/le.f                          \ the foreign cells are little-endian
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/image-lifecycle.f
require lib/task.f
require lib/aio.f                         \ the loop waits on AIO tickets
require lib/adt/result.f                  \ TASK:JOIN's answer, read by LOOP-STOP

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
E-CURL-CAPACITY constant E-CAPACITY

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
OPT-LONG 19 + constant OPT-LOW-SPEED-LIMIT \ CURLOPT_LOW_SPEED_LIMIT
OPT-LONG 20 + constant OPT-LOW-SPEED-TIME  \ CURLOPT_LOW_SPEED_TIME
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
$7FFFFFFF constant MAX-TIMEOUT            \ a C long option's own ceiling
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


\ libcurl's multi interface, the transfer driver the loop at the end of this
\ file runs on. CURLM* is opaque like CURL*, every answer is a CURLMcode, and
\ each out-parameter is a C int, so each line is checkable against
\ /usr/include/curl/multi.h on sight. curl_multi_info_read answers a CURLMsg*
\ or zero, which is foreign memory this package copies before it reads, and
\ curl_multi_fdset fills three fd_sets the CALLER cleared, which is why each of
\ them is declared as the whole FD_SETSIZE bitmap it writes into.
FUNCTION: MULTI-INIT curl_multi_init ( -- n ) ;FUNCTION
FUNCTION: MULTI-CLEANUP curl_multi_cleanup ( n -- n ) ;FUNCTION
FUNCTION: MULTI-ADD curl_multi_add_handle ( n n -- n ) ;FUNCTION
FUNCTION: MULTI-REMOVE curl_multi_remove_handle ( n n -- n ) ;FUNCTION

FUNCTION: MULTI-PERFORM curl_multi_perform ( n ptr u8 -- n )
   1 $04 WRITES-BYTES                     \ int *running_handles
;FUNCTION

FUNCTION: MULTI-INFO-READ curl_multi_info_read ( n ptr u8 -- n )
   1 $04 WRITES-BYTES                     \ int *msgs_in_queue
;FUNCTION

FUNCTION: MULTI-FDSET curl_multi_fdset ( n ptr u8 ptr u8 ptr u8 ptr u8 -- n )
   1 $80 WRITES-BYTES                     \ fd_set *read_fd_set
   2 $80 WRITES-BYTES                     \ fd_set *write_fd_set
   3 $80 WRITES-BYTES                     \ fd_set *exc_fd_set
   4 $04 WRITES-BYTES                     \ int *max_fd
;FUNCTION

FUNCTION: MULTI-TIMEOUT curl_multi_timeout ( n ptr u8 -- n )
   1 $08 WRITES-BYTES                     \ long *milliseconds
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


\ Every foreign cell here is little-endian and LE: reads it: LE:U64@ for a
\ pointer or a C long, LE:U32@ for the C int every multi out-parameter and both
\ CURLMsg enums are.
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
   INFO-CELL LE:U64@ ;


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
: STREAM-TAKE ( n ptr u8 ptr u8 -- n n ) {: stream:n buf len :}
   stream STREAM-CLOSE drop
   buf LE:U64@ len LE:U64@ ;


: WRITE-DATA-CLEAR ( n -- ) {: easy:n :}
   easy OPT-WRITE-DATA 0 SETOPT-NUM drop ;


: TRANSFER ( n n -- n ) {: easy:n stream:n :}
   easy OPT-WRITE-DATA stream SETOPT-NUM dup CURLE-OK <> if exit then drop
   easy EASY-PERFORM ;


: STATUS-READ ( n -- http-status ) {: easy:n :}
   INFO-CELL CELL-CLEAR
   easy INFO-STATUS INFO-CELL GETINFO-CELL CURLE-OK <> if E-RESULT throw then
   INFO-CELL LE:U64@ dup 0 MAX-STATUS WITHIN-RANGE >HTTP-STATUS ;


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


\ The body arrives without a Habu callback: libcurl's default write callback is
\ fwrite into CURLOPT_WRITEDATA, an open_memstream stream, and the finished bytes
\ are copied into the caller's span. A body larger than the span is TRUNCATED and
\ carries the WHOLE body's length while capacity bytes were copied, so a short
\ read is always visible. The stream and its buffer are released on every branch.
: PERFORM ( handle ptr u8 len -- fetch-result ) {: subject:handle target capacity:len :}
   subject HANDLE-CELL {: easy:n :}
   capacity LEN>N 1 MAX-BYTES WITHIN-RANGE
   BUF-CELL LEN-CELL STREAM-OPEN dup 0= if
      drop CURLE-OUT-OF-MEMORY FETCH-FAILED exit
   then
   {: stream:n :}
   easy stream TRANSFER {: rc:n :}
   stream BUF-CELL LEN-CELL STREAM-TAKE {: source:n u:n :}
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

private

\ ---- many transfers on one task ----------------------------------------------
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

$20 constant MAX-TRANSFERS                \ transfers in flight at once
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
5 constant STATE-ABANDONED                \ the owner was halted; the loop frees it
6 constant STATE-DONE                     \ the result is stored and the owner woken

0 constant KIND-RESPONSE                  \ which fetch-result AWAIT builds
1 constant KIND-TRUNCATED
2 constant KIND-FAILED

BEGIN-STRUCTURE REC-BYTES
   CELL +FIELD R.STATE
   CELL +FIELD R.EASY
   CELL +FIELD R.CAP
   CELL +FIELD R.STREAM
   CELL +FIELD R.ADDED                    \ the handle is in the multi handle
   CELL +FIELD R.KIND
   CELL +FIELD R.STATUS
   CELL +FIELD R.LEN
   CELL +FIELD R.CODE
END-STRUCTURE

\ One record per transfer, in cells the loop reads with atomic@ and atomic!.
\ BUFFER: and TYPED-BUFFER both allot zeroed storage on a cell-rounded address,
\ which is the alignment every row below wants - the atomic record fields, and
\ the fd_sets, the CURLMsg copy and the out-parameter cells libcurl reads and
\ writes as aligned C objects.
MAX-TRANSFERS REC-BYTES * 8 / TYPED-BUFFER REC-CELLS n

\ The owner as the TCB pointer TASK:WAKE takes and the span as the address it
\ is, both in declared rows, so this module needs no address cast of its own.
MAX-TRANSFERS TYPED-BUFFER REC-OWNER ptr n
MAX-TRANSFERS TYPED-BUFFER REC-TARGETS ptr u8

\ Two cells per record for open_memstream's buffer and length, because the loop
\ is what closes the stream and the opening task's row is not where it can read.
\ The FFI writes them, so the row is bytes.
MAX-TRANSFERS 2 * cells BUFFER: REC-STREAM-CELLS

\ The three fd_sets libcurl fills, the fds an armed ticket already covers, the
\ int and long out-parameters of the multi calls, one copied CURLMsg, and the
\ wake pipe's two ends.
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
AIO:GROUP-MAX TYPED-BUFFER FD-FDS n
AIO:GROUP-MAX TYPED-BUFFER FD-MASKS n
AIO:GROUP-MAX TYPED-BUFFER FD-TICKETS AIO:ticket

variable MULTI-CELL                       \ the CURLM* the loop drives
variable LOOP-LIVE                        \ atomic: a loop is running
variable STOP-FLAG                        \ atomic: the loop must end
variable LOCK-READY                       \ the facility has been initialized once
variable LOOP-THROW                       \ what ended the loop, reported by LOOP-STOP
variable LOOP-MRC                         \ the CURLMcode the loop's own body ended on
variable WAKE-R
variable WAKE-W
variable TIMER-MS                         \ the ms the pending timer holds, -1 for none
variable SHORT-TURN                       \ the sets wanted more tickets than there was room for

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


\ ---- the records -------------------------------------------------------------

: REC-CHECK ( n -- n ) {: idx:n :}
   idx 0 < idx MAX-TRANSFERS >= or if E-OPERAND throw then
   idx ;


: REC ( n -- ptr n ) REC-CHECK {: idx:n :}
   idx REC-BYTES * 8 / REC-CELLS ;


: REC-BUF-CELL ( n -- ptr u8 ) REC-CHECK {: idx:n :}
   REC-STREAM-CELLS idx $10 * + ;


: REC-LEN-CELL ( n -- ptr u8 ) REC-CHECK {: idx:n :}
   REC-STREAM-CELLS idx $10 * + $08 + ;


: REC-STATE@ ( n -- n )     REC R.STATE atomic@ ;
: REC-EASY@ ( n -- n )      REC R.EASY @ ;
: REC-CAP@ ( n -- n )       REC R.CAP @ ;
: REC-STREAM@ ( n -- n )    REC R.STREAM @ ;
: REC-TARGET@ ( n -- ptr u8 ) REC-CHECK REC-TARGETS @ ;
: REC-OWNER@ ( n -- ptr n ) REC-CHECK REC-OWNER @ ;


: REC-STATE! ( n n -- ) {: state:n idx:n :}
   state idx REC R.STATE atomic! ;


\ One claim wins: the state cell moves FREE -> CLAIMING in one step, so two
\ tasks starting a transfer at the same moment are handed different records.
\ CLAIMING is a state the loop passes over, so a record is never added to the
\ multi handle before the submitter has finished filling it.
: REC-CLAIM ( -- n )
   MAX-TRANSFERS 0 ?do
      STATE-FREE STATE-CLAIMING i REC R.STATE atomic-cas STATE-FREE = if
         i unloop exit
      then
   loop
   -1 ;


\ A record is free again only after its handle is cleared, so a scan can never
\ match a record whose transfer is over.
: REC-RELEASE ( n -- ) {: idx:n :}
   0 idx REC R.EASY !
   0 idx REC R.STREAM !
   0 idx REC R.ADDED !
   STATE-FREE idx REC-STATE! ;


: TABLE-CLEAR ( -- )
   MAX-TRANSFERS 0 ?do i REC-RELEASE loop ;


\ The record a handle is in, or -1. A handle is in at most one record at a time
\ - START refuses a second one - so the first match is the only match. This is
\ how a task finds its own transfer: asking libcurl about a handle the loop may
\ be driving is exactly what it must not do.
: REC-FIND ( n -- n ) {: easy:n :}
   MAX-TRANSFERS 0 ?do
      i REC-STATE@ STATE-FREE <> if
         i REC-EASY@ easy = if i unloop exit then
      then
   loop
   -1 ;


: ANY-BUSY? ( -- bool )
   MAX-TRANSFERS 0 ?do
      i REC-STATE@ STATE-FREE <> if true unloop exit then
   loop
   false ;


: REC-FILL ( n n ptr u8 n n -- ) {: idx:n easy:n target capacity:n stream:n :}
   easy idx REC R.EASY !
   capacity idx REC R.CAP !
   stream idx REC R.STREAM !
   0 idx REC R.ADDED !
   target idx REC-CHECK REC-TARGETS !
   TASK:SELF idx REC-CHECK REC-OWNER !
   KIND-FAILED idx REC R.KIND !
   0 idx REC R.STATUS !
   0 idx REC R.LEN !
   CURLE-ABORTED idx REC R.CODE ! ;


\ ---- the wake pipe -----------------------------------------------------------
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


\ ---- the loop's side of a record ---------------------------------------------

: REC-STREAM-TAKE ( n -- n n ) {: idx:n :}
   idx REC-STREAM@ idx REC-BUF-CELL idx REC-LEN-CELL STREAM-TAKE ;


\ The stream is closed and whatever it collected is dropped: the handle keeps no
\ pointer to a closed stream and the buffer is nobody's after this.
: STREAM-DROP ( n -- ) {: idx:n :}
   idx REC-STREAM@ 0= if exit then
   idx REC-STREAM-TAKE {: source:n u:n :}
   idx REC-EASY@ WRITE-DATA-CLEAR
   0 idx REC R.STREAM !
   source RELEASE ;


: FAILED-STORE ( n n -- ) {: idx:n rc:n :}
   KIND-FAILED idx REC R.KIND !
   rc idx REC R.CODE ! ;


\ FETCH-RESULT's decision, stored as fields because the value belongs to the
\ task that waits: the WHOLE body's length either way, and truncated when only
\ capacity bytes of it were copied.
: RESULT-STORE ( n n n -- ) {: idx:n easy:n u:n :}
   easy STATUS-READ HTTP-STATUS>N idx REC R.STATUS !
   u idx REC R.LEN !
   u idx REC-CAP@ > if KIND-TRUNCATED idx REC R.KIND ! exit then
   KIND-RESPONSE idx REC R.KIND ! ;


\ The state store is the barrier: every field above is written before it, and
\ AWAIT reads them after its wait.
: SETTLE ( n -- ) {: idx:n :}
   STATE-DONE idx REC-STATE!
   idx REC-OWNER@ TASK:WAKE ;


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


\ A finished transfer, copied into the owner's span exactly as PERFORM does. An
\ owner that was halted while it waited gets nothing written anywhere: its
\ record is released and nobody is woken, which is what keeps this loop from
\ waking a TCB the join has released.
: FINISH ( n n -- n ) {: idx:n rc:n :}
   idx REMOVE-ONE dup CURLM-OK <> if exit then drop
   idx REC-STREAM-TAKE {: source:n u:n :}
   idx REC-EASY@ WRITE-DATA-CLEAR
   0 idx REC R.STREAM !
   idx REC-STATE@ STATE-ABANDONED = if
      source RELEASE idx REC-RELEASE CURLM-OK exit
   then
   rc CURLE-OK <> if
      source RELEASE
      idx rc FAILED-STORE
      idx SETTLE
      CURLM-OK exit
   then
   idx REC-TARGET@ idx REC-CAP@ >LEN source u BODY-TAKE
   source RELEASE
   idx idx REC-EASY@ u RESULT-STORE
   idx SETTLE
   CURLM-OK ;


\ The owner asked for this transfer to end: libcurl's own code for a transfer an
\ application ended is the answer it gets.
: CANCEL-RUN ( n -- n ) {: idx:n :}
   idx REMOVE-ONE dup CURLM-OK <> if exit then drop
   idx STREAM-DROP
   idx CURLE-ABORTED FAILED-STORE
   idx SETTLE
   CURLM-OK ;


: ABANDON-RUN ( n -- n ) {: idx:n :}
   idx REMOVE-ONE dup CURLM-OK <> if exit then drop
   idx STREAM-DROP
   idx REC-RELEASE
   CURLM-OK ;


: SERVICE-ONE ( n -- n ) {: idx:n :}
   idx REC-STATE@ {: st:n :}
   st STATE-CLAIMED = if idx ADD-ONE exit then
   st STATE-CANCEL = if idx CANCEL-RUN exit then
   st STATE-ABANDONED = if idx ABANDON-RUN exit then
   CURLM-OK ;


: SERVICE-RECORDS ( -- n )
   MAX-TRANSFERS 0 ?do
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
   REAP ;


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


\ ---- the descriptors libcurl is waiting on -----------------------------------
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
   AIO:GROUP-MAX 0 ?do 0 i FD-MASK! -1 i FD-FD! loop ;


: FD-FREE-SLOT ( -- n )
   AIO:GROUP-MAX 0 ?do i FD-MASK@ 0= if i unloop exit then loop
   -1 ;


: FD-SLOT-OF ( AIO:ticket -- n ) {: t:AIO:ticket :}
   AIO:GROUP-MAX 0 ?do
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


\ Two of the group's tickets are the wake pipe's and the timer's, so arming
\ stops two short of the group's own ceiling. An fd left unarmed is not a hang:
\ the turn below shortens the timer and the loop asks again.
: FD-ARM ( n n -- ) {: fd:n mask:n :}
   LOOP-GROUP AIO:GROUP-COUNT AIO:GROUP-MAX 2 - >= if 1 SHORT-TURN ! exit then
   FD-FREE-SLOT dup 0 < if drop 1 SHORT-TURN ! exit then
   {: slot:n :}
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
   AIO:GROUP-MAX 0 ?do i SLOT-REVIEW loop
   maxfd 1 + 0 ?do i FD-REVIEW loop
   CURLM-OK ;


\ ---- the time libcurl is waiting for -----------------------------------------

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
   WANT-MS {: mrc:n want:n :}
   mrc CURLM-OK <> if mrc exit then
   SHORT-TURN @ 0 <> if want IDLE-MS min else want then {: ms:n :}
   TIMER-MS @ 0 < if ms TIMER-ARM CURLM-OK exit then
   ms TIMER-MS @ >= if CURLM-OK exit then
   LOOP-GROUP AIO:GROUP-COUNT AIO:GROUP-MAX 1 - >= if CURLM-OK exit then
   TIMER-TICKET @ AIO:CANCEL
   -1 TIMER-MS !
   ms TIMER-ARM
   CURLM-OK ;


: WAKE-ARM ( -- )
   WAKE-R @ >FD AIO:READABLE -1 >MS AIO:POLL {: t:AIO:ticket :}
   t LOOP-GROUP AIO:GROUP+
   t WAKE-TICKET ! ;


\ ---- the loop ----------------------------------------------------------------

: OUTCOME-DROP ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;


\ Whatever ended, the loop answers it: the wake pipe is drained and the table
\ scanned, a fired timer is forgotten so the review below arms the next one, a
\ poll that ended frees its slot for the review to arm again, and a ticket no
\ slot owns is one this loop cancelled and is done with.
: TURN-ANSWER ( AIO:ticket -- n ) {: t:AIO:ticket :}
   t WAKE-TICKET @ SAME-TICKET? if WAKE-DRAIN WAKE-ARM LOCKED-SERVICE exit then
   t TIMER-TICKET @ SAME-TICKET? if -1 TIMER-MS ! CURLM-OK exit then
   t FD-SLOT-OF dup 0 >= if FD-RELEASE CURLM-OK exit then
   drop CURLM-OK ;


: TURN ( -- n )
   LOOP-GROUP AIO:AWAIT-ANY OUTCOME-DROP
   TURN-ANSWER dup CURLM-OK <> if exit then drop
   LOCKED-TRANSFERS dup CURLM-OK <> if exit then drop
   0 SHORT-TURN !
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
   st STATE-FREE = st STATE-DONE = or st STATE-CLAIMING = or if exit then
   idx REMOVE-ONE drop
   idx STREAM-DROP
   st STATE-ABANDONED = if idx REC-RELEASE exit then
   idx CURLE-ABORTED FAILED-STORE
   idx SETTLE ;


: FAIL-ALL ( -- )
   CURL-LOCK TASK:GET
   MAX-TRANSFERS 0 ?do i FAIL-ONE loop
   CURL-LOCK TASK:RELEASE ;


\ Every ticket this loop submitted is ended and handed back before the task
\ returns: a record still in flight is what AIO:STOP refuses, and a ticket
\ nobody awaits is a record nobody frees.
: GROUP-DRAIN ( -- )
   AIO:GROUP-MAX 0 ?do i FD-MASK@ 0 <> if i FD-DROP then loop
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


\ ---- a transfer's own steps --------------------------------------------------

\ The handle is written into the record before the facility is given back, so a
\ second START on it - from this task or another - already finds it in flight.
: CLAIM-BODY ( n -- n n ) {: easy:n :}    \ ( easy -- rc idx )
   easy REC-FIND 0 >= if E-STATE -1 exit then
   REC-CLAIM dup 0 < if drop E-CAPACITY -1 exit then
   {: idx:n :}
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


\ The owner is gone: the record is the loop's to end and nobody is to be woken.
: ABANDON ( n -- ) {: idx:n :}
   CURL-LOCK TASK:GET
   STATE-ABANDONED idx REC-STATE!
   CURL-LOCK TASK:RELEASE
   WAKE-POKE ;


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
   PLATFORM
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
   0 SHORT-TURN !
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
   ANY-BUSY? if E-STATE throw then
   0 LOOP-LIVE atomic!
   1 STOP-FLAG atomic!
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
\ soon as the record is queued. Every per-handle option - URL!, METHOD!,
\ HEADER+, BODY!, COOKIE-FILE!, COOKIE-JAR!, TIMEOUT!, LOW-SPEED!, FOLLOW! and
\ the scheme restriction - is set before this and holds for that transfer alone.
\ From here until AWAIT answers, the handle and the span belong to the loop: no
\ other task may touch either. A START with no loop running, and a second START
\ on a handle already in flight, are E-STATE; a table with no record free is
\ E-CAPACITY.
: START ( handle ptr u8 len -- status ) {: subject:handle target capacity:len :}
   subject HANDLE-CELL {: easy:n :}
   LOOP-LIVE atomic@ 0= if E-STATE throw then
   capacity LEN>N 1 MAX-BYTES WITHIN-RANGE
   easy START-CLAIM {: idx:n :}
   idx REC-BUF-CELL idx REC-LEN-CELL STREAM-OPEN dup 0= if
      drop idx REC-RELEASE OUT-OF-MEMORY exit
   then
   {: stream:n :}
   idx easy target capacity LEN>N stream REC-FILL
   easy OPT-WRITE-DATA stream SETOPT-NUM dup CURLE-OK <> if
      idx STREAM-DROP
      idx REC-RELEASE
      CODE>STATUS exit
   then drop
   STATE-CLAIMED idx REC-STATE!
   WAKE-POKE
   CURL-STATUS:ok ;


\ Waits for that handle's transfer and answers the same fetch-result PERFORM
\ would have. It is what gives the record back, so every started transfer is
\ awaited exactly once, cancelled ones included. A handle with no transfer in
\ flight, and one another task started, are E-STATE.
\
\ The wait is a TASK:STOP loop, so it costs no CPU and the main thread may wait
\ too. A TASK:HALT while it waits ends the calling task at the PAUSE below, and
\ the record is abandoned first: the loop frees it and wakes nobody, which is
\ what keeps the loop from waking a TCB the join has released. No TASK:AT-EXIT
\ is registered here - the abandon in this wait is what a halted waiter needs.
: AWAIT ( handle -- fetch-result ) {: subject:handle :}
   subject HANDLE-CELL OWNED-INDEX {: idx:n :}
   begin
      idx REC-STATE@ STATE-DONE = if idx RESULT-TAKE exit then
      TASK:STOP
      TASK:HALTED? if
         idx ABANDON
         TASK:PAUSE
         CURLE-ABORTED FETCH-FAILED exit
      then
      TASK:PAUSE
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

;package
