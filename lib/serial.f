\ Raw serial byte streams on Linux AArch64, through bounded libc bindings.
require lib/errors.f
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/cad-num-types.f
require lib/task.f
require lib/memory.f

package SERIAL
public

DEFTYPE HANDLE
DEFTYPE BAUD
DEFTYPE ERRNO

SUMTYPE open-result 0
   VARIANT opened handle ;VARIANT
   VARIANT failed errno ;VARIANT
   VARIANT unsupported ;VARIANT
;SUMTYPE

SUMTYPE status 0
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE io-result 0
   VARIANT transferred CAD-NUM:byte-len ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT closed ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

E-SERIAL-OPERAND constant E-OPERAND
E-SERIAL-PLATFORM constant E-PLATFORM
E-SERIAL-SYMBOL constant E-SYMBOL
E-SERIAL-RESULT constant E-RESULT

private

$2C constant TERM-BYTES
$80902 constant OPEN-FLAGS       \ O_RDWR | O_NONBLOCK | O_NOCTTY | O_CLOEXEC.
$802C542A constant GET-TERM      \ TCGETS2: kernel termios2, not libc termios.
$402C542B constant SET-TERM      \ TCSETS2: immediate, without flushing queues.
$100F100F constant BAUD-MASK
$100018B0 constant RAW-CONTROL   \ BOTHER both ways, CS8 | CREAD | CLOCAL.
1000000 constant NS-PER-MS

variable FN-OPEN
variable FN-IOCTL
variable FN-READ
variable FN-WRITE
variable FN-POLL
variable FN-CLOSE
variable FN-ERRNO
here FFI:>CELL 7 and 8 swap - 7 and allot
variable READY
create SYMBOL-NAME $20 allot

\ No per-operation buffer is process-global; independent tasks may use ports.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $60 TASK:+USER IO-STORAGE drop
: TERM ( -- ptr u8 ) IO-STORAGE ;
: SAVED ( -- ptr u8 ) IO-STORAGE $2C + ;
: POLLFD ( -- ptr u8 ) IO-STORAGE $58 + ;

CAST: BLEN>N ( CAD-NUM:byte-len -- n )


: RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LENGTH ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-OPERAND throw ENDOF
      zero OF E-OPERAND throw ENDOF
      overflow OF E-OPERAND throw ENDOF
      underflow OF E-OPERAND throw ENDOF
      bad-alignment OF E-OPERAND throw ENDOF
      misaligned OF E-OPERAND throw ENDOF
   ;MATCH ;


: CHECK-HANDLE ( handle -- ) HANDLE>N 0 $7FFFFFFF RANGE ;


: C-INT ( n -- n )
   $FFFFFFFF and dup $80000000 and 0 <> if $100000000 - then ;


: LE32! ( n ptr u8 -- ) {: value:n target :}
   4 0 do value i 8 * rshift $FF and target i + c! loop ;


: LE32@ ( ptr u8 -- n ) {: source :}
   source c@ source $01 + c@ 8 lshift or
   source $02 + c@ 16 lshift or source $03 + c@ 24 lshift or ;


\ RTLD_DEFAULT borrows process symbols; the native executable already needs
\ libc.so.6. No library reference is acquired or retained by this module.
: SYMBOL ( ptr u8 n -- n )
   SYMBOL-NAME FFI:CSTR 0 SYMBOL-NAME FFI:DLSYM
   dup 0= if E-SYMBOL throw then ;


: LOAD-SYMBOLS ( -- )
   s" open" SYMBOL FN-OPEN ! s" ioctl" SYMBOL FN-IOCTL !
   s" read" SYMBOL FN-READ ! s" write" SYMBOL FN-WRITE !
   s" poll" SYMBOL FN-POLL ! s" close" SYMBOL FN-CLOSE !
   s" __errno_location" SYMBOL FN-ERRNO ! ;


: INIT ( -- )
   HB-TARGET-LINUX? 0= if E-PLATFORM throw then
   begin
      READY atomic@ 2 = if exit then
      0 1 READY atomic-cas 0= if
         [: LOAD-SYMBOLS ;] catch dup 0 <> if 0 READY atomic! throw then drop
         2 READY atomic! exit
      then TASK:PAUSE
   again ;


\ Exact Linux AArch64 libc schemas. ioctl's requests have separate pointer
\ directions and an explicit 44-byte kernel termios2 layout. errno is a
\ libc-owned, thread-local C int. Retirement owner: checked foreign bindings.
\ test/serial.py covers these boundaries through real kernel pseudoterminals.
TRUSTED: ERRNO-POINTER ( -- ptr u8 )
   FFI:ARGS FFI:REG-LENS 0 FN-ERRNO @ ffi-call-bounded ;


: LAST-ERROR ( -- errno ) FFI:RESET ERRNO-POINTER LE32@ >ERRNO ;


TRUSTED: OPEN-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 2 FN-OPEN @ ffi-call-bounded ;


: OPEN-RAW ( ptr u8 -- n ) {: path :}
   FFI:RESET path 0 FFI:READABLE! OPEN-FLAGS 1 FFI:VALUE!
   OPEN-CALL C-INT ;


TRUSTED: GET-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-IOCTL @ ffi-call-bounded ;


: GET-RAW ( handle ptr u8 -- n ) {: handle:handle target :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE! GET-TERM 1 FFI:VALUE!
   target TERM-BYTES 2 FFI:WRITABLE!
   GET-CALL C-INT ;


TRUSTED: SET-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-IOCTL @ ffi-call-bounded ;


: SET-RAW ( handle ptr u8 -- n ) {: handle:handle source :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE! SET-TERM 1 FFI:VALUE!
   source 2 FFI:READABLE!
   SET-CALL C-INT ;


TRUSTED: READ-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-READ @ ffi-call-bounded ;


: READ-RAW ( handle ptr u8 CAD-NUM:byte-len -- n )
   {: handle:handle bytes capacity:CAD-NUM:byte-len :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE!
   bytes capacity BLEN>N 1 FFI:WRITABLE! capacity BLEN>N 2 FFI:VALUE!
   READ-CALL ;


TRUSTED: WRITE-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-WRITE @ ffi-call-bounded ;


: WRITE-RAW ( handle ptr u8 CAD-NUM:byte-len -- n )
   {: handle:handle bytes size:CAD-NUM:byte-len :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE!
   bytes 1 FFI:READABLE! size BLEN>N 2 FFI:VALUE!
   WRITE-CALL ;


TRUSTED: POLL-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-POLL @ ffi-call-bounded ;


: POLL-RAW ( ms -- n ) {: timeout:ms :}
   FFI:RESET POLLFD $08 0 FFI:WRITABLE! 1 1 FFI:VALUE!
   timeout MS>N 2 FFI:VALUE!
   POLL-CALL C-INT ;


TRUSTED: CLOSE-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 1 FN-CLOSE @ ffi-call-bounded ;


: CLOSE-RAW ( handle -- n ) {: handle:handle :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE!
   CLOSE-CALL C-INT ;


: PATH-OPEN ( ptr u8 n -- n ) {: text size:n :}
   size 1 $FFF RANGE
   size 0 do text i + c@ 0= if E-OPERAND throw then loop
   size 1 + MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: path extent:CAD-NUM:alloc-byte-len :}
   text size path FFI:CSTR path OPEN-RAW
   dup 0 < if drop LAST-ERROR ERRNO>N negate then
   path extent MEM:RELEASE-BYTES ;


: RAW! ( baud -- ) {: baud:baud :}
   \ Preserve HUPCL; all other framing and flow settings are explicit.
   TERM-BYTES 0 do SAVED i + c@ TERM i + c! loop
   0 TERM LE32! 0 TERM $04 + LE32! 0 TERM $0C + LE32!
   SAVED $08 + LE32@ $400 and RAW-CONTROL or TERM $08 + LE32!
   0 TERM $16 + c! 1 TERM $17 + c!       \ VTIME=0, VMIN=1.
   baud BAUD>N TERM $24 + LE32! baud BAUD>N TERM $28 + LE32! ;


: RAW? ( baud -- bool ) {: baud:baud :}
   TERM LE32@ 0= TERM $04 + LE32@ 0= and TERM $0C + LE32@ 0= and
   TERM $08 + LE32@ BAUD-MASK invert and $400 invert and $8B0 = and
   TERM $10 + c@ 0= and
   TERM $16 + c@ 0= and TERM $17 + c@ 1 = and
   TERM $24 + LE32@ baud BAUD>N = and TERM $28 + LE32@ baud BAUD>N = and ;


: CONFIGURE ( handle baud -- open-result ) {: handle:handle baud:baud :}
   handle SAVED GET-RAW 0 < if LAST-ERROR SERIAL-OPEN--RESULT:failed exit then
   SAVED $10 + c@ 0 <> if SERIAL-OPEN--RESULT:unsupported exit then
   baud RAW!
   handle TERM SET-RAW 0 < if
      LAST-ERROR handle SAVED SET-RAW drop SERIAL-OPEN--RESULT:failed exit
   then
   handle TERM GET-RAW 0 < if
      LAST-ERROR handle SAVED SET-RAW drop SERIAL-OPEN--RESULT:failed exit
   then
   baud RAW? if handle SERIAL-OPEN--RESULT:opened
   else handle SAVED SET-RAW drop SERIAL-OPEN--RESULT:unsupported then ;


: DEADLINE ( ms -- ns )
   MS>N dup 0 $7FFFFFFF RANGE NS-PER-MS * mono-ns + >NS ;


: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: RETRY? ( errno -- bool ) ERRNO>N dup 4 = swap 11 = or ;


\ Positive poll event bits, zero timeout, or negative errno. EINTR keeps the
\ original deadline. Each operation can always make one immediate poll.
: AWAIT ( handle n ns -- n ) {: handle:handle events:n deadline:ns :}
   handle HANDLE>N POLLFD LE32! events POLLFD $04 + LE32!
   begin
      deadline REMAINING POLL-RAW dup 0 >= if
         0= if 0 exit then
         POLLFD $04 + LE32@ 16 rshift dup $20 and 0 <> if drop -9 then exit
      then drop
      LAST-ERROR dup ERRNO>N 4 <> if ERRNO>N negate exit then drop
      deadline REMAINING MS>N 0= if 0 exit then
   again ;


: TRANSFERRED ( n CAD-NUM:byte-len -- io-result ) {: actual:n capacity:CAD-NUM:byte-len :}
   actual 0 <= actual capacity BLEN>N > or if E-RESULT throw then
   actual LENGTH SERIAL-IO--RESULT:transferred ;


: CHECK-IO ( handle CAD-NUM:byte-len -- )
   BLEN>N 1 $7FFFF000 RANGE CHECK-HANDLE INIT ;


public

: BAUD ( n -- baud ) dup 1 $FFFFFFFF RANGE >BAUD ;
: BYTES ( n -- CAD-NUM:byte-len ) dup 1 $7FFFF000 RANGE LENGTH ;


\ Opens a caller-owned nonblocking raw 8N1 stream, no flow control. Numeric
\ speed is verified after configuration. The caller uses CLOSE exactly once.
: OPEN8N1 ( ptr u8 n baud -- open-result ) {: path size:n baud:baud :}
   baud BAUD>N 1 $FFFFFFFF RANGE INIT
   path size PATH-OPEN dup 0 < if negate >ERRNO SERIAL-OPEN--RESULT:failed exit then
   >HANDLE {: handle:handle :}
   handle baud CONFIGURE MATCH open-result
      opened OF SERIAL-OPEN--RESULT:opened ENDOF
      failed OF handle CLOSE-RAW drop SERIAL-OPEN--RESULT:failed ENDOF
      unsupported OF handle CLOSE-RAW drop SERIAL-OPEN--RESULT:unsupported ENDOF
   ;MATCH ;


\ One available chunk, up to capacity. The caller supplies writable storage.
\ Timeout is 0..INT32_MAX milliseconds; zero never waits for future readiness.
: READ ( handle ptr u8 CAD-NUM:byte-len ms -- io-result )
   {: handle:handle bytes capacity:CAD-NUM:byte-len timeout:ms :}
   handle capacity CHECK-IO timeout DEADLINE {: deadline:ns :}
   begin
      handle 1 deadline AWAIT {: events:n :}
      events 0 < if events negate >ERRNO SERIAL-IO--RESULT:failed exit then
      events 0= if SERIAL-IO--RESULT:timeout exit then
      handle bytes capacity READ-RAW dup 0 > if capacity TRANSFERRED exit then
      0= if SERIAL-IO--RESULT:closed exit then
      LAST-ERROR dup RETRY? 0= if SERIAL-IO--RESULT:failed exit then drop
      events $18 and 0 <> if SERIAL-IO--RESULT:closed exit then
      deadline REMAINING MS>N 0= if SERIAL-IO--RESULT:timeout exit then
   again ;


\ Accepts one chunk into the kernel queue; a short write is visible to callers.
\ Success does not prove bytes reached the device. Input is borrowed this call.
: WRITE ( handle ptr u8 CAD-NUM:byte-len ms -- io-result )
   {: handle:handle bytes size:CAD-NUM:byte-len timeout:ms :}
   handle size CHECK-IO timeout DEADLINE {: deadline:ns :}
   begin
      handle 4 deadline AWAIT {: events:n :}
      events 0 < if events negate >ERRNO SERIAL-IO--RESULT:failed exit then
      events 0= if SERIAL-IO--RESULT:timeout exit then
      handle bytes size WRITE-RAW dup 0 > if size TRANSFERRED exit then
      0= if SERIAL-IO--RESULT:closed exit then
      LAST-ERROR dup RETRY? 0= if SERIAL-IO--RESULT:failed exit then drop
      events $18 and 0 <> if SERIAL-IO--RESULT:closed exit then
      deadline REMAINING MS>N 0= if SERIAL-IO--RESULT:timeout exit then
   again ;


\ Linux consumes the fd even on EINTR. Do not retry CLOSE or reuse the handle.
: CLOSE ( handle -- status )
   dup CHECK-HANDLE INIT CLOSE-RAW 0 < if LAST-ERROR SERIAL-STATUS:failed else SERIAL-STATUS:ok then ;


;package
