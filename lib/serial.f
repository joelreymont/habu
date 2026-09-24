\ Raw serial byte streams on Linux and macOS, through bounded libc bindings.
\
\ STORAGE CLASS. TASK-LOCAL for everything a call runs through: the termios
\ struct and the saved termios are one task-local row, so each task drives
\ its own port, and the byte spans READ and WRITE take are caller-owned. The
\ resolved libc symbols and their one-time registration flags are PROCESS-WIDE,
\ as symbol resolution should be.
\ See docs/threads.md.
\
\ The wait is the AIO loop's (docs/aio.md): every READ and WRITE readiness
\ question is one POLL for the time left on its deadline and one AWAIT, so
\ no thread parks in poll(2) and a signal no longer cuts the wait short. A
\ program calls AIO:START before its first READ or WRITE; a wait with no
\ loop running is E-AIO-STATE.
require lib/errors.f
require lib/ffi-abi.f
require lib/le.f
require lib/type/deftype.f
require lib/num-types.f
require lib/task.f
require lib/image-lifecycle.f
require lib/memory.f
require lib/aio.f

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
   VARIANT transferred NUM:byte-len ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT closed ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

E-SERIAL-OPERAND constant E-OPERAND
E-SERIAL-PLATFORM constant E-PLATFORM
E-SERIAL-SYMBOL constant E-SYMBOL
E-SERIAL-RESULT constant E-RESULT

private

: TERM-BYTES ( -- n ) HB-TARGET-MACOS? if 72 else $2C then ;
: OPEN-FLAGS ( -- n ) HB-TARGET-MACOS? if $1020006 else $80902 then ; \ O_RDWR | O_NONBLOCK | O_NOCTTY | O_CLOEXEC.
: GET-TERM ( -- n ) HB-TARGET-MACOS? if $40487413 else $802C542A then ; \ TIOCGETA / TCGETS2.
: SET-TERM ( -- n ) HB-TARGET-MACOS? if $80487414 else $402C542B then ; \ TIOCSETA / TCSETS2, no queue flush.
$100F100F constant BAUD-MASK
$100018B0 constant RAW-CONTROL   \ BOTHER both ways, CS8 | CREAD | CLOCAL.
1000000 constant NS-PER-MS

variable FN-OPEN
variable FN-IOCTL
variable FN-READ
variable FN-WRITE
variable FN-CLOSE
variable FN-ERRNO
here FFI:>CELL 7 and 8 swap - 7 and allot
variable READY
variable REGISTERED
create SYMBOL-NAME $20 allot

\ No per-operation buffer is process-global; independent tasks may use ports.
\ Two records: Darwin's 72-byte termios or Linux's 44-byte termios2.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 144 TASK:+USER IO-STORAGE drop
: TERM ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: SAVED ( -- ptr u8 ) IO-STORAGE BYTE-VIEW 72 + ;

CAST: BLEN>N ( NUM:byte-len -- n )


: RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LENGTH ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH NUM:numeric-result
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


\ Image capture is quiescent. These are borrowed process addresses; clearing
\ them needs no foreign call. Caller-owned descriptors must already be closed.
: RESET-SYMBOLS ( -- )
   0 FN-OPEN ! 0 FN-IOCTL ! 0 FN-READ !
   0 FN-WRITE ! 0 FN-CLOSE !
   0 FN-ERRNO !
   0 REGISTERED ! 0 READY atomic! ;


\ INIT holds this module's READY lock. The shared registry must support
\ concurrent registration by different resource owners.
: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: RESET-SYMBOLS ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;


\ RTLD_DEFAULT borrows process symbols; the native executable already needs
\ libc on either target. No library reference is acquired or retained by this module.
: SYMBOL ( ptr u8 n -- n )
   SYMBOL-NAME FFI:CSTR HB-TARGET-MACOS? if -2 else 0 then SYMBOL-NAME FFI:DLSYM
   dup 0= if E-SYMBOL throw then ;


: LOAD-SYMBOLS ( -- )
   s" open" SYMBOL FN-OPEN ! s" ioctl" SYMBOL FN-IOCTL !
   s" read" SYMBOL FN-READ ! s" write" SYMBOL FN-WRITE !
   s" close" SYMBOL FN-CLOSE !
   HB-TARGET-MACOS? if s" __error" else s" __errno_location" then SYMBOL FN-ERRNO ! ;


: INIT ( -- )
   begin
      READY atomic@ 2 = if exit then
      0 1 READY atomic-cas 0= if
         [: REGISTER-CLEANUP LOAD-SYMBOLS ;] catch dup 0 <> if 0 READY atomic! throw then drop
         2 READY atomic! exit
      then TASK:PAUSE
   again ;


\ libc calls use separate pointer directions and target-specific termios
\ extents. Darwin ioctl passes its variadic pointer on the stack.
\ errno is a libc-owned, thread-local C int. The wait is not among them: it is
\ AIO's. Retirement owner: checked foreign bindings.
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
   HB-TARGET-MACOS? if
      FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
      1 FN-IOCTL @ ffi-call-abi-bounded
   else FFI:ARGS FFI:REG-LENS 3 FN-IOCTL @ ffi-call-bounded then ;


: GET-RAW ( handle ptr u8 -- n ) {: handle:handle target :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE! GET-TERM 1 FFI:VALUE!
   HB-TARGET-MACOS? if target TERM-BYTES 0 FFI:STACK-WRITABLE!
   else target TERM-BYTES 2 FFI:WRITABLE! then
   GET-CALL C-INT ;


TRUSTED: SET-CALL ( -- n )
   GET-CALL ;


: SET-RAW ( handle ptr u8 -- n ) {: handle:handle source :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE! SET-TERM 1 FFI:VALUE!
   HB-TARGET-MACOS? if source 0 FFI:STACK-READABLE!
   else source 2 FFI:READABLE! then
   SET-CALL C-INT ;


TRUSTED: READ-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-READ @ ffi-call-bounded ;


: READ-RAW ( handle ptr u8 NUM:byte-len -- n )
   {: handle:handle bytes capacity:NUM:byte-len :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE!
   bytes capacity BLEN>N 1 FFI:WRITABLE! capacity BLEN>N 2 FFI:VALUE!
   READ-CALL ;


TRUSTED: WRITE-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 3 FN-WRITE @ ffi-call-bounded ;


: WRITE-RAW ( handle ptr u8 NUM:byte-len -- n )
   {: handle:handle bytes size:NUM:byte-len :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE!
   bytes 1 FFI:READABLE! size BLEN>N 2 FFI:VALUE!
   WRITE-CALL ;


TRUSTED: CLOSE-CALL ( -- n )
   FFI:ARGS FFI:REG-LENS 1 FN-CLOSE @ ffi-call-bounded ;


: CLOSE-RAW ( handle -- n ) {: handle:handle :}
   FFI:RESET handle HANDLE>N 0 FFI:VALUE!
   CLOSE-CALL C-INT ;


: PATH-OPEN ( ptr u8 n -- n ) {: text size:n :}
   size 1 $FFF RANGE
   size 0 do text i + c@ 0= if E-OPERAND throw then loop
   size 1 + MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: path extent:NUM:alloc-byte-len :}
   text size path FFI:CSTR path OPEN-RAW
   dup 0 < if drop LAST-ERROR ERRNO>N negate then
   path extent MEM:RELEASE-BYTES ;


: MAC-RAW! ( baud -- ) {: baud:baud :}
   TERM-BYTES 0 do SAVED i + c@ TERM i + c! loop
   0 TERM LE:U64! 0 TERM 8 + LE:U64! 0 TERM 24 + LE:U64!
   SAVED 16 + LE:U64@ $4000 and $8B00 or TERM 16 + LE:U64!
   1 TERM 48 + c! 0 TERM 49 + c!
   baud BAUD>N TERM 56 + LE:U64! baud BAUD>N TERM 64 + LE:U64! ;

: MAC-RAW? ( baud -- bool ) {: baud:baud :}
   TERM LE:U64@ 0= TERM 8 + LE:U64@ 0= and TERM 24 + LE:U64@ 0= and
   TERM 16 + LE:U64@ $4000 invert and $8B00 = and
   TERM 48 + c@ 1 = and TERM 49 + c@ 0= and
   TERM 56 + LE:U64@ baud BAUD>N = and TERM 64 + LE:U64@ baud BAUD>N = and ;

: RAW! ( baud -- ) {: baud:baud :}
   HB-TARGET-MACOS? if baud MAC-RAW! exit then
   \ Preserve HUPCL; all other framing and flow settings are explicit.
   TERM-BYTES 0 do SAVED i + c@ TERM i + c! loop
   0 TERM LE32! 0 TERM $04 + LE32! 0 TERM $0C + LE32!
   SAVED $08 + LE32@ $400 and RAW-CONTROL or TERM $08 + LE32!
   0 TERM $16 + c! 1 TERM $17 + c!       \ VTIME=0, VMIN=1.
   baud BAUD>N TERM $24 + LE32! baud BAUD>N TERM $28 + LE32! ;


: RAW? ( baud -- bool ) {: baud:baud :}
   HB-TARGET-MACOS? if baud MAC-RAW? exit then
   TERM LE32@ 0= TERM $04 + LE32@ 0= and TERM $0C + LE32@ 0= and
   TERM $08 + LE32@ BAUD-MASK invert and $400 invert and $8B0 = and
   TERM $10 + c@ 0= and
   TERM $16 + c@ 0= and TERM $17 + c@ 1 = and
   TERM $24 + LE32@ baud BAUD>N = and TERM $28 + LE32@ baud BAUD>N = and ;


: CONFIGURE ( handle baud -- open-result ) {: handle:handle baud:baud :}
   handle SAVED GET-RAW 0 < if LAST-ERROR SERIAL-OPEN--RESULT:failed exit then
   HB-TARGET-LINUX? if
      SAVED $10 + c@ 0 <> if SERIAL-OPEN--RESULT:unsupported exit then
   then
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


: RETRY? ( errno -- bool ) ERRNO>N dup 4 = swap HB-TARGET-MACOS? if 35 else 11 then = or ;


\ Positive event bits, zero for the deadline, or a negative errno. One POLL
\ on the AIO loop for the time this operation has left, then one AWAIT: the
\ deadline is the poll's own linked timeout, so a signal no longer cuts the wait
\ short and there is nothing to restart. The answered bits keep the POLLNVAL
\ rule, though a descriptor the kernel refuses outright now arrives as EBADF on
\ the refused arm instead of as POLLNVAL on the ready one. The loop must be
\ running: a wait without AIO:START is E-AIO-STATE. `cancelled` cannot
\ arrive here - nothing in this module cancels, the ticket never leaves this
\ word, and the cleanup AIO registers on a submitting task runs only after that
\ task has ended - so it is a broken foreign result, exactly like an impossible
\ termios read-back.
: AWAIT ( handle n ns -- n ) {: handle:handle events:n deadline:ns :}
   handle HANDLE>N >FD events deadline REMAINING AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF dup $20 and 0 <> if drop -9 then ENDOF
      timed-out OF 0 ENDOF
      cancelled OF E-RESULT throw ENDOF
      refused OF negate ENDOF
   ;MATCH ;


: TRANSFERRED ( n NUM:byte-len -- io-result ) {: actual:n capacity:NUM:byte-len :}
   actual 0 <= actual capacity BLEN>N > or if E-RESULT throw then
   actual LENGTH SERIAL-IO--RESULT:transferred ;


: CHECK-IO ( handle NUM:byte-len -- )
   BLEN>N 1 $7FFFF000 RANGE CHECK-HANDLE INIT ;


public

: BAUD ( n -- baud ) dup 1 $FFFFFFFF RANGE >BAUD ;
: BYTES ( n -- NUM:byte-len ) dup 1 $7FFFF000 RANGE LENGTH ;


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
: READ ( handle ptr u8 NUM:byte-len ms -- io-result )
   {: handle:handle bytes capacity:NUM:byte-len timeout:ms :}
   handle capacity CHECK-IO timeout DEADLINE {: deadline:ns :}
   begin
      handle AIO:READABLE deadline AWAIT {: events:n :}
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
: WRITE ( handle ptr u8 NUM:byte-len ms -- io-result )
   {: handle:handle bytes size:NUM:byte-len timeout:ms :}
   handle size CHECK-IO timeout DEADLINE {: deadline:ns :}
   begin
      handle AIO:WRITABLE deadline AWAIT {: events:n :}
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
