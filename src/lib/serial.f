split-str
\ serial.f - raw serial byte streams on Linux and macOS through bounded libc
\ bindings. OS failures, a bad handle's included, are result variants; a baud
\ out of range or a negative length throws. Calls must not nest within one task;
\ close live descriptors before an image is prepared.
\ STORAGE CLASS. TASK-LOCAL: the termios and saved termios are rows per task,
\ so separate tasks drive separate ports; byte spans are caller-owned.
require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/le.f
require src/lib/type/deftype.f
require src/lib/adt/option.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/memory.f
require src/lib/aio.f

package SERIAL
public

DEFTYPE HANDLE
DEFTYPE BAUD
DEFTYPE ERRNO

SUMTYPE open-result
   VARIANT opened handle ;VARIANT
   VARIANT failed errno ;VARIANT
   VARIANT unsupported ;VARIANT
;SUMTYPE

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE io-result
   VARIANT transferred NUM:byte-len ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT closed ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

E-SERIAL-OPERAND constant E-OPERAND

private

: TERM-BYTES ( -- n ) HB-TARGET-LINUX-KERNEL? if $2C else 72 then ;
: OPEN-FLAGS ( -- n ) HB-TARGET-LINUX-KERNEL? if $80902 else $1020006 then ;   \ O_RDWR | O_NONBLOCK | O_NOCTTY | O_CLOEXEC.
: GET-TERM ( -- n ) HB-TARGET-LINUX-KERNEL? if $802C542A else $40487413 then ;   \ TIOCGETA / TCGETS2.
: SET-TERM ( -- n ) HB-TARGET-LINUX-KERNEL? if $402C542B else $80487414 then ;   \ TIOCSETA / TCSETS2, no queue flush.
$100F100F constant BAUD-MASK
$100018B0 constant RAW-CONTROL   \ BOTHER both ways, CS8 | CREAD | CLOCAL.
1000000 constant NS-PER-MS

\ The port's new and saved settings, raw bytes ioctl fills: Darwin's 72-byte
\ termios or Linux's 44-byte termios2.
72 TASK:+USERS TERM-ROW u8
72 TASK:+USERS SAVED-ROW u8
: TERM ( -- ^u8 ) 0 TERM-ROW ;
: SAVED ( -- ^u8 ) 0 SAVED-ROW ;

CAST: BLEN>N ( NUM:byte-len -- n )
\ Each caller has shown the cell nonnegative.
CAST: N>BLEN ( n -- NUM:byte-len )


: RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LE32! ( n ^u8 -- ) {: value:n target :}
   4 0 do value i 8 * rshift $FF and target i + c! loop ;


: LE32@ ( ^u8 -- n ) {: source :}
   source c@ source $01 + c@ 8 lshift or
   source $02 + c@ 16 lshift or source $03 + c@ 24 lshift or ;


\ libc's calls. Darwin passes ioctl's variadic pointer on the stack.
\ test/serial.py covers these boundaries through real kernel pseudoterminals.
PROCESS-SYMBOLS
FUNCTION: OPEN-CALL open ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: GET-CALL ioctl ( n n ^u8 -- i32 )
   2 VARIADIC
;FUNCTION
FUNCTION: SET-CALL ioctl ( n n ^u8 -- i32 ) 2 VARIADIC ;FUNCTION
FUNCTION: READ-CALL read ( n ^u8 n -- n ) ;FUNCTION
FUNCTION: WRITE-CALL write ( n str -- n ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION


\ errno is libc's thread-local C int.
: LAST-ERROR ( -- errno ) FFI:ERRNO >ERRNO ;


: OPEN-RAW ( ^u8 -- n ) OPEN-FLAGS OPEN-CALL ;


: GET-RAW ( handle ^u8 -- n ) {: handle:handle target :}
   handle HANDLE>N GET-TERM target GET-CALL ;


: SET-RAW ( handle ^u8 -- n ) {: handle:handle source :}
   handle HANDLE>N SET-TERM source SET-CALL ;


: READ-RAW ( handle ^u8 NUM:byte-len -- n )
   {: handle:handle bytes capacity:NUM:byte-len :}
   handle HANDLE>N bytes capacity BLEN>N READ-CALL ;


: WRITE-RAW ( handle str -- n )
   {: handle:handle bytes size:n :}
   handle HANDLE>N bytes size WRITE-CALL ;


: CLOSE-RAW ( handle -- n ) HANDLE>N CLOSE-CALL ;


\ The OS answers a path too long for it; the copy's mapping is released on
\ every exit, the refusal of a 0 byte included.
: PATH-OPEN ( str -- n )
   {: text size:n :}
   text size size FFI:CSTR-ROOM MEM:BYTES-ALLOC-LEN
   [: MEM:ALLOCATION>SPAN FFI:CSTR OPEN-RAW
      dup 0 < if drop LAST-ERROR ERRNO>N negate then ;] MEM:WITH-BYTES ;


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
   handle SAVED GET-RAW 0 < if LAST-ERROR SERIAL-OPEN--RESULT:FAILED exit then
   HB-TARGET-LINUX-KERNEL? if
      SAVED $10 + c@ 0 <> if SERIAL-OPEN--RESULT:UNSUPPORTED exit then
   then
   baud RAW!
   handle TERM SET-RAW 0 < if
      LAST-ERROR handle SAVED SET-RAW drop SERIAL-OPEN--RESULT:FAILED exit
   then
   handle TERM GET-RAW 0 < if
      LAST-ERROR handle SAVED SET-RAW drop SERIAL-OPEN--RESULT:FAILED exit
   then
   baud RAW? if handle SERIAL-OPEN--RESULT:OPENED
   else handle SAVED SET-RAW drop SERIAL-OPEN--RESULT:UNSUPPORTED then ;


\ No deadline is -1 ns, which no mono-ns reading is, and REMAINING answers -1 ms.
\ A wait longer than $7FFFFFFF ms, poll(2)'s widest, waits that long.
: DEADLINE ( ms -- ns )
   MS>N dup 0 < if drop -1 >NS exit then
   $7FFFFFFF min NS-PER-MS * mono-ns + >NS ;


: REMAINING ( ns -- ms )
   NS>N dup 0 < if drop -1 >MS exit then
   mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: RETRY? ( errno -- bool )
   ERRNO>N dup 4 = swap
   HB-TARGET-LINUX-KERNEL? if 11 else 35 then = or ;


\ Positive event bits, zero for the deadline, or a negative errno. One POLL on
\ the AIO loop for the time this operation has left, then one AWAIT: the
\ deadline is the poll's own linked timeout, so a signal cannot cut the wait
\ short. The answered bits keep the POLLNVAL rule; a descriptor the kernel
\ refuses outright arrives as EBADF on the refused arm. A wait without AIO:START
\ is E-AIO-STATE.
: AWAIT ( handle n ns -- n ) {: handle:handle events:n deadline:ns :}
   handle HANDLE>N >FD events deadline REMAINING AIO:WAIT-READY
   MATCH AIO:readiness
      ready OF dup $20 and 0 <> if drop -9 then ENDOF
      timed-out OF 0 ENDOF
      refused OF negate ENDOF
   ;MATCH ;


: TRANSFERRED ( n -- io-result )
   N>BLEN SERIAL-IO--RESULT:TRANSFERRED ;


public

: BAUD ( n -- baud ) dup 1 $FFFFFFFF RANGE >BAUD ;
: BYTES ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH option
      none OF E-OPERAND throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ Opens a caller-owned nonblocking, close-on-exec, O_NOCTTY raw 8N1 stream, no
\ flow control (Linux: N_TTY, numeric termios2 rates; macOS: numeric termios
\ speeds), keeps HUPCL, and reads the settings back: anything short of that is
\ `unsupported`. A failed configure restores the old settings best-effort and
\ closes. The caller uses CLOSE exactly once.
: OPEN8N1 ( str baud -- open-result ) {: path size:n baud:baud :}
   path size PATH-OPEN dup 0 < if negate >ERRNO SERIAL-OPEN--RESULT:FAILED exit then
   >HANDLE {: handle:handle :}
   handle baud CONFIGURE MATCH open-result
      opened OF SERIAL-OPEN--RESULT:OPENED ENDOF
      failed OF handle CLOSE-RAW drop SERIAL-OPEN--RESULT:FAILED ENDOF
      unsupported OF handle CLOSE-RAW drop SERIAL-OPEN--RESULT:UNSUPPORTED ENDOF
   ;MATCH ;


\ One available chunk, up to capacity, possibly short; a zero-byte request
\ transfers nothing at once.
\ Timeout is milliseconds, negative for no deadline; zero never waits for
\ future readiness.
: READ ( handle ^u8 NUM:byte-len ms -- io-result )
   {: handle:handle bytes capacity:NUM:byte-len timeout:ms :}
   capacity BLEN>N 0= if capacity SERIAL-IO--RESULT:TRANSFERRED exit then
   timeout DEADLINE {: deadline:ns :}
   begin
      handle AIO:READABLE deadline AWAIT {: events:n :}
      events 0 < if events negate >ERRNO SERIAL-IO--RESULT:FAILED exit then
      events 0= if SERIAL-IO--RESULT:TIMEOUT exit then
      handle bytes capacity READ-RAW dup 0 > if TRANSFERRED exit then
      0= if SERIAL-IO--RESULT:CLOSED exit then
      LAST-ERROR dup RETRY? 0= if SERIAL-IO--RESULT:FAILED exit then drop
      events $18 and 0 <> if SERIAL-IO--RESULT:CLOSED exit then
      deadline REMAINING MS>N 0= if SERIAL-IO--RESULT:TIMEOUT exit then
   again ;


\ Accepts one chunk into the kernel queue, possibly short; a zero-byte request
\ transfers nothing at once. Success does not prove bytes reached the device. Input is
\ borrowed for this call. Timeout is milliseconds, negative for no deadline.
: WRITE ( handle str ms -- io-result )
   {: handle:handle bytes size:n timeout:ms :}
   size 0= if size TRANSFERRED exit then
   timeout DEADLINE {: deadline:ns :}
   begin
      handle AIO:WRITABLE deadline AWAIT {: events:n :}
      events 0 < if events negate >ERRNO SERIAL-IO--RESULT:FAILED exit then
      events 0= if SERIAL-IO--RESULT:TIMEOUT exit then
      handle bytes size WRITE-RAW dup 0 > if TRANSFERRED exit then
      0= if SERIAL-IO--RESULT:CLOSED exit then
      LAST-ERROR dup RETRY? 0= if SERIAL-IO--RESULT:FAILED exit then drop
      events $18 and 0 <> if SERIAL-IO--RESULT:CLOSED exit then
      deadline REMAINING MS>N 0= if SERIAL-IO--RESULT:TIMEOUT exit then
   again ;


\ Consumes the handle even when it fails (Linux consumes the fd even on EINTR):
\ never retry CLOSE or reuse the handle. The terminal's settings persist.
: CLOSE ( handle -- status )
   CLOSE-RAW 0 < if LAST-ERROR SERIAL-STATUS:FAILED else SERIAL-STATUS:OK then ;


;package
