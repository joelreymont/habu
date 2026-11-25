\ fs-statx.f - an open descriptor's birth time, subvolume and permissions, by
\ statx(2) on the descriptor itself (AT_EMPTY_PATH), package FS-STATX. Birth
\ and subvolume carry whether stx_mask reports the field; a field the
\ filesystem does not keep answers false, its value the kernel's dummy. A
\ failing statx is logged with its text and throws E-FS-STAT with FS:ERRNO its
\ errno; the descriptor stays open.
\ STORAGE CLASS. TASK-LOCAL: the statx buffer, a TASK:+USERS row.
\ PROCESS-WIDE: the empty path, never written.
HB-TARGET-LINUX? ?TARGET

require src/lib/error.f
require src/lib/fs.f
require src/lib/ffi-abi.f
require src/lib/le.f
require src/lib/task.f
require src/lib/log.f

package FS-STATX
private

\ struct statx, include/uapi/linux/stat.h, one layout on every arch.
256 constant STATX-BYTES
$50 constant BTIME-SEC-OFF
$58 constant BTIME-NS-OFF
$A0 constant SUBVOL-OFF
$08 constant ATTR-OFF
$14 constant UID-OFF
$1C constant MODE-OFF
$38 constant ATTR-MASK-OFF
$800 constant STATX-BTIME
$8000 constant STATX-SUBVOL
$B constant STATX-TYPE-MODE-UID
public
$10 constant IMMUTABLE
$20 constant APPEND
private
$1000 constant AT-EMPTY-PATH

STATX-BYTES TASK:+USERS STATX-ROW u8
1 BUFFER: EMPTY-PATH

PROCESS-SYMBOLS
FUNCTION: STATX-CALL statx ( n ^u8 n n ^u8 -- i32 ) ;FUNCTION

: BUF ( -- ^u8 ) 0 STATX-ROW ;

: FAIL ( fd -- ) {: file:fd :}
   FFI:ERRNO {: e:n :}
   LOG:ERROR s" fs-statx" LOG:LINE
   s" statx of fd " LOG:TEXT+ file FD>N LOG:INT+
   s" : " LOG:TEXT+ e FS:ERRNO$ LOG:TEXT+ LOG:;LINE
   e FS:CAUSE!
   E-FS-STAT throw ;

\ statx asking for the mask's fields; whether stx_mask reports any of them.
: STATX ( fd n -- bool ) {: file:fd mask:n :}
   file FD>N EMPTY-PATH AT-EMPTY-PATH mask BUF STATX-CALL
   0<> if file FAIL then
   BUF LE:U32@ mask and 0<> ;

public
\ Birth time, seconds then nanoseconds since the epoch.
: BIRTH ( fd -- n n bool )
   STATX-BTIME STATX {: ok:bool :}
   BUF BTIME-SEC-OFF + LE:U64@  BUF BTIME-NS-OFF + LE:U32@  ok ;

: SUBVOLUME ( fd -- n bool )
   STATX-SUBVOL STATX {: ok:bool :}
   BUF SUBVOL-OFF + LE:U64@  ok ;

\ stx_mode with its type bits, stx_uid, and the attributes the filesystem
\ reports: stx_attributes masked by stx_attributes_mask.
: PERMISSIONS ( fd -- n n n )
   STATX-TYPE-MODE-UID STATX drop
   BUF MODE-OFF + LE:U16@  BUF UID-OFF + LE:U32@
   BUF ATTR-OFF + LE:U64@  BUF ATTR-MASK-OFF + LE:U64@ and ;

;package
