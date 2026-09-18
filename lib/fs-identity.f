\ File identity follows symlinks and preserves stat failures other than ENOENT.
require lib/fs.f
require lib/memory.f
require lib/ffi-abi.f
require lib/span.f

package FS
using MEM
using FFI

private
2 constant NO-ENTRY
8 constant INODE-OFFSET
256 constant STAT-BYTES
STAT-BYTES FS-PATHZ-CAP + constant WORK-BYTES

: SYMBOL ( ptr u8 -- n )
   HB-TARGET-MACOS? if -2 else 0 then swap DLSYM
   dup 0= if E-FS-STAT throw then ;


: STAT-SYMBOL ( -- n ) s\" stat\z" drop SYMBOL ;


: ERRNO-SYMBOL ( -- n )
   HB-TARGET-MACOS? if s\" __error\z" else s\" __errno_location\z" then drop SYMBOL ;


: UNSIGNED-INT ( ptr u8 -- n )
   dup FS-U16@ swap 2 + FS-U16@ 16 lshift or ;


\ Private fixed libc signatures. Symbols are resolved afresh per invocation;
\ no process-owned callable or errno pointer survives application capture.
\ Retirement owner: Habu's checked fixed-schema foreign calls.
TRUSTED: STAT-CALL ( n -- n ) >r ARGS REG-LENS 2 r> ffi-call-bounded ;
TRUSTED: ERRNO-CALL ( n -- ptr u8 ) >r ARGS REG-LENS 0 r> ffi-call-bounded ;


: CHECK-PATH ( ptr u8 n -- )
   {: text bytes:n :}
   bytes 0 <= bytes FS-PATH-CAP > or if E-FS-PATH throw then
   bytes 0 ?do text i + c@ 0= if unloop E-FS-PATH throw then loop ;


: STAT-EXISTS? ( ptr u8 SPAN:span<u8> n n -- bool )
   {: path output stat-fn:n errno-fn:n :}
   RESET path 0 READABLE! output SPAN:$ 1 WRITABLE!
   stat-fn STAT-CALL $FFFFFFFF and 0= if FS-TRUE exit then
   RESET errno-fn ERRNO-CALL UNSIGNED-INT NO-ENTRY = if FS-FALSE exit then
   E-FS-STAT throw ;


\ The work buffer is one span split in two: the stat block the foreign call
\ writes and the NUL-padded path behind it. Both halves are narrowings, so the
\ split cannot address past what SAMEFILE allocated.
: IDENTITY ( ptr u8 n SPAN:span<u8> n n -- n n bool )
   {: text bytes:n work stat-fn:n errno-fn:n :}
   text bytes CHECK-PATH
   text bytes  work STAT-BYTES FS-PATHZ-CAP SPAN:SUB  FS-PATHZ-INTO
   work 0 STAT-BYTES SPAN:SUB stat-fn errno-fn STAT-EXISTS? if
      HB-TARGET-MACOS? if work 0 SPAN:AT UNSIGNED-INT else work 0 SPAN:AT FS-U64@ then
      work INODE-OFFSET SPAN:AT FS-U64@ FS-TRUE
   else 0 0 FS-FALSE then ;


: COMPARE-IDENTITIES ( ptr u8 n ptr u8 n ptr u8 NUM:alloc-byte-len -- bool )
   drop {: left left-bytes:n right right-bytes:n buffer :}
   STAT-SYMBOL ERRNO-SYMBOL {: stat-fn:n errno-fn:n :}
   left left-bytes  buffer WORK-BYTES SPAN:MAKE  stat-fn errno-fn IDENTITY {: left-device:n left-inode:n left-exists:bool :}
   right right-bytes  buffer WORK-BYTES SPAN:MAKE  stat-fn errno-fn IDENTITY {: right-device:n right-inode:n right-exists:bool :}
   left-exists right-exists and left-device right-device = and left-inode right-inode = and ;

public
\ WITH-BYTES hands the quotation exactly the WORK-BYTES mapping it allocated
\ here, which is the reach COMPARE-IDENTITIES mints over.
: SAMEFILE ( ptr u8 n ptr u8 n -- bool )
   WORK-BYTES BYTES-ALLOC-LEN [: COMPARE-IDENTITIES ;] WITH-BYTES ;

;using
;using
;package
