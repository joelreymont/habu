\ protection-span.f - span-aware compiler-state protection regressions.

require lib/errors.f
require lib/test.f
require lib/test/outcome.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package PROTECTION-TEST

$800 constant CAP
10000 constant TIMEOUT-MS
83 constant VIOLATION-RC
67 constant UNCAUGHT-RC
76 constant CODE-FULL-RC

create OUT CAP allot
create ERR CAP allot

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: CAPTURE ( ptr u8 n -- len len outcome ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   HB$ >LEN
   src u >LEN
   OUT CAP >LEN
   ERR CAP >LEN
   TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE want T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop ;

: REJECTS ( ptr u8 n -- )
   VIOLATION-RC EXPECT ;

: ACCEPTS ( ptr u8 n -- )
   0 EXPECT ;

: TEST-SCALAR ( -- )
   s" 0 data-base TXN-STATE-OFF 1 + + c!" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + !" REJECTS
   s" 1 data-base TXN-STATE-OFF 1- + +!" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + atomic!" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + patch32" REJECTS
   s" 0 -4 !" REJECTS ;

: TEST-CODE ( -- )
   s" data-base TXN-STATE-OFF 1- + cp!" REJECTS
   s" dbase@ $C1000 + cp!" ACCEPTS
   s" dbase@ $400000 + $4000 - 4 - cp!" ACCEPTS
   s" dbase@ $400000 + 4 - cp!" ACCEPTS
   s" dbase@ $400000 + 4 - cp! create PST-END" REJECTS
   s" dbase@ $400000 + 4 - cp! 1 constant PST-END" REJECTS
   s" dbase@ $400000 + 4 - cp! : PST-END ( -- n ) 1 ;" CODE-FULL-RC EXPECT
   s" data-base TXN-STATE-OFF 1 cells - + cp! : PST-CROSS ( -- n ) 1 ;" REJECTS ;

: TEST-SYSCALL ( -- )
   s" 0 data-base TXN-STATE-OFF 1 cells - + 16 read drop" REJECTS
   s" 0 data-base TXN-STATE-OFF 1 cells - + 16 readlink drop" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + stat64 drop" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + lstat64 drop" REJECTS
   s" 0 data-base TXN-STATE-OFF 1 cells - + 16 data-base TXN-STATE-OFF $100 - + getdirentries64 drop" REJECTS
   s" 0 data-base TXN-STATE-OFF $100 - + 8 data-base TXN-STATE-OFF 1- + getdirentries64 drop" REJECTS
   s" data-base TXN-STATE-OFF 1 cells - + 2 0 poll drop" REJECTS
   s" data-base TXN-STATE-OFF 1 cells - + 16 3 $1012 -1 0 mmap drop" REJECTS
   s" : PST-OUT ( -- n ) HB-TARGET-MACOS? if $40487413 else $5401 then ; 0 PST-OUT data-base TXN-STATE-OFF 1 cells - + ioctl drop" REJECTS
   s" : PST-OUT ( -- n ) HB-TARGET-MACOS? if $40487413 else $5401 then ; 0 PST-OUT -4 ioctl drop" REJECTS
   s" : PST-IN ( -- n ) HB-TARGET-MACOS? if $80487414 else $5402 then ; 0 PST-IN -1 ioctl drop" ACCEPTS ;

: TEST-FFI ( -- )
   s" require lib/ffi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! 16 1 FFI:VALUE! FFI:ARGS FFI:REG-LENS 2 PST-RET ffi-call-bounded ; data-base TXN-STATE-OFF 1 cells - + PST-WRITE drop" REJECTS
   s" require lib/ffi-abi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-X8 ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 FFI:X8-WRITABLE! FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 PST-RET ffi-call-abi-bounded ; data-base TXN-STATE-OFF 1 cells - + PST-X8 drop" REJECTS
   s" require lib/ffi-abi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-STACK ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:STACK-WRITABLE! FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 1 PST-RET ffi-call-abi-bounded ; data-base TXN-STATE-OFF 1 cells - + PST-STACK drop" REJECTS
   s" require lib/ffi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-VALUE ( n -- n ) {: v:n :} FFI:RESET v 0 FFI:VALUE! FFI:ARGS FFI:REG-LENS 1 PST-RET ffi-call-bounded ; -1 PST-VALUE drop" ACCEPTS ;

: TEST-TASK-USER ( -- )
   s" require lib/task.f : PST-EXACT ( n -- ) TXN-STATE-OFF <> if 1 throw then ; TASK:#USER TXN-STATE-OFF over - TASK:+USER PST-EDGE PST-EXACT" ACCEPTS
   s" require lib/task.f TASK:#USER TXN-STATE-OFF over - 1+ TASK:+USER PST-OVER drop" UNCAUGHT-RC EXPECT
   s" require lib/task.f TASK:#USER -1 TASK:+USER PST-WRAP drop" UNCAUGHT-RC EXPECT ;

: TEST-BOUNDARY ( -- )
   s" 0 data-base $800 + c!" REJECTS
   s" 0 data-base $7FF + !" REJECTS
   s" 0 data-base $7F8 + !" ACCEPTS
   s" 0 data-base $2742 + c!" ACCEPTS
   s" 0 data-base TXN-STATE-OFF 1 cells - + !" ACCEPTS
   s" 0 data-base DATA-START 1- + !" REJECTS
   s" 0 data-base DATA-START + c!" ACCEPTS ;

: TEST-SNAP-REBASE ( -- )
   s" snap-rebase whole-band straddle is rejected" T-LABEL
   s" data-base TXN-STATE-OFF 8 - + data-base DATA-START 8 + + 0 0 0 0 snap-rebase" REJECTS ;

public

: RUN ( -- )
   T-RESET
   TEST-SCALAR
   TEST-CODE
   TEST-SYSCALL
   TEST-FFI
   TEST-TASK-USER
   TEST-BOUNDARY
   TEST-SNAP-REBASE
   T-REPORT
   s" protection-span: ok" type cr ;

end-package

PROTECTION-TEST:RUN
