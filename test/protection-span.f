\ protection-span.f - span-aware compiler-state protection regressions.

require src/lib/error.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package PROTECTION-TEST

$800 constant CAP
10000 constant TIMEOUT-MS
ENGINE-ERROR:SEAL-VIOLATION constant VIOLATION-RC
67 constant UNCAUGHT-RC
76 constant CODE-FULL-RC

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( ptr u8 n -- len len PROCESS:ending ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc want T-ENDING-EXITED= ;

: REJECTS ( ptr u8 n -- )
   VIOLATION-RC EXPECT ;

: ACCEPTS ( ptr u8 n -- )
   0 EXPECT ;

\ Each row reaches PROT-GUARD (src/habu/habu1.f) in its own child; a raw value
\ the checker refuses reaches it from the body of a TRUSTED: word (PST-RAW).
: TEST-SCALAR ( -- )
   s" 0 data-base TXN-STATE-OFF 1 + + c!" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + !" REJECTS
   s" 1 data-base TXN-STATE-OFF 1- + +!" REJECTS
   s" 0 data-base TXN-STATE-OFF 1- + atomic!" REJECTS
   s" TRUSTED: PST-RAW ( -- ) 0 data-base TXN-STATE-OFF 1- + patch32 ; PST-RAW" REJECTS
   s" TRUSTED: PST-RAW ( -- ) 0 -4 ! ; PST-RAW" REJECTS ;

\ Exercise compiled source stores, where the guard belongs to the
\ lowered operation rather than the interpreted store primitive.
: TEST-NATIVE-STORE ( -- )
   s" TRUSTED: PST-P ( -- ptr n ) data-base TXN-STATE-OFF 1- + ; : PST-S ( -- n ) 0 PST-P ! 1 ; PST-S drop" REJECTS
   s" TRUSTED: PST-P ( -- ptr u8 ) data-base TXN-STATE-OFF 1+ + byte-view ; : PST-S ( -- n ) 0 PST-P c! 1 ; PST-S drop" REJECTS
   s" TRUSTED: PST-P ( -- ptr n ) -4 ; : PST-S ( -- n ) 0 PST-P ! 1 ; PST-S drop" REJECTS
   s" variable PST-C : PST-S ( -- n ) 19 PST-C ! PST-C @ ; : PST-CHECK ( -- ) PST-S 19 <> if 1 throw then ; PST-CHECK" ACCEPTS ;

\ At the code ceiling `create` and `:` refuse before they write (src/habu/interpreter.f
\ DEF-CODE-ROOM); a constant's body write reaches the guard.
: TEST-CODE ( -- )
   s" TRUSTED: PST-RAW ( -- ) data-base TXN-STATE-OFF 1- + cp! ; PST-RAW" REJECTS
   s" dbase@ DICT-SIZE + cp!" ACCEPTS
   s" dbase@ REGION + $4000 - 4 - cp!" ACCEPTS
   s" dbase@ REGION + 4 - cp!" ACCEPTS
   s" dbase@ REGION + 4 - cp! create PST-END" CODE-FULL-RC EXPECT
   s" dbase@ REGION + 4 - cp! 1 constant PST-END" REJECTS
   s" dbase@ REGION + 4 - cp! : PST-END ( -- n ) 1 ;" CODE-FULL-RC EXPECT
   s" TRUSTED: PST-RAW ( -- ) data-base TXN-STATE-OFF 1 cells - + cp! ; PST-RAW : PST-CROSS ( -- n ) 1 ;" REJECTS ;

: TEST-SYSCALL ( -- )
   s" 0 data-base TXN-STATE-OFF 1 cells - + 16 read drop" REJECTS
   s" TRUSTED: PST-RAW ( -- ) 0 data-base TXN-STATE-OFF 1 cells - + 16 readlink drop ; PST-RAW" REJECTS
   s" TRUSTED: PST-RAW ( -- ) 0 data-base TXN-STATE-OFF 1- + stat64 drop ; PST-RAW" REJECTS
   s" TRUSTED: PST-RAW ( -- ) 0 data-base TXN-STATE-OFF 1- + lstat64 drop ; PST-RAW" REJECTS
   s" 0 data-base TXN-STATE-OFF 1 cells - + 16 data-base TXN-STATE-OFF $100 - + getdirentries64 drop" REJECTS
   s" 0 data-base TXN-STATE-OFF $100 - + 8 data-base TXN-STATE-OFF 1- + getdirentries64 drop" REJECTS
   s" data-base TXN-STATE-OFF 1 cells - + 2 0 poll drop" REJECTS
   s" TRUSTED: PST-RAW ( -- ) data-base TXN-STATE-OFF 1 cells - + 16 3 $1012 -1 0 mmap drop ; PST-RAW" REJECTS
   s" : PST-OUT ( -- n ) HB-TARGET-MACOS? if $40487413 else $5401 then ; 0 PST-OUT data-base TXN-STATE-OFF 1 cells - + ioctl drop" REJECTS
   s" : PST-OUT ( -- n ) HB-TARGET-MACOS? if $40487413 else $5401 then ; TRUSTED: PST-RAW ( -- ) 0 PST-OUT -4 ioctl drop ; PST-RAW" REJECTS
   s" : PST-IN ( -- n ) HB-TARGET-MACOS? if $80487414 else $5402 then ; TRUSTED: PST-RAW ( -- ) 0 PST-IN -1 ioctl drop ; PST-RAW" ACCEPTS ;

: TEST-FFI ( -- )
   s" require src/lib/ffi-abi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-WRITE ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:WRITABLE! 16 1 FFI:VALUE! FFI:ARGS FFI:REG-LENS 2 PST-RET ffi-call-bounded ; data-base TXN-STATE-OFF 1 cells - + PST-WRITE drop" REJECTS
   s" require src/lib/ffi-abi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-X8 ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 FFI:X8-WRITABLE! FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 PST-RET ffi-call-abi-bounded ; data-base TXN-STATE-OFF 1 cells - + PST-X8 drop" REJECTS
   s" require src/lib/ffi-abi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-STACK ( ptr a -- n ) {: p:ptr :} FFI:RESET p 16 0 FFI:STACK-WRITABLE! FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 1 PST-RET ffi-call-abi-bounded ; data-base TXN-STATE-OFF 1 cells - + PST-STACK drop" REJECTS
   s" require src/lib/ffi-abi.f TRUSTED: PST-RET ( -- n ) cp@ {: fn:n :} $D65F03C0 fn patch32 fn ; TRUSTED: PST-VALUE ( n -- n ) {: v:n :} FFI:RESET v 0 FFI:VALUE! FFI:ARGS FFI:REG-LENS 1 PST-RET ffi-call-bounded ; -1 PST-VALUE drop" ACCEPTS ;

\ USER-BAND is one declared run of the per-task header. The transaction band
\ below it rejects a store ($5000 its first cell, $52F8 its last) and
\ USER-BAND:START, the byte after, accepts one: the guard proved from both sides.
: TEST-TASK-USER ( -- )
   s" 0 data-base TXN-STATE-OFF + !" REJECTS
   s" 0 data-base TXN-STATE-OFF TXN-STATE-LEN + 1 cells - + !" REJECTS
   s" 0 data-base USER-BAND:START + !" ACCEPTS
   s" 0 data-base USER-BAND:END 1 cells - + !" ACCEPTS
   s" require src/lib/task.f TASK:#USER -1 TASK:+USER PST-WRAP drop" UNCAUGHT-RC EXPECT ;

\ Each band's first or last cell, or a store straddling its edge, rejects; a
\ cell outside every band accepts.
: TEST-BOUNDARY ( -- )
   s" 0 data-base $800 + c!" REJECTS
   s" 0 data-base $7FF + !" REJECTS
   s" data-base $7F8 + dup @ swap !" ACCEPTS
   s" 0 data-base $2742 + c!" ACCEPTS
   s" 0 data-base TXN-STATE-OFF 1 cells - + !" ACCEPTS
   s" 0 data-base TXN-STATE-OFF TXN-STATE-LEN + 1- + !" REJECTS
   s" 0 data-base PD-TABLE-OFF + c!" ACCEPTS
   s" 0 data-base PD-TABLE-END 1 cells - + !" ACCEPTS
   s" 0 data-base REPLAY-SCOPE:LATCH 1 cells - + !" ACCEPTS
   s" 0 data-base REPLAY-SCOPE:LATCH 1- + !" REJECTS
   s" 0 data-base REPLAY-SCOPE:LATCH + c!" REJECTS
   s" 0 data-base REPLAY-SCOPE:END 1 cells - + !" REJECTS
   s" 0 data-base REPLAY-SCOPE:END 1- + !" REJECTS
   s" 0 data-base SOURCE-EXEC-CTX-CELL + !" REJECTS
   s" 0 data-base SOURCE-EXEC-CTX-CELL 1 cells + 1- + !" REJECTS
   s" 0 data-base ENCLOSING:END 1 cells - + !" REJECTS
   s" 0 data-base ENCLOSING:END 1- + !" REJECTS
   s" 0 data-base REPLAY-SCOPE:ENCLOSING ENCLOSING:END ENCLOSING:PUB - + 1 cells - + !" REJECTS
   s" 0 data-base REPLAY-SCOPE:ENCLOSING ENCLOSING:END ENCLOSING:PUB - + 1- + !" REJECTS
   s" 0 data-base REPLAY-SCOPE:ENCLOSING ENCLOSING:END ENCLOSING:PUB - + + !" ACCEPTS
   s" 0 data-base DATA-START 1 cells - + !" ACCEPTS
   s" 0 data-base DATA-START 1- + !" ACCEPTS
   s" 0 data-base DATA-START + c!" ACCEPTS ;

\ The span guard runs a bounding test (src/habu/habu1.f GUARD-SPAN)
\ in front of the band walk and skips the walk whole when a span provably
\ misses every band, so the hull's own edges have to answer exactly as the
\ bands do. BAND-LO is FRIEND-ARENA's base and BAND-HI is the end of the
\ transaction band; the rows below stand one byte and one cell either side of
\ each (TEST-TASK-USER and TEST-BOUNDARY hold BAND-HI's cell rows), and the
\ last two are the spans a bounding test could wrongly admit: one entirely
\ under the hull, one straddling all of it.
: TEST-HULL-EDGE ( -- )
   s" 0 data-base FRIEND-ARENA 1 cells - + !" ACCEPTS
   s" 0 data-base FRIEND-ARENA 1- + !" REJECTS
   s" 0 data-base FRIEND-ARENA + c!" REJECTS
   s" 0 data-base FRIEND-ARENA 1+ + c!" REJECTS
   s" 0 data-base TXN-STATE-OFF TXN-STATE-LEN + 1- + c!" REJECTS
   s" 0 data-base TXN-STATE-OFF TXN-STATE-LEN + + c!" ACCEPTS
   s" 0 data-base 16 read drop" ACCEPTS
   s" 0 data-base $10 + DATA-START $100 + read drop" REJECTS ;

\ An ordinary definition whose body text passes 20000 bytes: PST-LONG calls a
\ word with a 59-byte name 350 times, 60 bytes of text each with its separator.
\ The child's check spells the count.
$6000 constant LONG-CAP
create LONG-SRC LONG-CAP allot
variable LONG-U
: LONG+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ LONG-SRC LONG-U @ + i + c! loop
   LONG-U @ u + LONG-U ! ;
: STEP$ ( -- ptr u8 n )
   s" PST-ADD-ONE-TO-THE-COUNT-OF-STEPS-IN-A-LONG-DEFINITION-BODY" ;
: LONG$ ( -- ptr u8 n )
   0 LONG-U !
   s" : " LONG+  STEP$ LONG+  s"  ( n -- n ) 1 + ; : PST-LONG ( -- n ) 0" LONG+
   350 0 ?do s"  " LONG+  STEP$ LONG+ loop
   s"  ; : PST-RUN ( -- ) PST-LONG 350 <> if 1 throw then ; PST-RUN" LONG+
   LONG-SRC LONG-U @ ;

\ The body text grows with the definition, and the guard follows it: the store
\ an immediate word makes into the text captured so far, at the address the
\ handle holds, is rejected, and a definition past 20000 bytes of text runs.
: TEST-BODY-TEXT ( -- )
   S\" TRUSTED: PST-POKE ( -- ) 0 data-base BODY-TEXT:BASE-CELL + 0 ptr-field @ c! ; immediate s\" PST-POKE\" 0 parse-imm : PST-HOST ( -- ) PST-POKE ;" REJECTS
   LONG$ ACCEPTS ;

public

: RUN ( -- )
   T-RESET
   TEST-SCALAR
   TEST-NATIVE-STORE
   TEST-CODE
   TEST-SYSCALL
   TEST-FFI
   TEST-TASK-USER
   TEST-BOUNDARY
   TEST-HULL-EDGE
   TEST-BODY-TEXT
   T-REPORT
   s" protection-span: ok" type cr ;

;package

PROTECTION-TEST:RUN
