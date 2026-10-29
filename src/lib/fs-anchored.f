\ fs-anchored.f - descriptor-anchored lookup; a call owns one mapping, two fds.
\ Each component opens relative to the live parent with O_NOFOLLOW (directories
\ also O_DIRECTORY), so a replaced symlink cannot redirect a lookup and an open
\ directory stays the anchor across a rename. Not covered: the anchor being the
\ intended root, cross-root renames, hard links, mounts, content changes.
\ STORAGE CLASS. CALLER-OWNED: OPEN-DIR's descriptors are the caller's.
require src/lib/fs.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/span.f

package FS
private

0 constant A-CUR-CELL
1 constant A-NEXT-CELL
2 constant A-RESULT-CELL
3 cells constant A-PATH-OFF
\ libc struct stat has cell alignment, independently of the byte path's length.
A-PATH-OFF PATHZ-CAP + CELL 1- + CELL negate and constant A-STAT-OFF
A-STAT-OFF STAT-CAP + constant A-PROBE-OFF
A-PROBE-OFF 1 + constant A-WORK-BYTES
4 constant A-INTERRUPTED

PROCESS-SYMBOLS
FUNCTION: A-READ-CALL read ( n ptr u8 n -- n )
   1 2 WRITES-ARG
;FUNCTION
FUNCTION: A-CLOSE-CALL close ( n -- i32 ) ;FUNCTION

: A-SLOT ( ptr u8 n -- ptr n ) {: work:ptr idx:n :}
   work CELL-VIEW idx cells + ;
: A-CUR@ ( ptr u8 -- n ) A-CUR-CELL A-SLOT @ ;
: A-CUR! ( n ptr u8 -- ) A-CUR-CELL A-SLOT ! ;
: A-NEXT@ ( ptr u8 -- n ) A-NEXT-CELL A-SLOT @ ;
: A-NEXT! ( n ptr u8 -- ) A-NEXT-CELL A-SLOT ! ;
: A-PATH-BUF ( ptr u8 -- ptr u8 ) A-PATH-OFF + ;
: A-STAT-BUF ( ptr u8 -- ptr u8 ) A-STAT-OFF + ;
: A-PROBE-BUF ( ptr u8 -- ptr u8 ) A-PROBE-OFF + ;

: A-MAP ( -- ptr u8 NUM:alloc-byte-len )
   A-WORK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: work:ptr len:NUM:alloc-byte-len :}
   -1 work A-CUR! -1 work A-NEXT!
   work len ;

\ close(2) can have retired a descriptor even on failure. Clear its slot first.
: A-CLOSE-SLOT ( ptr u8 n -- n ) {: work:ptr idx:n :}
   work idx A-SLOT @ {: file:n :}
   file 0 < if 0 exit then
   -1 work idx A-SLOT !
   file A-CLOSE-CALL 0= if 0 exit then
   FFI:ERRNO ;
: A-CLEANUP ( ptr u8 -- n ) {: work:ptr :}
   work A-NEXT-CELL A-CLOSE-SLOT {: first:n :}
   work A-CUR-CELL A-CLOSE-SLOT {: second:n :}
   first 0<> if first else second then ;

\ The primary cause was taken before cleanup's close calls replaced libc errno.
: A-FINISH ( ptr u8 NUM:alloc-byte-len n n -- )
   {: work:ptr len:NUM:alloc-byte-len code:n cause:n :}
   work A-CLEANUP {: closing:n :}
   work len MEM:RELEASE-BYTES
   code 0<> if cause CAUSE! code throw then
   closing 0<> if closing CAUSE! E-FS-IO throw then
   0 CAUSE! ;

\ O_RDONLY|O_NOFOLLOW|O_CLOEXEC|O_NONBLOCK|O_NOCTTY, READ-FILE-AT's last
\ component. Darwin: xnu bsd/sys/fcntl.h. Linux arm64: arch/arm64 asm/fcntl.h
\ overrides the generic nofollow bit; x86-64 uses the generic bits.
: A-FILE-FLAGS ( -- n )
   HB-TARGET-MACOS? if $100 $1000000 or $4 or $20000 or exit then
   HB-TARGET-LINUX-X86-64? if $20000 $80000 or $800 or $100 or exit then
   $8000 $80000 or $800 or $100 or ;

: A-SNAPSHOT ( ptr u8 n ptr u8 -- ptr u8 )
   {: src:ptr u:n work:ptr :}
   u 0 <= u PATH-CAP > or if E-FS-PATH throw then
   work A-PATH-BUF {: dst:ptr :}
   src u dst PATHZ-CAP SPAN:MAKE FFI:CSTR ;
: A-COMPONENT-END ( ptr u8 n n -- n ) {: path:ptr u:n at:n :}
   at begin dup u < while
      path over + c@ SLASH = if exit then
      1+
   repeat ;
: A-COMPONENT-OK ( ptr u8 n n -- ) {: path:ptr from:n stop:n :}
   stop from = if E-FS-PATH throw then
   stop from - 1 = if
      path from + c@ DOT = if E-FS-PATH throw then
   then
   stop from - 2 = if
      path from + c@ DOT = path from 1 + + c@ DOT = and
      if E-FS-PATH throw then
   then ;
: A-VALIDATE ( ptr u8 n bool -- n )
   {: path:ptr u:n base:bool :}
   path c@ SLASH = {: absolute:bool :}
   absolute base 0= and if E-FS-PATH throw then
   u 1 = base and if
      absolute if 1 exit then
      path c@ DOT = if 1 exit then
   then
   absolute if 1 else 0 then {: start:n :}
   start begin dup u < while
      {: at:n :}
      path u at A-COMPONENT-END {: stop:n :}
      path at stop A-COMPONENT-OK
      stop u < if stop 1 + else stop then
   repeat
   drop
   path u 1 - + c@ SLASH = if E-FS-PATH throw then
   start ;

: A-OPEN-BASE ( ptr u8 ptr u8 -- ) {: path:ptr work:ptr :}
   path c@ SLASH = if
      AT-CWD S\" /\z" drop OPEN-DIR-FLAGS OPEN-AT-CALL
   else
      AT-CWD S\" .\z" drop OPEN-DIR-FLAGS OPEN-AT-CALL
   then
   dup 0 < if drop E-FS-OPEN ERRNO-FAIL then
   work A-CUR! ;

: A-OPEN-NEXT ( ptr u8 n ptr u8 n -- ) {: work:ptr parent:n component:ptr flags:n :}
   parent component flags OPEN-AT-CALL
   dup 0 < if drop E-FS-OPEN ERRNO-FAIL then
   work A-NEXT!
   work A-CUR-CELL A-CLOSE-SLOT dup 0<> if CAUSE! E-FS-IO throw then drop
   work A-NEXT@ work A-CUR!
   -1 work A-NEXT! ;

\ A-VALIDATE checks every component before the first open. Splitting ends each
\ component through FFI:CSTR! inside this call's snapshot, never in the caller's
\ path.
: A-WALK ( ptr u8 n n ptr u8 n bool -- )
   {: path:ptr u:n start:n work:ptr parent:n file:bool :}
   start begin dup u < while
      {: at:n :}
      path u at A-COMPONENT-END {: stop:n :}
      stop at - path at + PATHZ-CAP at - SPAN:MAKE FFI:CSTR! drop
      stop u = file and if A-FILE-FLAGS else OPEN-DIR-FLAGS then {: flags:n :}
      work A-CUR@ dup 0 < if drop parent then {: from:n :}
      work from path at + flags A-OPEN-NEXT
      stop u < if stop 1 + else stop then
   repeat drop ;

: A-OPEN-RUN ( ptr u8 n ptr u8 -- ptr u8 n ptr u8 )
   {: src:ptr u:n work:ptr :}
   src u work A-SNAPSHOT {: path:ptr :}
   path u true A-VALIDATE {: start:n :}
   path work A-OPEN-BASE
   path u start work -1 false A-WALK
   src u work ;
: A-OPEN-AT-RUN ( fd ptr u8 n ptr u8 -- fd ptr u8 n ptr u8 )
   {: anchor:fd src:ptr u:n work:ptr :}
   src u work A-SNAPSHOT {: path:ptr :}
   path u false A-VALIDATE {: start:n :}
   path u start work anchor FD>N false A-WALK
   anchor src u work ;

: A-REGULAR ( ptr u8 -- ) {: work:ptr :}
   work A-CUR@ work A-STAT-BUF FSTAT-CALL 0<> if E-FS-STAT ERRNO-FAIL then
   work A-STAT-BUF LIBC-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then ;

: A-READ-STEP ( ptr u8 ptr u8 n -- n ) {: work:ptr dst:ptr count:n :}
   begin
      work A-CUR@ dst count A-READ-CALL {: got:n :}
      got 0 >= if got exit then
      FFI:ERRNO dup A-INTERRUPTED = if drop else CAUSE! E-FS-IO throw then
   again ;
: A-READ-CONTENT ( ptr u8 SPAN:span<u8> -- n ) {: work:ptr dst :}
   dst SPAN:$ {: output:ptr cap:n :}
   0 begin dup cap < while
      {: done:n :}
      work output done + cap done - A-READ-STEP {: got:n :}
      got 0= if done exit then
      got cap done - > if E-FS-IO throw then
      done got +
   repeat drop
   work work A-PROBE-BUF 1 A-READ-STEP {: extra:n :}
   extra 0 > if E-FS-CAPACITY throw then
   cap ;
: A-READ-RUN ( fd ptr u8 n SPAN:span<u8> ptr u8 -- fd ptr u8 n SPAN:span<u8> ptr u8 )
   {: anchor:fd src:ptr u:n dst work:ptr :}
   src u work A-SNAPSHOT {: path:ptr :}
   path u false A-VALIDATE {: start:n :}
   path u start work anchor FD>N true A-WALK
   work A-REGULAR
   work dst A-READ-CONTENT work A-RESULT-CELL A-SLOT !
   anchor src u dst work ;

public

\ / for an absolute path, . for a relative one; the returned descriptor is the
\ caller's. The AT words borrow their anchor and refuse absolute paths. An
\ empty, negative or over-PATH-CAP path and an empty, repeated, trailing, . or
\ .. component throw E-FS-PATH, a NUL E-FS-PATH-UNSAFE (.cache and a..b are
\ ordinary names). A failed syscall leaves its errno in FS:ERRNO, a refusal 0.
: OPEN-DIR ( ptr u8 n -- fd ) {: src:ptr u:n :}
   0 CAUSE!
   A-MAP {: work:ptr len:NUM:alloc-byte-len :}
   src u work [: A-OPEN-RUN ;] catch {: code:n :}
   drop 2drop
   ERRNO {: cause:n :}
   work A-CUR@ {: opened:n :}
   code 0= if -1 work A-CUR! then
   work len code cause A-FINISH
   opened >FD ;
: OPEN-DIR-AT ( fd ptr u8 n -- fd ) {: anchor:fd src:ptr u:n :}
   0 CAUSE!
   A-MAP {: work:ptr len:NUM:alloc-byte-len :}
   anchor src u work [: A-OPEN-AT-RUN ;] catch {: code:n :}
   drop 2drop drop
   ERRNO {: cause:n :}
   work A-CUR@ {: opened:n :}
   code 0= if -1 work A-CUR! then
   work len code cause A-FINISH
   opened >FD ;
\ Reads the descriptor fstat found regular (else E-FS-STAT), probing one byte
\ past a full span (excess is E-FS-CAPACITY); the span is unspecified after a
\ throw.
: READ-FILE-AT ( fd ptr u8 n SPAN:span<u8> -- n )
   {: anchor:fd src:ptr u:n dst :}
   0 CAUSE!
   A-MAP {: work:ptr len:NUM:alloc-byte-len :}
   anchor src u dst work [: A-READ-RUN ;] catch {: code:n :}
   drop 2drop 2drop
   ERRNO {: cause:n :}
   work A-RESULT-CELL A-SLOT @ {: got:n :}
   work len code cause A-FINISH
   got ;
: CLOSE-DIR ( fd -- )
   0 CAUSE!
   FD>N A-CLOSE-CALL 0<> if E-FS-IO ERRNO-FAIL then ;

;package
