\ spawn-test.f - focused tests for src/lib/spawn.f.
\ Run: bin/hb --load src/lib/spawn-test.f
\ A script names a child's fd as /dev/fd/N, which dash and macOS sh open for any
\ N, so a test places a pipe on any closed parent fd and patches its number in.
\ Environment cases use the first inherited row not named
\ DYLD_* (macOS strips those from env and printenv); without one, each fails.

require src/lib/error.f
require src/lib/fd.f
require src/lib/fmt.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/test/mapped.f
require src/core/exit-status.f

package SPAWN-TEST
private

1 constant F-GETFD                       \ fcntl's F_GETFD on Darwin and Linux alike
2 constant ENOENT
9 constant EBADF
$3D constant EQUAL-BYTE
$0A constant LF-BYTE

$1000 constant READ-CHUNK
DYNAMIC-BUFFER OUT u8
variable OUT-LEN
$1000 constant ROW-CAP
ROW-CAP BUFFER: ROW-BYTES
$100 constant SCRIPT-CAP
SCRIPT-CAP BUFFER: SCRIPT-BYTES
variable SCRIPT-LEN


: SH ( PROCESS:request str -- PROCESS:request )
   {: script:str :}
   s" -c" PROCESS:ARG
   script PROCESS:ARG ;


: FILL$ ( n n -- str )
   {: len c :}
   len 0 ?do c ROW-BYTES i + c! loop
   ROW-BYTES len >str ;


: SCRIPT! ( str -- )
   {: src:str :}
   src STR:DATA SCRIPT-BYTES src STR:LEN BYTE-COPY
   src STR:LEN SCRIPT-LEN ! ;

: SCRIPT$ ( -- str ) SCRIPT-BYTES SCRIPT-LEN @ >str ;


: MARKER-AT ( n -- n )
   {: c :}
   0 begin dup SCRIPT-LEN @ < if dup SCRIPT-BYTES + c@ c <> else false then while 1+ repeat ;

\ Writes fd's decimal digits over the first byte c, moving the bytes after it.
: PATCH ( n n -- )
   {: fd c :}
   c MARKER-AT {: at :}
   at SCRIPT-LEN @ < 0= if exit then
   fd FMT:INT$ {: d:str :}
   SCRIPT-LEN @ 1- d STR:LEN + {: len :}
   len SCRIPT-CAP > if s" spawn-test: script too long" EXIT-STATUS:FAILURE die then
   SCRIPT-LEN @ begin 1- dup at > while
      dup SCRIPT-BYTES + c@ over d STR:LEN + 1- SCRIPT-BYTES + c!
   repeat drop
   d STR:DATA SCRIPT-BYTES at + d STR:LEN BYTE-COPY
   len SCRIPT-LEN ! ;


: FD-OPEN? ( n -- bool ) F-GETFD 0 fcntl 0 >= ;

\ The first fd at or above n this process has closed.
: CLOSED-FD ( n -- n )
   begin dup FD-OPEN? while 1+ repeat ;


: DRAIN ( fd -- )
   {: r:fd :}
   0 OUT-LEN !
   begin
      OUT-LEN @ READ-CHUNK + OUT-RESERVE
      r FD>N OUT-LEN @ OUT READ-CHUNK read dup 0 > while
      OUT-LEN +!
   repeat drop
   r FD>N close ;

: OUT$ ( -- str ) 0 OUT OUT-LEN @ >str ;

: WAIT ( pid -- n ) PROCESS:WAIT PROCESS:STATUS>RC RC>N ;


: NAME-LEN ( str -- n )
   {: z:str :}
   0 begin dup z STR:LEN < if dup z STR:DATA + c@ EQUAL-BYTE <> else false then while 1+ repeat ;

: SAFE-ROW? ( n -- bool )
   ENVP dup ZLEN >str {: z:str :}
   z NAME-LEN {: nu :}
   nu 0 > nu z STR:LEN < and
   z STR:DATA nu >str s" DYLD_" STR:STARTS-WITH? 0= and ;

: PARENT-ROW ( -- n )
   0 begin
      dup ENVP 0= if drop -1 exit then
      dup SAFE-ROW? 0= while 1+ repeat ;

: PARENT-NAME$ ( n -- str )
   ENVP dup ZLEN >str {: z:str :}
   z STR:DATA z NAME-LEN >str ;

: PARENT-VALUE$ ( n -- str )
   ENVP dup ZLEN >str {: z:str :}
   z NAME-LEN {: nu :}
   z STR:DATA nu + 1+ z STR:LEN nu - 1- >str ;


: AT? ( n str -- bool )
   {: pos s:str :}
   pos s STR:LEN + OUT-LEN @ > if false exit then
   pos OUT s STR:LEN >str s STR:= ;

: LINE-IS? ( n str -- bool )
   {: pos s:str :}
   pos s AT? 0= if false exit then
   pos s STR:LEN + OUT-LEN @ < if pos s STR:LEN + OUT c@ LF-BYTE = else false then ;

: PAST-NAME ( n str -- n )
   {: pos name:str :}
   pos name AT? 0= if -1 exit then
   pos name STR:LEN + {: at :}
   at OUT-LEN @ < if at OUT c@ EQUAL-BYTE = if at 1+ exit then then
   -1 ;

: ROW-LINE? ( n str str -- bool )
   {: pos name:str val:str :}
   pos name PAST-NAME dup 0< if drop false exit then
   val LINE-IS? ;

: NEXT-LINE ( n -- n )
   begin dup OUT-LEN @ < if dup OUT c@ LF-BYTE <> else false then while 1+ repeat
   1+ ;

: NAME-LINES ( str -- n )
   {: name:str :}
   0 0 begin dup OUT-LEN @ < while
      dup name PAST-NAME 0 >= if swap 1+ swap then
      NEXT-LINE
   repeat drop ;

: ROW-LINES ( str str -- n )
   {: name:str val:str :}
   0 0 begin dup OUT-LEN @ < while
      dup name val ROW-LINE? if swap 1+ swap then
      NEXT-LINE
   repeat drop ;


\ 1: each target is the other's source, so a source must survive the first
\ placement.
: TEST-CROSSING ( -- )
   s" a crossing map: child fd a is parent fd b and child fd b is parent fd a" T-LABEL
   FD:PIPE-PAIR {: r1:fd w1:fd :}
   FD:PIPE-PAIR {: r2:fd w2:fd :}
   3 CLOSED-FD {: a :}
   w1 FD>N a dup2 drop
   w1 FD>N close
   a 1+ CLOSED-FD {: b :}
   w2 FD>N b dup2 drop
   w2 FD>N close
   s" printf A >/dev/fd/@; printf B >/dev/fd/#" SCRIPT!
   a [char] @ PATCH
   b [char] # PATCH
   s" /bin/sh" PROCESS:NEW
   SCRIPT$ SH
   a >FD b >FD PROCESS:FD
   b >FD a >FD PROCESS:FD
   PROCESS:SPAWN {: pid :}
   a close
   b close
   r1 DRAIN
   OUT$ s" B" T$=
   r2 DRAIN
   OUT$ s" A" T$=
   pid WAIT 0 T= ;


\ 2: parent fd d is open and unmapped, inside the gap the map's child fd d+1
\ opened.
: TEST-GAP ( -- )
   s" a map grown past its end leaves the gap closed" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   3 CLOSED-FD {: d :}
   w FD>N d dup2 drop
   s" exec 2>/dev/null; echo x >/dev/fd/#; echo y >/dev/fd/@ || exit 7" SCRIPT!
   d [char] @ PATCH
   d 1+ [char] # PATCH
   s" /bin/sh" PROCESS:NEW
   SCRIPT$ SH
   d 1+ >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   d close
   r DRAIN
   OUT$ S\" x\n" T$=
   pid WAIT 7 T= ;


\ 3
: TEST-CLOSED ( -- )
   s" CLOSED 1 starts the child with fd 1 closed" T-LABEL
   s" /bin/sh" PROCESS:NEW
   s" exec 2>/dev/null; printf z || exit 7" SH
   1 >FD PROCESS:CLOSED
   PROCESS:SPAWN WAIT 7 T= ;


\ 4
: TEST-CWD ( -- )
   s" CWD starts the child in the directory" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /bin/sh" PROCESS:NEW
   s" pwd" SH
   s" /" PROCESS:CWD
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   OUT$ S\" /\n" T$=
   pid WAIT 0 T= ;


\ 5: the arguments reach the child in the order ARG gave them, after the path.
: TEST-ARG-ORDER ( -- )
   s" ARG appends argv[1] onward in order" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /bin/echo" PROCESS:NEW
   s" a" PROCESS:ARG
   s" b" PROCESS:ARG
   s" c" PROCESS:ARG
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   OUT$ S\" a b c\n" T$=
   pid WAIT 0 T= ;


\ 6: the value bytes are rewritten before the spawn; the request keeps a copy.
: TEST-HERMETIC ( -- )
   s" HERMETIC gives the child only the request's rows, copied" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" one" SCRIPT!
   s" /usr/bin/env" PROCESS:NEW
   PROCESS:HERMETIC
   s" SPAWNT" SCRIPT$ PROCESS:ENV
   s" two" SCRIPT!
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   OUT$ S\" SPAWNT=one\n" T$=
   pid WAIT 0 T= ;


\ 7
: TEST-INHERIT ( -- )
   s" a request inherits this process's environment" T-LABEL
   PARENT-ROW {: row :}
   row 0 >= dup TTRUE 0= if exit then
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /usr/bin/printenv" PROCESS:NEW
   row PARENT-NAME$ PROCESS:ARG
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   row PARENT-VALUE$ {: val:str :}
   OUT-LEN @ val STR:LEN 1+ T=
   0 val LINE-IS? TTRUE
   pid WAIT 0 T= ;


\ 8: one row of the name reaches the child, the request's.
: TEST-OVERRIDE ( -- )
   s" an ENV row replaces the inherited row of its name" T-LABEL
   PARENT-ROW {: row :}
   row 0 >= dup TTRUE 0= if exit then
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /usr/bin/env" PROCESS:NEW
   row PARENT-NAME$ s" over" PROCESS:ENV
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   row PARENT-NAME$ NAME-LINES 1 T=
   row PARENT-NAME$ s" over" ROW-LINES 1 T=
   pid WAIT 0 T= ;


: ARGV-CHUNKS ( PROCESS:request -- PROCESS:request )
   s\" for a; do printf '%s%.1s ' \"${#a}\" \"$a\"; done" SH
   s" sh" PROCESS:ARG
   3000 [char] a FILL$ PROCESS:ARG
   3000 [char] b FILL$ PROCESS:ARG
   3000 [char] c FILL$ PROCESS:ARG
   3000 [char] d FILL$ PROCESS:ARG
   3000 [char] e FILL$ PROCESS:ARG
   s" f" PROCESS:ARG ;


\ 9: five 3000-byte rows need five 4 KiB chunks, so the request's chunk table
\ grows and copies its entries more than once.
: TEST-ARGV-CHUNKS ( -- )
   s" argv rows past one chunk reach the child whole" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /bin/sh" PROCESS:NEW
   ARGV-CHUNKS
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   OUT$ s" 3000a 3000b 3000c 3000d 3000e 1f " T$=
   pid WAIT 0 T= ;


\ 10
: TEST-MISSING ( -- )
   s" a missing program throws ENOENT in ERRNO and the next spawn clears it" T-LABEL
   [: s" /nonexistent" PROCESS:NEW PROCESS:SPAWN drop ;] E-PROC-SPAWN TTHROWSQ
   PROCESS:ERRNO ENOENT T=
   s" /bin/sh" PROCESS:NEW
   s" exit 0" SH
   PROCESS:SPAWN {: pid :}
   PROCESS:ERRNO 0 T=
   pid WAIT 0 T= ;


variable DEAD-FD

\ 11: an explicit slot keeps its closed parent fd, unlike a default one (case 12).
\ The second closed fd is the one pipe2 gives the Linux error pipe's write end,
\ so a source equal to it must still be EBADF, not the pipe placed on the slot.
: TEST-REJECTIONS ( -- )
   s" a negative child fd, an empty directory, a bad name and a closed parent fd are rejected" T-LABEL
   [: s" /bin/sh" PROCESS:NEW -1 >FD 1 >FD PROCESS:FD PROCESS:SPAWN drop ;] E-PROC-OUTPUT TTHROWSQ
   [: s" /bin/sh" PROCESS:NEW s" " PROCESS:CWD PROCESS:SPAWN drop ;] E-PROC-PATH TTHROWSQ
   [: s" /bin/sh" PROCESS:NEW s" " s" v" PROCESS:ENV PROCESS:SPAWN drop ;] E-PROC-ENV TTHROWSQ
   [: s" /bin/sh" PROCESS:NEW s" A=B" s" v" PROCESS:ENV PROCESS:SPAWN drop ;] E-PROC-ENV TTHROWSQ
   3 CLOSED-FD DEAD-FD !
   [: s" /bin/sh" PROCESS:NEW 0 >FD DEAD-FD @ >FD PROCESS:FD PROCESS:SPAWN drop ;]
   E-PROC-SPAWN TTHROWSQ
   PROCESS:ERRNO EBADF T=
   DEAD-FD @ 1+ CLOSED-FD DEAD-FD !
   [: s" /bin/sh" PROCESS:NEW 0 >FD DEAD-FD @ >FD PROCESS:FD PROCESS:SPAWN drop ;]
   E-PROC-SPAWN TTHROWSQ
   PROCESS:ERRNO EBADF T= ;


\ 11a: each string a request copies is rejected at the copy, so no child runs on
\ a truncated one.
: TEST-NUL ( -- )
   s" a 0 byte in a path, argument, environment name or value, or directory is rejected" T-LABEL
   [: s\" /usr/bin/\ztrue" PROCESS:NEW PROCESS:SPAWN drop ;] E-FFI-NUL TTHROWSQ
   [: s" /usr/bin/true" PROCESS:NEW s\" a\zb" PROCESS:ARG PROCESS:SPAWN drop ;] E-FFI-NUL TTHROWSQ
   [: s" /usr/bin/true" PROCESS:NEW s" K" s\" b\zC=x" PROCESS:ENV PROCESS:SPAWN drop ;] E-FFI-NUL TTHROWSQ
   [: s" /usr/bin/true" PROCESS:NEW s\" K\zL" s" v" PROCESS:ENV PROCESS:SPAWN drop ;] E-FFI-NUL TTHROWSQ
   [: s" /usr/bin/true" PROCESS:NEW s\" /tmp\zx" PROCESS:CWD PROCESS:SPAWN drop ;] E-FFI-NUL TTHROWSQ ;

: CLOSED-STDOUT-CHILD ( -- n )
   s" /bin/sh" PROCESS:NEW
   s" exec 2>/dev/null; printf z || exit 7" SH
   PROCESS:SPAWN WAIT ;


\ 12: this process's fd 1 is closed for the spawn and restored after it.
: TEST-CLOSED-STDIO ( -- )
   s" a default stdio slot whose parent fd is closed starts closed" T-LABEL
   1 FD:F-DUPFD-CLOEXEC 10 fcntl {: saved :}
   saved 0 >= TTRUE
   1 close
   0 [: drop CLOSED-STDOUT-CHILD ;] catch {: rc :}
   saved 1 dup2 drop
   saved close
   rc 0<> if drop rc throw then
   7 T= ;


\ 13: the request's record and each member it maps is one page.
\ Six ARGs grow argv past its first mapping and child fd 5 grows the map.
: DISCARDED ( -- )
   s" /bin/echo" PROCESS:NEW
   s" a" PROCESS:ARG s" b" PROCESS:ARG s" c" PROCESS:ARG
   s" d" PROCESS:ARG s" e" PROCESS:ARG s" f" PROCESS:ARG
   s" P" s" 1" PROCESS:ENV s" Q" s" 2" PROCESS:ENV
   5 >FD 1 >FD PROCESS:FD
   PROCESS:DISCARD ;

: TEST-DISCARD ( -- )
   s" a discarded request frees its record and its members" T-LABEL
   [: DISCARDED ;] MAPPED:RELEASED? TTRUE ;


\ 14
: TEST-CHECK-FD ( -- )
   s" CHECK-FD rejects a closed fd with EBADF; an open one leaves ERRNO" T-LABEL
   3 CLOSED-FD DEAD-FD !
   [: DEAD-FD @ >FD PROCESS:CHECK-FD ;] E-PROC-SPAWN TTHROWSQ
   PROCESS:ERRNO EBADF T=
   FD:STDOUT PROCESS:CHECK-FD
   PROCESS:ERRNO EBADF T= ;

public

: SPAWN-TEST-MAIN ( -- )
   TEST-CROSSING
   TEST-GAP
   TEST-CLOSED
   TEST-CWD
   TEST-ARG-ORDER
   TEST-HERMETIC
   TEST-INHERIT
   TEST-OVERRIDE
   TEST-ARGV-CHUNKS
   TEST-MISSING
   TEST-REJECTIONS
   TEST-NUL
   TEST-CLOSED-STDIO
   TEST-DISCARD
   TEST-CHECK-FD
   T-REPORT
   s" spawn-test: ok" type cr ;

;package

SPAWN-TEST:SPAWN-TEST-MAIN
