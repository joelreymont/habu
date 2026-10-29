\ spawn-test.f - focused tests for src/lib/spawn.f.
\ Run: bin/hb --load src/lib/spawn-test.f
\
\ Children run /bin/sh -c, /bin/echo, /usr/bin/env or /usr/bin/printenv; each
\ pipe is read to EOF and each child waited. A script names the child's fds by
\ digit, which a single-digit shell redirection needs, so tests that place a
\ pipe on a chosen parent fd take a closed one below 9 and patch its digit into
\ the script. The environment cases use this process's first inherited row with
\ a name and an `=` whose name does not start with DYLD_ (macOS strips those
\ from /usr/bin/env and printenv); without one, each records a failure.

require src/lib/errors.f
require src/lib/fd.f
require src/lib/string.f
require src/lib/test.f
require src/lib/process.f
require src/lib/spawn.f

package SPAWN-TEST
private

1 constant F-GETFD                       \ fcntl's F_GETFD on Darwin and Linux alike
2 constant ENOENT
9 constant EBADF
9 constant FD-CEILING                    \ single-digit fds a script can name
$3D constant EQUAL-BYTE
$0A constant LF-BYTE

$1000 constant READ-CHUNK                \ the bytes DRAIN asks each read for
DYNAMIC-BUFFER OUT u8                    \ a child's output, grown to its EOF
variable OUT-LEN
$1000 constant ROW-CAP
ROW-CAP BUFFER: ROW-BYTES
$100 constant SCRIPT-CAP
SCRIPT-CAP BUFFER: SCRIPT-BYTES
variable SCRIPT-LEN


\ Appends `-c script` to the request's arguments.
: SH ( PROCESS:request ptr u8 n -- PROCESS:request )
   {: script:ptr len :}
   s" -c" PROCESS:ARG
   script len PROCESS:ARG ;


\ len copies of byte c.
: FILL$ ( n n -- ptr u8 n )
   {: len c :}
   len 0 ?do c ROW-BYTES i + c! loop
   ROW-BYTES len ;


: SCRIPT! ( ptr u8 n -- )
   {: src:ptr len :}
   src SCRIPT-BYTES len BYTE-COPY
   len SCRIPT-LEN ! ;

: SCRIPT$ ( -- ptr u8 n ) SCRIPT-BYTES SCRIPT-LEN @ ;


\ Writes fd's digit over the script's first byte c.
: PATCH ( n n -- )
   {: fd c :}
   SCRIPT-LEN @ 0 ?do
      SCRIPT-BYTES i + c@ c = if fd $30 + SCRIPT-BYTES i + c! leave then
   loop ;


: FD-OPEN? ( n -- bool ) F-GETFD 0 fcntl 0 >= ;

\ The first fd at or above n this process has closed; it must be one digit.
: CLOSED-FD ( n -- n )
   begin dup FD-OPEN? while 1+ repeat
   dup FD-CEILING < TTRUE ;


\ Reads fd to EOF into OUT, then closes it.
: DRAIN ( fd -- )
   {: r:fd :}
   0 OUT-LEN !
   begin
      OUT-LEN @ READ-CHUNK + OUT-RESERVE
      r FD>N OUT-LEN @ OUT READ-CHUNK read dup 0 > while
      OUT-LEN +!
   repeat drop
   r FD>N close ;

: OUT$ ( -- ptr u8 n ) 0 OUT OUT-LEN @ ;

: WAIT ( pid -- n ) PROC-WAIT-STATUS PROC-STATUS>RC RC>N ;


\ ---- the inherited environment row the environment cases use --------------------
: NAME-LEN ( ptr u8 n -- n )
   {: z:ptr u :}
   0 begin dup u < if dup z + c@ EQUAL-BYTE <> else false then while 1+ repeat ;

\ Row n has a non-empty name and an `=`, and its name does not start with DYLD_.
: SAFE-ROW? ( n -- bool )
   ENVP dup ZLEN {: z:ptr u :}
   z u NAME-LEN {: nu :}
   nu 0 > nu u < and
   z nu s" DYLD_" STARTS-WITH? 0= and ;

\ The first safe row's index, or -1 when there is none.
: PARENT-ROW ( -- n )
   0 begin
      dup ENVP 0= if drop -1 exit then
      dup SAFE-ROW? 0= while 1+ repeat ;

: PARENT-NAME$ ( n -- ptr u8 n )
   ENVP dup ZLEN {: z:ptr u :}
   z z u NAME-LEN ;

: PARENT-VALUE$ ( n -- ptr u8 n )
   ENVP dup ZLEN {: z:ptr u :}
   z u NAME-LEN {: nu :}
   z nu + 1+ u nu - 1- ;


\ ---- lines of OUT$ ---------------------------------------------------------------
\ The bytes of OUT$ at pos are the string.
: AT? ( n ptr u8 n -- bool )
   {: pos s:ptr u :}
   pos u + OUT-LEN @ > if false exit then
   pos OUT u s u STR= ;

\ The line of OUT$ at pos is the string and its LF.
: LINE-IS? ( n ptr u8 n -- bool )
   {: pos s:ptr u :}
   pos s u AT? 0= if false exit then
   pos u + OUT-LEN @ < if pos u + OUT c@ LF-BYTE = else false then ;

\ The position just past name= when the line at pos starts with it, else -1.
: PAST-NAME ( n ptr u8 n -- n )
   {: pos name:ptr u :}
   pos name u AT? 0= if -1 exit then
   pos u + {: at :}
   at OUT-LEN @ < if at OUT c@ EQUAL-BYTE = if at 1+ exit then then
   -1 ;

\ The line of OUT$ at pos is name=value and its LF.
: ROW-LINE? ( n ptr u8 n ptr u8 n -- bool )
   {: pos name:ptr u val:ptr vu :}
   pos name u PAST-NAME dup 0< if drop false exit then
   val vu LINE-IS? ;

\ The position just past the line at pos and its LF.
: NEXT-LINE ( n -- n )
   begin dup OUT-LEN @ < if dup OUT c@ LF-BYTE <> else false then while 1+ repeat
   1+ ;

\ The lines of OUT$ that start with name=.
: NAME-LINES ( ptr u8 n -- n )
   {: name:ptr u :}
   0 0 begin dup OUT-LEN @ < while
      dup name u PAST-NAME 0 >= if swap 1+ swap then
      NEXT-LINE
   repeat drop ;

\ The lines of OUT$ that are name=value.
: ROW-LINES ( ptr u8 n ptr u8 n -- n )
   {: name:ptr u val:ptr vu :}
   0 0 begin dup OUT-LEN @ < while
      dup name u val vu ROW-LINE? if swap 1+ swap then
      NEXT-LINE
   repeat drop ;


\ 1: each target is the other's source, so a source must survive the first
\ placement.
: TEST-CROSSING ( -- )
   s" a crossing map: child fd a is parent fd b and child fd b is parent fd a" T-LABEL
   PIPE-PAIR {: r1:fd w1:fd :}
   PIPE-PAIR {: r2:fd w2:fd :}
   3 CLOSED-FD {: a :}
   w1 FD>N a dup2 drop
   w1 FD>N close
   a 1+ CLOSED-FD {: b :}
   w2 FD>N b dup2 drop
   w2 FD>N close
   s" printf A >&@; printf B >&#" SCRIPT!
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


\ 2: parent fd d is open and unmapped, inside the gap the FD 9 opened.
: TEST-GAP ( -- )
   s" a map grown past its end leaves the gap closed" T-LABEL
   PIPE-PAIR {: r:fd w:fd :}
   3 CLOSED-FD {: d :}
   w FD>N d dup2 drop
   s" exec 2>/dev/null; echo x >&9; echo y >&@ || exit 7" SCRIPT!
   d [char] @ PATCH
   s" /bin/sh" PROCESS:NEW
   SCRIPT$ SH
   9 >FD w PROCESS:FD
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
   PIPE-PAIR {: r:fd w:fd :}
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
   PIPE-PAIR {: r:fd w:fd :}
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
   PIPE-PAIR {: r:fd w:fd :}
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
   PIPE-PAIR {: r:fd w:fd :}
   s" /usr/bin/printenv" PROCESS:NEW
   row PARENT-NAME$ PROCESS:ARG
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   row PARENT-VALUE$ {: val:ptr vu :}
   OUT-LEN @ vu 1+ T=
   0 val vu LINE-IS? TTRUE
   pid WAIT 0 T= ;


\ 8: one row of the name reaches the child, the request's.
: TEST-OVERRIDE ( -- )
   s" an ENV row replaces the inherited row of its name" T-LABEL
   PARENT-ROW {: row :}
   row 0 >= dup TTRUE 0= if exit then
   PIPE-PAIR {: r:fd w:fd :}
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
   PIPE-PAIR {: r:fd w:fd :}
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
: TEST-REFUSALS ( -- )
   s" a negative child fd, an empty directory, a bad name and a closed parent fd are refused" T-LABEL
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


\ 11a: each string a request copies is refused at the copy, so no child runs on
\ a truncated one.
: TEST-NUL ( -- )
   s" a 0 byte in a path, argument, environment name or value, or directory is refused" T-LABEL
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
   1 F-DUPFD-CLOEXEC 10 fcntl {: saved :}
   saved 0 >= TTRUE
   1 close
   0 [: drop CLOSED-STDOUT-CHILD ;] catch {: rc :}
   saved 1 dup2 drop
   saved close
   rc 0<> if drop rc throw then
   7 T= ;


\ 13: the request's record and each member it maps is one page. PROBES one-page
\ mappings, freed, are the pages first-fit reuse hands the request next, as
\ src/lib/json-read-test.f JRT-RELEASES relies on; mapping them again lands on
\ the same pages only when DISCARD returned every one.
4096 constant PROBE-BYTES
8 constant PROBES
PROBES TYPED-BUFFER PROBE-AT ptr u8

: PROBE-LEN ( -- NUM:alloc-byte-len ) PROBE-BYTES MEM:BYTES-ALLOC-LEN ;
: PROBE ( -- ptr u8 ) PROBE-LEN MEM:ALLOC-BYTES drop ;
: UNPROBE ( -- ) PROBES 0 ?do i PROBE-AT @ PROBE-LEN MEM:RELEASE-BYTES loop ;

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
   PROBES 0 ?do PROBE i PROBE-AT ! loop
   UNPROBE
   DISCARDED
   true PROBES 0 ?do
      PROBE {: again:ptr :}
      again i PROBE-AT @ = and
      again i PROBE-AT !
   loop
   UNPROBE
   TTRUE ;


\ 14
: TEST-CHECK-FD ( -- )
   s" CHECK-FD refuses a closed fd with EBADF; an open one leaves ERRNO" T-LABEL
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
   TEST-REFUSALS
   TEST-NUL
   TEST-CLOSED-STDIO
   TEST-DISCARD
   TEST-CHECK-FD
   T-REPORT
   s" spawn-test: ok" type cr ;

;package

SPAWN-TEST:SPAWN-TEST-MAIN
