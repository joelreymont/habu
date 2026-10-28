\ spawn-test.f - focused tests for src/lib/spawn.f.
\ Run: bin/hb --load src/lib/spawn-test.f
\
\ Children run /bin/sh -c or /usr/bin/env; each pipe is read to EOF and each
\ child waited. A script names the child's fds by digit, which a single-digit
\ shell redirection needs, so tests that place a pipe on a chosen parent fd take
\ a closed one below 9 and patch its digit into the script.

require src/lib/errors.f
require src/lib/test.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/spawn.f

package SPAWN-TEST
private

$100 constant CAP
1 constant F-GETFD                       \ fcntl's F_GETFD on Darwin and Linux alike
2 constant ENOENT
9 constant EBADF
9 constant FD-CEILING                    \ single-digit fds a script can name

$1000 constant READ-CHUNK                \ the bytes DRAIN asks each read for
DYNAMIC-BUFFER OUT u8                    \ a child's output, grown to its EOF
variable OUT-LEN
$4000 constant ROW-CAP                   \ room for five rows past one 4 KiB chunk
ROW-CAP BUFFER: ROW-BYTES
variable ROW-USED
variable ROW-N
16 PTR-U8-TABLE ROWS
CAP BUFFER: SCRIPT-BYTES
variable SCRIPT-LEN

: ROWS-RESET ( -- )
   0 ROW-USED !
   0 ROW-N !
   NULL-PTR ROWS 0 ptr-field ! ;


: ROW-SLOT ( -- ptr u8 ) ROW-BYTES ROW-USED @ + ;

\ Ends the len bytes at ROW-SLOT with a NUL, appends them to ROWS and keeps
\ ROWS NULL-terminated.
: ROW-END ( n -- )
   {: len :}
   0 ROW-SLOT len + c!
   ROW-SLOT ROWS ROW-N @ ptr-field !
   len 1+ ROW-USED +!
   1 ROW-N +!
   NULL-PTR ROWS ROW-N @ ptr-field ! ;

: ROW+ ( ptr u8 n -- )
   {: src:ptr len :}
   src ROW-SLOT len BYTE-COPY
   len ROW-END ;

\ Appends a row of len copies of byte c.
: FILL+ ( n n -- )
   {: len c :}
   len 0 ?do c ROW-SLOT i + c! loop
   len ROW-END ;


\ argv for `sh -c script`.
: SH-ROWS ( ptr u8 n -- ptr ptr u8 )
   {: script:ptr len :}
   ROWS-RESET
   s" sh" ROW+
   s" -c" ROW+
   script len ROW+
   ROWS ;


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
   s" /bin/sh" SPAWN:NEW
   SCRIPT$ SH-ROWS SPAWN:ARGV
   a >FD b >FD SPAWN:FD
   b >FD a >FD SPAWN:FD
   SPAWN:SPAWN {: pid :}
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
   s" /bin/sh" SPAWN:NEW
   SCRIPT$ SH-ROWS SPAWN:ARGV
   9 >FD w SPAWN:FD
   SPAWN:SPAWN {: pid :}
   w FD>N close
   d close
   r DRAIN
   OUT$ S\" x\n" T$=
   pid WAIT 7 T= ;


\ 3
: TEST-CLOSE ( -- )
   s" CLOSE 1 starts the child with fd 1 closed" T-LABEL
   s" /bin/sh" SPAWN:NEW
   s" exec 2>/dev/null; printf z || exit 7" SH-ROWS SPAWN:ARGV
   1 >FD SPAWN:CLOSE
   SPAWN:SPAWN WAIT 7 T= ;


\ 4
: TEST-CWD ( -- )
   s" CWD starts the child in the directory" T-LABEL
   PIPE-PAIR {: r:fd w:fd :}
   s" /bin/sh" SPAWN:NEW
   s" pwd" SH-ROWS SPAWN:ARGV
   s" /" SPAWN:CWD
   1 >FD w SPAWN:FD
   SPAWN:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   OUT$ S\" /\n" T$=
   pid WAIT 0 T= ;


\ 5: the process-wide env staging rewrites the same bytes after a reset; the
\ request keeps copies of its own.
: TEST-ENV-OWNED ( -- )
   s" ENV copies rows the process-wide staging later rewrites" T-LABEL
   PIPE-PAIR {: r:fd w:fd :}
   PROC-ENV-RESET
   s" SPAWNT" >LEN s" one" >LEN PROC-ENV+
   s" /usr/bin/env" SPAWN:NEW
   PROC-ENV-PREPARE SPAWN:ENV
   PROC-ENV-RESET
   s" SPAWNT" >LEN s" two" >LEN PROC-ENV+
   1 >FD w SPAWN:FD
   SPAWN:SPAWN {: pid :}
   PROC-ENV-RESET
   w FD>N close
   r DRAIN
   OUT$ S\" SPAWNT=one\n" T$=
   pid WAIT 0 T= ;


\ 6: five 3000-byte rows need five 4 KiB chunks, so the request's chunk table
\ grows and copies its entries more than once.
: TEST-ARGV-CHUNKS ( -- )
   s" argv rows past one chunk reach the child whole" T-LABEL
   s\" for a; do printf '%s%.1s ' \"${#a}\" \"$a\"; done" SH-ROWS drop
   s" sh" ROW+
   3000 [char] a FILL+
   3000 [char] b FILL+
   3000 [char] c FILL+
   3000 [char] d FILL+
   3000 [char] e FILL+
   s" f" ROW+
   PIPE-PAIR {: r:fd w:fd :}
   s" /bin/sh" SPAWN:NEW
   ROWS SPAWN:ARGV
   1 >FD w SPAWN:FD
   SPAWN:SPAWN {: pid :}
   w FD>N close
   r DRAIN
   OUT$ s" 3000a 3000b 3000c 3000d 3000e 1f " T$=
   pid WAIT 0 T= ;


\ 7
: TEST-MISSING ( -- )
   s" a missing program throws ENOENT in ERRNO and the next spawn clears it" T-LABEL
   [: s" /nonexistent" SPAWN:NEW SPAWN:SPAWN drop ;] E-PROC-SPAWN TTHROWSQ
   SPAWN:ERRNO ENOENT T=
   s" /bin/sh" SPAWN:NEW
   s" exit 0" SH-ROWS SPAWN:ARGV
   SPAWN:SPAWN {: pid :}
   SPAWN:ERRNO 0 T=
   pid WAIT 0 T= ;


variable DEAD-FD

\ 8: an explicit slot keeps its closed parent fd, unlike a default one (case 9).
: TEST-REFUSALS ( -- )
   s" a negative child fd, an empty directory and a closed parent fd are refused" T-LABEL
   [: s" /bin/sh" SPAWN:NEW -1 >FD 1 >FD SPAWN:FD SPAWN:SPAWN drop ;] E-PROC-OUTPUT TTHROWSQ
   [: s" /bin/sh" SPAWN:NEW s" " SPAWN:CWD SPAWN:SPAWN drop ;] E-PROC-PATH TTHROWSQ
   3 CLOSED-FD DEAD-FD !
   [: s" /bin/sh" SPAWN:NEW 0 >FD DEAD-FD @ >FD SPAWN:FD SPAWN:SPAWN drop ;]
   E-PROC-SPAWN TTHROWSQ
   SPAWN:ERRNO EBADF T= ;


: CLOSED-STDOUT-CHILD ( -- n )
   s" /bin/sh" SPAWN:NEW
   s" exec 2>/dev/null; printf z || exit 7" SH-ROWS SPAWN:ARGV
   SPAWN:SPAWN WAIT ;


\ 9: this process's fd 1 is closed for the spawn and restored after it.
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

public

: SPAWN-TEST-MAIN ( -- )
   TEST-CROSSING
   TEST-GAP
   TEST-CLOSE
   TEST-CWD
   TEST-ENV-OWNED
   TEST-ARGV-CHUNKS
   TEST-MISSING
   TEST-REFUSALS
   TEST-CLOSED-STDIO
   T-REPORT
   s" spawn-test: ok" type cr ;

;package

SPAWN-TEST:SPAWN-TEST-MAIN
