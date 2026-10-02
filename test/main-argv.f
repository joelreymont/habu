\ main-argv.f - src/habu/main.f ENGINE-MAIN:RUN against the engine's own argv
\ reader, through the real load path.
\
\ Each case is an argument list that two child engines run, each with an empty
\ stdin:
\   bin/hb <args>                                                 the assembly reads argv
\   bin/hb --load src/habu/main.f test/main-argv-child.f -- <args>  RUN reads a vector built from <args>
\ The two must end with the same rc, stdout and stderr, and each case states the
\ rc and output that show which route ran, so two runs failing alike do not
\ pass. Two kinds of case differ on purpose: an empty file list, where RUN
\ keeps the engine's status and names the cause, and a full source arena,
\ where the engine's message ends in a NUL byte instead of a newline.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fmt.f
require test/gate-common.f
require src/habu/layout.f

package MAIN-ARGV-TEST

variable CASES

\ ---- the files --------------------------------------------------------------------
create PATH-BUF FS-PATH-CAP allot

\ The fixture name's path in this run's directory. Valid until the next call;
\ GE-ARG+ copies it.
: FIX ( ptr u8 n -- ptr u8 n )
   PATH-BUF GT-PATH PATH-BUF swap ;

: FIXTURE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   name nameu FIX src srcu WRITE-ALL ;

: FIXTURES ( -- )
   S\" s\" A\" type cr\n" s" a.f" FIXTURE
   S\" s\" B\" type cr\n" s" b.f" FIXTURE
   S\" #!/usr/bin/env hb\ns\" SH\" type cr\n" s" sh.f" FIXTURE
   s" #!" s" bang.f" FIXTURE
   S\" #x\n" s" hash.f" FIXTURE
   S\" nosuchword\n" s" bad.f" FIXTURE
   \ The files after the first of --build load through the registry.
   S\" package MA-D public : HI ( -- ) s\" HI\" type cr ; ;package\n" s" def.f" FIXTURE
   S\" MA-D:HI\n" s" use-def.f" FIXTURE
   S\" package MA-P public : HELLO ( -- ) s\" H\" type cr ; ;package using MA-P\n" s" pkg.f" FIXTURE
   S\" HELLO ;using\n" s" use-pkg.f" FIXTURE
   \ A program that requires itself runs once: its path was recorded first.
   SB-RESET
   S\" s\" S\" type cr s\" " SB-APPEND
   s" self.f" FIX SB-APPEND
   S\" \" required\n" SB-APPEND
   SB$ s" self.f" FIXTURE ;

\ ---- the two runs of a case --------------------------------------------------------
create WANT-OUT GT-OUT-CAP allot
variable WANT-OUT-U
create WANT-ERR GT-ERR-CAP allot
variable WANT-ERR-U
variable WANT-RC

: WANT-OUT$ ( -- ptr u8 n )
   WANT-OUT WANT-OUT-U @ ;

: WANT-ERR$ ( -- ptr u8 n )
   WANT-ERR WANT-ERR-U @ ;

\ The engine's run, kept while RUN's runs.
: KEEP ( -- )
   GT-RC@ WANT-RC !
   GT-OUT$ {: oa:ptr ou:n :}  oa WANT-OUT ou BYTE-COPY  ou WANT-OUT-U !
   GT-ERR$ {: ea:ptr eu:n :}  ea WANT-ERR eu BYTE-COPY  eu WANT-ERR-U ! ;

: MISMATCH ( ptr u8 n ptr u8 n -- ) {: why:ptr whyu:n label:ptr labelu:n :}
   why whyu type cr
   s" engine rc: " type WANT-RC @ FMT:.INT cr
   s" engine stdout:" type cr WANT-OUT$ type
   s" engine stderr:" type cr WANT-ERR$ type
   label labelu GE-FAIL ;

: SAME ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ WANT-RC @ <> if s" rc differs from the engine's" label labelu MISMATCH then
   GT-OUT$ WANT-OUT$ STR= 0= if s" stdout differs from the engine's" label labelu MISMATCH then
   GT-ERR$ WANT-ERR$ STR= 0= if s" stderr differs from the engine's" label labelu MISMATCH then ;

\ The arguments are a quotation that adds them, so each run builds its own.
: ENGINE ( [ -- ] -- ) {: args :}
   GE-HB-RESET
   args execute
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

: HABU ( [ -- ] -- ) {: args :}
   GE-HB-RESET
   s" --load" GE-ARG+
   s" src/habu/main.f" GE-ARG+
   s" test/main-argv-child.f" GE-ARG+
   s" --" GE-ARG+
   args execute
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

: BOTH ( [ -- ] ptr u8 n -- ) {: args label:ptr labelu:n :}
   args ENGINE KEEP
   args HABU
   label labelu SAME
   1 CASES +! ;

\ ---- the expectations, on the last run -------------------------------------------
: OUT ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   label labelu GE-EXPECT-OK
   want wantu label labelu GE-EXPECT-OUT
   s" " label labelu GE-EXPECT-ERR ;

: REFUSED ( n ptr u8 n ptr u8 n -- ) {: rc:n want:ptr wantu:n label:ptr labelu:n :}
   rc label labelu GE-EXPECT-RC
   s" " label labelu GE-EXPECT-OUT
   want wantu label labelu GE-EXPECT-ERR ;

\ stderr for a path the route could not open.
: CANNOT-OPEN$ ( ptr u8 n -- ptr u8 n )
   SB-RESET s" hb: cannot open " SB-APPEND SB-APPEND 10 SB-APPEND-C SB$ ;

: USAGE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET s" hb: unknown flag: " SB-APPEND SB-APPEND 10 SB-APPEND-C
   s" usage: bin/hb --load --build -- [file.f]  (source on stdin)" SB-APPEND
   10 SB-APPEND-C SB$ ;

\ ---- --load ----------------------------------------------------------------------
: LOAD-FILES ( -- )
   [: s" --load" GE-ARG+ s" a.f" FIX GE-ARG+ s" b.f" FIX GE-ARG+ ;] s" load two" BOTH
   S\" A\nB\n" s" load two" OUT
   [: s" --load" GE-ARG+ s" a.f" FIX GE-ARG+ s" --" GE-ARG+ s" b.f" FIX GE-ARG+ ;]
   s" load stops at --" BOTH
   S\" A\n" s" load stops at --" OUT
   \ After `--` a flag is the program's argument, not a flag and not a file.
   [: s" --load" GE-ARG+ s" a.f" FIX GE-ARG+ s" --" GE-ARG+ s" --load" GE-ARG+ ;]
   s" load, -- then a flag" BOTH
   S\" A\n" s" load, -- then a flag" OUT
   \ Before it, past argv[1], a flag-shaped argument is a file name.
   [: s" --load" GE-ARG+ s" a.f" FIX GE-ARG+ s" --x" GE-ARG+ ;] s" load, a flag-shaped file" BOTH
   74 s" load, a flag-shaped file" GE-EXPECT-RC
   S\" A\n" s" load, a flag-shaped file" GE-EXPECT-OUT
   s" include: cannot open " s" load, a flag-shaped file" GE-EXPECT-ERR-HAS
   s" /--x" s" load, a flag-shaped file" GE-EXPECT-ERR-HAS
   \ The registry loads the file as written: a `#!` line is a token.
   [: s" --load" GE-ARG+ s" sh.f" FIX GE-ARG+ ;] s" load, a shebang" BOTH
   70 S\" E-UNDEFINED: #!/usr/bin/env\n" s" load, a shebang" REFUSED
   [: s" --load" GE-ARG+ s" missing.f" FIX GE-ARG+ ;] s" load, a missing file" BOTH
   74 s" load, a missing file" GE-EXPECT-RC
   s" include: cannot open " s" load, a missing file" GE-EXPECT-ERR-HAS ;

\ ---- --build ---------------------------------------------------------------------
: BUILD-FILES ( -- )
   [: s" --build" GE-ARG+ s" a.f" FIX GE-ARG+ s" b.f" FIX GE-ARG+ ;] s" build two" BOTH
   S\" A\nB\n" s" build two" OUT
   [: s" --build" GE-ARG+ s" a.f" FIX GE-ARG+ s" --" GE-ARG+ s" b.f" FIX GE-ARG+ ;]
   s" build stops at --" BOTH
   S\" A\n" s" build stops at --" OUT
   [: s" --build" GE-ARG+ s" sh.f" FIX GE-ARG+ ;] s" build, a shebang" BOTH
   S\" SH\n" s" build, a shebang" OUT
   [: s" --build" GE-ARG+ s" self.f" FIX GE-ARG+ ;] s" build records the file" BOTH
   S\" S\n" s" build records the file" OUT
   [: s" --build" GE-ARG+ s" missing.f" FIX GE-ARG+ ;] s" build, a missing file" BOTH
   74 s" missing.f" FIX CANNOT-OPEN$ s" build, a missing file" REFUSED
   [: s" --build" GE-ARG+ s" def.f" FIX GE-ARG+ s" use-def.f" FIX GE-ARG+ ;]
   s" build, a definition carries" BOTH
   S\" HI\n" s" build, a definition carries" OUT
   \ A later file is loaded as written: its `#!` line is a token.
   [: s" --build" GE-ARG+ s" a.f" FIX GE-ARG+ s" sh.f" FIX GE-ARG+ ;]
   s" build, a later shebang" BOTH
   70 s" build, a later shebang" GE-EXPECT-RC
   S\" A\n" s" build, a later shebang" GE-EXPECT-OUT
   S\" E-UNDEFINED: #!/usr/bin/env\n" s" build, a later shebang" GE-EXPECT-ERR
   \ The first file runs before the registry looks for a later one.
   [: s" --build" GE-ARG+ s" a.f" FIX GE-ARG+ s" missing.f" FIX GE-ARG+ ;]
   s" build, a later missing file" BOTH
   74 s" build, a later missing file" GE-EXPECT-RC
   S\" A\n" s" build, a later missing file" GE-EXPECT-OUT
   s" include: cannot open " s" build, a later missing file" GE-EXPECT-ERR-HAS
   \ A later file has its own include frame: it sees the first file's using
   \ but may not close it.
   [: s" --build" GE-ARG+ s" pkg.f" FIX GE-ARG+ s" use-pkg.f" FIX GE-ARG+ ;]
   s" build, a later file closes a using" BOTH
   104 s" build, a later file closes a using" GE-EXPECT-RC
   S\" H\n" s" build, a later file closes a using" GE-EXPECT-OUT
   s" hb: ;using would close a using opened outside the file" s" build, a later file closes a using" GE-EXPECT-ERR-HAS ;

\ ---- the program file ------------------------------------------------------------
: PROGRAM-FILE ( -- )
   [: s" a.f" FIX GE-ARG+ s" b.f" FIX GE-ARG+ ;] s" program" BOTH
   S\" A\n" s" program" OUT
   [: s" sh.f" FIX GE-ARG+ ;] s" program, a shebang" BOTH
   S\" SH\n" s" program, a shebang" OUT
   [: s" bang.f" FIX GE-ARG+ ;] s" program, only #!" BOTH
   s" " s" program, only #!" OUT
   [: s" hash.f" FIX GE-ARG+ ;] s" program, # without !" BOTH
   70 S\" E-UNDEFINED: #x\n" s" program, # without !" REFUSED
   [: s" self.f" FIX GE-ARG+ ;] s" program records the file" BOTH
   S\" S\n" s" program records the file" OUT
   [: s" bad.f" FIX GE-ARG+ ;] s" program, a refusal" BOTH
   70 S\" E-UNDEFINED: nosuchword\n" s" program, a refusal" REFUSED
   [: s" missing.f" FIX GE-ARG+ ;] s" program, a missing file" BOTH
   74 s" missing.f" FIX CANNOT-OPEN$ s" program, a missing file" REFUSED
   \ argv[1] `--` is the program file's name.
   [: s" --" GE-ARG+ s" a.f" FIX GE-ARG+ ;] s" -- first" BOTH
   74 s" --" CANNOT-OPEN$ s" -- first" REFUSED ;

\ ---- unknown flags ---------------------------------------------------------------
: UNKNOWN-FLAGS ( -- )
   [: s" --bogus" GE-ARG+ s" a.f" FIX GE-ARG+ ;] s" unknown flag" BOTH
   64 s" --bogus" USAGE$ s" unknown flag" REFUSED
   [: s" -" GE-ARG+ ;] s" a lone dash" BOTH
   64 s" -" USAGE$ s" a lone dash" REFUSED
   [: s" --loadx" GE-ARG+ s" a.f" FIX GE-ARG+ ;] s" a flag's prefix" BOTH
   64 s" --loadx" USAGE$ s" a flag's prefix" REFUSED ;

\ ---- RUN's alone --------------------------------------------------------------------
\ No file before the end or a `--`: the engine says its arena is full.
: EMPTY ( [ -- ] ptr u8 n -- ) {: args label:ptr labelu:n :}
   args ENGINE
   74 label labelu GE-EXPECT-RC
   args HABU
   74 S\" hb: no source files\n" label labelu REFUSED
   1 CASES +! ;

: EMPTY-LISTS ( -- )
   [: s" --load" GE-ARG+ ;] s" load, no files" EMPTY
   [: s" --load" GE-ARG+ s" --" GE-ARG+ s" a.f" FIX GE-ARG+ ;] s" load, -- first" EMPTY
   [: s" --build" GE-ARG+ ;] s" build, no files" EMPTY ;

\ The --build stream holds the file's `s" <path>" provided` row and its bytes.
\ A file that leaves one byte to spare, for the read that finds its end, runs;
\ one more and it is refused before it runs.
TYPED-VARIABLE BLANKS-A ptr u8

: BLANKS ( -- ptr u8 )
   BLANKS-A @ ;

: ROW-LEN ( -- n )
   s" big.f" FIX nip S\" s\" \" provided\n" nip + ;

: BIG ( n -- ) {: u:n :}
   s" big.f" FIX BLANKS u WRITE-ALL ;

: BUILD-BIG ( -- )
   s" --build" GE-ARG+ s" big.f" FIX GE-ARG+ ;

: ARENA-BOUND ( -- )
   SOURCE-ARENA-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BLANKS-A !
   SOURCE-ARENA-CAP 0 ?do 32 BLANKS i + c! loop
   SOURCE-ARENA-CAP ROW-LEN - 1 - BIG
   [: BUILD-BIG ;] s" arena, one byte to spare" BOTH
   s" " s" arena, one byte to spare" OUT
   SOURCE-ARENA-CAP ROW-LEN - BIG
   [: BUILD-BIG ;] ENGINE
   74 s" arena, full" GE-EXPECT-RC
   [: BUILD-BIG ;] HABU
   74 S\" hb: source prefix buffer full\n" s" arena, full" REFUSED
   1 CASES +! ;

: MAIN ( -- )
   0 CASES !
   s" habu-main-argv" GT-START
   FIXTURES
   LOAD-FILES
   BUILD-FILES
   PROGRAM-FILE
   UNKNOWN-FLAGS
   EMPTY-LISTS
   ARENA-BOUND
   GT-CLEANUP
   s" main-argv: " type CASES @ FMT:.INT s"  cases pass" type cr ;

MAIN

;package
