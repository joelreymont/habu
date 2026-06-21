\ gate-debug.f - checked runner for prop/snapshot/debug gate checks.
\
\ Load after test/gate-common.f and lib/codesign.f.

create GDB-PATH1 FS-PATH-CAP allot
create GDB-PATH2 FS-PATH-CAP allot

variable GDB-PATH1-U
variable GDB-PATH2-U

: GDB-PATH1! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GDB-PATH1 GT-PATH GDB-PATH1-U ! ;

: GDB-PATH2! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GDB-PATH2 GT-PATH GDB-PATH2-U ! ;

: GDB-PATH1$ ( -- ptr u8 n )
   GDB-PATH1 GDB-PATH1-U @ ;

: GDB-PATH2$ ( -- ptr u8 n )
   GDB-PATH2 GDB-PATH2-U @ ;

: GDB-SNAP0! ( -- )
   s" hb-snap0" GDB-PATH1! ;

: GDB-SNAP0$ ( -- ptr u8 n )
   GDB-PATH1$ ;

: GDB-REMOVE-FILE? ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? if path pathu REMOVE-FILE then ;

: GDB-EXPECT-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu label:ptr labelu :}
   path pathu FILE? 0= if label labelu GE-FAIL then ;

: GDB-HB-TMP-ENV ( -- )
   s" HB_TMP" GT-ROOT PROC-ENV+ ;

: GDB-PROP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" test/prop-test.f" GE-SRC-FILE+
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" prop-test" GE-EXPECT-OK
   s" self-test OK" s" prop-test self-test/run did not complete" GE-EXPECT-OUT-HAS
   s" PASS: prop-test soundness smoke (self-hosted in habu, in-process via evaluate)" type cr ;

: GDB-SNAPSHOT-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-HB-RESET
   GDB-HB-TMP-ENV
   GDB-SNAP0!
   GDB-SNAP0$ GDB-REMOVE-FILE?
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   GDB-SNAP0$ label labelu GDB-EXPECT-FILE ;

: GDB-SNAPSHOT-REFRESH ( -- )
   GE-SRC-RESET
   s" src/habu/snap.f" GE-SRC-FILE+
   s" HB_TMP isolation" GDB-SNAPSHOT-RUN
   GDB-SNAP0$ s" bin/hb" PROMOTE-SIGNED-EXECUTABLE
   GE-CLEAN-BIN ;

: GDB-HOOK-SOURCE ( -- )
   GE-SRC-RESET
   s" DATA-VA $1B0 + @ 0= ." GE-SRC-LINE
   s" : SQOK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" 7 SQOK ." GE-SRC-LINE ;

: GDB-HOOK-CHECK ( -- )
   GE-HB-RESET
   GDB-HOOK-SOURCE
   s" hb refresh/check hook" GE-HB-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE
   s" 49" GE-OUT-LINE
   SB$ s" hb refresh/check hook output" GE-EXPECT-OUT ;

: GDB-LONG-SNAPSHOT-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-SNAPSHOT-DICTIONARY-WORD ( i64 -- i64 ) 3 + ;" GE-SRC-LINE
   s" src/habu/snap.f" GE-SRC-FILE+ ;

: GDB-LONG-SNAPSHOT-BUILD ( -- )
   GDB-LONG-SNAPSHOT-SOURCE
   s" long-name snapshot write" GDB-SNAPSHOT-RUN
   GDB-SNAP0$ CODESIGN-FORCE
   GDB-SNAP0$ CHMOD-X ;

: GDB-LONG-SNAPSHOT-RUN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" 39 LONG-SNAPSHOT-DICTIONARY-WORD ." GE-SRC-LINE
   GDB-SNAP0$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" long-name snapshot restore" GE-EXPECT-OK
   SB-RESET
   s" 42" GE-OUT-LINE
   SB$ s" long-name snapshot restore output" GE-EXPECT-OUT ;

: GDB-SNAPSHOT ( -- )
   GDB-SNAPSHOT-REFRESH
   GDB-HOOK-CHECK
   GDB-LONG-SNAPSHOT-BUILD
   GDB-LONG-SNAPSHOT-RUN
   s" PASS: HB_TMP isolation" type cr ;

: GDB-PTY ( -- )
   GE-HB-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" test/proc-pty.f" PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" process/pty" GE-EXPECT-OK
   s" PASS: process/pty primitives" s" process/pty output" GE-EXPECT-OUT-HAS
   s" PASS: process/pty primitives" type cr ;

: GDB-PROFILER-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-PROFILER-BUSY-WORD ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" : GO ( -- ) 100000 prof-on LONG-PROFILER-BUSY-WORD prof-report ;" GE-SRC-LINE
   s" GO" GE-SRC-LINE ;

: GDB-PROFILER ( -- )
   GE-HB-RESET
   GDB-PROFILER-SOURCE
   s" profiler long dictionary names" GE-HB-RUN-STDIN
   GT-OUT$ s" LONG-PROFILER-BUSY-WORD " STARTS-WITH? 0= if
      s" profiler long-name output" GE-FAIL
   then
   s" PASS: profiler long dictionary names" type cr ;

: GDB-JITDUMP ( -- )
   GE-HB-RESET
   s" --load" PROC-ARGV+
   s" src/arch/arm64/disasm.f" PROC-ARGV+
   s" tools/jitdump.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" : JITDUMP-SMOKE ( -- i64 ) 7 ;" PROC-ARGV+
   s" JITDUMP-SMOKE" PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" jitdump direct CLI" GE-EXPECT-OK
   s" ret" s" jitdump direct CLI output" GE-EXPECT-OUT-HAS
   s" PASS: jitdump direct CLI" type cr ;

: GDB-RUN ( -- )
   s" hb-gate-debug" GT-START
   GDB-PROP
   GDB-SNAPSHOT
   GDB-PTY
   GDB-PROFILER
   GDB-JITDUMP
   GT-CLEANUP
   s" PASS: native prop/snapshot/debug gate phase" type cr ;

GDB-RUN
