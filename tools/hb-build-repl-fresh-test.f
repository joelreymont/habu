\ hb-build-repl-fresh-test.f - the real CLI reuses a fresh REPL image, also
\ under --size-report, and rebuilds after an embedded asset or a dynamic child
\ changes, a nearer CWD file shadows a required one, or the output is deleted.
\ A deleted asset fails the build.
\ Run: bin/hb --load tools/hb-build-repl-fresh-test.f

require tools/hb-build-test-lib.f
require test/app-builder-engine.f
require src/lib/string.f
require src/lib/fmt.f

package HB-BUILD-CLI

variable HBT-FRESH-DEP-U
variable HBT-FRESH-ASSET-U
variable HBT-FRESH-OTHER-U
variable HBT-FRESH-SHADOW-FILE-U
create HBT-FRESH-DEP-BUF PATH-CAP allot
create HBT-FRESH-ASSET-BUF PATH-CAP allot
create HBT-FRESH-OTHER-BUF PATH-CAP allot
create HBT-FRESH-SHADOW-FILE-BUF PATH-CAP allot
variable HBT-FRESH-SIZE
variable HBT-FRESH-SEC
variable HBT-FRESH-NS

: HBT-FRESH-DEP ( -- ptr u8 n )
   HBT-FRESH-DEP-BUF HBT-FRESH-DEP-U @ ;

: HBT-FRESH-ASSET-FILE ( -- ptr u8 n )
   HBT-FRESH-ASSET-BUF HBT-FRESH-ASSET-U @ ;

: HBT-FRESH-OTHER ( -- ptr u8 n )
   HBT-FRESH-OTHER-BUF HBT-FRESH-OTHER-U @ ;

: HBT-FRESH-SHADOW-FILE ( -- ptr u8 n )
   HBT-FRESH-SHADOW-FILE-BUF HBT-FRESH-SHADOW-FILE-U @ ;

: HBT-FRESH-SHADOW-SOURCE$ ( -- ptr u8 n )
   S\" : CHECK-ARGS ( -- ) SCRIPT-ARGC 2 <> if -9001 throw then ; CHECK-ARGS\n: LOAD-SHADOW ( -- ) s\" tools/hb-build-fresh-shadow.f\" required ;\nLOAD-SHADOW\n: MAIN ( -- ) HBT-FRESH:VALUE . cr ;\n" ;

\ The application embeds an asset and loads a dynamic child, and prints both.
: HBT-FRESH-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : CHECK-ARGS ( -- ) SCRIPT-ARGC 2 <> if -9001 throw then ; CHECK-ARGS"
      SB-APPEND HBB-LF SB-APPEND-C
   S\" require src/lib/fs.f\nrequire src/lib/build-input.f\n4 BUFFER: CONTENT\n: LOAD-ASSET ( -- ) s\" " SB-APPEND
   HBT-FRESH-ASSET-FILE SB-APPEND
   S\" \" 2dup BUILD-INPUT:FILE CONTENT 4 FS:READ-ALL 4 <> if E-FS-IO throw then ;\nLOAD-ASSET\n: LOAD-CHILD ( -- ) s\" " SB-APPEND
   HBT-FRESH-DEP SB-APPEND
   S\" \" required ;\nLOAD-CHILD\n: MAIN ( -- ) CONTENT 4 type HBT-FRESH:VALUE . cr ;\n" SB-APPEND
   SB$ ;

: HBT-FRESH-DEP! ( n -- )
   {: value:n :}
   SB-RESET
   s" package HBT-FRESH public : VALUE ( -- n ) " SB-APPEND
   value FMT:SB-U
   s"  ; ;package" SB-APPEND HBB-LF SB-APPEND-C
   HBT-FRESH-DEP SB$ FS:WRITE-ALL ;

: HBT-FRESH-MARK ( -- )
   HBT-REPL-OUT FS:FILE-META
   {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size HBT-FRESH-SIZE ! sec HBT-FRESH-SEC ! ns HBT-FRESH-NS ! ;

: HBT-FRESH-SAME? ( -- bool )
   HBT-REPL-OUT FS:FILE-META
   {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size HBT-FRESH-SIZE @ = sec HBT-FRESH-SEC @ = and
   ns HBT-FRESH-NS @ = and ;

: HBT-FRESH-CLI-SOURCE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n :}
   HBT-TMP ENGINE-CANDIDATE:PATH$ HBT-ARGV-CLI
   s" --repl" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outn:n errn:n rc:n :}
   rc 0 <> if HBT-OUT outn type HBT-ERR errn type then
   rc 0 T= errn 0 T=
   HBT-OUT outn s" hb-build size:" CONTAINS? TFALSE ;

: HBT-FRESH-CLI ( ptr u8 n -- )
   {: out:ptr outu:n :}
   HBT-REPL-SRC out outu HBT-FRESH-CLI-SOURCE ;

: HBT-FRESH-SIZE-REPORT ( -- )
   HBT-ARGV-BASE-REPL
   s" --repl" >LEN PROC-ARGV+
   s" --size-report" >LEN PROC-ARGV+
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outn:n errn:n rc:n :}
   rc 0 <> if HBT-OUT outn type HBT-ERR errn type then
   rc 0 T= errn 0 T=
   HBT-OUT outn s" hb-build size:" CONTAINS? TTRUE ;

: HBT-FRESH-RUN$ ( ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n want:ptr wantu:n :}
   PROC-ARGV-RESET
   out outu >LEN
   HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N
   {: outn:n errn:n rc:n :}
   rc 0 T= errn 0 T=
   HBT-RUN-OUT outn want wantu CONTAINS? TTRUE ;

: HBT-FRESH-RUN ( ptr u8 n n -- )
   {: out:ptr outu:n expected:n :}
   SB-RESET expected FMT:SB-U
   out outu SB$ HBT-FRESH-RUN$ ;

\ The application's output: its asset's bytes, then its child's value.
: HBT-FRESH-RUN-BOTH ( ptr u8 n n -- )
   {: asset:ptr assetu:n expected:n :}
   SB-RESET asset assetu SB-APPEND expected FMT:SB-U
   HBT-REPL-OUT SB$ HBT-FRESH-RUN$ ;

\ Runs while the output is fresh for the unchanged source, so only the deleted
\ asset can make the build run, and the build refuses it.
: HBT-FRESH-ASSET-GONE ( -- )
   s" a deleted asset fails the build" T-LABEL
   HBT-FRESH-ASSET-FILE FS:REMOVE-FILE
   HBT-ARGV-BASE-REPL
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outn:n errn:n rc:n :}
   outn drop errn drop
   rc HBB-UNCAUGHT-RC T= ;

public

: HBT-FRESH-MAIN ( -- )
   T-RESET
   NULL$ HBT-KEYED!
   APP-BUILDER-ENGINE:ENSURE
   HBT-PREPARE
   HBT-ROOT s" child.f" HBT-FRESH-DEP-BUF HBT-FRESH-DEP-U HBT-PATH!
   HBT-ROOT s" asset.txt" HBT-FRESH-ASSET-BUF HBT-FRESH-ASSET-U HBT-PATH!
   HBT-ROOT s" other.f" HBT-FRESH-OTHER-BUF HBT-FRESH-OTHER-U HBT-PATH!
   HBT-ROOT s" tools/hb-build-fresh-shadow.f"
      HBT-FRESH-SHADOW-FILE-BUF HBT-FRESH-SHADOW-FILE-U HBT-PATH!
   s" asset and dynamic child freshness" T-LABEL
   HBT-FRESH-ASSET-FILE s" AAAA" FS:WRITE-ALL
   111 HBT-FRESH-DEP!
   HBT-REPL-SRC HBT-FRESH-SOURCE$ FS:WRITE-ALL
   HBT-REPL-OUT HBT-FRESH-CLI
   s" AAAA" 111 HBT-FRESH-RUN-BOTH
   HBT-FRESH-MARK
   \ Another process can date the shared keyed builder while this output is fresh.
   APP-BUILDER-ENGINE:PATH$ 2drop
   HBT-REPL-OUT HBT-FRESH-CLI
   HBT-FRESH-SAME? TTRUE
   HBT-FRESH-SIZE-REPORT
   HBT-FRESH-SAME? TTRUE
   HBT-FRESH-ASSET-FILE s" BBBB" FS:WRITE-ALL
   HBT-REPL-OUT HBT-FRESH-CLI
   s" BBBB" 111 HBT-FRESH-RUN-BOTH
   222 HBT-FRESH-DEP!
   HBT-REPL-OUT HBT-FRESH-CLI
   s" BBBB" 222 HBT-FRESH-RUN-BOTH
   HBT-FRESH-ASSET-GONE
   HBT-FRESH-OTHER s" : MAIN ( -- ) 333 . cr ;" FS:WRITE-ALL
   HBT-FRESH-OTHER HBT-REPL-OUT HBT-FRESH-CLI-SOURCE
   HBT-REPL-OUT 333 HBT-FRESH-RUN
   HBT-REPL-SRC HBT-FRESH-SHADOW-SOURCE$ FS:WRITE-ALL
   HBT-REPL-OUT HBT-FRESH-CLI
   HBT-REPL-OUT 444 HBT-FRESH-RUN
   HBT-FRESH-SHADOW-FILE SOURCE-ROOT:DIRNAME FS:MAKE-DIR
   HBT-FRESH-SHADOW-FILE
      s" package HBT-FRESH public : VALUE ( -- n ) 555 ; ;package" FS:WRITE-ALL
   HBT-REPL-OUT HBT-FRESH-CLI
   HBT-REPL-OUT 555 HBT-FRESH-RUN
   HBT-REPL-OUT FS:REMOVE-FILE
   HBT-FRESH-OTHER HBT-REPL-OUT HBT-FRESH-CLI-SOURCE
   HBT-REPL-OUT 333 HBT-FRESH-RUN
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-repl-fresh-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-FRESH-MAIN
