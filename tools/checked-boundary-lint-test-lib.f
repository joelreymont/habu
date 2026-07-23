\ checked-boundary-lint-test.f - checked fixtures for tools/checked-boundary-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/cli-run.f tools/checked-boundary-lint-core.f
\ tools/checked-boundary-lint-test.f

package CBLT

4096 constant BUF-CAP
1400 constant LARGE-LINES

variable ROOT-U
variable GOOD-U
variable BAD-U
variable NOPREF-U
variable OFF-U
variable CROSS-U
variable LARGE-U
variable TRUSTED-U
variable ROGUE-U
variable TOPROGUE-U
variable TOPGOOD-U

create ROOT-BUF FS-PATH-CAP allot
create GOOD-BUF FS-PATH-CAP allot
create BAD-BUF FS-PATH-CAP allot
create NOPREF-BUF FS-PATH-CAP allot
create OFF-BUF FS-PATH-CAP allot
create CROSS-BUF FS-PATH-CAP allot
create LARGE-BUF FS-PATH-CAP allot
create TRUSTED-BUF FS-PATH-CAP allot
create ROGUE-BUF FS-PATH-CAP allot
create TOPROGUE-BUF FS-PATH-CAP allot
create TOPGOOD-BUF FS-PATH-CAP allot
create OUT BUF-CAP allot
create LF-BYTE 10 c,

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: GOOD ( -- ptr u8 n )
   GOOD-BUF GOOD-U @ ;

: BAD ( -- ptr u8 n )
   BAD-BUF BAD-U @ ;

: NOPREF ( -- ptr u8 n )
   NOPREF-BUF NOPREF-U @ ;

: OFF ( -- ptr u8 n )
   OFF-BUF OFF-U @ ;

: CROSS ( -- ptr u8 n )
   CROSS-BUF CROSS-U @ ;

: LARGE ( -- ptr u8 n )
   LARGE-BUF LARGE-U @ ;

: TRUSTED ( -- ptr u8 n )
   TRUSTED-BUF TRUSTED-U @ ;

: ROGUE ( -- ptr u8 n )
   ROGUE-BUF ROGUE-U @ ;

: TOPROGUE ( -- ptr u8 n )
   TOPROGUE-BUF TOPROGUE-U @ ;

: TOPGOOD ( -- ptr u8 n )
   TOPGOOD-BUF TOPGOOD-U @ ;

: LF ( -- )
   10 SB-APPEND-C ;

: APPEND-LF ( ptr u8 n -- )
   LF-BYTE 1 APPEND-FILE ;

: GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND LF
   s" variable RAW-CELL" SB-APPEND LF
   s" : LINT-CHECK-HOOK ( -- ) CHECK! ;" SB-APPEND LF
   s" LOWER-CERT-HOOK:INSTALL" SB-APPEND LF
   s" ' LINT-CHECK-HOOK set-check" SB-APPEND LF
   s" : GOOD ( n -- n ) dup ;" SB-APPEND LF
   SB$ ;

: NOPREF$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND LF
   s" TRUSTED: PREP ( -- ) LOWER-CERT-HOOK:INSTALL ;" SB-APPEND LF
   s" ' LINT-CHECK-HOOK set-check" SB-APPEND LF
   s" : SHOULD-STAY-UNCHECKED ( n -- n ) dup ;" SB-APPEND LF
   s" 0 set-check" SB-APPEND LF
   s" KERNEL: PREP-K ( -- ) LOWER-CERT-HOOK:INSTALL ;" SB-APPEND LF
   s" ' LINT-CHECK-HOOK set-check" SB-APPEND LF
   s" : KERNEL-STAY-UNCHECKED ( n -- n ) dup ;" SB-APPEND LF
   s" 0 set-check" SB-APPEND LF
   s" CHECKED: PREP-C ( -- ) LOWER-CERT-HOOK:INSTALL ;" SB-APPEND LF
   s" ' LINT-CHECK-HOOK set-check" SB-APPEND LF
   s" : CHECKED-STAY-UNCHECKED ( n -- n ) dup ;" SB-APPEND LF
   s" 0 set-check" SB-APPEND LF
   s" +: PREP-P ( -- ) LOWER-CERT-HOOK:INSTALL ;" SB-APPEND LF
   s" ' LINT-CHECK-HOOK set-check" SB-APPEND LF
   s" : PLUS-STAY-UNCHECKED ( n -- n ) dup ;" SB-APPEND LF
   s" 0 set-check" SB-APPEND LF
   s" :noname ( -- ) LOWER-CERT-HOOK:INSTALL ; drop" SB-APPEND LF
   s" ' LINT-CHECK-HOOK set-check" SB-APPEND LF
   s" : ANON-STAY-UNCHECKED ( n -- n ) dup ;" SB-APPEND LF
   s" LOWER-CERT-HOOK:INSTALL" SB-APPEND LF
   s" TRUSTED: NEVER-RUN ( -- ) 0 set-check ;" SB-APPEND LF
   s" : BODY-DISABLE-NO-EFFECT ( n -- n ) dup ;" SB-APPEND LF
   SB$ ;

: ROGUE$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND LF
   s" ' EVIL-HOOK set-check" SB-APPEND LF
   s" : ROGUE-OK ( n -- n ) dup ;" SB-APPEND LF
   SB$ ;

\ Tier-2 escape-window audit rows (dot habu-typed-top-tier-589c550f): a
\ set-top-check install of any name but TR-HOOK is UNAUDITED-TOP-HOOK; the TR-HOOK
\ install (top-row.f TR-INSTALL) is the sole allowed enforcer and stays clean.
: TOPROGUE$ ( -- ptr u8 n )
   SB-RESET
   s" ['] EVIL-TOP-HOOK set-top-check" SB-APPEND LF
   SB$ ;

: TOPGOOD$ ( -- ptr u8 n )
   SB-RESET
   s" ['] TR-HOOK set-top-check" SB-APPEND LF
   SB$ ;

: BAD$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND LF
   s" : BAD ( n -- n ) dup ;" SB-APPEND LF
   SB$ ;

: OFF$ ( -- ptr u8 n )
   s" 0 set-check" ;

: CROSS$ ( -- ptr u8 n )
   s" : CROSS-BAD ( n -- n ) dup ;" ;

: TRUSTED$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: USER-HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> if 70 throw then ;" SB-APPEND LF
   s" TRUSTED: INSTALL-HOOK ( -- ) ['] USER-HOOK set-check ;" SB-APPEND LF
   s" INSTALL-HOOK" SB-APPEND LF
   s" : GOOD ( n -- n ) dup ;" SB-APPEND LF
   SB$ ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: LARGE-LINE$ ( -- ptr u8 n )
   s" \\ boundary lint large fixture padding line 0123456789 abcdefghijklmnopqrstuvwxyz" ;

: WRITE-LARGE ( -- )
   LARGE s" : LARGE-OK ( n -- n ) dup ;" WRITE-ALL
   LARGE APPEND-LF
   LARGE-LINES 0 ?do
      LARGE LARGE-LINE$ APPEND-FILE
      LARGE APPEND-LF
   loop ;

: CODE$ ( -- ptr u8 n )
   s" UNCHECKED-DEFINITION" ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-checked-boundary-lint" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-DIR+
   ROOT s" good.f" GOOD-BUF JOIN-PATH GOOD-U !
   ROOT s" bad.f" BAD-BUF JOIN-PATH BAD-U !
   ROOT s" no-preflight.f" NOPREF-BUF JOIN-PATH NOPREF-U !
   ROOT s" off.f" OFF-BUF JOIN-PATH OFF-U !
   ROOT s" cross.f" CROSS-BUF JOIN-PATH CROSS-U !
   ROOT s" large.f" LARGE-BUF JOIN-PATH LARGE-U !
   ROOT s" trusted.f" TRUSTED-BUF JOIN-PATH TRUSTED-U !
   ROOT s" rogue.f" ROGUE-BUF JOIN-PATH ROGUE-U !
   ROOT s" toprogue.f" TOPROGUE-BUF JOIN-PATH TOPROGUE-U !
   ROOT s" topgood.f" TOPGOOD-BUF JOIN-PATH TOPGOOD-U !
   GOOD CLEANUP+
   BAD CLEANUP+
   NOPREF CLEANUP+
   OFF CLEANUP+
   CROSS CLEANUP+
   LARGE CLEANUP+
   TRUSTED CLEANUP+
   ROGUE CLEANUP+
   TOPROGUE CLEANUP+
   TOPGOOD CLEANUP+
   GOOD GOOD$ WRITE-ALL
   BAD BAD$ WRITE-ALL
   NOPREF NOPREF$ WRITE-ALL
   OFF OFF$ WRITE-ALL
   CROSS CROSS$ WRITE-ALL
   TRUSTED TRUSTED$ WRITE-ALL
   ROGUE ROGUE$ WRITE-ALL
   TOPROGUE TOPROGUE$ WRITE-ALL
   TOPGOOD TOPGOOD$ WRITE-ALL
   WRITE-LARGE ;

: CORE-SETUP ( bool -- ) {: strict:bool :}
   CHECKED-BOUNDARY-LINT-RESET
   OUT BUF-CAP LINT-OUT-BUFFER!
   strict UB-STRICT-BOUNDARY! ;

: CORE-FINISH ( -- n n outcome )
   [: CHECKED-BOUNDARY-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc OUTCOME:EXITED ;

: RUN-CURRENT ( -- n n outcome )
   LINT-FALSE CORE-SETUP
   s" tools/checked-boundary-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/json-file.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/host-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/filemap-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/parallel-agent-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/checked-boundary-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/signature-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/signature-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/typed-local-diff-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/typed-local-diff-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/typed-local-diff-lint-test.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/stale-status-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/stale-status-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/trust-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/trust-lint.f" CHECKED-BOUNDARY-LINT-FILE
   CORE-FINISH ;

: RUN-CORE-FILE ( ptr u8 n bool -- n n outcome ) {: path:ptr pathu:n strict:bool :}
   strict CORE-SETUP
   path pathu CHECKED-BOUNDARY-LINT-FILE
   CORE-FINISH ;

: RUN-CORE-GOOD ( -- n n outcome )
   GOOD LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-LARGE ( -- n n outcome )
   LARGE LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-BAD ( -- n n outcome )
   BAD LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-NOPREF ( -- n n outcome )
   NOPREF LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-STRICT-GOOD ( -- n n outcome )
   GOOD LINT-TRUE RUN-CORE-FILE ;

: RUN-CORE-STRICT-TRUSTED ( -- n n outcome )
   TRUSTED LINT-TRUE RUN-CORE-FILE ;

: RUN-CORE-ROGUE ( -- n n outcome )
   ROGUE LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-TOPROGUE ( -- n n outcome )
   TOPROGUE LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-TOPGOOD ( -- n n outcome )
   TOPGOOD LINT-FALSE RUN-CORE-FILE ;

: RUN-CORE-CROSS ( -- n n outcome )
   LINT-FALSE CORE-SETUP
   OFF CHECKED-BOUNDARY-LINT-FILE
   CROSS CHECKED-BOUNDARY-LINT-FILE
   CORE-FINISH ;

: ASSERT-CLEAN ( n n outcome -- )
   0 T-OUTCOME-EXITED= {: outu:n erru:n :}
   OUT outu EMPTY$ T$=
   erru 0 T= ;

: EXPECT-EXIT ( n n outcome n -- n n ) {: expect:n :}
   expect T-OUTCOME-EXITED= ;

: TEST-CURRENT ( -- )
   RUN-CURRENT ASSERT-CLEAN ;

: TEST-GOOD ( -- )
   RUN-CORE-GOOD ASSERT-CLEAN ;

: TEST-LARGE ( -- )
   RUN-CORE-LARGE ASSERT-CLEAN ;

: TEST-BAD ( -- )
   RUN-CORE-BAD 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu CODE$ CONTAINS? TTRUE ;

: TEST-NOPREF ( -- )
   RUN-CORE-NOPREF 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" MISSING-PREFLIGHT-REARM" CONTAINS? TTRUE
   OUT outu s" SHOULD-STAY-UNCHECKED" CONTAINS? TTRUE
   OUT outu s" KERNEL-STAY-UNCHECKED" CONTAINS? TTRUE
   OUT outu s" CHECKED-STAY-UNCHECKED" CONTAINS? TTRUE
   OUT outu s" PLUS-STAY-UNCHECKED" CONTAINS? TTRUE
   OUT outu s" ANON-STAY-UNCHECKED" CONTAINS? TTRUE
   OUT outu s" BODY-DISABLE-NO-EFFECT" CONTAINS? TFALSE ;

: TEST-CROSS ( -- )
   RUN-CORE-CROSS 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" CROSS-BAD" CONTAINS? TTRUE ;

: TEST-STRICT ( -- )
   RUN-CORE-STRICT-GOOD 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" CHECKER-MUTATION" CONTAINS? TTRUE
   OUT outu s" set-check" CONTAINS? TTRUE ;

: TEST-STRICT-TRUSTED ( -- )
   RUN-CORE-STRICT-TRUSTED ASSERT-CLEAN ;

: TEST-ROGUE ( -- )
   RUN-CORE-ROGUE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" UNAUDITED-HOOK" CONTAINS? TTRUE
   OUT outu s" EVIL-HOOK" CONTAINS? TTRUE ;

: TEST-TOPROGUE ( -- )                 \ ['] EVIL-TOP-HOOK set-top-check -> finding
   RUN-CORE-TOPROGUE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" UNAUDITED-TOP-HOOK" CONTAINS? TTRUE
   OUT outu s" EVIL-TOP-HOOK" CONTAINS? TTRUE ;

: TEST-TOPGOOD ( -- )                  \ ['] TR-HOOK set-top-check -> allowed, clean
   RUN-CORE-TOPGOOD ASSERT-CLEAN ;

public

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-CURRENT
   TEST-GOOD
   TEST-LARGE
   TEST-BAD
   TEST-NOPREF
   TEST-CROSS
   TEST-STRICT
   TEST-STRICT-TRUSTED
   TEST-ROGUE
   TEST-TOPROGUE
   TEST-TOPGOOD
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" checked-boundary-lint-test: ok" type cr ;

;package
