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
variable EMPTY-U

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
create EMPTY-BUF FS-PATH-CAP allot
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

: EMPTY ( -- ptr u8 n )
   EMPTY-BUF EMPTY-U @ ;

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
   ROOT s" empty.f" EMPTY-BUF JOIN-PATH EMPTY-U !
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
   EMPTY CLEANUP+
   GOOD GOOD$ WRITE-ALL
   BAD BAD$ WRITE-ALL
   NOPREF NOPREF$ WRITE-ALL
   OFF OFF$ WRITE-ALL
   CROSS CROSS$ WRITE-ALL
   TRUSTED TRUSTED$ WRITE-ALL
   ROGUE ROGUE$ WRITE-ALL
   TOPROGUE TOPROGUE$ WRITE-ALL
   TOPGOOD TOPGOOD$ WRITE-ALL
   EMPTY EMPTY$ WRITE-ALL
   WRITE-LARGE ;

: CORE-SETUP ( bool -- ) {: strict:bool :}
   CHECKED-BOUNDARY-LINT:RESET
   OUT BUF-CAP LINT-OUT-BUFFER!
   strict CHECKED-BOUNDARY-LINT:STRICT! ;

: CORE-FINISH ( -- n n outcome )
   [: CHECKED-BOUNDARY-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc OUTCOME:EXITED ;

: RUN-CURRENT ( -- n n outcome )
   LINT-FALSE CORE-SETUP
   s" tools/checked-boundary-lint.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/json-file.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/host-lint.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/checked-boundary-lint-core.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/signature-lint-core.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/signature-lint.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/typed-local-diff-lint-core.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/typed-local-diff-lint.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/typed-local-diff-lint-test.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/stale-status-lint-core.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/stale-status-lint.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/trust-lint-core.f" CHECKED-BOUNDARY-LINT:FILE
   s" tools/trust-lint.f" CHECKED-BOUNDARY-LINT:FILE
   CORE-FINISH ;

: RUN-CORE-FILE ( ptr u8 n bool -- n n outcome ) {: path:ptr pathu:n strict:bool :}
   strict CORE-SETUP
   path pathu CHECKED-BOUNDARY-LINT:FILE
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
   OFF CHECKED-BOUNDARY-LINT:FILE
   CROSS CHECKED-BOUNDARY-LINT:FILE
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

\ The public FILE wrapper propagates a scan failure and remains reusable.
: TEST-MAP-THROW ( -- )
   CHECKED-BOUNDARY-LINT:RESET
   OUT 1 LINT-OUT-BUFFER!
   LINT-FALSE CHECKED-BOUNDARY-LINT:STRICT!
   [: BAD CHECKED-BOUNDARY-LINT:FILE ;] catch
   LINT-OUT-BUFFER-OFF
   E-STR-CAPACITY T=
   [: GOOD CHECKED-BOUNDARY-LINT:FILE ;] catch 0 T=
   CHECKED-BOUNDARY-LINT:RESET ;

;package

\ The lifecycle probes run inside the provider package so they can observe
\ mapped spans before MEM:WITH-BYTES releases the allocation. They add no
\ public test bridge or production hook.
package CHECKED-BOUNDARY-LINT

private

create TP-ROOT-BUF FS-PATH-CAP allot
create TP-GOOD-BUF FS-PATH-CAP allot
create TP-BAD-BUF FS-PATH-CAP allot
create TP-EMPTY-BUF FS-PATH-CAP allot
create TP-OUT 1 allot

variable TP-ROOT-U
variable TP-GOOD-U
variable TP-BAD-U
variable TP-EMPTY-U
variable TP-PUB-WID
variable TP-PRIV-WID

: TP-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: TP-ROOT ( -- ptr u8 n )
   TP-ROOT-BUF TP-ROOT-U @ ;

: TP-GOOD ( -- ptr u8 n )
   TP-GOOD-BUF TP-GOOD-U @ ;

: TP-BAD ( -- ptr u8 n )
   TP-BAD-BUF TP-BAD-U @ ;

: TP-EMPTY ( -- ptr u8 n )
   TP-EMPTY-BUF TP-EMPTY-U @ ;

: TP-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-cbl-map" TMPDIR-MKDIR {: a:ptr u:n :}
   a u TP-ROOT-BUF TP-ROOT-U TP-COPY!
   TP-ROOT CLEANUP-TREE+
   TP-ROOT s" good.f" TP-GOOD-BUF JOIN-PATH TP-GOOD-U !
   TP-ROOT s" bad.f" TP-BAD-BUF JOIN-PATH TP-BAD-U !
   TP-ROOT s" empty.f" TP-EMPTY-BUF JOIN-PATH TP-EMPTY-U !
   TP-GOOD s\" 0 set-check\nvariable TP-RAW\n: LINT-CHECK-HOOK ( -- ) CHECK! ;\nLOWER-CERT-HOOK:INSTALL\n' LINT-CHECK-HOOK set-check\n: TP-GOOD ( n -- n ) dup ;\n" WRITE-ALL
   TP-BAD s\" 0 set-check\n: TP-BAD ( n -- n ) dup ;\n" WRITE-ALL
   SB-RESET
   TP-EMPTY SB$ WRITE-ALL ;

: TP-FILE! ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path UB-FILE-A! pathu UB-FILE-U ! ;

: TP-MAP-OK-ACT ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   UB-MAPPED-FILE
   UB-MAPPED-SPANS-CLEAR? TTRUE ;

: TP-MAP-OK-TEST ( -- )
   RESET
   LINT-FALSE STRICT!
   TP-GOOD 2dup TP-FILE! FILE-SIZE {: bytes:n :}
   bytes bytes MEM:BYTES-ALLOC-LEN
   [: TP-MAP-OK-ACT ;] MEM:WITH-BYTES
   UB-MAPPED-SPANS-CLEAR? TTRUE
   RESET ;

\ Checked catch requires a stack-preserving quotation, but UB-MAPPED-FILE
\ consumes the linear mapping extent. This private test boundary catches that
\ exact production helper and checks its state before WITH-BYTES can release
\ the mapping or any outer cleanup can clear it.
TRUSTED: TP-MAP-THROW-ACT ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   TP-OUT 1 LINT-OUT-BUFFER!
   [: UB-MAPPED-FILE ;] catch
   dup 0= if
      LINT-OUT-BUFFER-OFF
      drop
      LINT-FALSE TTRUE
      exit
   then
   {: rc:n :}
   drop 2drop
   LINT-OUT-BUFFER-OFF
   rc E-STR-CAPACITY T=
   UB-MAPPED-SPANS-CLEAR? TTRUE ;

: TP-MAP-THROW-TEST ( -- )
   RESET
   LINT-FALSE STRICT!
   TP-BAD 2dup TP-FILE! FILE-SIZE {: bytes:n :}
   bytes bytes MEM:BYTES-ALLOC-LEN
   [: TP-MAP-THROW-ACT ;] MEM:WITH-BYTES
   UB-MAPPED-SPANS-CLEAR? TTRUE
   RESET ;

: TP-ZERO-TEST ( -- )
   RESET
   19 UB-I ! 23 UB-LINE ! 29 UB-COL !
   TP-EMPTY FILE
   UB-I @ 0 T=
   UB-LINE @ 1 T=
   UB-COL @ 1 T=
   UB-SPANS-CLEAR? TTRUE
   RESET ;

get-current TP-PRIV-WID !

public

get-current TP-PUB-WID !

private

: TP-PUBLIC? ( ptr u8 n -- bool )
   TP-PUB-WID @ XREF-FIND-WL XREF-FOUND? ;

: TP-PRIVATE? ( ptr u8 n -- bool )
   TP-PRIV-WID @ XREF-FIND-WL XREF-FOUND? ;

: TP-GLOBAL? ( ptr u8 n -- bool )
   0 XREF-FIND-WL XREF-FOUND? ;

: TP-PUBLIC-N ( -- n )
   0 0
   begin over ndict@ < while
      over XREF-REC XREF-WORDLIST TP-PUB-WID @ = if 1+ then
      swap 1+ swap
   repeat
   nip ;

: TP-API-TEST ( -- )
   TP-PUBLIC-N 6 T=
   s" RESET" TP-PUBLIC? TTRUE
   s" JSON!" TP-PUBLIC? TTRUE
   s" STRICT!" TP-PUBLIC? TTRUE
   s" OUT-FD!" TP-PUBLIC? TTRUE
   s" FILE" TP-PUBLIC? TTRUE
   s" FINISH" TP-PUBLIC? TTRUE
   s" CHECKED-BOUNDARY-LINT-RESET" TP-PUBLIC? TFALSE
   s" CHECKED-BOUNDARY-LINT-FILE" TP-PUBLIC? TFALSE
   s" CHECKED-BOUNDARY-LINT-FINISH" TP-PUBLIC? TFALSE
   s" UB-JSON!" TP-PUBLIC? TFALSE
   s" UB-STRICT-BOUNDARY!" TP-PUBLIC? TFALSE
   s" UB-OUT-FD!" TP-PUBLIC? TFALSE
   s" UB-MAPPED-FILE" TP-PUBLIC? TFALSE
   s" UB-CLEAR-SPANS" TP-PUBLIC? TFALSE
   s" UB-JSON!" TP-PRIVATE? TFALSE
   s" UB-STRICT-BOUNDARY!" TP-PRIVATE? TFALSE
   s" UB-OUT-FD!" TP-PRIVATE? TFALSE
   s" CHECKED-BOUNDARY-LINT-RESET" TP-GLOBAL? TFALSE
   s" CHECKED-BOUNDARY-LINT-FILE" TP-GLOBAL? TFALSE
   s" CHECKED-BOUNDARY-LINT-FINISH" TP-GLOBAL? TFALSE
   s" UB-JSON!" TP-GLOBAL? TFALSE
   s" UB-STRICT-BOUNDARY!" TP-GLOBAL? TFALSE
   s" UB-OUT-FD!" TP-GLOBAL? TFALSE
   s" UB-MAPPED-FILE" TP-GLOBAL? TFALSE
   s" UB-CLEAR-SPANS" TP-GLOBAL? TFALSE ;

: TP-TEST ( -- )
   T-RESET
   TP-PREPARE
   TP-API-TEST
   TP-ZERO-TEST
   TP-MAP-OK-TEST
   TP-MAP-THROW-TEST
   CLEANUP-RUN
   TP-ROOT EXISTS? TFALSE
   T-REPORT ;

TP-TEST

;package

package CBLT

public

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-CURRENT
   TEST-MAP-THROW
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
