\ boot-pin-test.f - checked regression for tools/boot-pin.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f tools/boot-pin.f
\   test/boot-pin-test.f
\
\ Proves: (1) the on-disk boot-prefix manifest digest is deterministic and
\ matches itself; (2) a drifted prefix (a sandbox copy with one changed file)
\ produces a different digest and is detected (BP-MATCH? false); (3) the CLI
\ `verify` fails with the named diagnostic + rc 70 on a drifted sandbox and
\ succeeds on a pristine one; (4) the pinned path list matches src/habu/habu2.f
\ (the exact set the engine re-reads at boot) — membership + count tripwire.

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<idx> FIND-SUB consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require tools/boot-pin.f

64 constant BPT-HEX-U
4096 constant BPT-CAP
30000 constant BPT-TIMEOUT-MS
70 constant BPT-DRIFT-RC
31 constant BPT-PFX-ROW#            \ PFX-LOAD-ROW occurrences in habu2.f (1 def + 30 calls)
                                    \ 22->26: +src/core/type-family.f +type-schema.f +sumtype.f
                                    \ +type-family-sha.f (TFAM growth; user sign-off dot
                                    \ habu-boot-pin-sign-be74cfd3). 26->31: +src/core/
                                    \ layout-buffer.f +layout-buffer-seal.f +layout-valid.f
                                    \ +lower-cert-base.f +lower-cert-seal.f (wide-ADT stack +
                                    \ LAYOUT-BUFFER + verified-lowering seals, tfam lane
                                    \ 2026-07-12; growth surfaced to the user at merge).

create BPT-D1   80 allot            \ pristine digest hex
create BPT-HEX  80 allot            \ scratch digest hex
create BPT-SBX  1024 allot          \ sandbox root
create BPT-DST  1024 allot          \ scratch path
create BPT-OUT  BPT-CAP allot
create BPT-ERR  BPT-CAP allot
variable BPT-SBX-U
PTR-VARIABLE BPT-HABU2-A
variable BPT-HABU2-U
variable BPT-CNT
variable BPT-OFF

: BPT-TRUE ( -- bool )  0 0= ;

\ ---- sandbox build (reuses BP-EACH: the single boot-prefix path list) -------
: BPT-TMP$ ( -- ptr u8 n )
   s" HB_TMP" GETENV dup 0 > if exit then 2drop
   s" TMPDIR" GETENV dup 0 > if exit then 2drop
   s" /tmp" ;

: BPT-SBX! ( -- )
   BPT-TMP$ s" habu-bootpin-test" BPT-SBX JOIN-PATH BPT-SBX-U ! ;

: BPT-MKDIR ( ptr u8 n -- ) {: a:ptr u:n :}
   BPT-SBX BPT-SBX-U @ a u BPT-DST JOIN-PATH {: du:n :}
   BPT-DST du MAKE-DIRS ;

: BPT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   BPT-SBX BPT-SBX-U @ a u BPT-DST JOIN-PATH {: du:n :}
   a u BPT-DST du COPY-FILE-STREAM ;

: BPT-SETUP-SBX ( -- )                                    \ fresh pristine target-specific prefix copy
   s" src/core" BPT-MKDIR
   s" src/os/macos" BPT-MKDIR
   s" src/habu" BPT-MKDIR
   [: BPT-COPY ;] BP-EACH ;

: BPT-CHECKER-SBX$ ( -- ptr u8 n )
   BPT-SBX BPT-SBX-U @ s" src/core/checker.f" BPT-DST JOIN-PATH BPT-DST swap ;

: BPT-DRIFT ( -- )                                        \ change one file's content (only ever hashed)
   BPT-CHECKER-SBX$ s" boot-pin-test-drift-marker" APPEND-FILE ;

: BPT-ROOT-SBX! ( -- )  BPT-SBX BPT-SBX-U @ BP-ROOT! ;
: BPT-ROOT-CWD! ( -- )  0 BP-ROOT-U ! ;

\ ---- in-process digest + drift assertions ----------------------------------
: BPT-PRISTINE! ( -- )
   BPT-ROOT-CWD!
   BPT-D1 BP-HASH-HEX ;

: BPT-DETERMINISTIC ( -- )
   BPT-ROOT-CWD!
   BPT-HEX BP-HASH-HEX
   BPT-HEX BPT-HEX-U BPT-D1 BPT-HEX-U T$= ;

: BPT-OWN-MATCH ( -- )
   BPT-D1 BPT-HEX-U BP-DIGEST-HEX! TTRUE                  \ 64-hex accepted
   BPT-ROOT-CWD! BP-MATCH? TTRUE ;

: BPT-SANDBOX-PRISTINE ( -- )
   BPT-ROOT-SBX!
   BP-MATCH? TTRUE                                        \ copy == original
   BPT-HEX BP-HASH-HEX  BPT-HEX BPT-HEX-U BPT-D1 BPT-HEX-U T$= ;

: BPT-SANDBOX-DRIFT ( -- )
   BPT-DRIFT
   BPT-ROOT-SBX!
   BP-MATCH? TFALSE                                       \ drift detected
   BPT-HEX BP-HASH-HEX  BPT-HEX BPT-HEX-U BPT-D1 BPT-HEX-U T$<> ;

: BPT-DIAG-TEXT ( -- )
   BP-DIAG$ s" boot-pin: on-disk boot prefix does not match expected digest (drift from certified checker/core source)" T$= ;

\ ---- CLI subprocess: verify fails on drift, passes on pristine --------------
: BPT-ARGV ( -- )                                         \ verify BPT-D1 --root BPT-SBX
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/boot-pin.f" >LEN PROC-ARGV+
   s" tools/boot-pin-main.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" verify" >LEN PROC-ARGV+
   BPT-D1 BPT-HEX-U >LEN PROC-ARGV+
   s" --root" >LEN PROC-ARGV+
   BPT-SBX BPT-SBX-U @ >LEN PROC-ARGV+ ;

: BPT-RUN ( -- n n n )                                    \ -> outu erru rc
   BPT-ARGV
   s" bin/hb" >LEN BPT-OUT BPT-CAP >LEN BPT-ERR BPT-CAP >LEN BPT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: BPT-CLI-DRIFT ( -- )                                    \ drifted sandbox -> rc 70 + named diag
   BPT-RUN {: outu:n erru:n rc:n :}
   rc BPT-DRIFT-RC T=
   BPT-ERR erru BP-DIAG$ CONTAINS? TTRUE ;

: BPT-CLI-PRISTINE ( -- )                                 \ fresh pristine sandbox -> rc 0 + OK
   BPT-SETUP-SBX
   BPT-RUN {: outu:n erru:n rc:n :}
   rc 0 T=
   BPT-OUT outu s" boot-pin: OK" CONTAINS? TTRUE ;

\ ---- cross-check the pinned list against the engine's re-read set -----------
: BPT-READ-HABU2 ( -- )
   s" src/habu/habu2.f" FILE-SIZE MEM-ALLOC-64K-SPAN {: a:ptr cap:n :}
   a BPT-HABU2-A !
   s" src/habu/habu2.f" a cap READ-ALL BPT-HABU2-U ! ;

: BPT-HABU2$ ( -- ptr u8 n )  BPT-HABU2-A @ BPT-HABU2-U @ ;

: BPT-HABU2-HAS ( ptr u8 n -- ) {: a:ptr u:n :}          \ each pinned path is loaded by habu2.f
   BPT-HABU2$ a u CONTAINS? TTRUE ;

: BPT-COUNT-SUB ( ptr u8 n -- n ) {: na:ptr nu:n :}       \ occurrences of needle in habu2.f
   0 BPT-CNT !  0 BPT-OFF !
   begin
      BPT-HABU2-A @ BPT-OFF @ BYTE+  BPT-HABU2-U @ BPT-OFF @ -  na nu FIND-SUB
      MATCH option
        none OF BPT-TRUE 0= ENDOF
        some OF IDX>N BPT-OFF @ + nu + BPT-OFF !  BPT-CNT @ 1 + BPT-CNT !  BPT-TRUE ENDOF
      ;MATCH
   while repeat
   BPT-CNT @ ;

: BPT-LIST-CONSISTENT ( -- )
   [: BPT-HABU2-HAS ;] BP-EACH                            \ membership: no pinned path dropped/renamed
   s" PFX-LOAD-ROW" BPT-COUNT-SUB BPT-PFX-ROW# T= ;       \ tripwire: no prefix file added/removed

\ ---- run --------------------------------------------------------------------
: BPT-RUN-ALL ( -- )
   T-RESET
   BPT-SBX!
   BPT-PRISTINE!
   BPT-DETERMINISTIC
   BPT-OWN-MATCH
   BPT-SETUP-SBX
   BPT-SANDBOX-PRISTINE
   BPT-SANDBOX-DRIFT
   BPT-DIAG-TEXT
   BPT-CLI-DRIFT
   BPT-CLI-PRISTINE
   BPT-READ-HABU2
   BPT-LIST-CONSISTENT
   T-REPORT ;

BPT-RUN-ALL
