\ toolchain-test.f - checked tests for PTXTC artifact helpers.

require lib/test.f
require lib/adt/option.f
require lib/ptx/toolchain.f

package PTXTC-TEST

create PATH-A FS-PATH-CAP allot
create PATH-B FS-PATH-CAP allot
variable PATH-A-U
variable PATH-B-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PATH-A$ ( -- ptr u8 n )
   PATH-A PATH-A-U @ ;

: PATH-B$ ( -- ptr u8 n )
   PATH-B PATH-B-U @ ;

\ probe present (this host has ptxas): PTXAS$ returns the same non-empty path.
: ASSERT-PTXAS-PRESENT ( n -- )   \ n = TRY-PTXAS$ reported length
   PTXTC:PTXAS$ nip          \ ( probe-len ptxas-len )
   dup 0 > TTRUE             \ resolved path is non-empty
   = TTRUE ;                 \ and PTXAS$ agrees with the probe

\ Per host class: TRY-PTXAS$ is the fail-open probe. Present -> PTXAS$ resolves the
\ same path; absent (this CUDA-less Mac) -> PTXAS$ fails closed with E-PTXTC-PTXAS.
\ Both arms are real assertions; the present arm runs on a ptxas-equipped host.
: ASSERT-PTXAS ( -- )
   PTXTC:TRY-PTXAS$ MATCH option
     none OF [: PTXTC:PTXAS$ 2drop ;] E-PTXTC-PTXAS TTHROWSQ ENDOF
     some OF ASSERT-PTXAS-PRESENT ENDOF
   ;MATCH ;

: PREPARE-PATHS ( -- )
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ FILE? TFALSE
   PTXTC:CUBIN$ FILE? TFALSE
   ASSERT-PTXAS
   PTXTC:PTX$ s" ptx" WRITE-ALL
   PTXTC:PTX$ FILE? TTRUE
   PTXTC:CLEAN
   PTXTC:PTX$ FILE? TFALSE ;

: PREPARE-CLEANS-OLD ( -- )
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ PATH-A PATH-A-U COPY!
   PATH-A$ s" old" WRITE-ALL
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ PATH-B PATH-B-U COPY!
   PATH-A$ PATH-B$ T$<>
   PATH-A$ FILE? TFALSE
   PTXTC:CLEAN ;

\ ASSEMBLE with no arch configured must fail closed - there is no fallback target.
create ASM-O 64 allot   create ASM-E 64 allot

public

: RUN ( -- )
   T-RESET
   \ fail-closed before any TC-ARCH! runs in this process
   [: ASM-O 64 >LEN  ASM-E 64 >LEN  PTXTC:ASSEMBLE drop ;] E-PTXTC-ARCH TTHROWSQ
   PREPARE-PATHS
   PREPARE-CLEANS-OLD
   T-REPORT ;

;package

PTXTC-TEST:RUN

\ Fail-closed toolchain identity (dot habu-enforce-pinned-ptxas-4598a743): reopen
\ the owner package so the private allowlist, descriptor, probe, and policy words
\ are in scope. Fake tools are shell scripts written under a temp ROOT; every probe
\ outcome, version, and digest case is driven on the descriptor directly, because
\ the enforcement verifies whatever path resolves and never trusts its source.
package PTXTC
T-RESET

\ PARSE-VER discriminates the two assemblers that motivated the pin (13.3 = the
\ Blackwell-grade scheduler, 13.0 = the 40-NOP one) and rejects malformed text.
: T-VER-PARSE ( -- )
   s" Cuda compilation tools, release 13.3, V13.3.33" PARSE-VER PTXAS-PINNED-VER T=
   s" Cuda compilation tools, release 13.0, V13.0.88" PARSE-VER 13000 T=
   s" no release line here"  PARSE-VER -1 T=
   s" release 13"            PARSE-VER -1 T=
   s" release 13.x"          PARSE-VER -1 T= ;

\ SM121-TARGET? keys off the configured arch (the profile the pin protects).
: T-SM121 ( -- )
   s" sm_121a" TC-ARCH!  SM121-TARGET? TTRUE
   s" sm_90a"  TC-ARCH!  SM121-TARGET? TFALSE ;

\ Habu's own tool store is in the probe list at $HOME/.habu/toolchain/ptxas-13.3.33.
: T-STORE-SHAPE ( -- )
   s" HOME" GETENV nip 0 > if
      STORE-PTXAS$ s" /.habu/toolchain/ptxas-13.3.33" CONTAINS? TTRUE
   then ;

\ Resolution precedence is data (no env mutation): env override, store, system
\ CUDA 13, legacy 12.6 - in that order. An env override is present when non-empty
\ (verified downstream); a path candidate is present only when it exists on disk.
: T-PRECEDENCE ( -- )
   PTXAS-CAND-N 4 T=
   0 PTXAS-CAND$ s" PTXAS" GETENV T$=
   1 PTXAS-CAND$ STORE-PTXAS$ T$=
   2 PTXAS-CAND$ s" /usr/local/cuda/bin/ptxas" T$=
   3 PTXAS-CAND$ s" /usr/local/cuda-12.6/bin/ptxas" T$=
   0 s" /any/thing"     PTXAS-CAND-PRESENT? TTRUE
   0 NULL$              PTXAS-CAND-PRESENT? TFALSE
   2 s" /no/such/ptxas" PTXAS-CAND-PRESENT? TFALSE ;

\ ---- fake-tool matrix ------------------------------------------------------
create FT-PATH FS-PATH-CAP allot   variable FT-PATH-U
create FT-MARK FS-PATH-CAP allot   variable FT-MARK-U
create FT-AO   64 allot             create FT-AE 64 allot

: FT-PATH$ ( -- ptr u8 n )  FT-PATH FT-PATH-U @ ;
: FT-MARK$ ( -- ptr u8 n )  FT-MARK FT-MARK-U @ ;

: FB-133$   ( -- ptr u8 n )  S\" #!/bin/sh\necho 'Cuda compilation tools, release 13.3, V13.3.33'\n" ;
: FB-130$   ( -- ptr u8 n )  S\" #!/bin/sh\necho 'Cuda compilation tools, release 13.0, V13.0.88'\n" ;
: FB-MALF$  ( -- ptr u8 n )  S\" #!/bin/sh\necho 'no release line here'\n" ;
: FB-FAIL$  ( -- ptr u8 n )  S\" #!/bin/sh\nexit 3\n" ;
: FB-TRUNC$ ( -- ptr u8 n )  S\" #!/bin/sh\ni=0\nwhile [ $i -lt 80 ]; do echo 0123456789; i=$((i+1)); done\n" ;

\ Write BODY as an executable fake ptxas under ROOT; park its path in FT-PATH.
: FT-BUILD ( ptr u8 n -- ) {: b:ptr bu:n :}
   ROOT$ s" fake-ptxas" FT-PATH JOIN-PATH FT-PATH-U !
   FT-PATH$ b bu WRITE-ALL
   FT-PATH$ CHMOD-X ;

\ A counting fake: prints 13.3 for --version, appends one line to FT-MARK for any
\ assembly invocation, so the marker size is the exact assembly-spawn count.
: FT-COUNT-BUILD ( -- )
   SB-RESET
   S\" #!/bin/sh\ncase \"$1\" in\n--version) echo 'Cuda compilation tools, release 13.3, V13.3.33' ;;\n*) echo x >> '" SB-APPEND
   FT-MARK$ SB-APPEND
   S\" ' ;;\nesac\n" SB-APPEND
   SB$ FT-BUILD ;

: FT-ASM-COUNT ( -- n )  FT-MARK$ FILE? if FT-MARK$ FILE-SIZE else 0 then ;
: FT-MARK-CLEAR ( -- )   FT-MARK$ FILE? if FT-MARK$ REMOVE-FILE then ;

: SET-TC-PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   a TC-PATH-BUF u BYTE-COPY  u TC-PATH-U ! ;

: FT-RESET-CACHE ( -- )   0 TC-ID-DONE !  0 TC-DIGEST-DONE ! ;
: FT-ALLOW-RESET ( -- )   0 ALLOW-N !  0 ALLOW-SEEDED !  ALLOW-SEED ;
: TC-ENFORCE-OK? ( -- bool )  [: TC-ENFORCE ;] catch 0= ;

\ Point the descriptor at a nonexistent path (probe and hash both fail).
: FT-MISSING-PATH ( -- )
   ROOT$ s" no-such-tool" FT-PATH JOIN-PATH FT-PATH-U !
   FT-PATH$ SET-TC-PATH ;

\ Every probe outcome resolves fail-closed to a NAMED error, or a real version.
: T-PROBE ( -- )
   FB-133$   FT-BUILD  FT-PATH$ SET-TC-PATH  PROBE-VER PTXAS-PINNED-VER T=
   FB-130$   FT-BUILD  FT-PATH$ SET-TC-PATH  PROBE-VER 13000 T=
   FB-MALF$  FT-BUILD  FT-PATH$ SET-TC-PATH  [: PROBE-VER drop ;] E-PTXTC-VERSION TTHROWSQ
   FB-FAIL$  FT-BUILD  FT-PATH$ SET-TC-PATH  [: PROBE-VER drop ;] E-PTXTC-PROBE TTHROWSQ
   FB-TRUNC$ FT-BUILD  FT-PATH$ SET-TC-PATH  [: PROBE-VER drop ;] E-PTXTC-PROBE TTHROWSQ
   FT-MISSING-PATH                           [: PROBE-VER drop ;] E-PTXTC-PROBE TTHROWSQ ;

\ Digest allowlist: a fresh fake is unlisted; adding its digest lists it; the
\ pinned identity stays listed; an unreadable tool fails closed.
: T-DIGEST ( -- )
   FT-ALLOW-RESET
   FB-133$ FT-BUILD  FT-PATH$ SET-TC-PATH  FT-RESET-CACHE
   TC-DIGEST$ ALLOW-HAS? TFALSE
   TC-DIGEST$ ALLOW-DIGEST+
   TC-DIGEST$ ALLOW-HAS? TTRUE
   PINNED-DIGEST$ ALLOW-HAS? TTRUE
   FT-MISSING-PATH  FT-RESET-CACHE
   [: TC-DIGEST$ 2drop ;] E-PTXTC-DIGEST TTHROWSQ ;

\ sm_121 policy, digest and version gates isolated: an allowlisted-but-old tool
\ rejects on the FLOOR; a current-but-unlisted tool rejects on the DIGEST.
: T-GATE-GB10 ( -- )
   s" sm_121a" TC-ARCH!
   FT-ALLOW-RESET
   FB-133$ FT-BUILD  FT-PATH$ SET-TC-PATH  FT-RESET-CACHE
   TC-DIGEST$ ALLOW-DIGEST+
   PTXAS-PINNED-VER TC-VER !  STR-TRUE TC-ID-DONE !  0 TC-DIGEST-DONE !
   TC-ENFORCE-OK? TTRUE
   13000 TC-VER !  0 TC-DIGEST-DONE !
   [: TC-ENFORCE ;] E-PTXTC-STALE TTHROWSQ
   FT-ALLOW-RESET
   PTXAS-PINNED-VER TC-VER !  0 TC-DIGEST-DONE !
   [: TC-ENFORCE ;] E-PTXTC-DIGEST TTHROWSQ ;

\ Non-sm_121 target: the explicit compat policy accepts an old, unlisted tool (the
\ pin guards only sm_121) but still fails closed on an unreadable one.
: T-GATE-COMPAT ( -- )
   s" sm_90a" TC-ARCH!
   FT-ALLOW-RESET
   FB-130$ FT-BUILD  FT-PATH$ SET-TC-PATH
   13000 TC-VER !  STR-TRUE TC-ID-DONE !  0 TC-DIGEST-DONE !
   TC-ENFORCE-OK? TTRUE
   FT-MISSING-PATH  0 TC-DIGEST-DONE !
   [: TC-ENFORCE ;] E-PTXTC-DIGEST TTHROWSQ ;

\ Digest replacement after probe: accept the pinned-listed bytes, swap the file for
\ an unlisted tool, and a fresh resolution re-hashes and fails closed.
: T-DIGEST-SWAP ( -- )
   s" sm_121a" TC-ARCH!
   FT-ALLOW-RESET
   FB-133$ FT-BUILD  FT-PATH$ SET-TC-PATH  FT-RESET-CACHE
   TC-DIGEST$ ALLOW-DIGEST+
   PTXAS-PINNED-VER TC-VER !  STR-TRUE TC-ID-DONE !  0 TC-DIGEST-DONE !
   TC-ENFORCE-OK? TTRUE
   FT-PATH$ FB-130$ WRITE-ALL
   0 TC-DIGEST-DONE !
   [: TC-ENFORCE ;] E-PTXTC-DIGEST TTHROWSQ ;

\ Exact no-assembly-on-reject count: ASSEMBLE spawns the assembler exactly once on
\ accept and never on a rejected identity, measured by the counting fake's marker.
: T-ASM-COUNT ( -- )
   s" habu-ptx-enforce-asm" PREPARE
   ROOT$ s" asm-marker" FT-MARK JOIN-PATH FT-MARK-U !
   s" sm_121a" TC-ARCH!
   FT-ALLOW-RESET
   FT-COUNT-BUILD  FT-PATH$ SET-TC-PATH  FT-RESET-CACHE
   TC-DIGEST$ ALLOW-DIGEST+
   FT-MARK-CLEAR
   PTXAS-PINNED-VER TC-VER !  STR-TRUE TC-ID-DONE !  0 TC-DIGEST-DONE !
   FT-AO 64 >LEN FT-AE 64 >LEN ASSEMBLE 0 T=
   FT-ASM-COUNT 0 > TTRUE
   FT-MARK-CLEAR
   FT-ALLOW-RESET
   STR-TRUE TC-ID-DONE !  0 TC-DIGEST-DONE !
   [: FT-AO 64 >LEN FT-AE 64 >LEN ASSEMBLE drop ;] E-PTXTC-DIGEST TTHROWSQ
   FT-ASM-COUNT 0 T= ;

\ The REAL pinned store on this host runs the whole gate: resolve -> hash the true
\ 13.3.33 bytes -> match the pinned digest -> >= floor -> accept, no override.
: T-STORE-REAL ( -- )
   s" PTXAS" GETENV nip 0 > if exit then
   STORE-PTXAS$ FILE? 0= if exit then
   FT-ALLOW-RESET
   0 TC-ID-DONE !  0 TC-DIGEST-DONE !
   s" sm_121a" TC-ARCH!
   TC-GATE
   TC-PATH$ STORE-PTXAS$ T$=
   TC-VER @ PTXAS-PINNED-VER T=
   TC-DIGEST$ PINNED-DIGEST$ T$= ;

s" habu-ptx-enforce" PREPARE
T-VER-PARSE  T-SM121  T-STORE-SHAPE  T-PRECEDENCE
T-PROBE  T-DIGEST  T-GATE-GB10  T-GATE-COMPAT  T-DIGEST-SWAP
T-ASM-COUNT  T-STORE-REAL
s" sm_121a" TC-ARCH!  TC-ARCH$ s" sm_121a" T$=
CLEAN
T-REPORT
;package
