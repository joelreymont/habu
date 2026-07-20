\ toolchain.f - checked PTX artifact paths and assembler runner.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/adt/option.f             \ option<n>: TRY-PTXAS$ reports a resolved ptxas as present/absent

\ tcpol - the assembler compatibility policy for the CONFIGURED target, decided
\ before any PTX is consumed. sm_121 (GB10) demands the pinned Blackwell-grade
\ identity (gb10); every other target takes the explicit non-pinned branch
\ (compat). The MATCH is exhaustive by construction, so a target class can never
\ fall through into an implicit unknown-success assembly.
SUMTYPE tcpol 0
  VARIANT gb10 ;VARIANT
  VARIANT compat ;VARIANT
;SUMTYPE

package PTXTC

$2710 constant ASM-TIMEOUT-MS

create ROOT-BUF FS-PATH-CAP allot
create PTX-BUF FS-PATH-CAP allot
create CUBIN-BUF FS-PATH-CAP allot

variable ROOT-U
variable PTX-U
variable CUBIN-U

variable ERR-P            \ last ASSEMBLE stderr buffer + byte count, for diagnostics
variable ERR-U

variable PTXAS-P          \ resolved ptxas path pointer, parked by TRY-PTXAS$ for PTXAS$
                          \ (env-override and string-literal pointers are process-stable, so no copy)

create TC-ARCH-BUF 32 allot   \ assembler target label (e.g. sm_121a); TC-ARCH-U 0 = unset
variable TC-ARCH-U            \ never defaulted: ASSEMBLE fails closed until a caller sets it

: TC-ARCH$ ( -- ptr u8 n )   \ configured arch, or fail closed (no fallback target)
   TC-ARCH-U @ 0= if E-PTXTC-ARCH throw then
   TC-ARCH-BUF TC-ARCH-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: root:ptr rootu:n name:ptr nameu:n dst:ptr lenp:ptr :}
   root rootu name nameu dst JOIN-PATH lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: CLEAN-ROOT ( -- )
   ROOT-U @ 0 > if ROOT$ REMOVE-TREE then
   0 ROOT-U ! ;

\ Park a resolved ptxas path pointer and report it present with its byte length.
: PTXAS-FOUND ( ptr u8 n -- option<n> ) {: a:ptr u:n :}
   a PTXAS-P !  u OPTION:SOME ;

\ ---- fail-closed toolchain identity (dot habu-enforce-pinned-ptxas-4598a743) --
\ system CUDA 13.0's ptxas ships an immature sm_121 scheduler that issues every
\ HMMA at a fixed yield-set interval (the 40-NOP steady body), costing ~27% of
\ GEMM tensor-core throughput; ptxas 13.3 schedules the same UNMODIFIED PTX with
\ zero NOPs. The 13.3 assembler is pinned into Habu's own tool store, SHA-256
\ identified. The precursor dot habu-pin-blackwell-grade-8ec5ee0a only WARNED on
\ an older assembler and let ASSEMBLE continue, so a replaced/corrupt store
\ binary, an arbitrary PTXAS override, an unreadable tool, or the known-bad 13.0
\ could still assemble sm_121 code. This module now enforces one fail-closed GB10
\ identity BEFORE consuming any PTX: resolve once, hash the executable bytes and
\ require the pinned allowlisted SHA-256, parse and require a version >= the 13.3
\ floor, then bind path+digest+version+target. Probe failure, output truncation,
\ path/digest/version drift, and old versions each reject with a named throw. An
\ explicit PTXAS override earns no trust from presence: it is verified against the
\ same allowlist and floor. Non-sm_121 targets take an explicit typed compat
\ policy (tcpol), never an implicit unknown-success branch.
\ Seam note: the private descriptor here is the "audited PTXTC discovery facts"
\ the R3 toolchain-identity owner (dot habu-v2-r3-define-987402c7) will adapt into
\ its semantic descriptor, and the artifact provenance dot
\ habu-attest-proprietary-ptxas-6ce9fda2 attests around; neither is reimplemented
\ here.

1000 constant VER-MUL                           \ version encoding: MAJOR*VER-MUL + MINOR
13 VER-MUL * 3 + constant PTXAS-PINNED-VER       \ 13.3 floor: the Blackwell-grade sm_121 scheduler
char . constant VER-SEP                          \ '.' MAJOR/MINOR separator in `ptxas --version`

512 constant VER-CAP                             \ `ptxas --version` stdout capture
256 constant VER-ECAP
create VER-BUF VER-CAP allot
create VER-ERR VER-ECAP allot

create STORE-BUF FS-PATH-CAP allot               \ $HOME/.habu/toolchain/ptxas-13.3.33, built once
variable STORE-U

\ ---- pinned identity + digest allowlist ------------------------------------
64 constant DIGEST-LEN                            \ SHA-256 hex digest length
4  constant ALLOW-MAX                             \ pinned artifacts + fake-tool test injection

\ The documented pinned artifact: the ptxas from NVIDIA's cuda_nvcc-linux-sbsa-
\ 13.3.33 archive, provisioned to ~/.habu/toolchain/ptxas-13.3.33 (docs/codegen-
\ verdict.md "Pinned ptxas toolchain"). This SHA-256 is the single allowlist
\ truth; a binary merely present at a resolved path is never itself trust.
: PINNED-DIGEST$ ( -- ptr u8 n )
   s" f9a0a7f1f7f03b402ca222168a8ae4870fdb312354356b444941fbba7754326e" ;

create ALLOW-BUF DIGEST-LEN ALLOW-MAX * allot
variable ALLOW-N
variable ALLOW-SEEDED

: ALLOW-AT ( n -- ptr u8 )  DIGEST-LEN * ALLOW-BUF + ;

\ Append an allowed 64-hex digest. Private: only the seed and the fake-tool tests
\ that reopen this package add rows; there is no public "trust this digest" verb.
: ALLOW-DIGEST+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u DIGEST-LEN <> if E-PTXTC-DIGEST throw then
   ALLOW-N @ ALLOW-MAX >= if E-PTXTC-DIGEST throw then
   a ALLOW-N @ ALLOW-AT DIGEST-LEN BYTE-COPY
   ALLOW-N @ 1+ ALLOW-N ! ;

: ALLOW-SEED ( -- )                               \ install the pinned digest once
   ALLOW-SEEDED @ if exit then
   STR-TRUE ALLOW-SEEDED !
   PINNED-DIGEST$ ALLOW-DIGEST+ ;

: ALLOW-HAS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   ALLOW-SEED
   u DIGEST-LEN <> if STR-FALSE exit then
   ALLOW-N @ 0 ?do
      a u  i ALLOW-AT DIGEST-LEN  STR= if STR-TRUE unloop exit then
   loop STR-FALSE ;

\ ---- resolved-tool descriptor (bound once, before any PTX is consumed) ------
create TC-PATH-BUF FS-PATH-CAP allot
variable TC-PATH-U
variable TC-VER                                   \ parsed release version of the resolved tool
variable TC-ID-DONE                               \ path+version resolved and probed once this process
create TC-DIGEST-BUF DIGEST-LEN allot
variable TC-DIGEST-DONE                            \ executable bytes hashed once this process

: TC-PATH$ ( -- ptr u8 n )  TC-PATH-BUF TC-PATH-U @ ;

\ The pinned assembler in Habu's own tool store (not another project's cache dir,
\ which can vanish). Empty when HOME is unset so the probe simply skips it.
: STORE-PTXAS$ ( -- ptr u8 n )
   STORE-U @ 0 > if STORE-BUF STORE-U @ exit then
   s" HOME" GETENV dup 0= if 2drop STORE-BUF 0 exit then
   s" .habu/toolchain/ptxas-13.3.33" STORE-BUF JOIN-PATH STORE-U !
   STORE-BUF STORE-U @ ;

\ Read a leading decimal run at [a,u); return its value and the digit count (0 = none).
: LEAD-UINT ( ptr u8 n -- n n ) {: a:ptr u:n :}
   0 0
   begin dup u < if dup a + c@ STR-DIGIT? else STR-FALSE then while
      dup a + c@ STR-DIGIT-VALUE >r swap STR-BASE * r> + swap 1+
   repeat ;

\ "MAJOR.MINOR..." (the text right after "release ") -> MAJOR*VER-MUL+MINOR, else -1.
: VER-FROM-REL ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LEAD-UINT {: maj nd :}
   nd 0= if -1 exit then
   nd u >= if -1 exit then
   a nd + c@ VER-SEP <> if -1 exit then
   a nd 1+ + u nd 1+ - LEAD-UINT {: mnr nd2 :}
   nd2 0= if -1 exit then
   maj VER-MUL * mnr + ;

: REL-KEY ( -- ptr u8 n )  s" release " ;

\ Parse `ptxas --version` text -> MAJOR*VER-MUL+MINOR, else -1. Never throws: a
\ version we cannot read is reported unknown, never fatal.
: PARSE-VER ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u REL-KEY FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N REL-KEY nip + {: p :} a p + u p - VER-FROM-REL ENDOF
   ;MATCH ;

\ Spawn `<TC-PATH$> --version`, capture stdout, and store the parsed release into
\ PROBE-VER-OUT (-1 if the output carries no release line). A nonzero child exit
\ is an unusable probe -> E-PTXTC-PROBE; capture-layer spawn/timeout/truncation
\ throws propagate out and PROBE-VER folds them into the same named reject.
variable PROBE-VER-OUT
: PROBE-VER-RAW ( -- )
   PROC-ARGV-RESET
   s" --version" >LEN PROC-ARGV+
   TC-PATH$ >LEN VER-BUF VER-CAP >LEN VER-ERR VER-ECAP >LEN ASM-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} VER-BUF o LEN>N PARSE-VER PROBE-VER-OUT ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} E-PTXTC-PROBE throw ENDOF
   ;MATCH ;

\ Probe the resolved tool's version, fail-closed. Any process-level failure
\ (spawn, timeout, truncated capture, nonzero exit) -> E-PTXTC-PROBE; output with
\ no parseable release line -> E-PTXTC-VERSION.
: PROBE-VER ( -- n )
   [: PROBE-VER-RAW ;] catch dup 0 <> if drop E-PTXTC-PROBE throw then drop
   PROBE-VER-OUT @ dup 0 < if drop E-PTXTC-VERSION throw then ;

\ Hash the resolved executable's bytes once. An unreadable tool cannot present a
\ pinned identity -> E-PTXTC-DIGEST. Cached: the good path never re-hashes.
: TC-DIGEST$ ( -- ptr u8 n )
   TC-DIGEST-DONE @ if TC-DIGEST-BUF DIGEST-LEN exit then
   TC-PATH$ TC-DIGEST-BUF SHA256-FILE-HEX 0 <> if E-PTXTC-DIGEST throw then
   STR-TRUE TC-DIGEST-DONE !
   TC-DIGEST-BUF DIGEST-LEN ;

\ Is the configured assembler arch an sm_121-family target? (the profile whose
\ scheduler quality the 13.3 pin protects).
: SM121-TARGET? ( -- bool )
   TC-ARCH-U @ 0= if STR-FALSE exit then
   TC-ARCH-BUF TC-ARCH-U @ s" sm_121" CONTAINS? ;

public

\ Set the ptxas target from the active target's label (maki resolves it by
\ probing the device). No default: a caller that never sets it cannot assemble.
: TC-ARCH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u 32 > or if E-PTXTC-ARCH throw then
   a TC-ARCH-BUF u BYTE-COPY  u TC-ARCH-U ! ;

: CLEAN ( -- )
   CLEAN-ROOT ;

: PREPARE ( ptr u8 n -- )
   CLEAN-ROOT
   TMPDIR-MKDIR ROOT-BUF ROOT-U COPY!
   ROOT$ s" kernel.ptx" PTX-BUF PTX-U PATH!
   ROOT$ s" kernel.cubin" CUBIN-BUF CUBIN-U PATH! ;

: PTX$ ( -- ptr u8 n )
   PTX-BUF PTX-U @ ;

: CUBIN$ ( -- ptr u8 n )
   CUBIN-BUF CUBIN-U @ ;

4 constant PTXAS-CAND-N                            \ number of resolution candidates

\ The ptxas resolution candidates in precedence order: the PTXAS env override,
\ then Habu's own pinned 13.3 tool store, then the CUDA 13 install (its ptxas
\ knows sm_121a), then the legacy 12.6 path. The list lives here ONLY, so tests
\ assert the order against PTXAS-CAND$ without mutating the environment.
: PTXAS-CAND$ ( n -- ptr u8 n ) {: idx :}
   idx 0= if s" PTXAS" GETENV exit then
   idx 1 = if STORE-PTXAS$ exit then
   idx 2 = if s" /usr/local/cuda/bin/ptxas" exit then
   idx 3 = if s" /usr/local/cuda-12.6/bin/ptxas" exit then
   NULL$ ;                                         \ out-of-range index: empty candidate

\ A candidate is present if the env override is non-empty (set means chosen - its
\ identity is still verified downstream), or a path candidate exists on disk.
: PTXAS-CAND-PRESENT? ( n ptr u8 n -- bool ) {: idx a:ptr u:n :}
   idx 0= if u 0 > exit then
   u 0 > if a u FILE? else STR-FALSE then ;

\ Probe ptxas without failing: walk the candidates in order, parking the first
\ present path in PTXAS-P. NONE means no ptxas resolved anywhere. No throws -
\ callers decide whether absence is fatal; identity is enforced later at TC-GATE.
: TRY-PTXAS$ ( -- option<n> )
   PTXAS-CAND-N 0 ?do
      i PTXAS-CAND$ {: a:ptr u:n :}
      i a u PTXAS-CAND-PRESENT? if a u PTXAS-FOUND unloop exit then
   loop OPTION:NONE ;

\ Resolve ptxas, fail closed: the single place that turns an unresolved ptxas into
\ a loud named throw. No silent dead-path default.
: PTXAS$ ( -- ptr u8 n )
   TRY-PTXAS$ MATCH option
     none OF E-PTXTC-PTXAS throw ENDOF
     some OF PTXAS-P @ swap ENDOF
   ;MATCH ;

\ Resolve the ptxas path once and probe its version once, binding both into the
\ descriptor. Target-independent facts: the digest allowlist and version floor
\ are policy applied per target at TC-ENFORCE. E-PTXTC-PTXAS / E-PTXTC-PROBE /
\ E-PTXTC-VERSION on failure. The good path resolves and spawns `--version` once.
: TC-RESOLVE-ID ( -- )
   TC-ID-DONE @ if exit then
   PTXAS$ {: a:ptr u:n :}
   a TC-PATH-BUF u BYTE-COPY  u TC-PATH-U !
   PROBE-VER TC-VER !
   STR-TRUE TC-ID-DONE ! ;

\ sm_121 policy: the resolved tool must hash to a pinned allowlisted digest AND
\ report a version >= the 13.3 floor. Digest drift/override-not-pinned -> reject,
\ old version -> reject. Presence is never trust.
: TC-CHECK-GB10 ( -- )
   TC-DIGEST$ ALLOW-HAS? 0= if E-PTXTC-DIGEST throw then
   TC-VER @ PTXAS-PINNED-VER < if E-PTXTC-STALE throw then ;

\ Non-sm_121 policy: the pinned identity guards only the sm_121 scheduler, so no
\ floor and no allowlist apply - but the tool must still present a readable
\ identity (hash succeeds), so an unreadable assembler fails closed here too.
: TC-CHECK-COMPAT ( -- )
   TC-DIGEST$ 2drop ;

\ Decide the compatibility policy for the configured target: sm_121 -> gb10 (the
\ pinned pin), everything else -> compat. An exhaustive typed match, never an
\ implicit unknown-success branch.
: TC-POLICY ( -- tcpol )
   SM121-TARGET? if TCPOL:GB10 else TCPOL:COMPAT then ;

: TC-ENFORCE ( -- )
   TC-POLICY MATCH tcpol
     gb10   OF TC-CHECK-GB10 ENDOF
     compat OF TC-CHECK-COMPAT ENDOF
   ;MATCH ;

\ The single fail-closed gate: resolve+probe the tool once, then apply the target
\ policy. ASSEMBLE calls this BEFORE consuming any PTX, so a rejected toolchain
\ never reaches the assembly spawn.
: TC-GATE ( -- )
   TC-RESOLVE-ID
   TC-ENFORCE ;

: ASSEMBLE ( ptr u8 len ptr u8 len -- n )
   {: out:ptr outcap:len err:ptr errcap:len :}
   TC-ARCH$ 2drop                                   \ fail closed on a missing target before resolving the toolchain
   TC-GATE                                          \ fail-closed identity: reject BEFORE consuming PTX
   PROC-ARGV-RESET
   SB-RESET s" -arch=" SB-APPEND TC-ARCH$ SB-APPEND SB$ >LEN PROC-ARGV+
   PTX$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   CUBIN$ >LEN PROC-ARGV+
   TC-PATH$ >LEN out outcap err errcap ASM-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   err ERR-P !                                      \ stderr buffer ptr is constant; store before MATCH
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} e LEN>N ERR-U !  0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} e LEN>N ERR-U !  c RC>N ENDOF
   ;MATCH ;

\ stderr from the last ASSEMBLE, so a failing assemble can be diagnosed not masked
: ERR$ ( -- ptr u8 n )  ERR-P @  ERR-U @ ;

\ surface a nonzero ptxas rc: type its captured stderr, pass rc through to the assert
: ASM-REPORT ( n -- n )
   dup 0= 0= if ERR$ type cr then ;

\ surface a spawned kernel-emit failure: nonzero child rc -> type its stderr, named throw
: EMIT-GUARD ( ptr u8 n n -- ) {: err:ptr erru:n rc:n :}
   rc 0= if exit then
   err erru type cr
   E-PTX-EMIT throw ;

;package
