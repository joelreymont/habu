\ toolchain.f - checked PTX artifact paths and assembler runner.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/adt/option.f             \ option<n>: TRY-PTXAS$ reports a resolved ptxas as present/absent

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

\ ---- version-aware resolution (dot habu-pin-blackwell-grade-8ec5ee0a) ---------
\ system CUDA 13.0's ptxas ships an immature sm_121 scheduler that issues every
\ HMMA at a fixed yield-set interval (the 40-NOP steady body), costing ~27% of
\ GEMM tensor-core throughput; ptxas 13.3 schedules the same UNMODIFIED PTX with
\ zero NOPs. The 13.3 assembler is pinned into Habu's own tool store; when only
\ an older ptxas resolves for an sm_121 target the degradation is announced, not
\ hidden - the older assembler still emits element-exact kernels, proven.

1000 constant VER-MUL                           \ version encoding: MAJOR*VER-MUL + MINOR
13 VER-MUL * 3 + constant PTXAS-PINNED-VER       \ 13.3, the Blackwell-grade sm_121 scheduler
char . constant VER-SEP                          \ '.' MAJOR/MINOR separator in `ptxas --version`
2 constant TC-STDERR-FD

512 constant VER-CAP                             \ `ptxas --version` stdout capture
256 constant VER-ECAP
create VER-BUF VER-CAP allot
create VER-ERR VER-ECAP allot

create STORE-BUF FS-PATH-CAP allot               \ $HOME/.habu/toolchain/ptxas-13.3.33, built once
variable STORE-U

variable VER-CACHE                                \ resolved ptxas version, gated by VER-PROBED
variable VER-PROBED
variable STALE-WARNED                             \ latch: the stale-toolchain diagnostic fires at most once

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

\ Spawn `<path> --version`, capture stdout, parse the release version; -1 if the
\ probe fails or does not parse (unknown, never fatal).
: PTXAS-VERSION ( ptr u8 n -- n ) {: a:ptr u:n :}
   PROC-ARGV-RESET
   s" --version" >LEN PROC-ARGV+
   a u >LEN VER-BUF VER-CAP >LEN VER-ERR VER-ECAP >LEN ASM-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} VER-BUF o LEN>N PARSE-VER ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} -1 ENDOF
   ;MATCH ;

\ Is the configured assembler arch an sm_121-family target? (the profile whose
\ scheduler quality the 13.3 pin protects).
: SM121-TARGET? ( -- bool )
   TC-ARCH-U @ 0= if STR-FALSE exit then
   TC-ARCH-BUF TC-ARCH-U @ s" sm_121" CONTAINS? ;

\ Should the stale-toolchain diagnostic fire for this resolved version? Only for
\ an sm_121 target with a KNOWN version older than the pin.
: STALE-SM121? ( n -- bool ) {: ver :}
   SM121-TARGET? 0= if STR-FALSE exit then
   ver 0 < if STR-FALSE exit then
   ver PTXAS-PINNED-VER < ;

: TC-STDERR-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   TC-STDERR-FD a u write drop
   TC-STDERR-FD S\" \n" write drop ;

\ Announce the sm_121 scheduling penalty ONCE on stderr and name the dot that
\ records the root cause. Never throws: the degraded assembler must be visible.
: WARN-STALE-PTXAS ( n -- ) {: ver :}
   STALE-WARNED @ if exit then
   ver STALE-SM121? 0= if exit then
   STR-TRUE STALE-WARNED !
   s" hb: PTXAS-STALE-SM121: resolved ptxas is older than 13.3; sm_121 GEMM loses ~27% of tensor-core throughput to the 13.0 scheduler (40-NOP HMMA yield-sets). Kernels stay element-exact. Fix: provision ~/.habu/toolchain/ptxas-13.3.33 or set PTXAS. Root cause: dot habu-pin-blackwell-grade-8ec5ee0a." TC-STDERR-LINE ;

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

\ Probe ptxas without failing: the PTXAS env override wins (trusted, as before -
\ set means chosen); then Habu's own pinned 13.3 tool store; then the CUDA 13
\ install (its ptxas knows sm_121a); then the legacy 12.6 path, each existence-
\ checked with FILE?. SOME len parks the winning path pointer in PTXAS-P; NONE
\ means no ptxas resolved on any known path. The path list lives here only. No
\ throws - callers decide whether absence is fatal.
: TRY-PTXAS$ ( -- option<n> )
   s" PTXAS" GETENV dup 0 > if PTXAS-FOUND exit then 2drop
   STORE-PTXAS$ dup 0 > if 2dup FILE? if PTXAS-FOUND exit then then 2drop
   s" /usr/local/cuda/bin/ptxas"      2dup FILE? if PTXAS-FOUND exit then 2drop
   s" /usr/local/cuda-12.6/bin/ptxas" 2dup FILE? if PTXAS-FOUND exit then 2drop
   OPTION:NONE ;

\ Resolve ptxas, fail closed: the single place that turns an unresolved ptxas into
\ a loud named throw. No silent dead-path default.
: PTXAS$ ( -- ptr u8 n )
   TRY-PTXAS$ MATCH option
     none OF E-PTXTC-PTXAS throw ENDOF
     some OF PTXAS-P @ swap ENDOF
   ;MATCH ;

\ Read the resolved ptxas version once, then serve it from cache so the good path
\ never re-spawns `--version`.
: RESOLVED-VERSION ( -- n )
   VER-PROBED @ if VER-CACHE @ exit then
   PTXAS$ PTXAS-VERSION dup VER-CACHE !  STR-TRUE VER-PROBED ! ;

\ Before assembling for an sm_121 target, warn (once) if only a pre-13.3 ptxas
\ resolved. Cheap after the first check: non-sm_121 and post-warn calls short out.
: PTXAS-VERSION-CHECK ( -- )
   STALE-WARNED @ if exit then
   SM121-TARGET? 0= if exit then
   RESOLVED-VERSION WARN-STALE-PTXAS ;

: ASSEMBLE ( ptr u8 len ptr u8 len -- n )
   {: out:ptr outcap:len err:ptr errcap:len :}
   PTXAS-VERSION-CHECK
   PROC-ARGV-RESET
   SB-RESET s" -arch=" SB-APPEND TC-ARCH$ SB-APPEND SB$ >LEN PROC-ARGV+
   PTX$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   CUBIN$ >LEN PROC-ARGV+
   PTXAS$ >LEN out outcap err errcap ASM-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
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
