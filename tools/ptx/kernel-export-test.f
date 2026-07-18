\ kernel-export-test.f - focused tests for the kernel artifact export.
\
\ In-process negatives run the library words directly (they throw before any
\ producer include, so the shared gate image stays clean); the positive path
\ spawns the real CLI twice into two private out-dirs and byte-compares both
\ artifact pairs (determinism is the build-step contract), then pins key
\ manifest and PTX fragments.

require lib/ptx/process-test-prelude.f
require lib/fs-mutate.f
require tools/ptx/kernel-export-lib.f

package KEXPORT-TEST

$2000 constant CAP-CAP
$1000 constant ART-CAP
20000 constant SPAWN-MS

create SPAWN-OUT CAP-CAP allot
create SPAWN-ERR CAP-CAP allot
create DIR-A-BUF FS-PATH-CAP allot
create DIR-B-BUF FS-PATH-CAP allot
create PATH-BUF FS-PATH-CAP allot
create ART-1 ART-CAP allot
create ART-2 ART-CAP allot

variable DIR-A-U
variable DIR-B-U

: DIR-A$ ( -- ptr u8 n )  DIR-A-BUF DIR-A-U @ ;
: DIR-B$ ( -- ptr u8 n )  DIR-B-BUF DIR-B-U @ ;

: TMP! ( ptr u8 n ptr u8 ptr a -- )    \ mkdir a private tmp dir, keep its path
   {: prefix:ptr prefixu:n dst:ptr lenp:ptr :}
   prefix prefixu TMPDIR-MKDIR {: ta:ptr tu:n :}
   tu FS-PATH-CAP > if E-FS-PATH throw then
   ta dst tu BYTE-COPY
   tu lenp ! ;

\ spawn the real CLI; attribute a failing child before asserting rc 0
: SPAWN-EXPORT ( ptr u8 n -- ) {: oa:ptr ou:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/ptx/kernel-export.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" SAXPY" >LEN PROC-ARGV+
   oa ou >LEN PROC-ARGV+
   s" bin/hb" >LEN SPAWN-OUT CAP-CAP >LEN SPAWN-ERR CAP-CAP >LEN SPAWN-MS >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 0 0 T= ENDOF               \ clean exit -> rc 0 passes
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
        s" kernel-export-test: CLI spawn failed, stderr:" type cr
        SPAWN-ERR e LEN>N type cr
        c RC>N 0 T= ENDOF                                         \ nonzero completion -> assert fails
   ;MATCH ;

: ART-READ ( ptr u8 n ptr u8 n ptr u8 -- ptr u8 n )   \ dir file dst -> artifact bytes
   {: oa:ptr ou:n fa:ptr fu:n dst:ptr :}
   oa ou fa fu PATH-BUF JOIN-PATH {: pu:n :}
   dst  PATH-BUF pu dst ART-CAP READ-ALL ;

: PAIR= ( ptr u8 n -- )                \ artifact name: dir-a copy == dir-b copy
   {: fa:ptr fu:n :}
   DIR-A$ fa fu ART-1 ART-READ {: a1:ptr u1:n :}
   DIR-B$ fa fu ART-2 ART-READ {: a2:ptr u2:n :}
   u1 0 > TTRUE
   a1 u1 a2 u2 T$= ;

: PINNED-CHECK ( -- )                  \ pinned artifact content (dir-a copies)
   DIR-A$ s" SAXPY.ptx" ART-1 ART-READ {: xa:ptr xu:n :}
   xa 12 s" .version 8.3" T$=
   xa xu s" .visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" CONTAINS? TTRUE
   DIR-A$ s" SAXPY.manifest.json" ART-2 ART-READ {: ma:ptr mu:n :}
   ma mu S\" \"schema\":\"habu-kernel-manifest\",\"version\":1,\"name\":\"SAXPY\"" CONTAINS? TTRUE
   ma mu S\" \"grid_derivation\":\"ceil-n-256\"" CONTAINS? TTRUE
   ma mu S\" \"param_bytes\":24" CONTAINS? TTRUE
   ma mu S\" \"param\":\"p_n\",\"offset\":20,\"size\":4,\"ptx_type\":\".u32\",\"role\":\"len\"" CONTAINS? TTRUE ;

\ --- negatives (throw before any producer include) ---

: KXT-UNKNOWN ( -- )
   s" NOPE" s" ." KEXPORT:EXPORT ;

: KXT-NODIR ( -- )
   s" SAXPY" s" /nonexistent-habu-kexport-outdir" KEXPORT:EXPORT ;

: KXT-FILE-AS-DIR ( -- )
   s" SAXPY" s" tools/ptx/kernel-export.f" KEXPORT:EXPORT ;

T-RESET

' KXT-UNKNOWN E-KEXPORT-KERNEL TTHROWS
' KXT-NODIR E-KEXPORT-OUTDIR TTHROWS
' KXT-FILE-AS-DIR E-KEXPORT-OUTDIR TTHROWS

\ --- determinism: two CLI runs write byte-identical artifact pairs ---
s" habu-kexport-a" DIR-A-BUF DIR-A-U TMP!
s" habu-kexport-b" DIR-B-BUF DIR-B-U TMP!
DIR-A$ SPAWN-EXPORT
DIR-B$ SPAWN-EXPORT
s" SAXPY.ptx" PAIR=
s" SAXPY.manifest.json" PAIR=

PINNED-CHECK

DIR-A$ REMOVE-TREE
DIR-B$ REMOVE-TREE

T-REPORT

;package
