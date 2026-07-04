\ maki/sched-key-test.f - checked tests for the cad-4 schedule keys + replay table.
\ Shape-class bucketing, the FNV-1a region signature (determinism + sensitivity), the
\ full section 7.4 key string, the alignment-class field, and the cad-5 replay-table
\ seam (miss -> defaults, hit roundtrip, update-in-place, fail-closed overflow/region).

require lib/test.f
require lib/string.f
require maki/sched-key.f   \ transitively brings fusion-plan + model-ir (float before fmt)

package MAKI

\ stable copy (the shared SB builder is overwritten by the next render) ---------
256 constant KT-CAP                       \ holds a full section-7.4 key (~120 bytes)
create KT-BUF KT-CAP allot  variable KT-BU
: KT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a KT-BUF u BYTE-COPY  u KT-BU ! ;
: KT-BUF$ ( -- ptr u8 n )  KT-BUF KT-BU @ ;

\ ---- IR builders (one gelu/relu elementwise chain over a single input) ------
: BUILD ( n n -- ) {: rows:n cols:n :}
   MIR-RESET
   rows cols DT-F32 LAY-ROW MIR-INPUT+ drop
   OP-GELU MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ rows cols DT-F32 LAY-ROW 0 1 MIR-OP+ drop
   OP-RELU MIR-OP-BEGIN 0 MIR-IN+        rows cols DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;
: BUILD-DT ( n n n -- ) {: rows:n cols:n dt:n :}   \ same chain, chosen input dtype
   MIR-RESET
   rows cols dt LAY-ROW MIR-INPUT+ drop
   OP-GELU MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ rows cols dt LAY-ROW 0 1 MIR-OP+ drop
   OP-RELU MIR-OP-BEGIN 0 MIR-IN+        rows cols dt LAY-ROW 0 1 MIR-OP+ drop ;

T-RESET

\ ---- shape class: exact <= 64, else pow2 bucket + tail flag, ? for unbound ---
2   4   SK-SHAPE-CLASS$ s" 2x4"           T$=
64  64  SK-SHAPE-CLASS$ s" 64x64"         T$=
2   100 SK-SHAPE-CLASS$ s" 2xp128+t"      T$=
65  1   SK-SHAPE-CLASS$ s" p128+tx1"      T$=
128 128 SK-SHAPE-CLASS$ s" p128xp128"     T$=
256 256 SK-SHAPE-CLASS$ s" p256xp256"     T$=
200 300 SK-SHAPE-CLASS$ s" p256+txp512+t" T$=
0   65  SK-SHAPE-CLASS$ s" ?xp128+t"      T$=

\ ---- key fields: honest v1 constants + the real engine content key -----------
SK-TARGET$ s" sm_87"    T$=
SK-ENGINE$ ENGINE-KEY$  T$=              \ engine field is the real bin/hb content key
SK-ENGINE$ nip 64       T=              \ a 64-char SHA-256 hex digest, not a placeholder
SK-PTXAS$  s" unprobed"  T$=

\ ---- full section 7.4 key over a built region ------------------------------
2 100 BUILD  0 AL-16 MIR-SLOT-AL!  FP-BUILD
0 SK-RSIG$ s" 431E24867468A764" T$=                 \ deterministic FNV-1a signature
\ exact full-key equality: copy the actual out (SK-KEY$ builds in the shared SB
\ builder), then splice the binary-dependent engine key into the expected string.
0 SK-KEY$ KT-COPY
SB-RESET
s" 431E24867468A764|2xp128+t|f32|row|al16|sm_87|" SB-APPEND
ENGINE-KEY$ SB-APPEND  s" |unprobed" SB-APPEND
KT-BUF$ SB$ STR= TTRUE
0 SK-ALIGN$ s" al16" T$=

\ ---- alignment class falls back to al? for an unrecorded input --------------
2 100 BUILD  FP-BUILD
0 SK-ALIGN$ s" al?" T$=

\ ---- signature determinism + sensitivity -----------------------------------
2 100 BUILD FP-BUILD  0 SK-RSIG$ KT-COPY                       \ baseline (f32) signature
2 100 BUILD FP-BUILD  0 SK-RSIG$ KT-BUF$ STR= TTRUE            \ same facts -> identical
2 100 DT-F16 BUILD-DT FP-BUILD  0 SK-RSIG$ KT-BUF$ STR= TFALSE \ dtype change -> different

\ ---- replay table: cad-5 store seam ----------------------------------------
2 100 BUILD 0 AL-16 MIR-SLOT-AL! FP-BUILD
SK-TAB-RESET
0 SK-KEY$ SK-GET nip TFALSE                          \ miss -> not found
0 SK-KEY$ SK-GET drop -1 T=                          \ miss selection is -1 (use defaults)
SK-TAB-COUNT 0 T=
0 SK-KEY$ 7 SK-PUT
0 SK-KEY$ SK-GET nip  TTRUE                          \ now found
0 SK-KEY$ SK-GET drop 7 T=                           \ roundtrips the stored selection
SK-TAB-COUNT 1 T=
0 SK-KEY$ 9 SK-PUT                                   \ same key -> update in place
SK-TAB-COUNT 1 T=
0 SK-KEY$ SK-GET drop 9 T=

\ ---- fail-closed throws -----------------------------------------------------
: PUTN ( n -- ) {: n:n :}  SB-RESET s" k" SB-APPEND n SB-INT SB$ n SK-PUT ;
: TRY-FULL   ( -- )  SK-TAB-RESET 33 0 ?do i PUTN loop ;   \ 33 > SK-TAB-CAP (32)
: TRY-REGION ( -- )  99 SK-KEY$ 2drop ;                     \ region 99 out of range
: TRY-ALIGN  ( -- )  AL-N AL-KEY 2drop ;                    \ AL-N is out of the AL-* domain
' TRY-FULL   E-SK-FULL   TTHROWS
' TRY-REGION E-SK-REGION TTHROWS
' TRY-ALIGN  E-SK-ALIGN  TTHROWS

T-REPORT

end-package
