\ maki/sched-key-test.f - checked tests for the cad-4 schedule keys + replay table.
\ Shape-class bucketing, the FNV-1a region signature (determinism + sensitivity), the
\ full section 7.4 key string, the alignment-class field, and the cad-5 replay-table
\ seam (miss -> defaults, hit roundtrip, update-in-place, fail-closed overflow/region).

require lib/test.f
require lib/string.f
require test/checker-assert.f   \ CHECK-QUIET-CANDIDATE! for the swapped-family negatives
require maki/sched-key.f   \ transitively brings fusion-plan + model-ir (float before fmt)

package MAKI

\ stable copy (the shared SB builder is overwritten by the next render) ---------
256 constant KT-CAP                       \ holds a full section-7.4 key (~120 bytes)
create KT-BUF KT-CAP allot  variable KT-BU
: KT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a KT-BUF u BYTE-COPY  u KT-BU ! ;
: KT-BUF$ ( -- ptr u8 n )  KT-BUF KT-BU @ ;
: KT-ALT-TARGET ( -- CAD-KIND:target-id )
   s" sm_87-noasync"
   TARGET:ISA-PTX 87 32 1024 49152
   TARGET:CAP-ALL TARGET:CAP-ASYNC invert and TARGET:DESCRIPTOR
   TARGET:REGISTER ;

\ ---- IR builders (one gelu/relu elementwise chain over a single input) ------
: BUILD ( n n -- ) {: rows:n cols:n :}
   MIR-RESET
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MAKI-OPKIND:RELU MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;
\ same chain, chosen input dtype; the dtype family cannot bind into a local, so
\ it rides the stack into MIR-INPUT+ and the nodes read it back from the slot
: BUILD-DT ( n n dtype -- )
   MIR-RESET
   >r SHAPE r> MAKI-LAYOUT:ROW MIR-INPUT+ drop
   0 MIR-SLOT-ID MIR-SLOT-ROWS@ 0 MIR-SLOT-ID MIR-SLOT-COLS@
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   rows cols 0 MIR-SLOT-ID MIR-SLOT-DT@ MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MAKI-OPKIND:RELU MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
   rows cols 0 MIR-SLOT-ID MIR-SLOT-DT@ MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

T-RESET

\ ---- shape class: exact <= 64, else pow2 bucket + tail flag, ? for unbound ---
2   4   SHAPE SK-SHAPE-CLASS$ s" 2x4"           T$=
64  64  SHAPE SK-SHAPE-CLASS$ s" 64x64"         T$=
2   100 SHAPE SK-SHAPE-CLASS$ s" 2xp128+t"      T$=
65  1   SHAPE SK-SHAPE-CLASS$ s" p128+tx1"      T$=
128 128 SHAPE SK-SHAPE-CLASS$ s" p128xp128"     T$=
256 256 SHAPE SK-SHAPE-CLASS$ s" p256xp256"     T$=
200 300 SHAPE SK-SHAPE-CLASS$ s" p256+txp512+t" T$=
0   65  SHAPE SK-SHAPE-CLASS$ s" ?xp128+t"      T$=

\ ---- dimclass: typed field identity == rendered single-dim text identity -----
\ Encode each extent as (dimclass, magnitude) through DIM>CLASS and prove the
\ pair's equality is EXACTLY the render's text equality across the bucket-domain
\ boundaries (63/64/65, 128, non-pow2 tail, 0/unbound). This is what licenses the
\ text-keyed replay table as the durable in-memory mirror of the typed key.
: DIM-MAG ( e -- n )        DIM>CLASS nip ;              \ magnitude (drop the class)
: DIM-CLS ( e -- dimclass ) DIM>CLASS drop ;            \ class (drop the magnitude)
: DIM-FEQ ( e e -- bool ) {: a:n b:n :}                 \ typed field equality
   a DIM-CLS b DIM-CLS MAKI-DIMCLASS:EQ
   a DIM-MAG b DIM-MAG = and ;
: DIM-TXT= ( e e -- bool ) {: a:n b:n :}                 \ rendered single-dim text equality
   a 1 SHAPE SK-SHAPE-CLASS$ KT-COPY  b 1 SHAPE SK-SHAPE-CLASS$ KT-BUF$ STR= ;
\ field-eq and text-eq return the SAME verdict on both sides of every bucket
\ boundary -> the (dimclass, magnitude) encoding IS rendered-text identity:
63  64  DIM-FEQ TFALSE     63  64  DIM-TXT= TFALSE    \ exact/63 vs exact/64
64  65  DIM-FEQ TFALSE     64  65  DIM-TXT= TFALSE    \ exact/64 vs pow2-tail/128 (<=64 boundary)
65  66  DIM-FEQ TTRUE      65  66  DIM-TXT= TTRUE     \ both pow2-tail/128 (same bucket)
128 127 DIM-FEQ TFALSE     128 127 DIM-TXT= TFALSE    \ pow2/128 vs pow2-tail/128 (class differs)
128 256 DIM-FEQ TFALSE     128 256 DIM-TXT= TFALSE    \ pow2/128 vs pow2/256 (magnitude differs)
0   0   DIM-FEQ TTRUE      0   0   DIM-TXT= TTRUE      \ unbound vs unbound
0   64  DIM-FEQ TFALSE     0   64  DIM-TXT= TFALSE    \ unbound vs exact/64
64  64  DIM-FEQ TTRUE      64  64  DIM-TXT= TTRUE      \ exact vs exact, same magnitude

\ ---- key fields: honest v1 constants + the real engine content key -----------
TARGET:SM87 SK-TARGET$ s" sm_87"    T$=
SK-ENGINE$ ENGINE-KEY$  T$=              \ engine field is the real bin/hb content key
SK-ENGINE$ nip 64       T=              \ a 64-char SHA-256 hex digest, not a placeholder
SK-PTXAS$  s" unprobed"  T$=

\ ---- full section 7.4 key over a built region ------------------------------
2 100 BUILD  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!  FP-BUILD
0 FP-REGION-ID SK-RSIG$ s" 431E24867468A764" T$=    \ deterministic FNV-1a signature
\ exact full-key equality: copy the actual out (SK-KEY$ builds in the shared SB
\ builder), then splice the binary-dependent engine key into the expected string.
0 FP-REGION-ID TARGET:SM87 SK-KEY$ KT-COPY
SB-RESET
s" 431E24867468A764|2xp128+t|f32|row|al16|sm_87|" SB-APPEND
ENGINE-KEY$ SB-APPEND  s" |unprobed" SB-APPEND
KT-BUF$ SB$ STR= TTRUE
0 FP-REGION-ID KT-ALT-TARGET SK-KEY$ KT-BUF$ STR= TFALSE   \ a different target -> different durable key
0 FP-REGION-ID SK-ALIGN$ s" al16" T$=

\ ---- typed schedule key (skey product): construction, identity, field roles --
\ The region-0 build above (2x100, f32, row, a16) is still live. SK-KEY reads the
\ same eight facts SK-KEY+ renders; these asserts live inside : definitions
\ because a top-level word cannot push a layout value onto the stack.
: SK-SELF-EQ ( n -- bool ) {: r:n :}
   r FP-REGION-ID SK-KEY  r FP-REGION-ID SK-KEY  MAKI-SKEY:EQ ;  \ determinism
0 SK-SELF-EQ TTRUE
\ every field of SK-KEY matches the region facts: compare to an explicitly-built
\ expected key (rsig from the golden above, dims 2x100 -> exact/2 x pow2-tail/128,
\ df32 / row / a16). Any wrong field would make MAKI-SKEY:EQ false.
: SK-EXPECT ( -- skey )
   $431e24867468a764 MAKI-DIMCLASS:EXACT 2 MAKI-DIMCLASS:POW2-TAIL 128
   MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI-ALIGN:A16 MAKI-SKEY:MAKE ;
: SK-MATCHES ( n -- bool )  FP-REGION-ID SK-KEY SK-EXPECT MAKI-SKEY:EQ ;
0 SK-MATCHES TTRUE
\ MAKI-SKEY:EQ discriminates every semantic field: one differing field -> unequal.
: SK-BASE ( -- skey )
   7 MAKI-DIMCLASS:EXACT 2 MAKI-DIMCLASS:POW2-TAIL 128
   MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI-ALIGN:A16 MAKI-SKEY:MAKE ;
: SK-DT-DIFF ( -- skey )      \ only dtype differs
   7 MAKI-DIMCLASS:EXACT 2 MAKI-DIMCLASS:POW2-TAIL 128
   MAKI-DTYPE:DF16 MAKI-LAYOUT:ROW MAKI-ALIGN:A16 MAKI-SKEY:MAKE ;
: SK-LAY-DIFF ( -- skey )     \ only layout differs
   7 MAKI-DIMCLASS:EXACT 2 MAKI-DIMCLASS:POW2-TAIL 128
   MAKI-DTYPE:DF32 MAKI-LAYOUT:COL MAKI-ALIGN:A16 MAKI-SKEY:MAKE ;
: SK-AL-DIFF ( -- skey )      \ only align differs
   7 MAKI-DIMCLASS:EXACT 2 MAKI-DIMCLASS:POW2-TAIL 128
   MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI-ALIGN:A8 MAKI-SKEY:MAKE ;
: SK-RK-DIFF ( -- skey )      \ only rows shape class differs (exact vs pow2)
   7 MAKI-DIMCLASS:POW2 2 MAKI-DIMCLASS:POW2-TAIL 128
   MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI-ALIGN:A16 MAKI-SKEY:MAKE ;
\ the EQ comparisons ride inside : definitions (interpret mode cannot hold skey):
: SK-EQ-SELF   ( -- bool )  SK-BASE SK-BASE     MAKI-SKEY:EQ ;
: SK-EQ-DT     ( -- bool )  SK-BASE SK-DT-DIFF  MAKI-SKEY:EQ ;
: SK-EQ-LAY    ( -- bool )  SK-BASE SK-LAY-DIFF MAKI-SKEY:EQ ;
: SK-EQ-AL     ( -- bool )  SK-BASE SK-AL-DIFF  MAKI-SKEY:EQ ;
: SK-EQ-RK     ( -- bool )  SK-BASE SK-RK-DIFF  MAKI-SKEY:EQ ;
SK-EQ-SELF TTRUE
SK-EQ-DT   TFALSE
SK-EQ-LAY  TFALSE
SK-EQ-AL   TFALSE
SK-EQ-RK   TFALSE

\ ---- swapped-family negatives: a role swap at MAKE is a CHECKER reject --------
\ MAKI-SKEY:MAKE ( n dimclass n dimclass n dtype layout align -- skey ). A dtype
\ in the layout slot (and vice-versa), or a bare n laundered into an enum column,
\ rejects in BOTH directions; the positive control certifies.
s" SKP  ( n dimclass n dimclass n dtype layout align -- skey ) MAKI-SKEY:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SKN1 ( n dimclass n dimclass n layout dtype align -- skey ) MAKI-SKEY:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SKN2 ( n dimclass n dimclass n n layout align -- skey ) MAKI-SKEY:MAKE"     CHECK-QUIET-CANDIDATE! 0 T=
s" SKN3 ( n dimclass n dimclass n dtype layout n -- skey ) MAKI-SKEY:MAKE"     CHECK-QUIET-CANDIDATE! 0 T=
s" SKN4 ( n n n dimclass n dtype layout align -- skey ) MAKI-SKEY:MAKE"        CHECK-QUIET-CANDIDATE! 0 T=

\ ---- alignment class falls back to al? for an unrecorded input --------------
2 100 BUILD  FP-BUILD
0 FP-REGION-ID SK-ALIGN$ s" al?" T$=

\ ---- signature determinism + sensitivity -----------------------------------
2 100 BUILD FP-BUILD  0 FP-REGION-ID SK-RSIG$ KT-COPY               \ baseline (f32) signature
2 100 BUILD FP-BUILD  0 FP-REGION-ID SK-RSIG$ KT-BUF$ STR= TTRUE    \ same facts -> identical
2 100 MAKI-DTYPE:DF16 BUILD-DT FP-BUILD  0 FP-REGION-ID SK-RSIG$ KT-BUF$ STR= TFALSE \ dtype change -> different

\ ---- replay table: cad-5 store seam ----------------------------------------
2 100 BUILD 0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL! FP-BUILD
SK-TAB-RESET
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET nip TFALSE      \ miss -> not found
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop -1 T=      \ miss selection is -1 (use defaults)
SK-TAB-COUNT 0 T=
0 FP-REGION-ID TARGET:SM87 SK-KEY$ 7 SK-PUT
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET nip  TTRUE      \ now found
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 7 T=       \ roundtrips the stored selection
SK-TAB-COUNT 1 T=
0 FP-REGION-ID TARGET:SM87 SK-KEY$ 9 SK-PUT               \ same key -> update in place
SK-TAB-COUNT 1 T=
0 FP-REGION-ID TARGET:SM87 SK-KEY$ SK-GET drop 9 T=

\ ---- fail-closed throws -----------------------------------------------------
: PUTN ( n -- ) {: n:n :}  SB-RESET s" k" SB-APPEND n SB-INT SB$ n SK-PUT ;
: TRY-FULL   ( -- )  SK-TAB-RESET 33 0 ?do i PUTN loop ;   \ 33 > SK-TAB-CAP (32)
: TRY-REGION ( -- )  99 FP-REGION-ID TARGET:SM87 SK-KEY$ 2drop ;   \ region 99 rejects at the id refinement
: TRY-ALIGN  ( -- )  AL-N AL-KEY 2drop ;                    \ AL-N is out of the AL-* domain
: TRY->ALIGN ( -- )  AL-N >ALIGN drop ;                     \ the n->align lift is fail-closed too
' TRY-FULL   E-SK-FULL   TTHROWS
' TRY-REGION E-FP-IDX    TTHROWS
' TRY-ALIGN  E-SK-ALIGN  TTHROWS
' TRY->ALIGN E-TV-ALIGN  TTHROWS

\ ---- a STALE region id (held across a rebuild) rejects at sched-key's guard --
\ Hold region 1 of a 2-region plan (the standalone concat materializes as its
\ own region), rebuild a 1-region plan, then replay the held id: SK-REGION-CK
\ revalidates against the CURRENT plan and throws E-SK-REGION.
variable KT-STALE-R
: KT-STALE-SET ( -- )  1 FP-REGION-ID KT-STALE-R ! ;
: KT-TRY-STALE ( -- )  KT-STALE-R @ TARGET:SM87 SK-KEY$ 2drop ;
: KT-BUILD-2R ( -- )    \ gelu (region 0) + matmul (region 1: EW->MM is backend-refused)
   MIR-RESET
   4 8 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   8 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   4 8 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MAKI-OPKIND:MATMUL MIR-OP-BEGIN
   0 MIR-NODE-ID MIR-NODE-REF MIR-IN+  1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   4 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 2 MIR-OP+ drop ;
KT-BUILD-2R FP-BUILD
FP-REGION-COUNT 2 T=
KT-STALE-SET
2 100 BUILD FP-BUILD                     \ rebuild: one region; the held id is now out of range
' KT-TRY-STALE E-SK-REGION TTHROWS

T-REPORT

;package
