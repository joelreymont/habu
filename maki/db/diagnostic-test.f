\ maki/db/diagnostic-test.f - acceptance for the structured Diagnostic IR
\ (maki/db/diagnostic.f + maki/db/diagnostic-render.f; dot
\ habu-v2-structured-diagnostic-18d24536).
\
\ Proves the § 23.9 / § 23.2 acceptance, each item by a named test:
\   DG-RT-*          : a rich diagnostic round-trips (field-for-field + byte-identical)
\   DG-NO-OWNER      : construction with no owner -> missing-owner (typed, not a throw)
\   DG-NO-REPRO      : construction with no reproduction -> missing-reproduction (typed)
\   DG-RENDER-*      : the human AND json renderers consume the SAME value
\   DG-EX-{A,B,C,D}-*: four realistic repo failure classes lower losslessly:
\                      A checker reject, B ptxas failure, C device-launch fault,
\                      D gate timeout - each proven byte-identical + field-for-field
\   DG-MALFORMED / -NONCANON / -DUP / -BOUNDS / -UNKNOWN-REQ : the decode-result
\                      reject taxonomy is reachable and typed
\
\ The test reopens package DIAG (a friend) to reach the private wire constants and
\ slot helpers for adversarial byte buffers; happy-path calls use the public API.
\ Identity fixtures mint real ids through their owner constructors
\ (PRODUCER:REGISTER, CONFIG:REGISTER, REV:COMMIT, ARTIFACT:REGISTER) - never a raw cast.

require lib/test.f
require maki/db/diagnostic.f
require maki/db/diagnostic-render.f
require maki/producer.f
require maki/config.f
require maki/rev.f
require maki/artifact.f

package DIAG

create DG-A 1024 allot
create DG-B 1024 allot
create DG-TB 512 allot
variable DG-TB-U
variable DG-D-SLOT                 \ the last round-tripped diagnostic slot

\ ---- result inspectors --------------------------------------------------------
: BUILD-CODE ( build-result -- n )         \ 0 ok, 1 missing-owner, 2 missing-repro
   MATCH build-result
      ok OF DIAG> drop 0 ENDOF
      missing-owner OF 1 ENDOF
      missing-reproduction OF 2 ENDOF
   ;MATCH ;

: OK-DIAG ( build-result -- diagnostic )   \ unwrap an expected ok build
   MATCH build-result
      ok OF ENDOF
      missing-owner OF 0 >DIAG ENDOF
      missing-reproduction OF 0 >DIAG ENDOF
   ;MATCH ;

: DEC-CODE ( ptr u8 n -- n )               \ 0 ok, else the taxonomy ordinal
   DECODE MATCH decode-result
      ok OF DIAG> drop 0 ENDOF
      malformed OF 1 ENDOF
      noncanonical OF 2 ENDOF
      bounds OF 3 ENDOF
      duplicate OF 4 ENDOF
      unknown-required OF 5 ENDOF
   ;MATCH ;

: DEC-SLOT ( ptr u8 n -- n )               \ decoded slot on ok, else -1
   DECODE MATCH decode-result
      ok OF DIAG> ENDOF
      malformed OF -1 ENDOF
      noncanonical OF -1 ENDOF
      bounds OF -1 ENDOF
      duplicate OF -1 ENDOF
      unknown-required OF -1 ENDOF
   ;MATCH ;

: BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr an:n b:ptr bn:n :}
   an bn <> if false exit then
   0 begin dup an < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ round-trip a built diagnostic through ENCODE/DECODE and stash the decoded slot.
: RT-STORE ( diagnostic -- ) {: d:diagnostic :}
   d DG-A 1024 ENCODE {: len:n :}
   DG-A len DEC-SLOT DG-D-SLOT ! ;

\ byte-identical re-encode: encode a diagnostic, decode it, re-encode, compare.
: BYTES-RT ( diagnostic -- bool ) {: d:diagnostic :}
   d DG-A 1024 ENCODE {: l1:n :}
   DG-A l1 DEC-SLOT >DIAG DG-B 1024 ENCODE {: l2:n :}
   DG-A l1 DG-B l2 BYTES= ;

\ ---- field readers over the last round-tripped diagnostic (DG-D-SLOT) ----------
: D ( -- diagnostic )         DG-D-SLOT @ >DIAG ;
: F-CODE ( -- n )             D CODE@ ;
: F-CLASS ( -- n )            D CLASS@ CLASS>N ;
: F-SEV ( -- n )              D SEVERITY@ SEV>N ;
: F-PHASE ( -- n )            D PHASE@ PHASE>N ;
: F-OWNER= ( ptr u8 n -- bool )  D OWNER@ PRODUCER:NAME$ STR= ;
: F-REPRO= ( ptr u8 n -- bool )  D REPRODUCTION@ STR= ;
: F-LOC= ( ptr u8 n -- bool )    D LOCATION@ STR= ;
: F-EXP-N ( -- n )            D EXPECTED-COUNT ;
: F-EXP0= ( ptr u8 n -- bool )   D 0 EXPECTED@ STR= ;
: F-OBS0= ( ptr u8 n -- bool )   D 0 OBSERVED@ STR= ;
: F-INVAL0= ( ptr u8 n -- bool ) D 0 INVALIDATED@ STR= ;
: F-CONE-N ( -- n )           D CONE-COUNT ;
: F-CONE= ( ptr u8 n n -- bool ) {: a:ptr u:n k:n :}   a u  D k CONE@ ARTIFACT:KEY$  STR= ;
: F-REPAIR-N ( -- n )         D REPAIR-COUNT ;
: F-REPAIR0 ( -- n )          D 0 REPAIR@ REPAIR>N ;
: F-HAS-SUBJECT ( -- bool )   D HAS-SUBJECT? ;
: F-SUBJECT= ( ptr u8 n -- bool ) D SUBJECT@ ARTIFACT:KEY$ STR= ;
: F-HAS-CX ( -- bool )        D HAS-COUNTEREXAMPLE? ;
: F-CX= ( ptr u8 n -- bool )  D COUNTEREXAMPLE@ ARTIFACT:KEY$ STR= ;
: F-HAS-ENV ( -- bool )       D HAS-ENVIRONMENT? ;
: F-ENV= ( ptr u8 n -- bool ) D ENVIRONMENT@ CONFIG:FACTS$ STR= ;
: F-HAS-REV ( -- bool )       D HAS-REVISION? ;
: F-REV= ( ptr u8 n -- bool ) D REVISION@ REV:CONTENT$ STR= ;
: F-HAS-PARENT ( -- bool )    D HAS-PARENT? ;
: F-PARENT ( -- n )           D PARENT@ ;
: F-HAS-PROGRESS ( -- bool )  D HAS-PROGRESS? ;
: F-PROGRESS ( -- n )         D PROGRESS@ ;

\ ---- a rich, everything-set diagnostic ----------------------------------------
: RICH ( -- build-result )
   NEW
   1234 CODE  DIAG-CLASS:NUMERIC CLASS  DIAG-SEVERITY:ERROR SEVERITY
   DIAG-PHASE:VERIFY PHASE
   s" numpolicy" PRODUCER:REGISTER OWNER
   s" hb --load x.f" REPRODUCTION
   s" node 42 region 3" LOCATION
   s" cfg-a" CONFIG:REGISTER ENVIRONMENT
   s" rev-abc123" REV:COMMIT REVISION
   s" rank=3" EXPECTED+  s" rank=2" OBSERVED+
   s" ev-golden-1" INVALIDATED+
   s" art-dep-1" ARTIFACT:REGISTER CONE+
   s" art-dep-2" ARTIFACT:REGISTER CONE+
   DIAG-REPAIR:CHECKER-FIX REPAIR+  DIAG-REPAIR:RETRY REPAIR+
   s" art-subject" ARTIFACT:REGISTER SUBJECT
   s" art-cx" ARTIFACT:REGISTER COUNTEREXAMPLE
   99 PARENT  7 PROGRESS
   BUILD ;

: RICH-DIAG ( -- diagnostic )   RICH OK-DIAG ;
: RICH-BYTES ( -- bool )   RICH-DIAG BYTES-RT ;
: RICH-STORE ( -- )        RICH-DIAG RT-STORE ;

T-RESET

\ ---- 1. rich round-trip: byte-identical + field-for-field ----------------------
RICH-BYTES TTRUE
RICH-STORE
F-CODE 1234 T=
F-CLASS 5 T=                                \ numeric
F-SEV 0 T=                                  \ error
F-PHASE 5 T=                                \ verify
s" numpolicy" F-OWNER= TTRUE
s" hb --load x.f" F-REPRO= TTRUE
s" node 42 region 3" F-LOC= TTRUE
s" rank=3" F-EXP0= TTRUE
s" rank=2" F-OBS0= TTRUE
s" ev-golden-1" F-INVAL0= TTRUE
F-CONE-N 2 T=
s" art-dep-1" 0 F-CONE= TTRUE
s" art-dep-2" 1 F-CONE= TTRUE
F-REPAIR-N 2 T=
F-REPAIR0 0 T=                              \ checker-fix
F-HAS-SUBJECT TTRUE
s" art-subject" F-SUBJECT= TTRUE
F-HAS-CX TTRUE
s" art-cx" F-CX= TTRUE
F-HAS-ENV TTRUE
s" cfg-a" F-ENV= TTRUE
F-HAS-REV TTRUE
s" rev-abc123" F-REV= TTRUE
F-HAS-PARENT TTRUE
F-PARENT 99 T=
F-HAS-PROGRESS TTRUE
F-PROGRESS 7 T=

\ ---- 2. validation: missing owner / reproduction reject (typed, not a throw) ----
: NO-OWNER ( -- n )
   NEW 1 CODE s" r" REPRODUCTION BUILD BUILD-CODE ;
NO-OWNER 1 T=

: NO-REPRO ( -- n )
   NEW 1 CODE s" p" PRODUCER:REGISTER OWNER BUILD BUILD-CODE ;
NO-REPRO 2 T=

\ owner present but empty reproduction still rejects on the reproduction check
: EMPTY-REPRO ( -- n )
   NEW 1 CODE s" p2" PRODUCER:REGISTER OWNER s" " REPRODUCTION BUILD BUILD-CODE ;
EMPTY-REPRO 2 T=

\ ---- 3. both renderers consume the SAME value ----------------------------------
: TEXT-HAS ( ptr u8 n -- bool )   RICH-DIAG RENDER 2swap CONTAINS? ;
: JSON-HAS ( ptr u8 n -- bool )   RICH-DIAG RENDER-JSON 2swap CONTAINS? ;

s" class: numeric" TEXT-HAS TTRUE
s" owner: numpolicy" TEXT-HAS TTRUE
s" reproduction: hb --load x.f" TEXT-HAS TTRUE
s" legal-repairs:" TEXT-HAS TTRUE
s\" \"code\":1234" JSON-HAS TTRUE
s\" \"class\":\"numeric\"" JSON-HAS TTRUE
s\" \"owner\":\"numpolicy\"" JSON-HAS TTRUE
s\" \"dependency_cone\":[\"art-dep-1\",\"art-dep-2\"]" JSON-HAS TTRUE
s\" \"parent\":99" JSON-HAS TTRUE

\ ---- 4. four realistic failure classes lower losslessly ------------------------
\ A: a checker reject (invariant, phase checker, owner "checker").
: EX-A ( -- diagnostic )
   NEW
   70 CODE  DIAG-CLASS:INVARIANT CLASS  DIAG-SEVERITY:ERROR SEVERITY
   DIAG-PHASE:CHECKER PHASE
   s" checker" PRODUCER:REGISTER OWNER
   s" bin/hb --load bad-square.f" REPRODUCTION
   s" SQUARE at token dup" LOCATION
   s" ( n -- n )" EXPECTED+  s" ( n -- n n )" OBSERVED+
   DIAG-REPAIR:CHECKER-FIX REPAIR+
   s" rev-9f3a" REV:COMMIT REVISION
   BUILD OK-DIAG ;
: A-BYTES ( -- bool )   EX-A BYTES-RT ;
: A-STORE ( -- )        EX-A RT-STORE ;
A-BYTES TTRUE
A-STORE
F-CODE 70 T=
F-CLASS 0 T=                                \ invariant
F-PHASE 0 T=                                \ checker
s" checker" F-OWNER= TTRUE
s" bin/hb --load bad-square.f" F-REPRO= TTRUE
s" SQUARE at token dup" F-LOC= TTRUE
s" ( n -- n )" F-EXP0= TTRUE
s" ( n -- n n )" F-OBS0= TTRUE
F-REPAIR0 0 T=                              \ checker-fix
s" rev-9f3a" F-REV= TTRUE

\ B: a ptxas (external tool) failure (external, phase codegen, owner "ptxas").
: EX-B ( -- diagnostic )
   NEW
   255 CODE  DIAG-CLASS:EXTERNAL CLASS  DIAG-SEVERITY:ERROR SEVERITY
   DIAG-PHASE:CODEGEN PHASE
   s" ptxas" PRODUCER:REGISTER OWNER
   s" ptxas -arch sm_87 gemm.ptx" REPRODUCTION
   s" kernel gemm_128x128" LOCATION
   s" ptxas fatal: unresolved symbol __smem" OBSERVED+
   DIAG-REPAIR:EXTERNAL-FIX REPAIR+
   s" sm87|cuda12" CONFIG:REGISTER ENVIRONMENT
   s" rev-b1ce" REV:COMMIT REVISION
   BUILD OK-DIAG ;
: B-BYTES ( -- bool )   EX-B BYTES-RT ;
: B-STORE ( -- )        EX-B RT-STORE ;
B-BYTES TTRUE
B-STORE
F-CODE 255 T=
F-CLASS 4 T=                                \ external
F-PHASE 6 T=                                \ codegen
s" ptxas" F-OWNER= TTRUE
s" ptxas fatal: unresolved symbol __smem" F-OBS0= TTRUE
F-REPAIR0 6 T=                              \ external-fix
F-HAS-ENV TTRUE
s" sm87|cuda12" F-ENV= TTRUE
s" rev-b1ce" F-REV= TTRUE

\ C: a device-launch fault (external, phase runtime, subject + counterexample).
: EX-C ( -- diagnostic )
   NEW
   700 CODE  DIAG-CLASS:EXTERNAL CLASS  DIAG-SEVERITY:BLOCKED SEVERITY
   DIAG-PHASE:RUNTIME PHASE
   s" cuda-driver" PRODUCER:REGISTER OWNER
   s" hb --load launch-mm.f" REPRODUCTION
   s" launch mm_kernel grid=64" LOCATION
   s" CUDA_ERROR_ILLEGAL_ADDRESS (700)" OBSERVED+
   DIAG-REPAIR:RETRY REPAIR+
   s" kernel-mm-sm87" ARTIFACT:REGISTER SUBJECT
   s" cx-illegal-idx" ARTIFACT:REGISTER COUNTEREXAMPLE
   s" rev-c2df" REV:COMMIT REVISION
   BUILD OK-DIAG ;
: C-BYTES ( -- bool )   EX-C BYTES-RT ;
: C-STORE ( -- )        EX-C RT-STORE ;
C-BYTES TTRUE
C-STORE
F-CODE 700 T=
F-CLASS 4 T=                                \ external
F-SEV 2 T=                                  \ blocked
F-PHASE 11 T=                               \ runtime
s" cuda-driver" F-OWNER= TTRUE
s" CUDA_ERROR_ILLEGAL_ADDRESS (700)" F-OBS0= TTRUE
F-HAS-SUBJECT TTRUE
s" kernel-mm-sm87" F-SUBJECT= TTRUE
F-HAS-CX TTRUE
s" cx-illegal-idx" F-CX= TTRUE
s" rev-c2df" F-REV= TTRUE

\ D: a gate timeout (resource, phase verify, dependency cone + parent + progress).
: EX-D ( -- diagnostic )
   NEW
   124 CODE  DIAG-CLASS:RESOURCE CLASS  DIAG-SEVERITY:BLOCKED SEVERITY
   DIAG-PHASE:VERIFY PHASE
   s" gate-runner" PRODUCER:REGISTER OWNER
   s" bin/hb --load maki/test.f" REPRODUCTION
   s" suite maki/db/diagnostic-test.f" LOCATION
   s" wall 600s exceeded" OBSERVED+
   s" ev-suite-green" INVALIDATED+
   s" art-x" ARTIFACT:REGISTER CONE+
   s" art-y" ARTIFACT:REGISTER CONE+
   DIAG-REPAIR:RETRY REPAIR+
   70 PARENT  3 PROGRESS
   s" rev-d3ea" REV:COMMIT REVISION
   BUILD OK-DIAG ;
: D-BYTES ( -- bool )   EX-D BYTES-RT ;
: D-STORE ( -- )        EX-D RT-STORE ;
D-BYTES TTRUE
D-STORE
F-CODE 124 T=
F-CLASS 3 T=                                \ resource
F-PHASE 5 T=                                \ verify
s" gate-runner" F-OWNER= TTRUE
s" wall 600s exceeded" F-OBS0= TTRUE
s" ev-suite-green" F-INVAL0= TTRUE
F-CONE-N 2 T=
s" art-x" 0 F-CONE= TTRUE
s" art-y" 1 F-CONE= TTRUE
F-HAS-PARENT TTRUE
F-PARENT 70 T=
F-HAS-PROGRESS TTRUE
F-PROGRESS 3 T=
s" rev-d3ea" F-REV= TTRUE

\ ---- 5. decode reject taxonomy is reachable and typed --------------------------
\ malformed: truncate a valid envelope.
: DG-MALFORMED ( -- n )
   RICH-DIAG DG-A 1024 ENCODE {: len:n :}
   DG-A len 1-  DEC-CODE ;
DG-MALFORMED 1 T=

\ unknown-required: a valid envelope + an unknown REQUIRED field.
: DG-UNKNOWN-REQ ( -- n )
   RICH-DIAG DG-A 1024 ENCODE {: len:n :}
   200 DG-A len + c!                        \ tag 200 (> known max)
   FLAG-REQUIRED DG-A len 1+ + c!           \ flags = required
   0 DG-A len 2 + + U32W LE-PUT             \ len = 0
   DG-A len HDR-W + U32W + DEC-CODE ;
DG-UNKNOWN-REQ 5 T=

\ ---- hand-built adversarial buffers (private wire constants) --------------------
: TBB-RST ( -- )   0 DG-TB-U ! ;
: TBB-U8 ( n -- ) {: c:n :}   c DG-TB DG-TB-U @ + c!  DG-TB-U @ 1+ DG-TB-U ! ;
: TBB-LE ( n n -- ) {: v:n w:n :}   v DG-TB DG-TB-U @ + w LE-PUT  DG-TB-U @ w + DG-TB-U ! ;
: TBB-U64F ( n n n -- ) {: tag:n flags:n v:n :}   \ tag flags value: a length-8 scalar field
   tag TBB-U8  flags TBB-U8  U64W U32W TBB-LE  v U64W TBB-LE ;
: TBB$ ( -- ptr u8 n )   DG-TB DG-TB-U @ ;

\ noncanonical: TAG-CLASS emitted before TAG-CODE (descending tags).
: DG-NONCANON ( -- n )
   TBB-RST
   TAG-CLASS FLAG-REQUIRED 5 TBB-U64F
   TAG-CODE  FLAG-REQUIRED 1 TBB-U64F
   TBB$ DEC-CODE ;
DG-NONCANON 2 T=

\ duplicate: TAG-CODE emitted twice.
: DG-DUP ( -- n )
   TBB-RST
   TAG-CODE FLAG-REQUIRED 1 TBB-U64F
   TAG-CODE FLAG-REQUIRED 1 TBB-U64F
   TBB$ DEC-CODE ;
DG-DUP 4 T=

\ bounds: a scalar field with a non-8 declared length.
: DG-BOUNDS ( -- n )
   TBB-RST
   TAG-CODE TBB-U8  FLAG-REQUIRED TBB-U8
   4 U32W TBB-LE                             \ declared len 4 (a u64 scalar must be 8)
   0 U32W TBB-LE
   TBB$ DEC-CODE ;
DG-BOUNDS 3 T=

T-REPORT

;package
