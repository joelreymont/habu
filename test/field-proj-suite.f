\ field-proj-suite.f — checker field-projection capability (dot
\ habu-checker-type-structure-d996215b, docs/type-families.md §2.2). Run BY THE
\ ENGINE over stdin, like test/structure-make-suite.f:
\     bin/hb < test/field-proj-suite.f
\
\ Proves the sealed, schema-aware FIELD-PROJECTION armed window that mints
\ `ptr <field-type>` from a `ptr family<args>` input and a committed field id —
\ the one shape the ordinary layout fence refuses. The generate-field lane will
\ emit accessor words that call the `field-project` op inside this window; this
\ suite drives the op directly under a test-armed window (arming through a
\ TRUSTED forwarder to the sealed FIELD-PROJ!, exactly as the whitebox suites
\ reach CTOR-PEND! / TFAM-FIND-IN), so it pins the exact contract the generator
\ binds to. The forwarder, the lookups and the accessors are
\ test/field-proj-lib.f's, which test/field-proj-boundary-child.f shares.
\
\ Sections:
\   1. positives: cell field, byte-offset field, and a generic-substituted field
\      projected from MAKE-built bundles in typed memory, each read back by value.
\   2. negatives (red-first): unarmed op, forged offset, offset past the family
\      width, non-layout pointer, foreign family, role/output mismatch, and a
\      malformed (uncommitted-id) arming — every one a checker reject (verdict 0).
\ That checked source cannot arm the window itself is
\ test/field-proj-boundary-child.f's (FPX-FORGE).
\ REPORT prints "ok" and exits 0, or F<index> + detail and exits 1.

require lib/fmt.f                        \ FMT:.INT - one-line number text

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
   then ;

require test/checker-assert.f
require test/field-proj-lib.f
using FIELD-PROJ-LIB

\ ===========================================================================
\ 1. positives
\ ===========================================================================
\ store {a=10,b=20} at slot 0, project each field, read the exact value back
10 20 0 FP-STORE
0 FP-GETA 10 T=
0 FP-GETB 20 T=
\ a second slot stays independent
30 40 1 FP-STORE
1 FP-GETA 30 T=
1 FP-GETB 40 T=
0 FP-GETA 10 T=

\ pointer-role field: projecting a `ptr u8` field yields `ptr ptr u8`
PRODUCT fpptr 0
  FIELD p ptr u8
  FIELD k n
;PRODUCT
variable FPPTR-FAM   variable FID-P
s" fpptr" FAM-ID FPPTR-FAM !
FPPTR-FAM @ s" p" FLD-ID FID-P !
s" FPX-P" FID-P @ 0 FP-ARM
s" FPX-P ( ptr fpptr -- ptr ptr u8 ) 0 field-project" CHECK-QUIET-CANDIDATE! -1 T=

\ generic-substituted field: the generic accessor certifies with the substituted
\ output type, and at fpg<n> reads the stored value back.
s" FPX-V" FID-V @ 0 FP-ARM
s" FPX-V ( ptr fpg<a> -- ptr a ) 0 field-project" CHECK-QUIET-CANDIDATE! -1 T=
42 0 FPG-STORE
0 FPG-GET 42 T=

\ ===========================================================================
\ 2. negatives (red-first): every projection reject is verdict 0
\ ===========================================================================
\ outside the window: the op is inert (no arming) — a layout pointer is fenced.
FP-CLEAR
s" FPX-U ( ptr fprec -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=

\ forged offset: the baked offset disagrees with the committed offset of the id.
s" FPX-WO" FID-A @ CELL FP-ARM
s" FPX-WO ( ptr fprec -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=

\ offset past the family width (also disagrees with the committed offset).
s" FPX-PW" FID-A @ 999 FP-ARM
s" FPX-PW ( ptr fprec -- ptr n ) 999 field-project" CHECK-QUIET-CANDIDATE! 0 T=

\ projection on a non-layout pointer: the input pointee is a scalar, not a family.
s" FPX-NL" FID-A @ 0 FP-ARM
s" FPX-NL ( ptr n -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=

\ foreign family: field id from fprec applied to a pointer of another family.
PRODUCT fprec2 0
  FIELD a n
;PRODUCT
s" FPX-FF" FID-A @ 0 FP-ARM
s" FPX-FF ( ptr fprec2 -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=

\ value/pointer role confusion: field p is a POINTER (`ptr u8`), so its
\ projection is `ptr ptr u8`; declaring a scalar output `ptr n` rejects.
s" FPX-RO" FID-P @ 0 FP-ARM
s" FPX-RO ( ptr fpptr -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=
\ wrong scalar type: field a is `n` (integer cell), declaring `ptr r` (real)
\ rejects — the projected `ptr n` does not coerce to `ptr r`.
s" FPX-RS" FID-A @ 0 FP-ARM
s" FPX-RS ( ptr fprec -- ptr r ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=

\ malformed arming: an uncommitted field id fails closed (E-PF-ID caught).
s" FPX-BID" TYPE-FIELD:COUNT 100 + 0 FP-ARM
s" FPX-BID ( ptr fprec -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 T=
FP-CLEAR

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" field-proj-suite: failures" 1 die ;
REPORT

;using
