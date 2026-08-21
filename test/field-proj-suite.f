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
\ binds to.
\
\ Sections:
\   1. positives: cell field, byte-offset field, and a generic-substituted field
\      projected from MAKE-built bundles in typed memory, each read back by value.
\   2. negatives (red-first): unarmed op, forged offset, offset past the family
\      width, non-layout pointer, foreign family, role/output mismatch, and a
\      malformed (uncommitted-id) arming — every one a checker reject (verdict 0).
\   3. seal: the arming word is not reachable by name from user source.
\ REPORT prints "ok" and exits 0, or F<index> + detail and exits 1.

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

require test/checker-assert.f
using TFAM


\ --- sealed friend boundary (dot habu-hb-crash-bare-c5be6634 idiom): the
\ field-projection window is armed only by the generative crossing, so its arming
\ word is a pre-hook internal that the seal marks non-executable. A whitebox test
\ reaches it through a named TRUSTED forwarder, exactly as structure-make-suite.f
\ forwards to SUMV-ADD / TFAM-DECL and type-layout-lower forwards to TFAM-FIND-IN.
TRUSTED: FP-ARM ( ptr u8 n n n -- ) FIELD-PROJ! ;
TRUSTED: FP-CLEAR ( -- ) FIELD-PROJ-CLEAR ;
TRUSTED: TWX-FAM ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;

\ field-id lookup helper (TYPE-FIELD:FIND is a public sealed-package API).
: FLD-ID ( n ptr u8 n -- n ) {: fam:n na:ptr nu:n :}   \ fam name$ -> committed field id
   fam TYPE-FIELD:NO-VARIANT na nu TYPE-FIELD:FIND 0= if
      s" field-proj-suite: field not found" 76 die then ;
: FAM-ID ( ptr u8 n -- n ) {: na:ptr nu:n :}   \ top-level family name -> id
   s" " na nu TWX-FAM 0= if s" field-proj-suite: family not found" 76 die then ;

\ --- runtime for the projection op. The checker intercepts `field-project` only
\ inside the armed window (replacing the effect below with the schema-aware
\ projection); the engine still compiles an ordinary call, so the runtime is a
\ plain pointer + byte-offset add. The generate-field lane supplies the same
\ contract in production. Outside the window the effect below is what a caller
\ gets, and it is exactly `+`: no retype, layout-fenced on a layout pointer.
: field-project ( ptr a n -- ptr a ) + ;

\ ===========================================================================
\ 1. positives
\ ===========================================================================
PRODUCT fprec 0
  FIELD a n
  FIELD b n
;PRODUCT
2 LAYOUT-BUFFER FP-BUF fprec

variable FPREC-FAM   variable FID-A   variable FID-B
s" fprec" FAM-ID FPREC-FAM !
FPREC-FAM @ s" a" FLD-ID FID-A !
FPREC-FAM @ s" b" FLD-ID FID-B !

\ cell field at offset 0
s" FPX-A" FID-A @ 0 FP-ARM
: FPX-A ( ptr fprec -- ptr n ) 0 field-project ;
\ byte-offset field at offset CELL
s" FPX-B" FID-B @ CELL FP-ARM
: FPX-B ( ptr fprec -- ptr n ) CELL field-project ;

\ MAKE + whole-bundle store happen in compiled words (a layout value cannot sit
\ on the interpret stack); the projected reads are scalars, so they surface fine.
: FP-STORE ( n n n -- ) {: av:n bv:n idx:n :} av bv FPREC:MAKE idx FP-BUF ! ;
: FP-GETA ( n -- n ) FP-BUF FPX-A @ ;
: FP-GETB ( n -- n ) FP-BUF FPX-B @ ;

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

\ generic-substituted field: `fpg<a>` field v:a projects as `ptr a`, and at a
\ concrete instantiation reads the stored value back.
PRODUCT fpg 1
  FIELD v a
;PRODUCT
1 LAYOUT-BUFFER FPG-BUF fpg<n>
variable FPG-FAM   variable FID-V
s" fpg" FAM-ID FPG-FAM !
FPG-FAM @ s" v" FLD-ID FID-V !
\ generic accessor certifies with the substituted output type
s" FPX-V" FID-V @ 0 FP-ARM
s" FPX-V ( ptr fpg<a> -- ptr a ) 0 field-project" CHECK-QUIET-CANDIDATE! -1 T=
\ define + run it at fpg<n>
s" FPX-V" FID-V @ 0 FP-ARM
: FPX-V ( ptr fpg<a> -- ptr a ) 0 field-project ;
: FPG-STORE ( n n -- ) {: vv:n idx:n :} vv FPG:MAKE idx FPG-BUF ! ;
: FPG-GET ( n -- n ) FPG-BUF FPX-V @ ;
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

\ ===========================================================================
\ 3. seal: user CHECKED source cannot arm the window. FIELD-PROJ! is a pre-hook
\ internal the seal marks non-executable, so a checked body that names it is
\ E-UNDEFINED — only the generative crossing (a TRUSTED friend forwarder, above)
\ reaches it, exactly like CTOR-PEND!.
\ ===========================================================================
\ Verdict 1 = uncheckable: the checker treats FIELD-PROJ! exactly like an
\ undefined word (an undefined tail returns 1 the same way), never certifying a
\ body that names it, so user source cannot arm the window; and the seal-time
\ internal-word pass additionally makes it non-executable, so even a forced build
\ could not call it.
s" FPX-FORGE ( ptr u8 n n n -- ) FIELD-PROJ!" CHECK-QUIET-CANDIDATE! 1 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" field-proj-suite: failures" 1 die ;
REPORT

;using
