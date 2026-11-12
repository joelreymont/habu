\ program-diagnostics-test.f - direct production-load diagnostic contracts.

require test/gate-common.f
require src/lib/string.f
require src/lib/fmt.f

package PROGRAM-DIAGNOSTICS

: RUN-LOAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   path pathu GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV ;

\ The Habu loop reads the file and recovers at `;` (src/habu/interpreter.f
\ DEF-SEMI?), as it does for RUN-LOAD.
: RUN-LOAD-HABU ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/interpreter-loop-on.f" GE-ARG+
   path pathu GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV ;

: EXPECT-MARKER ( ptr u8 n ptr u8 n -- )
   {: marker:ptr markeru:n label:ptr labelu:n :}
   SB-RESET marker markeru SB-APPEND GE-SB-LF
   SB$ label labelu GE-EXPECT-OUT ;

: EXPECT-OK-END ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-OUT$ {: out:ptr outu:n :}
   outu 3 < if label labelu GE-FAIL then
   out outu 3 - + 2 s" ok" STR= 0= if label labelu GE-FAIL then
   out outu 1- + c@ GE-LF <> if label labelu GE-FAIL then ;

: NEGATIVE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n rc:n marker:ptr markeru:n needle:ptr needleu:n :}
   path pathu RUN-LOAD
   rc path pathu GE-EXPECT-RC
   marker markeru path pathu EXPECT-MARKER
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

: EXPECT-DIAGNOSTIC ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu GE-EXPECT-OK
   path pathu EXPECT-OK-END
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

: DIAGNOSTIC ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu RUN-LOAD
   path pathu needle needleu EXPECT-DIAGNOSTIC ;

: HABU-DIAGNOSTIC ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu RUN-LOAD-HABU
   path pathu needle needleu EXPECT-DIAGNOSTIC ;

: ADDRESS-CAP ( -- )
   s" test/address-cell-cap-bad.f" 96
      s" ADDRESS-CELL-CAP-ARMED" s" hb: invalid address-cell storage header" NEGATIVE
   GT-ERR$ S\" hb: invalid address-cell storage header\n" STR= 0= if
      s" address cell capacity diagnostic" GE-FAIL then ;

: NEGATIVES ( -- )
   s" test/address-cell-kind-bad.f" 99
      s" ADDRESS-CELL-KIND-ARMED" s" hb: snapshot address cell kind mismatch" NEGATIVE
   ADDRESS-CAP
   s" test/xt-cell-band-bad.f" 98
      s" XT-CELL-BAND-ARMED" s" hb: snapshot address cell out of range" NEGATIVE
   s" test/checker-decl-depth0-bad.f" 76
      s" CHECKER-DECL-DEPTH0-ARMED" s" declaration rollback failed" NEGATIVE
   s" test/enum-ctor-collide-bad.f"
      s" name is reserved or already taken" DIAGNOSTIC
   s" test/deftype-dup-bad.f" 70
      s" DEFTYPE-DUP-ARMED" s" duplicate family" NEGATIVE
   s" test/layout-buffer-forge.f" 70
      s" LAYOUT-BUFFER-FORGE-ARMED" s" undefined word 'LBUF-PEND!'" NEGATIVE
   s" test/layout-valid-w1-bad.f" 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-w1-neg-bad.f" 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-product-bad.f" 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-active-bad.f" 85
      s" LAYOUT-VALID-ACTIVE-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-root-bad.f" 85
      s" LAYOUT-VALID-ROOT-ARMED" s" hb: bad layout tag" NEGATIVE
   \ The trap ends the load's process, so its exit code and stderr are what is checked.
   s" test/layout-valid-part-bad.f" 85
      s" LAYOUT-VALID-PART-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-tags-bad.f" 85
      s" LAYOUT-VALID-TAGS-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-hook-forge.f" 70
      s" LAYOUT-VALID-FORGE-ARMED" s" undefined word 'LAYOUT-VALID-RECORD-XT'" NEGATIVE
   s" test/layout-valid-walk-forge.f" 70
      s" LAYOUT-VALID-FORGE-ARMED" s" undefined word 'VP-TERM-WALK-XT'" NEGATIVE
   s" test/layout-valid-desc-forge.f" 70
      s" LAYOUT-VALID-FORGE-ARMED" s" undefined word 'LAYOUT-VALID-DESC-XT'" NEGATIVE
   s" test/bootstrap-created-effect-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" actual: ptr a" NEGATIVE
   s" test/bootstrap-created-raw-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" expected: nom-id actual: a" NEGATIVE
   s" test/bootstrap-created-const-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" expected: kon-id actual: a" NEGATIVE
   s" test/bootstrap-created-does-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" expected: dow-id actual: a" NEGATIVE ;

\ Each refused signature of a multi-error load is counted once and written once:
\ a second report of one would be a second line on stderr and a count of 2.
: BADSIG-ONCE ( -- )
   s" test/program-diagnostic-badsig.f" {: path:ptr pathu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   GT-OUT$ S\" 1\n1\n1\n0\n1\nok\n" STR= 0= if s" badsig counts" GE-FAIL then
   SB-RESET
   S\" habu: in pdb-colon: unknown type 'zz' in signature; suggestion: Use a known stack-signature type or a single-letter type variable.\n" SB-APPEND
   S\" habu: in pdb-trusted: bad stored signature '-- zz'\n" SB-APPEND
   S\" habu: in pdb-defer: bad stored signature '-- zz'\n" SB-APPEND
   S\" habu: in pdb-native: bad stored signature '-- zz'\n" SB-APPEND
   GT-ERR$ SB$ STR= 0= if s" badsig reports" GE-FAIL then ;

: BADSIG-WIDE-ONCE ( -- )
   s" test/program-diagnostic-wide-native.f" {: path:ptr pathu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   GT-OUT$ S\" 0\n1\nok\n" STR= 0= if s" wide badsig count" GE-FAIL then
   s" habu: in pdb-wide: bad stored signature" path pathu GE-EXPECT-ERR-HAS
   s" input row too wide to record (256 cells, at most 255)" path pathu GE-EXPECT-ERR-HAS ;

\ A multi-error load counts the refusals it renders: a quiet scan's refusal is
\ neither printed nor counted, so the count is 1 and stderr holds BADC's line.
: QUIET-COUNT-ONCE ( -- )
   s" test/program-diagnostic-quiet-count.f" {: path:ptr pathu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   GT-OUT$ S\" 0\n1\n1\nok\n" STR= 0= if s" quiet count" GE-FAIL then
   SB-RESET
   s" habu: in badc: at 'drop' input underflow: the call takes more cells than " SB-APPEND
   s" the definition's declared inputs leave (needs 1, has 0); suggestion: " SB-APPEND
   s" Push the missing inputs before the call, or declare them in the signature; " SB-APPEND
   S\" a definition may not consume below its declared inputs.\n" SB-APPEND
   GT-ERR$ SB$ STR= 0= if s" quiet count reports" GE-FAIL then ;

: DIAGNOSTICS ( -- )
   BADSIG-ONCE
   BADSIG-WIDE-ONCE
   QUIET-COUNT-ONCE
   s" test/program-diagnostic-me.f" s" habu: in mea1:" DIAGNOSTIC
   s" test/native-multi-error-recovery.f" s" habu: in bad:" HABU-DIAGNOSTIC
   s" native-multi-error-recovery.f: at 'c@' expected: ptr u8 actual: n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/native-multi-error-recovery.f" s" habu: in bad:" DIAGNOSTIC
   s" native-multi-error-recovery.f: at 'c@' expected: ptr u8 actual: n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-ctor.f" s" duplicate family" DIAGNOSTIC
   s" test/program-diagnostic-using.f" s" hb: using: unknown package:" DIAGNOSTIC
   s" test/program-diagnostic-export.f" s" habu: in xpu3:" DIAGNOSTIC
   s" test/program-diagnostic-lower.f" s" habu: in lc-named-bad:" DIAGNOSTIC ;

public

: TEST ( -- )
   s" habu-program-diagnostics" GT-START
   NEGATIVES
   DIAGNOSTICS
   GT-CLEANUP
   s" program-diagnostics-test: ok" type cr ;

;package

PROGRAM-DIAGNOSTICS:TEST
