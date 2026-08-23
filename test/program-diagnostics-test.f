\ program-diagnostics-test.f - direct production-load diagnostic contracts.

require test/gate-common.f

package PROGRAM-DIAGNOSTICS

: RUN-LOAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
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

: DIAGNOSTIC ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   path pathu EXPECT-OK-END
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

: NEGATIVES ( -- )
   s" test/xt-cell-band-bad.f" 98
      s" XT-CELL-BAND-ARMED" s" hb: snapshot address cell out of range" NEGATIVE
   s" test/checker-decl-nested-bad.f" 76
      s" CHECKER-DECL-NESTED-ARMED" s" checker: declaration rollback frame mismatch" NEGATIVE
   s" test/checker-decl-depth0-bad.f" 76
      s" CHECKER-DECL-DEPTH0-ARMED" s" checker: declaration rollback frame mismatch" NEGATIVE
   s" test/enum-ctor-collide-bad.f" 76
      s" ENUM-CTOR-COLLIDE-ARMED" s" sumtype: generated declaration already defined" NEGATIVE
   s" test/deftype-dup-bad.f" 67
      s" DEFTYPE-DUP-ARMED" s" duplicate family" NEGATIVE
   s" test/layout-buffer-forge.f" 70
      s" LAYOUT-BUFFER-FORGE-ARMED" s" E-UNDEFINED: LBUF-PEND!" NEGATIVE
   s" test/layout-valid-w1-bad.f" 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-product-bad.f" 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-active-bad.f" 85
      s" LAYOUT-VALID-ACTIVE-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-root-bad.f" 85
      s" LAYOUT-VALID-ROOT-ARMED" s" hb: bad layout tag" NEGATIVE
   s" test/layout-valid-hook-forge.f" 70
      s" LAYOUT-VALID-FORGE-ARMED" s" E-UNDEFINED: LAYOUT-VALID-RECORD-XT" NEGATIVE
   s" test/layout-valid-walk-forge.f" 70
      s" LAYOUT-VALID-FORGE-ARMED" s" E-UNDEFINED: VP-TERM-WALK-XT" NEGATIVE
   s" test/layout-valid-desc-forge.f" 70
      s" LAYOUT-VALID-FORGE-ARMED" s" E-UNDEFINED: LAYOUT-VALID-DESC-XT" NEGATIVE
   s" test/bootstrap-created-effect-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" actual: ptr a" NEGATIVE
   s" test/bootstrap-created-raw-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" expected: nom-id<> actual: a" NEGATIVE
   s" test/bootstrap-created-const-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" expected: kon-id<> actual: a" NEGATIVE
   s" test/bootstrap-created-does-src.f" 70
      s" BOOTSTRAP-CREATED-ARMED" s" expected: dow-id<> actual: a" NEGATIVE ;

: DIAGNOSTICS ( -- )
   s" test/engine-suite.f" s" habu: in mea1:" DIAGNOSTIC
   s" test/type-ctor-suite.f" s" duplicate family" DIAGNOSTIC
   s" test/using-test.f" s" hb: using: unknown package:" DIAGNOSTIC
   s" test/type-export-suite.f" s" habu: in xpu3:" DIAGNOSTIC
   s" test/lower-cert.f" s" habu: in lc-named-bad:" DIAGNOSTIC ;

public

: TEST ( -- )
   s" habu-program-diagnostics" GT-START
   NEGATIVES
   DIAGNOSTICS
   GT-CLEANUP
   s" program-diagnostics-test: ok" type cr ;

;package

PROGRAM-DIAGNOSTICS:TEST
