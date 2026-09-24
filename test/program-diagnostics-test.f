\ program-diagnostics-test.f - direct production-load diagnostic contracts.

require test/gate-common.f
require lib/fmt.f

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

\ The same assertion for a refusal that says more than its die line. The AOT
\ capture writes the coordinate it refused - an offset, a row, a range - with `type`
\ ahead of the `die` that carries the message on stderr (src/habu/aot-capture.f), so
\ its stdout is the marker line FOLLOWED by those lines rather than the marker
\ alone. Requiring the marker as a PREFIX keeps what the exact form proves - the
\ case armed, and printed nothing before it - and admits the detail a reader needs.
: EXPECT-MARKER-FIRST ( ptr u8 n ptr u8 n -- )
   {: marker:ptr markeru:n label:ptr labelu:n :}
   SB-RESET marker markeru SB-APPEND GE-SB-LF
   GT-OUT$ {: out:ptr outu:n :}
   SB$ {: want:ptr wantu:n :}
   outu wantu < if label labelu GE-FAIL then
   out wantu want wantu STR= 0= if label labelu GE-FAIL then ;

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

: NEGATIVE-DIAG ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n rc:n marker:ptr markeru:n needle:ptr needleu:n :}
   path pathu RUN-LOAD
   rc path pathu GE-EXPECT-RC
   marker markeru path pathu EXPECT-MARKER-FIRST
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

: DIAGNOSTIC ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   path pathu EXPECT-OK-END
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

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
   s" test/aot-address-cell-lower-straddle-bad.f" 74
      s" AOT-XTCELL-LOWER-STRADDLE-ARMED" s" aot-capture: declared address cell straddles the window edge" NEGATIVE-DIAG
   s" test/aot-address-cell-upper-straddle-bad.f" 74
      s" AOT-XTCELL-UPPER-STRADDLE-ARMED" s" aot-capture: declared address cell straddles the window edge" NEGATIVE-DIAG
   s" test/aot-address-cell-target-out-bad.f" 74
      s" AOT-XTCELL-TARGET-OUT-ARMED" s" aot-capture: declared address target is not self-contained" NEGATIVE-DIAG
   s" test/aot-address-cell-off-grid-bad.f" 74
      s" AOT-XTCELL-OFF-GRID-ARMED" s" aot-capture: a declared DATA cell is not on the window cell grid" NEGATIVE-DIAG
   s" test/aot-window-base-off-grid-bad.f" 74
      s" AOT-WINDOW-BASE-OFF-GRID-ARMED" s" aot-capture: the captured DATA window base is not cell-aligned" NEGATIVE
   s" test/checker-decl-nested-bad.f" 76
      s" CHECKER-DECL-NESTED-ARMED" s" checker: declaration rollback frame mismatch" NEGATIVE
   s" test/checker-decl-depth0-bad.f" 76
      s" CHECKER-DECL-DEPTH0-ARMED" s" checker: declaration rollback frame mismatch" NEGATIVE
   s" test/enum-ctor-collide-bad.f"
      s" name is reserved or already taken" DIAGNOSTIC
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
