\ program-diagnostics-test.f - direct production-load diagnostic contracts.

require src/core/exit-status.f
require test/gate-common.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/process.f

package PROGRAM-DIAGNOSTICS

: RUN-LOAD ( ^u8 n -- ) {: path:^ pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   path pathu GE-ARG+
   s" bin/hb" PROCESS:WAIT-FOREVER MS>N GE-RUN-ENV ;

\ The Habu loop reads the file and recovers at `;` (src/habu/interpreter.f
\ DEF-SEMI?), as it does for RUN-LOAD.
: RUN-LOAD-HABU ( ^u8 n -- ) {: path:^ pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/interpreter-loop-on.f" GE-ARG+
   path pathu GE-ARG+
   s" bin/hb" PROCESS:WAIT-FOREVER MS>N GE-RUN-ENV ;

: EXPECT-MARKER ( ^u8 n ^u8 n -- )
   {: marker:^ markeru:n label:^ labelu:n :}
   SB-RESET marker markeru SB-APPEND GE-SB-LF
   SB$ label labelu GE-EXPECT-OUT ;

: EXPECT-OK-END ( ^u8 n -- ) {: label:^ labelu:n :}
   GT-OUT$ {: out:^ outu:n :}
   outu 3 < if label labelu GE-FAIL then
   out outu 3 - + 2 s" ok" STR= 0= if label labelu GE-FAIL then
   out outu 1- + c@ GE-LF <> if label labelu GE-FAIL then ;

: NEGATIVE ( ^u8 n n ^u8 n ^u8 n -- )
   {: path:^ pathu:n rc:n marker:^ markeru:n needle:^ needleu:n :}
   path pathu RUN-LOAD
   rc path pathu GE-EXPECT-RC
   marker markeru path pathu EXPECT-MARKER
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

: EXPECT-DIAGNOSTIC ( ^u8 n ^u8 n -- )
   {: path:^ pathu:n needle:^ needleu:n :}
   path pathu GE-EXPECT-OK
   path pathu EXPECT-OK-END
   needle needleu path pathu GE-EXPECT-ERR-HAS ;

: DIAGNOSTIC ( ^u8 n ^u8 n -- )
   {: path:^ pathu:n needle:^ needleu:n :}
   path pathu RUN-LOAD
   path pathu needle needleu EXPECT-DIAGNOSTIC ;

: HABU-DIAGNOSTIC ( ^u8 n ^u8 n -- )
   {: path:^ pathu:n needle:^ needleu:n :}
   path pathu RUN-LOAD-HABU
   path pathu needle needleu EXPECT-DIAGNOSTIC ;

\ stderr is one engine exit line: the text, " lr 0x", the lr's 16 hex digits
\ and a newline.
: EXPECT-DIE-LINE ( ^u8 n ^u8 n -- )
   {: text:^ textu:n label:^ labelu:n :}
   SB-RESET text textu SB-APPEND s"  lr 0x" SB-APPEND
   SB$ {: want:^ wantu:n :}
   GT-ERR$ {: err:^ erru:n :}
   erru wantu 17 + <> if label labelu GE-FAIL then
   err wantu want wantu STR= 0= if label labelu GE-FAIL then
   true erru 1- wantu ?do err i + c@ STR-HEX-DIGIT? and loop
   0= if label labelu GE-FAIL then
   err erru + 1- c@ GE-LF <> if label labelu GE-FAIL then ;

: NEGATIVE-LINE ( ^u8 n n ^u8 n ^u8 n -- )
   {: path:^ pathu:n rc:n marker:^ markeru:n text:^ textu:n :}
   path pathu rc marker markeru text textu NEGATIVE
   text textu path pathu EXPECT-DIE-LINE ;

\ A bootstrap-created word's cell refused where its signature promises another
\ type: the record names both.
: CREATED-MISMATCH ( ^u8 n ^u8 n ^u8 n -- )
   {: path:^ pathu:n want:^ wantu:n found:^ foundu:n :}
   path pathu 70 s" BOOTSTRAP-CREATED-ARMED" s" error[E-MISMATCH]" NEGATIVE
   want wantu path pathu GE-EXPECT-ERR-HAS
   found foundu path pathu GE-EXPECT-ERR-HAS ;

\ How many rejection records stderr holds.
: RECORDS ( -- n )
   GT-ERR$ s" error[" COUNT-SUB ;

: NEGATIVES ( -- )
   s" test/address-cell-kind-bad.f" EXIT-STATUS:SNAPSHOT s" ADDRESS-CELL-KIND-ARMED"
      s" E-XTKIND habu: snapshot address cell kind mismatch" NEGATIVE-LINE
   s" test/address-cell-cap-bad.f" EXIT-STATUS:SNAPSHOT s" ADDRESS-CELL-CAP-ARMED"
      s" E-XTCELL habu: invalid address-cell storage header" NEGATIVE-LINE
   s" test/xt-cell-band-bad.f" EXIT-STATUS:SNAPSHOT s" XT-CELL-BAND-ARMED"
      s" E-XTBAND habu: snapshot address cell out of range" NEGATIVE-LINE
   s" test/enum-ctor-collide-bad.f" s" enum-ctor-collide-bad.f:18:1" DIAGNOSTIC
   s" error[E-BAD-DECLARATION]" s" test/enum-ctor-collide-bad.f" GE-EXPECT-ERR-HAS
   s" ^ generated declaration already defined" s" test/enum-ctor-collide-bad.f"
      GE-EXPECT-ERR-HAS
   s" = evaluated: ENUM ecol green red ;ENUM"
      s" test/enum-ctor-collide-bad.f" GE-EXPECT-ERR-HAS
   s" = word: ECOL-RED" s" test/enum-ctor-collide-bad.f" GE-EXPECT-ERR-HAS
   s" test/deftype-dup-bad.f" 70
      s" DEFTYPE-DUP-ARMED" s" duplicate family" NEGATIVE
   s" test/bootstrap-created-effect-src.f"
      S\" = expected: (nothing)\n" S\" = found: ^a\n" CREATED-MISMATCH
   s" test/bootstrap-created-raw-src.f"
      S\" = expected: nom-id\n" S\" = found: a\n" CREATED-MISMATCH
   s" test/bootstrap-created-const-src.f"
      S\" = expected: kon-id\n" S\" = found: a\n" CREATED-MISMATCH
   s" test/bootstrap-created-does-src.f"
      S\" = expected: dow-id\n" S\" = found: a\n" CREATED-MISMATCH ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ The trap ends the load's process, so its exit code and stderr are what is checked.
: BAD-TAGS ( -- )
   s" test/layout-valid-w1-bad.f" 85
      s" LAYOUT-VALID-ARMED"
      s" E-BAD-TAG habu: layout tag 2 outside 0..1" NEGATIVE
   s" test/layout-valid-w1-neg-bad.f" 85
      s" LAYOUT-VALID-ARMED"
      s" E-BAD-TAG habu: layout tag -1 outside 0..1" NEGATIVE
   s" test/layout-valid-product-bad.f" 85
      s" LAYOUT-VALID-ARMED"
      s" E-BAD-TAG habu: layout tag 2 outside 0..1" NEGATIVE
   s" test/layout-valid-active-bad.f" 85
      s" LAYOUT-VALID-ACTIVE-ARMED"
      s" E-BAD-TAG habu: layout tag 2 outside 0..1" NEGATIVE
   s" test/layout-valid-root-bad.f" 85
      s" LAYOUT-VALID-ROOT-ARMED"
      s" E-BAD-TAG habu: layout tag 2 outside 0..1" NEGATIVE ;
[then]

\ Each rejected signature of a multi-error load is counted once and written once:
\ a second report of one would be a fourth record on stderr and a count of 2.
: BADSIG-ONCE ( -- )
   s" test/program-diagnostic-badsig.f" {: path:^ pathu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   GT-OUT$ S\" 1\n1\n1\nok\n" STR= 0= if s" badsig counts" GE-FAIL then
   RECORDS 3 <> if s" badsig reports" GE-FAIL then
   s" error[E-UNKNOWN-SIGNATURE-TYPE]: unknown type `zz` in signature" path pathu GE-EXPECT-ERR-HAS
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `PDB-TRUSTED`" path pathu GE-EXPECT-ERR-HAS
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `PDB-DEFER`" path pathu GE-EXPECT-ERR-HAS ;

\ A multi-error load counts the rejections it renders: a quiet scan's rejection
\ is neither printed nor counted, so the count is 1 and stderr holds one record.
: QUIET-COUNT-ONCE ( -- )
   s" test/program-diagnostic-quiet-count.f" {: path:^ pathu:n :}
   path pathu RUN-LOAD
   path pathu GE-EXPECT-OK
   GT-OUT$ S\" 0\n1\n1\nok\n" STR= 0= if s" quiet count" GE-FAIL then
   RECORDS 1 <> if s" quiet count reports" GE-FAIL then
   s" error[E-INPUT-UNDERFLOW]" path pathu GE-EXPECT-ERR-HAS
   s" (needs 1, has 0)" path pathu GE-EXPECT-ERR-HAS ;

\ A computed 0 that a top-level call throws stops its text with one record at
\ the call: the line after it never runs and hb exits 67. A literal 0 there is
\ rejected before the line runs.
: THROW-ZERO-STOPS ( -- )
   s" throw-zero" {: label:^ labelu:n :}
   GE-HB-RESET  GE-SRC-RESET
   s" : Z ( n -- ) throw ;" GE-SRC-LINE
   S\" s\" \" nip Z" GE-SRC-LINE
   S\" s\" after\" type cr" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN
   67 label labelu GE-EXPECT-RC
   s" " label labelu GE-EXPECT-OUT
   GT-ERR$ s" error[E-THROW-ZERO]: a computed 0 was thrown"
   STARTS-WITH? 0= if label labelu GE-FAIL then
   S\" \n --> <input>:2:10\n" label labelu GE-EXPECT-ERR-HAS
   s" throw a computed code only when it is nonzero: `rc 0<> if rc throw then`"
   label labelu GE-EXPECT-ERR-HAS ;

\ A catch that answered 0 lands no throw, so its code thrown again stops the text
\ with E-THROW-ZERO's record at the call, not one an earlier caught throw kept,
\ and hb exits 67.
: CAUGHT-ZERO-STOPS ( -- )
   s" caught-zero" {: label:^ labelu:n :}
   GE-HB-RESET  GE-SRC-RESET
   S\" : K ( -- ) s\" E-KEPT\" REJECTION:OPEN  REJECTION:SAVE  1000 REJECTION:PASS ;" GE-SRC-LINE
   s" : ON ( -- ) [: K ;] catch drop  [: ;] catch {: rc:n :} rc throw ;" GE-SRC-LINE
   s" ON" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN
   67 label labelu GE-EXPECT-RC
   GT-ERR$ s" error[E-THROW-ZERO]: a computed 0 was thrown"
   STARTS-WITH? 0= if label labelu GE-FAIL then
   S\" \n --> <input>:3:1\n" label labelu GE-EXPECT-ERR-HAS ;

\ An evaluate that returns gives the place back to the outer text: a later throw
\ in the same word is placed at the outer token, not in the evaluated string.
: THROW-AFTER-EVAL ( -- )
   s" throw-after-eval" {: label:^ labelu:n :}
   GE-HB-RESET  GE-SRC-RESET
   s" : Z ( n -- ) throw ;" GE-SRC-LINE
   S\" : G ( -- ) s\" 1 drop\" evaluate-closed  s\" \" nip Z ;" GE-SRC-LINE
   s" G" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN
   67 label labelu GE-EXPECT-RC
   s" " label labelu GE-EXPECT-OUT
   GT-ERR$ s" error[E-THROW-ZERO]: a computed 0 was thrown"
   STARTS-WITH? 0= if label labelu GE-FAIL then
   S\" \n --> <input>:3:1\n" label labelu GE-EXPECT-ERR-HAS ;

: GENERATED-DUPLICATE ( -- )
   s" generated-duplicate" {: label:^ labelu:n :}
   GE-HB-RESET  GE-SRC-RESET
   s" package PD-GEN" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" STRUCTURE box FIELD x n ;STRUCTURE" GE-SRC-LINE
   s" : BOX-MAKE ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN
   70 label labelu GE-EXPECT-RC
   s" error[E-DUPLICATE-DEFINITION]" label labelu GE-EXPECT-ERR-HAS
   s" = generated by: type `box`" label labelu GE-EXPECT-ERR-HAS
   s" = help: rename this word, or the type `box`, which generates it"
   label labelu GE-EXPECT-ERR-HAS ;

\ A public word may share a private generated word's spelling, so its own
\ duplicate names no type.
: PUBLIC-DUPLICATE ( -- )
   s" public-duplicate" {: label:^ labelu:n :}
   GE-HB-RESET  GE-SRC-RESET
   s" package PD-PUB" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" STRUCTURE box FIELD x n ;STRUCTURE" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : BOX-MAKE ( n -- n ) ;" GE-SRC-LINE
   s" : BOX-MAKE ( n -- n ) ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN
   70 label labelu GE-EXPECT-RC
   s" error[E-DUPLICATE-DEFINITION]" label labelu GE-EXPECT-ERR-HAS
   s" = generated by" label labelu GE-EXPECT-ERR-LACKS ;

\ A refused storage mapping stops the load with one E-MAP record, which has no
\ place, as no run-time record has one.
: MAP-REJECT-CASE ( ^u8 n ^u8 n -- )
   {: path:^ pathu:n count:^ countu:n :}
   path pathu RUN-LOAD
   67 path pathu GE-EXPECT-RC
   RECORDS 1 <> if path pathu GE-FAIL then
   s" error[E-MAP]: the system refused " path pathu GE-EXPECT-ERR-HAS
   count countu path pathu GE-EXPECT-ERR-HAS
   s"  --> " path pathu GE-EXPECT-ERR-LACKS ;

: MAP-REJECTS ( -- )
   s" test/program-diagnostic-map-size.f"
      S\" refused 9223372036854775808 bytes of storage\n  = bytes: 9223372036854775808\n"
      MAP-REJECT-CASE
   s" test/program-diagnostic-map-big.f"
      S\" refused 2305843009213693952 bytes of storage\n  = bytes: 2305843009213693952\n"
      MAP-REJECT-CASE ;

: DIAGNOSTICS ( -- )
   BADSIG-ONCE
   THROW-ZERO-STOPS
   CAUGHT-ZERO-STOPS
   THROW-AFTER-EVAL
   GENERATED-DUPLICATE
   PUBLIC-DUPLICATE
   QUIET-COUNT-ONCE
   MAP-REJECTS
   s" test/program-diagnostic-long-row.f" 70 s" LONG-ROW-ARMED"
      s" error[E-MISMATCH]: LONG-ROW ends with (904 more) n n" NEGATIVE
   s" test/program-diagnostic-long-quot.f" 70 s" LONG-QUOT-ARMED"
      s" error[E-MISMATCH]: LONG-QUOT ends with [ -- (905 more) n n" NEGATIVE
   s" test/program-diagnostic-me.f" s" program-diagnostic-me.f:9:30" DIAGNOSTIC
   s" error[E-MISMATCH]" s" test/program-diagnostic-me.f" GE-EXPECT-ERR-HAS
   s" = evaluated: : MEA1 ( n -- n ) drop ;" s" test/program-diagnostic-me.f" GE-EXPECT-ERR-HAS
   s" test/native-multi-error-recovery.f" s" native-multi-error-recovery.f:26:18" HABU-DIAGNOSTIC
   s" error[E-MISMATCH]: `c@` takes ^u8; the stack holds n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/native-multi-error-recovery.f" s" native-multi-error-recovery.f:26:18" DIAGNOSTIC
   s" error[E-MISMATCH]: `c@` takes ^u8; the stack holds n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-ctor.f" s" duplicate family" DIAGNOSTIC
   s" test/program-diagnostic-using.f" s" error[E-USING-UNKNOWN]" DIAGNOSTIC
   s" no package is named `NOPE-PKG`" s" test/program-diagnostic-using.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-export.f" s" <input>:1:15" DIAGNOSTIC
   s" error[E-INPUT-UNDERFLOW]" s" test/program-diagnostic-export.f" GE-EXPECT-ERR-HAS
   s" 1 | XPU3 ( -- n ) XPD:XP-INC" s" test/program-diagnostic-export.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-lower.f" s" <input>:1:36" DIAGNOSTIC
   s" error[E-MISMATCH]" s" test/program-diagnostic-lower.f" GE-EXPECT-ERR-HAS
   s" 1 | LC-NAMED-BAD ( ^lcnamed -- color ) @" s" test/program-diagnostic-lower.f" GE-EXPECT-ERR-HAS ;

public

: TEST ( -- )
   s" habu-program-diagnostics" GT-START
   NEGATIVES
   DIAGNOSTICS
   GT-CLEANUP
   s" program-diagnostics-test: ok" type cr ;

;package

PROGRAM-DIAGNOSTICS:TEST
