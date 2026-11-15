\ program-diagnostics-test.f - direct production-load diagnostic contracts.

require test/gate-common.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/process.f

package PROGRAM-DIAGNOSTICS

: RUN-LOAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   path pathu GE-ARG+
   s" bin/hb" PROCESS:WAIT-FOREVER MS>N GE-RUN-ENV ;

\ The Habu loop reads the file and recovers at `;` (src/habu/interpreter.f
\ DEF-SEMI?), as it does for RUN-LOAD.
: RUN-LOAD-HABU ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/interpreter-loop-on.f" GE-ARG+
   path pathu GE-ARG+
   s" bin/hb" PROCESS:WAIT-FOREVER MS>N GE-RUN-ENV ;

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

\ stderr is one engine exit line: the text, " lr 0x", the lr's 16 hex digits
\ and a newline.
: EXPECT-DIE-LINE ( ptr u8 n ptr u8 n -- )
   {: text:ptr textu:n label:ptr labelu:n :}
   SB-RESET text textu SB-APPEND s"  lr 0x" SB-APPEND
   SB$ {: want:ptr wantu:n :}
   GT-ERR$ {: err:ptr erru:n :}
   erru wantu 17 + <> if label labelu GE-FAIL then
   err wantu want wantu STR= 0= if label labelu GE-FAIL then
   true erru 1- wantu ?do err i + c@ STR-HEX-DIGIT? and loop
   0= if label labelu GE-FAIL then
   err erru + 1- c@ GE-LF <> if label labelu GE-FAIL then ;

: NEGATIVE-LINE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n rc:n marker:ptr markeru:n text:ptr textu:n :}
   path pathu rc marker markeru text textu NEGATIVE
   text textu path pathu EXPECT-DIE-LINE ;

\ A bootstrap-created word's cell refused where its signature promises another
\ type: the record names both.
: CREATED-MISMATCH ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n want:ptr wantu:n found:ptr foundu:n :}
   path pathu 70 s" BOOTSTRAP-CREATED-ARMED" s" error[E-MISMATCH]" NEGATIVE
   want wantu path pathu GE-EXPECT-ERR-HAS
   found foundu path pathu GE-EXPECT-ERR-HAS ;

\ How many rejection records stderr holds.
: RECORDS ( -- n )
   GT-ERR$ s" error[" COUNT-SUB ;

: NEGATIVES ( -- )
   s" test/address-cell-kind-bad.f" 99 s" ADDRESS-CELL-KIND-ARMED"
      s" E-XTKIND habu: snapshot address cell kind mismatch" NEGATIVE-LINE
   s" test/address-cell-cap-bad.f" 96 s" ADDRESS-CELL-CAP-ARMED"
      s" E-XTCELL habu: invalid address-cell storage header" NEGATIVE-LINE
   s" test/xt-cell-band-bad.f" 98 s" XT-CELL-BAND-ARMED"
      s" E-XTBAND habu: snapshot address cell out of range" NEGATIVE-LINE
   s" test/enum-ctor-collide-bad.f" s" enum-ctor-collide-bad.f:21:1" DIAGNOSTIC
   s" error[E-BAD-DECLARATION]" s" test/enum-ctor-collide-bad.f" GE-EXPECT-ERR-HAS
   s" ^ generated declaration already defined" s" test/enum-ctor-collide-bad.f"
      GE-EXPECT-ERR-HAS
   s" = evaluated: ENUM ecol green red ;ENUM"
      s" test/enum-ctor-collide-bad.f" GE-EXPECT-ERR-HAS
   s" = word: ECOL:RED" s" test/enum-ctor-collide-bad.f" GE-EXPECT-ERR-HAS
   s" test/deftype-dup-bad.f" 70
      s" DEFTYPE-DUP-ARMED" s" duplicate family" NEGATIVE
   s" test/bootstrap-created-effect-src.f"
      S\" = expected: (nothing)\n" S\" = found: ptr a\n" CREATED-MISMATCH
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
   s" test/program-diagnostic-badsig.f" {: path:ptr pathu:n :}
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
   s" test/program-diagnostic-quiet-count.f" {: path:ptr pathu:n :}
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
   s" throw-zero" {: label:ptr labelu:n :}
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

\ A refused storage mapping stops the load with one E-MAP record; the second
\ line's path is absolute, so its file:line:col is pinned as the line's end.
: MAP-REFUSED-CASE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n count:ptr countu:n place:ptr placeu:n :}
   path pathu RUN-LOAD
   67 path pathu GE-EXPECT-RC
   RECORDS 1 <> if path pathu GE-FAIL then
   s" error[E-MAP]: the system refused " path pathu GE-EXPECT-ERR-HAS
   count countu path pathu GE-EXPECT-ERR-HAS
   place placeu path pathu GE-EXPECT-ERR-HAS ;

: MAP-REFUSED ( -- )
   s" test/program-diagnostic-map-size.f"
      S\" refused 9223372036854775808 bytes of storage\n --> /"
      S\" /test/program-diagnostic-map-size.f:4:19\n" MAP-REFUSED-CASE
   s" test/program-diagnostic-map-big.f"
      S\" refused 2305843009213693952 bytes of storage\n --> /"
      S\" /test/program-diagnostic-map-big.f:3:18\n" MAP-REFUSED-CASE ;

: DIAGNOSTICS ( -- )
   BADSIG-ONCE
   THROW-ZERO-STOPS
   QUIET-COUNT-ONCE
   MAP-REFUSED
   s" test/program-diagnostic-me.f" s" program-diagnostic-me.f:9:30" DIAGNOSTIC
   s" error[E-MISMATCH]" s" test/program-diagnostic-me.f" GE-EXPECT-ERR-HAS
   s" = evaluated: : MEA1 ( n -- n ) drop ;" s" test/program-diagnostic-me.f" GE-EXPECT-ERR-HAS
   s" test/native-multi-error-recovery.f" s" native-multi-error-recovery.f:25:18" HABU-DIAGNOSTIC
   s" error[E-MISMATCH]: `c@` takes ptr u8; the stack holds n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/native-multi-error-recovery.f" s" native-multi-error-recovery.f:25:18" DIAGNOSTIC
   s" error[E-MISMATCH]: `c@` takes ptr u8; the stack holds n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-ctor.f" s" duplicate family" DIAGNOSTIC
   s" test/program-diagnostic-using.f" s" error[E-USING-UNKNOWN]" DIAGNOSTIC
   s" no package is named `NOPE-PKG`" s" test/program-diagnostic-using.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-export.f" s" <input>:1:15" DIAGNOSTIC
   s" error[E-INPUT-UNDERFLOW]" s" test/program-diagnostic-export.f" GE-EXPECT-ERR-HAS
   s" 1 | XPU3 ( -- n ) XPD:XP-INC" s" test/program-diagnostic-export.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-lower.f" s" <input>:1:39" DIAGNOSTIC
   s" error[E-MISMATCH]" s" test/program-diagnostic-lower.f" GE-EXPECT-ERR-HAS
   s" 1 | LC-NAMED-BAD ( ptr lcnamed -- color ) @" s" test/program-diagnostic-lower.f" GE-EXPECT-ERR-HAS ;

public

: TEST ( -- )
   s" habu-program-diagnostics" GT-START
   NEGATIVES
   DIAGNOSTICS
   GT-CLEANUP
   s" program-diagnostics-test: ok" type cr ;

;package

PROGRAM-DIAGNOSTICS:TEST
