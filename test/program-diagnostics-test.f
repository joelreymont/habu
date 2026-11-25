\ program-diagnostics-test.f - direct production-load diagnostic contracts.

require test/gate-common.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/process.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test/runner.f

package PROGRAM-DIAGNOSTICS

: RUN-LOAD ( str -- ) {: path:str :}
   GE-HB-RESET
   s" --load" GE-ARG+
   path GE-ARG+
   s" bin/hb" PROCESS:WAIT-FOREVER GE-RUN-ENV ;

: EXPECT-MARKER ( str str -- )
   {: marker:str label:str :}
   SB-RESET marker SB-APPEND GE-SB-LF
   SB$ label GE-EXPECT-OUT ;

: EXPECT-OK-END ( str -- ) {: label:str :}
   GT-OUT$ {: out:str :}  out STR:LEN {: outu:n :}
   outu 3 < if label GE-FAIL then
   out STR:DATA outu 3 - + 2 >str s" ok" STR:= 0= if label GE-FAIL then
   out STR:DATA outu 1- + c@ GE-LF <> if label GE-FAIL then ;

: NEGATIVE ( str n str str -- )
   {: path:str rc:n marker:str needle:str :}
   path RUN-LOAD
   rc path GE-EXPECT-RC
   marker path EXPECT-MARKER
   needle path GE-EXPECT-ERR-HAS ;

: EXPECT-DIAGNOSTIC ( str str -- )
   {: path:str needle:str :}
   path GE-EXPECT-OK
   path EXPECT-OK-END
   needle path GE-EXPECT-ERR-HAS ;

: DIAGNOSTIC ( str str -- )
   {: path:str needle:str :}
   path RUN-LOAD
   path needle EXPECT-DIAGNOSTIC ;

\ stderr is one engine exit line: the text, " lr 0x", the lr's 16 hex digits
\ and a newline.
: EXPECT-DIE-LINE ( str str -- )
   {: text:str label:str :}
   SB-RESET text SB-APPEND s"  lr 0x" SB-APPEND
   SB$ {: want:str :}  want STR:LEN {: wantu:n :}
   GT-ERR$ {: err:str :}  err STR:LEN {: erru:n :}
   erru wantu 17 + <> if label GE-FAIL then
   err STR:DATA wantu >str want STR:= 0= if label GE-FAIL then
   true erru 1- wantu ?do err STR:DATA i + c@ STR:HEX-DIGIT? and loop
   0= if label GE-FAIL then
   err STR:DATA erru + 1- c@ GE-LF <> if label GE-FAIL then ;

: NEGATIVE-LINE ( str n str str -- )
   {: path:str rc:n marker:str text:str :}
   path rc marker text NEGATIVE
   text path EXPECT-DIE-LINE ;

\ A bootstrap-created word's cell refused where its signature promises another
\ type: the record names both.
: CREATED-MISMATCH ( str str str -- )
   {: path:str want:str found:str :}
   path 70 s" BOOTSTRAP-CREATED-ARMED" s" error[E-MISMATCH]" NEGATIVE
   want path GE-EXPECT-ERR-HAS
   found path GE-EXPECT-ERR-HAS ;

\ How many rejection records stderr holds.
: RECORDS ( -- n )
   GT-ERR$ s" error[" STR:COUNT-SUB ;

: NEGATIVES ( -- )
   s" test/address-cell-kind-bad.f" 96 s" ADDRESS-CELL-KIND-ARMED"
      s" E-XTKIND habu: address cell kind mismatch" NEGATIVE-LINE
   s" test/xt-cell-band-bad.f" 96 s" XT-CELL-BAND-ARMED"
      s" E-XTBAND habu: address cell out of range" NEGATIVE-LINE
   s" test/enum-ctor-collide-bad.f" s" enum-ctor-collide-bad.f:17:1" DIAGNOSTIC
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
   s" test/program-diagnostic-badsig.f" {: path:str :}
   path RUN-LOAD
   path GE-EXPECT-OK
   GT-OUT$ S\" 1\n1\n1\nok\n" STR:= 0= if s" badsig counts" GE-FAIL then
   RECORDS 3 <> if s" badsig reports" GE-FAIL then
   s" error[E-UNKNOWN-SIGNATURE-TYPE]: unknown type `zz` in signature" path GE-EXPECT-ERR-HAS
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `PDB-TRUSTED`" path GE-EXPECT-ERR-HAS
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `PDB-DEFER`" path GE-EXPECT-ERR-HAS ;

\ A multi-error load counts the rejections it renders: a quiet scan's rejection
\ is neither printed nor counted, so the count is 1 and stderr holds one record.
: QUIET-COUNT-ONCE ( -- )
   s" test/program-diagnostic-quiet-count.f" {: path:str :}
   path RUN-LOAD
   path GE-EXPECT-OK
   GT-OUT$ S\" 0\n1\n1\nok\n" STR:= 0= if s" quiet count" GE-FAIL then
   RECORDS 1 <> if s" quiet count reports" GE-FAIL then
   s" error[E-INPUT-UNDERFLOW]" path GE-EXPECT-ERR-HAS
   s" (needs 1, has 0)" path GE-EXPECT-ERR-HAS ;

\ A computed 0 that a top-level call throws stops its text with one record at
\ the call: the line after it never runs and hb exits 85. A literal 0 there is
\ rejected before the line runs.
: THROW-ZERO-STOPS ( -- )
   s" throw-zero" {: label:str :}
   GE-HB-RESET  GE-SRC-RESET
   s" : Z ( n -- ) throw ;" GE-SRC-LINE
   S\" s\" \" str> nip Z" GE-SRC-LINE
   S\" s\" after\" type cr" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN
   EXIT-STATUS:UNCAUGHT label GE-EXPECT-RC
   s" " label GE-EXPECT-OUT
   GT-ERR$ s" error[E-THROW-ZERO]: a computed 0 was thrown"
   STR:STARTS-WITH? 0= if label GE-FAIL then
   S\" \n --> <input>:2:15\n" label GE-EXPECT-ERR-HAS
   s" throw a computed code only when it is nonzero: `rc 0<> if rc throw then`"
   label GE-EXPECT-ERR-HAS ;

\ A catch that answered 0 lands no throw, so its code thrown again stops the text
\ with E-THROW-ZERO's record at the call, not one an earlier caught throw kept,
\ and hb exits 85.
: CAUGHT-ZERO-STOPS ( -- )
   s" caught-zero" {: label:str :}
   GE-HB-RESET  GE-SRC-RESET
   S\" : K ( -- ) s\" E-KEPT\" REJECTION:OPEN  REJECTION:SAVE  1000 REJECTION:PASS ;" GE-SRC-LINE
   s" : ON ( -- ) [: K ;] catch drop  [: ;] catch {: rc:n :} rc throw ;" GE-SRC-LINE
   s" ON" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN
   EXIT-STATUS:UNCAUGHT label GE-EXPECT-RC
   GT-ERR$ s" error[E-THROW-ZERO]: a computed 0 was thrown"
   STR:STARTS-WITH? 0= if label GE-FAIL then
   S\" \n --> <input>:3:1\n" label GE-EXPECT-ERR-HAS ;

\ An evaluate that returns gives the place back to the outer text: a later throw
\ in the same word is placed at the outer token, not in the evaluated string.
: THROW-AFTER-EVAL ( -- )
   s" throw-after-eval" {: label:str :}
   GE-HB-RESET  GE-SRC-RESET
   s" : Z ( n -- ) throw ;" GE-SRC-LINE
   S\" : G ( -- ) s\" 1 drop\" evaluate-closed  s\" \" str> nip Z ;" GE-SRC-LINE
   s" G" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN
   EXIT-STATUS:UNCAUGHT label GE-EXPECT-RC
   s" " label GE-EXPECT-OUT
   GT-ERR$ s" error[E-THROW-ZERO]: a computed 0 was thrown"
   STR:STARTS-WITH? 0= if label GE-FAIL then
   S\" \n --> <input>:3:1\n" label GE-EXPECT-ERR-HAS ;

: GENERATED-DUPLICATE ( -- )
   s" generated-duplicate" {: label:str :}
   GE-HB-RESET  GE-SRC-RESET
   s" package PD-GEN" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" STRUCTURE box FIELD x n ;STRUCTURE" GE-SRC-LINE
   s" : BOX-MAKE ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN
   70 label GE-EXPECT-RC
   s" error[E-DUPLICATE-DEFINITION]" label GE-EXPECT-ERR-HAS
   s" = generated by: type `box`" label GE-EXPECT-ERR-HAS
   s" = help: rename this word, or the type `box`, which generates it"
   label GE-EXPECT-ERR-HAS ;

\ A public word may share a private generated word's spelling, so its own
\ duplicate names no type.
: PUBLIC-DUPLICATE ( -- )
   s" public-duplicate" {: label:str :}
   GE-HB-RESET  GE-SRC-RESET
   s" package PD-PUB" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" STRUCTURE box FIELD x n ;STRUCTURE" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : BOX-MAKE ( n -- n ) ;" GE-SRC-LINE
   s" : BOX-MAKE ( n -- n ) ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN
   70 label GE-EXPECT-RC
   s" error[E-DUPLICATE-DEFINITION]" label GE-EXPECT-ERR-HAS
   s" = generated by" label GE-EXPECT-ERR-LACKS ;

: MAP-HELP$ ( -- str )
   s" = help: raise the process's address-space limit (ulimit -v), or reserve less storage" ;

\ A refused storage mapping throws E-MEM-MAP carrying its one record, which
\ has no place, as no run-time record has one.
: MAP-REJECT-CASE ( str str -- )
   {: path:str count:str :}
   path RUN-LOAD
   EXIT-STATUS:SYSTEM path GE-EXPECT-RC
   RECORDS 1 <> if path GE-FAIL then
   s" error[E-MEM-MAP]: the system refused " path GE-EXPECT-ERR-HAS
   count path GE-EXPECT-ERR-HAS
   MAP-HELP$ path GE-EXPECT-ERR-HAS
   s"  --> " path GE-EXPECT-ERR-LACKS ;

: MAP-REJECTS ( -- )
   s" test/program-diagnostic-map-size.f"
      S\" refused 9223372036854775808 bytes of storage for a dynamic buffer\n"
      MAP-REJECT-CASE
   s" test/program-diagnostic-map-big.f"
      S\" refused 2305843009213693952 bytes of storage for a dynamic buffer\n"
      MAP-REJECT-CASE ;

create BIG-PATH PATH-CAP allot
variable BIG-U

: BIG$ ( -- str ) BIG-PATH BIG-U @ >str ;

\ The offset of k in a u, or u when a u lacks it.
: KEY-AT ( str str -- n ) {: s:str k:str :}
   s k STR:FIND-SUB MATCH option
      none OF s STR:LEN ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ The VmPeak, in kB, of the /proc/self/status the probe printed.
: VM-PEAK ( -- n )
   GT-OUT$ {: out:str :}  out STR:LEN {: outu:n :}
   out s" VmPeak:" KEY-AT 7 + outu min {: at:n :}
   out STR:DATA at +  outu at - >str {: s:str :}
   s STR:DATA  s s"  kB" KEY-AT >str  STR:TRIM STR:>NUMBER? MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The address space, in kB, a child hb peaks at loading src/lib/fs.f.
: PROBE-PEAK ( -- n )
   s" map-probe" {: label:str :}
   GE-HB-RESET  GE-SRC-RESET
   s" require src/lib/fs.f" GE-SRC-LINE
   s" 4096 BUFFER: PD-STATUS" GE-SRC-LINE
   s" /proc/self/status" GE-SRC-S"
   s"  PD-STATUS 4096 FS:READ-ALL PD-STATUS swap >str type" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN
   label GE-EXPECT-OK
   VM-PEAK {: kb:n :}
   kb 0= if label GE-FAIL then
   kb ;

\ A 32 MB source of 64-byte comment lines at path.
: BIG-SOURCE ( str -- ) {: path:str :}
   GE-SRC-RESET
   GE-SRC-CAP 64 / 0 ?do
      s" \ " GE-SRC+  61 $78 GE-SRC-REPEAT-C  GE-SRC-LF
   loop
   path GE-SRC-BUF GE-SRC-U @ >str FS:WRITE-ALL
   $2000000 GE-SRC-CAP / 1- 0 ?do path GE-SRC-BUF GE-SRC-U @ >str FS:APPEND-FILE loop ;

\ A refused source frame is one E-MEM-MAP record naming the file.
: MAP-FRAME ( n -- ) {: limit:n :}
   s" map-frame" {: label:str :}
   SB-RESET  s" ulimit -v " SB-APPEND  limit FMT:SB-U
   s" ; exec bin/hb --load '" SB-APPEND  BIG$ SB-APPEND  s" '" SB-APPEND
   GE-HB-RESET  s" -c" GE-ARG+  SB$ GE-ARG+
   s" /bin/sh" PROCESS:WAIT-FOREVER GE-RUN-ENV
   RECORDS 1 <> if label GE-FAIL then
   s" error[E-MEM-MAP]: the system refused " label GE-EXPECT-ERR-HAS
   s"  for a source frame" label GE-EXPECT-ERR-HAS
   SB-RESET  S\" \n  = file: " SB-APPEND  BIG$ SB-APPEND  S\" \n" SB-APPEND
   SB$ label GE-EXPECT-ERR-HAS
   EXIT-STATUS:SYSTEM label GE-EXPECT-RC ;

: COVER-ROWS ( -- )
   s" test/program-diagnostic-cover.f" {: path:str :}
   path s" program-diagnostic-cover.f:5:45" DIAGNOSTIC
   S\" error[E-MISMATCH]: `!` takes str ^str; the stack holds ^u8 n ^str\n" path GE-EXPECT-ERR-HAS
   S\" = expected: str ^str\n  = found: ^u8 n ^str\n" path GE-EXPECT-ERR-HAS
   S\" error[E-MISMATCH]: `PDC-LEN` takes str; the stack holds ^u8 n\n" path GE-EXPECT-ERR-HAS
   S\" = expected: str\n  = found: ^u8 n\n" path GE-EXPECT-ERR-HAS
   s" = help: give `PDC-LEN` str: convert or replace the value before it" path GE-EXPECT-ERR-HAS
   s" = help: give `PDC-TWO` n n: convert or replace the value before it" path GE-EXPECT-ERR-HAS ;

\ A refused reserve of the engine's own, the stdin stream's text, is one
\ E-MEM-MAP record whose help holds though the program reserved nothing, and
\ exits SYSTEM.
: MAP-STREAM ( n -- ) {: limit:n :}
   s" map-stream" {: label:str :}
   SB-RESET  s" ulimit -v " SB-APPEND  limit FMT:SB-U
   s" ; exec bin/hb < '" SB-APPEND  BIG$ SB-APPEND  s" '" SB-APPEND
   GE-HB-RESET  s" -c" GE-ARG+  SB$ GE-ARG+
   s" /bin/sh" PROCESS:WAIT-FOREVER GE-RUN-ENV
   RECORDS 1 <> if label GE-FAIL then
   s" error[E-MEM-MAP]: the system refused " label GE-EXPECT-ERR-HAS
   MAP-HELP$ label GE-EXPECT-ERR-HAS
   EXIT-STATUS:SYSTEM label GE-EXPECT-RC ;

\ Linux's ulimit -v: 8 MB over the probe's peak holds hb, not what a 32 MB
\ source grows to.
: MAP-LIMITED ( -- )
   PROBE-PEAK 8192 + {: limit:n :}
   s" map-limit-big.f" BIG-PATH GT-PATH BIG-U !
   BIG$ BIG-SOURCE
   limit MAP-FRAME
   limit MAP-STREAM ;

create NEED-PATH PATH-CAP allot

\ The fixture path of that name, valid until the next.
: NEED$ ( str -- str ) NEED-PATH GT-PATH NEED-PATH swap >str ;

: NEED-DIR ( str -- ) NEED$ FS:MAKE-DIRS ;

: NEED-FILE ( str str -- ) {: name:str text:str :}
   name NEED$ text FS:WRITE-ALL ;

\ hb with arguments args in the fixture's directory dir.
: NEED-RUN ( str str -- ) {: dir:str args:str :}
   SB-RESET  s" cd '" SB-APPEND  dir NEED$ SB-APPEND
   s" '; exec '" SB-APPEND  SOURCE-ROOT:CWD$ SB-APPEND
   s" /bin/hb' " SB-APPEND  args SB-APPEND
   GE-HB-RESET  s" -c" GE-ARG+  SB$ GE-ARG+
   s" /bin/sh" PROCESS:WAIT-FOREVER GE-RUN-ENV ;

\ The run's help says to add the require line req to the file it calls file, and
\ with that line before the text of the file, name's in the fixture, it succeeds.
: NEED-FOLLOWED ( str str str str str str -- )
   {: dir:str args:str req:str file:str name:str text:str :}
   dir args NEED-RUN
   EXIT-STATUS:REJECTED file GE-EXPECT-RC
   SB-RESET  s" = help: add `" SB-APPEND  req SB-APPEND
   s" ` to `" SB-APPEND  file SB-APPEND  s" `" SB-APPEND
   SB$ file GE-EXPECT-ERR-HAS
   SB-RESET  req SB-APPEND  GE-SB-LF  text SB-APPEND
   name SB$ NEED-FILE
   dir args NEED-RUN
   file GE-EXPECT-OK ;

: NEED-M$ ( -- str ) s" : PD-M ( -- n ) PD-Q ;" ;

\ The working directory's file, required from the entry's directory.
: NEED-CWD ( -- )
   s" q/w/lib" NEED-DIR
   s" q/q3.f" s" : PD-Q ( -- n ) 3 ;" NEED-FILE
   s" q/w/e3.f" S\" require q3.f\nrequire lib/m3.f\n" NEED-FILE
   s" q/w/lib/m3.f" NEED-M$ NEED-FILE
   s" q" s" --load w/e3.f" s" require ../q3.f" s" w/lib/m3.f"
      s" q/w/lib/m3.f" NEED-M$ NEED-FOLLOWED ;

\ A file beside the entry's directory.
: NEED-BESIDE ( -- )
   s" x/r" NEED-DIR  s" x/lib" NEED-DIR
   s" x/lib/e.f" s" : PD-Q ( -- n ) 3 ;" NEED-FILE
   s" x/r/s2.f" s" require ../lib/e.f" NEED-FILE
   s" x/r/r2.f" S\" require s2.f\n: PD-M ( -- n ) PD-Q ;" NEED-FILE
   s" x" s" --load r/r2.f" s" require ../lib/e.f" s" r/r2.f"
      s" x/r/r2.f" S\" require s2.f\n: PD-M ( -- n ) PD-Q ;" NEED-FOLLOWED ;

: NEED-L$ ( -- str ) s" : PD-L ( -- n ) PD-W ;" ;

\ The program file itself, required by a file it requires.
: NEED-PROGRAM ( -- )
   s" p" NEED-DIR
   s" p/prog.f" S\" : PD-W ( -- n ) 5 ;\nrequire lib.f\nPD-L .\n" NEED-FILE
   s" p/lib.f" NEED-L$ NEED-FILE
   s" p" s" prog.f" s" require prog.f" s" lib.f"
      s" p/lib.f" NEED-L$ NEED-FOLLOWED ;

: DIAGNOSTICS ( -- )
   NEED-CWD
   NEED-BESIDE
   NEED-PROGRAM
   BADSIG-ONCE
   THROW-ZERO-STOPS
   CAUGHT-ZERO-STOPS
   THROW-AFTER-EVAL
   GENERATED-DUPLICATE
   PUBLIC-DUPLICATE
   QUIET-COUNT-ONCE
   MAP-REJECTS
   HB-TARGET-LINUX? if MAP-LIMITED then
   s" test/program-diagnostic-long-row.f" 70 s" LONG-ROW-ARMED"
      s" error[E-MISMATCH]: LONG-ROW ends with (2952 more) n n" NEGATIVE
   s" test/program-diagnostic-long-quot.f" 70 s" LONG-QUOT-ARMED"
      s" error[E-MISMATCH]: LONG-QUOT ends with [ -- (2953 more) n n" NEGATIVE
   s" test/program-diagnostic-me.f" s" program-diagnostic-me.f:9:30" DIAGNOSTIC
   s" error[E-MISMATCH]" s" test/program-diagnostic-me.f" GE-EXPECT-ERR-HAS
   s" = evaluated: : MEA1 ( n -- n ) drop ;" s" test/program-diagnostic-me.f" GE-EXPECT-ERR-HAS
   s" test/native-multi-error-recovery.f" s" native-multi-error-recovery.f:24:18" DIAGNOSTIC
   s" error[E-MISMATCH]: `c@` takes ^u8; the stack holds n" s" test/native-multi-error-recovery.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-one-cell.f" s" program-diagnostic-one-cell.f:19:25" DIAGNOSTIC
   s" error[E-MISMATCH]: `t` in `AP`'s effect is one cell; `pr` is 2 cells" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" error[E-MISMATCH]: `t` in `D`'s effect is one cell; `pr` is 2 cells" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" error[E-MISMATCH]: `t` in `G`'s effect is one cell; `pr` is 2 cells" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" error[E-MISMATCH]: `t` in `IN-BOX`'s effect is one cell; `pr` is 2 cells" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" error[E-MISMATCH]: `t` in `Step`'s effect is stepped by an offset; `pr` is not a plain cell type" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" help: declare `Step` for the plain cell type it steps, or step `pr` by its size" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" in a word whose effect names `pr`" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-HAS
   s" @pr.tag" s" test/program-diagnostic-one-cell.f" GE-EXPECT-ERR-LACKS
   s" test/program-diagnostic-ctor.f" s" duplicate family" DIAGNOSTIC
   s" test/program-diagnostic-using.f" s" error[E-USING-UNKNOWN]" DIAGNOSTIC
   s" no package is named `NOPE-PKG`" s" test/program-diagnostic-using.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-export.f" s" <input>:1:15" DIAGNOSTIC
   s" error[E-INPUT-UNDERFLOW]" s" test/program-diagnostic-export.f" GE-EXPECT-ERR-HAS
   s" 1 | XPU3 ( -- n ) XPD:XP-INC" s" test/program-diagnostic-export.f" GE-EXPECT-ERR-HAS
   s" test/program-diagnostic-lower.f" s" <input>:1:36" DIAGNOSTIC
   s" error[E-MISMATCH]" s" test/program-diagnostic-lower.f" GE-EXPECT-ERR-HAS
   s" 1 | LC-NAMED-BAD ( ^lcnamed -- color ) @" s" test/program-diagnostic-lower.f" GE-EXPECT-ERR-HAS
   COVER-ROWS ;

public

: TEST ( -- )
   s" habu-program-diagnostics" GT-START
   NEGATIVES
   DIAGNOSTICS
   GT-CLEANUP
   s" program-diagnostics-test: ok" type cr ;

;package

PROGRAM-DIAGNOSTICS:TEST
