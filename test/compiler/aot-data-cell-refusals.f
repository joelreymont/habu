\ aot-data-cell-refusals.f - a persistent cell holding a code or dictionary
\ pointer is refused naming the cell, the value and, for a data pointer, the
\ bounds, whether the span scan (src/habu/aot-lib.f) or the span bound
\ (src/habu/aot-closure.f) meets it. A cell declared to hold an xt is relocated
\ instead (test/stripped-image.f); `' word ,` declares nothing. Three stripped
\ images go through the real native linker.
require test/gate-common.f
require test/preloaded-engine.f

package AOT-DATA-CELL-REFUSAL-TEST

600000 constant TIMEOUT-MS
create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

\ TMPDIR is the gate root, so AOT-OUT's image lands at this path if one is ever
\ emitted: every case refuses before that.
: PREPARE ( -- )
   s" aot-data-cell-refusals" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SUBJECT$ a u FS:WRITE-ALL ;

\ The production maker script (tools/hb-build-lib.f HBB-RUN-MAKER-CMD) on stdin:
\ a definition ahead of tools/aot-build-open.f lands in pre-window DATA, and the
\ subject loads inside the window. The child runs the keyed linker image
\ (test/preloaded-engine.f), where phase two's require is a no-op.
: LINK-ONLY$ ( -- ptr u8 n )
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n" ;

\ The linker's second argument is its JSON-diagnostics flag. The linker image
\ is taken before the child's argv is staged: building it stages its own.
: BUILD-DIAG ( ptr u8 n ptr u8 n -- ) {: json:ptr jsonu:n in:ptr inu:n :}
   PRELOADED-ENGINE:LINKER$ {: linker:ptr linkeru:n :}
   GE-HB-RESET
   linker linkeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ json jsonu GE-ARG+
   s" TMPDIR" >LEN GT-ROOT >LEN PROC-ENV+
   linker linkeru in inu TIMEOUT-MS GE-RUN-STDIN ;

: BUILD ( ptr u8 n -- ) {: in:ptr inu:n :}
   s" 0" in inu BUILD-DIAG ;

: NO-IMAGE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   IMAGE$ FS:EXISTS? if label labelu GE-FAIL then ;

\ Each field is present and non-empty: fields are space separated and the last
\ ends the line, so an empty one (a number the emitter cannot print) is what the
\ LACKS needles find.
: FIELDS-FILLED ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" data-off=" label labelu GE-EXPECT-ERR-HAS
   s" data-off= " label labelu GE-EXPECT-ERR-LACKS
   s" value=" label labelu GE-EXPECT-ERR-HAS
   S\" value=\n" label labelu GE-EXPECT-ERR-LACKS ;

\ ---- a cell inside the capture window: the span scan meets it -----------------

\ The checker is suspended around the store so the image, not the checker, refuses it.
: CASE-TICK ( -- )
   S\" : DCR-BUMP ( n -- n ) 1+ ;\ncreate DCR-TABLE check@ 0 set-check ' DCR-BUMP , set-check\n: MAIN ( -- ) DCR-TABLE @ . ;\n"
      WRITE-SUBJECT
   LINK-ONLY$ BUILD
   70 s" ' word , cell: refusal code" GE-EXPECT-RC
   s" stripped AOT persistent data holds an undeclared code/dict pointer"
      s" ' word , cell: refusal reason" GE-EXPECT-ERR-HAS
   s" word=DCR-TABLE" s" ' word , cell: names the owning word" GE-EXPECT-ERR-HAS
   s" ' word , cell: fields filled" FIELDS-FILLED
   s" (defer/is/xt!)" s" ' word , cell: names the declaring forms" GE-EXPECT-ERR-HAS
   s" outside the restored span" s" ' word , cell: not the unrestored answer" GE-EXPECT-ERR-LACKS
   s" ' word , cell: no image" NO-IMAGE ;

\ ---- a cell outside it: the span bound meets the same two kinds ---------------

\ This xt cell sits below the window, so only the span bound meets it: same
\ verdict and three facts, its reason that the image does not restore the cell.
\ Every cell refusal renders through src/habu/aot-closure.f DATA-CELL-PROSE and
\ DATA-CELL-JSON: CASE-TICK locks the prose rendering, this case the JSON one.
: CASE-PRE-XT ( -- )
   S\" : MAIN ( -- ) DCR-PRE-XT @ . ;\n" WRITE-SUBJECT
   s" 1"
   S\" : DCR-PRE ( n -- n ) 1+ ;\ncreate DCR-PRE-XT check@ 0 set-check ' DCR-PRE , set-check\nrequire tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
      BUILD-DIAG
   70 s" pre-window xt cell: refusal code" GE-EXPECT-RC
   S\" \"code\":\"E-AOT-UNSUPPORTED\"" s" pre-window xt cell: schema code" GE-EXPECT-ERR-HAS
   S\" \"reason\":\"stripped AOT persistent data outside the restored span holds a code/dict pointer\""
      s" pre-window xt cell: refusal reason" GE-EXPECT-ERR-HAS
   S\" \"word\":\"DCR-PRE-XT\"" s" pre-window xt cell: names the owning word" GE-EXPECT-ERR-HAS
   S\" \"data_off\":" s" pre-window xt cell: names the DATA offset" GE-EXPECT-ERR-HAS
   S\" \"data_off\":," s" pre-window xt cell: DATA offset filled" GE-EXPECT-ERR-LACKS
   S\" \"value\":" s" pre-window xt cell: names the value" GE-EXPECT-ERR-HAS
   S\" \"value\":," s" pre-window xt cell: value filled" GE-EXPECT-ERR-LACKS
   s" aot: address refers to data"
      s" pre-window xt cell: named as a cell, not as a bare span bound" GE-EXPECT-ERR-LACKS
   s" pre-window xt cell: no image" NO-IMAGE ;

: CASE-PRE-DATA ( -- )
   S\" : MAIN ( -- ) DCR-PRE-CELL @ . ;\n" WRITE-SUBJECT
   S\" create DCR-PRE-CELL 41 ,\nrequire tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n" BUILD
   74 s" pre-window data cell: refusal code" GE-EXPECT-RC
   s" aot: address refers to data outside the restored span"
      s" pre-window data cell: refusal reason" GE-EXPECT-ERR-HAS
   s" caller=MAIN" s" pre-window data cell: names the recorded cell's word" GE-EXPECT-ERR-HAS
   s" target=DCR-PRE-CELL" s" pre-window data cell: names the data's word" GE-EXPECT-ERR-HAS
   s" region-off=" s" pre-window data cell: names the region offset" GE-EXPECT-ERR-HAS
   s" region-off= " s" pre-window data cell: region offset filled" GE-EXPECT-ERR-LACKS
   s" value=" s" pre-window data cell: names the value" GE-EXPECT-ERR-HAS
   s" value= " s" pre-window data cell: value filled" GE-EXPECT-ERR-LACKS
   s" span=[" s" pre-window data cell: names both bounds" GE-EXPECT-ERR-HAS
   s" span=[," s" pre-window data cell: lower bound filled" GE-EXPECT-ERR-LACKS
   S\" ,]\n" s" pre-window data cell: upper bound filled" GE-EXPECT-ERR-LACKS
   s" holds a code/dict pointer"
      s" pre-window data cell: a plain datum is not called a pointer" GE-EXPECT-ERR-LACKS
   s" pre-window data cell: no image" NO-IMAGE ;

: BODY ( -- )
   PREPARE
   CASE-TICK
   CASE-PRE-XT
   CASE-PRE-DATA
   s" PASS: AOT persistent-cell refusals name the cell, the value and the bounds" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
