\ Two checks meet a persistent cell that holds a code or dictionary pointer: the
\ span scan in src/habu/aot-lib.f, which reads every cell the capture window
\ covers, and the span bound in src/habu/aot-closure.f, which meets the same kind
\ of cell through a recorded address naming one the window never opened over.
\ They must answer alike, and the refusal that a genuine data pointer keeps must
\ say which cell, which value and which bounds. Build four stripped images
\ through the real native linker and read the diagnostic each is refused with.
\
\ RED BEFORE THIS LANE: a PRE-WINDOW cell holding an xt (PRE-XT below) was
\ refused with "aot: address refers to data outside the restored span", exit 74,
\ naming neither the cell nor the value nor the bounds, while the same class of
\ cell inside the window was refused as unsupported persistent data, exit 70. One
\ program was classified two ways by which check ran first, and the 74 read as an
\ engine fault rather than a refusal (dot habu-name-the-cell-740feb52; Tender's
\ server image took the first path and its CLI the second).
require test/gate-common.f
require lib/engine-candidate.f

package AOT-DATA-CELL-REFUSAL-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

\ HB_TMP is the gate root, so AOT-OUT's image lands at this path if one is ever
\ emitted. Every case below must refuse before that.
: PREPARE ( -- )
   s" aot-data-cell-refusals" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SUBJECT$ a u WRITE-ALL ;

\ The linker child reads its prelude from stdin, so a definition placed there is
\ compiled BEFORE tools/aot-build.f opens the capture window and lands in
\ pre-window DATA; the subject file is loaded after and lands inside it.
: LINK-ONLY$ ( -- ptr u8 n )
   S\" require tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n" ;

\ The linker's second argument is its JSON-diagnostics flag.
: BUILD-DIAG ( ptr u8 n ptr u8 n -- ) {: json:ptr jsonu:n in:ptr inu:n :}
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ json jsonu GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ in inu TIMEOUT-MS GE-RUN-STDIN ;

: BUILD ( ptr u8 n -- ) {: in:ptr inu:n :}
   s" 0" in inu BUILD-DIAG ;

: NO-IMAGE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   IMAGE$ EXISTS? if label labelu GE-FAIL then ;

\ Each field is there AND rendered something. The fields are space separated and
\ the last one ends the line, so an empty one is exactly what the LACKS needles
\ find - which is what a number the emitter cannot print (a negative offset)
\ would leave behind. Presence alone would pass on a field that was dropped, and
\ non-emptiness alone would pass on one that was never emitted.
: FIELDS-FILLED ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" data-off=" label labelu GE-EXPECT-ERR-HAS
   s" data-off= " label labelu GE-EXPECT-ERR-LACKS
   s" value=" label labelu GE-EXPECT-ERR-HAS
   S\" value=\n" label labelu GE-EXPECT-ERR-LACKS ;

\ ---- a cell inside the capture window: the span scan meets it -----------------

: CASE-DEFER ( -- )
   S\" : DCR-BUMP ( n -- n ) 1+ ;\ndefer DCR-STEP ( n -- n )\n: DCR-INIT ( -- ) [: DCR-BUMP ;] is DCR-STEP ;\nDCR-INIT\n: MAIN ( -- ) 41 DCR-STEP . ;\n"
      WRITE-SUBJECT
   LINK-ONLY$ BUILD
   70 s" defer cell: refusal code" GE-EXPECT-RC
   s" stripped AOT persistent data holds a code/dict pointer"
      s" defer cell: refusal reason" GE-EXPECT-ERR-HAS
   s" word=DCR-STEP" s" defer cell: names the owning word" GE-EXPECT-ERR-HAS
   s" defer cell: fields filled" FIELDS-FILLED
   s" outside the restored span" s" defer cell: one answer only" GE-EXPECT-ERR-LACKS
   s" defer cell: no image" NO-IMAGE ;

: CASE-TICK ( -- )
   S\" : DCR-BUMP ( n -- n ) 1+ ;\ncreate DCR-TABLE ' DCR-BUMP ,\n: MAIN ( -- ) DCR-TABLE @ . ;\n"
      WRITE-SUBJECT
   LINK-ONLY$ BUILD
   70 s" ' word , cell: refusal code" GE-EXPECT-RC
   s" stripped AOT persistent data holds a code/dict pointer"
      s" ' word , cell: refusal reason" GE-EXPECT-ERR-HAS
   s" word=DCR-TABLE" s" ' word , cell: names the owning word" GE-EXPECT-ERR-HAS
   s" ' word , cell: fields filled" FIELDS-FILLED
   s" outside the restored span" s" ' word , cell: one answer only" GE-EXPECT-ERR-LACKS
   s" ' word , cell: no image" NO-IMAGE ;

\ ---- a cell outside it: the span bound meets the same two kinds ---------------

\ THE CONSISTENCY CASE. This cell holds an xt and sits below the window, so the
\ scan never reads it and only the span bound meets it. It must still be the
\ unsupported-persistent-data refusal, named the same way.
: CASE-PRE-XT ( -- )
   S\" : MAIN ( -- ) DCR-PRE-XT @ . ;\n" WRITE-SUBJECT
   S\" : DCR-PRE ( n -- n ) 1+ ;\ncreate DCR-PRE-XT ' DCR-PRE ,\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
      BUILD
   70 s" pre-window xt cell: refusal code" GE-EXPECT-RC
   s" stripped AOT persistent data holds a code/dict pointer"
      s" pre-window xt cell: refusal reason" GE-EXPECT-ERR-HAS
   s" word=DCR-PRE-XT" s" pre-window xt cell: names the owning word" GE-EXPECT-ERR-HAS
   s" pre-window xt cell: fields filled" FIELDS-FILLED
   s" outside the restored span"
      s" pre-window xt cell: one answer whichever check meets it" GE-EXPECT-ERR-LACKS
   s" pre-window xt cell: no image" NO-IMAGE ;

\ A genuine data pointer keeps exit 74, and now names the recorded cell, the
\ value, the word whose data it is, and the span it had to fall in.
: CASE-PRE-DATA ( -- )
   S\" : MAIN ( -- ) DCR-PRE-CELL @ . ;\n" WRITE-SUBJECT
   S\" create DCR-PRE-CELL 41 ,\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n" BUILD
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

\ hb-build --json publishes this shape, so the three facts are locked in both
\ renderings rather than only in the prose one.
: CASE-JSON ( -- )
   S\" : MAIN ( -- ) DCR-PRE-XT @ . ;\n" WRITE-SUBJECT
   s" 1"
   S\" : DCR-PRE ( n -- n ) 1+ ;\ncreate DCR-PRE-XT ' DCR-PRE ,\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
      BUILD-DIAG
   70 s" json refusal: code" GE-EXPECT-RC
   S\" \"code\":\"E-AOT-UNSUPPORTED\"" s" json refusal: schema code" GE-EXPECT-ERR-HAS
   S\" \"word\":\"DCR-PRE-XT\"" s" json refusal: names the owning word" GE-EXPECT-ERR-HAS
   S\" \"data_off\":" s" json refusal: names the DATA offset" GE-EXPECT-ERR-HAS
   S\" \"data_off\":," s" json refusal: DATA offset filled" GE-EXPECT-ERR-LACKS
   S\" \"value\":" s" json refusal: names the value" GE-EXPECT-ERR-HAS
   S\" \"value\":," s" json refusal: value filled" GE-EXPECT-ERR-LACKS
   s" json refusal: no image" NO-IMAGE ;

: BODY ( -- )
   PREPARE
   CASE-DEFER
   CASE-TICK
   CASE-PRE-XT
   CASE-PRE-DATA
   CASE-JSON
   s" PASS: AOT persistent-cell refusals name the cell, the value and the bounds" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
