\ manifest-audit.f - checked manifest gate for expanded LLM tasks.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and bench/llm/manifest.f.

65536 constant BMA-CAP

create BMA-BUF BMA-CAP allot

variable BMA-U
variable BMA-NEXT
variable BMA-LINE-N
variable BMA-LINE-A
variable BMA-LINE-U
variable BMA-TAG-NEXT
variable BMA-TAG-A
variable BMA-TAG-U
variable BMA-ID-A
variable BMA-ID-U
variable BMA-NAME-A
variable BMA-NAME-U
variable BMA-CAT-A
variable BMA-CAT-U
variable BMA-HARNESS-A
variable BMA-HARNESS-U
variable BMA-CONV-A
variable BMA-CONV-U
variable BMA-TAGS-A
variable BMA-TAGS-U
variable BMA-REPORT-MISSING

TRUSTED: BMA-LINE! ( ptr u8 n -- )
   BMA-LINE-U ! BMA-LINE-A ! ;

TRUSTED: BMA-LINE$ ( -- ptr u8 n )
   BMA-LINE-A @ BMA-LINE-U @ ;

TRUSTED: BMA-TAG! ( ptr u8 n -- )
   BMA-TAG-U ! BMA-TAG-A ! ;

TRUSTED: BMA-TAG$ ( -- ptr u8 n )
   BMA-TAG-A @ BMA-TAG-U @ ;

TRUSTED: BMA-ID! ( ptr u8 n -- )
   BMA-ID-U ! BMA-ID-A ! ;

TRUSTED: BMA-ID$ ( -- ptr u8 n )
   BMA-ID-A @ BMA-ID-U @ ;

TRUSTED: BMA-NAME! ( ptr u8 n -- )
   BMA-NAME-U ! BMA-NAME-A ! ;

TRUSTED: BMA-NAME$ ( -- ptr u8 n )
   BMA-NAME-A @ BMA-NAME-U @ ;

TRUSTED: BMA-CAT! ( ptr u8 n -- )
   BMA-CAT-U ! BMA-CAT-A ! ;

TRUSTED: BMA-CAT$ ( -- ptr u8 n )
   BMA-CAT-A @ BMA-CAT-U @ ;

TRUSTED: BMA-HARNESS! ( ptr u8 n -- )
   BMA-HARNESS-U ! BMA-HARNESS-A ! ;

TRUSTED: BMA-HARNESS$ ( -- ptr u8 n )
   BMA-HARNESS-A @ BMA-HARNESS-U @ ;

TRUSTED: BMA-CONV! ( ptr u8 n -- )
   BMA-CONV-U ! BMA-CONV-A ! ;

TRUSTED: BMA-CONV$ ( -- ptr u8 n )
   BMA-CONV-A @ BMA-CONV-U @ ;

TRUSTED: BMA-TAGS! ( ptr u8 n -- )
   BMA-TAGS-U ! BMA-TAGS-A ! ;

TRUSTED: BMA-TAGS$ ( -- ptr u8 n )
   BMA-TAGS-A @ BMA-TAGS-U @ ;

: BMA-DATA! ( ptr u8 n -- ) {: a:ptr u :}
   u BMA-CAP > if E-BM-SCHEMA throw then
   a BMA-BUF u BYTE-COPY
   u BMA-U ! ;

: BMA-LOAD ( ptr u8 n -- )
   BMA-BUF BMA-CAP READ-ALL BMA-U ! ;

: BMA-RESET-SCAN ( -- )
   0 BMA-NEXT !
   0 BMA-LINE-N ! ;

: BMA-READ-LINE ( -- bool )
   BMA-BUF BMA-U @ BMA-NEXT @ BM-LINE-NEXT if
      BMA-NEXT !
      BMA-LINE!
      BMA-LINE-N @ 1+ BMA-LINE-N !
      BM-TRUE exit
   then
   drop 2drop BM-FALSE ;

: BMA-CHECK-SHAPE ( -- )
   BMA-RESET-SCAN
   BMA-READ-LINE 0= if E-BM-SCHEMA throw then
   BMA-LINE$ BM-REQUIRE-TASK-HEADER
   begin BMA-READ-LINE while
      BMA-LINE$ BM-BLANK-OR-COMMENT? 0= if
         BMA-LINE$ BM-TASK-FIELDS BM-REQUIRE-FIELDS
      then
   repeat ;

: BMA-FIELD-MATCH? ( n ptr u8 n -- bool ) {: idx want:ptr wantu :}
   BMA-LINE$ idx BM-TASK-FIELD$ want wantu STR= ;

: BMA-ROW-TAGS$ ( -- ptr u8 n )
   BMA-LINE$ BM-T-TAGS BM-TASK-FIELD$ ;

: BMA-CURRENT-HAS-TAG? ( -- bool )
   BMA-ROW-TAGS$ BMA-TAG$ BM-LIST-CONTAINS? ;

: BMA-TAGS-MATCH? ( ptr u8 n -- bool ) {: tags:ptr tagsu :}
   tagsu 0= if BM-TRUE exit then
   0 BMA-TAG-NEXT !
   begin tags tagsu 44 BMA-TAG-NEXT @ SPLIT-NEXT while
      BMA-TAG-NEXT !
      TRIM dup 0 > if
         BMA-TAG!
         BMA-CURRENT-HAS-TAG? 0= if BM-FALSE exit then
      else
         2drop
      then
   repeat
   drop 2drop BM-TRUE ;

: BMA-ROW-MATCH? ( -- bool )
   BMA-LINE$ BM-BLANK-OR-COMMENT? if BM-FALSE exit then
   BM-T-ID BMA-ID$ BMA-FIELD-MATCH? 0= if BM-FALSE exit then
   BM-T-NAME BMA-NAME$ BMA-FIELD-MATCH? 0= if BM-FALSE exit then
   BM-T-CATEGORY BMA-CAT$ BMA-FIELD-MATCH? 0= if BM-FALSE exit then
   BM-T-HARNESS BMA-HARNESS$ BMA-FIELD-MATCH? 0= if BM-FALSE exit then
   BM-T-CONV BMA-CONV$ BMA-FIELD-MATCH? 0= if BM-FALSE exit then
   BMA-TAGS$ BMA-TAGS-MATCH? ;

: BMA-FIND-ROW? ( -- bool )
   BMA-RESET-SCAN
   BMA-READ-LINE 0= if E-BM-SCHEMA throw then
   begin BMA-READ-LINE while
      BMA-ROW-MATCH? if BM-TRUE exit then
   repeat
   BM-FALSE ;

: BMA-EXPECT! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   BMA-TAGS! BMA-CONV! BMA-HARNESS! BMA-CAT! BMA-NAME! BMA-ID! ;

: BMA-REPORT-ON ( -- )
   -1 BMA-REPORT-MISSING ! ;

: BMA-REPORT-OFF ( -- )
   0 BMA-REPORT-MISSING ! ;

: BMA-PRINT-MISSING ( -- )
   s" FAIL: missing V2 task row " type
   BMA-ID$ type space
   BMA-NAME$ type space
   s" (" type
   BMA-CAT$ type s" /" type
   BMA-HARNESS$ type s" /" type
   BMA-CONV$ type s" /" type
   BMA-TAGS$ type s" )" type cr ;

: BMA-MISSING ( -- )
   BMA-REPORT-MISSING @ 0= 0= if BMA-PRINT-MISSING then
   E-BM-SCHEMA throw ;

: BMA-REQ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   BMA-EXPECT!
   BMA-FIND-ROW? 0= if BMA-MISSING then ;

: BMA-REQUIRE-EXPANDED-TASKS ( -- )
   s" 56" s" CALL-TWICE" s" quotation" s" forth" s" stack" s" v2" BMA-REQ
   s" 57" s" R-KEEP2" s" return-stack" s" forth" s" stack" s" v2" BMA-REQ
   s" 58" s" ROW-DUP" s" row-polymorphism" s" forth" s" stack" s" v2" BMA-REQ
   s" 59" s" UNTIL5" s" control-loop" s" forth" s" stack" s" v2" BMA-REQ
   s" 60" s" MEM-SWAPCELL" s" memory" s" forth" s" stack" s" v2" BMA-REQ
   s" 61" s" TRI" s" checked-combinator" s" forth" s" stack" s" v2" BMA-REQ
   s" 62" s" DATE-PARSE-OK?" s" date" s" stdlib" s" stack" s" parse-ymd" BMA-REQ
   s" 63" s" DATE-FORMAT-OK?" s" date" s" stdlib" s" stack" s" format-ymd" BMA-REQ
   s" 64" s" EPOCH-UTC-OK?" s" date" s" stdlib" s" stack" s" format-epoch-utc" BMA-REQ
   s" 65" s" MONO-ELAPSED?" s" time" s" stdlib" s" stack" s" mono-ns" BMA-REQ
   s" 66" s" INVALID-DATE?" s" date" s" stdlib" s" stack" s" invalid-date" BMA-REQ
   s" 67" s" AOT-MAIN-ARITH" s" aot-safe" s" aot" s" build-run" s" aot-positive" BMA-REQ
   s" 68" s" AOT-MAIN-STRING" s" aot-safe" s" aot" s" build-run" s" aot-positive" BMA-REQ
   s" 69" s" AOT-UNSAFE-HERE" s" aot-unsupported" s" aot-negative" s" reject" s" aot-negative" BMA-REQ
   s" 70" s" AOT-UNSAFE-ALLOT" s" aot-unsupported" s" aot-negative" s" reject" s" aot-negative" BMA-REQ
   s" 71" s" DIAG-REMOVE-PRODUCER" s" diagnostic-repair" s" forth" s" stack" s" v2,remove_producer" BMA-REQ
   s" 72" s" DIAG-ADD-PRODUCER" s" diagnostic-repair" s" forth" s" stack" s" v2,add_producer" BMA-REQ
   s" 73" s" DIAG-FIX-TYPE" s" diagnostic-repair" s" forth" s" stack" s" v2,fix_type" BMA-REQ
   s" 74" s" DIAG-FIX-RSTACK" s" diagnostic-repair" s" forth" s" stack" s" v2,fix_return_stack" BMA-REQ
   s" 75" s" DIAG-TRUSTED-BOUNDARY" s" diagnostic-repair" s" forth" s" stack" s" v2,trusted_boundary_required" BMA-REQ
   s" 122" s" DIAG-TRUST-BOUNDARY" s" diagnostic-repair" s" forth" s" stack" s" v2,trusted_boundary_required,trust" BMA-REQ
   s" 123" s" DIAG-SET-CHECK-BOUNDARY" s" diagnostic-repair" s" forth" s" stack" s" v2,trusted_boundary_required,set-check" BMA-REQ
   s" 76" s" DIAG-SIGNATURE-SYNTAX" s" diagnostic-repair" s" forth" s" stack" s" v2,fix_signature_syntax" BMA-REQ
   s" 77" s" DIAG-REWRITE-UNCHECKABLE" s" diagnostic-repair" s" forth" s" stack" s" v2,rewrite_uncheckable" BMA-REQ
   s" 78" s" FIND-FIRST-NEG" s" arrays" s" array" s" as" s" v2,find-index" BMA-REQ
   s" 79" s" ABS-EACH" s" arrays" s" array" s" aa" s" v2,map" BMA-REQ
   s" 80" s" ADD-INDEX" s" arrays" s" array" s" aa" s" v2,indexed-map" BMA-REQ
   s" 81" s" PREFIX-PROD" s" arrays" s" array" s" aa" s" v2,scan" BMA-REQ
   s" 82" s" REVERSE-INNER" s" arrays" s" array" s" aa" s" v2,reverse-range" BMA-REQ
   s" 83" s" STR-TRIM-OK?" s" strings" s" stdlib" s" stack" s" v2,trim" BMA-REQ
   s" 84" s" STR-SPLIT-OK?" s" strings" s" stdlib" s" stack" s" v2,split" BMA-REQ
   s" 85" s" STR-BUILDER-OK?" s" strings" s" stdlib" s" stack" s" v2,builder" BMA-REQ
   s" 86" s" STR-PARSE-I64-OK?" s" strings" s" stdlib" s" stack" s" v2,parse-i64" BMA-REQ
   s" 87" s" STR-PREFIX-SUFFIX-OK?" s" strings" s" stdlib" s" stack" s" v2,prefix-suffix" BMA-REQ
   s" 88" s" STR-SEARCH-OK?" s" strings" s" stdlib" s" stack" s" v2,search" BMA-REQ
   s" 89" s" MAP-COUNT-OK?" s" maps" s" stdlib" s" stack" s" v2,count" BMA-REQ
   s" 90" s" MAP-MISS-OK?" s" maps" s" stdlib" s" stack" s" v2,miss" BMA-REQ
   s" 91" s" MAP-UPDATE-OK?" s" maps" s" stdlib" s" stack" s" v2,update" BMA-REQ
   s" 92" s" MAP-COLLISION-OK?" s" maps" s" stdlib" s" stack" s" v2,collision" BMA-REQ
   s" 93" s" MAP-EACH-OK?" s" maps" s" stdlib" s" stack" s" v2,iteration" BMA-REQ
   s" 94" s" MAP-GROUP-OK?" s" maps" s" stdlib" s" stack" s" v2,grouping" BMA-REQ
   s" 95" s" RX-MATCH-OK?" s" regex" s" stdlib" s" stack" s" v2,match" BMA-REQ
   s" 96" s" RX-FIND-OK?" s" regex" s" stdlib" s" stack" s" v2,find" BMA-REQ
   s" 97" s" RX-COUNT-OK?" s" regex" s" stdlib" s" stack" s" v2,count" BMA-REQ
   s" 98" s" RX-BAD-PATTERN" s" regex" s" stdlib-negative" s" reject" s" v2,negative-syntax" BMA-REQ
   s" 99" s" RX-CAPACITY" s" regex" s" stdlib-negative" s" reject" s" v2,negative-capacity" BMA-REQ
   s" 100" s" FS-PATH-KINDS-OK?" s" files" s" stdlib" s" stack" s" v2,path-kind" BMA-REQ
   s" 101" s" FS-BASENAME-OK?" s" files" s" stdlib" s" stack" s" v2,basename" BMA-REQ
   s" 102" s" FS-JOIN-OK?" s" files" s" stdlib" s" stack" s" v2,join-path" BMA-REQ
   s" 103" s" FS-READ-ALL-OK?" s" files" s" stdlib-file" s" run" s" v2,read-all" BMA-REQ
   s" 104" s" FS-WRITE-ALL-OK?" s" files" s" stdlib-file" s" run" s" v2,write-all" BMA-REQ
   s" 105" s" FS-APPEND-OK?" s" files" s" stdlib-file" s" run" s" v2,append" BMA-REQ
   s" 106" s" FS-READ-CAPACITY" s" files" s" stdlib-negative" s" reject" s" v2,negative-capacity" BMA-REQ
   s" 107" s" PROC-RUN-RC-OK?" s" process" s" stdlib-process" s" run" s" v2,run-rc" BMA-REQ
   s" 108" s" PROC-CAPTURE-OUTERR-OK?" s" process" s" stdlib-process" s" run" s" v2,capture-streams" BMA-REQ
   s" 109" s" PROC-CAPTURE-NONZERO-OK?" s" process" s" stdlib-process" s" run" s" v2,nonzero-rc" BMA-REQ
   s" 110" s" PROC-CAPTURE-TIMEOUT" s" process" s" stdlib-negative" s" reject" s" v2,timeout" BMA-REQ
   s" 111" s" PROC-CAPTURE-TRUNCATED" s" process" s" stdlib-negative" s" reject" s" v2,negative-truncation" BMA-REQ
   s" 112" s" PROP-DEFAULTS-OK?" s" property" s" stdlib-property" s" run" s" v2,defaults" BMA-REQ
   s" 113" s" PROP-RND-SEQ-OK?" s" property" s" stdlib-property" s" run" s" v2,deterministic-rnd" BMA-REQ
   s" 114" s" PROP-GEN-SCRIPT-OK?" s" property" s" stdlib-property" s" run" s" v2,generator" BMA-REQ
   s" 115" s" PROP-SHRINK-OK?" s" property" s" stdlib-property" s" run" s" v2,shrink" BMA-REQ
   s" 116" s" PROP-BAD-SEED" s" property" s" stdlib-negative" s" reject" s" v2,negative-seed" BMA-REQ
   s" 117" s" BUILD-CHECK-SOURCE-OK?" s" build" s" stdlib-build" s" run" s" v2,check-source" BMA-REQ
   s" 118" s" BUILD-ARTIFACT-OK?" s" build" s" stdlib-build" s" run" s" v2,artifact" BMA-REQ
   s" 119" s" BUILD-STEP-STATUS" s" build" s" stdlib-negative" s" reject" s" v2,step-status" BMA-REQ
   s" 120" s" BUILD-RUN-ARTIFACT-OK?" s" build" s" stdlib-build" s" run" s" v2,run-artifact" BMA-REQ
   s" 121" s" BUILD-MISSING-ARTIFACT" s" build" s" stdlib-negative" s" reject" s" v2,missing-artifact" BMA-REQ ;

: BMA-CHECK ( -- )
   s" bench/llm/tasks.tsv" BMA-LOAD
   BMA-CHECK-SHAPE
   BMA-REQUIRE-EXPANDED-TASKS ;
