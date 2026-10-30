\ Real engine rebuilds through tools/native-build.f and the ordinary source
\ loader. The default run covers a warm build and an early edited FILE. The
\ explicit `-- qualify` run also measures middle and late edits and compares
\ each cached product with a fresh-cache build of the same source tree.
\
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- warm
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- legacy
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- kernel
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- emitter
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- writer
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- engine-artifact
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- handover
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- prefix
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- prefix-facts
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- emitter-role
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- emitter-live
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- emitter-probe
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- source-copy
\ bin/hb --load test/compiler/native-build-edit-e2e.f
\ bin/hb --load test/compiler/native-build-edit-e2e.f -- qualify
\ The printed private tree retains executable images, maps, logs and timings.

require src/lib/test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f
require src/lib/process-cwd.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/tree-copy.f
require src/lib/time.f
require src/lib/json-read.f
require tools/chain-run.f

package NATIVE-BUILD-EDIT-TEST

$20000 constant CAP
1800000 constant BUILD-MS
create ROOT PATH-CAP allot       variable ROOT-U
create PATH PATH-CAP allot       variable PATH-U
create HOST PATH-CAP allot       variable HOST-U
create OUTPUT PATH-CAP allot     variable OUTPUT-U
create CACHE PATH-CAP allot      variable CACHE-U
create TMP PATH-CAP allot        variable TMP-U
create CMP-A PATH-CAP allot      variable CMP-A-U
create CMP-B PATH-CAP allot      variable CMP-B-U
create BASE-ROOT PATH-CAP allot  variable BASE-ROOT-U
create DONOR-ROOT PATH-CAP allot variable DONOR-ROOT-U
create DONOR-HOST PATH-CAP allot variable DONOR-HOST-U
DYNAMIC-BUFFER EDIT-SOURCE u8
DYNAMIC-BUFFER IMAGE-SOURCE u8
variable IMAGE-U
create OUT CAP allot               variable OUT-U
create ERR CAP allot               variable ERR-U
create OVERLAY PATH-CAP allot    variable OVERLAY-U
create RACE-A PATH-CAP allot     variable RACE-A-U
create RACE-B PATH-CAP allot     variable RACE-B-U
create RACE-CORE PATH-CAP allot  variable RACE-CORE-U
TYPED-VARIABLE ENGINE-JSON-STATE JR:cursor
create BASE-ROLES 192 allot
create WARM-ROLES 192 allot
create EARLY-ROLES 192 allot
create COLD-ROLES 192 allot
create NEXT-ROLES 192 allot
variable RC
variable START-NS
variable FIRST-AT
variable SECOND-AT
variable ABS-WANT
variable PREFIX-WANT
variable DIV-WANT
variable MEM-WANT
variable LIVE-OVERRIDE

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;
: OUTPUT$ ( -- ptr u8 n ) OUTPUT OUTPUT-U @ ;
: CACHE$ ( -- ptr u8 n ) CACHE CACHE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: IMAGE$ ( -- ptr u8 n ) 0 IMAGE-SOURCE IMAGE-U @ ;
: IMAGE-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   IMAGE-U @ u + IMAGE-SOURCE-RESERVE
   a IMAGE-U @ IMAGE-SOURCE u BYTE-COPY
   u IMAGE-U +! ;
: IMAGE-INT ( n -- )
   SB-RESET FMT:SB-INT SB$ IMAGE-APPEND
   s"  " IMAGE-APPEND ;
: ROOT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U ! ;
: HOST! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a HOST u BYTE-COPY u HOST-U ! ;

: AT ( ptr u8 n -- ptr u8 n )
   {: rel:ptr relu:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH PATH-U !
   PATH PATH-U @ ;

: IMAGE-SYMBOL-PATH ( -- )
   S\" s\" " IMAGE-APPEND
   s" src/compiler/ir/symbol.f" AT SOURCE-ROOT:CANONICAL TTRUE IMAGE-APPEND
   S\" \" " IMAGE-APPEND ;

: PATH! ( ptr u8 n ptr u8 ptr n -- )
   {: rel:ptr relu:n dst:ptr size:ptr :}
   ROOT$ rel relu dst FS:JOIN-PATH size ! ;

: PREPARE ( -- )
   0 OVERLAY-U !
   s" native-build-edit-e2e" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ TREE-COPY:BUILD-SOURCES
   s" bin" AT FS:MAKE-DIR
   s" bin/hb" HOST HOST-U PATH!
   ENGINE-CANDIDATE:PATH$ HOST$ FS:COPY-FILE-STREAM
   HOST$ FS:CHMOD-X
   s" cached" AT FS:MAKE-DIR
   s" cold" AT FS:MAKE-DIR
   s" rejected" AT FS:MAKE-DIR
   s" logs" AT FS:MAKE-DIR
   s" scratch" AT FS:MAKE-DIR ;

\ The extra --load installs a different live compiler entry in the donor.
\ It emits an instruction in the next code slot after each compiled word,
\ changing the saved code while leaving every word's result intact.
: LIVE-EMITTER ( -- )
   s" src/live-emitter-override.f" AT
   S\" 1 set-tier\nrequire src/compiler/native/compiler.f\npackage BUILD-EDIT-OVERRIDE private\nTRUSTED: EMIT ( ptr u8 n -- )\n   NCOMP:COMPILE\n   HB-TARGET-LINUX-X86-64? if\n      $90909090 cp@ patch32  cp@ 16 + cp!\n   else\n      $D503201F cp@ patch32  cp@ 4 + cp!\n   then ;\n' EMIT NCOMP:DISPATCH!\n;package\n"
   FS:WRITE-ALL ;

\ Each edit rewrites only its own file in the private source tree. Distinct
\ packages leave the other two FILEs independent, while the REPL can observe
\ the compiled value and its source origin after the image boots.
: TWIN-AT ( ptr u8 n n -- n )
   {: body:ptr bodyu:n from:n :}
   from 0 < from bodyu >= or if s" native edit: bad twin span" 76 die then
   body from + bodyu from - s" : TWIN" FIND-SUB MATCH option
      none OF s" native edit: missing twin" 76 die ENDOF
      some OF IDX>N from + 2 + ENDOF
   ;MATCH ;

: DIVISION-FILE ( n -- )
   {: denominator:n :}
   s" src/core/division-probe.f" AT
   S\" package BUILD-EDIT-DIV\npublic\n: DENOMINATOR ( -- n ) " FS:WRITE-ALL
   SB-RESET denominator FMT:SB-INT
   s" src/core/division-probe.f" AT SB$ FS:APPEND-FILE
   s" src/core/division-probe.f" AT
   S\"  ;\n84 DENOMINATOR / constant DIV-VALUE\n;package\n" FS:APPEND-FILE ;

: EARLY ( bool -- )
   {: changed:bool :}
   s" src/compiler/ir/symbol.f" ROOT$ TREE-COPY:FILE
   s" src/compiler/ir/symbol.f" AT FS:FILE-SIZE {: prior:n :}
   S\" \npackage BUILD-EDIT-TWIN-A public\n: TWIN ( -- n ) 7 ;\n;package\npackage BUILD-EDIT-EARLY public\n: TWIN ( -- n ) 9 ;\n-1777 abs constant ABS-VALUE\n: ABS-READ ( -- n ) ABS-VALUE ;\n: DIRECT ( -- ptr u8 n bool ) kernel-producer$ ;\n: QUOTED ( -- ptr u8 n bool ) [: kernel-producer$ ;] execute ;\n"
      {: body:ptr bodyu:n :}
   body bodyu 0 TWIN-AT {: first:n :}
   body bodyu first 4 + TWIN-AT {: second:n :}
   prior first + FIRST-AT !
   prior second + SECOND-AT !
   s" src/compiler/ir/symbol.f" AT body bodyu FS:APPEND-FILE
   changed if
      28 DIV-WANT !
      3 DIVISION-FILE
   else
      42 DIV-WANT !
      2 DIVISION-FILE
   then
   s" src/compiler/ir/symbol.f" AT
   changed if
      S\" : VALUE ( -- n ) 111 ;\n;package\nrequire src/core/division-probe.f\n"
   else
      S\" : VALUE ( -- n ) 101 ;\n;package\nrequire src/core/division-probe.f\n"
   then FS:APPEND-FILE ;

: PREFIX-FILE ( n -- )
   {: value:n :}
   s" src/core/util.f" ROOT$ TREE-COPY:FILE
   s" src/core/util.f" AT
   S\" \npackage BUILD-EDIT-PREFIX public\n: VALUE ( -- n ) " FS:APPEND-FILE
   s" src/core/util.f" AT
   SB-RESET value FMT:SB-INT SB$ FS:APPEND-FILE
   s" src/core/util.f" AT
   S\"  ;\n;package\n" FS:APPEND-FILE
   s" src/core/engine-error-effects.f" ROOT$ TREE-COPY:FILE
   s" src/core/engine-error-effects.f" AT
   S\" PPRIM: BUILD-EDIT-PREFIX VALUE PE-N PE-OUT PPRIM;\n" FS:APPEND-FILE ;

: PREFIX-REJECT ( -- )
   s" src/core/util.f" AT
   S\" \npackage BUILD-EDIT-PREFIX-BAD public\n: BAD ( -- ptr u8 ) 1 ;\n;package\n"
   FS:APPEND-FILE ;

: ZERO-DENOMINATOR ( -- )
   s" src/core/division-probe.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size <> if
      s" native edit: early source read short" 76 die
   then
   0 EDIT-SOURCE size s" : DENOMINATOR ( -- n ) 3 ;" FIND-SUB MATCH option
      none OF s" native edit: denominator missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   s" : DENOMINATOR ( -- n ) 0 ;" off EDIT-SOURCE swap BYTE-COPY
   path pathu 0 EDIT-SOURCE size FS:WRITE-ALL
   EDIT-SOURCE-RELEASE ;

: REDEFINE-DIVISION ( -- )
   s" src/core/division-probe.f" AT
   S\" \npackage BUILD-EDIT-DIV-MEM\nprivate\nvariable HIT\n: / ( n n -- n ) 2drop 1 HIT ! 1 ;\npublic\n84 2 / constant VALUE\n;package\n"
   FS:APPEND-FILE ;

: MIDDLE ( bool -- )
   {: changed:bool :}
   s" src/compiler/native/publish.f" ROOT$ TREE-COPY:FILE
   s" src/compiler/native/publish.f" AT
   changed if
      S\" \npackage BUILD-EDIT-MIDDLE public\n: VALUE ( -- n ) 211 ;\n;package\n"
   else
      S\" \npackage BUILD-EDIT-MIDDLE public\n: VALUE ( -- n ) 201 ;\n;package\n"
   then FS:APPEND-FILE ;

: LATE ( bool -- )
   {: changed:bool :}
   s" src/habu/debug.f" ROOT$ TREE-COPY:FILE
   s" src/habu/debug.f" AT
   changed if
      S\" \npackage BUILD-EDIT-LATE public\n: VALUE ( -- n ) 311 ;\n;package\n"
   else
      S\" \npackage BUILD-EDIT-LATE public\n: VALUE ( -- n ) 301 ;\n;package\n"
   then FS:APPEND-FILE
   s" src/habu/debug.f" AT
   S\" package BUILD-EDIT-TYPED\npublic\nNEWTYPE token\nSTRUCTURE pair FIELD left n FIELD right n ;STRUCTURE\nSUMTYPE answer VARIANT value n ;VARIANT ;SUMTYPE\nTYPED-VARIABLE SAVED answer\nPERSISTED-PTR-VARIABLE LINK\ncreate NUMBER 42 ,\n: INIT ( -- ) 41 construct answer value SAVED ! NUMBER LINK ! ;\n: RESULT ( -- n ) SAVED @ MATCH answer value OF ENDOF ;MATCH ;\n: PAIR-SUM ( -- n ) 20 22 BUILD--EDIT--TYPED-PAIR:MAKE BUILD--EDIT--TYPED-PAIR:UNMAKE + ;\n: STORED ( -- n ) LINK @ @ ;\nINIT\n;package\n" FS:APPEND-FILE ;

: REJECT-LATE ( -- )
   s" src/habu/debug.f" AT
   S\" \npackage BUILD-EDIT-REJECT public\n: BAD ( -- ptr u8 ) 1 ;\n;package\n"
   FS:APPEND-FILE ;

\ The new FILE is loaded after checker handover and before the first runtime
\ requirement. Refining 1+ at that exact boundary must invalidate its saved
\ primitive input even though neither the FILE nor the checker source changes.
: MANIFEST-PREFIX ( ptr u8 n ptr u8 n -- )
   {: insert:ptr insertu:n needle:ptr needleu:n :}
   s" src/habu/native-runtime.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size <> if
      s" native edit: manifest read short" 76 die
   then
   0 EDIT-SOURCE size needle needleu FIND-SUB MATCH option
      none OF s" native edit: manifest anchor missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   path pathu 0 EDIT-SOURCE off FS:WRITE-ALL
   path pathu insert insertu FS:APPEND-FILE
   path pathu off EDIT-SOURCE size off - FS:APPEND-FILE
   EDIT-SOURCE-RELEASE ;

: HANDOVER-FILE ( -- )
   s" src/core/handover-step.f" AT
   S\" s\" handover step source\" type cr\npackage BUILD-HANDOVER\npublic\n: STEP ( ptr u8 -- ptr u8 ) 1+ ;\n;package\n"
   FS:WRITE-ALL
   S\" s\" src/core/handover-step.f\" required\n"
   S\" s\" src/core/enums.f\" required"
   MANIFEST-PREFIX ;

: HANDOVER-TRUST ( -- )
   S\" s\" 1+\" s\" n -- n\" trust\ns\" handover trust armed\" type cr\n"
   S\" s\" src/core/handover-step.f\" required"
   MANIFEST-PREFIX ;

\ The producer loads under the retained source checker. The consumer loads
\ after HANDOVER, before native/dict.f installs the fresh fixed reader.
: PREFIX-PRODUCER ( bool -- )
   {: changed:bool :}
   s" src/core/checker-owner-extension.f" ROOT$ TREE-COPY:FILE
   s" src/core/checker-owner-extension.f" AT
   changed if
      S\" \npackage BUILD-PREFIX-FACT\npublic\n43 constant VALUE\n: TWICE ( n -- n ) 2 * ;\n;package\n"
   else
      S\" \npackage BUILD-PREFIX-FACT\npublic\n41 constant VALUE\n: TWICE ( n -- n ) 2 * ;\n;package\n"
   then FS:APPEND-FILE ;

\ The replacement checker has published its target owner here, while the
\ retained source owner still certifies definitions until HANDOVER.
: PREFIX-LATE-PRODUCER ( bool -- )
   {: changed:bool :}
   s" src/core/layout-valid.f" ROOT$ TREE-COPY:FILE
   s" src/core/layout-valid.f" AT
   changed if
      S\" \npackage BUILD-PREFIX-LATE\npublic\n10 constant VALUE\n: TWICE ( n -- n ) 2 * ;\n;package\n"
   else
      S\" \npackage BUILD-PREFIX-LATE\npublic\n9 constant VALUE\n: TWICE ( n -- n ) 2 * ;\n;package\n"
   then FS:APPEND-FILE ;

: PREFIX-CONSUMER ( -- )
   s" src/core/prefix-fact-consumer.f" AT
   S\" package BUILD-PREFIX-ALIAS\npublic\nEXPORT BUILD-PREFIX-FACT:TWICE\n;package\npackage BUILD-PREFIX-CONSUMER\npublic\n: RESULT ( -- n ) BUILD-PREFIX-FACT:VALUE BUILD-PREFIX-ALIAS:TWICE ;\n: LATE ( -- n ) BUILD-PREFIX-LATE:VALUE BUILD-PREFIX-LATE:TWICE ;\n;package\n"
   FS:WRITE-ALL
   S\" s\" src/core/prefix-fact-consumer.f\" required\n"
   S\" s\" src/core/enums.f\" required"
   MANIFEST-PREFIX ;

\ Change the registers in one emitted primitive without changing its result.
\ The writer loads this private source after capture, so the next build emits
\ different kernel instructions from the same host image.
: PATCH-KERNEL-BODY ( ptr u8 n -- )
   {: data:ptr size:n :}
   data size S\" : BKERNELPRODUCER ( -- )\n   9 LKERNELDIGEST LABEL@ ADR,  9 G-PUSH" FIND-SUB
   MATCH option
      none OF s" native edit: kernel body missing" 76 die ENDOF
      some OF
         IDX>N data + {: at:ptr :}
         S\" : BKERNELPRODUCER ( -- )\n   8 LKERNELDIGEST LABEL@ ADR,  8 G-PUSH"
         at swap BYTE-COPY
      ENDOF
   ;MATCH ;

: KERNEL-BODY-EDIT ( -- )
   s" src/habu/habu2.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: data:ptr :}
   path pathu data size FS:READ-ALL size <> if
      s" native edit: kernel source read short" 76 die
   then
   data size PATCH-KERNEL-BODY
   path pathu data size FS:WRITE-ALL
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: LOG-PATH ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: label:ptr labelu:n ext:ptr extu:n :}
   SB-RESET s" logs/" SB-APPEND
   label labelu SB-APPEND ext extu SB-APPEND
   SB$ AT ;

: LOGS ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   label labelu s" .out" LOG-PATH OUT$ FS:WRITE-ALL
   label labelu s" .err" LOG-PATH ERR$ FS:WRITE-ALL ;

: ELAPSED ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   SB-RESET label labelu SB-APPEND
   s"  wall-ms " SB-APPEND
   TIME:MONO-NS START-NS @ - 1000000 / FMT:SB-INT
   S\" \n" SB-APPEND
   SB$ 2dup type
   s" logs/times.txt" AT 2swap FS:APPEND-FILE ;

: BUILD-SETUP ( ptr u8 n ptr u8 n -- )
   {: output:ptr outputu:n cache:ptr cacheu:n :}
   output outputu OUTPUT OUTPUT-U PATH!
   cache cacheu CACHE CACHE-U PATH!
   s" scratch" TMP TMP-U PATH!
   CACHE$ FS:MAKE-DIRS
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_OUT" >LEN CACHE$ >LEN PROC-ENV+
   s" HABU_UNDER_TEST" >LEN HOST$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN HOST$ >LEN PROC-ENV+
   s" HB_BUILD_TIMEOUT_MS" >LEN s" 1800000" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: BUILD-RUN-ON ( ptr u8 n ptr u8 n -- n )
   {: label:ptr labelu:n cwd:ptr cwdu:n :}
   TIME:MONO-NS START-NS !
   HOST$ >LEN cwd cwdu >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC !
   label labelu LOGS
   label labelu ELAPSED
   rc ;

: BUILD-RUN ( ptr u8 n -- n )
   ROOT$ BUILD-RUN-ON ;

: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: label:ptr labelu:n output:ptr outputu:n cache:ptr cacheu:n :}
   output outputu cache cacheu BUILD-SETUP
   s" --load" ARG
   LIVE-OVERRIDE @ if
      s" src/live-emitter-override.f" ARG
   then
   s" tools/native-build.f" ARG
   s" --" ARG
   OUTPUT$ ARG
   label labelu BUILD-RUN ;

: ENGINE-BUILD ( ptr u8 n ptr u8 n ptr u8 n bool -- n )
   {: label:ptr labelu:n output:ptr outputu:n cache:ptr cacheu:n json:bool :}
   output outputu cache cacheu BUILD-SETUP
   s" --load" ARG s" tools/hb-build.f" AT ARG
   s" --" ARG s" --engine" ARG
   json if s" --report-json" ARG then
   s" -o" ARG OUTPUT$ ARG
   OVERLAY-U @ 0= if label labelu ROOT$ BUILD-RUN-ON exit then
   label labelu OVERLAY OVERLAY-U @ BUILD-RUN-ON ;

: SUCCESS ( n -- )
   dup 0<> if OUT$ type ERR$ type then
   0 T=
   OUTPUT$ FS:FILE? TTRUE
   OUT$ s" native-build OK:" CONTAINS? TTRUE
   OUT$ s" (product image)" CONTAINS? TTRUE ;

\ Run `-- legacy` with an older host whose SOURCE-PRODUCER has no baked
\ kernel primitive. The product checks in RUN-IMAGE then cover the transition.
: LEGACY-HOST ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   PROC-ENV-INHERIT-MISSING
   HOST$ >LEN ROOT$ >LEN
   S\" : HB-LEGACY-ROLE ( -- ) SOURCE-PRODUCER:KERNEL SOURCE-PRODUCER:ROLE$ if 2drop s\" known\" else 2drop s\" absent\" then type cr ;\nHB-LEGACY-ROLE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc 0 T=
   OUT$ s" absent" CONTAINS? TTRUE ;

: COUNTS ( bool n -- )
   {: hit:bool compiled:n :}
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ s" files reused 0" CONTAINS?
      hit if TFALSE else TTRUE then
   compiled 0 > if
      OUT$ s"  compiled 0" CONTAINS? TFALSE
   then
   compiled 0= if
      OUT$ s"  compiled 0" CONTAINS? TTRUE
   then ;

: FILE-REUSE ( -- )
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ s" files reused 0" CONTAINS? TFALSE ;

: DEFINITION-COMPILE ( -- )
   OUT$ s" compiled " CONTAINS? TTRUE
   OUT$ s" compiled 0" CONTAINS? TFALSE ;

\ Change only the donor's abs primitive. Product source stays byte-for-byte
\ identical when the second host reuses the first host's FILE cache.
: PATCH-DONOR-ABS ( -- )
   s" src/habu/habu1.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size <> if
      s" native edit: donor source read changed" 76 die
   then
   s"    A G-POP  A 0 CMPI,  C-GE PRIM-DONE LABEL@ BCOND,  A SP A SUB,  PRIM-DONE LABEL@ LBL,  A G-PUSH ;"
      {: old:ptr oldu:n :}
   0 EDIT-SOURCE size old oldu FIND-SUB MATCH option
      none OF s" native edit: donor abs body missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   path pathu 0 EDIT-SOURCE off FS:WRITE-ALL
   path pathu s"    A G-POP  A 0 CMPI,  C-GE PRIM-DONE LABEL@ BCOND,  A SP A SUB,  PRIM-DONE LABEL@ LBL," FS:APPEND-FILE
   path pathu S\" \n   LBL {: ordinary:label :}\n" FS:APPEND-FILE
   path pathu S\"    A 1777 CMPI,  C-NE ordinary LABEL@ BCOND,\n" FS:APPEND-FILE
   path pathu s"    A A 1 ADDI,  ordinary LBL,  A G-PUSH ;" FS:APPEND-FILE
   path pathu off oldu + EDIT-SOURCE size off oldu + - FS:APPEND-FILE
   EDIT-SOURCE-RELEASE ;

: PATCH-DONOR-DIV ( -- )
   s" src/habu/habu1.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size <> if
      s" native edit: donor source read short" 76 die
   then
   S\" : BDIV ( -- )\n   B G-POP A G-POP  BDIV0?  A A B SDIV, A G-PUSH ;"
      {: old:ptr oldu:n :}
   0 EDIT-SOURCE size old oldu FIND-SUB MATCH option
      none OF s" native edit: donor division body missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   path pathu 0 EDIT-SOURCE off FS:WRITE-ALL
   path pathu S\" : BDIV ( -- )\n   B G-POP A G-POP  BDIV0?\n   LBL {: ordinary:label :} LBL {: done:label :}\n   A 84 CMPI, C-NE ordinary BCOND,\n   B 3 CMPI, C-NE ordinary BCOND,\n   A A B SDIV, A A 1 ADDI, A G-PUSH  done B,\n   ordinary LBL, A A B SDIV, A G-PUSH  done LBL, ;" FS:APPEND-FILE
   path pathu off oldu + EDIT-SOURCE size off oldu + - FS:APPEND-FILE
   EDIT-SOURCE-RELEASE ;

\ These forms copy the same register value and preserve the surrounding state,
\ but change the real backend's target FILE bytes.
: PATCH-EMITTER ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n old:ptr oldu:n new:ptr newu:n :}
   name nameu AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size <> if
      s" native edit: emitter source read changed" 76 die
   then
   0 EDIT-SOURCE size old oldu FIND-SUB MATCH option
      none OF s" native edit: emitter body missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   path pathu 0 EDIT-SOURCE off FS:WRITE-ALL
   path pathu new newu FS:APPEND-FILE
   path pathu off oldu + EDIT-SOURCE size off oldu + - FS:APPEND-FILE
   EDIT-SOURCE-RELEASE ;

: PATCH-DONOR-EMITTER ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/compiler/native/emit-x64.f"
      s" id 0 RES-R64  id 0 OPD-R64  s ENC-MOV-RR ;"
      s" id 0 RES-R64  id 0 OPD-R64 MEM-AT  s ENC-LEA ;"
      PATCH-EMITTER exit
   then
   s" src/compiler/native/emit.f"
   s" id 0 RESULT-REG  id 0 OPERAND-REG  ENC-MOV ;"
   s" id 0 RESULT-REG  id 0 OPERAND-REG  0 ENC-ADDI ;"
   PATCH-EMITTER ;

: DONOR-EMITTER ( -- )
   ROOT$ {: base:ptr baseu:n :}
   base BASE-ROOT baseu BYTE-COPY baseu BASE-ROOT-U !
   ROOT$ s" emitter-source" DONOR-ROOT FS:JOIN-PATH DONOR-ROOT-U !
   DONOR-ROOT DONOR-ROOT-U @ TREE-COPY:BUILD-SOURCES
   DONOR-ROOT DONOR-ROOT-U @ ROOT!
   s" bin" AT FS:MAKE-DIR
   s" donor" AT FS:MAKE-DIR
   s" logs" AT FS:MAKE-DIR
   s" scratch" AT FS:MAKE-DIR
   HOST$ s" bin/hb" AT FS:COPY-FILE-STREAM
   s" bin/hb" AT FS:CHMOD-X
   PATCH-DONOR-EMITTER
   s" emitter-donor" s" donor/hb" s" donor-cache" BUILD SUCCESS
   s" donor/hb" DONOR-HOST DONOR-HOST-U PATH!
   BASE-ROOT BASE-ROOT-U @ ROOT!
   DONOR-HOST DONOR-HOST-U @ HOST! ;

: DONOR-DEBUG ( -- )
   ROOT$ {: base:ptr baseu:n :}
   base BASE-ROOT baseu BYTE-COPY baseu BASE-ROOT-U !
   ROOT$ s" debug-source" DONOR-ROOT FS:JOIN-PATH DONOR-ROOT-U !
   DONOR-ROOT DONOR-ROOT-U @ TREE-COPY:BUILD-SOURCES
   DONOR-ROOT DONOR-ROOT-U @ ROOT!
   s" bin" AT FS:MAKE-DIR
   s" donor" AT FS:MAKE-DIR
   s" logs" AT FS:MAKE-DIR
   s" scratch" AT FS:MAKE-DIR
   HOST$ s" bin/hb" AT FS:COPY-FILE-STREAM
   s" bin/hb" AT FS:CHMOD-X
   PATCH-DONOR-EMITTER
   s" src/habu/debug.f" AT
      S\" \n\\ debug-only donor change\n" FS:APPEND-FILE
   s" debug-donor" s" donor/hb" s" donor-cache" BUILD SUCCESS
   s" donor/hb" DONOR-HOST DONOR-HOST-U PATH!
   BASE-ROOT BASE-ROOT-U @ ROOT!
   DONOR-HOST DONOR-HOST-U @ HOST! ;

: DONOR-B ( [ -- ] -- )
   {: edit :}
   ROOT$ {: base:ptr baseu:n :}
   base BASE-ROOT baseu BYTE-COPY baseu BASE-ROOT-U !
   ROOT$ s" donor-source" DONOR-ROOT FS:JOIN-PATH DONOR-ROOT-U !
   DONOR-ROOT DONOR-ROOT-U @ TREE-COPY:BUILD-SOURCES
   DONOR-ROOT DONOR-ROOT-U @ ROOT!
   s" bin" AT FS:MAKE-DIR
   s" donor" AT FS:MAKE-DIR
   s" logs" AT FS:MAKE-DIR
   s" scratch" AT FS:MAKE-DIR
   HOST$ s" bin/hb" AT FS:COPY-FILE-STREAM
   s" bin/hb" AT FS:CHMOD-X
   edit execute
   s" donor-build" s" donor/hb" s" cache" BUILD SUCCESS
   s" donor/hb" DONOR-HOST DONOR-HOST-U PATH!
   BASE-ROOT BASE-ROOT-U @ ROOT!
   DONOR-HOST DONOR-HOST-U @ HOST! ;

: PATCH-WRITER ( -- )
   s" src/habu/habu2.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size <> if
      s" native edit: writer source read changed" 76 die
   then
   s" ['] BKERNELPRODUCER FPRIM" {: old:ptr oldu:n :}
   0 EDIT-SOURCE size old oldu FIND-SUB MATCH option
      none OF s" native edit: kernel entry missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   path pathu 0 EDIT-SOURCE off FS:WRITE-ALL
   path pathu s" ['] BKERNELPRODUCER NOP, FPRIM" FS:APPEND-FILE
   path pathu off oldu + EDIT-SOURCE size off oldu + - FS:APPEND-FILE
   EDIT-SOURCE-RELEASE ;

: ROLE-FIND ( ptr u8 n -- ptr u8 )
   {: label:ptr labelu:n :}
   OUT-U @ labelu 65 + < if s" native edit: missing role line" 76 die then
   OUT-U @ labelu 65 + - 1+ 0 ?do
      OUT i + labelu label labelu STR= if
         OUT i + labelu + {: digest:ptr :}
         digest 64 + c@ 10 <> if
            s" native edit: malformed role digest" 76 die
         then
         digest unloop exit
      then
   loop
   s" native edit: missing role line" 76 die ;

: SAVE-ROLES ( ptr u8 -- ) {: dst:ptr :}
   s" role-k=" ROLE-FIND dst 64 BYTE-COPY
   s" role-c=" ROLE-FIND dst 64 + 64 BYTE-COPY
   s" role-p=" ROLE-FIND dst 128 + 64 BYTE-COPY ;

: SAME-ROLES? ( ptr u8 ptr u8 -- bool )
   {: a:ptr b:ptr :}
   a 192 b 192 STR= ;

: SAME-CHECKER? ( ptr u8 ptr u8 -- bool )
   {: a:ptr b:ptr :}
   a 64 + 64 b 64 + 64 STR= ;

: SAME-KERNEL? ( ptr u8 ptr u8 -- bool )
   {: a:ptr b:ptr :}
   a 64 b 64 STR= ;

: SAME-COMPILER? ( ptr u8 ptr u8 -- bool )
   {: a:ptr b:ptr :}
   a 128 + 64 b 128 + 64 STR= ;

: DIFFERENT-COMPILER? ( ptr u8 ptr u8 -- bool )
   {: a:ptr b:ptr :}
   a b SAME-COMPILER? 0= ;

: RUN-IMAGE ( n n n -- )
   {: early:n middle:n late:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   PROC-ENV-INHERIT-MISSING
   OUTPUT$ >LEN ROOT$ >LEN
   0 IMAGE-U !
   S\" s\" early=\" type BUILD-EDIT-EARLY:VALUE .\ns\" middle=\" type BUILD-EDIT-MIDDLE:VALUE .\ns\" late=\" type BUILD-EDIT-LATE:VALUE .\n" IMAGE-APPEND
   S\" s\" abs=\" type BUILD-EDIT-EARLY:ABS-READ .\n" IMAGE-APPEND
   S\" s\" typed=\" type BUILD-EDIT-TYPED:RESULT . BUILD-EDIT-TYPED:PAIR-SUM . BUILD-EDIT-TYPED:STORED . cr\n" IMAGE-APPEND
   S\" s\" bad-typed=\" type s\q HB-BAD-TYPED ( BUILD-EDIT-TYPED:token -- n ) 1+\q CHECK-CANDIDATE! . cr\n" IMAGE-APPEND
   PREFIX-WANT @ if
      S\" s\" prefix=\" type BUILD-EDIT-PREFIX:VALUE .\n" IMAGE-APPEND
   then
   S\" s\" div=\" type BUILD-EDIT-DIV:DIV-VALUE .\n" IMAGE-APPEND
   MEM-WANT @ 0<> if
      S\" s\" div-mem=\" type BUILD-EDIT-DIV-MEM:VALUE .\n" IMAGE-APPEND
   then
   S\" create HB-ROLE-HEX 64 allot\n: HB-DIGEST. ( ptr u8 n bool -- ) if drop HB-ROLE-HEX SHA256>HEX HB-ROLE-HEX 64 type else 2drop s\" absent\" type then cr ;\n: HB-ROLE. ( n -- ) SOURCE-PRODUCER:ROLE$ HB-DIGEST. ;\n" IMAGE-APPEND
   S\" s\" role-k=\" type SOURCE-PRODUCER:KERNEL HB-ROLE.\ns\" role-c=\" type SOURCE-PRODUCER:CHECKER HB-ROLE.\ns\" role-p=\" type SOURCE-PRODUCER:COMPILER HB-ROLE.\n" IMAGE-APPEND
   S\" s\" kernel-direct=\" type BUILD-EDIT-EARLY:DIRECT HB-DIGEST.\ns\" kernel-quote=\" type BUILD-EDIT-EARLY:QUOTED HB-DIGEST.\n" IMAGE-APPEND
   S\" variable HB-ORIGIN-FIRST\nvariable HB-ORIGIN-SECOND\n: HB-ORIGIN-PROBE ( -- )\n  " IMAGE-APPEND
   IMAGE-SYMBOL-PATH
   FIRST-AT @ IMAGE-INT
   S\" ENGINE-ORIGIN:SYMBOL-AT if HB-ORIGIN-FIRST ! else drop 0 HB-ORIGIN-FIRST ! then\n  " IMAGE-APPEND
   IMAGE-SYMBOL-PATH
   SECOND-AT @ IMAGE-INT
   S\" ENGINE-ORIGIN:SYMBOL-AT if HB-ORIGIN-SECOND ! else drop 0 HB-ORIGIN-SECOND ! then\n  s\" origin-first=\" type HB-ORIGIN-FIRST @ 0<> if 1 else 0 then . cr\n  s\" origin-second=\" type HB-ORIGIN-SECOND @ 0<> if 1 else 0 then . cr\n  s\" origin-distinct=\" type HB-ORIGIN-FIRST @ HB-ORIGIN-SECOND @ <> if 1 else 0 then . cr ;\nHB-ORIGIN-PROBE\n" IMAGE-APPEND
   S\" package HB-KVERIFY private\ncreate CTX SHA256-CTX-BYTES allot\ncreate HASH 32 allot\nCAST: >CODE ( n -- ptr u8 )\n" IMAGE-APPEND
   S\" : TARGET$ ( -- ptr u8 n )\n  HB-TARGET-LINUX? if s\" linux-aarch64\" exit then\n  HB-TARGET-MACOS? if s\" macos-aarch64\" exit then\n  HB-TARGET-LINUX-X86-64? if s\" linux-x86-64\" exit then\n  s\" unknown kernel target\" 76 die ;\n" IMAGE-APPEND
   S\" public\n: MATCH? ( -- bool )\n  kernel-producer$ {: digest:ptr size:n known:bool :}\n  known 0= size 32 <> or if false exit then\n  data-base RBASE-CELL + @ {: base:n :}\n" IMAGE-APPEND
   S\"   CTX SHA256-BEGIN\n  CTX s\" native-kernel-bytes-v1\" SHA256-FEED\n  CTX TARGET$ SHA256-FEED\n  CTX base >CODE digest NULL-PTR BYTE-VIEW - base - SHA256-FEED\n" IMAGE-APPEND
   S\"   CTX HASH SHA256-END\n  HASH 32 digest 32 STR= ;\n: MATCH-N ( -- n ) MATCH? if -1 else 0 then ;\n;package\ns\" role-k-matches-code=\" type HB-KVERIFY:MATCH-N . cr\n" IMAGE-APPEND
   IMAGE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   IMAGE-SOURCE-RELEASE
   rc 0<> if OUT$ type ERR$ type then
   rc 0 T=
   SB-RESET s" early=" SB-APPEND early FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE
   PREFIX-WANT @ if
      SB-RESET s" prefix=" SB-APPEND PREFIX-WANT @ FMT:SB-INT
      OUT$ SB$ CONTAINS? TTRUE
   then
   SB-RESET s" middle=" SB-APPEND middle FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE
   SB-RESET s" late=" SB-APPEND late FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE
   SB-RESET s" abs=" SB-APPEND ABS-WANT @ FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE
   SB-RESET s" div=" SB-APPEND DIV-WANT @ FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE
   MEM-WANT @ 0<> if OUT$ s" div-mem=1" CONTAINS? TTRUE then
   OUT$ s" origin-first=1" CONTAINS? TTRUE
   OUT$ s" origin-second=1" CONTAINS? TTRUE
   OUT$ s" origin-distinct=1" CONTAINS? TTRUE
   OUT$ S\" typed=41\n42\n42\n\n" CONTAINS? TTRUE
   OUT$ s" bad-typed=0" CONTAINS? TTRUE
   s" role-k=" ROLE-FIND {: kernel:ptr :}
   s" kernel-direct=" ROLE-FIND 64 kernel 64 STR= TTRUE
   s" kernel-quote=" ROLE-FIND 64 kernel 64 STR= TTRUE
   OUT$ s" role-k-matches-code=-1" CONTAINS? TTRUE ;

: SAME-PRODUCT ( -- )
   s" cached/hb" CMP-A CMP-A-U PATH!
   s" cold/hb" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TTRUE
   s" cached/hb.map" CMP-A CMP-A-U PATH!
   s" cold/hb.map" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TTRUE ;

: SAVE-BASE-PRODUCT ( -- )
   s" baseline" AT FS:MAKE-DIR
   s" cached/hb" CMP-A CMP-A-U PATH!
   s" baseline/hb" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ FS:COPY-FILE-STREAM
   s" cached/hb.map" CMP-A CMP-A-U PATH!
   s" baseline/hb.map" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ FS:COPY-FILE-STREAM ;

: SAME-BASE-PRODUCT ( -- )
   s" cached/hb" CMP-A CMP-A-U PATH!
   s" baseline/hb" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TTRUE
   s" cached/hb.map" CMP-A CMP-A-U PATH!
   s" baseline/hb.map" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TTRUE ;

: CHANGED-PRODUCT ( -- )
   s" baseline/hb" CMP-A CMP-A-U PATH!
   s" cached/hb" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TFALSE ;

: BASELINE ( -- )
   false EARLY false MIDDLE false LATE
   s" base" s" cached/hb" s" cache" BUILD SUCCESS
   OUT$ s" native-build: support image built" CONTAINS? TTRUE
   false 1 COUNTS
   101 201 301 RUN-IMAGE
   BASE-ROLES SAVE-ROLES
   SAVE-BASE-PRODUCT ;

: WARM ( -- )
   s" repeat" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   WARM-ROLES SAVE-ROLES
   BASE-ROLES WARM-ROLES SAME-ROLES? TTRUE
   SAME-BASE-PRODUCT ;

: EMITTER-EDIT ( -- )
   KERNEL-BODY-EDIT
   s" kernel" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   NEXT-ROLES SAVE-ROLES
   BASE-ROLES NEXT-ROLES SAME-KERNEL? TFALSE
   BASE-ROLES NEXT-ROLES SAME-CHECKER? TTRUE
   BASE-ROLES NEXT-ROLES SAME-COMPILER? TTRUE
   s" kernel-cold" s" cold/hb" s" cold-cache-kernel" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   COLD-ROLES SAVE-ROLES
   NEXT-ROLES COLD-ROLES SAME-ROLES? TTRUE
   SAME-PRODUCT ;

: EARLY-EDIT ( -- )
   true EARLY
   s" early" s" cached/hb" s" cache" BUILD SUCCESS
   true 1 COUNTS
   111 201 301 RUN-IMAGE
   EARLY-ROLES SAVE-ROLES
   \ Kernel bytes contain resolved payload addresses, which an edit can move.
   \ RUN-IMAGE checks each digest against its bytes; cold builds match below.
   BASE-ROLES EARLY-ROLES SAME-CHECKER? TTRUE
   BASE-ROLES EARLY-ROLES DIFFERENT-COMPILER? TTRUE
   s" early-cold" s" cold/hb" s" cold-cache-early" BUILD SUCCESS
   false 1 COUNTS
   111 201 301 RUN-IMAGE
   COLD-ROLES SAVE-ROLES
   EARLY-ROLES COLD-ROLES SAME-ROLES? TTRUE
   SAME-PRODUCT ;

: MIDDLE-EDIT ( -- )
   true MIDDLE
   s" middle" s" cached/hb" s" cache" BUILD SUCCESS
   true 1 COUNTS
   111 211 301 RUN-IMAGE
   NEXT-ROLES SAVE-ROLES
   EARLY-ROLES NEXT-ROLES SAME-CHECKER? TTRUE
   EARLY-ROLES NEXT-ROLES DIFFERENT-COMPILER? TTRUE
   s" middle-cold" s" cold/hb" s" cold-cache-middle" BUILD SUCCESS
   false 1 COUNTS
   111 211 301 RUN-IMAGE
   COLD-ROLES SAVE-ROLES
   NEXT-ROLES COLD-ROLES SAME-ROLES? TTRUE
   SAME-PRODUCT ;

: LATE-EDIT ( -- )
   true LATE
   s" late" s" cached/hb" s" cache" BUILD SUCCESS
   true 1 COUNTS
   111 211 311 RUN-IMAGE
   EARLY-ROLES SAVE-ROLES
   NEXT-ROLES EARLY-ROLES SAME-CHECKER? TTRUE
   NEXT-ROLES EARLY-ROLES SAME-COMPILER? TTRUE
   s" late-cold" s" cold/hb" s" cold-cache-late" BUILD SUCCESS
   false 1 COUNTS
   111 211 311 RUN-IMAGE
   COLD-ROLES SAVE-ROLES
   EARLY-ROLES COLD-ROLES SAME-ROLES? TTRUE
   SAME-PRODUCT ;

: REJECTION ( -- )
   REJECT-LATE
   s" rejected" s" rejected/hb" s" cache" BUILD 0<> TTRUE
   OUT$ s" expected: ptr u8 actual: n" CONTAINS?
   ERR$ s" expected: ptr u8 actual: n" CONTAINS? or TTRUE
   s" rejected/hb" AT FS:EXISTS? TFALSE
   s" rejected/hb.map" AT FS:EXISTS? TFALSE ;

: FINISH ( -- )
   s" native build edit artifacts: " type ROOT$ type cr
   T-REPORT ;

: ENGINE-SUCCESS ( n -- )
   dup 0<> if OUT$ type ERR$ type then
   0 T=
   OUTPUT$ FS:EXECUTABLE? TTRUE
   s" cached/hb.map" AT FS:FILE? TTRUE
   OUT$ s" hb-build OK:" CONTAINS? TTRUE ;

: ENGINE-PRODUCT-HAS ( ptr u8 n ptr u8 n -- )
   {: wanted:ptr wantedu:n absent:ptr absentu:n :}
   OUTPUT$ FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   OUTPUT$ 0 EDIT-SOURCE size FS:READ-ALL size T=
   0 EDIT-SOURCE size wanted wantedu CONTAINS? TTRUE
   0 EDIT-SOURCE size absent absentu CONTAINS? TFALSE
   EDIT-SOURCE-RELEASE ;

\ rt.f is loaded by the late source writer, outside the saved support image's
\ source closure. The next engine build must take an edit to it.
: ENGINE-RUNTIME-EDIT ( -- )
   s" src/habu/rt.f" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size EDIT-SOURCE-RESERVE
   path pathu 0 EDIT-SOURCE size FS:READ-ALL size T=
   0 EDIT-SOURCE size s" hb: cannot map guarded VM stack" FIND-SUB
   MATCH option
      none OF s" native edit: runtime diagnostic missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: off:n :}
   s" hb: REVIEW map guarded VM stack" off EDIT-SOURCE swap BYTE-COPY
   path pathu 0 EDIT-SOURCE size FS:WRITE-ALL
   EDIT-SOURCE-RELEASE ;

: RACE-A$ ( -- ptr u8 n ) RACE-A RACE-A-U @ ;
: RACE-B$ ( -- ptr u8 n ) RACE-B RACE-B-U @ ;
: RACE-CORE$ ( -- ptr u8 n ) RACE-CORE RACE-CORE-U @ ;

\ The support file changes core A to B before its required read and restores
\ A after that read has returned. The B engine reports the consumed body.
: ENGINE-SUPPORT-RACE ( -- )
   s" tools/native-build-core.f" RACE-CORE RACE-CORE-U PATH!
   s" scratch/core-a.f" RACE-A RACE-A-U PATH!
   s" scratch/core-b.f" RACE-B RACE-B-U PATH!
   RACE-CORE$ RACE-A$ FS:COPY-FILE-STREAM
   RACE-CORE$ RACE-B$ FS:COPY-FILE-STREAM
   s" scratch/core-b.f"
      S\" : RUN ( bool -- )\n   {: report:bool :}\n   BUILD-ARGS!"
      S\" : RUN ( bool -- )\n   {: report:bool :}\n   s\q support-B\q type cr\n   BUILD-ARGS!"
      PATCH-EMITTER
   s" tools/native-build-support.f"
      S\" 1 set-tier\nrequire tools/native-build-support-deps.f"
      S\" 1 set-tier\nrequire src/lib/fs-mutate.f\nrequire tools/native-build-support-deps.f"
      PATCH-EMITTER
   SB-RESET
   S\" : LOAD ( -- )\n   s\q " SB-APPEND RACE-B$ SB-APPEND
   S\" \q s\q " SB-APPEND RACE-CORE$ SB-APPEND
   S\" \q FS:COPY-FILE-STREAM\n   s\q tools/native-build-core.f\q required\n   s\q "
      SB-APPEND RACE-A$ SB-APPEND
   S\" \q s\q " SB-APPEND RACE-CORE$ SB-APPEND
   S\" \q FS:COPY-FILE-STREAM ;" SB-APPEND
   s" tools/native-build-support.f"
      S\" : LOAD ( -- )\n   s\q tools/native-build-core.f\q required ;"
      SB$ PATCH-EMITTER ;

: ENGINE-SUPPORT-A ( -- )
   RACE-A$ RACE-B$ FS:COPY-FILE-STREAM ;

: ENGINE-OVERLAY! ( ptr u8 n -- )
   ROOT$ 2swap OVERLAY FS:JOIN-PATH OVERLAY-U !
   OVERLAY OVERLAY-U @ FS:MAKE-DIRS ;

: ENGINE-OVERLAY-REWIND ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   root rootu s" src/habu" TMP FS:JOIN-PATH TMP-U !
   TMP TMP-U @ FS:MAKE-DIRS
   root rootu s" src/habu/window-rewind.f" TMP FS:JOIN-PATH TMP-U !
   TMP TMP-U @ S\" s\" engine rewind overlay selected\" 73 die\n" FS:WRITE-ALL ;

: ENGINE-OVERLAY-REFUSED ( n -- )
   73 T=
   ERR$ s" engine rewind overlay selected" CONTAINS? TTRUE
   OUTPUT$ FS:EXISTS? TFALSE ;

: ENGINE-JSON-SUCCESS ( n -- )
   dup 0<> if OUT$ type ERR$ type then
   0 T=
   OUTPUT$ FS:EXECUTABLE? TTRUE
   ERR-U @ 0 T=
   ENGINE-JSON-STATE OUT$ JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" schema" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-STR T=
   s" hb-build-report" JR:STR-EQ? TTRUE
   s" maker_ran" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-TRUE T=
   begin JR:NEXT dup JR:T-END <> while drop repeat drop
   JR:CLOSE ;

: ENGINE-COMPAT-TIMEOUT ( -- )
   s" rejected/hb" s" timeout-cache" BUILD-SETUP
   s" HB_BUILD_TIMEOUT_MS" >LEN s" 1" >LEN PROC-ENV-SET
   s" --load" ARG s" tools/native-build.f" ARG
   s" --" ARG OUTPUT$ ARG
   s" compat-timeout" BUILD-RUN 124 T=
   OUTPUT$ FS:EXISTS? TFALSE ;

: ENGINE-ARTIFACT ( -- )
   T-RESET
   s" public engine target rebuilds the product and takes runtime edits" T-LABEL
   PREPARE
   ENGINE-SUPPORT-RACE
   s" engine-cold" s" cached/hb" s" cache" false
      ENGINE-BUILD ENGINE-SUCCESS
   OUT$ s" support-B" CONTAINS? TTRUE
   s" hb: cannot map guarded VM stack"
      s" hb: REVIEW map guarded VM stack" ENGINE-PRODUCT-HAS
   ENGINE-SUPPORT-A
   s" engine-support-A" s" cached/hb" s" cache" false
      ENGINE-BUILD ENGINE-SUCCESS
   OUT$ s" support-B" CONTAINS? TFALSE
   SAVE-BASE-PRODUCT
   ENGINE-RUNTIME-EDIT
   s" engine-edited" s" cached/hb" s" cache" true
      ENGINE-BUILD ENGINE-JSON-SUCCESS
   s" hb: REVIEW map guarded VM stack"
      s" hb: cannot map guarded VM stack" ENGINE-PRODUCT-HAS
   CHANGED-PRODUCT
   s" overlay-a" ENGINE-OVERLAY!
   OVERLAY OVERLAY-U @ ENGINE-OVERLAY-REWIND
   s" engine-cwd-overlay" s" rejected/hb" s" cache" false
      ENGINE-BUILD ENGINE-OVERLAY-REFUSED
   0 OVERLAY-U !
   ROOT$ ENGINE-OVERLAY-REWIND
   s" engine-rewind-overlay" s" rejected/hb" s" cache" false
      ENGINE-BUILD ENGINE-OVERLAY-REFUSED
   s" src/habu/window-rewind.f" ROOT$ TREE-COPY:FILE
   ENGINE-COMPAT-TIMEOUT
   FINISH ;

: PREFIX-RUN ( n n -- )
   {: want:n late:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   PROC-ENV-INHERIT-MISSING
   OUTPUT$ >LEN ROOT$ >LEN
   S\" s\" prefix-result=\" type BUILD-PREFIX-CONSUMER:RESULT . cr\ns\" prefix-late=\" type BUILD-PREFIX-CONSUMER:LATE . cr\n" >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc 0<> if OUT$ type ERR$ type then
   rc 0 T=
   SB-RESET s" prefix-result=" SB-APPEND want FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE
   SB-RESET s" prefix-late=" SB-APPEND late FMT:SB-INT
   OUT$ SB$ CONTAINS? TTRUE ;

: PREFIX-FACTS ( -- )
   T-RESET
   s" retained and late prefix facts feed an exported consumer" T-LABEL
   PREPARE false PREFIX-PRODUCER false PREFIX-LATE-PRODUCER PREFIX-CONSUMER
   s" prefix-base" s" cached/hb" s" cache" BUILD SUCCESS
   82 18 PREFIX-RUN
   s" prefix-warm" s" cached/hb" s" cache" BUILD SUCCESS
   82 18 PREFIX-RUN
   true PREFIX-PRODUCER true PREFIX-LATE-PRODUCER
   s" prefix-edit" s" cached/hb" s" cache" BUILD SUCCESS
   86 20 PREFIX-RUN
   s" prefix-cold" s" cold/hb" s" cold-cache-prefix" BUILD SUCCESS
   86 20 PREFIX-RUN
   SAME-PRODUCT
   FINISH ;

\ The two product builds read identical source bytes and the same cache. Only
\ the retained host changes: its abs maps magnitude 1777 to 1778. A cached
\ compile-time constant from the first donor would leave the product at 1777.
: KERNEL-BOUNDARY ( -- )
   T-RESET
   s" retained kernel changes invalidate executed FILE construction" T-LABEL
   1777 ABS-WANT !
   PREPARE BASELINE WARM
   ['] PATCH-DONOR-ABS DONOR-B
   s" donor-reuse" s" cached/hb" s" cache" BUILD SUCCESS
   1778 ABS-WANT !
   101 201 301 RUN-IMAGE
   s" donor-cold" s" cold/hb" s" cold-cache-kernel" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   SAME-PRODUCT
   FINISH ;

: DIVISION-BOUNDARY ( -- )
   T-RESET
   s" FILE division construction follows source and retained kernel bytes" T-LABEL
   1777 ABS-WANT !
   PREPARE BASELINE WARM
   3 DIVISION-FILE  28 DIV-WANT !
   s" division-source" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   ZERO-DENOMINATOR
   s" division-zero" s" rejected/hb" s" cache" BUILD 0<> TTRUE
   OUT$ s" uncaught throw code -6400" CONTAINS?
   ERR$ s" uncaught throw code -6400" CONTAINS? or TTRUE
   s" rejected/hb" AT FS:EXISTS? TFALSE
   s" rejected/hb.map" AT FS:EXISTS? TFALSE
   3 DIVISION-FILE
   s" division-restored" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   REDEFINE-DIVISION
   1 MEM-WANT !
   s" division-redefined" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   s" division-memory-repeat" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   0 MEM-WANT !
   3 DIVISION-FILE
   ['] PATCH-DONOR-DIV DONOR-B
   s" division-kernel" s" cached/hb" s" cache" BUILD SUCCESS
   29 DIV-WANT !
   101 201 301 RUN-IMAGE
   s" division-kernel-cold" s" cold/hb" s" cold-cache-division" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   SAME-PRODUCT
   FINISH ;

\ An unchanged target FILE must use the donor's actual emitting compiler.
\ Debug-only source and a relocated donor checkout preserve the cached code.
: EMITTER-ROLE-BOUNDARY ( -- )
   T-RESET
   s" retained compiler changes invalidate emitted FILE code" T-LABEL
   1777 ABS-WANT !
   PREPARE BASELINE WARM
   SAVE-BASE-PRODUCT
   DONOR-EMITTER
   s" emitter-reuse" s" cached/hb" s" cache" BUILD SUCCESS
   CHANGED-PRODUCT
   101 201 301 RUN-IMAGE
   s" emitter-cold" s" cold/hb" s" cold-cache-emitter" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   SAME-PRODUCT
   DONOR-DEBUG
   s" debug-reuse" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   s" debug-cold" s" cold/hb" s" cold-cache-debug" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   SAME-PRODUCT
   FINISH ;

\ A prior --load can replace the live NCOMP dispatch without changing the
\ donor's baked SOURCE-PRODUCER:COMPILER description. Its cached FILEs must
\ rebuild under that unknown compiler and match a fresh-cache product.
: LIVE-EMITTER-BOUNDARY ( -- )
   T-RESET
   s" preloaded live emitter invalidates saved FILE code" T-LABEL
   1777 ABS-WANT !
   PREPARE BASELINE WARM
   SAVE-BASE-PRODUCT
   LIVE-EMITTER
   1 LIVE-OVERRIDE !
   s" live-reuse" s" cached/hb" s" cache" BUILD SUCCESS
   CHANGED-PRODUCT
   101 201 301 RUN-IMAGE
   s" live-cold" s" cold/hb" s" cold-cache-live" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   SAME-PRODUCT
   FINISH ;

\ A short child checks the supported --load path before the full donor build:
\ the baked compiler role is still present while the live dispatch has changed.
\ The wrapper also emits an extra code-slot instruction for a real definition.
: LIVE-EMITTER-PROBE ( -- )
   T-RESET
   s" preloaded emitter changes live dispatch, not baked role" T-LABEL
   PREPARE LIVE-EMITTER
   s" src/live-emitter-probe.f" AT
   S\" package BUILD-EDIT-PROBE private\nCAST: ENTRY>N ( [ ptr u8 n -- ] -- n )\nCAST: >CODE ( n -- ptr n )\n: B>N ( bool -- n ) if -1 else 0 then ;\n: LIVE-ROLE? ( -- bool ) SOURCE-PRODUCER:COMPILER SOURCE-PRODUCER:ROLE$ >r 2drop r> ;\n: LIVE-DISPATCH? ( -- bool ) data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ ['] NCOMP:COMPILE ENTRY>N <> ;\n: LIVE-SAMPLE ( -- n ) 3 ;\n: LIVE-MARKER? ( -- bool ) HB-TARGET-LINUX-X86-64? if cp@ 16 - >CODE @ $FFFFFFFF and $90909090 = else cp@ 4 - >CODE @ $FFFFFFFF and $D503201F = then ;\ns\" role-known=\" type LIVE-ROLE? B>N . cr\ns\" dispatch-changed=\" type LIVE-DISPATCH? B>N . cr\ns\" sample=\" type LIVE-SAMPLE . cr\ns\" marker=\" type LIVE-MARKER? B>N . cr\n;package\n" FS:WRITE-ALL
   PROC-CWD:ARGV-ENV-CWD-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" src/live-emitter-override.f" ARG
   s" src/live-emitter-probe.f" ARG
   HOST$ >LEN ROOT$ >LEN
   s" " >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   s" emitter-probe" LOGS
   rc 0<> if OUT$ type ERR$ type then
   rc 0 T=
   OUT$ s" role-known=-1" CONTAINS? TTRUE
   OUT$ s" dispatch-changed=-1" CONTAINS? TTRUE
   OUT$ s" sample=3" CONTAINS? TTRUE
   OUT$ s" marker=-1" CONTAINS? TTRUE
   FINISH ;

\ Changing only the source writer moves the primitive entry in the newly
\ emitted image. Unchanged definitions reuse their source compilation while
\ the edited writer is compiled for the fresh target.
: WRITER-RECOVERY ( -- )
   s" tools/native-emit.f" AT S\" \nE-FS-PATH throw\n" FS:APPEND-FILE
   s" writer-recovery.f" AT
   S\" 1 set-tier\nrequire tools/native-build-core.f\npackage WRITER-RECOVERY private\n: DRIVE ( -- n ) 0 SCRIPT-ARGV$ NATIVE-BUILD:RUN-PATH-RC ;\npublic\n: RUN ( -- )\n   tier@ {: prior:n :}\n   DRIVE E-FS-PATH <> if 76 throw then\n   tier@ prior <> if 76 throw then\n   BUILD-TARGET:IDLE-CK\n   s\" writer recovery: ok\" type cr ;\n;package\nWRITER-RECOVERY:RUN\n"
      FS:WRITE-ALL
   \ The child needs the original donor dictionary: loading a target rewinds
   \ its source records and installs its compiler before the writer fails.
   s" cached/hb" s" cache" BUILD-SETUP
   s" --load" ARG s" writer-recovery.f" ARG s" --" ARG
   OUTPUT$ ARG
   s" writer-throw" BUILD-RUN 0 T=
   OUT$ s" writer recovery: ok" CONTAINS? TTRUE ;

: WRITER-RELOCATION ( -- )
   T-RESET
   s" late writer definitions reuse and recompile edited source" T-LABEL
   1777 ABS-WANT !
   PREPARE
   \ This source is loaded only after capture. Its tier must not inherit the
   \ native-only target window's compilation scope.
   s" tools/native-emit.f" AT
   S\" \npackage BUILD-WRITER-TIER\npublic\n: CHECK ( -- ) HB-TARGET-LINUX-X86-64? if 1 else 0 then tier@ <> if s\" native edit: transient writer tier\" 76 die then ;\n;package\nBUILD-WRITER-TIER:CHECK\n"
      FS:APPEND-FILE
   false EARLY false MIDDLE false LATE
   s" base" s" cached/hb" s" cache" BUILD SUCCESS
   DEFINITION-COMPILE
   101 201 301 RUN-IMAGE
   BASE-ROLES SAVE-ROLES
   s" repeat" s" cached/hb" s" cache" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   PATCH-WRITER
   s" writer" s" cached/hb" s" cache" BUILD SUCCESS
   FILE-REUSE
   DEFINITION-COMPILE
   101 201 301 RUN-IMAGE
   NEXT-ROLES SAVE-ROLES
   BASE-ROLES NEXT-ROLES SAME-KERNEL? TFALSE
   s" writer-cold" s" cold/hb" s" cold-cache-writer" BUILD SUCCESS
   101 201 301 RUN-IMAGE
   SAME-PRODUCT
   WRITER-RECOVERY
   FINISH ;

: HANDOVER-REFUSAL ( -- )
   T-RESET
   s" FILE primitive input survives checker handover" T-LABEL
   PREPARE HANDOVER-FILE
   s" handover-base" s" cached/hb" s" cache" BUILD SUCCESS
   false 1 COUNTS
   OUT$ s" handover step source" CONTAINS? TTRUE
   s" handover-repeat" s" cached/hb" s" cache" BUILD SUCCESS
   HANDOVER-TRUST
   s" handover-rejected" s" rejected/hb" s" cache" BUILD 74 T=
   OUT$ s" handover trust armed" CONTAINS? TTRUE
   OUT$ s" handover step source" CONTAINS? TTRUE
   OUT$ s" primitive state changed" CONTAINS? TTRUE
   ERR$ s" habu: in step: at '1+'" CONTAINS? TTRUE
   ERR$ s" expected: ptr u8 actual: n" CONTAINS? TTRUE
   s" rejected/hb" AT FS:EXISTS? TFALSE
   s" rejected/hb.map" AT FS:EXISTS? TFALSE
   FINISH ;

\ The first FILEs execute on the retained checker and loader. The same
\ package's value stays callable after handover and after a warm import.
: PREFIX-REUSE ( -- )
   T-RESET
   s" pre-hook FILEs use retained checker and survive handover" T-LABEL
   1777 ABS-WANT !
   PREPARE
   401 PREFIX-WANT ! 401 PREFIX-FILE
   BASELINE
   WARM
   411 PREFIX-WANT ! 411 PREFIX-FILE
   s" prefix-edited" s" cached/hb" s" cache" BUILD SUCCESS
   true 1 COUNTS
   101 201 301 RUN-IMAGE
   NEXT-ROLES SAVE-ROLES
   s" prefix-cold" s" cold/hb" s" cold-cache-prefix" BUILD SUCCESS
   false 1 COUNTS
   101 201 301 RUN-IMAGE
   COLD-ROLES SAVE-ROLES
   NEXT-ROLES COLD-ROLES SAME-ROLES? TTRUE
   SAME-PRODUCT
   PREFIX-REJECT
   s" prefix-rejected" s" rejected/hb" s" cache" BUILD 0<> TTRUE
   OUT$ s" expected: ptr u8 actual: n" CONTAINS?
   ERR$ s" expected: ptr u8 actual: n" CONTAINS? or TTRUE
   s" rejected/hb" AT FS:EXISTS? TFALSE
   s" rejected/hb.map" AT FS:EXISTS? TFALSE
   FINISH ;

\ These two cold builds consume identical source bytes under different roots
\ with the same donor. Compare the complete products without normalization.
: SOURCE-COPY ( -- )
   T-RESET
   s" identical source copies produce identical runnable engines" T-LABEL
   1777 ABS-WANT !
   PREPARE
   ROOT$ BASE-ROOT swap BYTE-COPY ROOT-U @ BASE-ROOT-U !
   false EARLY false MIDDLE false LATE
   s" source-copy-a" s" cached/hb" s" cache" BUILD SUCCESS
   false 1 COUNTS
   101 201 301 RUN-IMAGE
   BASE-ROLES SAVE-ROLES
   BASE-ROOT BASE-ROOT-U @ s" source-copy" DONOR-ROOT FS:JOIN-PATH DONOR-ROOT-U !
   DONOR-ROOT DONOR-ROOT-U @ TREE-COPY:BUILD-SOURCES
   DONOR-ROOT DONOR-ROOT-U @ ROOT!
   s" cached" AT FS:MAKE-DIR
   s" logs" AT FS:MAKE-DIR
   s" scratch" AT FS:MAKE-DIR
   false EARLY false MIDDLE false LATE
   s" source-copy-b" s" cached/hb" s" cache" BUILD SUCCESS
   false 1 COUNTS
   101 201 301 RUN-IMAGE
   COLD-ROLES SAVE-ROLES
   BASE-ROLES COLD-ROLES SAME-ROLES? TTRUE
   BASE-ROOT BASE-ROOT-U @ s" cached/hb" CMP-A FS:JOIN-PATH CMP-A-U !
   s" cached/hb" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TTRUE
   BASE-ROOT BASE-ROOT-U @ s" cached/hb.map" CMP-A FS:JOIN-PATH CMP-A-U !
   s" cached/hb.map" CMP-B CMP-B-U PATH!
   CMP-A CMP-A-U @ CMP-B CMP-B-U @ CHAIN-RUN:SAME-FILES? TTRUE
   BASE-ROOT BASE-ROOT-U @ ROOT!
   FINISH ;

public
: MAIN ( -- )
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" source-copy" STR= if SOURCE-COPY exit then
      0 SCRIPT-ARGV$ s" kernel" STR= if KERNEL-BOUNDARY exit then
      0 SCRIPT-ARGV$ s" division" STR= if DIVISION-BOUNDARY exit then
      0 SCRIPT-ARGV$ s" writer" STR= if WRITER-RELOCATION exit then
      0 SCRIPT-ARGV$ s" engine-artifact" STR= if ENGINE-ARTIFACT exit then
      0 SCRIPT-ARGV$ s" handover" STR= if HANDOVER-REFUSAL exit then
      0 SCRIPT-ARGV$ s" prefix" STR= if PREFIX-REUSE exit then
      0 SCRIPT-ARGV$ s" prefix-facts" STR= if PREFIX-FACTS exit then
      0 SCRIPT-ARGV$ s" emitter-role" STR= if EMITTER-ROLE-BOUNDARY exit then
      0 SCRIPT-ARGV$ s" emitter-live" STR= if LIVE-EMITTER-BOUNDARY exit then
      0 SCRIPT-ARGV$ s" emitter-probe" STR= if LIVE-EMITTER-PROBE exit then
   then
   T-RESET
   s" native-build reuses definitions and recompiles edited source" T-LABEL
   1777 ABS-WANT !
   PREPARE
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" legacy" STR= if LEGACY-HOST then
   then
   BASELINE WARM
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" warm" STR= if FINISH exit then
      0 SCRIPT-ARGV$ s" legacy" STR= if FINISH exit then
      0 SCRIPT-ARGV$ s" emitter" STR= if EMITTER-EDIT FINISH exit then
   then
   EARLY-EDIT
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" qualify" T$=
      MIDDLE-EDIT LATE-EDIT REJECTION
   then
   FINISH ;
;package

NATIVE-BUILD-EDIT-TEST:MAIN
