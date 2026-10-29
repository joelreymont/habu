\ Source actions run on every load while individual definitions may be reused.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-REPLAY-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U
create CANON-EXPECT PATH-CAP allot variable CANON-EXPECT-U
variable EDIT
\ Set while a probe must name why its file missed (HABU_NATIVE_CACHE_DIAG).
variable DIAG
DYNAMIC-BUFFER COLD-PRODUCT u8
DYNAMIC-BUFFER WARM-PRODUCT u8

\ A saved build of root/app into root/product with root/cache as its out root.
: BUILD-IN ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: app:ptr appu:n product:ptr productu:n cache:ptr cacheu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   EDIT @ if s" HABU_REVIEW_EDIT" >LEN s" 1" >LEN PROC-ENV+ then
   DIAG @ if s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING
   s" --" ARG app appu AT ARG product productu AT ARG
   builder builderu S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" CAPTURE ;

: BUILD ( -- n ) s" app.f" s" app-hb" s" cache" BUILD-IN ;

: RUN-PRODUCT ( ptr u8 n -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" " CAPTURE ;

\ root/a and root/b hold the same non-empty bytes. AT shares one path buffer,
\ so each file is read before the other is named.
: SAME-FILE ( ptr u8 n ptr u8 n -- )
   {: a:ptr au:n b:ptr bu:n :}
   a au AT {: apath:ptr apathu:n :}
   apath apathu FS:FILE-SIZE {: asize:n :}
   asize 0 > TTRUE
   asize COLD-PRODUCT-RESERVE
   apath apathu 0 COLD-PRODUCT asize FS:READ-ALL asize T=
   b bu AT {: bpath:ptr bpathu:n :}
   bpath bpathu FS:FILE-SIZE {: bsize:n :}
   bsize asize T=
   bsize WARM-PRODUCT-RESERVE
   bpath bpathu 0 WARM-PRODUCT bsize FS:READ-ALL bsize T=
   0 COLD-PRODUCT asize 0 WARM-PRODUCT bsize STR= TTRUE ;

: REPORT ( -- ) OUT$ s" files reused " CONTAINS? TTRUE ;
: REUSED ( -- )
   REPORT
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE ;

: MUTATOR ( -- )
   0 INPUT-U !
   S\" : REVIEW-MAYBE-EDIT ( -- ) s\" HABU_REVIEW_EDIT\" GETENV nip 0 > if s\" "
      INPUT CAP INPUT-U BUF-APPEND
   s" child.f" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" s\" : REVIEW-CHILD ( -- n ) 42 ;\" FS:WRITE-ALL then ; REVIEW-MAYBE-EDIT\n"
      INPUT CAP INPUT-U BUF-APPEND
   s" mutate.f" INPUT INPUT-U @ PUT ;

variable REMOVED
: REMOVE-CONTAINER ( ptr u8 n -- )
   2dup s" .hfo" ENDS-WITH? if
      FS:REMOVE-FILE 1 REMOVED +! exit
   then 2drop ;

: EVICT-CACHE ( ptr u8 n -- )
   0 REMOVED !
   AT ['] REMOVE-CONTAINER FS:WALK-FILES
   REMOVED @ 0 > TTRUE ;

\ A replayed child may close the compiler session opened by an earlier parent
\ range. Probe in separate processes so a warm cache hit cannot inherit the
\ cold process's live context or its generation counters.
: SESSION-PROBE-RC-WITH ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n )
   {: prefix:ptr prefixu:n source:ptr sourceu:n cache:ptr cacheu:n
      tail:ptr tailu:n before:ptr beforeu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   0 INPUT-U !
   S\" 1 set-tier\n" INPUT CAP INPUT-U BUF-APPEND
   prefix prefixu INPUT CAP INPUT-U BUF-APPEND
   S\" NATIVE-RUNTIME:CAPTURE-PREPARE\n"
      INPUT CAP INPUT-U BUF-APPEND
   before beforeu INPUT CAP INPUT-U BUF-APPEND
   S\" s\" "
      INPUT CAP INPUT-U BUF-APPEND
   source sourceu AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" s\" repl\" NDEF-CACHE:LOAD\n" INPUT CAP INPUT-U BUF-APPEND
   tail tailu INPUT CAP INPUT-U BUF-APPEND
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   DIAG @ if s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING
   builder builderu INPUT INPUT-U @ CAPTURE ;

: SESSION-PROBE-WITH ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   SESSION-PROBE-RC-WITH SUCCESS ;

: SESSION-PROBE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   s" " SESSION-PROBE-WITH ;

: SCOPE-DEFS$ ( -- ptr u8 n )
   S\" package REVIEW-SCOPE-A\npublic\n: REVIEW-PICK ( -- n ) 40 ;\n;package\npackage REVIEW-SCOPE-B\npublic\n: REVIEW-PICK ( -- n ) 50 ;\n;package\n" ;

: SCOPE-TAIL$ ( -- ptr u8 n )
   S\" ;using\ns\" binding=\" type REVIEW-CONSUME . cr\n" ;

: SCOPE-PROBE ( ptr u8 n -- )
   {: scope:ptr scopeu:n :}
   SCOPE-DEFS$ s" scope-consumer.f" s" cache-using" SCOPE-TAIL$
      scope scopeu SESSION-PROBE-WITH ;

: SCOPE-CASES ( -- )
   s" a FILE consumer is keyed by its incoming using scope" T-LABEL
   s" cache-using" AT FS:MAKE-DIR
   s" scope-consumer.f" S\" : REVIEW-CONSUME ( -- n ) REVIEW-PICK 1+ ;\n" PUT
   S\" using REVIEW-SCOPE-A\n" SCOPE-PROBE
   REPORT
   OUT$ s" binding=41" CONTAINS? TTRUE
   S\" using REVIEW-SCOPE-B\n" SCOPE-PROBE
   REPORT
   OUT$ s" binding=51" CONTAINS? TTRUE
   S\" using REVIEW-SCOPE-B\n" SCOPE-PROBE
   REPORT
   OUT$ s" binding=51" CONTAINS? TTRUE
   S\" using REVIEW-SCOPE-A\n" SCOPE-PROBE
   OUT$ s" binding=41" CONTAINS? TTRUE
   s" an incoming global shadow makes a cached using binding refuse" T-LABEL
   SCOPE-DEFS$ s" scope-consumer.f" s" cache-using" SCOPE-TAIL$
      S\" : REVIEW-PICK ( -- n ) 99 ;\nusing REVIEW-SCOPE-A\n"
      SESSION-PROBE-RC-WITH 70 T=
   ERR$ s" E-USING-SHADOW-GLOBAL" CONTAINS? TTRUE
   OUT$ s" binding=" CONTAINS? 0= TTRUE ;

: CHECK-CANON-OUTPUT ( -- )
   0 INPUT-U !
   s" canon=" INPUT CAP INPUT-U BUF-APPEND
   CANON-EXPECT CANON-EXPECT-U @ INPUT CAP INPUT-U BUF-APPEND
   S\" \n" INPUT CAP INPUT-U BUF-APPEND
   OUT$ INPUT INPUT-U @ CONTAINS? TTRUE ;

: CANON-CASE ( -- )
   s" FILE replay preserves a stored canonical result until the next call" T-LABEL
   s" test/native-file-replay-e2e.f" SOURCE-ROOT:CANONICAL TTRUE
      {: a:ptr u:n :}
   a CANON-EXPECT u BYTE-COPY u CANON-EXPECT-U !
   s" cache-canon" AT FS:MAKE-DIR
   s" canonical.f"
      S\" package REVIEW-CANON\nprivate\nPERSISTED-PTR-VARIABLE SAVED\nvariable SAVED-U\n: CAPTURE ( -- ) s\" test/native-file-replay-e2e.f\" SOURCE-ROOT:CANONICAL drop {: a:ptr u:n :} a SAVED ! u SAVED-U ! ;\npublic\n: TEXT$ ( -- ptr u8 n ) SAVED @ SAVED-U @ ;\nCAPTURE\n;package\n"
      PUT
   s" " s" canonical.f" s" cache-canon"
      S\" s\" canon=\" type REVIEW-CANON:TEXT$ type cr\n" SESSION-PROBE
   REPORT
   CHECK-CANON-OUTPUT
   s" " s" canonical.f" s" cache-canon"
      S\" s\" canon=\" type REVIEW-CANON:TEXT$ type cr\n" SESSION-PROBE
   REPORT
   CHECK-CANON-OUTPUT ;

: SESSION-CLOSE-CASE ( -- )
   s" FILE replay closes a nested session before the next range" T-LABEL
   s" cache-close" AT FS:MAKE-DIR
   s" session-close.f" S\" IR-CTX:SESSION-CLOSE\n" PUT
   s" session-parent.f"
      S\" : REVIEW-BEFORE ( -- ) ;\ninclude session-close.f\n1 constant REVIEW-SESSION-VALUE\n" PUT
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" session-parent.f" s" cache-close"
      S\" s\" session=\" type NCOMP:SESSION-LIVE? bool>n . cr\ns\" open-result=\" type ' REVIEW-OPEN catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=0" CONTAINS? TTRUE
   OUT$ s" open-result=0" CONTAINS? TTRUE
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" session-parent.f" s" cache-close"
      S\" s\" session=\" type NCOMP:SESSION-LIVE? bool>n . cr\ns\" open-result=\" type ' REVIEW-OPEN catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=0" CONTAINS? TTRUE
   OUT$ s" open-result=0" CONTAINS? TTRUE ;

: FOREIGN-CASE ( -- )
   s" FILE source opening a foreign IR session remains a source load" T-LABEL
   s" cache-foreign" AT FS:MAKE-DIR
   s" foreign.f" S\" REVIEW-OPEN\n" PUT
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" foreign.f" s" cache-foreign"
      S\" s\" session=\" type IR-CTX:SESSION-LIVE? bool>n . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=-1" CONTAINS? TTRUE
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" foreign.f" s" cache-foreign"
      S\" s\" session=\" type IR-CTX:SESSION-LIVE? bool>n . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=-1" CONTAINS? TTRUE ;

: ROOT-REGISTRY-CASE ( -- )
   s" an app FILE does not reuse an unrelated live compiler session" T-LABEL
   s" cache-root-registry" AT FS:MAKE-DIR
   s" root-registry.f" S\" : REVIEW-ROOT-REGISTRY ( -- n ) 42 ;\n" PUT
   s" " s" root-registry.f" s" cache-root-registry"
      S\" s\" root-registry=\" type REVIEW-ROOT-REGISTRY . cr\n"
      S\" NCOMP:SESSION-ENSURE\n" SESSION-PROBE-WITH
   REPORT
   OUT$ s" root-registry=42" CONTAINS? TTRUE
   s" " s" root-registry.f" s" cache-root-registry"
      S\" s\" root-registry=\" type REVIEW-ROOT-REGISTRY . cr\n"
      S\" NCOMP:SESSION-ENSURE\n" SESSION-PROBE-WITH
   REPORT
   OUT$ s" root-registry=42" CONTAINS? TTRUE ;

: SYMBOL-CASE ( -- )
   s" FILE replay reuses a symbol interned then undefined before the unit" T-LABEL
   s" cache-symbol" AT FS:MAKE-DIR
   s" symbol.f"
      S\" package REVIEW-SYMBOL public\n: NUMBER ( -- n ) 5 ;\n;package\ns\" symbol-source-ran\" type cr\n" PUT
   S\" package REVIEW-SYMBOL public\n: KEEP ( -- n ) 7 ;\n: NUMBER ( -- n ) 3 ;\nundefine NUMBER\n;package\n"
      s" symbol.f" s" cache-symbol"
      S\" s\" number=\" type REVIEW-SYMBOL:NUMBER . cr\ns\" keep=\" type REVIEW-SYMBOL:KEEP . cr\n"
      s" "
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" symbol-source-ran" CONTAINS? TTRUE
   OUT$ s" number=5" CONTAINS? TTRUE
   OUT$ s" keep=7" CONTAINS? TTRUE
   S\" package REVIEW-SYMBOL public\n: KEEP ( -- n ) 7 ;\n: NUMBER ( -- n ) 3 ;\nundefine NUMBER\n;package\n"
      s" symbol.f" s" cache-symbol"
      S\" s\" number=\" type REVIEW-SYMBOL:NUMBER . cr\ns\" keep=\" type REVIEW-SYMBOL:KEEP . cr\n"
      s" "
      SESSION-PROBE-WITH
   REUSED
   OUT$ s" symbol-source-ran" CONTAINS? TTRUE
   OUT$ s" number=5" CONTAINS? TTRUE
   OUT$ s" keep=7" CONTAINS? TTRUE ;

: DEFER-SOURCE$ ( -- ptr u8 n )
   S\" : INSTALL ( -- ) ['] TWO is SLOT ;\n" ;

: DEFER-TAIL$ ( -- ptr u8 n )
   S\" INSTALL\ns\" private=\" type 40 SLOT . cr\n;package\ns\" global=\" type 40 SLOT . cr\n" ;

: DEFER-PREFIX$ ( -- ptr u8 n )
   S\" defer SLOT ( n -- n )\n: GLOBAL-IMPL ( n -- n ) 100 + ;\n: SET-GLOBAL ( -- ) ['] GLOBAL-IMPL is SLOT ;\nSET-GLOBAL\npackage REVIEW-DEFER\nprivate\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\n" ;

: DEFER-MOVED-PREFIX$ ( -- ptr u8 n )
   S\" defer SLOT ( n -- n )\n: GLOBAL-IMPL ( n -- n ) 100 + ;\n: SET-GLOBAL ( -- ) ['] GLOBAL-IMPL is SLOT ;\nSET-GLOBAL\ncreate REVIEW-PAD 256 allot\npackage REVIEW-DEFER\nprivate\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\n" ;

: DEFER-OUTSIDE$ ( -- ptr u8 n )
   S\" ;package\n: OUTSIDE ( -- ) REVIEW-DEFER:INSTALL ;\nOUTSIDE\ns\" outside=reached\" type cr\n" ;

: DEFER-PUBLIC-PREFIX$ ( -- ptr u8 n )
   S\" defer SLOT ( n -- n )\n: GLOBAL-IMPL ( n -- n ) 100 + ;\n: SET-GLOBAL ( -- ) ['] GLOBAL-IMPL is SLOT ;\nSET-GLOBAL\ncreate REVIEW-PAD 256 allot\npackage REVIEW-DEFER\nprivate\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\npublic\n" ;

\ An outside reference to the private INSTALL the file defined dies.
: OUTSIDE-REJECTED ( n -- )
   70 T=
   ERR$ s" undefined word 'REVIEW-DEFER:INSTALL'" CONTAINS? TTRUE
   OUT$ s" outside=reached" CONTAINS? TFALSE ;

\ A saved app whose defer.f loads inside the private package section.
: DEFER-APP ( -- )
   0 INPUT-U !
   DEFER-MOVED-PREFIX$ INPUT CAP INPUT-U BUF-APPEND
   S\" include defer.f\nINSTALL\n;package\n: MAIN ( -- ) 40 SLOT . ;\n"
      INPUT CAP INPUT-U BUF-APPEND
   s" defer-app.f" INPUT INPUT-U @ PUT ;

\ The package scope a file loads in is part of its key: the reused file's
\ INSTALL stays private, a load in the package's public section misses and
\ makes INSTALL public, the next public load hits that public object, and the
\ private load after it misses back and refuses the outside reference as the
\ hit did. A warm build reusing the package-loaded file saves the cold bytes.
: DEFER-VISIBILITY-CASE ( -- )
   DEFER-MOVED-PREFIX$ s" defer.f" s" cache-defer" DEFER-OUTSIDE$
      s" " SESSION-PROBE-RC-WITH
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   OUTSIDE-REJECTED
   -1 DIAG !
   DEFER-PUBLIC-PREFIX$ s" defer.f" s" cache-defer" DEFER-OUTSIDE$
      SESSION-PROBE
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   ERR$ s" native-file: dependency miss: scope changed" CONTAINS? TTRUE
   OUT$ s" outside=reached" CONTAINS? TTRUE
   DEFER-PUBLIC-PREFIX$ s" defer.f" s" cache-defer" DEFER-OUTSIDE$
      SESSION-PROBE
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   OUT$ s" outside=reached" CONTAINS? TTRUE
   DEFER-MOVED-PREFIX$ s" defer.f" s" cache-defer" DEFER-OUTSIDE$
      s" " SESSION-PROBE-RC-WITH
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   ERR$ s" native-file: dependency miss: scope changed" CONTAINS? TTRUE
   OUTSIDE-REJECTED
   0 DIAG !
   s" a warm build reusing a file loaded inside a package saves the cold product" T-LABEL
   s" cache-defer-app" AT FS:MAKE-DIR
   DEFER-APP
   s" defer-app.f" s" defer-hb" s" cache-defer-app" BUILD-IN SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   s" defer-app.f" s" defer-warm-hb" s" cache-defer-app" BUILD-IN SUCCESS
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   s" defer-hb" s" defer-warm-hb" SAME-FILE ;

: DEFER-CASE ( -- )
   s" FILE replay relocates a private defer cell from its exact declaration" T-LABEL
   s" cache-defer" AT FS:MAKE-DIR
   s" defer.f" DEFER-SOURCE$ PUT
   DEFER-PREFIX$ s" defer.f" s" cache-defer" DEFER-TAIL$ SESSION-PROBE
   REPORT
   OUT$ s" private=42" CONTAINS? TTRUE
   OUT$ s" global=140" CONTAINS? TTRUE
   DEFER-MOVED-PREFIX$ s" defer.f" s" cache-defer" DEFER-TAIL$ SESSION-PROBE
   REUSED
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   OUT$ s" private=42" CONTAINS? TTRUE
   OUT$ s" global=140" CONTAINS? TTRUE
   DEFER-VISIBILITY-CASE ;

: DEFER-PUBLISHED-CASE ( -- )
   s" a FILE source defer publishes its contract before caching its users" T-LABEL
   s" cache-defer-published" AT FS:MAKE-DIR
   s" published-defer.f"
      S\" package REVIEW-PUBLISHED\npublic\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\n: INSTALL ( -- ) ['] TWO is SLOT ;\n: USE ( n -- n ) SLOT ;\n;package\n" PUT
   S\" package REVIEW-CONTRACT\nprivate\nCAST: CONTRACT-XT ( n -- [ n -- ptr u8 n bool ] )\npublic\n: STATUS ( ptr u8 n -- n ) XREF-FIND-INDEX data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-EXT:RECORD-CONTRACT-OFF + CELL-VIEW @ CONTRACT-XT execute nip nip if -1 else 0 then ;\n;package\n"
      {: prefix:ptr prefixu:n :}
   S\" s\" contract=\" type s\" REVIEW-PUBLISHED:SLOT\" REVIEW-CONTRACT:STATUS . cr\nREVIEW-PUBLISHED:INSTALL\ns\" published=\" type 40 REVIEW-PUBLISHED:USE . cr\n"
      {: tail:ptr tailu:n :}
   prefix prefixu s" published-defer.f" s" cache-defer-published" tail tailu
      SESSION-PROBE
   REPORT
   OUT$ s" contract=-1" CONTAINS? TTRUE
   OUT$ s" published=42" CONTAINS? TTRUE
   prefix prefixu s" published-defer.f" s" cache-defer-published" tail tailu
      SESSION-PROBE
   REUSED
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   OUT$ s" contract=-1" CONTAINS? TTRUE
   OUT$ s" published=42" CONTAINS? TTRUE
   s" a warm build reusing a published defer saves the cold product" T-LABEL
   s" cache-defer-published-app" AT FS:MAKE-DIR
   s" published-app.f"
      S\" include published-defer.f\n: MAIN ( -- ) REVIEW-PUBLISHED:INSTALL 40 REVIEW-PUBLISHED:USE . ;\n" PUT
   s" published-app.f" s" published-hb" s" cache-defer-published-app" BUILD-IN
      SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   s" published-app.f" s" published-warm-hb" s" cache-defer-published-app"
      BUILD-IN SUCCESS
   OUT$ S\" files reused 2\n" CONTAINS? TTRUE
   s" published-hb" s" published-warm-hb" SAME-FILE ;

: DEFER-REPLACEMENT-CASE ( -- )
   s" FILE replay refuses a same-key colon or variable replacing a defer" T-LABEL
   s" cache-defer-replace" AT FS:MAKE-DIR
   s" replace.f" DEFER-SOURCE$ PUT
   DEFER-PREFIX$ s" replace.f" s" cache-defer-replace" DEFER-TAIL$
      SESSION-PROBE
   REPORT
   OUT$ s" private=42" CONTAINS? TTRUE
   S\" defer SLOT ( n -- n )\npackage REVIEW-DEFER private\n: SLOT ( n -- n ) 1 + ;\n: TWO ( n -- n ) 2 + ;\n"
      s" replace.f" s" cache-defer-replace" DEFER-TAIL$
      s" " SESSION-PROBE-RC-WITH 70 T=
   S\" defer SLOT ( n -- n )\npackage REVIEW-DEFER private\nvariable SLOT\ns\" SLOT\" s\" n -- n\" trust\n: TWO ( n -- n ) 2 + ;\n"
      s" replace.f" s" cache-defer-replace" DEFER-TAIL$
      s" " SESSION-PROBE-RC-WITH 70 T= ;

: DEFER-ALIAS-CASE ( -- )
   s" a cached installer binds the exported private defer declaration" T-LABEL
   s" cache-defer-alias" AT FS:MAKE-DIR
   s" alias-defer.f"
      S\" : ALIAS-INSTALL ( -- ) ['] ALIAS-TWO is REVIEW-ALIAS:SLOT ;\n"
      PUT
   S\" : ALIAS-TWO ( n -- n ) 2 + ;\npackage REVIEW-ALIAS\nprivate\ndefer SLOT ( n -- n )\npublic\nEXPORT SLOT\n;package\n"
      s" alias-defer.f" s" cache-defer-alias"
      S\" ALIAS-INSTALL\ns\" alias=\" type 40 REVIEW-ALIAS:SLOT . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" alias=42" CONTAINS? TTRUE
   S\" create REVIEW-ALIAS-PAD 256 allot\n: ALIAS-TWO ( n -- n ) 2 + ;\npackage REVIEW-ALIAS\nprivate\ndefer SLOT ( n -- n )\npublic\nEXPORT SLOT\n;package\n"
      s" alias-defer.f" s" cache-defer-alias"
      S\" ALIAS-INSTALL\ns\" alias=\" type 40 REVIEW-ALIAS:SLOT . cr\n"
      SESSION-PROBE
   REUSED
   OUT$ s" alias=42" CONTAINS? TTRUE ;

: DEFER-EFFECT-CASE ( -- )
   s" a changed defer effect rejects its incompatible cached installer" T-LABEL
   s" cache-defer-effect" AT FS:MAKE-DIR
   s" effect-defer.f" DEFER-SOURCE$ PUT
   DEFER-PREFIX$ s" effect-defer.f" s" cache-defer-effect"
      DEFER-TAIL$ SESSION-PROBE
   REPORT
   OUT$ s" private=42" CONTAINS? TTRUE
   S\" defer SLOT ( n -- n )\n: GLOBAL-IMPL ( n -- n ) 100 + ;\n: SET-GLOBAL ( -- ) ['] GLOBAL-IMPL is SLOT ;\nSET-GLOBAL\npackage REVIEW-DEFER\nprivate\ndefer SLOT ( -- n )\n: TWO ( n -- n ) 2 + ;\n"
      s" effect-defer.f" s" cache-defer-effect"
      S\" ;package\n" s" " SESSION-PROBE-RC-WITH 70 T= ;

: DEFER-RETIRED-CASE ( -- )
   s" a retained installer updates its retired defer, not the redeclared name" T-LABEL
   s" cache-defer-retired" AT FS:MAKE-DIR
   s" retired.f" DEFER-SOURCE$ PUT
   S\" package REVIEW-DEFER private\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\n: OLD-CALL ( n -- n ) SLOT ;\n"
      {: prefix:ptr prefixu:n :}
   S\" undefine SLOT\ndefer SLOT ( n -- n )\n: NEW-IMPL ( n -- n ) 100 + ;\n: SET-NEW ( -- ) ['] NEW-IMPL is SLOT ;\nSET-NEW\nINSTALL\ns\" old=\" type 40 OLD-CALL . cr\ns\" new=\" type 40 SLOT . cr\n;package\n"
      {: tail:ptr tailu:n :}
   prefix prefixu s" retired.f" s" cache-defer-retired" tail tailu
      SESSION-PROBE
   REPORT
   OUT$ s" old=42" CONTAINS? TTRUE
   OUT$ s" new=140" CONTAINS? TTRUE
   prefix prefixu s" retired.f" s" cache-defer-retired" tail tailu
      SESSION-PROBE
   REUSED
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   OUT$ s" old=42" CONTAINS? TTRUE
   OUT$ s" new=140" CONTAINS? TTRUE ;

: EVICTED-CHILD-CASE ( -- )
   s" evicted definition containers compile from source" T-LABEL
   s" cache-changed-child" AT FS:MAKE-DIR
   s" changed-parent.f"
      S\" include changed-child.f\n: REVIEW-PARENT ( -- n ) REVIEW-CHANGED:VALUE 1+ ;\n" PUT
   s" changed-child.f"
      S\" package REVIEW-CHANGED public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" " s" changed-parent.f" s" cache-changed-child"
      S\" s\" parent=\" type REVIEW-PARENT . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" parent=42" CONTAINS? TTRUE
   s" cache-changed-child" EVICT-CACHE
   s" " s" changed-parent.f" s" cache-changed-child"
      S\" s\" parent=\" type REVIEW-PARENT . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" parent=42" CONTAINS? TTRUE ;

: EMPTY-CHILD-CASE ( -- )
   s" an empty child can be selected without source-buffer bounds" T-LABEL
   s" cache-empty-child" AT FS:MAKE-DIR
   s" empty-child.f" s" " PUT
   s" empty-parent.f"
      S\" include empty-child.f\n: REVIEW-EMPTY ( -- n ) 42 ;\n" PUT
   s" " s" empty-parent.f" s" cache-empty-child"
      S\" s\" empty=\" type REVIEW-EMPTY . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" empty=42" CONTAINS? TTRUE
   s" " s" empty-parent.f" s" cache-empty-child"
      S\" s\" empty=\" type REVIEW-EMPTY . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" empty=42" CONTAINS? TTRUE ;

: TRAILING-COLON-CASE ( -- )
   s" a trailing-colon word is an ordinary global FILE input" T-LABEL
   s" cache-trailing-colon" AT FS:MAKE-DIR
   s" trailing-provider.f"
      S\" : REVIEW-EDGE: ( -- n ) 42 ;\n" PUT
   s" trailing-colon.f"
      S\" include trailing-provider.f\n: REVIEW-COLON-ANSWER ( -- n ) REVIEW-EDGE: ;\n" PUT
   s" " s" trailing-colon.f" s" cache-trailing-colon"
      S\" s\" colon=\" type REVIEW-COLON-ANSWER . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" colon=42" CONTAINS? TTRUE
   s" " s" trailing-colon.f" s" cache-trailing-colon"
      S\" s\" colon=\" type REVIEW-COLON-ANSWER . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" colon=42" CONTAINS? TTRUE ;

: NESTED-SCOPE-CASE ( -- )
   s" parent scope refs remain owned across a child FILE" T-LABEL
   s" cache-nested-scope" AT FS:MAKE-DIR
   s" nested-scope-child.f" S\" : B2 ( -- n ) 2 ;\n" PUT
   s" nested-scope-parent.f"
      S\" package REVIEW-NESTED public\n: A1 ( -- n ) 40 ;\ninclude nested-scope-child.f\n;package\n: REVIEW-NESTED-RESULT ( -- n ) REVIEW-NESTED:A1 REVIEW-NESTED:B2 + ;\n" PUT
   s" " s" nested-scope-parent.f" s" cache-nested-scope"
      S\" s\" nested=\" type REVIEW-NESTED-RESULT . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" nested=42" CONTAINS? TTRUE
   s" " s" nested-scope-parent.f" s" cache-nested-scope"
      S\" s\" nested=\" type REVIEW-NESTED-RESULT . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" nested=42" CONTAINS? TTRUE
   s" nested-scope-child.f" S\" : B2 ( -- n ) 3 ;\n" PUT
   s" " s" nested-scope-parent.f" s" cache-nested-scope"
      S\" s\" nested=\" type REVIEW-NESTED-RESULT . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" nested=43" CONTAINS? TTRUE ;

: QUALIFIED-GLOBAL-CASE ( -- )
   s" qualified open-package global fallback keeps source semantics" T-LABEL
   s" cache-qualified-global" AT FS:MAKE-DIR
   s" qualified-provider.f"
      S\" : GLOBAL-WORD ( -- n ) 91 ;\n" PUT
   s" qualified-global.f"
      S\" include qualified-provider.f\npackage REVIEW-QUAL public\n: ANSWER ( -- n ) REVIEW-QUAL:GLOBAL-WORD ;\n;package\n" PUT
   s" " s" qualified-global.f" s" cache-qualified-global"
      S\" s\" answer=\" type REVIEW-QUAL:ANSWER . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" answer=91" CONTAINS? TTRUE
   s" " s" qualified-global.f" s" cache-qualified-global"
      S\" s\" answer=\" type REVIEW-QUAL:ANSWER . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" answer=91" CONTAINS? TTRUE ;

: CHILD-CLOSES-PACKAGE-CASE ( -- )
   s" a descendant after an inherited package closes can hit" T-LABEL
   s" cache-child-closes" AT FS:MAKE-DIR
   s" closes-grand.f" S\" : REVIEW-GRAND ( -- n ) 2 ;\n" PUT
   s" closes-child.f" S\" ;package\ninclude closes-grand.f\n" PUT
   s" closes-parent.f"
      S\" package REVIEW-CLOSE public\n: VALUE ( -- n ) 40 ;\ninclude closes-child.f\n: REVIEW-CLOSE-RESULT ( -- n ) REVIEW-CLOSE:VALUE REVIEW-GRAND + ;\n" PUT
   s" " s" closes-parent.f" s" cache-child-closes"
      S\" s\" closes=\" type REVIEW-CLOSE-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" closes=42" CONTAINS? TTRUE
   s" " s" closes-parent.f" s" cache-child-closes"
      S\" s\" closes=\" type REVIEW-CLOSE-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" closes=42" CONTAINS? TTRUE ;

\ The diagnostic refusal line for the file name (a path's last component)
\ names exactly reason, and no object line names the file.
: REFUSED-ONLY ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n reason:ptr reasonu:n :}
   0 INPUT-U !
   s" /" INPUT CAP INPUT-U BUF-APPEND
   name nameu INPUT CAP INPUT-U BUF-APPEND
   s"  reason=" INPUT CAP INPUT-U BUF-APPEND
   reason reasonu INPUT CAP INPUT-U BUF-APPEND
   S\" \n" INPUT CAP INPUT-U BUF-APPEND
   ERR$ INPUT INPUT-U @ CONTAINS? TTRUE
   0 INPUT-U !
   s" /" INPUT CAP INPUT-U BUF-APPEND
   name nameu INPUT CAP INPUT-U BUF-APPEND
   s"  key=" INPUT CAP INPUT-U BUF-APPEND
   ERR$ INPUT INPUT-U @ CONTAINS? TFALSE ;

\ opens-parent.f closes the package its child left open, which a hit would not
\ replay, so it refuses scope-after-load on every build.
: CHILD-OPENS-PACKAGE-CASE ( -- )
   s" a child opening a package leaves later sibling dependencies live" T-LABEL
   s" cache-child-opens" AT FS:MAKE-DIR
   s" opens-child.f"
      S\" package REVIEW-OPEN public\n: VALUE ( -- n ) 40 ;\n" PUT
   s" opens-sibling.f"
      S\" : REVIEW-SIBLING ( -- n ) REVIEW-OPEN:VALUE 2 + ;\n" PUT
   s" opens-independent.f"
      S\" : REVIEW-INDEPENDENT ( -- n ) 5 ;\n" PUT
   s" opens-parent.f"
      S\" include opens-child.f\n;package\ninclude opens-sibling.f\ninclude opens-independent.f\n: REVIEW-OPEN-RESULT ( -- n ) REVIEW-SIBLING REVIEW-INDEPENDENT + ;\n" PUT
   -1 DIAG !
   s" " s" opens-parent.f" s" cache-child-opens"
      S\" s\" opens=\" type REVIEW-OPEN-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" opens=47" CONTAINS? TTRUE
   s" opens-parent.f" s" scope-after-load" REFUSED-ONLY
   s" " s" opens-parent.f" s" cache-child-opens"
      S\" s\" opens=\" type REVIEW-OPEN-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" opens=47" CONTAINS? TTRUE
   s" opens-parent.f" s" scope-after-load" REFUSED-ONLY
   0 DIAG !
   s" opens-child.f"
      S\" package REVIEW-OPEN public\n: VALUE ( -- n ) 41 ;\n" PUT
   s" " s" opens-parent.f" s" cache-child-opens"
      S\" s\" opens=\" type REVIEW-OPEN-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" opens=48" CONTAINS? TTRUE ;

\ PTR-VARIABLE's does> re-stamps the record it created, which reissues the
\ record's occurrence and publishes it again. The cold object must name the slot
\ by the key a warm install, which publishes it once, gives it.
: PTR-RESTAMP-CASE ( -- )
   s" a FILE reading a does> record it defines reuses its stored key" T-LABEL
   s" cache-restamp" AT FS:MAKE-DIR
   s" restamp.f"
      S\" package REVIEW-RESTAMP\npublic\nPTR-VARIABLE SLOT\n: READ ( -- ptr u8 ) SLOT @ ;\n;package\n" PUT
   S\" s\" restamp\" drop REVIEW-RESTAMP:SLOT !\ns\" restamp=\" type REVIEW-RESTAMP:READ 7 type cr\n"
      {: tail:ptr tailu:n :}
   s" " s" restamp.f" s" cache-restamp" tail tailu SESSION-PROBE
   REPORT
   OUT$ s" restamp=restamp" CONTAINS? TTRUE
   s" " s" restamp.f" s" cache-restamp" tail tailu SESSION-PROBE
   REUSED
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   OUT$ s" restamp=restamp" CONTAINS? TTRUE ;

\ rv-base.f's definer RV-DEF reads RV, then the file retires RV and declares a
\ live RV of the same spelling. RD-DEF calls the defer RD, bound to impl, and
\ the file then retires RD. rv-user.f runs RV-DEF and RD-DEF at top level, so
\ its object depends on the retired RV's DATA and the retired RD's cell by key
\ and occurrence. An unrelated edit to rv-base.f keeps rv-user.f reused, and
\ the warm product is the cold one; rebinding the retired RD, or a new value
\ stored in the retired RV, misses rv-user.f, while the live RV's value stays.
: RV-BASE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: value:ptr valueu:n impl:ptr implu:n tail:ptr tailu:n :}
   0 INPUT-U !
   S\" variable RV\n" INPUT CAP INPUT-U BUF-APPEND
   value valueu INPUT CAP INPUT-U BUF-APPEND
   S\"  RV !\n: RV-DEF ( -- ) create RV @ , ;\nundefine RV\nvariable RV\n5 RV !\n"
      INPUT CAP INPUT-U BUF-APPEND
   S\" defer RD ( -- n )\n: RD-SEVEN ( -- n ) 7 ;\n: RD-NINE ( -- n ) 9 ;\n: RD-SET ( -- ) ['] "
      INPUT CAP INPUT-U BUF-APPEND
   impl implu INPUT CAP INPUT-U BUF-APPEND
   S\"  is RD ;\nRD-SET\n: RD-DEF ( -- ) create RD , ;\nundefine RD\n"
      INPUT CAP INPUT-U BUF-APPEND
   tail tailu INPUT CAP INPUT-U BUF-APPEND
   s" rv-base.f" INPUT INPUT-U @ PUT ;

: RV-REUSED ( -- bool )
   0 INPUT-U !
   s" native-file: reused path=" INPUT CAP INPUT-U BUF-APPEND
   s" rv-user.f" AT INPUT CAP INPUT-U BUF-APPEND
   ERR$ INPUT INPUT-U @ CONTAINS? ;

: RETIRED-DATA-CASE ( -- )
   s" a run reaching a retired variable's DATA reuses its file" T-LABEL
   s" cache-retired-data" AT FS:MAKE-DIR
   s" 7" s" RD-SEVEN" s" " RV-BASE
   s" rv-user.f" S\" RV-DEF RX\nRD-DEF RY\n" PUT
   s" rv-app.f" S\" require rv-base.f\nrequire rv-user.f\n: MAIN ( -- ) RX @ . RY @ . RV @ . ;\n" PUT
   -1 DIAG !
   s" rv-app.f" s" rv-cold-hb" s" cache-retired-data" BUILD-IN SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   s" rv-cold-hb" RUN-PRODUCT SUCCESS OUT$ S\" 7\n7\n5\n" CONTAINS? TTRUE
   s" 7" s" RD-SEVEN" S\" \\ unrelated\n" RV-BASE
   s" rv-app.f" s" rv-warm-hb" s" cache-retired-data" BUILD-IN SUCCESS
   RV-REUSED TTRUE
   s" rv-cold-hb" s" rv-warm-hb" SAME-FILE
   s" a retired defer rebound misses the file whose run called it" T-LABEL
   s" 7" s" RD-NINE" s" " RV-BASE
   s" rv-app.f" s" rv-rebind-hb" s" cache-retired-data" BUILD-IN SUCCESS
   RV-REUSED TFALSE
   s" rv-rebind-hb" RUN-PRODUCT SUCCESS OUT$ S\" 7\n9\n5\n" CONTAINS? TTRUE
   s" a new value in a retired variable misses the file whose run read it" T-LABEL
   s" 9" s" RD-NINE" s" " RV-BASE
   s" rv-app.f" s" rv-new-hb" s" cache-retired-data" BUILD-IN SUCCESS
   ERR$ s" native-file: dependency miss: data changed" CONTAINS? TTRUE
   s" rv-new-hb" RUN-PRODUCT SUCCESS OUT$ S\" 9\n9\n5\n" CONTAINS? TTRUE
   0 DIAG ! ;

\ A reused file's FFI declarations do not run, so the declarer's working state
\ must rest at the file boundary: a warm build that reuses a file saves the
\ bytes a cold build saves. The window's first declaring file grows the
\ declaration rows and is refused ffi-grown, so zl.f grows them and the file
\ under test is reused. Each file is the last of its product, since a later
\ declaration word would rest what it left. zp.f declares a long symbol and
\ then a short one and ends with a library selection; its first build follows
\ zl.f's selection of the same token lengths and its warm one follows none, so
\ the selection's tokens must rest too. za.f and zb.f each catch a throw from a
\ declaration word, a selection's and a declaration's, which must rest the
\ cells it wrote all the same.
: FFI-ZL ( ptr u8 n -- )
   {: tail:ptr tailu:n :}
   0 INPUT-U !
   S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY m 6\nVERSIONED-LIBRARY z 1\nFUNCTION: ZL-CRC crc32 ( n ptr u8 n -- n ) ;FUNCTION\n"
      INPUT CAP INPUT-U BUF-APPEND
   tail tailu INPUT CAP INPUT-U BUF-APPEND
   s" zl.f" INPUT INPUT-U @ PUT ;

\ ffi-app.f loads zl.f and then file; MAIN passes seed to zl.f's function.
: FFI-APP ( ptr u8 n ptr u8 n -- )
   {: file:ptr fileu:n seed:ptr seedu:n :}
   0 INPUT-U !
   S\" require zl.f\nrequire " INPUT CAP INPUT-U BUF-APPEND
   file fileu INPUT CAP INPUT-U BUF-APPEND
   S\" \n: MAIN ( -- ) " INPUT CAP INPUT-U BUF-APPEND
   seed seedu INPUT CAP INPUT-U BUF-APPEND
   S\"  s\" abc\" ZL-CRC . cr ;\n" INPUT CAP INPUT-U BUF-APPEND
   s" ffi-app.f" INPUT INPUT-U @ PUT ;

\ Build file's product in cache, rebuild it there after zl.f changes, which
\ reuses file, and compare it with a build in the empty cold cache.
: FFI-REUSE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: file:ptr fileu:n body:ptr bodyu:n cache:ptr cacheu:n cold:ptr coldu:n :}
   cache cacheu AT FS:MAKE-DIR
   cold coldu AT FS:MAKE-DIR
   S\" VERSIONED-LIBRARY z 1\n" FFI-ZL
   file fileu body bodyu PUT
   file fileu s" 0" FFI-APP
   s" ffi-app.f" s" ffi-hb" cache cacheu BUILD-IN SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   s" " FFI-ZL
   file fileu s" 1" FFI-APP
   -1 DIAG !
   s" ffi-app.f" s" ffi-warm-hb" cache cacheu BUILD-IN SUCCESS
   0 DIAG !
   REUSED
   0 INPUT-U !
   s" /" INPUT CAP INPUT-U BUF-APPEND
   file fileu INPUT CAP INPUT-U BUF-APPEND
   s"  records=" INPUT CAP INPUT-U BUF-APPEND
   ERR$ INPUT INPUT-U @ CONTAINS? TTRUE
   s" ffi-app.f" s" ffi-cold-hb" cold coldu BUILD-IN SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   s" ffi-cold-hb" s" ffi-warm-hb" SAME-FILE ;

: FFI-REUSE-CASE ( -- )
   s" a warm build reusing a file of FFI declarations saves the cold product" T-LABEL
   s" zp.f"
      S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nFUNCTION: ZP-ADLER adler32 ( n ptr u8 n -- n ) ;FUNCTION\nFUNCTION: ZP-CRC crc32 ( n ptr u8 n -- n ) ;FUNCTION\nVERSIONED-LIBRARY m 6\n"
      s" cache-ffp" s" cache-ffp-cold" FFI-REUSE
   s" za.f"
      S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\n' VERSIONED-LIBRARY catch zz x\nconstant ZA-RC\n"
      s" cache-ffa" s" cache-ffa-cold" FFI-REUSE
   s" zb.f"
      S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\n' FUNCTION: catch constant ZB-RC 1 constant ZB-K\n"
      s" cache-ffb" s" cache-ffb-cold" FFI-REUSE ;

: SESSION-CASES ( -- )
   SESSION-CLOSE-CASE FOREIGN-CASE ROOT-REGISTRY-CASE
   SYMBOL-CASE EVICTED-CHILD-CASE
   DEFER-CASE DEFER-PUBLISHED-CASE DEFER-REPLACEMENT-CASE DEFER-ALIAS-CASE
   DEFER-EFFECT-CASE DEFER-RETIRED-CASE
   EMPTY-CHILD-CASE TRAILING-COLON-CASE
   QUALIFIED-GLOBAL-CASE NESTED-SCOPE-CASE
   CHILD-CLOSES-PACKAGE-CASE CHILD-OPENS-PACKAGE-CASE PTR-RESTAMP-CASE
   RETIRED-DATA-CASE ;

public
: MAIN ( -- )
   T-RESET
   s" FILE selection preserves carried values, from include and from included with an argument, and using scope" T-LABEL
   s" native-file-replay" TREE
   s" cache" AT FS:MAKE-DIR
   s" defs.f" S\" package REVIEW-USING public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" child.f" S\" : REVIEW-CHILD ( -- n ) REVIEW-USING:VALUE ;\n" PUT
   s" arg.f" S\" \\ no definitions\n" PUT
   MUTATOR
   s" app.f" S\" require defs.f\nusing REVIEW-USING\n123 include mutate.f\ninclude child.f\nconstant CARRIED\n;using\n456 s\" arg.f\" included constant ARG-CARRIED\n: MAIN ( -- ) REVIEW-CHILD CARRIED + ARG-CARRIED + . ;\n" PUT
   0 EDIT ! BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 620" CONTAINS? TTRUE
   s" child.f" S\" : REVIEW-CHILD ( -- n ) REVIEW-USING:VALUE 0 + ;\n" PUT
   0 EDIT ! BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ s" 620" CONTAINS? TTRUE
   1 EDIT ! BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ s" 621" CONTAINS? TTRUE
   s" hb-build rebuilds an edited #! file and then finds the load up to date" T-LABEL
   s" defs.f" S\" #!/usr/bin/env hb\npackage REVIEW-USING public : VALUE ( -- n ) 101 ; ;package\n" PUT
   s" child.f" S\" : REVIEW-CHILD ( -- n ) REVIEW-USING:VALUE ;\n" PUT
   BUILD-APP SUCCESS
   RUN-APP SUCCESS OUT$ s" 680" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   OUT$ s" hb-build up to date: " CONTAINS? TTRUE
   SCOPE-CASES
   CANON-CASE
   SESSION-CASES
   FFI-REUSE-CASE
   T-REPORT ;

;package
;using

NATIVE-FILE-REPLAY-TEST:MAIN
