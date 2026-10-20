\ Source actions run on every load while individual definitions may be reused.
\ bin/hb --load test/native-file-replay-e2e.f

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
create CANON-EXPECT FS-PATH-CAP allot variable CANON-EXPECT-U
variable EDIT

: BUILD ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   EDIT @ if s" HABU_REVIEW_EDIT" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   builder builderu S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" CAPTURE ;

: REPORT ( -- ) OUT$ s" definition hits " CONTAINS? TTRUE ;
: HITS ( -- )
   REPORT
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE ;

: MUTATOR ( -- )
   0 INPUT-U !
   S\" : REVIEW-MAYBE-EDIT ( -- ) s\" HABU_REVIEW_EDIT\" GETENV nip 0 > if s\" "
      INPUT CAP INPUT-U BUF-APPEND
   s" child.f" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" s\" : REVIEW-CHILD ( -- n ) 42 ;\" WRITE-ALL then ; REVIEW-MAYBE-EDIT\n"
      INPUT CAP INPUT-U BUF-APPEND
   s" mutate.f" INPUT INPUT-U @ PUT ;

variable REMOVED
: REMOVE-CONTAINER ( ptr u8 n -- )
   2dup s" .hbd" ENDS-WITH? if
      REMOVE-FILE 1 REMOVED +! exit
   then 2drop ;

: EVICT-CACHE ( ptr u8 n -- )
   0 REMOVED !
   AT ['] REMOVE-CONTAINER WALK-FILES
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
   S\" \" s\" repl\" NFILE-CACHE:LOAD\n" INPUT CAP INPUT-U BUF-APPEND
   tail tailu INPUT CAP INPUT-U BUF-APPEND
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
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
   s" cache-using" AT MAKE-DIR
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
   s" cache-canon" AT MAKE-DIR
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

: SESSION-CASES-START ( -- )
   s" FILE replay closes a nested session before the next range" T-LABEL
   s" cache-close" AT MAKE-DIR
   s" session-close.f" S\" IR-CTX:SESSION-CLOSE\n" PUT
   s" session-parent.f"
      S\" : REVIEW-BEFORE ( -- ) ;\ninclude session-close.f\n1 constant REVIEW-SESSION-VALUE\n" PUT
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" session-parent.f" s" cache-close"
      S\" s\" session=\" type NCOMP:SESSION-LIVE? . cr\ns\" open-result=\" type ' REVIEW-OPEN catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=0" CONTAINS? TTRUE
   OUT$ s" open-result=0" CONTAINS? TTRUE
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" session-parent.f" s" cache-close"
      S\" s\" session=\" type NCOMP:SESSION-LIVE? . cr\ns\" open-result=\" type ' REVIEW-OPEN catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=0" CONTAINS? TTRUE
   OUT$ s" open-result=0" CONTAINS? TTRUE

   s" FILE replay never revives a retired child context" T-LABEL
   s" cache-revive" AT MAKE-DIR
   s" revive-child.f" S\" REVIEW-ID:SAVE\n: REVIEW-AFTER ( -- ) ;\n" PUT
   s" revive-parent.f"
      S\" package REVIEW-ID\npublic\n1 TYPED-BUFFER SAVED IR-CTX:ctx\n: SAVE ( -- ) NABI:BINDING [: 0 SAVED ! ;] IR-CTX:WITH-CONTEXT ;\n: LIVE ( -- bool ) 0 SAVED @ IR-CTX:LIVE? ;\n;package\nIR-CTX:SESSION-CLOSE\ninclude revive-child.f\n" PUT
   s" " s" revive-parent.f" s" cache-revive"
      S\" s\" saved-live=\" type REVIEW-ID:LIVE . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" saved-live=0" CONTAINS? TTRUE
   s" " s" revive-parent.f" s" cache-revive"
      S\" s\" saved-live=\" type REVIEW-ID:LIVE . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" saved-live=0" CONTAINS? TTRUE

   s" FILE replay does not revive a retired arena at a new session offset" T-LABEL
   s" cache-arena" AT MAKE-DIR
   s" arena-child.f" S\" REVIEW-ARENA:SAVE\n: REVIEW-AFTER ( -- ) ;\n" PUT
   s" arena-parent.f"
      S\" package REVIEW-ARENA\npublic\n1 TYPED-BUFFER SAVED IR-ARENA:arena\n: SAVE ( -- ) NABI:BINDING [: 16 IR-ARENA:NEW 0 SAVED ! ;] IR-CTX:WITH-CONTEXT ;\n: LIVE ( -- bool ) 0 SAVED @ IR-ARENA:LIVE? ;\n;package\nIR-CTX:SESSION-CLOSE\ninclude arena-child.f\n" PUT
   s" " s" arena-parent.f" s" cache-arena"
      S\" s\" saved-live=\" type REVIEW-ARENA:LIVE . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" saved-live=0" CONTAINS? TTRUE
   s" " s" arena-parent.f" s" cache-arena"
      S\" s\" saved-live=\" type REVIEW-ARENA:LIVE . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" saved-live=0" CONTAINS? TTRUE

;

: PROTOTYPE-CASE ( -- )
   s" FILE replay keeps a provider's session module distinct from saved IDs" T-LABEL
   s" cache-prototype" AT MAKE-DIR
   s" prototype-child.f"
      S\" IR-ID:NEW-MODULE 0 REVIEW-PROTOTYPE:SAVED ! drop\n: REVIEW-AFTER ( -- ) ;\n" PUT
   s" prototype-parent.f"
      S\" package REVIEW-PROTOTYPE\npublic\n1 TYPED-BUFFER SAVED IR-ID:ir-module-id\n;package\nIR-CTX:SESSION-CLOSE\ninclude prototype-child.f\n" PUT
   S\" package REVIEW-BACK\npublic\n1 TYPED-BUFFER MODULE IR-ID:ir-module-id\nprivate\n: NO ( CTARGET:contract -- bool ) drop false ;\n: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;\n: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;\n: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) 2drop ;\n: NO-STAGE ( -- ) ;\n: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ) 0 IR-ID:PACK-SYMBOL IR-ID:SYMBOL-OWNER 0 MODULE ! 2drop drop ;\n: DECLARE ( n n NBACK:linkage -- ) 2drop drop ;\n: FAKE-PASS ( -- NBACK:pass ) [: DECLARE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;] [: PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE ;\npublic\n: INSTALL ( -- ) 30 CTARGET:ID CTARGET-ARCH:PTX [: NO ;] [: NO ;] CTARGET-BACKEND:MAKE FAKE-PASS NBACK:REGISTER ;\n;package\nREVIEW-BACK:INSTALL\n"
      s" prototype-parent.f" s" cache-prototype"
      S\" s\" module=\" type 0 REVIEW-PROTOTYPE:SAVED @ 0 REVIEW-BACK:MODULE @ IR-ID:MODULE-SAME? . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" module=0" CONTAINS? TTRUE
   S\" package REVIEW-BACK\npublic\n1 TYPED-BUFFER MODULE IR-ID:ir-module-id\nprivate\n: NO ( CTARGET:contract -- bool ) drop false ;\n: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;\n: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;\n: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) 2drop ;\n: NO-STAGE ( -- ) ;\n: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ) 0 IR-ID:PACK-SYMBOL IR-ID:SYMBOL-OWNER 0 MODULE ! 2drop drop ;\n: DECLARE ( n n NBACK:linkage -- ) 2drop drop ;\n: FAKE-PASS ( -- NBACK:pass ) [: DECLARE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;] [: PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE ;\npublic\n: INSTALL ( -- ) 30 CTARGET:ID CTARGET-ARCH:PTX [: NO ;] [: NO ;] CTARGET-BACKEND:MAKE FAKE-PASS NBACK:REGISTER ;\n;package\nREVIEW-BACK:INSTALL\n"
      s" prototype-parent.f" s" cache-prototype"
      S\" s\" module=\" type 0 REVIEW-PROTOTYPE:SAVED @ 0 REVIEW-BACK:MODULE @ IR-ID:MODULE-SAME? . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" module=0" CONTAINS? TTRUE

;

: SESSION-CASES-END ( -- )
   s" FILE replay keeps retired compiler leases stale" T-LABEL
   s" cache-lease" AT MAKE-DIR
   s" lease.f"
      S\" package REVIEW-LEASE\npublic\n1 TYPED-BUFFER SAVED NLEASE:lease\n: SAVE ( -- ) [: 0 SAVED ! ;] NLEASE:WITH ;\n: PROBE ( -- ) 32 0 ?do [: drop 0 SAVED @ NLEASE:LIVE? if 73 throw then ;] NLEASE:WITH loop ;\n;package\nREVIEW-LEASE:SAVE\n" PUT
   s" " s" lease.f" s" cache-lease"
      S\" s\" lease-probe=\" type ' REVIEW-LEASE:PROBE catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" lease-probe=0" CONTAINS? TTRUE
   s" " s" lease.f" s" cache-lease"
      S\" s\" lease-probe=\" type ' REVIEW-LEASE:PROBE catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" lease-probe=0" CONTAINS? TTRUE

   s" FILE source opening a foreign IR session remains a source load" T-LABEL
   s" cache-foreign" AT MAKE-DIR
   s" foreign.f" S\" REVIEW-OPEN\n" PUT
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" foreign.f" s" cache-foreign"
      S\" s\" session=\" type IR-CTX:SESSION-LIVE? . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=-1" CONTAINS? TTRUE
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" foreign.f" s" cache-foreign"
      S\" s\" session=\" type IR-CTX:SESSION-LIVE? . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" session=-1" CONTAINS? TTRUE

   s" FILE replay rejects unrelated module serials before import" T-LABEL
   s" cache-module" AT MAKE-DIR
   s" module.f"
      S\" 1 TYPED-BUFFER REVIEW-MODULE IR-ID:ir-module-id\nIR-ID:NEW-MODULE 0 REVIEW-MODULE ! drop\n" PUT
   S\" 1 TYPED-BUFFER REVIEW-OUTSIDE IR-ID:ir-module-id\n"
      s" module.f" s" cache-module"
      S\" s\" module-collision=\" type 0 REVIEW-MODULE @ 0 REVIEW-OUTSIDE @ IR-ID:MODULE-SAME? . cr\n"
      s" " SESSION-PROBE-WITH
   REPORT
   OUT$ s" module-collision=0" CONTAINS? TTRUE
   S\" 1 TYPED-BUFFER REVIEW-OUTSIDE IR-ID:ir-module-id\n"
      s" module.f" s" cache-module"
      S\" s\" module-collision=\" type 0 REVIEW-MODULE @ 0 REVIEW-OUTSIDE @ IR-ID:MODULE-SAME? . cr\n"
      S\" IR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\n"
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" module-collision=0" CONTAINS? TTRUE

   s" FILE selection falls back after an unrelated retired context" T-LABEL
   s" cache-guard" AT MAKE-DIR
   s" guard.f" S\" : REVIEW-GUARD ( -- n ) 7 ;\n" PUT
   S\" : REVIEW-UNRELATED ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" guard.f" s" cache-guard"
      S\" s\" guard=\" type REVIEW-GUARD . cr\n" s" "
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" guard=7" CONTAINS? TTRUE
   S\" : REVIEW-UNRELATED ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" guard.f" s" cache-guard"
      S\" s\" guard=\" type REVIEW-GUARD . cr\n"
      S\" REVIEW-UNRELATED\n"
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" guard=7" CONTAINS? TTRUE ;

: HISTORY-CASES ( -- )
   s" FILE replay rebases an unrelated lease before a unit" T-LABEL
   s" cache-lease-history" AT MAKE-DIR
   s" lease-history.f" S\" REVIEW-LEASE-BURN\n" PUT
   S\" variable REVIEW-BEFORE\n: REVIEW-LEASE-BURN ( -- ) [: drop ;] NLEASE:WITH ;\n"
      s" lease-history.f" s" cache-lease-history"
      S\" s\" lease-delta=\" type NLEASE:HISTORY REVIEW-BEFORE @ - . cr\n"
      S\" NLEASE:HISTORY REVIEW-BEFORE !\n" SESSION-PROBE-WITH
   REPORT
   OUT$ s" lease-delta=1" CONTAINS? TTRUE
   S\" variable REVIEW-BEFORE\n: REVIEW-LEASE-BURN ( -- ) [: drop ;] NLEASE:WITH ;\n"
      s" lease-history.f" s" cache-lease-history"
      S\" s\" lease-delta=\" type NLEASE:HISTORY REVIEW-BEFORE @ - . cr\n"
      S\" REVIEW-LEASE-BURN NLEASE:HISTORY REVIEW-BEFORE !\n"
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" lease-delta=1" CONTAINS? TTRUE

   s" FILE replay rebases an unrelated context before a unit" T-LABEL
   s" cache-context-history" AT MAKE-DIR
   s" context-history.f" S\" REVIEW-CONTEXT-BURN\n" PUT
   S\" variable REVIEW-BEFORE-GEN\nvariable REVIEW-BEFORE-EPOCH\n: REVIEW-CONTEXT-BURN ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" context-history.f" s" cache-context-history"
      S\" s\" gen-delta=\" type IR-CTX:HISTORY drop REVIEW-BEFORE-GEN @ - . cr\ns\" epoch-delta=\" type IR-CTX:EPOCH REVIEW-BEFORE-EPOCH @ - . cr\n"
      S\" IR-CTX:HISTORY REVIEW-BEFORE-EPOCH ! REVIEW-BEFORE-GEN !\n"
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" gen-delta=1" CONTAINS? TTRUE
   OUT$ s" epoch-delta=1" CONTAINS? TTRUE
   S\" variable REVIEW-BEFORE-GEN\nvariable REVIEW-BEFORE-EPOCH\n: REVIEW-CONTEXT-BURN ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" context-history.f" s" cache-context-history"
      S\" s\" gen-delta=\" type IR-CTX:HISTORY drop REVIEW-BEFORE-GEN @ - . cr\ns\" epoch-delta=\" type IR-CTX:EPOCH REVIEW-BEFORE-EPOCH @ - . cr\n"
      S\" REVIEW-CONTEXT-BURN IR-CTX:HISTORY REVIEW-BEFORE-EPOCH ! REVIEW-BEFORE-GEN !\n"
      SESSION-PROBE-WITH
   REPORT
   OUT$ s" gen-delta=1" CONTAINS? TTRUE
   OUT$ s" epoch-delta=1" CONTAINS? TTRUE ;

: ROOT-REGISTRY-CASE ( -- )
   s" an app FILE does not reuse an unrelated live compiler session" T-LABEL
   s" cache-root-registry" AT MAKE-DIR
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

: BUILDER-CASE ( -- )
   s" FILE replay never revives a retired builder" T-LABEL
   s" cache-builder" AT MAKE-DIR
   s" builder.f"
      S\" package REVIEW-BUILDER\npublic\n1 TYPED-BUFFER SAVED IR-BUILD:builder\n: NEW ( IR-CTX:ctx -- IR-BUILD:builder ) IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT HIR:NEW-BUILDER ;\n: SAVE ( -- ) NABI:BINDING [: NEW dup 0 SAVED ! IR-BUILD:ABORT ;] IR-CTX:WITH-CONTEXT ;\n: PROBE ( -- ) 128 0 ?do NABI:BINDING [: NEW 0 SAVED @ IR-BUILD:LIVE? if 73 throw then IR-BUILD:ABORT ;] IR-CTX:WITH-CONTEXT loop ;\n;package\nREVIEW-BUILDER:SAVE\n" PUT
   s" " s" builder.f" s" cache-builder"
      S\" s\" builder-probe=\" type ' REVIEW-BUILDER:PROBE catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" builder-probe=0" CONTAINS? TTRUE
   s" " s" builder.f" s" cache-builder"
      S\" s\" builder-probe=\" type ' REVIEW-BUILDER:PROBE catch . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" builder-probe=0" CONTAINS? TTRUE ;

: SYMBOL-CASE ( -- )
   s" FILE replay reuses a symbol interned then undefined before the unit" T-LABEL
   s" cache-symbol" AT MAKE-DIR
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
   HITS
   OUT$ s" symbol-source-ran" CONTAINS? TTRUE
   OUT$ s" number=5" CONTAINS? TTRUE
   OUT$ s" keep=7" CONTAINS? TTRUE ;

: PREPARED-CODE-CASE ( -- )
   s" prepared FILE code and origin rows remain runnable after a warm import" T-LABEL
   s" cache-prepared-code" AT MAKE-DIR
   s" prepared-code.f"
      S\" package REVIEW-PREPARED public\n: ANSWER ( -- n ) 91 ;\n;package\n" PUT
   s" " s" prepared-code.f" s" cache-prepared-code"
      S\" s\" answer=\" type REVIEW-PREPARED:ANSWER . cr\n"
      SESSION-PROBE
   REPORT
   OUT$ s" answer=91" CONTAINS? TTRUE
   s" " s" prepared-code.f" s" cache-prepared-code"
      S\" s\" answer=\" type REVIEW-PREPARED:ANSWER . cr\n"
      SESSION-PROBE
   HITS
   OUT$ s" answer=91" CONTAINS? TTRUE ;

: DEFER-SOURCE$ ( -- ptr u8 n )
   S\" : INSTALL ( -- ) ['] TWO is SLOT ;\n" ;

: DEFER-TAIL$ ( -- ptr u8 n )
   S\" INSTALL\ns\" private=\" type 40 SLOT . cr\n;package\ns\" global=\" type 40 SLOT . cr\n" ;

: DEFER-PREFIX$ ( -- ptr u8 n )
   S\" defer SLOT ( n -- n )\n: GLOBAL-IMPL ( n -- n ) 100 + ;\n: SET-GLOBAL ( -- ) ['] GLOBAL-IMPL is SLOT ;\nSET-GLOBAL\npackage REVIEW-DEFER\nprivate\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\n" ;

: DEFER-MOVED-PREFIX$ ( -- ptr u8 n )
   S\" defer SLOT ( n -- n )\n: GLOBAL-IMPL ( n -- n ) 100 + ;\n: SET-GLOBAL ( -- ) ['] GLOBAL-IMPL is SLOT ;\nSET-GLOBAL\ncreate REVIEW-PAD 256 allot\npackage REVIEW-DEFER\nprivate\ndefer SLOT ( n -- n )\n: TWO ( n -- n ) 2 + ;\n" ;

: DEFER-CASE ( -- )
   s" FILE replay relocates a private defer cell from its exact declaration" T-LABEL
   s" cache-defer" AT MAKE-DIR
   s" defer.f" DEFER-SOURCE$ PUT
   DEFER-PREFIX$ s" defer.f" s" cache-defer" DEFER-TAIL$ SESSION-PROBE
   REPORT
   OUT$ s" private=42" CONTAINS? TTRUE
   OUT$ s" global=140" CONTAINS? TTRUE
   DEFER-MOVED-PREFIX$ s" defer.f" s" cache-defer" DEFER-TAIL$ SESSION-PROBE
   HITS
   OUT$ S\" definition hits 1\n" CONTAINS? TTRUE
   OUT$ s" private=42" CONTAINS? TTRUE
   OUT$ s" global=140" CONTAINS? TTRUE ;

: DEFER-PUBLISHED-CASE ( -- )
   s" a FILE source defer publishes its contract before caching its users" T-LABEL
   s" cache-defer-published" AT MAKE-DIR
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
   HITS
   OUT$ S\" definition hits 3\n" CONTAINS? TTRUE
   OUT$ s" contract=-1" CONTAINS? TTRUE
   OUT$ s" published=42" CONTAINS? TTRUE ;

: DEFER-REPLACEMENT-CASE ( -- )
   s" FILE replay refuses a same-key colon or variable replacing a defer" T-LABEL
   s" cache-defer-replace" AT MAKE-DIR
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
   s" cache-defer-alias" AT MAKE-DIR
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
   HITS
   OUT$ s" alias=42" CONTAINS? TTRUE ;

: DEFER-EFFECT-CASE ( -- )
   s" a changed defer effect rejects its incompatible cached installer" T-LABEL
   s" cache-defer-effect" AT MAKE-DIR
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
   s" cache-defer-retired" AT MAKE-DIR
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
   HITS
   OUT$ S\" definition hits 1\n" CONTAINS? TTRUE
   OUT$ s" old=42" CONTAINS? TTRUE
   OUT$ s" new=140" CONTAINS? TTRUE ;

: EVICTED-CHILD-CASE ( -- )
   s" evicted definition containers compile from source" T-LABEL
   s" cache-changed-child" AT MAKE-DIR
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
   s" cache-empty-child" AT MAKE-DIR
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
   s" cache-trailing-colon" AT MAKE-DIR
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
   s" cache-nested-scope" AT MAKE-DIR
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
   s" cache-qualified-global" AT MAKE-DIR
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
   s" cache-child-closes" AT MAKE-DIR
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

: CHILD-OPENS-PACKAGE-CASE ( -- )
   s" a child opening a package leaves later sibling dependencies live" T-LABEL
   s" cache-child-opens" AT MAKE-DIR
   s" opens-child.f"
      S\" package REVIEW-OPEN public\n: VALUE ( -- n ) 40 ;\n" PUT
   s" opens-sibling.f"
      S\" : REVIEW-SIBLING ( -- n ) REVIEW-OPEN:VALUE 2 + ;\n" PUT
   s" opens-independent.f"
      S\" : REVIEW-INDEPENDENT ( -- n ) 5 ;\n" PUT
   s" opens-parent.f"
      S\" include opens-child.f\n;package\ninclude opens-sibling.f\ninclude opens-independent.f\n: REVIEW-OPEN-RESULT ( -- n ) REVIEW-SIBLING REVIEW-INDEPENDENT + ;\n" PUT
   s" " s" opens-parent.f" s" cache-child-opens"
      S\" s\" opens=\" type REVIEW-OPEN-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" opens=47" CONTAINS? TTRUE
   s" " s" opens-parent.f" s" cache-child-opens"
      S\" s\" opens=\" type REVIEW-OPEN-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" opens=47" CONTAINS? TTRUE
   s" opens-child.f"
      S\" package REVIEW-OPEN public\n: VALUE ( -- n ) 41 ;\n" PUT
   s" " s" opens-parent.f" s" cache-child-opens"
      S\" s\" opens=\" type REVIEW-OPEN-RESULT . cr\n" SESSION-PROBE
   REPORT
   OUT$ s" opens=48" CONTAINS? TTRUE ;

: SESSION-CASES ( -- )
   SESSION-CASES-START PROTOTYPE-CASE SESSION-CASES-END HISTORY-CASES
   ROOT-REGISTRY-CASE
   BUILDER-CASE SYMBOL-CASE PREPARED-CODE-CASE EVICTED-CHILD-CASE
   DEFER-CASE DEFER-PUBLISHED-CASE DEFER-REPLACEMENT-CASE DEFER-ALIAS-CASE
   DEFER-EFFECT-CASE DEFER-RETIRED-CASE
   EMPTY-CHILD-CASE TRAILING-COLON-CASE
   QUALIFIED-GLOBAL-CASE NESTED-SCOPE-CASE
   CHILD-CLOSES-PACKAGE-CASE CHILD-OPENS-PACKAGE-CASE ;

public
: MAIN ( -- )
   T-RESET
   s" FILE selection preserves carried values and using scope" T-LABEL
   s" native-file-replay" TREE
   s" cache" AT MAKE-DIR
   s" defs.f" S\" package REVIEW-USING public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" child.f" S\" : REVIEW-CHILD ( -- n ) REVIEW-USING:VALUE ;\n" PUT
   MUTATOR
   s" app.f" S\" require defs.f\nusing REVIEW-USING\n123 include mutate.f\ninclude child.f\nconstant CARRIED\n;using\n: MAIN ( -- ) REVIEW-CHILD CARRIED + . ;\n" PUT
   0 EDIT ! BUILD SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 164" CONTAINS? TTRUE
   s" child.f" S\" : REVIEW-CHILD ( -- n ) REVIEW-USING:VALUE 0 + ;\n" PUT
   0 EDIT ! BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ s" 164" CONTAINS? TTRUE
   1 EDIT ! BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ s" 165" CONTAINS? TTRUE
   SCOPE-CASES
   CANON-CASE
   SESSION-CASES
   T-REPORT
   s" FILE replay artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-REPLAY-TEST:MAIN
