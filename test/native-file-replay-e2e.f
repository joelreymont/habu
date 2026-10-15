\ FILE selection checks children before importing the parent's first range.
\ bin/hb --load test/native-file-replay-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-REPLAY-TEST

$8000 constant CAP
create TARGET FS-PATH-CAP allot variable TARGET-U
create ARTIFACT FS-PATH-CAP allot variable ARTIFACT-U
create HEADER FS-PATH-CAP allot
create INPUT CAP allot variable INPUT-U
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

: MUTATOR ( -- )
   0 INPUT-U !
   S\" : REVIEW-MAYBE-EDIT ( -- ) s\" HABU_REVIEW_EDIT\" GETENV nip 0 > if s\" "
      INPUT CAP INPUT-U BUF-APPEND
   s" child.f" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" s\" : REVIEW-CHILD ( -- n ) 42 ;\" WRITE-ALL then ; REVIEW-MAYBE-EDIT\n"
      INPUT CAP INPUT-U BUF-APPEND
   s" mutate.f" INPUT INPUT-U @ PUT ;

: ARTIFACT? ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" .hfo" ENDS-WITH? 0= if exit then
   a u FS-PATHZ open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd HEADER FS-PATH-CAP read {: got:n :}
   fd close
   got 128 < if exit then
   HEADER 16 + LE:U64@ {: pathu:n :}
   pathu 0 <= pathu got 128 - > or if exit then
   HEADER 128 + pathu TARGET TARGET-U @ ENDS-WITH? if
      a ARTIFACT u BYTE-COPY u ARTIFACT-U !
   then ;

: REMOVE-UNIT ( ptr u8 n -- )
   {: name:ptr u:n :}
   name TARGET u BYTE-COPY u TARGET-U !
   0 ARTIFACT-U !
   s" cache" AT [: ARTIFACT? ;] WALK-FILES
   ARTIFACT-U @ 0 > TTRUE
   ARTIFACT ARTIFACT-U @ REMOVE-FILE ;

\ A replayed child may close the compiler session opened by an earlier parent
\ range. Probe in separate processes so a warm cache hit cannot inherit the
\ cold process's live context or its generation counters.
: SESSION-PROBE-WITH ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
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
   builder builderu INPUT INPUT-U @ CAPTURE SUCCESS ;

: SESSION-PROBE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   s" " SESSION-PROBE-WITH ;

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
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   OUT$ s" session=0" CONTAINS? TTRUE
   OUT$ s" open-result=0" CONTAINS? TTRUE
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" session-parent.f" s" cache-close"
      S\" s\" session=\" type NCOMP:SESSION-LIVE? . cr\ns\" open-result=\" type ' REVIEW-OPEN catch . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 2\n compiled 0" CONTAINS? TTRUE
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
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   OUT$ s" saved-live=0" CONTAINS? TTRUE
   s" " s" revive-parent.f" s" cache-revive"
      S\" s\" saved-live=\" type REVIEW-ID:LIVE . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 2\n compiled 0" CONTAINS? TTRUE
   OUT$ s" saved-live=0" CONTAINS? TTRUE

   s" FILE replay does not revive a retired arena at a new session offset" T-LABEL
   s" cache-arena" AT MAKE-DIR
   s" arena-child.f" S\" REVIEW-ARENA:SAVE\n: REVIEW-AFTER ( -- ) ;\n" PUT
   s" arena-parent.f"
      S\" package REVIEW-ARENA\npublic\n1 TYPED-BUFFER SAVED IR-ARENA:arena\n: SAVE ( -- ) NABI:BINDING [: 16 IR-ARENA:NEW 0 SAVED ! ;] IR-CTX:WITH-CONTEXT ;\n: LIVE ( -- bool ) 0 SAVED @ IR-ARENA:LIVE? ;\n;package\nIR-CTX:SESSION-CLOSE\ninclude arena-child.f\n" PUT
   s" " s" arena-parent.f" s" cache-arena"
      S\" s\" saved-live=\" type REVIEW-ARENA:LIVE . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   OUT$ s" saved-live=0" CONTAINS? TTRUE
   s" " s" arena-parent.f" s" cache-arena"
      S\" s\" saved-live=\" type REVIEW-ARENA:LIVE . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 2\n compiled 0" CONTAINS? TTRUE
   OUT$ s" saved-live=0" CONTAINS? TTRUE

;

: PROTOTYPE-CASE ( -- )
   s" FILE replay keeps a provider's session module distinct from saved IDs" T-LABEL
   s" cache-prototype" AT MAKE-DIR
   s" prototype-child.f"
      S\" IR-ID:NEW-MODULE 0 REVIEW-PROTOTYPE:SAVED ! drop\n: REVIEW-AFTER ( -- ) ;\n" PUT
   s" prototype-parent.f"
      S\" package REVIEW-PROTOTYPE\npublic\n1 TYPED-BUFFER SAVED IR-ID:ir-module-id\n;package\nIR-CTX:SESSION-CLOSE\ninclude prototype-child.f\n" PUT
   S\" package REVIEW-BACK\npublic\n1 TYPED-BUFFER MODULE IR-ID:ir-module-id\nprivate\n: NO ( CTARGET:contract -- bool ) drop false ;\n: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;\n: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;\n: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) 2drop ;\n: NO-STAGE ( -- ) ;\n: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ) 0 IR-ID:PACK-SYMBOL IR-ID:SYMBOL-OWNER 0 MODULE ! 2drop drop ;\n: DECLARE ( n n NBACK:linkage -- ) 2drop drop ;\n: FAKE-PASS ( -- NBACK:pass ) [: DECLARE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;] [: PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE ;\npublic\n: INSTALL ( -- ) 30 CTARGET:ID CTARGET-ARCH:PTX [: NO ;] [: NO ;] CTARGET-BACKEND:MAKE FAKE-PASS NBACK:REGISTER ;\n;package\nREVIEW-BACK:INSTALL\n"
      s" prototype-parent.f" s" cache-prototype"
      S\" s\" module=\" type 0 REVIEW-PROTOTYPE:SAVED @ 0 REVIEW-BACK:MODULE @ IR-ID:MODULE-SAME? . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   OUT$ s" module=0" CONTAINS? TTRUE
   S\" package REVIEW-BACK\npublic\n1 TYPED-BUFFER MODULE IR-ID:ir-module-id\nprivate\n: NO ( CTARGET:contract -- bool ) drop false ;\n: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;\n: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;\n: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) 2drop ;\n: NO-STAGE ( -- ) ;\n: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ) 0 IR-ID:PACK-SYMBOL IR-ID:SYMBOL-OWNER 0 MODULE ! 2drop drop ;\n: DECLARE ( n n NBACK:linkage -- ) 2drop drop ;\n: FAKE-PASS ( -- NBACK:pass ) [: DECLARE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;] [: PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE ;\npublic\n: INSTALL ( -- ) 30 CTARGET:ID CTARGET-ARCH:PTX [: NO ;] [: NO ;] CTARGET-BACKEND:MAKE FAKE-PASS NBACK:REGISTER ;\n;package\nREVIEW-BACK:INSTALL\n"
      s" prototype-parent.f" s" cache-prototype"
      S\" s\" module=\" type 0 REVIEW-PROTOTYPE:SAVED @ 0 REVIEW-BACK:MODULE @ IR-ID:MODULE-SAME? . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 2\n compiled 0" CONTAINS? TTRUE
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
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" lease-probe=0" CONTAINS? TTRUE
   s" " s" lease.f" s" cache-lease"
      S\" s\" lease-probe=\" type ' REVIEW-LEASE:PROBE catch . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 1\n compiled 0" CONTAINS? TTRUE
   OUT$ s" lease-probe=0" CONTAINS? TTRUE

   s" FILE source opening a foreign IR session remains a source load" T-LABEL
   s" cache-foreign" AT MAKE-DIR
   s" foreign.f" S\" REVIEW-OPEN\n" PUT
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" foreign.f" s" cache-foreign"
      S\" s\" session=\" type IR-CTX:SESSION-LIVE? . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" session=-1" CONTAINS? TTRUE
   S\" : REVIEW-OPEN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n"
      s" foreign.f" s" cache-foreign"
      S\" s\" session=\" type IR-CTX:SESSION-LIVE? . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" session=-1" CONTAINS? TTRUE

   s" FILE replay rejects unrelated module serials before import" T-LABEL
   s" cache-module" AT MAKE-DIR
   s" module.f"
      S\" 1 TYPED-BUFFER REVIEW-MODULE IR-ID:ir-module-id\nIR-ID:NEW-MODULE 0 REVIEW-MODULE ! drop\n" PUT
   S\" 1 TYPED-BUFFER REVIEW-OUTSIDE IR-ID:ir-module-id\n"
      s" module.f" s" cache-module"
      S\" s\" module-collision=\" type 0 REVIEW-MODULE @ 0 REVIEW-OUTSIDE @ IR-ID:MODULE-SAME? . cr\n"
      s" " SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" module-collision=0" CONTAINS? TTRUE
   S\" 1 TYPED-BUFFER REVIEW-OUTSIDE IR-ID:ir-module-id\n"
      s" module.f" s" cache-module"
      S\" s\" module-collision=\" type 0 REVIEW-MODULE @ 0 REVIEW-OUTSIDE @ IR-ID:MODULE-SAME? . cr\n"
      S\" IR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\nIR-ID:NEW-MODULE 0 REVIEW-OUTSIDE ! drop\n"
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" module-collision=0" CONTAINS? TTRUE

   s" FILE selection falls back after an unrelated retired context" T-LABEL
   s" cache-guard" AT MAKE-DIR
   s" guard.f" S\" : REVIEW-GUARD ( -- n ) 7 ;\n" PUT
   S\" : REVIEW-UNRELATED ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" guard.f" s" cache-guard"
      S\" s\" guard=\" type REVIEW-GUARD . cr\n" s" "
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" guard=7" CONTAINS? TTRUE
   S\" : REVIEW-UNRELATED ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" guard.f" s" cache-guard"
      S\" s\" guard=\" type REVIEW-GUARD . cr\n"
      S\" REVIEW-UNRELATED\n"
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" guard=7" CONTAINS? TTRUE ;

: HISTORY-CASES ( -- )
   s" FILE replay falls back when an unrelated lease shifts future serials" T-LABEL
   s" cache-lease-history" AT MAKE-DIR
   s" lease-history.f" S\" REVIEW-LEASE-BURN\n" PUT
   S\" variable REVIEW-BEFORE\n: REVIEW-LEASE-BURN ( -- ) [: drop ;] NLEASE:WITH ;\n"
      s" lease-history.f" s" cache-lease-history"
      S\" s\" lease-delta=\" type NLEASE:HISTORY REVIEW-BEFORE @ - . cr\n"
      S\" NLEASE:HISTORY REVIEW-BEFORE !\n" SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" lease-delta=1" CONTAINS? TTRUE
   S\" variable REVIEW-BEFORE\n: REVIEW-LEASE-BURN ( -- ) [: drop ;] NLEASE:WITH ;\n"
      s" lease-history.f" s" cache-lease-history"
      S\" s\" lease-delta=\" type NLEASE:HISTORY REVIEW-BEFORE @ - . cr\n"
      S\" REVIEW-LEASE-BURN NLEASE:HISTORY REVIEW-BEFORE !\n"
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" lease-delta=1" CONTAINS? TTRUE

   s" FILE replay falls back when a context shifts future generations" T-LABEL
   s" cache-context-history" AT MAKE-DIR
   s" context-history.f" S\" REVIEW-CONTEXT-BURN\n" PUT
   S\" variable REVIEW-BEFORE-GEN\nvariable REVIEW-BEFORE-EPOCH\n: REVIEW-CONTEXT-BURN ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" context-history.f" s" cache-context-history"
      S\" s\" gen-delta=\" type IR-CTX:HISTORY drop REVIEW-BEFORE-GEN @ - . cr\ns\" epoch-delta=\" type IR-CTX:EPOCH REVIEW-BEFORE-EPOCH @ - . cr\n"
      S\" IR-CTX:HISTORY REVIEW-BEFORE-EPOCH ! REVIEW-BEFORE-GEN !\n"
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" gen-delta=1" CONTAINS? TTRUE
   OUT$ s" epoch-delta=1" CONTAINS? TTRUE
   S\" variable REVIEW-BEFORE-GEN\nvariable REVIEW-BEFORE-EPOCH\n: REVIEW-CONTEXT-BURN ( -- ) NABI:BINDING [: drop ;] IR-CTX:WITH-CONTEXT ;\n"
      s" context-history.f" s" cache-context-history"
      S\" s\" gen-delta=\" type IR-CTX:HISTORY drop REVIEW-BEFORE-GEN @ - . cr\ns\" epoch-delta=\" type IR-CTX:EPOCH REVIEW-BEFORE-EPOCH @ - . cr\n"
      S\" REVIEW-CONTEXT-BURN IR-CTX:HISTORY REVIEW-BEFORE-EPOCH ! REVIEW-BEFORE-GEN !\n"
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" gen-delta=1" CONTAINS? TTRUE
   OUT$ s" epoch-delta=1" CONTAINS? TTRUE ;

: BUILDER-CASE ( -- )
   s" FILE replay never revives a retired builder" T-LABEL
   s" cache-builder" AT MAKE-DIR
   s" builder.f"
      S\" package REVIEW-BUILDER\npublic\n1 TYPED-BUFFER SAVED IR-BUILD:builder\n: NEW ( IR-CTX:ctx -- IR-BUILD:builder ) IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT HIR:NEW-BUILDER ;\n: SAVE ( -- ) NABI:BINDING [: NEW dup 0 SAVED ! IR-BUILD:ABORT ;] IR-CTX:WITH-CONTEXT ;\n: PROBE ( -- ) 128 0 ?do NABI:BINDING [: NEW 0 SAVED @ IR-BUILD:LIVE? if 73 throw then IR-BUILD:ABORT ;] IR-CTX:WITH-CONTEXT loop ;\n;package\nREVIEW-BUILDER:SAVE\n" PUT
   s" " s" builder.f" s" cache-builder"
      S\" s\" builder-probe=\" type ' REVIEW-BUILDER:PROBE catch . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" builder-probe=0" CONTAINS? TTRUE
   s" " s" builder.f" s" cache-builder"
      S\" s\" builder-probe=\" type ' REVIEW-BUILDER:PROBE catch . cr\n"
      SESSION-PROBE
   OUT$ S\" file unit hits 1\n compiled 0" CONTAINS? TTRUE
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
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   OUT$ s" symbol-source-ran" CONTAINS? TTRUE
   OUT$ s" number=5" CONTAINS? TTRUE
   OUT$ s" keep=7" CONTAINS? TTRUE
   S\" package REVIEW-SYMBOL public\n: KEEP ( -- n ) 7 ;\n: NUMBER ( -- n ) 3 ;\nundefine NUMBER\n;package\n"
      s" symbol.f" s" cache-symbol"
      S\" s\" number=\" type REVIEW-SYMBOL:NUMBER . cr\ns\" keep=\" type REVIEW-SYMBOL:KEEP . cr\n"
      s" "
      SESSION-PROBE-WITH
   OUT$ S\" file unit hits 1\n compiled 0" CONTAINS? TTRUE
   OUT$ s" symbol-source-ran" CONTAINS? 0= TTRUE
   OUT$ s" number=5" CONTAINS? TTRUE
   OUT$ s" keep=7" CONTAINS? TTRUE ;

: SESSION-CASES ( -- )
   SESSION-CASES-START PROTOTYPE-CASE SESSION-CASES-END HISTORY-CASES
   BUILDER-CASE SYMBOL-CASE ;

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
   OUT$ S\" file unit hits 0\n compiled 4" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 164" CONTAINS? TTRUE
   s" child.f" REMOVE-UNIT
   0 EDIT ! BUILD SUCCESS
   OUT$ S\" file unit hits 2\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 164" CONTAINS? TTRUE
   s" mutate.f" REMOVE-UNIT
   1 EDIT ! BUILD SUCCESS
   OUT$ S\" file unit hits 1\n compiled 3" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 165" CONTAINS? TTRUE
   SESSION-CASES
   T-REPORT ;

;package
;using

NATIVE-FILE-REPLAY-TEST:MAIN
