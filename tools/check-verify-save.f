\ check-verify-save.f - the saved verifier image's builder. The program
\ tools/check-verify-image.f hands the engine on stdin loads this file once, by
\ absolute path, at tier 1:
\
\    tick-order@
\    1 set-tier
\    s" ROOT/tools/check-verify-save.f" required
\    CHECK-VERIFY-SAVE:TICK!
\    0 SCRIPT-ARGV$ CHECK-VERIFY-SAVE:SAVE
\
\ It loads the verifier and tools/check-verify-child-lib.f, the child's body,
\ rooted at its own tree ROOT as tools/check-verify-child.f roots them: the
\ builder's working directory can be another Habu tree. TICK! is the fresh
\ engine's tick observation, taken before anything loaded, as the source child
\ takes it. SAVE binds ENTER as the image's entry and saves. The saver and the
\ engine identity it reads are temporary: loaded after the marker below, bound
\ into cells, then retired with their checker rows and registry rows
\ (tools/aot-builder.f), so the image holds the boot prefix, the verifier, the
\ lib and this file, and nothing of the saver answers `held`.
\
\ The image runs as `IMAGE -- SUBJECT ...`, every argument form of
\ tools/check-verify-child.f. ENTER finds, when the image starts, the root the
\ source child finds: SOURCE-ROOT:ENGINE-FOR$ of the engine path baked here, the
\ working directory when that is a tree: SOURCE-ROOT:RESET cleared it at
\ capture, and the image's own executable lies in the build cache, under no tree
\ (docs/bootstrap.md "Stable local command").

package CHECK-VERIFY-SAVE

create ROOT PATH-CAP allot            variable ROOT-U
create ENGINE PATH-CAP allot          variable ENGINE-U        \ ENGINE-ID:PATH$ when built
create ENGINE-ROOT PATH-CAP allot     variable ENGINE-ROOT-U   \ ENTER's copy of its root
TYPED-VARIABLE START-XT [ [ -- ] -- ]
TYPED-VARIABLE SAVE-XT [ ptr u8 n -- ]

\ This file's tree: tools/check-verify-child.f CHECK-VERIFY-LOAD:SAVE.
: ROOT! ( -- )
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop
   SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U ! ;

: IN-ROOT ( [ -- ] -- )
   {: q :}
   ROOT ROOT-U @ q SOURCE-ROOT:WITH ;
: LOAD-LIB ( -- ) s" tools/check-verify-child-lib.f" required ;
: LOAD-SUPPORT ( -- )
   s" src/lib/engine-id.f" required
   s" src/habu/app-image.f" required ;

public

\ The builder exits after capture. Its engine-provided files belong to this
\ entry's tree during both the child and saver loads, as in the source child.
: LOAD ( -- )
   ROOT!
   ROOT ROOT-U @ SOURCE-ROOT:ENGINE!
   [: LOAD-LIB ;] IN-ROOT ;

;package

CHECK-VERIFY-SAVE:LOAD

package CHECK-VERIFY-SAVE
public

: TICK! ( n ptr u8 -- ) VERIFY:ENTRY-TICK-ORDER! ;

: ENTER ( -- )
   ENGINE ENGINE-U @ SOURCE-ROOT:ENGINE-FOR$ {: a:ptr u:n :}
   a ENGINE-ROOT u BYTE-COPY  u ENGINE-ROOT-U !
   ENGINE-ROOT ENGINE-ROOT-U @ SOURCE-ROOT:ENGINE!
   CHECK-VERIFY-CHILD:MAIN ;

\ Invoke from the outer stdin stream after this file has returned.
: SAVE ( ptr u8 n -- )
   [: ENTER ;] START-XT @ execute
   SAVE-XT @ execute ;

;package

\ The saver and the engine identity are temporary: bound into the cells above
\ before the dictionary and checker rows after this marker are retired.
CHECKER-SCOPE-START
REQUIRE-REG:COUNT
: CHECK-VERIFY-SAVE-SUPPORT ( -- ) ;

package CHECK-VERIFY-SAVE
public
: LOAD-SAVER ( -- ) [: LOAD-SUPPORT ;] IN-ROOT ;
;package

CHECK-VERIFY-SAVE:LOAD-SAVER

package CHECK-VERIFY-SAVE
private
: ENGINE! ( -- )
   ENGINE-ID:PATH$ {: a:ptr u:n :}
   a ENGINE u BYTE-COPY  u ENGINE-U ! ;
ENGINE!
' APP-IMAGE:START! START-XT !
' APP-IMAGE:SAVE SAVE-XT !
;package

s" CHECK-VERIFY-SAVE-SUPPORT" HIDE-DEFS-FROM
CHECKER-SCOPE-DONE
REQUIRE-REG:TRUNCATE
