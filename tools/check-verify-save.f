\ check-verify-save.f - builder of the saved verifier image. tools/check-verify-image.f
\ feeds the engine on stdin: tick-order@ 1 set-tier s" ROOT/tools/check-verify-save.f"
\ required CHECK-VERIFY-SAVE:TICK! 0 SCRIPT-ARGV$ CHECK-VERIFY-SAVE:SAVE
\ It loads the verifier and tools/check-verify-child-lib.f rooted at its own tree, as
\ tools/check-verify-child.f does. The image runs as `IMAGE -- SUBJECT ...`; ENTER
\ finds the root the source child finds, never its executable's (tools/build-out.f).

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
