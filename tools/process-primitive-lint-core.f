\ process-primitive-lint-core.f - confine raw process primitives.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f

package PROCESS-LINT

variable BAD
variable PREV-PRIM
variable PREV-TICK
variable ENGINE-REFS
variable LINE-START
variable LINE-END

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: PATH= ( ptr u8 n ptr u8 n -- bool )
   {: path:ptr pathu:n want:ptr wantu:n :}
   path pathu want wantu FS-PATH= if TRUE exit then
   pathu 2 < if FALSE exit then
   path c@ 46 <> if FALSE exit then
   path 1 BYTE+ c@ 47 <> if FALSE exit then
   path 2 BYTE+ pathu 2 - want wantu FS-PATH= ;

: ALLOWED? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu s" lib/process.f" PATH= if TRUE exit then
   path pathu s" lib/process-argv.f" PATH= if TRUE exit then
   path pathu s" lib/process-env.f" PATH= if TRUE exit then
   path pathu s" lib/process-cwd.f" PATH= if TRUE exit then
   path pathu s" lib/process-fork.f" PATH= ;

: FORTH? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu s" .f" HAS-EXT? if TRUE exit then
   path pathu s" .fs" HAS-EXT? ;

: RAW? ( -- bool )
   s" spawn-io" PAT-TOK= if TRUE exit then
   s" spawn-argv-io" PAT-TOK= if TRUE exit then
   s" spawn-argv-env-io" PAT-TOK= if TRUE exit then
   s" spawn-argv-env-cwd-io" PAT-TOK= if TRUE exit then
   s" fork" PAT-TOK= ;

: LINE-BEFORE? ( -- bool )
   LINE-START @ 0= if FALSE exit then
   PSA@ LINE-START @ 1- + c@ 10 <> ;

: LINE-AFTER? ( -- bool )
   LINE-END @ PSU @ >= if FALSE exit then
   PSA@ LINE-END @ + c@ 10 <> ;

: TOKEN-LINE$ ( -- ptr u8 n )
   PTA@ PSA@ - LINE-START !
   LINE-START @ PTU @ + LINE-END !
   begin LINE-BEFORE? while
      LINE-START @ 1- LINE-START !
   repeat
   begin LINE-AFTER? while
      LINE-END @ 1+ LINE-END !
   repeat
   PSA@ LINE-START @ + LINE-END @ LINE-START @ - LINT-TRIM ;

: ENGINE-LINE? ( -- bool )
   TOKEN-LINE$
   S\" s\" smoke-baked-word-resolves\" T-LABEL ' spawn-argv-env-cwd-io 0 <> -1 T="
   LINT-STR= ;

: ENGINE-REF? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu s" test/engine-suite.f" PATH= 0= if FALSE exit then
   s" spawn-argv-env-cwd-io" PAT-TOK= 0= if FALSE exit then
   ENGINE-LINE? 0= if FALSE exit then
   ENGINE-REFS @ 0 <> if FALSE exit then
   ENGINE-REFS @ 1+ ENGINE-REFS !
   TRUE ;

: PREV-SAFE? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   PREV-PRIM @ if TRUE exit then
   PREV-TICK @ if path pathu ENGINE-REF? exit then
   FALSE ;

: SAVE-PREV ( -- )
   s" PRIM:" PAT-TOK= PREV-PRIM !
   s" '" PAT-TOK=
   s" [']" PAT-TOK= or PREV-TICK ! ;

: SKIP-ESC-STRING ( -- )
   begin PAT-END? 0= while
      PAT-C@ 92 = if
         PAT-ADV
         PAT-END? 0= if PAT-ADV then
      else
         PAT-C@ 34 = if PAT-ADV exit then
         PAT-ADV
      then
   repeat ;

: REPORT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" PROCESS-PRIMITIVE-LINT " type path pathu type
   s" : raw `" type PAT-TOK$ type
   s" ` bypasses checked process wrappers" type cr
   BAD @ 1+ BAD ! ;

: SCAN-TEXT ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n src:ptr srcu:n :}
   path pathu ALLOWED? if exit then
   0 PREV-PRIM !
   0 PREV-TICK !
   0 ENGINE-REFS !
   src srcu PAT-RESET
   begin PAT-READ-TOKEN while
      PAT-TOK$ LINT-ESC-STRING-OPENER? if
         SKIP-ESC-STRING
      else PAT-TOK-STRING? if
         PAT-SKIP-STRING-BODY
      else
         RAW? path pathu PREV-SAFE? 0= and if path pathu REPORT then
      then then
      SAVE-PREV
   repeat ;

: FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FORTH? 0= if exit then
   path pathu LINT-SOURCE:LOAD
   path pathu LINT-SOURCE:TEXT SCAN-TEXT ;

public

: SOURCE ( ptr u8 n ptr u8 n -- n )
   0 BAD !
   SCAN-TEXT
   BAD @ ;

: SCAN ( ptr u8 n -- n )
   0 BAD !
   [: FILE ;] WALK-FILES
   BAD @ ;

;package
