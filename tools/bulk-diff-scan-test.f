\ bulk-diff-scan-test.f - direct bulk scanner filesystem regressions.
\ Run: bin/hb --load tools/bulk-diff-scan-test.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/json-write.f
require tools/json.f
require tools/bulk-diff-scan-core.f
require tools/diff-side-content-read.f

package BULK-DIFF-SCAN-TEST

8192 constant META-CAP

create ROOT FS-PATH-CAP allot
variable ROOT-U
create LEFT FS-PATH-CAP allot
variable LEFT-U
create RIGHT FS-PATH-CAP allot
variable RIGHT-U
create META-PATH FS-PATH-CAP allot
variable META-PATH-U
create NODE FS-PATH-CAP allot
create META META-CAP allot
variable META-U
create PATH-BUF FS-PATH-CAP allot
create DIGEST $20 allot
create EXPECT-DIGEST $20 allot
create BIN $61 c, 0 c, $62 c,
create ADV-PATH $61 c, $20 c, $09 c, $22 c, $5C c, $0A c, $0D c, $7A c,

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: LEFT$ ( -- ptr u8 n )
   LEFT LEFT-U @ ;

: RIGHT$ ( -- ptr u8 n )
   RIGHT RIGHT-U @ ;

: META-PATH$ ( -- ptr u8 n )
   META-PATH META-PATH-U @ ;

: META$ ( -- ptr u8 n )
   META META-U @ ;

: PATH! ( ptr u8 n ptr u8 ptr n -- ) {: root:ptr rootu:n dst:ptr up:ptr :}
   root rootu s" left" dst JOIN-PATH up ! ;

: COPY-ROOT ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT u BYTE-COPY
   u ROOT-U ! ;

: SETUP-PATHS ( -- )
   s" habu-bulk-diff" TMPDIR-MKDIR COPY-ROOT
   ROOT$ CLEANUP-TREE+
   ROOT$ LEFT LEFT-U PATH!
   ROOT$ s" right" RIGHT JOIN-PATH RIGHT-U !
   ROOT$ s" metadata.jsonl" META-PATH JOIN-PATH META-PATH-U !
   LEFT$ MAKE-DIR
   RIGHT$ MAKE-DIR ;

: NODE! ( ptr u8 n ptr u8 n -- ptr u8 n ) {: root:ptr rootu:n rel:ptr relu:n :}
   root rootu rel relu NODE JOIN-PATH NODE swap ;

: FILE+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n rel:ptr relu:n body:ptr bodyu:n :}
   root rootu rel relu NODE! body bodyu WRITE-ALL ;

: LINK+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n rel:ptr relu:n target:ptr targetu:n :}
   target targetu root rootu rel relu NODE! MAKE-SYMLINK ;

: SETUP-FILES ( -- )
   LEFT$ s" empty" s" " FILE+
   RIGHT$ s" empty" s" " FILE+
   LEFT$ s" text" s" old" FILE+
   RIGHT$ s" text" BIN 3 FILE+
   LEFT$ s" sym" s" old-target" LINK+
   RIGHT$ s" sym" s" new-target" LINK+
   RIGHT$ s" added" s" added-body" FILE+
   LEFT$ ADV-PATH 8 s" old-adv" FILE+
   RIGHT$ ADV-PATH 8 s" new-adv" FILE+
   LEFT$ s" removed" s" removed-body" FILE+ ;

: META-RESET ( -- )
   0 META-U ! ;

: META+ ( ptr u8 n -- ) {: a:ptr u:n :}
   META-U @ u + META-U @ < if E-SIDE-CAPACITY throw then
   META-U @ u + META-CAP > if E-SIDE-CAPACITY throw then
   a META META-U @ + u BYTE-COPY
   META-U @ u + META-U ! ;

: META-C+ ( n -- ) {: c:n :}
   META-U @ 1+ META-CAP > if E-SIDE-CAPACITY throw then
   c META META-U @ + c!
   META-U @ 1+ META-U ! ;

: FIELD-S ( ptr u8 n -- )
   JW-STRING JW-COMMA ;

: FIELD-B ( bool -- )
   JW-BOOL JW-COMMA ;

: ROW+ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: status:ptr statusu:n old:ptr oldu:n oldtype:ptr oldtypeu:n new:ptr newu:n newtype:ptr newtypeu:n :}
   JW-RESET
   JW-ARRAY-START
   status statusu FIELD-S
   old oldu FIELD-S
   oldtype oldtypeu FIELD-S
   false FIELD-B false FIELD-B
   new newu FIELD-S
   newtype newtypeu FIELD-S
   false FIELD-B false JW-BOOL
   JW-ARRAY-END
   JW$ META+
   $0A META-C+ ;

: BUILD-META ( -- )
   META-RESET
   s" modified" s" empty" s" file" s" empty" s" file" ROW+
   s" modified" s" text" s" file" s" text" s" file" ROW+
   s" modified" s" sym" s" symlink" s" sym" s" symlink" ROW+
   s" added" s" " s" " s" added" s" file" ROW+
   s" modified" s" sub" s" git-submodule" s" sub" s" git-submodule" ROW+
   s" modified" ADV-PATH 8 s" file" ADV-PATH 8 s" file" ROW+
   s" removed" s" removed" s" file" s" " s" " ROW+
   META-PATH$ META$ WRITE-ALL ;

: KIND-N ( content-kind -- n )
   MATCH content-kind
      absent  OF 0 ENDOF
      file    OF 1 ENDOF
      symlink OF 2 ENDOF
      gitlink OF 3 ENDOF
   ;MATCH ;

: EXPECT-PATH ( ptr u8 n bool -- ) {: want:ptr wantu:n old?:bool :}
   old? if
      PATH-BUF FS-PATH-CAP DIFF-CONTENT:OLD-PATH
   else
      PATH-BUF FS-PATH-CAP DIFF-CONTENT:NEW-PATH
   then {: gotu:n :}
   PATH-BUF gotu want wantu T$= ;

: EXPECT-HASH ( ptr u8 n bool -- ) {: body:ptr bodyu:n old?:bool :}
   body bodyu EXPECT-DIGEST SHA256
   old? if
      DIGEST DIFF-CONTENT:OLD-DIGEST
   else
      DIGEST DIFF-CONTENT:NEW-DIGEST
   then
   DIGEST $20 EXPECT-DIGEST $20 T$= ;

: TEST-SUCCESS ( -- )
   LEFT$ RIGHT$ META-PATH$ BULK-DIFF:RUN
   META$ DIFF-CONTENT:VALIDATE-BINDING 7 T=

   0 DIFF-CONTENT:ROW-SELECT
   DIFF-CONTENT:OLD-CONTENT-SIZE 0 T=
   DIFF-CONTENT:NEW-CONTENT-SIZE 0 T=
   DIFF-CONTENT:OLD-BINARY? TFALSE

   1 DIFF-CONTENT:ROW-SELECT
   DIFF-CONTENT:OLD-CONTENT-SIZE 3 T=
   DIFF-CONTENT:OLD-BINARY? TFALSE
   s" old" true EXPECT-HASH
   DIFF-CONTENT:NEW-CONTENT-SIZE 3 T=
   DIFF-CONTENT:NEW-BINARY? TTRUE
   BIN 3 false EXPECT-HASH

   2 DIFF-CONTENT:ROW-SELECT
   DIFF-CONTENT:OLD-KIND KIND-N 2 T=
   DIFF-CONTENT:OLD-CONTENT-SIZE 10 T=
   s" old-target" true EXPECT-HASH
   DIFF-CONTENT:NEW-CONTENT-SIZE 10 T=
   s" new-target" false EXPECT-HASH

   3 DIFF-CONTENT:ROW-SELECT
   DIFF-CONTENT:OLD-PRESENT? TFALSE
   DIFF-CONTENT:OLD-KIND KIND-N 0 T=
   DIFF-CONTENT:NEW-CONTENT-SIZE 10 T=

   4 DIFF-CONTENT:ROW-SELECT
   DIFF-CONTENT:OLD-KIND KIND-N 3 T=
   DIFF-CONTENT:NEW-KIND KIND-N 3 T=
   DIFF-CONTENT:OLD-CONTENT-SIZE 0 T=

   5 DIFF-CONTENT:ROW-SELECT
   ADV-PATH 8 true EXPECT-PATH
   ADV-PATH 8 false EXPECT-PATH
   DIFF-CONTENT:OLD-CONTENT-SIZE 7 T=
   s" old-adv" true EXPECT-HASH
   DIFF-CONTENT:NEW-CONTENT-SIZE 7 T=
   s" new-adv" false EXPECT-HASH

   6 DIFF-CONTENT:ROW-SELECT
   DIFF-CONTENT:OLD-CONTENT-SIZE 12 T=
   s" removed-body" true EXPECT-HASH
   DIFF-CONTENT:NEW-PRESENT? TFALSE ;

: RUN-CODE ( -- n )
   [: LEFT$ RIGHT$ META-PATH$ BULK-DIFF:RUN 2drop ;] catch ;

: JSON-FIELD$ ( n ptr u8 n -- ptr u8 n )
   JSON-GET JSON-STRING$ ;

: TEST-REPORT ( n -- ) {: code:n :}
   code BULK-DIFF:REPORT JSON-PARSE {: root:n :}
   root s" phase" JSON-FIELD$ s" stat" T$=
   root s" row" JSON-GET JSON-NUMBER$ s" 0" T$=
   root s" side" JSON-FIELD$ s" old" T$=
   root s" path_hex" JSON-FIELD$ s" 6d697373696e67" T$= ;

: TEST-MISSING ( -- )
   META-RESET
   s" modified" s" missing" s" file" s" missing" s" file" ROW+
   META-PATH$ META$ WRITE-ALL
   RUN-CODE dup E-FS-STAT T= TEST-REPORT ;

: TEST-TYPE ( -- )
   META-RESET
   s" modified" s" text" s" symlink" s" text" s" file" ROW+
   META-PATH$ META$ WRITE-ALL
   RUN-CODE E-FS-STAT T= ;

: TEST-MALFORMED ( -- )
   META-PATH$ s" [" WRITE-ALL
   RUN-CODE E-SIDE-SYNTAX T=
   META-PATH$ s" []" WRITE-ALL
   RUN-CODE E-SIDE-SYNTAX T= ;

: TEST-ABSENT-PATH ( -- )
   META-RESET
   s" added" s" not-empty" s" " s" added" s" file" ROW+
   META-PATH$ META$ WRITE-ALL
   RUN-CODE E-SIDE-SYNTAX T= ;

: TEST-EMPTY-META ( -- )
   META-RESET
   META-PATH$ META$ WRITE-ALL
   LEFT$ RIGHT$ META-PATH$ BULK-DIFF:RUN
   META$ DIFF-CONTENT:VALIDATE-BINDING 0 T= ;

public

: RUN ( -- )
   T-RESET
   SETUP-PATHS
   SETUP-FILES
   BUILD-META
   TEST-SUCCESS
   TEST-MISSING
   TEST-TYPE
   TEST-MALFORMED
   TEST-ABSENT-PATH
   TEST-EMPTY-META
   CLEANUP-RUN
   T-REPORT ;

;package

BULK-DIFF-SCAN-TEST:RUN
