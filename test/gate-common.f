\ gate-common.f - checked helpers for native gate runner slices.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f.

$40000 constant GE-SRC-CAP
64 constant GE-SRC-MAX
120000 constant GE-TIMEOUT-MS
10 constant GE-LF
32 constant GE-SP
34 constant GE-DQ

create GE-SRC-BUF GE-SRC-CAP allot
create GE-SRC-A GE-SRC-MAX cells allot
create GE-SRC-LEN GE-SRC-MAX cells allot

variable GE-SRC-U
variable GE-SRC-N
variable GE-RD

: GE-STORE-CAPTURE ( len len rc -- ) {: outu erru rc :}
   rc RC>N GT-OUTCOME-CODE !
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   erru LEN>N GT-ERR-U !
   outu LEN>N GT-OUT-U ! ;

: GE-RUN-ENV ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN GT-OUT-BUF GT-OUT-CAP >LEN
   GT-ERR-BUF GT-ERR-CAP >LEN timeout >MS
   RUN-ARGV-ENV-CAPTURE
   GE-STORE-CAPTURE ;

: GE-RUN-STDIN ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu in:ptr inu timeout :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN in inu >LEN GT-OUT-BUF GT-OUT-CAP >LEN
   GT-ERR-BUF GT-ERR-CAP >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE
   GE-STORE-CAPTURE ;

: GE-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" gate phase failed" 1 die ;

: GE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-RC@ 0 <> if label labelu GE-FAIL then ;

: GE-EXPECT-RC ( n ptr u8 n -- ) {: want label:ptr labelu :}
   GT-RC@ want <> if label labelu GE-FAIL then ;

: GE-EXPECT-NONZERO ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-RC@ 0= if label labelu GE-FAIL then ;

: GE-EXPECT-SILENT ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-OUT$ nip 0 <> if label labelu GE-FAIL then
   GT-ERR$ nip 0 <> if label labelu GE-FAIL then ;

: GE-EXPECT-OUT ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu label:ptr labelu :}
   GT-OUT$ want wantu STR= 0= if label labelu GE-FAIL then ;

: GE-EXPECT-OUT-HAS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu label:ptr labelu :}
   GT-OUT$ want wantu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-EXPECT-ERR-HAS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu label:ptr labelu :}
   GT-ERR$ want wantu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-SB-LF ( -- )
   GE-LF SB-APPEND-C ;

: GE-OUT-LINE ( ptr u8 n -- )
   SB-APPEND
   GE-SB-LF ;

: GE-SRC-RESET ( -- )
   0 GE-SRC-U !
   0 GE-SRC-N ! ;

: GE-SRC$ ( n -- ptr u8 n ) {: idx :}
   idx 0 < if E-STR-BOUNDS throw then
   idx GE-SRC-N @ >= if E-STR-BOUNDS throw then
   idx cells GE-SRC-A + @
   idx cells GE-SRC-LEN + @ ;

: GE-SRC-PATH+ ( ptr u8 n -- ) {: path:ptr pathu :}
   GE-SRC-N @ GE-SRC-MAX >= if E-STR-CAPACITY throw then
   path GE-SRC-A GE-SRC-N @ cells + !
   pathu GE-SRC-LEN GE-SRC-N @ cells + !
   GE-SRC-N @ 1+ GE-SRC-N ! ;

: GE-SRC-C ( n -- ) {: c :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   GE-SRC-U @ 1 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   c GE-SRC-BUF GE-SRC-U @ + c!
   GE-SRC-U @ 1+ GE-SRC-U ! ;

: GE-SRC-U+ ( n -- ) {: n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + GE-SRC-C ;

: GE-SRC-REPEAT-C ( n n -- ) {: u c :}
   u 0 < if E-STR-BOUNDS throw then
   u 0 ?do c GE-SRC-C loop ;

: GE-SRC+ ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   GE-SRC-U @ u + GE-SRC-CAP > if E-STR-CAPACITY throw then
   a GE-SRC-BUF GE-SRC-U @ + u BYTE-COPY
   GE-SRC-U @ u + GE-SRC-U ! ;

: GE-SRC-SP ( -- )
   GE-SP GE-SRC-C ;

: GE-SRC-LF ( -- )
   GE-LF GE-SRC-C ;

: GE-SRC-LINE ( ptr u8 n -- )
   GE-SRC+
   GE-SRC-LF ;

: GE-SRC-S" ( ptr u8 n -- ) {: a:ptr u :}
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GE-SRC-CHECK-LINE ( ptr u8 n -- )
   GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE ;

: GE-SRC-FILE+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu GE-SRC-PATH+
   path pathu GE-SRC-BUF GE-SRC-U @ + GE-SRC-CAP GE-SRC-U @ -
   READ-ALL GE-RD !
   GE-SRC-U @ GE-RD @ + GE-SRC-U ! ;

: GE-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: GE-FILES-END? ( ptr u8 n -- bool )
   s" ;GE-FILES" STR= ;

: GE-FILES-ITEM, ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u c,
   0 begin dup u < while
      dup a + c@ c,
      1+
   repeat drop ;

: GE-FILES-PARSE ( -- )
   begin
      parse-name dup 0= if 2drop E-STR-BOUNDS throw then
      2dup GE-FILES-END? if 2drop 0 c, exit then
      GE-FILES-ITEM,
   again ;

: GE-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: GE-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap GE-FILES-WALK ;

: GE-FILES: ( -- )
   create GE-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      GE-FILES-RUN ;

: GE-HB-RESET ( -- )
   PROC-ARGV-ENV-RESET ;

: GE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO
   label labelu GT-PROGRESS-PASS ;

: GE-BIN-HB? ( ptr u8 n -- bool )
   s" bin/hb" STR= ;

: GE-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? if
      path pathu GE-BIN-HB? 0= if path pathu REMOVE-FILE then
   then ;

: GE-CLEAN-BIN ( -- )
   s" bin" [: GE-REMOVE-BIN-OTHER ;] WALK-FILES ;

: GE-CHECK-ARGV ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" tools/date.f" GE-ARG+
   s" lib/errors.f" GE-ARG+
   s" lib/string.f" GE-ARG+
   s" lib/memory.f" GE-ARG+
   s" lib/vector.f" GE-ARG+
   s" lib/fs.f" GE-ARG+
   s" lib/fs-mutate.f" GE-ARG+
   s" lib/process.f" GE-ARG+
   s" lib/process-argv.f" GE-ARG+
   s" lib/source.f" GE-ARG+
   s" tools/lint/text.f" GE-ARG+
   s" tools/lint/token.f" GE-ARG+
   s" tools/lint/lib.f" GE-ARG+
   s" tools/lint/json-writer.f" GE-ARG+
   s" tools/lint/source-lex.f" GE-ARG+
   s" tools/diag-origin-core.f" GE-ARG+
   s" tools/json.f" GE-ARG+
   s" tools/json-only-core.f" GE-ARG+
   s" tools/signature-lint-core.f" GE-ARG+
   s" tools/checked-boundary-lint-core.f" GE-ARG+
   s" tools/trust-lint-core.f" GE-ARG+
   s" tools/argv.f" GE-ARG+
   s" tools/check.f" GE-ARG+
   s" --" GE-ARG+ ;

: GE-CHECK-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   label labelu GE-EXPECT-SILENT ;

: GE-CHECK-SRC-LIST ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --source-list" GE-ARG+
   0 begin dup GE-SRC-N @ < while
      dup GE-SRC$ GE-ARG+
      1+
   repeat drop
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GE-EXPECT-SILENT ;
