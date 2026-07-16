\ diff-capture-test.f - real-jj framed diff producer fixtures.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require tools/diff-capture-core.f
require tools/lint/diff-frame.f

require tools/diff-capture-test-support.f
require tools/diff-capture-command-test-support.f

package DIFF-CAPTURE-TEST
private

$10000 constant CAP
5 constant WARN-U
$1000 constant LARGE-U
$0A constant LF-C
$0D constant CR-C
$09 constant TAB-C
$22 constant QUOTE-C
$5C constant SLASH-C

create ROOT FS-PATH-CAP allot
variable ROOT-U
create PATH-A FS-PATH-CAP allot
variable PATH-A-U
create PATH-B FS-PATH-CAP allot
variable PATH-B-U
create ART-PATH FS-PATH-CAP allot
variable ART-PATH-U
create BAD-PATH FS-PATH-CAP allot
variable BAD-PATH-U
create WEIRD FS-PATH-CAP allot
variable WEIRD-U
create WEIRD-DIR FS-PATH-CAP allot
variable WEIRD-DIR-U
create NESTED FS-PATH-CAP allot
variable NESTED-U
create OUT CAP allot
create ERR CAP allot
create ART CAP allot
variable ART-U
create JJ-BUF FS-PATH-CAP allot
PTR-VARIABLE JJ-A
variable JJ-U
create GIT-BUF FS-PATH-CAP allot
PTR-VARIABLE GIT-A
variable GIT-U
variable LAST-SECTION
variable SAW-PURE
variable SAW-CHANGED-RENAME
variable SAW-PURE-COPY
variable SAW-CHANGED-COPY
variable SAW-RENAME-MODE
variable SAW-COPY-MODE
variable SAW-RENAME-BODY-MODE
variable SAW-COPY-BODY-MODE
variable SAW-COPY-SOURCE
variable SAW-WEIRD
variable SAW-BINARY
variable SAW-MODE
variable SAW-EMPTY
variable SAW-REMOVED
variable SAW-GITLINK
variable SAW-MODULES

create OLD-BIN $41 c, 0 c, $42 c,
create NEW-BIN $41 c, 0 c, $43 c,

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: PATH-A$ ( -- ptr u8 n )
   PATH-A PATH-A-U @ ;

: PATH-B$ ( -- ptr u8 n )
   PATH-B PATH-B-U @ ;

: ART-PATH$ ( -- ptr u8 n )
   ART-PATH ART-PATH-U @ ;

: BAD-PATH$ ( -- ptr u8 n )
   BAD-PATH BAD-PATH-U @ ;

: WEIRD$ ( -- ptr u8 n )
   WEIRD WEIRD-U @ ;

: JJ$ ( -- ptr u8 n )
   JJ-A @ JJ-U @ ;

: GIT$ ( -- ptr u8 n )
   GIT-A @ GIT-U @ ;

: NESTED$ ( -- ptr u8 n )
   NESTED NESTED-U @ ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: JJ! ( -- )
   s" jj" >LEN JJ-BUF FIND-EXECUTABLE MATCH option
      none OF E-DIFF-CAPTURE throw ENDOF
      some OF LEN>N JJ-U ! JJ-BUF JJ-A ! ENDOF
   ;MATCH ;

: GIT! ( -- )
   s" git" >LEN GIT-BUF FIND-EXECUTABLE MATCH option
      none OF E-DIFF-CAPTURE throw ENDOF
      some OF LEN>N GIT-U ! GIT-BUF GIT-A ! ENDOF
   ;MATCH ;

: JJ-BEGIN ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: RUN-AT ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n cwd:ptr cwdu:n :}
   exe exeu >LEN cwd cwdu >LEN OUT CAP >LEN ERR CAP >LEN 10000 >MS
   RUN-ARGV-ENV-CWD-CAPTURE
   {: outu:len erru:len rc:rc :}
   outu drop erru drop
   rc RC>N 0<> if E-DIFF-CAPTURE throw then ;

: JJ-RUN ( -- )
   JJ$ ROOT$ RUN-AT ;

: GIT-RUN ( -- )
   GIT$ ROOT$ RUN-AT ;

: NESTED-GIT-RUN ( -- )
   GIT$ NESTED$ RUN-AT ;

: JJ-INIT ( -- )
   JJ-BEGIN
   s" git" ARG s" init" ARG
   JJ-RUN ;

: JJ-CONFIG ( -- )
   JJ-BEGIN
   s" config" ARG s" set" ARG s" --repo" ARG
   S\" revset-aliases.\qtrunk()\q" ARG s" root()" ARG
   JJ-RUN ;

: JJ-COMMIT ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   JJ-BEGIN
   s" commit" ARG s" -m" ARG msg msgu ARG
   JJ-RUN ;

: JJ-CHMOD-X ( ptr u8 n -- ) {: path:ptr pathu:n :}
   JJ-BEGIN
   s" file" ARG s" chmod" ARG s" x" ARG path pathu ARG
   JJ-RUN ;

: JJ-IMPORT ( -- )
   JJ-BEGIN
   s" git" ARG s" import" ARG
   JJ-RUN ;

: PATH-A! ( ptr u8 n -- ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH-A JOIN-PATH PATH-A-U ! ;

: PATH-B! ( ptr u8 n -- ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH-B JOIN-PATH PATH-B-U ! ;

: WRITE-REL ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n a:ptr u:n :}
   rel relu PATH-A!
   PATH-A$ a u WRITE-ALL ;

: REMOVE-REL ( ptr u8 n -- )
   PATH-A! PATH-A$ REMOVE-FILE ;

: RENAME-REL ( ptr u8 n ptr u8 n -- ) {: old:ptr oldu:n new:ptr newu:n :}
   old oldu PATH-A!
   new newu PATH-B!
   PATH-A$ PATH-B$ RENAME-FILE ;

: GIT-CONFIG-AT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: cwd:ptr cwdu:n key:ptr keyu:n value:ptr valueu:n :}
   JJ-BEGIN
   s" config" ARG key keyu ARG value valueu ARG
   GIT$ cwd cwdu RUN-AT ;

: GIT-ROOT-CONFIG ( -- )
   ROOT$ s" user.name" s" Habu Test" GIT-CONFIG-AT
   ROOT$ s" user.email" s" habu-test@example.invalid" GIT-CONFIG-AT ;

: GIT-NESTED-CONFIG ( -- )
   NESTED$ s" user.name" s" Habu Test" GIT-CONFIG-AT
   NESTED$ s" user.email" s" habu-test@example.invalid" GIT-CONFIG-AT ;

: NESTED-INIT ( -- )
   JJ-BEGIN
   s" init" ARG
   NESTED-GIT-RUN
   GIT-NESTED-CONFIG
   NESTED$ s" child.f" PATH-A JOIN-PATH PATH-A-U !
   PATH-A$ s" child" WRITE-ALL
   JJ-BEGIN
   s" add" ARG s" child.f" ARG
   NESTED-GIT-RUN
   JJ-BEGIN
   s" commit" ARG s" -m" ARG s" child" ARG
   NESTED-GIT-RUN ;

: ADD-SUBMODULE ( -- )
   ROOT$ s" nested" NESTED JOIN-PATH NESTED-U !
   NESTED$ MAKE-DIRS
   NESTED-INIT
   JJ-BEGIN
   s" -c" ARG s" protocol.file.allow=always" ARG
   s" submodule" ARG s" add" ARG s" ./nested" ARG s" sub" ARG
   GIT-RUN
   NESTED$ REMOVE-TREE ;

: GIT-COMMIT-CHANGES ( -- )
   JJ-BEGIN
   s" add" ARG s" -A" ARG
   GIT-RUN
   JJ-BEGIN
   s" commit" ARG s" -m" ARG s" changed" ARG
   GIT-RUN ;

: WEIRD-NAME! ( -- )
   SB-RESET
   s" line" SB-APPEND LF-C SB-APPEND-C
   s" diff --git a/fake b/fake" SB-APPEND CR-C SB-APPEND-C
   s"  space" SB-APPEND TAB-C SB-APPEND-C
   s" quote" SB-APPEND QUOTE-C SB-APPEND-C
   s" slash" SB-APPEND SLASH-C SB-APPEND-C
   s"  b/ and " SB-APPEND
   SB$ WEIRD-DIR WEIRD-DIR-U COPY!
   s" /file.f" SB-APPEND
   SB$ WEIRD WEIRD-U COPY! ;

: PREPARE-ROOT ( -- )
   CLEANUP-RESET
   s" habu-diff-capture-test" TMPDIR-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" artifact.hbdiff" ART-PATH JOIN-PATH ART-PATH-U !
   ROOT$ s" invalid.hbdiff" BAD-PATH JOIN-PATH BAD-PATH-U !
   WEIRD-NAME!
   ROOT$ WEIRD-DIR WEIRD-DIR-U @ PATH-A JOIN-PATH PATH-A-U !
   PATH-A$ MAKE-DIRS ;

: BASE-RENAME$ ( -- ptr u8 n )
   SB-RESET
   s" rename anchor 01 alpha" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 02 beta" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 03 gamma" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 04 delta" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 05 epsilon" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 06 zeta" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: CHANGED-RENAME$ ( -- ptr u8 n )
   SB-RESET
   s" rename anchor 01 alpha" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 02 beta" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 03 changed" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 04 delta" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 05 epsilon" SB-APPEND LF-C SB-APPEND-C
   s" rename anchor 06 zeta" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: BASE-COPY$ ( -- ptr u8 n )
   SB-RESET
   s" copy anchor 01 alpha" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 02 beta" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 03 gamma" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 04 delta" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 05 epsilon" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 06 zeta" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: CHANGED-COPY$ ( -- ptr u8 n )
   SB-RESET
   s" copy anchor 01 alpha" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 02 beta" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 03 target" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 04 delta" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 05 epsilon" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 06 zeta" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: CHANGED-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" copy anchor 01 source" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 02 beta" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 03 gamma" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 04 delta" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 05 epsilon" SB-APPEND LF-C SB-APPEND-C
   s" copy anchor 06 zeta" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: BASE-FILES ( -- )
   s" pure-old.f" s" unchanged" WRITE-REL
   s" changed-old.f" BASE-RENAME$ WRITE-REL
   s" copy-source.f" BASE-COPY$ WRITE-REL
   s" exec-old.f" s" rename and mode" WRITE-REL
   s" mix-old.f" BASE-RENAME$ WRITE-REL
   s" binary.f" OLD-BIN 3 WRITE-REL
   s" mode.f" s" executable later" WRITE-REL
   s" removed.f" s" removed" WRITE-REL
   WEIRD$ s" before" WRITE-REL ;

: CHANGE-FILES ( -- )
   s" pure-old.f" s" pure-new.f" RENAME-REL
   s" changed-old.f" s" changed-new.f" RENAME-REL
   s" changed-new.f" CHANGED-RENAME$ WRITE-REL
   s" copy-pure.f" BASE-COPY$ WRITE-REL
   s" copy-changed.f" CHANGED-COPY$ WRITE-REL
   s" copy-mode.f" BASE-COPY$ WRITE-REL
   s" copy-mode.f" JJ-CHMOD-X
   s" copy-body-mode.f" CHANGED-COPY$ WRITE-REL
   s" copy-body-mode.f" JJ-CHMOD-X
   s" copy-source.f" CHANGED-SOURCE$ WRITE-REL
   s" exec-old.f" s" exec-new.f" RENAME-REL
   s" exec-new.f" JJ-CHMOD-X
   s" mix-old.f" s" mix-new.f" RENAME-REL
   s" mix-new.f" CHANGED-RENAME$ WRITE-REL
   s" mix-new.f" JJ-CHMOD-X
   s" binary.f" NEW-BIN 3 WRITE-REL
   s" mode.f" JJ-CHMOD-X
   s" empty-added.f" s" " WRITE-REL
   s" removed.f" REMOVE-REL
   WEIRD$ s" after" WRITE-REL ;

: STATUS# ( DIFF:status -- n )
   MATCH DIFF:status
      modified OF 0 ENDOF
      added    OF 1 ENDOF
      removed  OF 2 ENDOF
      renamed  OF 3 ENDOF
      copied   OF 4 ENDOF
   ;MATCH ;

: FORM# ( DIFF:form -- n )
   MATCH DIFF:form
      text   OF 0 ENDOF
      binary OF 1 ENDOF
      mode   OF 2 ENDOF
      empty  OF 3 ENDOF
      pure   OF 4 ENDOF
      gitlink OF 5 ENDOF
   ;MATCH ;

: NEW= ( ptr u8 n -- bool )
   DIFF:SECTION-NEW$ 2swap STR= ;

: OLD= ( ptr u8 n -- bool )
   DIFF:SECTION-OLD$ 2swap STR= ;

: CHECK-PURE ( -- )
   DIFF:SECTION-STATUS STATUS# 3 T=
   DIFF:SECTION-FORM FORM# 4 T=
   DIFF:SECTION-BODY? TFALSE
   s" pure-old.f" OLD= TTRUE
   s" pure-new.f" NEW= TTRUE
   true SAW-PURE ! ;

: CHECK-CHANGED-RENAME ( -- )
   DIFF:SECTION-STATUS STATUS# 3 T=
   DIFF:SECTION-FORM FORM# 0 T=
   DIFF:SECTION-BODY? TTRUE
   s" changed-old.f" OLD= TTRUE
   true SAW-CHANGED-RENAME ! ;

: CHECK-PURE-COPY ( -- )
   DIFF:SECTION-STATUS STATUS# 4 T=
   DIFF:SECTION-FORM FORM# 4 T=
   DIFF:SECTION-BODY? TFALSE
   s" copy-source.f" OLD= TTRUE
   true SAW-PURE-COPY ! ;

: CHECK-CHANGED-COPY ( -- )
   DIFF:SECTION-STATUS STATUS# 4 T=
   DIFF:SECTION-FORM FORM# 0 T=
   DIFF:SECTION-BODY? TTRUE
   s" copy-source.f" OLD= TTRUE
   true SAW-CHANGED-COPY ! ;

: CHECK-RENAME-MODE ( -- )
   DIFF:SECTION-STATUS STATUS# 3 T=
   DIFF:SECTION-FORM FORM# 2 T=
   DIFF:SECTION-BODY? TFALSE
   DIFF:SECTION-MODE? TTRUE
   s" exec-old.f" OLD= TTRUE
   true SAW-RENAME-MODE ! ;

: CHECK-COPY-MODE ( -- )
   DIFF:SECTION-STATUS STATUS# 4 T=
   DIFF:SECTION-FORM FORM# 2 T=
   DIFF:SECTION-BODY? TFALSE
   DIFF:SECTION-MODE? TTRUE
   s" copy-source.f" OLD= TTRUE
   true SAW-COPY-MODE ! ;

: CHECK-RENAME-BODY-MODE ( -- )
   DIFF:SECTION-STATUS STATUS# 3 T=
   DIFF:SECTION-FORM FORM# 0 T=
   DIFF:SECTION-BODY? TTRUE
   DIFF:SECTION-MODE? TTRUE
   s" mix-old.f" OLD= TTRUE
   true SAW-RENAME-BODY-MODE ! ;

: CHECK-COPY-BODY-MODE ( -- )
   DIFF:SECTION-STATUS STATUS# 4 T=
   DIFF:SECTION-FORM FORM# 0 T=
   DIFF:SECTION-BODY? TTRUE
   DIFF:SECTION-MODE? TTRUE
   s" copy-source.f" OLD= TTRUE
   true SAW-COPY-BODY-MODE ! ;

: CHECK-COPY-SOURCE ( -- )
   DIFF:SECTION-STATUS STATUS# 0 T=
   DIFF:SECTION-FORM FORM# 0 T=
   DIFF:SECTION-BODY? TTRUE
   true SAW-COPY-SOURCE ! ;

: CHECK-WEIRD ( -- )
   DIFF:SECTION-STATUS STATUS# 0 T=
   DIFF:SECTION-FORM FORM# 0 T=
   WEIRD$ NEW= TTRUE
   true SAW-WEIRD ! ;

: CHECK-BINARY ( -- )
   DIFF:SECTION-FORM FORM# 1 T=
   DIFF:SECTION-BODY? TTRUE
   true SAW-BINARY ! ;

: CHECK-MODE ( -- )
   DIFF:SECTION-FORM FORM# 2 T=
   DIFF:SECTION-BODY? TFALSE
   DIFF:SECTION-MODE? TTRUE
   true SAW-MODE ! ;

: CHECK-EMPTY ( -- )
   DIFF:SECTION-STATUS STATUS# 1 T=
   DIFF:SECTION-FORM FORM# 3 T=
   DIFF:SECTION-OLD? TFALSE
   DIFF:SECTION-NEW? TTRUE
   true SAW-EMPTY ! ;

: CHECK-REMOVED ( -- )
   DIFF:SECTION-STATUS STATUS# 2 T=
   DIFF:SECTION-NEW? TFALSE
   true SAW-REMOVED ! ;

: CHECK-GITLINK ( -- )
   DIFF:SECTION-STATUS STATUS# 1 T=
   DIFF:SECTION-FORM FORM# 5 T=
   DIFF:SECTION-BODY? TFALSE
   true SAW-GITLINK ! ;

: CHECK-MODULES ( -- )
   DIFF:SECTION-STATUS STATUS# 1 T=
   DIFF:SECTION-FORM FORM# 0 T=
   true SAW-MODULES ! ;

: CHECK-SECTION ( -- )
   s" pure-new.f" NEW= if CHECK-PURE exit then
   s" changed-new.f" NEW= if CHECK-CHANGED-RENAME exit then
   s" copy-pure.f" NEW= if CHECK-PURE-COPY exit then
   s" copy-changed.f" NEW= if CHECK-CHANGED-COPY exit then
   s" exec-new.f" NEW= if CHECK-RENAME-MODE exit then
   s" copy-mode.f" NEW= if CHECK-COPY-MODE exit then
   s" mix-new.f" NEW= if CHECK-RENAME-BODY-MODE exit then
   s" copy-body-mode.f" NEW= if CHECK-COPY-BODY-MODE exit then
   s" copy-source.f" NEW= if CHECK-COPY-SOURCE exit then
   WEIRD$ NEW= if CHECK-WEIRD exit then
   s" binary.f" NEW= if CHECK-BINARY exit then
   s" mode.f" NEW= if CHECK-MODE exit then
   s" empty-added.f" NEW= if CHECK-EMPTY exit then
   s" removed.f" OLD= if CHECK-REMOVED exit then
   s" sub" NEW= if CHECK-GITLINK exit then
   s" .gitmodules" NEW= if CHECK-MODULES exit then
   0 1 T= ;

: STEP? ( -- bool )
   DIFF:NEXT? {: a:ptr u:n meta:n kind:DIFF:event present:bool :}
   a drop u drop meta drop kind drop
   present 0= if false exit then
   DIFF:SECTION-INDEX LAST-SECTION @ <> if
      DIFF:SECTION-INDEX LAST-SECTION !
      CHECK-SECTION
   then
   true ;

: READ-ARTIFACT ( -- )
   ART-PATH$ FILE-SIZE {: u:n :}
   u CAP <= TTRUE
   u ART-U !
   ART-PATH$ ART u READ-ALL u T=
   ART u DIFF:OPEN
   DIFF:FROM$ nip 40 T=
   DIFF:TO$ nip 40 T=
   DIFF:SECTION-COUNT 16 T=
   -1 LAST-SECTION !
   begin STEP? while repeat
   SAW-PURE @ TTRUE
   SAW-CHANGED-RENAME @ TTRUE
   SAW-PURE-COPY @ TTRUE
   SAW-CHANGED-COPY @ TTRUE
   SAW-RENAME-MODE @ TTRUE
   SAW-COPY-MODE @ TTRUE
   SAW-RENAME-BODY-MODE @ TTRUE
   SAW-COPY-BODY-MODE @ TTRUE
   SAW-COPY-SOURCE @ TTRUE
   SAW-WEIRD @ TTRUE
   SAW-BINARY @ TTRUE
   SAW-MODE @ TTRUE
   SAW-EMPTY @ TTRUE
   SAW-REMOVED @ TTRUE
   SAW-GITLINK @ TTRUE
   SAW-MODULES @ TTRUE ;

: VALID-CAPTURE ( -- )
   DIFF-CMD:TEST-CONTENT-ON
   ROOT$ ART-PATH$ s" @--" s" @-" DIFF-CAPTURE:RUN-IN ;

: CAPTURE-CHECK ( -- )
   [: VALID-CAPTURE ;] catch {: code:n :}
   code 0= if exit then
   DIFF-CAPTURE:REPORT$ type cr
   code throw ;

: RESTORE-SEAMS ( n -- ) {: code:n :}
   DIFF-CMD:TEST-SEAMS-OFF
   DIFF-CONTENT:RESET
   code 0<> if code throw then ;

: HOOKED-CAPTURE ( -- )
   DIFF-CMD:TEST-SPAWN-WARN-ON
   DIFF-CMD:TEST-BARRIER-ON
   [: CAPTURE-CHECK ;] catch RESTORE-SEAMS ;

: CHECK-BYTE-REPORT ( -- )
   DIFF-CAPTURE:LAST-ERR$ {: a:ptr u:n :}
   u WARN-U T=
   a c@ $FF T=
   a 1+ c@ $80 T=
   DIFF-CAPTURE:REPORT$ JSON-PARSE {: root:n :}
   root s" command_present" JSON-GET JSON-BOOL@ TTRUE
   root s" stderr_encoding" JSON-GET JSON-STRING$ s" hex" T$=
   root s" stderr" JSON-GET JSON-STRING$ s" ff80225c0a" T$= ;


: MAIN ( -- )
   T-RESET
   PREPARE-ROOT
   JJ!
   GIT!
   JJ-INIT
   JJ-CONFIG
   GIT-ROOT-CONFIG
   BASE-FILES
   s" base" JJ-COMMIT
   CHANGE-FILES
   ADD-SUBMODULE
   GIT-COMMIT-CHANGES
   JJ-IMPORT
   ART-PATH$ s" prior" WRITE-ALL
   HOOKED-CAPTURE
   DIFF-CMD:TEST-WARN-COUNT 0 > TTRUE
   DIFF-CMD:TEST-BARRIER-COUNT 1 T=
   DIFF-CAPTURE:LAST-ERR$ nip 0 > TTRUE
   CHECK-BYTE-REPORT
   READ-ARTIFACT
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" diff-capture-test: ok" type cr ;

MAIN

;package
