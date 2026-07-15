\ diff-test.f - canonical event and fail-closed parser tests.

require lib/prelude.f
require lib/string.f
require lib/test.f
require tools/lint/diff.f

package DIFF-TEST
private

$0D constant CR-C
$09 constant TAB-C

: FORM# ( DIFF:form -- n )
   MATCH DIFF:form
      modify      OF 0 ENDOF
      add-file    OF 1 ENDOF
      delete-file OF 2 ENDOF
      rename      OF 3 ENDOF
      copy        OF 4 ENDOF
      mode        OF 5 ENDOF
      binary      OF 6 ENDOF
   ;MATCH ;

: SECTION# ( ptr u8 n ptr u8 n DIFF:form bool -- n )
   if FORM# 2 * 1+ else FORM# 2 * then
   >r 2drop 2drop r> 10 + ;

: EVENT# ( DIFF:event -- n )
   MATCH DIFF:event
      none OF 0 ENDOF
      section OF
         DIFF:SECTION-OLD$ DIFF:SECTION-NEW$
         DIFF:SECTION-FORM DIFF:SECTION-BODY? SECTION#
      ENDOF
      hunk OF 2 ENDOF
      add OF 3 ENDOF
      context OF 4 ENDOF
      delete OF 5 ENDOF
   ;MATCH ;

: EXPECT-NONE ( ptr u8 n -- )
   DIFF:LINE EVENT# 0 T= ;

: EXPECT-HUNK ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u DIFF:LINE MATCH DIFF:event
      none OF 0 1 T= ENDOF
      section OF 0 1 T= ENDOF
      hunk OF DIFF:HUNK-NEW-START want T= ENDOF
      add OF 0 1 T= ENDOF
      context OF 0 1 T= ENDOF
      delete OF 0 1 T= ENDOF
   ;MATCH ;

: EXPECT-CONTENT ( ptr u8 n n ptr u8 n -- )
   {: a:ptr u:n want-kind:n want:ptr wantu:n :}
   a u DIFF:LINE MATCH DIFF:event
      none OF 0 1 T= ENDOF
      section OF 0 1 T= ENDOF
      hunk OF 0 1 T= ENDOF
      add OF DIFF:CONTENT$ {: got:ptr gotu:n :}
         want-kind 3 T= got gotu want wantu T$=
      ENDOF
      context OF DIFF:CONTENT$ {: got:ptr gotu:n :}
         want-kind 4 T= got gotu want wantu T$=
      ENDOF
      delete OF DIFF:CONTENT$ {: got:ptr gotu:n :}
         want-kind 5 T= got gotu want wantu T$=
      ENDOF
   ;MATCH ;

: FINISH-NONE ( -- )
   DIFF:FINISH EVENT# 0 T= ;

: EXPECT-SECTION-TYPE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u DIFF:LINE EVENT# want T= ;

: FINISH-SECTION-TYPE ( n -- )
   DIFF:FINISH EVENT# swap T= ;

: MOD-A-HEAD ( -- )
   s" diff --git a/a.f b/a.f" EXPECT-NONE
   s" index 1234567..abcdef0 100644" EXPECT-NONE
   s" --- a/a.f" EXPECT-NONE
   s" +++ b/a.f" 11 EXPECT-SECTION-TYPE ;

: MOD-SPACE-PRE ( -- )
   s" diff --git a/same file.f b/same file.f" EXPECT-NONE
   s" index 1234567..abcdef0 100644" EXPECT-NONE
   s" --- a/same file.f" EXPECT-NONE ;

: MOD-SPACE-EVENT ( -- DIFF:event )
   MOD-SPACE-PRE
   s" +++ b/same file.f" DIFF:LINE ;

: MOD-SPACE-HEAD ( -- )
   MOD-SPACE-EVENT EVENT# 11 T= ;

: TEST-IDENTITIES ( -- )
   DIFF:RESET MOD-SPACE-EVENT EVENT# 11 T=
   DIFF:SECTION-OLD$ s" same file.f" T$=
   DIFF:RESET MOD-SPACE-EVENT EVENT# 11 T=
   DIFF:SECTION-NEW$ s" same file.f" T$= ;

: TEST-BACKSLASH-PATH ( -- )
   DIFF:RESET
   S\" diff --git a/back\\slash.f b/back\\slash.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   20 FINISH-SECTION-TYPE ;

: TEST-QUOTE-PATH ( -- )
   DIFF:RESET
   S\" diff --git a/quote\qname.f b/quote\qname.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   20 FINISH-SECTION-TYPE ;

: TEST-UTF8-PATH ( -- )
   DIFF:RESET
   s" diff --git a/café.f b/café.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   20 FINISH-SECTION-TYPE ;

: RUN-FAIL ( ptr u8 n n -- ) {: label:ptr labelu:n rc:n :}
   s" diff-test: " type label labelu type s"  threw " type rc .
   rc throw ;

\ typed-local-lint: allow-bare-local - q preserves the named test action effect.
: RUN-CASE ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   q catch {: rc:n :}
   rc 0= if exit then
   label labelu rc RUN-FAIL ;

: TEST-MODIFY ( -- )
   DIFF:RESET
   MOD-SPACE-HEAD
   s" @@ -1,2 +4,2 @@ WORD" 4 EXPECT-HUNK
   s" -old" 5 s" old" EXPECT-CONTENT
   s" +new" 3 s" new" EXPECT-CONTENT
   s"  same" 4 s" same" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-SPOOF-CONTENT ( -- )
   DIFF:RESET
   MOD-A-HEAD
   s" @@ -0,0 +1,2 @@" 1 EXPECT-HUNK
   s" ++++ b/spoof.f" 3 s" +++ b/spoof.f" EXPECT-CONTENT
   s" +--- a/spoof.f" 3 s" --- a/spoof.f" EXPECT-CONTENT
   FINISH-NONE ;

: ADD-HEAD ( -- )
   s" diff --git a/added.f b/added.f" EXPECT-NONE
   s" new file mode 100644" EXPECT-NONE
   s" index 0000000..abcdef0" EXPECT-NONE
   s" --- /dev/null" EXPECT-NONE
   s" +++ b/added.f" 13 EXPECT-SECTION-TYPE ;

: TEST-ADD ( -- )
   DIFF:RESET
   ADD-HEAD
   s" @@ -0,0 +1 @@" 1 EXPECT-HUNK
   s" +new" 3 s" new" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-DELETE ( -- )
   DIFF:RESET
   s" diff --git a/gone.f b/gone.f" EXPECT-NONE
   s" deleted file mode 100644" EXPECT-NONE
   s" index abcdef0..0000000" EXPECT-NONE
   s" --- a/gone.f" EXPECT-NONE
   s" +++ /dev/null" 15 EXPECT-SECTION-TYPE
   s" @@ -1 +0,0 @@" 0 EXPECT-HUNK
   s" -gone" 5 s" gone" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-RENAME ( -- )
   DIFF:RESET
   s" diff --git a/old name.f b/new name.f" EXPECT-NONE
   s" similarity index 100%" EXPECT-NONE
   s" rename from old name.f" EXPECT-NONE
   s" rename to new name.f" EXPECT-NONE
   16 FINISH-SECTION-TYPE ;

: TEST-RENAME-MODE ( -- )
   DIFF:RESET
   s" diff --git a/a.f b/b.f" EXPECT-NONE
   s" rename from a.f" EXPECT-NONE
   s" rename to b.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   16 FINISH-SECTION-TYPE ;

: TEST-RENAME-BODY ( -- )
   DIFF:RESET
   s" diff --git a/old.f b/new.f" EXPECT-NONE
   s" rename from old.f" EXPECT-NONE
   s" rename to new.f" EXPECT-NONE
   s" index 1234567..abcdef0 100644" EXPECT-NONE
   s" --- a/old.f" EXPECT-NONE
   s" +++ b/new.f" 17 EXPECT-SECTION-TYPE
   s" @@ -1 +1 @@" 1 EXPECT-HUNK
   s" -old" 5 s" old" EXPECT-CONTENT
   s" +new" 3 s" new" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-COPY ( -- )
   DIFF:RESET
   s" diff --git a/src.f b/dst.f" EXPECT-NONE
   s" similarity index 100%" EXPECT-NONE
   s" copy from src.f" EXPECT-NONE
   s" copy to dst.f" EXPECT-NONE
   18 FINISH-SECTION-TYPE ;

: TEST-MODE ( -- )
   DIFF:RESET
   s" diff --git a/tool.f b/tool.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   20 FINISH-SECTION-TYPE
   FINISH-NONE ;

: TEST-ADJACENT ( -- )
   DIFF:RESET
   s" diff --git a/tool.f b/tool.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   s" diff --git a/a.f b/a.f" 20 EXPECT-SECTION-TYPE
   s" index 1234567..abcdef0 100644" EXPECT-NONE
   s" --- a/a.f" EXPECT-NONE
   s" +++ b/a.f" 11 EXPECT-SECTION-TYPE
   s" @@ -1 +1 @@" 1 EXPECT-HUNK
   s" -old" 5 s" old" EXPECT-CONTENT
   s" +new" 3 s" new" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-MODE-BODY ( -- )
   DIFF:RESET
   s" diff --git a/tool.f b/tool.f" EXPECT-NONE
   s" old mode 100644" EXPECT-NONE
   s" new mode 100755" EXPECT-NONE
   s" index 1234567..abcdef0" EXPECT-NONE
   s" --- a/tool.f" EXPECT-NONE
   s" +++ b/tool.f" 21 EXPECT-SECTION-TYPE
   s" @@ -1 +1 @@" 1 EXPECT-HUNK
   s" -old" 5 s" old" EXPECT-CONTENT
   s" +new" 3 s" new" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-INSERT-HUNK ( -- )
   DIFF:RESET
   MOD-A-HEAD
   s" @@ -5,0 +6 @@" 6 EXPECT-HUNK
   s" +new" 3 s" new" EXPECT-CONTENT
   FINISH-NONE ;

: TEST-NO-LF-MARKERS ( -- )
   DIFF:RESET
   MOD-A-HEAD
   s" @@ -1 +1 @@" 1 EXPECT-HUNK
   s" -old" 5 s" old" EXPECT-CONTENT
   s" \ No newline at end of file" EXPECT-NONE
   s" +new" 3 s" new" EXPECT-CONTENT
   s" \ No newline at end of file" EXPECT-NONE
   FINISH-NONE ;

: TEST-SOURCE-LF ( -- )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND 10 SB-APPEND-C
   SB$ DIFF:SOURCE-VALIDATE ;

: TEST-BINARY ( -- )
   DIFF:RESET
   s" diff --git a/a.bin b/a.bin" EXPECT-NONE
   s" index 1234567..abcdef0 100644" EXPECT-NONE
   s" Binary files a/a.bin and b/a.bin differ" 22 EXPECT-SECTION-TYPE
   FINISH-NONE ;

: TEST-BINARY-ADD ( -- )
   DIFF:RESET
   s" diff --git a/new.bin b/new.bin" EXPECT-NONE
   s" new file mode 100644" EXPECT-NONE
   s" index 0000000..abcdef0" EXPECT-NONE
   s" Binary files /dev/null and b/new.bin differ" 22 EXPECT-SECTION-TYPE
   FINISH-NONE ;

: TEST-BINARY-DELETE ( -- )
   DIFF:RESET
   s" diff --git a/old.bin b/old.bin" EXPECT-NONE
   s" deleted file mode 100644" EXPECT-NONE
   s" index abcdef0..0000000" EXPECT-NONE
   s" Binary files a/old.bin and /dev/null differ" 22 EXPECT-SECTION-TYPE
   FINISH-NONE ;

: TEST-EMPTY-ADD ( -- )
   DIFF:RESET
   s" diff --git a/empty.f b/empty.f" EXPECT-NONE
   s" new file mode 100644" EXPECT-NONE
   s" index 0000000..e69de29" EXPECT-NONE
   12 FINISH-SECTION-TYPE ;

: DROP-EVENT ( DIFF:event -- )
   MATCH DIFF:event
      none OF ENDOF
      section OF ENDOF
      hunk OF ENDOF
      add OF ENDOF
      context OF ENDOF
      delete OF ENDOF
   ;MATCH ;

: BAD-FINISH ( -- )
   DIFF:FINISH DROP-EVENT ;

: REJECT-LINE ( ptr u8 n -- )
   DIFF:LINE DROP-EVENT ;

: EXPECT-REJECT ( [ -- ] -- )
   DIFF:E-SYNTAX TTHROWSQ ;

: BAD-HEAD-EOF ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE BAD-FINISH ;

: BAD-MODE ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" old mode 100688" REJECT-LINE ;

: BAD-DUP-INDEX ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" index 1234567..abcdef0 100644" REJECT-LINE
   s" index 1234567..abcdef0 100644" REJECT-LINE ;

: BAD-INDEX-MODE-MISSING ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" index 1234567..abcdef0" REJECT-LINE ;

: BAD-ADD-INDEX-MODE ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" new file mode 100644" REJECT-LINE
   s" index 0000000..abcdef0 100644" REJECT-LINE ;

: BAD-DUP-MODE ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" old mode 100644" REJECT-LINE
   s" new mode 100755" REJECT-LINE
   s" old mode 100755" REJECT-LINE ;

: BAD-DUP-RENAME ( -- )
   DIFF:RESET s" diff --git a/a.f b/b.f" REJECT-LINE
   s" similarity index 100%" REJECT-LINE
   s" rename from a.f" REJECT-LINE
   s" rename to b.f" REJECT-LINE
   s" rename from a.f" REJECT-LINE ;

: BAD-SAME-RENAME ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" rename from a.f" REJECT-LINE
   s" rename to a.f" REJECT-LINE ;

: BAD-ADD-RENAME ( -- )
   DIFF:RESET s" diff --git a/a.f b/a.f" REJECT-LINE
   s" new file mode 100644" REJECT-LINE
   s" rename from a.f" REJECT-LINE ;

: BAD-SIM-PLUS ( -- )
   DIFF:RESET s" diff --git a/a.f b/b.f" REJECT-LINE
   s" similarity index +1%" REJECT-LINE ;

: BAD-SIM-ZERO ( -- )
   DIFF:RESET s" diff --git a/a.f b/b.f" REJECT-LINE
   s" similarity index 01%" REJECT-LINE ;

: BAD-SIM-RANGE ( -- )
   DIFF:RESET s" diff --git a/a.f b/b.f" REJECT-LINE
   s" similarity index 101%" REJECT-LINE ;

: BAD-HUNK-COUNT ( -- )
   DIFF:RESET MOD-A-HEAD
   s" @@ -1,2 +1,2 @@" REJECT-LINE
   s" -one" REJECT-LINE
   BAD-FINISH ;

: BAD-HUNK-RANGE ( -- )
   DIFF:RESET MOD-A-HEAD
   s" @@ -0 +1 @@" REJECT-LINE ;

: BAD-HUNK-OVERLAP ( -- )
   DIFF:RESET MOD-A-HEAD
   s" @@ -1 +1 @@" REJECT-LINE
   s" -old" REJECT-LINE
   s" +new" REJECT-LINE
   s" @@ -1 +1 @@" REJECT-LINE ;

: BAD-ADD-HUNK-SIDES ( -- )
   DIFF:RESET ADD-HEAD
   s" @@ -1 +1 @@" REJECT-LINE ;

: BAD-EMPTY-ADD-BODY ( -- )
   DIFF:RESET
   s" diff --git a/empty.f b/empty.f" REJECT-LINE
   s" new file mode 100644" REJECT-LINE
   s" index 0000000..e69de29" REJECT-LINE
   s" --- /dev/null" REJECT-LINE ;

: BAD-EARLY-MARKER ( -- )
   DIFF:RESET MOD-A-HEAD
   s" @@ -1,2 +1,2 @@" REJECT-LINE
   s" -old" REJECT-LINE
   s" \ No newline at end of file" REJECT-LINE ;

: BAD-SOURCE-LF ( -- )
   s" diff --git a/a.f b/a.f" DIFF:SOURCE-VALIDATE ;

: BAD-QUOTE ( -- )
   DIFF:RESET S\" diff --git \q a/a.f\q  \q b/a.f\q " REJECT-LINE ;

: BAD-PARENT ( -- )
   DIFF:RESET s" diff --git a/../a.f b/../a.f" REJECT-LINE
   s" old mode 100644" REJECT-LINE
   s" new mode 100755" REJECT-LINE
   BAD-FINISH ;

: BAD-CRLF ( -- )
   DIFF:RESET
   SB-RESET s" diff --git a/a.f b/a.f" SB-APPEND CR-C SB-APPEND-C
   SB$ REJECT-LINE ;

: BAD-CONTROL ( -- )
   DIFF:RESET
   SB-RESET s" diff --git a/a" SB-APPEND TAB-C SB-APPEND-C
   s" x.f b/a" SB-APPEND TAB-C SB-APPEND-C s" x.f" SB-APPEND
   SB$ REJECT-LINE ;

: BAD-BINARY-PATCH ( -- )
   DIFF:RESET s" diff --git a/a.bin b/a.bin" REJECT-LINE
   s" index 1234567..abcdef0 100644" REJECT-LINE
   s" GIT binary patch" REJECT-LINE ;

: TEST-REJECTS ( -- )
   [: BAD-HEAD-EOF ;] EXPECT-REJECT
   [: BAD-MODE ;] EXPECT-REJECT
   [: BAD-DUP-INDEX ;] EXPECT-REJECT
   [: BAD-INDEX-MODE-MISSING ;] EXPECT-REJECT
   [: BAD-ADD-INDEX-MODE ;] EXPECT-REJECT
   [: BAD-DUP-MODE ;] EXPECT-REJECT
   [: BAD-DUP-RENAME ;] EXPECT-REJECT
   [: BAD-SAME-RENAME ;] EXPECT-REJECT
   [: BAD-ADD-RENAME ;] EXPECT-REJECT
   [: BAD-SIM-PLUS ;] EXPECT-REJECT
   [: BAD-SIM-ZERO ;] EXPECT-REJECT
   [: BAD-SIM-RANGE ;] EXPECT-REJECT
   [: BAD-HUNK-COUNT ;] EXPECT-REJECT
   [: BAD-HUNK-RANGE ;] EXPECT-REJECT
   [: BAD-HUNK-OVERLAP ;] EXPECT-REJECT
   [: BAD-ADD-HUNK-SIDES ;] EXPECT-REJECT
   [: BAD-EMPTY-ADD-BODY ;] EXPECT-REJECT
   [: BAD-EARLY-MARKER ;] EXPECT-REJECT
   [: BAD-SOURCE-LF ;] EXPECT-REJECT
   [: BAD-QUOTE ;] EXPECT-REJECT
   [: BAD-PARENT ;] EXPECT-REJECT
   [: BAD-CRLF ;] EXPECT-REJECT
   [: BAD-CONTROL ;] EXPECT-REJECT
   [: BAD-BINARY-PATCH ;] EXPECT-REJECT ;

: RUN ( -- )
   T-RESET
   s" modify" [: TEST-MODIFY ;] RUN-CASE
   s" identities" [: TEST-IDENTITIES ;] RUN-CASE
   s" backslash-path" [: TEST-BACKSLASH-PATH ;] RUN-CASE
   s" quote-path" [: TEST-QUOTE-PATH ;] RUN-CASE
   s" utf8-path" [: TEST-UTF8-PATH ;] RUN-CASE
   s" spoof" [: TEST-SPOOF-CONTENT ;] RUN-CASE
   s" add" [: TEST-ADD ;] RUN-CASE
   s" delete" [: TEST-DELETE ;] RUN-CASE
   s" rename" [: TEST-RENAME ;] RUN-CASE
   s" rename-mode" [: TEST-RENAME-MODE ;] RUN-CASE
   s" rename-body" [: TEST-RENAME-BODY ;] RUN-CASE
   s" copy" [: TEST-COPY ;] RUN-CASE
   s" mode" [: TEST-MODE ;] RUN-CASE
   s" adjacent" [: TEST-ADJACENT ;] RUN-CASE
   s" mode-body" [: TEST-MODE-BODY ;] RUN-CASE
   s" insert-hunk" [: TEST-INSERT-HUNK ;] RUN-CASE
   s" no-lf-markers" [: TEST-NO-LF-MARKERS ;] RUN-CASE
   s" source-lf" [: TEST-SOURCE-LF ;] RUN-CASE
   s" binary" [: TEST-BINARY ;] RUN-CASE
   s" binary-add" [: TEST-BINARY-ADD ;] RUN-CASE
   s" binary-delete" [: TEST-BINARY-DELETE ;] RUN-CASE
   s" empty-add" [: TEST-EMPTY-ADD ;] RUN-CASE
   s" rejects" [: TEST-REJECTS ;] RUN-CASE
   T-REPORT ;

RUN

;package
