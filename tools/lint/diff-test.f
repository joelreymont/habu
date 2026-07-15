\ diff-test.f - focused event and fail-closed tests for tools/lint/diff.f.
\ Run: bin/hb --load tools/lint/diff-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require tools/lint/diff.f

package DIFF
private

128 constant CR-CAP
$0D constant CR-B

create CR-HEAD CR-CAP allot
create CR-PATH CR-CAP allot
variable CR-HEAD-U
variable CR-PATH-U

: SAVE$ ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PREP-CR ( -- )
   SB-RESET
   s" diff --git a/file" SB-APPEND CR-B SB-APPEND-C
   s"  b/file" SB-APPEND CR-B SB-APPEND-C
   SB$ CR-HEAD CR-HEAD-U SAVE$
   SB-RESET
   s" file" SB-APPEND CR-B SB-APPEND-C
   SB$ CR-PATH CR-PATH-U SAVE$ ;

: EVENT# ( DIFF:event -- n )
   MATCH DIFF:event
      none    OF 0 ENDOF
      file    OF 1 ENDOF
      hunk    OF 2 ENDOF
      add     OF 3 ENDOF
      context OF 4 ENDOF
      delete  OF 5 ENDOF
   ;MATCH ;

: EXPECT ( ptr u8 n n ptr u8 n n -- )
   {: line:ptr lineu:n kind:n want:ptr wantu:n meta:n :}
   line lineu RAW-LINE EVENT# {: got:ptr gotu:n gotmeta:n gotkind:n :}
   gotkind kind T=
   got gotu want wantu T$=
   gotmeta meta T= ;

: EXPECT-NONE ( ptr u8 n -- )
   0 s" " 0 EXPECT ;

: DROP-LINE ( ptr u8 n -- )
   RAW-LINE drop drop 2drop ;

: HEAD ( -- )
   s" diff --git a/a.f b/a.f" EXPECT-NONE
   s" index 123..456 100644" EXPECT-NONE
   s" --- a/a.f" 1 s" a.f" 0 EXPECT
   s" +++ b/a.f" 1 s" a.f" 0 EXPECT ;

: TEST-EVENTS ( -- )
   RAW-RESET
   HEAD
   s" @@ -1,2 +4,2 @@ WORD" 2 s" @@ -1,2 +4,2 @@ WORD" 4 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   s"  same" 4 s" same" 0 EXPECT
   s" @@ -9 +9 @@" 2 s" @@ -9 +9 @@" 9 EXPECT
   s" -old-next" 5 s" old-next" 0 EXPECT
   s" +new-next" 3 s" new-next" 0 EXPECT
   s" \ No newline at end of file" EXPECT-NONE
   s" diff --git a/b.f b/b.f" EXPECT-NONE
   s" index 123..456 100644" EXPECT-NONE
   s" --- a/b.f" 1 s" b.f" 0 EXPECT
   s" +++ b/b.f" 1 s" b.f" 0 EXPECT
   s" @@ -0,0 +1 @@" 2 s" @@ -0,0 +1 @@" 1 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   RAW-FINISH ;

: TEST-EMPTY ( -- )
   RAW-RESET
   RAW-FINISH ;

: TEST-DELETE-FILE ( -- )
   RAW-RESET
   s" diff --git a/gone.f b/gone.f" EXPECT-NONE
   s" deleted file mode 100644" EXPECT-NONE
   s" index abcdef1234..0000000000" EXPECT-NONE
   s" --- a/gone.f" 1 s" gone.f" 0 EXPECT
   s" +++ /dev/null" EXPECT-NONE
   s" @@ -1 +0,0 @@" 2 s" @@ -1 +0,0 @@" 0 EXPECT
   s" -gone" 5 s" gone" 0 EXPECT
   RAW-FINISH ;

: TEST-MID-MARKERS ( -- )
   RAW-RESET
   HEAD
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" \ No newline at end of file" EXPECT-NONE
   s" +new" 3 s" new" 0 EXPECT
   s" \ No newline at end of file" EXPECT-NONE
   RAW-FINISH ;

: TEST-METADATA ( -- )
   RAW-RESET
   s" diff --git a/old.f b/new.f" EXPECT-NONE
   s" similarity index 100%" EXPECT-NONE
   s" rename from old.f" 1 s" old.f" 0 EXPECT
   s" rename to new.f" 1 s" new.f" 0 EXPECT
   s" diff --git a/src.f b/dst.f" EXPECT-NONE
   s" similarity index 100%" EXPECT-NONE
   s" copy from src.f" 1 s" src.f" 0 EXPECT
   s" copy to dst.f" 1 s" dst.f" 0 EXPECT
   s" diff --git a/tool.f b/tool.f" EXPECT-NONE
   s" old mode 100644" 1 s" tool.f" 0 EXPECT
   s" new mode 100755" 1 s" tool.f" 0 EXPECT
   s" diff --git a/old.sh b/new.sh" EXPECT-NONE
   s" similarity index 100%" EXPECT-NONE
   s" rename from old.sh" 1 s" old.sh" 0 EXPECT
   s" rename to new.sh" 1 s" new.sh" 0 EXPECT
   s" old mode 100644" 1 s" old.sh" 0 EXPECT
   s" new mode 100755" 1 s" new.sh" 0 EXPECT
   RAW-FINISH ;

: TEST-EMPTY-METADATA ( -- )
   RAW-RESET
   s" diff --git a/new.f b/new.f" EXPECT-NONE
   s" new file mode 100644" EXPECT-NONE
   s" index 0000000000..e69de29bb2" 1 s" new.f" 0 EXPECT
   s" diff --git a/old.f b/old.f" EXPECT-NONE
   s" deleted file mode 100644" EXPECT-NONE
   s" index e69de29bb2..0000000000" 1 s" old.f" 0 EXPECT
   RAW-FINISH ;

: TEST-BINARY ( -- )
   RAW-RESET
   s" diff --git a/a.bin b/a.bin" EXPECT-NONE
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" Binary files a/a.bin and b/a.bin differ" 1 s" a.bin" 0 EXPECT
   s" diff --git a/new.bin b/new.bin" EXPECT-NONE
   s" new file mode 100644" EXPECT-NONE
   s" index 0000000000..abcdef1234" EXPECT-NONE
   s" Binary files /dev/null and b/new.bin differ" 1 s" new.bin" 0 EXPECT
   s" diff --git a/old.bin b/old.bin" EXPECT-NONE
   s" deleted file mode 100644" EXPECT-NONE
   s" index abcdef1234..0000000000" EXPECT-NONE
   s" Binary files a/old.bin and /dev/null differ" 1 s" old.bin" 0 EXPECT
   s" diff --git a/x and y.bin b/x and y.bin" EXPECT-NONE
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" Binary files a/x and y.bin and b/x and y.bin differ" 1 s" x and y.bin" 0 EXPECT
   RAW-FINISH ;

: TEST-SPACES ( -- )
   RAW-RESET
   s" diff --git a/same file.f b/same file.f" EXPECT-NONE
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" --- a/same file.f" 1 s" same file.f" 0 EXPECT
   s" +++ b/same file.f" 1 s" same file.f" 0 EXPECT
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   s" diff --git a/old file.f b/new file.f" EXPECT-NONE
   s" rename from old file.f" 1 s" old file.f" 0 EXPECT
   s" rename to new file.f" 1 s" new file.f" 0 EXPECT
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" --- a/old file.f" 1 s" old file.f" 0 EXPECT
   s" +++ b/new file.f" 1 s" new file.f" 0 EXPECT
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   s" diff --git a/alpha b/beta.txt b/gamma b/delta.txt" EXPECT-NONE
   s" rename from alpha b/beta.txt" 1 s" alpha b/beta.txt" 0 EXPECT
   s" rename to gamma b/delta.txt" 1 s" gamma b/delta.txt" 0 EXPECT
   s" diff --git a/alpha b/beta.txt b/alpha b/beta.txt" EXPECT-NONE
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" --- a/alpha b/beta.txt" 1 s" alpha b/beta.txt" 0 EXPECT
   s" +++ b/alpha b/beta.txt" 1 s" alpha b/beta.txt" 0 EXPECT
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   s" diff --git a/alpha b/beta.txt b/alpha b/beta.txt" EXPECT-NONE
   s" old mode 100644" 1 s" alpha b/beta.txt" 0 EXPECT
   s" new mode 100755" 1 s" alpha b/beta.txt" 0 EXPECT
   RAW-FINISH ;

: TEST-MODE-CONTENT ( -- )
   RAW-RESET
   s" diff --git a/mode file.f b/mode file.f" EXPECT-NONE
   s" old mode 100644" 1 s" mode file.f" 0 EXPECT
   s" new mode 100755" 1 s" mode file.f" 0 EXPECT
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" --- a/mode file.f" 1 s" mode file.f" 0 EXPECT
   s" +++ b/mode file.f" 1 s" mode file.f" 0 EXPECT
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   RAW-FINISH ;

: TEST-CR-PATH ( -- )
   PREP-CR
   RAW-RESET
   CR-HEAD CR-HEAD-U @ EXPECT-NONE
   s" old mode 100644" 1 CR-PATH CR-PATH-U @ 0 EXPECT
   s" new mode 100755" 1 CR-PATH CR-PATH-U @ 0 EXPECT
   RAW-FINISH ;

: TEST-DISSIMILARITY ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" EXPECT-NONE
   s" dissimilarity index 80%" EXPECT-NONE
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" --- a/a.f" 1 s" a.f" 0 EXPECT
   s" +++ b/a.f" 1 s" a.f" 0 EXPECT
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   RAW-FINISH ;

: TEST-MODIFIED-RENAME ( -- )
   RAW-RESET
   s" diff --git a/old.f b/new.f" EXPECT-NONE
   s" similarity index 50%" EXPECT-NONE
   s" rename from old.f" 1 s" old.f" 0 EXPECT
   s" rename to new.f" 1 s" new.f" 0 EXPECT
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" --- a/old.f" 1 s" old.f" 0 EXPECT
   s" +++ b/new.f" 1 s" new.f" 0 EXPECT
   s" @@ -1 +1 @@" 2 s" @@ -1 +1 @@" 1 EXPECT
   s" -old" 5 s" old" 0 EXPECT
   s" +new" 3 s" new" 0 EXPECT
   RAW-FINISH ;

: TEST-MODIFIED-COPY ( -- )
   RAW-RESET
   s" diff --git a/src.bin b/dst.bin" EXPECT-NONE
   s" similarity index 50%" EXPECT-NONE
   s" copy from src.bin" 1 s" src.bin" 0 EXPECT
   s" copy to dst.bin" 1 s" dst.bin" 0 EXPECT
   s" index 1234567890..abcdef1234 100644" EXPECT-NONE
   s" Binary files a/src.bin and b/dst.bin differ" 1 s" dst.bin" 0 EXPECT
   RAW-FINISH ;

: BAD-HUNK-FIRST ( -- )
   RAW-RESET
   s" @@ -1 +1 @@" DROP-LINE ;

: BAD-EMPTY-HEAD ( -- )
   RAW-RESET
   s" diff --git " DROP-LINE ;

: BAD-ADD-FIRST ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" +outside" DROP-LINE ;

: BAD-RANGE ( -- )
   RAW-RESET
   HEAD
   s" @@ -1,x +1 @@" DROP-LINE ;

: BAD-OLD-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" --- a/a.f" DROP-LINE
   RAW-FINISH ;

: BAD-HEAD-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   RAW-FINISH ;

: BAD-HUNK-EOF ( -- )
   RAW-RESET
   HEAD
   s" @@ -0,0 +1,2 @@" DROP-LINE
   s" +one" DROP-LINE
   RAW-FINISH ;

: BAD-NEXT-FILE ( -- )
   RAW-RESET
   HEAD
   s" @@ -0,0 +1,2 @@" DROP-LINE
   s" +one" DROP-LINE
   s" diff --git a/b.f b/b.f" DROP-LINE ;

: BAD-EXTRA-ADD ( -- )
   RAW-RESET
   HEAD
   s" @@ -0,0 +1 @@" DROP-LINE
   s" +one" DROP-LINE
   s" +two" DROP-LINE ;

: BAD-HEADER-SPOOF ( -- )
   RAW-RESET
   HEAD
   s" @@ -0,0 +1 @@" DROP-LINE
   s" +safe" DROP-LINE
   s" +++ b/tools/ptx/perf-rows.tsv" DROP-LINE ;

: BAD-MARKER-FIRST ( -- )
   RAW-RESET
   HEAD
   s" @@ -1 +1 @@" DROP-LINE
   s" \ No newline at end of file" DROP-LINE ;

: BAD-MARKER-TWICE ( -- )
   RAW-RESET
   HEAD
   s" @@ -0,0 +1 @@" DROP-LINE
   s" +line" DROP-LINE
   s" \ No newline at end of file" DROP-LINE
   s" \ No newline at end of file" DROP-LINE ;

: BAD-OLD-SPOOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 123..456 100644" DROP-LINE
   s" --- a/spoof.f" DROP-LINE
   s" +++ b/a.f" DROP-LINE ;

: BAD-NEW-SPOOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 123..456 100644" DROP-LINE
   s" --- a/a.f" DROP-LINE
   s" +++ b/spoof.f" DROP-LINE ;

: BAD-RENAME-FROM ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" similarity index 100%" DROP-LINE
   s" rename from spoof.f" DROP-LINE
   s" rename to b.f" DROP-LINE ;

: BAD-RENAME-TO ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" rename from a.f" DROP-LINE
   s" rename to spoof.f" DROP-LINE ;

: BAD-COPY-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" copy from a.f" DROP-LINE
   RAW-FINISH ;

: BAD-RENAME-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" rename from a.f" DROP-LINE
   RAW-FINISH ;

: BAD-MODE-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" old mode 100644" DROP-LINE
   RAW-FINISH ;

: BAD-NULL-OLD ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 123..456 100644" DROP-LINE
   s" --- /dev/null" DROP-LINE ;

: BAD-NULL-NEW ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 123..456 100644" DROP-LINE
   s" --- a/a.f" DROP-LINE
   s" +++ /dev/null" DROP-LINE ;

: BAD-NESTED-HEAD ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" rename from a.f" DROP-LINE
   s" diff --git a/c.f b/c.f" DROP-LINE ;

: BAD-REPLACEMENT-HEAD ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" rename from a.f" DROP-LINE
   s" rename to b.f" DROP-LINE
   s" diff --git a/c.f b/" DROP-LINE ;

: BAD-BINARY-SPOOF ( -- )
   RAW-RESET
   s" diff --git a/a.bin b/a.bin" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" Binary files a/spoof.bin and b/a.bin differ" DROP-LINE ;

: BAD-ZERO-HUNK ( -- )
   RAW-RESET
   HEAD
   s" @@ -0,0 +0,0 @@" DROP-LINE ;

: BAD-INDEX ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index not-a-hash" DROP-LINE ;

: BAD-INDEX-BAD-MODE ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 1234567890..abcdef1234 100688" DROP-LINE ;

: BAD-CHANGED-TEXT ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" index 123..456 100644" DROP-LINE
   s" --- a/a.f" DROP-LINE
   s" +++ b/b.f" DROP-LINE ;

: BAD-CHANGED-MODE ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" old mode 100644" DROP-LINE
   s" new mode 100755" DROP-LINE
   RAW-FINISH ;

: BAD-CHANGED-BINARY ( -- )
   RAW-RESET
   s" diff --git a/a.bin b/b.bin" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" Binary files a/a.bin and b/b.bin differ" DROP-LINE ;

: BAD-SIMILARITY ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" similarity index 101%" DROP-LINE ;

: BAD-NEW-FILE-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" new file mode 100644" DROP-LINE
   RAW-FINISH ;

: BAD-DELETE-FILE-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" deleted file mode 100644" DROP-LINE
   RAW-FINISH ;

: BAD-SPACE-TEXT-SPOOF ( -- )
   RAW-RESET
   s" diff --git a/same file.f b/same file.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" --- a/spoof file.f" DROP-LINE
   s" +++ b/same file.f" DROP-LINE ;

: BAD-SPACE-RENAME-SPOOF ( -- )
   RAW-RESET
   s" diff --git a/old file.f b/new file.f" DROP-LINE
   s" rename from spoof file.f" DROP-LINE
   s" rename to new file.f" DROP-LINE ;

: BAD-AMBIGUOUS-MODE ( -- )
   RAW-RESET
   s" diff --git a/alpha b/beta.txt b/gamma b/delta.txt" DROP-LINE
   s" old mode 100644" DROP-LINE ;

: BAD-INDEX-MODE ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" old mode 100644" DROP-LINE ;

: BAD-INDEX-RENAME ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" rename from a.f" DROP-LINE ;

: BAD-DUP-INDEX ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE ;

: BAD-DUP-MODE ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" old mode 100644" DROP-LINE
   s" new mode 100755" DROP-LINE
   s" old mode 100755" DROP-LINE ;

: BAD-DUP-RENAME ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" rename from a.f" DROP-LINE
   s" rename to b.f" DROP-LINE
   s" rename from a.f" DROP-LINE ;

: BAD-DUP-SIMILARITY ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" similarity index 100%" DROP-LINE
   s" similarity index 100%" DROP-LINE ;

: BAD-BINARY-TEXT ( -- )
   RAW-RESET
   s" diff --git a/a.bin b/a.bin" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" Binary files a/a.bin and b/a.bin differ" DROP-LINE
   s" --- a/a.bin" DROP-LINE ;

: BAD-INDEX-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   RAW-FINISH ;

: BAD-TEXT-NO-INDEX ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" --- a/a.f" DROP-LINE ;

: BAD-MODE-NONOCTAL ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" old mode 100688" DROP-LINE ;

: BAD-MODE-SAME ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" old mode 100644" DROP-LINE
   s" new mode 100644" DROP-LINE ;

: BAD-DUP-DISSIMILARITY ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" dissimilarity index 80%" DROP-LINE
   s" dissimilarity index 80%" DROP-LINE ;

: BAD-TEXT-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" --- a/a.f" DROP-LINE
   s" +++ b/a.f" DROP-LINE
   RAW-FINISH ;

: BAD-TEXT-NEXT ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" --- a/a.f" DROP-LINE
   s" +++ b/a.f" DROP-LINE
   s" diff --git a/b.f b/b.f" DROP-LINE ;

: BAD-CONTEXT-HUNK ( -- )
   RAW-RESET
   HEAD
   s" @@ -1 +1 @@" DROP-LINE
   s"  same" DROP-LINE ;

: BAD-BINARY-AND-SPOOF ( -- )
   RAW-RESET
   s" diff --git a/x and y.bin b/x and y.bin" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   s" Binary files a/x and spoof.bin and b/x and y.bin differ" DROP-LINE ;

: BAD-LF-PATH ( -- )
   RAW-RESET
   s" diff --git a/line" DROP-LINE ;

: BAD-SIMILARITY-INDEX ( -- )
   RAW-RESET
   s" diff --git a/a.f b/b.f" DROP-LINE
   s" similarity index 80%" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE ;

: BAD-PARTIAL-RENAME-EOF ( -- )
   RAW-RESET
   s" diff --git a/old.f b/new.f" DROP-LINE
   s" similarity index 50%" DROP-LINE
   s" rename from old.f" DROP-LINE
   s" rename to new.f" DROP-LINE
   RAW-FINISH ;

: BAD-PARTIAL-COPY-EOF ( -- )
   RAW-RESET
   s" diff --git a/src.f b/dst.f" DROP-LINE
   s" similarity index 50%" DROP-LINE
   s" copy from src.f" DROP-LINE
   s" copy to dst.f" DROP-LINE
   RAW-FINISH ;

: BAD-PARTIAL-RENAME-NEXT ( -- )
   RAW-RESET
   s" diff --git a/old.f b/new.f" DROP-LINE
   s" similarity index 50%" DROP-LINE
   s" rename from old.f" DROP-LINE
   s" rename to new.f" DROP-LINE
   s" diff --git a/a.f b/a.f" DROP-LINE ;

: BAD-PARTIAL-COPY-NEXT ( -- )
   RAW-RESET
   s" diff --git a/src.f b/dst.f" DROP-LINE
   s" similarity index 50%" DROP-LINE
   s" copy from src.f" DROP-LINE
   s" copy to dst.f" DROP-LINE
   s" diff --git a/a.f b/a.f" DROP-LINE ;

: BAD-DISSIMILARITY-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" dissimilarity index 80%" DROP-LINE
   RAW-FINISH ;

: BAD-DISSIMILARITY-INDEX-EOF ( -- )
   RAW-RESET
   s" diff --git a/a.f b/a.f" DROP-LINE
   s" dissimilarity index 80%" DROP-LINE
   s" index 1234567890..abcdef1234 100644" DROP-LINE
   RAW-FINISH ;

: REJECT ( [ -- ] ptr u8 n -- )
   T-LABEL
   E-DIFF-SYNTAX TTHROWSQ ;

: TEST-REJECTIONS ( -- )
   [: BAD-HUNK-FIRST ;] s" BAD-HUNK-FIRST" REJECT
   [: BAD-EMPTY-HEAD ;] s" BAD-EMPTY-HEAD" REJECT
   [: BAD-ADD-FIRST ;] s" BAD-ADD-FIRST" REJECT
   [: BAD-RANGE ;] s" BAD-RANGE" REJECT
   [: BAD-HEAD-EOF ;] s" BAD-HEAD-EOF" REJECT
   [: BAD-OLD-EOF ;] s" BAD-OLD-EOF" REJECT
   [: BAD-HUNK-EOF ;] s" BAD-HUNK-EOF" REJECT
   [: BAD-NEXT-FILE ;] s" BAD-NEXT-FILE" REJECT
   [: BAD-EXTRA-ADD ;] s" BAD-EXTRA-ADD" REJECT
   [: BAD-HEADER-SPOOF ;] s" BAD-HEADER-SPOOF" REJECT
   [: BAD-MARKER-FIRST ;] s" BAD-MARKER-FIRST" REJECT
   [: BAD-MARKER-TWICE ;] s" BAD-MARKER-TWICE" REJECT
   [: BAD-OLD-SPOOF ;] s" BAD-OLD-SPOOF" REJECT
   [: BAD-NEW-SPOOF ;] s" BAD-NEW-SPOOF" REJECT
   [: BAD-RENAME-FROM ;] s" BAD-RENAME-FROM" REJECT
   [: BAD-RENAME-TO ;] s" BAD-RENAME-TO" REJECT
   [: BAD-COPY-EOF ;] s" BAD-COPY-EOF" REJECT
   [: BAD-RENAME-EOF ;] s" BAD-RENAME-EOF" REJECT
   [: BAD-MODE-EOF ;] s" BAD-MODE-EOF" REJECT
   [: BAD-NULL-OLD ;] s" BAD-NULL-OLD" REJECT
   [: BAD-NULL-NEW ;] s" BAD-NULL-NEW" REJECT
   [: BAD-NESTED-HEAD ;] s" BAD-NESTED-HEAD" REJECT
   [: BAD-REPLACEMENT-HEAD ;] s" BAD-REPLACEMENT-HEAD" REJECT
   [: BAD-BINARY-SPOOF ;] s" BAD-BINARY-SPOOF" REJECT
   [: BAD-ZERO-HUNK ;] s" BAD-ZERO-HUNK" REJECT
   [: BAD-INDEX ;] s" BAD-INDEX" REJECT
   [: BAD-INDEX-BAD-MODE ;] s" BAD-INDEX-BAD-MODE" REJECT
   [: BAD-CHANGED-TEXT ;] s" BAD-CHANGED-TEXT" REJECT
   [: BAD-CHANGED-MODE ;] s" BAD-CHANGED-MODE" REJECT
   [: BAD-CHANGED-BINARY ;] s" BAD-CHANGED-BINARY" REJECT
   [: BAD-SIMILARITY ;] s" BAD-SIMILARITY" REJECT
   [: BAD-NEW-FILE-EOF ;] s" BAD-NEW-FILE-EOF" REJECT
   [: BAD-DELETE-FILE-EOF ;] s" BAD-DELETE-FILE-EOF" REJECT
   [: BAD-SPACE-TEXT-SPOOF ;] s" BAD-SPACE-TEXT-SPOOF" REJECT
   [: BAD-SPACE-RENAME-SPOOF ;] s" BAD-SPACE-RENAME-SPOOF" REJECT
   [: BAD-AMBIGUOUS-MODE ;] s" BAD-AMBIGUOUS-MODE" REJECT
   [: BAD-INDEX-MODE ;] s" BAD-INDEX-MODE" REJECT
   [: BAD-INDEX-RENAME ;] s" BAD-INDEX-RENAME" REJECT
   [: BAD-DUP-INDEX ;] s" BAD-DUP-INDEX" REJECT
   [: BAD-DUP-MODE ;] s" BAD-DUP-MODE" REJECT
   [: BAD-DUP-RENAME ;] s" BAD-DUP-RENAME" REJECT
   [: BAD-DUP-SIMILARITY ;] s" BAD-DUP-SIMILARITY" REJECT
   [: BAD-BINARY-TEXT ;] s" BAD-BINARY-TEXT" REJECT
   [: BAD-INDEX-EOF ;] s" BAD-INDEX-EOF" REJECT
   [: BAD-TEXT-NO-INDEX ;] s" BAD-TEXT-NO-INDEX" REJECT
   [: BAD-MODE-NONOCTAL ;] s" BAD-MODE-NONOCTAL" REJECT
   [: BAD-MODE-SAME ;] s" BAD-MODE-SAME" REJECT
   [: BAD-DUP-DISSIMILARITY ;] s" BAD-DUP-DISSIMILARITY" REJECT
   [: BAD-TEXT-EOF ;] s" BAD-TEXT-EOF" REJECT
   [: BAD-TEXT-NEXT ;] s" BAD-TEXT-NEXT" REJECT
   [: BAD-CONTEXT-HUNK ;] s" BAD-CONTEXT-HUNK" REJECT
   [: BAD-BINARY-AND-SPOOF ;] s" BAD-BINARY-AND-SPOOF" REJECT
   [: BAD-LF-PATH ;] s" BAD-LF-PATH" REJECT
   [: BAD-SIMILARITY-INDEX ;] s" BAD-SIMILARITY-INDEX" REJECT
   [: BAD-PARTIAL-RENAME-EOF ;] s" BAD-PARTIAL-RENAME-EOF" REJECT
   [: BAD-PARTIAL-COPY-EOF ;] s" BAD-PARTIAL-COPY-EOF" REJECT
   [: BAD-PARTIAL-RENAME-NEXT ;] s" BAD-PARTIAL-RENAME-NEXT" REJECT
   [: BAD-PARTIAL-COPY-NEXT ;] s" BAD-PARTIAL-COPY-NEXT" REJECT
   [: BAD-DISSIMILARITY-EOF ;] s" BAD-DISSIMILARITY-EOF" REJECT
   [: BAD-DISSIMILARITY-INDEX-EOF ;]
      s" BAD-DISSIMILARITY-INDEX-EOF" REJECT ;

: RUN ( -- )
   T-RESET
   [: TEST-EMPTY ;] catch s" TEST-EMPTY" T-LABEL 0 T=
   [: TEST-EVENTS ;] catch s" TEST-EVENTS" T-LABEL 0 T=
   [: TEST-DELETE-FILE ;] catch s" TEST-DELETE-FILE" T-LABEL 0 T=
   [: TEST-MID-MARKERS ;] catch s" TEST-MID-MARKERS" T-LABEL 0 T=
   [: TEST-METADATA ;] catch s" TEST-METADATA" T-LABEL 0 T=
   [: TEST-EMPTY-METADATA ;] catch s" TEST-EMPTY-METADATA" T-LABEL 0 T=
   [: TEST-BINARY ;] catch s" TEST-BINARY" T-LABEL 0 T=
   [: TEST-SPACES ;] catch s" TEST-SPACES" T-LABEL 0 T=
   [: TEST-MODE-CONTENT ;] catch s" TEST-MODE-CONTENT" T-LABEL 0 T=
   [: TEST-CR-PATH ;] catch s" TEST-CR-PATH" T-LABEL 0 T=
   [: TEST-DISSIMILARITY ;] catch s" TEST-DISSIMILARITY" T-LABEL 0 T=
   [: TEST-MODIFIED-RENAME ;] catch s" TEST-MODIFIED-RENAME" T-LABEL 0 T=
   [: TEST-MODIFIED-COPY ;] catch s" TEST-MODIFIED-COPY" T-LABEL 0 T=
   TEST-REJECTIONS
   T-REPORT ;

RUN

;package
