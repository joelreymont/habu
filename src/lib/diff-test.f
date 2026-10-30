\ diff-test.f - src/lib/diff.f's unified patches and three-way merges, byte
\ for byte. Every expected text is what git 2.x writes for the same input
\ (`git diff --no-index` without its index line, and `git merge-file -p --diff3
\ -L ours -L base -L theirs`).
\ Run: bin/hb --load src/lib/diff-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/byte-buffer.f
require src/lib/diff.f

package DIFF-TEST
using DIFF

create OUT BUF:HDR-BYTES allot
create WANT BUF:HDR-BYTES allot

: W+ ( ptr u8 n -- )  BUF:N>BLEN WANT BUF:APPEND-SPAN ;
: GOT$ ( -- ptr u8 n )  OUT BUF:SPAN$ BUF:BLEN>N ;
: WANT$ ( -- ptr u8 n )  WANT BUF:SPAN$ BUF:BLEN>N ;
: SAME ( -- )  GOT$ WANT$ T$= ;

: FRESH ( ptr u8 n -- )  T-LABEL OUT BUF:CLEAR WANT BUF:CLEAR ;

: PATH ( -- ptr u8 n )  s" src/f.f" ;
: HEAD+ ( -- )  s\" diff --git a/src/f.f b/src/f.f\n--- a/src/f.f\n+++ b/src/f.f\n" W+ ;
: NINE ( -- ptr u8 n )  s\" 1\n2\n3\n4\n5\n6\n7\n8\n9\n" ;
: TWENTY ( -- ptr u8 n )
   s\" 1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n" ;


: TEST-END ( -- )
   s" edit at the end" FRESH
   PATH NINE s\" 1\n2\n3\n4\n5\n6\n7\n8\n99\n" OUT WRITE-EDIT
   HEAD+ s\" @@ -6,4 +6,4 @@\n 6\n 7\n 8\n-9\n+99\n" W+ SAME ;


\ Six unchanged lines between two changes join their hunks; seven part them.
: TEST-HUNKS ( -- )
   s" hunks join across six lines, not seven" FRESH
   PATH TWENTY
   s\" 1\n22\n3\n4\n5\n6\n7\n8\n99\n10\n11\n12\n13\n14\n15\n16\n177\n18\n19\n20\n"
   OUT WRITE-EDIT
   HEAD+
   s\" @@ -1,12 +1,12 @@\n 1\n-2\n+22\n 3\n 4\n 5\n 6\n 7\n 8\n-9\n+99\n 10\n 11\n 12\n" W+
   s\" @@ -14,7 +14,7 @@\n 14\n 15\n 16\n-17\n+177\n 18\n 19\n 20\n" W+ SAME ;


\ After its `@@` a hunk header names the nearest old line before the hunk that
\ starts with a letter, `_` or `$`; a hunk with none since the previous hunk's
\ start keeps the previous hunk's.
: TEST-FUNC ( -- )
   s" function line before a hunk" FRESH
   PATH s\" a\n1\n2\n3\n4\n5\n" s\" a\n1\n2\n3\n4\n55\n" OUT WRITE-EDIT
   HEAD+ s\" @@ -3,4 +3,4 @@ a\n 2\n 3\n 4\n-5\n+55\n" W+ SAME
   s" function line carried to the next hunk" FRESH
   PATH s\" f\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n"
   s\" f\n1\n2\n3\n44\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n155\n16\n" OUT WRITE-EDIT
   HEAD+ s\" @@ -2,7 +2,7 @@ f\n 1\n 2\n 3\n-4\n+44\n 5\n 6\n 7\n" W+
   s\" @@ -13,5 +13,5 @@ f\n 12\n 13\n 14\n-15\n+155\n 16\n" W+ SAME ;


\ A path holding a space ends its `---` and `+++` names with a tab; one holding
\ a control byte or a byte past ASCII is C-quoted, so a line feed in it cannot
\ start a patch line.
: TEST-PATHS ( -- )
   s" path with a space" FRESH
   s" my file" s\" 1\n2\n" OUT WRITE-ADD
   s\" diff --git a/my file b/my file\nnew file mode 100644\n--- /dev/null\n" W+
   s\" +++ b/my file\t\n@@ -0,0 +1,2 @@\n+1\n+2\n" W+ SAME
   s" path with a line feed" FRESH
   s\" x\n+++ b/evil" s\" 1\n" s\" 2\n" OUT WRITE-EDIT
   s\" diff --git \"a/x\\n+++ b/evil\" \"b/x\\n+++ b/evil\"\n" W+
   s\" --- \"a/x\\n+++ b/evil\"\t\n+++ \"b/x\\n+++ b/evil\"\t\n" W+
   s\" @@ -1 +1 @@\n-1\n+2\n" W+ SAME
   s" path past ASCII" FRESH
   s\" \xc3\xa9.txt" s\" 1\n" s\" 2\n" OUT WRITE-EDIT
   s\" diff --git \"a/\\303\\251.txt\" \"b/\\303\\251.txt\"\n" W+
   s\" --- \"a/\\303\\251.txt\"\n+++ \"b/\\303\\251.txt\"\n" W+
   s\" @@ -1 +1 @@\n-1\n+2\n" W+ SAME ;


: TEST-INSERT ( -- )
   s" insert only" FRESH
   PATH NINE s\" 1\n2\n3\n4\n45\n5\n6\n7\n8\n9\n" OUT WRITE-EDIT
   HEAD+ s\" @@ -2,6 +2,7 @@\n 2\n 3\n 4\n+45\n 5\n 6\n 7\n" W+ SAME ;


: TEST-DELETE ( -- )
   s" delete only" FRESH
   PATH NINE s\" 1\n2\n3\n4\n6\n7\n8\n9\n" OUT WRITE-EDIT
   HEAD+ s\" @@ -2,7 +2,6 @@\n 2\n 3\n 4\n-5\n 6\n 7\n 8\n" W+ SAME ;


: TEST-ADDED ( -- )
   s" added file" FRESH
   PATH s\" 1\n2\n" OUT WRITE-ADD
   s\" diff --git a/src/f.f b/src/f.f\nnew file mode 100644\n--- /dev/null\n" W+
   s\" +++ b/src/f.f\n@@ -0,0 +1,2 @@\n+1\n+2\n" W+ SAME
   s" added empty file" FRESH
   PATH s" " OUT WRITE-ADD
   s\" diff --git a/src/f.f b/src/f.f\nnew file mode 100644\n" W+ SAME ;


: TEST-DELETED ( -- )
   s" deleted file" FRESH
   PATH s\" 1\n2\n" OUT WRITE-DELETE
   s\" diff --git a/src/f.f b/src/f.f\ndeleted file mode 100644\n--- a/src/f.f\n" W+
   s\" +++ /dev/null\n@@ -1,2 +0,0 @@\n-1\n-2\n" W+ SAME ;


: TEST-NO-NEWLINE ( -- )
   s" no final newline on either side" FRESH
   PATH s\" 1\n2\n3" s\" 1\n2\n33" OUT WRITE-EDIT
   HEAD+ s\" @@ -1,3 +1,3 @@\n 1\n 2\n-3\n\\ No newline at end of file\n" W+
   s\" +33\n\\ No newline at end of file\n" W+ SAME
   s" final newline dropped" FRESH
   PATH s\" 1\n2\n3\n" s\" 1\n2\n3" OUT WRITE-EDIT
   HEAD+ s\" @@ -1,3 +1,3 @@\n 1\n 2\n-3\n+3\n\\ No newline at end of file\n" W+ SAME ;


: TEST-IDENTICAL ( -- )
   s" identical texts write nothing" FRESH
   PATH NINE NINE OUT WRITE-EDIT
   GOT$ nip 0 T= ;


: TEST-CRLF ( -- )
   s" CR is content" FRESH
   PATH s\" 1\r\n2\r\n3\r\n" s\" 1\r\n22\r\n3\r\n" OUT WRITE-EDIT
   HEAD+ s\" @@ -1,3 +1,3 @@\n 1\r\n-2\r\n+22\r\n 3\r\n" W+ SAME ;


: TEST-DISJOINT ( -- )
   s" merge disjoint edits" FRESH
   NINE s\" 1\n22\n3\n4\n5\n6\n7\n8\n9\n" s\" 1\n2\n3\n4\n5\n6\n7\n88\n9\n" OUT MERGE TTRUE
   s\" 1\n22\n3\n4\n5\n6\n7\n88\n9\n" W+ SAME ;


: TEST-SAME-EDIT ( -- )
   s" merge identical edits once" FRESH
   NINE s\" 1\n2\n3\n4\n55\n6\n7\n8\n9\n" s\" 1\n2\n3\n4\n55\n6\n7\n8\n9\n" OUT MERGE TTRUE
   s\" 1\n2\n3\n4\n55\n6\n7\n8\n9\n" W+ SAME ;


: TEST-OVERLAP ( -- )
   s" merge overlapping edits conflict" FRESH
   NINE s\" 1\n2\n3\n44\n55\n6\n7\n8\n9\n" s\" 1\n2\n3\n4\n555\n66\n7\n8\n9\n" OUT MERGE TFALSE
   s\" 1\n2\n3\n<<<<<<< ours\n44\n55\n6\n||||||| base\n4\n5\n6\n" W+
   s\" =======\n4\n555\n66\n>>>>>>> theirs\n7\n8\n9\n" W+ SAME ;


: TEST-TOUCH ( -- )
   s" merge touching edits conflict" FRESH
   NINE s\" 1\n2\n3\n44\n5\n6\n7\n8\n9\n" s\" 1\n2\n3\n4\n55\n6\n7\n8\n9\n" OUT MERGE TFALSE
   s\" 1\n2\n3\n<<<<<<< ours\n44\n5\n||||||| base\n4\n5\n" W+
   s\" =======\n4\n55\n>>>>>>> theirs\n6\n7\n8\n9\n" W+ SAME ;


: TEST-EMPTY-BASE ( -- )
   s" merge over an empty base" FRESH
   s" " s\" 1\n2\n" s\" 3\n" OUT MERGE TFALSE
   s\" <<<<<<< ours\n1\n2\n||||||| base\n=======\n3\n>>>>>>> theirs\n" W+ SAME ;


\ Each conflict section ends in LF, as git's does, though the texts do not.
: TEST-MERGE-NO-NEWLINE ( -- )
   s" merge conflict without final newlines" FRESH
   s\" 1\n2\n3" s\" 1\n2\n33" s\" 1\n2\n333" OUT MERGE TFALSE
   s\" 1\n2\n<<<<<<< ours\n33\n||||||| base\n3\n=======\n333\n>>>>>>> theirs\n" W+ SAME ;


\ git ends the markers, and a line end it adds, with CR LF when the texts do.
: TEST-MERGE-CRLF ( -- )
   s" merge conflict in CR LF texts" FRESH
   s\" 1\r\n2\r\n3" s\" 1\r\n2\r\n33" s\" 1\r\n2\r\n333" OUT MERGE TFALSE
   s\" 1\r\n2\r\n<<<<<<< ours\r\n33\r\n||||||| base\r\n3\r\n" W+
   s\" =======\r\n333\r\n>>>>>>> theirs\r\n" W+ SAME ;


: TEST-MAIN ( -- )
   OUT 64 BUF:N>BLEN BUF:INIT
   WANT 64 BUF:N>BLEN BUF:INIT
   T-RESET
   TEST-END
   TEST-HUNKS
   TEST-FUNC
   TEST-PATHS
   TEST-INSERT
   TEST-DELETE
   TEST-ADDED
   TEST-DELETED
   TEST-NO-NEWLINE
   TEST-IDENTICAL
   TEST-CRLF
   TEST-DISJOINT
   TEST-SAME-EDIT
   TEST-OVERLAP
   TEST-TOUCH
   TEST-EMPTY-BASE
   TEST-MERGE-NO-NEWLINE
   TEST-MERGE-CRLF
   OUT BUF:DISPOSE
   WANT BUF:DISPOSE
   T-REPORT
   s" diff-test: ok" type cr ;

TEST-MAIN

;using
;package
