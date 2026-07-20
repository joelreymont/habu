\ diff-frame-test.f - framed diff codec: round-trip, framing rejects, memory.
\ Run: bin/hb --load tools/lint/diff-frame-test.f
\
\ The frame layout and its integrity rejects are the behavioural spec kept from
\ the stranded recovery. The per-line event stream and the derived form are
\ re-derived against the Option B design (the section body is validated by the
\ shared DIFF:LINE parser, not a second byte-exact validator).

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require tools/lint/diff-frame-write.f
require tools/lint/diff-frame.f

package DIFF-FRAME-TEST
private

$10000 constant ART-CAP
$20 constant DIGEST-U
112 constant FIRST-SECTION
114 constant FORM-OFF
115 constant BODY-OFF
116 constant MODE-OFF
120 constant OLD-LEN-OFF
128 constant OLD-PATH-OFF

create ART ART-CAP allot
create COPY ART-CAP allot
create DIGEST DIGEST-U allot
create PATH-BUF $200 allot
create GROW-RAW $2000 allot
create CTRL-BUF $10 allot

variable ART-U
variable PATH-U
variable PREFIX-U
variable GROW-U

: FROM$ ( -- ptr u8 n ) s" 1111111111111111111111111111111111111111" ;
: TO$   ( -- ptr u8 n ) s" 2222222222222222222222222222222222222222" ;

: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u $200 > if E-DIFF-FRAME-CAP throw then
   a PATH-BUF u BYTE-COPY
   u PATH-U ! ;

: BOOL>N ( bool -- n ) if 1 else 0 then ;

\ ---- raw single-file diff bodies (built with the string builder) ----------

: LF ( -- ) $0A SB-APPEND-C ;

: TEXT-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" --- a/" SB-APPEND PATH$ SB-APPEND LF
   s" +++ b/" SB-APPEND PATH$ SB-APPEND LF
   s" @@ -1,1 +1,1 @@" SB-APPEND LF
   s" -old" SB-APPEND LF
   s" +new" SB-APPEND LF
   SB$ ;

: MODE-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" old mode 100644" SB-APPEND LF
   s" new mode 100755" SB-APPEND LF
   SB$ ;

: EMPTY-ADD-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" new file mode 100644" SB-APPEND LF
   s" index 0000000000..e69de29bb2" SB-APPEND LF
   SB$ ;

: BINARY-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" Binary files a/" SB-APPEND PATH$ SB-APPEND s"  and b/" SB-APPEND PATH$ SB-APPEND
   s"  differ" SB-APPEND LF
   SB$ ;

: PURE-RAW ( ptr u8 n ptr u8 n -- ptr u8 n ) {: old:ptr oldu:n new:ptr newu:n :}
   SB-RESET
   s" diff --git a/" SB-APPEND old oldu SB-APPEND s"  b/" SB-APPEND new newu SB-APPEND LF
   s" similarity index 100%" SB-APPEND LF
   s" rename from " SB-APPEND old oldu SB-APPEND LF
   s" rename to " SB-APPEND new newu SB-APPEND LF
   SB$ ;

: COPY-PURE-RAW ( ptr u8 n ptr u8 n -- ptr u8 n ) {: old:ptr oldu:n new:ptr newu:n :}
   SB-RESET
   s" diff --git a/" SB-APPEND old oldu SB-APPEND s"  b/" SB-APPEND new newu SB-APPEND LF
   s" similarity index 100%" SB-APPEND LF
   s" copy from " SB-APPEND old oldu SB-APPEND LF
   s" copy to " SB-APPEND new newu SB-APPEND LF
   SB$ ;

\ A text-modify body for an arbitrary literal path, decoupled from PATH$ so a
\ test can declare a different (mismatched) side path over a known raw body.
: TEXT-RAW-FOR ( ptr u8 n -- ptr u8 n ) {: p:ptr pu:n :}
   SB-RESET
   s" diff --git a/" SB-APPEND p pu SB-APPEND s"  b/" SB-APPEND p pu SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" --- a/" SB-APPEND p pu SB-APPEND LF
   s" +++ b/" SB-APPEND p pu SB-APPEND LF
   s" @@ -1,1 +1,1 @@" SB-APPEND LF
   s" -old" SB-APPEND LF
   s" +new" SB-APPEND LF
   SB$ ;

: ADD-BODY-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" new file mode 100644" SB-APPEND LF
   s" index 0000000000..2222222222" SB-APPEND LF
   s" --- /dev/null" SB-APPEND LF
   s" +++ b/" SB-APPEND PATH$ SB-APPEND LF
   s" @@ -0,0 +1,1 @@" SB-APPEND LF
   s" +new" SB-APPEND LF
   SB$ ;

: DELETE-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" deleted file mode 100644" SB-APPEND LF
   s" index 2222222222..0000000000" SB-APPEND LF
   s" --- a/" SB-APPEND PATH$ SB-APPEND LF
   s" +++ /dev/null" SB-APPEND LF
   s" @@ -1,1 +0,0 @@" SB-APPEND LF
   s" -gone" SB-APPEND LF
   SB$ ;

\ Two complete single-file diffs concatenated: the shared parser emits two
\ section events, which the framed section contract must reject.
: TWO-FILE-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" --- a/a.f" SB-APPEND LF s" +++ b/a.f" SB-APPEND LF
   s" @@ -1,1 +1,1 @@" SB-APPEND LF
   s" -old" SB-APPEND LF s" +new" SB-APPEND LF
   s" diff --git a/b.f b/b.f" SB-APPEND LF
   s" index 3333333333..4444444444 100644" SB-APPEND LF
   s" --- a/b.f" SB-APPEND LF s" +++ b/b.f" SB-APPEND LF
   s" @@ -1,1 +1,1 @@" SB-APPEND LF
   s" -x" SB-APPEND LF s" +y" SB-APPEND LF
   SB$ ;

\ "a" c "f": a 3-byte path whose middle byte is arbitrary (used to smuggle a
\ raw control byte the string builders cannot express).
: CTRL-PATH ( n -- ptr u8 n ) {: c:n :}
   $61 CTRL-BUF c!
   c   CTRL-BUF 1+ c!
   $66 CTRL-BUF 2 + c!
   CTRL-BUF 3 ;

\ ---- writing helpers ------------------------------------------------------

: START ( -- )
   ART ART-CAP FROM$ TO$ DIFF-FRAME-WRITE:START ;

: SAVE ( ptr u8 n -- )
   nip ART-U ! ;

: SECT-TEXT ( -- )
   DIFF-FRAME:STATUS-MODIFIED true PATH$ true PATH$ TEXT-RAW DIFF-FRAME-WRITE:SECTION ;

: ONE-TEXT ( -- )
   START SECT-TEXT DIFF-FRAME-WRITE:FINISH SAVE ;

\ ---- enum decoders for assertions -----------------------------------------

: STATUS# ( DIFF-FRAME:status -- n )
   MATCH DIFF-FRAME:status
      modified OF 0 ENDOF added OF 1 ENDOF removed OF 2 ENDOF
      renamed OF 3 ENDOF copied OF 4 ENDOF
   ;MATCH ;

: FORM# ( DIFF-FRAME:form -- n )
   MATCH DIFF-FRAME:form
      text OF 0 ENDOF mode OF 1 ENDOF empty OF 2 ENDOF
      pure OF 3 ENDOF binary OF 4 ENDOF
   ;MATCH ;

: EVENT# ( DIFF-FRAME:event -- n )
   MATCH DIFF-FRAME:event
      none OF 0 ENDOF file OF 1 ENDOF hunk OF 2 ENDOF
      add OF 3 ENDOF context OF 4 ENDOF delete OF 5 ENDOF
   ;MATCH ;

: EXPECT-NEXT ( n -- ) {: want:n :}
   DIFF-FRAME:NEXT? {: a:ptr u:n meta:n k:DIFF-FRAME:event present:bool :}
   a u 2drop meta drop
   present BOOL>N want 0<> BOOL>N T=
   k EVENT# want T= ;

\ ---- tests ----------------------------------------------------------------

: TEST-ROUNDTRIP ( -- )
   s" a.f" PATH!
   ONE-TEXT
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 1 T=
   DIFF-FRAME:FROM$ FROM$ T$=
   DIFF-FRAME:TO$ TO$ T$=
   \ FILE event exposes the section fields
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-INDEX 0 T=
   DIFF-FRAME:SECTION-STATUS STATUS# 0 T=
   DIFF-FRAME:SECTION-FORM FORM# 0 T=
   DIFF-FRAME:SECTION-BODY? TTRUE
   DIFF-FRAME:SECTION-MODE? TFALSE
   DIFF-FRAME:SECTION-OLD? TTRUE
   DIFF-FRAME:SECTION-NEW? TTRUE
   DIFF-FRAME:SECTION-NEW$ PATH$ T$=
   \ body events, then end
   2 EXPECT-NEXT
   5 EXPECT-NEXT
   3 EXPECT-NEXT
   0 EXPECT-NEXT ;

: BUILD-FORMS ( -- )
   START
   s" a.f" PATH!
   DIFF-FRAME:STATUS-MODIFIED true PATH$ true PATH$ MODE-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME:STATUS-ADDED false s" " true PATH$ EMPTY-ADD-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME:STATUS-MODIFIED true PATH$ true PATH$ BINARY-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME:STATUS-RENAMED true s" old" true s" new"
   s" old" s" new" PURE-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME-WRITE:FINISH SAVE ;

: TEST-FORMS ( -- )
   BUILD-FORMS
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 4 T=
   \ mode section
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-FORM FORM# 1 T=
   DIFF-FRAME:SECTION-MODE? TTRUE
   DIFF-FRAME:SECTION-BODY? TFALSE
   \ empty add section
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-STATUS STATUS# 1 T=
   DIFF-FRAME:SECTION-FORM FORM# 2 T=
   DIFF-FRAME:SECTION-OLD? TFALSE
   \ binary section
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-FORM FORM# 4 T=
   DIFF-FRAME:SECTION-BODY? TTRUE
   \ pure rename section
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-STATUS STATUS# 3 T=
   DIFF-FRAME:SECTION-FORM FORM# 3 T=
   0 EXPECT-NEXT ;

\ Paths carrying only bytes a unified-diff body admits round-trip; a NUL byte
\ and bytes the parser forbids inside a diff line do not (re-derived: the old
\ RAW-STEP validator accepted embedded newline/tab paths, the shared parser
\ rejects them, which is the correct unified-diff contract).
: PATH-CASE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u PATH!
   [: ONE-TEXT ;] catch ;

: TEST-PATH-MATRIX ( -- )
   s" plain" PATH-CASE 0 T=
   s" has space" PATH-CASE 0 T=
   S\" has\"quote" PATH-CASE 0 T=
   S\" has\\slash" PATH-CASE 0 T=
   s" has b/ split" PATH-CASE 0 T=
   s" has and split" PATH-CASE 0 T= ;

: NUL-PATH ( -- )
   s" a-b" PATH!
   0 PATH-BUF 1+ c!
   ONE-TEXT ;

: TEST-NUL-PATH ( -- )
   [: NUL-PATH ;] E-DIFF-SYNTAX TTHROWSQ ;

: SHORT-ID ( -- )
   ART ART-CAP s" 0123456789" TO$ DIFF-FRAME-WRITE:START ;

: TEST-SHORT-ID ( -- )
   [: SHORT-ID ;] E-DIFF-SYNTAX TTHROWSQ ;

: OPEN-PREFIX ( -- )
   ART PREFIX-U @ DIFF-FRAME:OPEN ;

: TEST-TRUNCATION ( -- )
   s" a.f" PATH!
   ONE-TEXT
   0 begin dup ART-U @ < while
      dup PREFIX-U !
      [: OPEN-PREFIX ;] E-DIFF-SYNTAX TTHROWSQ
      1+
   repeat drop ;

: COPY-ART ( -- )
   ART COPY ART-U @ BYTE-COPY ;

: RESEAL ( -- )
   COPY ART-U @ DIGEST-U - DIGEST SHA256
   DIGEST COPY ART-U @ DIGEST-U - + DIGEST-U BYTE-COPY ;

: COPY-OPEN ( -- )
   COPY ART-U @ DIFF-FRAME:OPEN ;

: LE64! ( n ptr u8 -- ) {: value:n dst:ptr :}
   0 begin dup 8 < while
      value over 8 * rshift $FF and dst over + c!
      1+
   repeat drop ;

\ Corrupt a stored tag/length/digest/count and confirm the reader rejects it.
: TEST-MISMATCHES ( -- )
   s" a.f" PATH!
   ONE-TEXT
   \ invalid form byte
   COPY-ART $FF COPY FORM-OFF + c! RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   \ wrong (but valid) form byte: derived form disagrees
   COPY-ART 2 COPY FORM-OFF + c! RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   \ flipped body flag
   COPY-ART 0 COPY BODY-OFF + c! RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   \ flipped mode flag
   COPY-ART 1 COPY MODE-OFF + c! RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   \ corrupted old-path length
   COPY-ART 99 COPY OLD-LEN-OFF + LE64! RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   \ digest bit flip without reseal
   COPY-ART COPY ART-U @ 1- + dup c@ 1 xor swap c!
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   \ wrong section count in the trailer
   COPY-ART 2 COPY ART-U @ DIGEST-U - 8 - + LE64! RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ ;

\ The reader owns the artifact bytes: mutating the caller's buffer after OPEN
\ leaves the reader's view intact.
: TEST-OWNERSHIP ( -- )
   s" a.f" PATH!
   ONE-TEXT
   ART ART-U @ DIFF-FRAME:OPEN
   0 begin dup ART-U @ < while
      $A5 ART over + c!
      1+
   repeat drop
   1 EXPECT-NEXT
   2 EXPECT-NEXT
   5 EXPECT-NEXT
   3 EXPECT-NEXT
   0 EXPECT-NEXT ;

\ Returned spans are copies: mutating one does not corrupt later reads.
: TEST-DETACHED-VIEWS ( -- )
   s" a.f" PATH!
   ONE-TEXT
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:FROM$ {: from:ptr fromu:n :}
   fromu 0 > TTRUE
   $58 from c!
   DIFF-FRAME:TO$ TO$ T$=
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-NEW$ {: new:ptr newu:n :}
   newu 0 > TTRUE
   $58 new c!
   DIFF-FRAME:SECTION-NEW$ PATH$ T$= ;

: GROW-C ( n -- ) {: c:n :}
   GROW-U @ $2000 >= if E-DIFF-FRAME-CAP throw then
   c GROW-RAW GROW-U @ + c!
   GROW-U @ 1+ GROW-U ! ;

: GROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GROW-U @ u + $2000 > if E-DIFF-FRAME-CAP throw then
   a GROW-RAW GROW-U @ + u BYTE-COPY
   GROW-U @ u + GROW-U ! ;

: GROW-LF ( -- ) $0A GROW-C ;
: GROW$ ( -- ptr u8 n ) GROW-RAW GROW-U @ ;

: BUILD-BIG ( -- )
   0 GROW-U !
   s" diff --git a/a.f b/a.f" GROW+ GROW-LF
   s" index 1111111111..2222222222 100644" GROW+ GROW-LF
   s" --- a/a.f" GROW+ GROW-LF s" +++ b/a.f" GROW+ GROW-LF
   s" @@ -1,1 +1,1 @@" GROW+ GROW-LF
   s" -x" GROW+ GROW-LF
   $2B GROW-C
   0 begin dup $1000 < while $61 GROW-C 1+ repeat drop
   GROW-LF
   START
   s" a.f" PATH!
   DIFF-FRAME:STATUS-MODIFIED true PATH$ true PATH$ GROW$ DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME-WRITE:FINISH SAVE ;

\ OPEN reuses its owned buffer across artifacts, and grows it for a larger one.
: TEST-BUFFER-REUSE ( -- )
   s" a.f" PATH!
   ONE-TEXT
   ART ART-U @ DIFF-FRAME:OPEN
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 1 T=
   BUILD-BIG
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 1 T=
   1 EXPECT-NEXT
   2 EXPECT-NEXT
   5 EXPECT-NEXT
   3 EXPECT-NEXT
   0 EXPECT-NEXT ;

\ ---- identity-binding round trips (one file per section) ------------------

: BUILD-ADD-BODY ( -- )
   s" added.f" PATH!
   START
   DIFF-FRAME:STATUS-ADDED false s" " true PATH$ ADD-BODY-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME-WRITE:FINISH SAVE ;

: TEST-ADD-BODY ( -- )
   BUILD-ADD-BODY
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 1 T=
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-STATUS STATUS# 1 T=
   DIFF-FRAME:SECTION-FORM FORM# 0 T=
   DIFF-FRAME:SECTION-OLD? TFALSE
   DIFF-FRAME:SECTION-NEW? TTRUE
   DIFF-FRAME:SECTION-NEW$ PATH$ T$=
   2 EXPECT-NEXT
   3 EXPECT-NEXT
   0 EXPECT-NEXT ;

: BUILD-DELETE ( -- )
   s" gone.f" PATH!
   START
   DIFF-FRAME:STATUS-REMOVED true PATH$ false s" " DELETE-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME-WRITE:FINISH SAVE ;

: TEST-DELETE ( -- )
   BUILD-DELETE
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 1 T=
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-STATUS STATUS# 2 T=
   DIFF-FRAME:SECTION-FORM FORM# 0 T=
   DIFF-FRAME:SECTION-OLD? TTRUE
   DIFF-FRAME:SECTION-NEW? TFALSE
   DIFF-FRAME:SECTION-OLD$ PATH$ T$=
   2 EXPECT-NEXT
   5 EXPECT-NEXT
   0 EXPECT-NEXT ;

: BUILD-COPY ( -- )
   START
   DIFF-FRAME:STATUS-COPIED true s" src.f" true s" dst.f"
   s" src.f" s" dst.f" COPY-PURE-RAW DIFF-FRAME-WRITE:SECTION
   DIFF-FRAME-WRITE:FINISH SAVE ;

: TEST-COPY ( -- )
   BUILD-COPY
   ART ART-U @ DIFF-FRAME:OPEN
   DIFF-FRAME:SECTION-COUNT 1 T=
   1 EXPECT-NEXT
   DIFF-FRAME:SECTION-STATUS STATUS# 4 T=
   DIFF-FRAME:SECTION-FORM FORM# 3 T=
   DIFF-FRAME:SECTION-OLD? TTRUE
   DIFF-FRAME:SECTION-NEW? TTRUE
   DIFF-FRAME:SECTION-OLD$ s" src.f" T$=
   DIFF-FRAME:SECTION-NEW$ s" dst.f" T$=
   0 EXPECT-NEXT ;

\ ---- the three proven holes (commit 04034c04) -----------------------------
\
\ Each was accepted before the section identity was bound to the parser event;
\ each must now be rejected at the write boundary that calls VALIDATE-SECTION.

\ Hole 1: a raw a.f diff framed under a foreign declared path.
: HOLE-CROSS-PATH ( -- )
   START
   DIFF-FRAME:STATUS-MODIFIED true s" declared.f" true s" declared.f"
   s" a.f" TEXT-RAW-FOR DIFF-FRAME-WRITE:SECTION ;

\ Hole 2: two complete file diffs packed under one declared section.
: HOLE-DUP-SECTION ( -- )
   START
   DIFF-FRAME:STATUS-MODIFIED true s" a.f" true s" a.f"
   TWO-FILE-RAW DIFF-FRAME-WRITE:SECTION ;

\ Hole 3: a modify body declared old-absent/new-present.
: HOLE-SIDE-PRESENCE ( -- )
   START
   DIFF-FRAME:STATUS-MODIFIED false s" " true s" a.f"
   s" a.f" TEXT-RAW-FOR DIFF-FRAME-WRITE:SECTION ;

: TEST-HOLES ( -- )
   [: HOLE-CROSS-PATH ;] E-DIFF-FRAME-IDENTITY TTHROWSQ
   [: HOLE-DUP-SECTION ;] E-DIFF-FRAME-SECTIONS TTHROWSQ
   [: HOLE-SIDE-PRESENCE ;] E-DIFF-FRAME-IDENTITY TTHROWSQ ;

\ ---- further binding rejects ----------------------------------------------

\ An empty raw body carries no parser section at all.
: ZERO-SECTION ( -- )
   START
   DIFF-FRAME:STATUS-MODIFIED true s" a.f" true s" a.f"
   s" " DIFF-FRAME-WRITE:SECTION ;

\ A rename whose declared old/new paths are swapped against the raw body.
: CROSS-SWAP-RENAME ( -- )
   START
   DIFF-FRAME:STATUS-RENAMED true s" new" true s" old"
   s" old" s" new" PURE-RAW DIFF-FRAME-WRITE:SECTION ;

\ Declaring "added" over a rename body: status disagrees with the parser form.
: STATUS-MISLABEL ( -- )
   START
   DIFF-FRAME:STATUS-ADDED false s" " true s" new"
   s" old" s" new" PURE-RAW DIFF-FRAME-WRITE:SECTION ;

: TEST-BINDING-REJECTS ( -- )
   [: ZERO-SECTION ;] E-DIFF-FRAME-SECTIONS TTHROWSQ
   [: CROSS-SWAP-RENAME ;] E-DIFF-FRAME-IDENTITY TTHROWSQ
   [: STATUS-MISLABEL ;] E-DIFF-FRAME-IDENTITY TTHROWSQ ;

\ ---- M5 arbitrary path bytes (tab/LF/CR): fail closed --------------------
\
\ The unified-diff raw body is the sole authority for a section's path bytes,
\ and the shared parser admits only printable non-DEL bytes in a path. Tab, LF,
\ and CR cannot appear in any parser-extracted path, so a declared path holding
\ one cannot bind to the body: the framing does not narrow the M5 byte contract
\ silently, it rejects it with a named error (E-DIFF-FRAME-IDENTITY).
: DECL-CTRL ( n -- ) {: c:n :}
   c CTRL-PATH {: pa:ptr pu:n :}
   START
   s" a.f" TEXT-RAW-FOR {: raw:ptr rawu:n :}
   DIFF-FRAME:STATUS-MODIFIED true pa pu true pa pu raw rawu DIFF-FRAME-WRITE:SECTION ;

: TEST-ARBITRARY-PATH-BYTES ( -- )
   [: $09 DECL-CTRL ;] E-DIFF-FRAME-IDENTITY TTHROWSQ
   [: $0A DECL-CTRL ;] E-DIFF-FRAME-IDENTITY TTHROWSQ
   [: $0D DECL-CTRL ;] E-DIFF-FRAME-IDENTITY TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   [: TEST-ROUNDTRIP ;] catch s" roundtrip" T-LABEL 0 T=
   [: TEST-FORMS ;] catch s" forms" T-LABEL 0 T=
   [: TEST-ADD-BODY ;] catch s" add-body" T-LABEL 0 T=
   [: TEST-DELETE ;] catch s" delete" T-LABEL 0 T=
   [: TEST-COPY ;] catch s" copy" T-LABEL 0 T=
   [: TEST-PATH-MATRIX ;] catch s" path-matrix" T-LABEL 0 T=
   TEST-HOLES
   TEST-BINDING-REJECTS
   TEST-ARBITRARY-PATH-BYTES
   TEST-NUL-PATH
   TEST-SHORT-ID
   [: TEST-TRUNCATION ;] catch s" truncation" T-LABEL 0 T=
   [: TEST-MISMATCHES ;] catch s" mismatches" T-LABEL 0 T=
   [: TEST-OWNERSHIP ;] catch s" ownership" T-LABEL 0 T=
   [: TEST-DETACHED-VIEWS ;] catch s" detached-views" T-LABEL 0 T=
   [: TEST-BUFFER-REUSE ;] catch s" buffer-reuse" T-LABEL 0 T=
   T-REPORT ;

;package

DIFF-FRAME-TEST:RUN
