\ diff.f - package DIFF: a line diff of two byte texts, written in git's unified
\ form for one path, and diff3's three-way merge of base, ours and theirs.
\ A line ends at LF, and a last line without one is a line too: "x" and "x\n"
\ are different lines, and CR is content. The edit script is Myers' shortest
\ one, found by his linear-space middle-snake bisection (1986, section 4b)
\ over the lines that can match: as in git's xdiff, a line whose bytes the
\ other side never holds is changed before the search, so a rewrite of every
\ line costs linear time. git also leaves very frequent lines out of its
\ search and DIFF does not, so on text with many repeated lines DIFF's edit
\ can be shorter than git's and its output differ.
\ STORAGE CLASS. CALLER-OWNED: every word appends to the caller's vector. A
\ call's scratch is one MEM:WITH-BYTES scope, zeroed on entry and released
\ whole when the call returns or throws.
\
\ The middle snake. The rectangle is old lines x0.. (nn of them) against new
\ lines y0.. (mm), positions in seq; x and y count from its corner. The
\ forward pass walks from the start and the reverse pass from the end, each
\ recording per diagonal k = x - y the furthest x it reached; where they meet
\ lies a split on a shortest path.

require src/lib/error.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/vector.f
require src/lib/map.f
require src/lib/adt/option.f

package DIFF
private

10 constant LF
3 constant CONTEXT          \ unchanged lines shown around a change, git's default
CONTEXT 2 * constant JOIN   \ the most unchanged lines between two changes one hunk spans
4 constant GROUP-CELLS      \ a change: old start, old end, new start, new end
80 constant FUNC-MAX        \ the most bytes of a function line a hunk header shows
-1 constant NO-FUNC         \ no function line yet
1 constant OLD-SIDE         \ a class's seen bit: an old line of the script has it
2 constant NEW-SIDE         \ a class's seen bit: a new line of the script has it

\ One edit script: old lines a0..a1 against new lines b0..b1, a flag cell per
\ line of the call that is nonzero when the line changed, and the changes in
\ order. Between two changes lies at least one unchanged line.
STRUCTURE script DERIVE addr
   FIELD a0 n
   FIELD a1 n
   FIELD b0 n
   FIELD b1 n
   FIELD chg ^n
   FIELD groups ^n
   FIELD count n
;STRUCTURE

\ One call's state, at the start of its scratch. Lines are numbered across the
\ call's texts, which lie back to back in text; a line's class is the first
\ line with the same bytes, so equal lines have equal classes. out is the
\ caller's vector; starts holds each line's offset in text, then the end
\ of text; ids each line's class; map finds a class by a line's
\ bytes. For the script being run, seen holds per class the sides that have it,
\ and seq the lines the search runs over, the old side's then the new side's. v1
\ and v2 hold the furthest x reached on each diagonal, forward and in reverse;
\ x0, y0, nn and mm are the rectangle being bisected (its old and new start in
\ seq and length); off is diagonal 0's index in v1 and v2 and the round count;
\ k1lo..k2hi are the diagonals each pass has run off the rectangle on; sx and sy
\ are the split the bisection found.
STRUCTURE work DERIVE addr
   FIELD out ^VEC:vector<u8>
   FIELD text ^u8
   FIELD starts ^n
   FIELD ids ^n
   FIELD map ^n
   FIELD seen ^n
   FIELD seq ^n
   FIELD v1 ^n
   FIELD v2 ^n
   FIELD x0 n
   FIELD y0 n
   FIELD nn n
   FIELD mm n
   FIELD off n
   FIELD k1lo n
   FIELD k1hi n
   FIELD k2lo n
   FIELD k2hi n
   FIELD sx n
   FIELD sy n
   FIELD conflicts n
;STRUCTURE

STRUCTURE text
   FIELD at ^u8
   FIELD len n
   FIELD lines n
;STRUCTURE

ENUM kind edit add delete ;ENUM

\ How a text ends its lines, as git's merge reads it at one line.
ENUM eol crlf lf unknown ;ENUM

CAST: >WORK ( ^u8 -- ^work )
CAST: >SCRIPT ( ^u8 -- ^script )

: NTH@ ( ^n n -- n )  cells + @ ;
: NTH! ( n ^n n -- )  cells + ! ;


: LINE-START? ( ^u8 n -- bool )
   {: a i :}
   i 0= if true exit then
   a i 1- + c@ LF = ;


: COUNT-LINES ( str -- n )
   {: s:str :}
   0 s STR:LEN 0 ?do s STR:DATA i LINE-START? if 1+ then loop ;


: >TEXT ( str -- text )
   {: s:str :}
   s STR:DATA s STR:LEN s COUNT-LINES TEXT-MAKE ;

: TEXT-LINES ( text -- n )  TEXT-UNMAKE nip nip ;
: TEXT-LEN ( text -- n )  TEXT-UNMAKE drop nip ;


: START@ ( ^work n -- n )
   {: w g :}
   w WORK-STARTS @ g NTH@ ;


: START! ( n ^work n -- )
   {: v w g :}
   v w WORK-STARTS @ g NTH! ;


: ID@ ( ^work n -- n )
   {: w g :}
   w WORK-IDS @ g NTH@ ;


: LINE$ ( ^work n -- str )
   {: w g :}
   w g START@ {: s :}
   w WORK-TEXT @ s +
   w g 1+ START@ s - >str ;


: ENDS-LF? ( str -- bool )
   {: s:str :}
   s STR:LEN 0= if false exit then
   s STR:DATA s STR:LEN 1- + c@ LF = ;


\ Copy t to text offset at, its first line numbered g; the next line and offset.
: ADD-TEXT ( ^work text n n -- n n )
   {: w t g at :}
   t TEXT-UNMAKE drop {: a u :}
   a w WORK-TEXT @ at + u BYTE-COPY
   g u 0 ?do
      a i LINE-START? if {: line :} at i + w line START! line 1+ then
   loop
   at u + ;


\ The map keeps a pointer into text, which lives for the whole call.

: CLASS ( ^work n -- n )
   {: w g :}
   w WORK-MAP @ w g LINE$ {: m line:str :}
   m line MAP:GET MATCH option
      none OF g m line MAP:SET g ENDOF
      some OF ENDOF
   ;MATCH ;


: CLASSIFY ( ^work n -- )
   {: w lines :}
   lines 0 ?do w i CLASS w WORK-IDS @ i NTH! loop ;


\ Line seq[p]'s class.
: KEY@ ( ^work n -- n )
   {: w p :}
   w w WORK-SEQ @ p NTH@ ID@ ;


: DELTA ( ^work -- n )
   {: w :}
   w WORK-NN @ w WORK-MM @ - ;

: FRONT? ( ^work -- bool )  DELTA 1 and 0<> ;
: VLEN ( ^work -- n )  WORK-OFF @ 2 * ;


: OLD@ ( ^work n -- n )
   {: w x :}
   w w WORK-X0 @ x + KEY@ ;


: NEW@ ( ^work n -- n )
   {: w y :}
   w w WORK-Y0 @ y + KEY@ ;


: AHEAD? ( ^work n n -- bool )
   {: w x y :}
   x w WORK-NN @ < 0= if false exit then
   y w WORK-MM @ < 0= if false exit then
   w x OLD@ w y NEW@ = ;


: BEHIND? ( ^work n n -- bool )
   {: w x y :}
   x w WORK-NN @ < 0= if false exit then
   y w WORK-MM @ < 0= if false exit then
   w w WORK-NN @ x - 1- OLD@ w w WORK-MM @ y - 1- NEW@ = ;


: SLIDE ( ^work n n -- n )
   {: w x k :}
   x begin {: p :} p w p p k - AHEAD? while 1+ repeat ;


: SLIDE-BACK ( ^work n n -- n )
   {: w x k :}
   x begin {: p :} p w p p k - BEHIND? while 1+ repeat ;


\ Diagonal k's starting x in round d, from the reach v recorded on its
\ neighbours; at is k's index in v.
: REACH ( ^n n n n -- n )
   {: v at k d :}
   k d negate = if v at 1+ NTH@ exit then
   k d = if v at 1- NTH@ 1+ exit then
   v at 1- NTH@ v at 1+ NTH@ < if v at 1+ NTH@ exit then
   v at 1- NTH@ 1+ ;


\ The other pass's reach on the diagonal facing k, or -1 when it has none.
: FACING ( ^work ^n n -- n )
   {: w v k :}
   w WORK-OFF @ w DELTA + k - {: at :}
   at 0 < if -1 exit then
   at w VLEN < 0= if -1 exit then
   v at NTH@ ;


: SPLIT! ( ^work n n -- )
   {: w x y :}
   w WORK-X0 @ x + w WORK-SX !
   w WORK-Y0 @ y + w WORK-SY ! ;


: SPLIT@ ( ^work -- n n )
   {: w :}
   w WORK-SX @ w WORK-SY @ ;


: FORWARD-MEETS? ( ^work n n n -- bool )
   {: w k x y :}
   w FRONT? 0= if false exit then
   w w WORK-V2 @ k FACING {: r :}
   r -1 = if false exit then
   x w WORK-NN @ r - < if false exit then
   w x y SPLIT! true ;


: REVERSE-MEETS? ( ^work n n -- bool )
   {: w k x :}
   w FRONT? if false exit then
   w w WORK-V1 @ k FACING {: r :}
   r -1 = if false exit then
   r w WORK-NN @ x - < if false exit then
   w r r w DELTA - k + SPLIT! true ;


: FORWARD-ON ( ^work n n -- bool )
   {: w k d :}
   w WORK-OFF @ k + {: at :}
   w w WORK-V1 @ at k d REACH k SLIDE {: x :}
   x k - {: y :}
   x w WORK-V1 @ at NTH!
   x w WORK-NN @ > if 2 w WORK-K1HI +! false exit then
   y w WORK-MM @ > if 2 w WORK-K1LO +! false exit then
   w k x y FORWARD-MEETS? ;


: REVERSE-ON ( ^work n n -- bool )
   {: w k d :}
   w WORK-OFF @ k + {: at :}
   w w WORK-V2 @ at k d REACH k SLIDE-BACK {: x :}
   x k - {: y :}
   x w WORK-V2 @ at NTH!
   x w WORK-NN @ > if 2 w WORK-K2HI +! false exit then
   y w WORK-MM @ > if 2 w WORK-K2LO +! false exit then
   w k x REVERSE-MEETS? ;


\ Round d of one pass over the diagonals still inside the rectangle; true at a
\ meeting.
: PASS ( ^work n ^n ^n [ ^work n n -- bool ] -- bool )
   {: w d lo:^ hi:^ step :}
   d negate lo @ +
   begin {: k :} k k d hi @ - > 0= while
      w k d step execute if drop true exit then
      2 +
   repeat drop false ;


: BISECT-START ( ^work n n n n -- )
   {: w x0 x1 y0 y1 :}
   x0 w WORK-X0 !  y0 w WORK-Y0 !
   x1 x0 - w WORK-NN !  y1 y0 - w WORK-MM !
   x1 x0 - y1 y0 - + 1+ 2 / w WORK-OFF !
   0 w WORK-K1LO !  0 w WORK-K1HI !  0 w WORK-K2LO !  0 w WORK-K2HI !
   w VLEN 1+ 0 ?do -1 w WORK-V1 @ i NTH!  -1 w WORK-V2 @ i NTH! loop
   0 w WORK-V1 @ w WORK-OFF @ 1+ NTH!
   0 w WORK-V2 @ w WORK-OFF @ 1+ NTH! ;


\ A split of old x0..x1 against new y0..y1 on a shortest path, leaving both
\ halves smaller; all of old against none of new when no path meets.
: BISECT ( ^work n n n n -- n n )
   {: w x0 x1 y0 y1 :}
   w x0 x1 y0 y1 BISECT-START
   w WORK-OFF @ 0 ?do
      w i w WORK-K1LO w WORK-K1HI [: FORWARD-ON ;] PASS if w SPLIT@ unloop exit then
      w i w WORK-K2LO w WORK-K2HI [: REVERSE-ON ;] PASS if w SPLIT@ unloop exit then
   loop
   x1 y0 ;


\ Edit scripts. COMPARE works on positions in seq, MARK maps them to lines.

: SAME-LINES? ( ^work n n n n -- bool )
   {: w x y xe ye :}
   x xe < 0= if false exit then
   y ye < 0= if false exit then
   w x KEY@ w y KEY@ = ;


: SAME-BEFORE? ( ^work n n n n -- bool )
   {: w x y xs ys :}
   x xs > 0= if false exit then
   y ys > 0= if false exit then
   w x 1- KEY@ w y 1- KEY@ = ;


: HEAD ( ^work n n n n -- n n )
   {: w a0 a1 b0 b1 :}
   a0 b0 begin {: x y :} x y w x y a1 b1 SAME-LINES? while 1+ swap 1+ swap repeat ;


: TAIL ( ^work n n n n -- n n )
   {: w a0 a1 b0 b1 :}
   a1 b1 begin {: x y :} x y w x y a0 b0 SAME-BEFORE? while 1- swap 1- swap repeat ;


: MARK ( ^work ^script n n -- )
   {: w s lo hi :}
   hi lo ?do 1 s SCRIPT-CHG @ w WORK-SEQ @ i NTH@ NTH! loop ;


: COMPARE ( ^work ^script n n n n -- )
   {: w s a0 a1 b0 b1 :}
   w a0 a1 b0 b1 HEAD {: x0 y0 :}
   w x0 a1 y0 b1 TAIL {: x1 y1 :}
   x0 x1 = if w s y0 y1 MARK exit then
   y0 y1 = if w s x0 x1 MARK exit then
   w x0 x1 y0 y1 BISECT {: x y :}
   w s x0 x y0 y RECURSE
   w s x x1 y y1 RECURSE ;


: CHANGED? ( ^script n -- bool )
   {: s g :}
   s SCRIPT-CHG @ g NTH@ 0<> ;


: KEPT? ( ^script n n -- bool )
   {: s x y :}
   x s SCRIPT-A1 @ < 0= if false exit then
   y s SCRIPT-B1 @ < 0= if false exit then
   s x CHANGED? if false exit then
   s y CHANGED? 0= ;


: KEEP ( ^script n n -- n n )
   {: s i j :}
   i j begin {: x y :} x y s x y KEPT? while 1+ swap 1+ swap repeat ;


: RUN-END ( ^script n n -- n )
   {: s i e :}
   i begin {: x :} x x e < if s x CHANGED? else false then while 1+ repeat ;


: G@ ( ^script n n -- n )
   {: s g f :}
   s SCRIPT-GROUPS @ g GROUP-CELLS * f + NTH@ ;


: G! ( n ^script n n -- )
   {: v s g f :}
   v s SCRIPT-GROUPS @ g GROUP-CELLS * f + NTH! ;


: ADD-GROUP ( ^script n n n n -- )
   {: s x xe y ye :}
   s SCRIPT-COUNT @ {: g :}
   x s g 0 G!  xe s g 1 G!  y s g 2 G!  ye s g 3 G!
   1 s SCRIPT-COUNT +! ;


: GROUPS ( ^script -- )
   {: s :}
   s SCRIPT-A0 @ s SCRIPT-B0 @
   begin {: i j :}
      s i j KEEP {: x y :}
      s x s SCRIPT-A1 @ RUN-END {: xe :}
      s y s SCRIPT-B1 @ RUN-END {: ye :}
      x xe = y ye = and if exit then
      s x xe y ye ADD-GROUP
      xe ye
   again ;


: UNSEE ( ^work n n -- )
   {: w lo hi :}
   hi lo ?do 0 w WORK-SEEN @ w i ID@ NTH! loop ;


: TALLY ( ^work n n n -- )
   {: w lo hi side :}
   hi lo ?do
      w WORK-SEEN @ w i ID@ {: v c :}
      v c NTH@ side or v c NTH!
   loop ;


\ Each line lo..hi-1 whose class side has goes to seq from at, and any other
\ is changed; the next free position in seq.
: SIFT ( ^work ^script n n n n -- n )
   {: w s lo hi side at :}
   at hi lo ?do
      w WORK-SEEN @ w i ID@ NTH@ side and 0<> if
         {: k :} i w WORK-SEQ @ k NTH! k 1+
      else
         1 s SCRIPT-CHG @ i NTH!
      then
   loop ;


\ A line whose class the other side lacks can never match: it is changed
\ first, and the search runs over the other lines alone, so dropping them
\ keeps the edit shortest.
: RUN ( ^work ^script -- )
   {: w s :}
   s SCRIPT-A0 @ s SCRIPT-A1 @ s SCRIPT-B0 @ s SCRIPT-B1 @ {: a0 a1 b0 b1 :}
   w a0 a1 UNSEE  w b0 b1 UNSEE
   w a0 a1 OLD-SIDE TALLY  w b0 b1 NEW-SIDE TALLY
   w s a0 a1 NEW-SIDE 0 SIFT {: ka :}
   w s b0 b1 OLD-SIDE ka SIFT {: kb :}
   w s 0 ka ka kb COMPARE
   s GROUPS ;


: RANGES! ( n n n n ^script -- )
   {: a0 a1 b0 b1 s :}
   a0 s SCRIPT-A0 !  a1 s SCRIPT-A1 !  b0 s SCRIPT-B0 !  b1 s SCRIPT-B1 ! ;


\ Scratch. Every region is cells but the text, cut last; a cut past the
\ mapping is E-SPAN-RANGE.

\ The map's slots: twice the lines.
: MAP-CAP ( n -- count )
   {: lines :}
   lines 2 * >COUNT ;


: MAP-SPACE ( n -- n )
   {: lines :}
   lines MAP-CAP MAP:CELL-COUNT COUNT>N ;


: SCRIPT-SPACE ( n -- n )
   {: lines :}
   SCRIPT-CELLS lines 1+ GROUP-CELLS * + lines + cells ;


: SIZE ( n n n -- NUM:alloc-byte-len )
   {: lines bytes k :}
   WORK-CELLS lines 1+ + lines 3 * + lines MAP-SPACE + lines 2 + 2 * + cells
   lines SCRIPT-SPACE k * + bytes + MEM:BYTES-ALLOC-LEN ;


: CUT ( SPAN:span<u8> n -- SPAN:span<u8> ^u8 )
   {: s u :}
   s u SPAN:SKIP s u SPAN:TAKE SPAN:$ STR:DATA ;


: OPEN ( ^u8 NUM:alloc-byte-len ^VEC:vector<u8> n -- SPAN:span<u8> ^work )
   {: base size out lines :}
   base size MEM:ALLOCATION>SPAN
   WORK-CELLS cells CUT >WORK {: w :}
   out w WORK-OUT !
   lines 1+ cells CUT CELL-VIEW w WORK-STARTS !
   lines cells CUT CELL-VIEW w WORK-IDS !
   lines MAP-SPACE cells CUT CELL-VIEW w WORK-MAP !
   w WORK-MAP @ lines MAP-CAP MAP:INIT
   lines cells CUT CELL-VIEW w WORK-SEEN !
   lines cells CUT CELL-VIEW w WORK-SEQ !
   lines 2 + cells CUT CELL-VIEW w WORK-V1 !
   lines 2 + cells CUT CELL-VIEW w WORK-V2 !
   w ;


: CUT-SCRIPT ( SPAN:span<u8> n -- SPAN:span<u8> ^script )
   {: s lines :}
   s SCRIPT-CELLS cells CUT >SCRIPT {: r :}
   lines 1+ GROUP-CELLS * cells CUT CELL-VIEW r SCRIPT-GROUPS !
   lines cells CUT CELL-VIEW r SCRIPT-CHG !
   r ;


\ The texts' bytes take the rest of the scratch; their lines follow in order.
: LOAD-TEXTS ( SPAN:span<u8> ^work n -- )
   {: s w bytes :}
   s bytes CUT w WORK-TEXT ! SPAN:$ drop ;


: FINISH ( ^work n n -- )
   {: w at lines :}
   at w lines START!
   w lines CLASSIFY ;


: OUT$ ( ^work str -- )
   {: w s:str :}
   s w WORK-OUT @ VEC:APPEND ;


: OUT-BYTE ( ^work n -- )
   {: w c :}
   c w WORK-OUT @ VEC:PUSH c! ;


: OUT-LINE ( ^work str -- )
   {: w s:str :}
   w s OUT$ w LF OUT-BYTE ;


: OUT-U ( ^work n -- )
   {: w u :}
   u 10 < 0= if w u 10 / RECURSE then
   w u 10 mod 48 + OUT-BYTE ;


: COPY-LINES ( ^work n n -- )
   {: w lo hi :}
   w WORK-TEXT @ w lo START@ + w hi START@ w lo START@ - >str {: line:str :}
   w line OUT$ ;


: OUT-MARKED ( ^work n n -- )
   {: w c g :}
   w c OUT-BYTE
   w g LINE$ {: line:str :}
   w line OUT$
   line ENDS-LF? if exit then
   w LF OUT-BYTE
   w s" \ No newline at end of file" OUT-LINE ;


: OUT-RUN ( ^work n n n -- )
   {: w c lo hi :}
   hi lo ?do w c i OUT-MARKED loop ;


\ git's start[,count]: an empty range names the line before it, and a count of
\ one is left out.
: OUT-RANGE ( ^work n n -- )
   {: w at count :}
   count 0= if w at OUT-U else w at 1+ OUT-U then
   count 1 = if exit then
   w 44 OUT-BYTE
   w count OUT-U ;


: FUNC? ( ^work n -- bool )
   {: w g :}
   w g LINE$ STR:DATA c@ {: c :}
   c [char] _ = c [char] $ = or if true exit then
   c STR:ASCII-LOWER {: l :}
   l STR:BEFORE-LOWER-A > l STR:AFTER-LOWER-Z < and ;


\ git's function line for a hunk whose old text starts at line a: the nearest
\ old line before it starting with an ASCII letter, `_` or `$`, searched back
\ to lo, the previous hunk's start; else fn, the previous hunk's.
: FUNC-LINE ( ^work n n n -- n )
   {: w a lo fn :}
   a begin 1- {: g :}
      g lo < if fn exit then
      w g FUNC? if g exit then
      g
   again ;


: OUT-FUNC ( ^work n -- )
   {: w g :}
   g NO-FUNC = if exit then
   w g LINE$ {: line:str :}
   line STR:DATA line STR:LEN FUNC-MAX min >str STR:RTRIM {: head:str :}
   w 32 OUT-BYTE w head OUT$ ;


: OUT-HUNK-HEAD ( ^work ^script n n n n n -- )
   {: w s a ae b be fn :}
   w s" @@ -" OUT$ w a s SCRIPT-A0 @ - ae a - OUT-RANGE
   w s"  +" OUT$ w b s SCRIPT-B0 @ - be b - OUT-RANGE
   w s"  @@" OUT$ w fn OUT-FUNC w LF OUT-BYTE ;


\ The changes first..last-1 with their context, after a hunk that started at
\ old line lo with function line fn; this hunk's start and function line.
: OUT-HUNK ( ^work ^script n n n n -- n n )
   {: w s first last lo fn :}
   s first 0 G@ CONTEXT - s SCRIPT-A0 @ max {: a :}
   s first 2 G@ s first 0 G@ a - - {: b :}
   s last 1- 1 G@ CONTEXT + s SCRIPT-A1 @ min {: ae :}
   s last 1- 3 G@ ae s last 1- 1 G@ - + {: be :}
   w a lo fn FUNC-LINE {: f :}
   w s a ae b be f OUT-HUNK-HEAD
   a last first ?do
      {: at :} w 32 at s i 0 G@ OUT-RUN
      w 45 s i 0 G@ s i 1 G@ OUT-RUN
      w 43 s i 2 G@ s i 3 G@ OUT-RUN
      s i 1 G@
   loop
   {: at :} w 32 at ae OUT-RUN
   a f ;


\ The index after the last change one hunk joins to change first.
: HUNK-END ( ^script n -- n )
   {: s first :}
   first begin {: g :} g
      g 1+ s SCRIPT-COUNT @ < if s g 1+ 0 G@ s g 1 G@ - JOIN > 0= else false then
   while 1+ repeat 1+ ;


: OUT-MODE ( ^work kind -- )
   {: w k :}
   k MATCH kind
      edit OF ENDOF
      add OF w s" new file mode 100644" OUT-LINE ENDOF
      delete OF w s" deleted file mode 100644" OUT-LINE ENDOF
   ;MATCH ;


\ Paths. git C-quotes a path holding a control byte, `"`, `\`, DEL or a byte
\ past ASCII (core.quotePath), so no path byte can start a patch line.

: ANY-BYTE? ( str [ n -- bool ] -- bool )
   {: s:str pred :}
   s STR:LEN 0 ?do s STR:DATA i + c@ pred execute if true unloop exit then loop false ;


: QUOTE-BYTE? ( n -- bool )
   {: c :}
   c STR:SPACE < c 127 < 0= or c [char] " = or c [char] \ = or ;


\ A byte of a quoted path: C's escape letter for BEL..CR, a backslash before
\ `"` and `\`, three octal digits for any other byte QUOTE-BYTE? admits.
: OUT-QUOTED ( ^work n -- )
   {: w c :}
   c QUOTE-BYTE? 0= if w c OUT-BYTE exit then
   w [char] \ OUT-BYTE
   c 7 < 0= c STR:CR > 0= and if w s" abtnvfr" STR:DATA c 7 - + c@ OUT-BYTE exit then
   c [char] " = c [char] \ = or if w c OUT-BYTE exit then
   w c 6 rshift [char] 0 + OUT-BYTE
   w c 3 rshift 7 and [char] 0 + OUT-BYTE
   w c 7 and [char] 0 + OUT-BYTE ;


\ Prefix pre and path p as git names a side, quoted whole when p needs it.
: OUT-NAME ( ^work str str -- )
   {: w pre:str p:str :}
   p [: QUOTE-BYTE? ;] ANY-BYTE? 0= if w pre OUT$ w p OUT$ exit then
   w [char] " OUT-BYTE w pre OUT$
   p STR:LEN 0 ?do w p STR:DATA i + c@ OUT-QUOTED loop
   w [char] " OUT-BYTE ;


\ The rest of a `---` or `+++` line naming a side, which ends in a tab when the
\ path holds a space.
: OUT-SIDE ( ^work str str -- )
   {: w pre:str p:str :}
   w pre p OUT-NAME
   p [: STR:SPACE = ;] ANY-BYTE? if w STR:TAB OUT-BYTE then
   w LF OUT-BYTE ;


: OUT-NAMES ( ^work str kind -- )
   {: w p:str k :}
   k MATCH kind
      edit OF
         w s" --- " OUT$ w s" a/" p OUT-SIDE w s" +++ " OUT$ w s" b/" p OUT-SIDE
      ENDOF
      add OF w s" --- /dev/null" OUT-LINE w s" +++ " OUT$ w s" b/" p OUT-SIDE ENDOF
      delete OF w s" --- " OUT$ w s" a/" p OUT-SIDE w s" +++ /dev/null" OUT-LINE ENDOF
   ;MATCH ;


: EDIT? ( kind -- bool )
   MATCH kind edit OF true ENDOF add OF false ENDOF delete OF false ENDOF ;MATCH ;


\ Nothing for an edit that changes no line. An added or deleted file always
\ has its mode line, as git writes for a regular file, and like git no names
\ when it is empty.
: OUT-PATCH ( ^work ^script str kind -- )
   {: w s p:str k :}
   s SCRIPT-COUNT @ 0= k EDIT? and if exit then
   w s" diff --git " OUT$ w s" a/" p OUT-NAME
   w STR:SPACE OUT-BYTE w s" b/" p OUT-NAME w LF OUT-BYTE
   w k OUT-MODE
   s SCRIPT-COUNT @ 0= if exit then
   w p k OUT-NAMES
   0 s SCRIPT-A0 @ NO-FUNC begin {: first lo fn :} first s SCRIPT-COUNT @ < while
      s first HUNK-END {: last :}
      w s first last lo fn OUT-HUNK {: a f :}
      last a f
   repeat ;


: PATCH-BODY ( str text text kind ^VEC:vector<u8> ^u8 NUM:alloc-byte-len -- )
   {: p:str a b k out base size :}
   a TEXT-LINES {: la :}
   la b TEXT-LINES + {: lines :}
   base size out lines OPEN {: w :}
   lines CUT-SCRIPT {: s :}
   0 la la lines s RANGES!
   w a TEXT-LEN b TEXT-LEN + LOAD-TEXTS
   w a 0 0 ADD-TEXT {: g at :}
   w b g at ADD-TEXT nip w swap lines FINISH
   w s RUN
   w s p k OUT-PATCH ;


: PATCH ( str text text kind ^VEC:vector<u8> -- )
   {: p:str a b k out :}
   p a b k out
   a TEXT-LINES b TEXT-LINES + a TEXT-LEN b TEXT-LEN + 1 SIZE
   [: PATCH-BODY ;] MEM:WITH-BYTES ;


\ Merge. A block is the changes of either side whose base ranges overlap or
\ touch: ours i..i2-1 and theirs j..j2-1 over base g0..g1.

STRUCTURE block
   FIELD i n
   FIELD i2 n
   FIELD j n
   FIELD j2 n
   FIELD g0 n
   FIELD g1 n
;STRUCTURE


\ Change g's base start, or past the end of base when s has no change g.
: START-OF ( ^script n -- n )
   {: s g :}
   g s SCRIPT-COUNT @ < if s g 0 G@ exit then
   s SCRIPT-A1 @ 1+ ;


: GROW ( ^script ^script n n n -- n n n )
   {: o t i0 j0 e0 :}
   i0 j0 e0 begin {: i j e :}
      o i START-OF e > 0= if i 1+ j e o i 1 G@ max true
      else t j START-OF e > 0= if i j 1+ e t j 1 G@ max true
      else i j e false then then
   while repeat ;


\ The lines of s's side standing for base g0..g1, which its changes first..last-1
\ fall inside.
: SIDE ( ^script n n n n -- n n )
   {: s first last g0 g1 :}
   s first 2 G@ s first 0 G@ g0 - -
   s last 1- 3 G@ g1 s last 1- 1 G@ - + ;


: SAME-RUN? ( ^work n n n n -- bool )
   {: w a ae b be :}
   ae a - be b - <> if false exit then
   ae a - 0 ?do w a i + ID@ w b i + ID@ <> if false unloop exit then loop
   true ;


\ Line ends in a conflict. git's merge matches CR LF texts: it ends the markers,
\ and a line end it adds, with CR LF when neither side's line before the
\ conflict, or first line, ends in LF alone and base's first line ends in CR LF.

: CRLF-LINE? ( ^work n -- bool )
   {: w g :}
   w g LINE$ {: line:str :}
   line STR:LEN 2 < if false exit then
   line STR:DATA line STR:LEN + {: end:^ :}
   end 2 - c@ STR:CR = end 1- c@ LF = and ;

: >EOL ( bool -- eol )  if construct eol crlf else construct eol lf then ;


\ The line end of the text in lines f0..f1-1 at its line g, read from the line
\ before when g is a last line without LF; unknown when the text is empty or
\ its only line has no LF.
: EOL ( ^work n n n -- eol )
   {: w f0 f1 g :}
   f0 f1 = if construct eol unknown exit then
   w g LINE$ ENDS-LF? if w g CRLF-LINE? >EOL exit then
   g f0 = if construct eol unknown exit then
   w g 1- CRLF-LINE? >EOL ;


\ The line end of a side in lines f0..f1-1 whose conflict starts at line at:
\ at the line before, or at the first line when it starts there.
: EOL-BEFORE ( ^work n n n -- eol )
   {: w f0 f1 at :}
   w f0 f1 at f0 > if at 1- else f0 then EOL ;


: LF-ONLY? ( eol -- bool )
   MATCH eol crlf OF false ENDOF lf OF true ENDOF unknown OF false ENDOF ;MATCH ;


: CRLF? ( eol -- bool )
   MATCH eol crlf OF true ENDOF lf OF false ENDOF unknown OF false ENDOF ;MATCH ;


: CRLF-NEEDED? ( ^work ^script ^script n n -- bool )
   {: w o t os ts :}
   w o SCRIPT-B0 @ o SCRIPT-B1 @ os EOL-BEFORE LF-ONLY? if false exit then
   w t SCRIPT-B0 @ t SCRIPT-B1 @ ts EOL-BEFORE LF-ONLY? if false exit then
   o SCRIPT-A0 @ {: g0 :}
   w g0 o SCRIPT-A1 @ g0 EOL CRLF? ;


: OUT-EOL ( ^work bool -- )
   {: w crlf :}
   crlf if w STR:CR OUT-BYTE then
   w LF OUT-BYTE ;


\ A marker line and lines lo..hi-1, the last ended if it has no line end.
: SECTION ( ^work str n n bool -- )
   {: w s:str lo hi crlf :}
   w s OUT$ w crlf OUT-EOL
   w lo hi COPY-LINES
   lo hi = if exit then
   w hi 1- LINE$ ENDS-LF? if exit then
   w crlf OUT-EOL ;


: CONFLICT ( ^work n n n n n n bool -- )
   {: w os oe g0 g1 ts te crlf :}
   w s" <<<<<<< ours" os oe crlf SECTION
   w s" ||||||| base" g0 g1 crlf SECTION
   w s" =======" ts te crlf SECTION
   w s" >>>>>>> theirs" OUT$ w crlf OUT-EOL
   1 w WORK-CONFLICTS +! ;


: RESOLVE ( ^work ^script ^script block -- )
   {: w o t b :}
   b BLOCK-UNMAKE {: i i2 j j2 g0 g1 :}
   i i2 = if w t j j2 g0 g1 SIDE COPY-LINES exit then
   j j2 = if w o i i2 g0 g1 SIDE COPY-LINES exit then
   o i i2 g0 g1 SIDE {: os oe :}
   t j j2 g0 g1 SIDE {: ts te :}
   w os oe ts te SAME-RUN? if w os oe COPY-LINES exit then
   w os oe g0 g1 ts te w o t os ts CRLF-NEEDED? CONFLICT ;


: MERGE-RUN ( ^work ^script ^script -- )
   {: w o t :}
   0 0 o SCRIPT-A0 @
   begin {: i j pos :}
      o i START-OF t j START-OF min {: g0 :}
      g0 o SCRIPT-A1 @ > if w pos o SCRIPT-A1 @ COPY-LINES exit then
      w pos g0 COPY-LINES
      o t i j g0 GROW {: i2 j2 g1 :}
      w o t i i2 j j2 g0 g1 BLOCK-MAKE RESOLVE
      i2 j2 g1
   again ;


: MERGE-BODY ( text text text ^VEC:vector<u8> ^u8 NUM:alloc-byte-len -- bool )
   {: a b c out base size :}
   a TEXT-LINES {: la :}
   la b TEXT-LINES + {: lb :}
   lb c TEXT-LINES + {: lines :}
   base size out lines OPEN {: w :}
   lines CUT-SCRIPT {: o :}
   lines CUT-SCRIPT {: t :}
   0 la la lb o RANGES!
   0 la lb lines t RANGES!
   w a TEXT-LEN b TEXT-LEN + c TEXT-LEN + LOAD-TEXTS
   w a 0 0 ADD-TEXT {: g at :}
   w b g at ADD-TEXT {: g2 at2 :}
   w c g2 at2 ADD-TEXT nip w swap lines FINISH
   w o RUN
   w t RUN
   w o t MERGE-RUN
   w WORK-CONFLICTS @ 0= ;


public

\ Append git's unified diff of path P's old and new text: `diff --git a/P b/P`,
\ `--- a/P`, `+++ b/P` and hunks with three lines of context, joined when
\ their context touches; nothing when the texts have the same lines. P is
\ quoted and hunk headers name a function line as git's are.
: WRITE-EDIT ( str str str ^VEC:vector<u8> -- )
   {: p:str a:str b:str out :}
   p a >TEXT b >TEXT construct kind edit out PATCH ;


\ The patch adding path P with this text: `new file mode 100644`, then
\ `--- /dev/null` and one hunk, which an empty text leaves out.
: WRITE-ADD ( str str ^VEC:vector<u8> -- )
   {: p:str b:str out :}
   p s" " >TEXT b >TEXT construct kind add out PATCH ;


\ The patch deleting path P, whose text this was: `deleted file mode 100644`,
\ then `+++ /dev/null` and one hunk, which an empty text leaves out.
: WRITE-DELETE ( str str ^VEC:vector<u8> -- )
   {: p:str a:str out :}
   p a >TEXT s" " >TEXT construct kind delete out PATCH ;


\ Append the three-way merge of base, ours and theirs by line, by diff3's rules:
\ a change on one side applies, the same change on both applies once, and
\ changes whose base ranges overlap or touch and differ conflict. True when
\ nothing conflicts; false when the text holds git's diff3 markers around each
\ conflict (`<<<<<<< ours`, `||||||| base`, `=======`, `>>>>>>> theirs`), every
\ section ending in a line end, CR LF where git's merge writes one.
: MERGE ( str str str ^VEC:vector<u8> -- bool )
   {: a:str b:str c:str out :}
   a >TEXT {: ta :}
   b >TEXT {: tb :}
   c >TEXT {: tc :}
   ta tb tc out
   ta TEXT-LINES tb TEXT-LINES + tc TEXT-LINES +
   a STR:LEN b STR:LEN + c STR:LEN + 2 SIZE
   [: MERGE-BODY ;] MEM:WITH-BYTES ;

;package
