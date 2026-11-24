\ regex.f - regular expressions, package REGEX, matched with no backtracking.
\ COMPILE reads a UTF-8 pattern into a program; MATCH? answers whether the
\ program matches anywhere in a UTF-8 text, in time the text's length times
\ the program's size.
\
\ DIALECT. ECMA-262 with the u flag, as JSON Schema 2020-12's `pattern` asks,
\ over code points: literal characters; `.`, any code point but U+000A,
\ U+000D, U+2028 and U+2029; `^` and `$`, the text's start and end; `|`;
\ `( )` and `(?: )`, which capture nothing; `*` `+` `?` `{n}` `{n,}` `{n,m}`,
\ each optionally followed by `?`, which gives the same answer; classes `[ ]`
\ and `[^ ]` with ranges, `-` literal first or last; the escapes `\t \n \v \f
\ \r`, `\0` not before a digit, `\uXXXX` (a high and low surrogate pair in
\ sequence is one code point), `\u{X...}` up to 10FFFF, `\d \D \w \W \s \S`
\ (`\s` is ECMA's WhiteSpace and LineTerminator), and `\` before any of
\ `^ $ \ . * + ? ( ) [ ] { } | / -`; in a class `\b` is U+0008. COMPILE
\ rejects the rest: backreferences, lookahead, lookbehind, named groups,
\ `\b \B \p \P \c`, any other escape, an unbalanced group or class, a
\ quantifier with nothing to repeat, `{n,m}` with m < n or a count a cell
\ does not hold, a reversed range, and a byte that is not UTF-8.
\
\ COMPILE reads the pattern in one pass with no recursion, a stack of open
\ groups in a scratch arena, into a tree whose nodes know their instruction
\ counts; it then lays out Thompson's construction as an instruction array, a
\ split's targets known from the sizes. The layout runs on an explicit stack
\ of work in the scratch arena, a node to lay out or a split or jump to put,
\ so nothing recurses as deep as the pattern nests.
\ MATCH? is a Pike VM: the threads at a text offset are a list of program
\ counters, each added once per offset through a generation mark per
\ instruction, and a new thread starts at every offset. The closure over
\ split, jump and the assertions runs on an explicit stack the program's
\ size bounds, so nothing recurses as deep as the text is long.
\
\ CONTRACT. A rejected pattern throws E-PATTERN through REASON:THROW with the
\ text `C: expected X, found Y; fix`, C the 1-based byte column of Y. Y is
\ one character or a run cut at 32 bytes with `...`, made visible; a byte
\ that is no UTF-8 character as `byte $HH`; or `the end of the pattern`. A text byte that
\ starts no valid UTF-8 sequence reads as U+FFFD. A counted repetition
\ repeats its operand's code, so a program's size is the product of its
\ nested counts; memory COMPILE or MATCH? cannot have throws its code.
\ STORAGE CLASS. CALLER-OWNED: a program lives in the arena COMPILE was
\ given, read-only after COMPILE. MATCH? keeps no state; its scratch is one
\ MEM:WITH-BYTES scope, freed on every path.

require src/lib/error.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/arena.f
require src/lib/utf8-scalar.f
require src/lib/reason.f
require src/lib/sort.f

package REGEX
private

-9940 constant E-FIRST
-9949 constant E-LAST

ENUM opcode single class any start end split jump accept ;ENUM

\ x: single's code point, class's pair count, split's and jump's target; y:
\ split's other target. ranges: class's lo hi pairs, ascending, not touching.
STRUCTURE inst DERIVE addr
   FIELD op opcode
   FIELD x n
   FIELD y n
   FIELD ranges ^n
   FIELD negated bool
;STRUCTURE

public

-9940 s" the pattern is not one the regular expression dialect takes"
ERROR:CODE E-PATTERN

STRUCTURE regex OPAQUE
   FIELD code ^inst
   FIELD size n
;STRUCTURE

private

$10FFFF constant MOST-POINT
$FFFD constant REPLACEMENT
$5C constant BACKSLASH
-1 constant UNBOUNDED
\ Sizes stop here, so none wraps: the arena refuses a program this large
\ with E-MEM-MAP.
MEM-MAX-N INST-BYTES / 4 / constant SIZE-CAP
\ The most pairs one class byte adds: `\S` adds 11 for its 2 bytes.
6 constant PAIRS-PER-BYTE

CAST: >INSTS ( ^u8 -- ^inst )


\ ---- the tree --------------------------------------------------------------

ENUM kind atom sequence choice counted ;ENUM

\ An atom's op, x, ranges and negated are inst's; members link from first
\ through next; counted repeats first x to y times; size counts instructions.
STRUCTURE node DERIVE addr
   FIELD form kind
   FIELD op opcode
   FIELD x n
   FIELD y n
   FIELD ranges ^n
   FIELD negated bool
   FIELD first ^node
   FIELD next ^node
   FIELD size n
;STRUCTURE

\ An open group. last is seq's last member, NULL-PTR before the first;
\ repeatable is whether a quantifier may follow it.
STRUCTURE frame DERIVE addr
   FIELD choice ^node
   FIELD seq ^node
   FIELD last ^node
   FIELD repeatable bool
   FIELD outer ^frame
;STRUCTURE

\ The pattern, the offset read next, the innermost open group, and the
\ pairs of the class being read.
STRUCTURE parser DERIVE addr
   FIELD src ^u8
   FIELD len n
   FIELD pos n
   FIELD scratch ^ARENA:arena
   FIELD top ^frame
   FIELD ranges ^n
   FIELD count n
;STRUCTURE

ARENA NODES node
ARENA FRAMES frame
ARENA PARSERS parser

: TEXT@ ( ^parser n -- n ) swap PARSER-SRC @ + c@ ;
: POS ( ^parser -- n ) PARSER-POS @ ;
: SKIP ( ^parser n -- ) swap PARSER-POS +! ;

\ The byte k past the offset read next, -1 past the end.
: BYTE-AT ( ^parser n -- n ) {: p:^ k:n :}
   p POS k + {: at:n :}
   at p PARSER-LEN @ >= if -1 exit then
   p at TEXT@ ;

: PEEK ( ^parser -- n ) 0 BYTE-AT ;

\ The byte width of the character at offset at, 1 for a byte that starts
\ none, 0 at the end.
: WIDTH ( ^parser n -- n ) {: p:^ at:n :}
   at p PARSER-LEN @ >= if 0 exit then
   p PARSER-SRC @ p PARSER-LEN @ >str at UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip at - ENDOF
      raw-byte OF 2drop 1 ENDOF
   ;MATCH ;


\ ---- rejections ------------------------------------------------------------

\ A byte a found text never shows as itself.
: RAW? ( n -- bool ) {: b:n :} b $20 < b $7F >= or ;

\ The bytes of the token a found text keeps.
: KEPT ( str -- str )
   dup STR:DATA swap REASON:QUOTE-MOST UTF8:FIT >str ;

: PUT-FOUND ( ^parser n n -- ) {: p:^ at:n u:n :}
   u 0= if s" the end of the pattern" REASON:APPEND exit then
   p PARSER-SRC @ at + {: tok:^ :}
   u 1 = tok c@ RAW? and if tok c@ REASON:BYTE exit then
   tok u >str KEPT {: kept:str :}
   kept  kept STR:LEN u <  REASON:QUOTE ;

\ Throws E-PATTERN with the text `C: expected e, found Y; f`, Y the u bytes at
\ offset at, the pattern's end when u is 0.
: REJECT ( ^parser n n str str -- )
   {: p:^ at:n u:n e:str f:str :}
   REASON:OPEN
   at 1+ FMT:INT$ REASON:APPEND  s" : " REASON:APPEND
   REASON:EXPECTED e REASON:APPEND
   REASON:FOUND p at u PUT-FOUND
   REASON:FIX f REASON:APPEND
   E-PATTERN REASON:STOP ;

\ Rejects the character at the offset read next.
: REJECT-HERE ( ^parser str str -- )
   {: p:^ e:str f:str :}
   p p POS  p p POS WIDTH  e f REJECT ;

: REJECT-END ( ^parser str str -- )
   {: p:^ e:str f:str :}
   p p PARSER-LEN @ 0 e f REJECT ;

: NOT-UTF8 ( ^parser -- )
   s" a UTF-8 character" s" write the pattern as UTF-8" REJECT-HERE ;

: STRAY ( ^parser -- )
   s" a character, class or group"
   s" put `\` before it to match the character" REJECT-HERE ;

: NOTHING-TO-REPEAT ( ^parser -- )
   s" a character, class or group before the quantifier"
   s" put `\` before it to match the character" REJECT-HERE ;

: UNCLOSED-GROUP ( ^parser -- )
   s" `)` to close a group" s" close each `(` with `)`" REJECT-END ;

: UNCLOSED-CLASS ( ^parser -- )
   s" `]` to close a class"
   s" close the class with `]`, or write `\[` to match `[`" REJECT-END ;

: UNCLOSED-BRACE ( ^parser -- )
   s" `}` to close the quantifier"
   s" write `{n}`, `{n,}` or `{n,m}`" REJECT-HERE ;

: NO-COUNT ( ^parser -- )
   s" a count"
   s" write `{n}`, `{n,}` or `{n,m}`, or `\{` to match `{`"
   REJECT-HERE ;

: TOO-BIG ( ^parser n n -- )
   s" a count a cell holds" s" write a smaller count" REJECT ;

: FEWER ( ^parser n n -- )
   s" a most count no less than the least"
   s" write the smaller count first" REJECT ;

: LOOKAHEAD ( ^parser n -- )
   3 s" a group other than a lookahead"
   s" rewrite the pattern without it" REJECT ;

: LOOKBEHIND ( ^parser n -- )
   4 s" a group other than a lookbehind"
   s" rewrite the pattern without it" REJECT ;

: NAMED ( ^parser n -- )
   3 s" a group without a name"
   s" write `(` or `(?:` in its place" REJECT ;

: NOT-COLON ( ^parser -- )
   s" `:` after `(?`"
   s" write `(?:` for a group that captures nothing" REJECT-HERE ;

: BACKREFERENCE ( ^parser n n -- )
   s" an escape other than a backreference"
   s" repeat the group's pattern in place of the reference" REJECT ;

: BOUNDARY ( ^parser n -- )
   2 s" an escape other than a word boundary"
   s" rewrite the pattern without it" REJECT ;

: PROPERTY ( ^parser n -- )
   2 s" an escape other than a Unicode property"
   s" write the property's characters as a class of ranges" REJECT ;

: CONTROL ( ^parser n -- )
   2 s" an escape other than a control letter"
   s" write the character as `\uXXXX`" REJECT ;

: OTHER-ESCAPE ( ^parser n n -- )
   s" an escape the dialect takes"
   s" write the character itself or as `\uXXXX`" REJECT ;

: ZERO-DIGIT ( ^parser n -- )
   3 s" `\0` with no digit after it"
   s" write `\u0000` for the NUL character" REJECT ;

: TRAILING-BACKSLASH ( ^parser -- )
   s" a character after `\`" s" write `\\` to match `\`" REJECT-END ;

: NOT-HEX ( ^parser -- )
   s" a hex digit"
   s" write `\uXXXX` or `\u{X...}` with hex digits" REJECT-HERE ;

: PAST-UNICODE ( ^parser n n -- )
   s" a code point at most 10FFFF"
   s" write a code point Unicode assigns" REJECT ;

: UNCLOSED-POINT ( ^parser -- )
   s" `}` to close `\u{`" s" write `\u{X...}`" REJECT-HERE ;

: REVERSED ( ^parser n n -- )
   s" a range end no less than its start"
   s" write the range's lower end first" REJECT ;

: SET-END ( ^parser n n -- )
   s" a character as the range's end"
   s" write `\-` to match `-`" REJECT ;

: SET-START ( ^parser -- )
   s" a class escape alone, not a range's start"
   s" write `\-` to match `-`" REJECT-HERE ;


\ ---- reading characters ----------------------------------------------------

\ The code point at the offset read next and its byte width.
: DECODE ( ^parser -- n n ) {: p:^ :}
   p PARSER-SRC @ p PARSER-LEN @ >str p POS UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF p POS - ENDOF
      raw-byte OF 2drop p NOT-UTF8 ENDOF
   ;MATCH ;

: TAKE-POINT ( ^parser -- n ) {: p:^ :}
   p DECODE p swap SKIP ;

: DIGIT? ( n -- bool ) dup [char] 0 >= swap [char] 9 <= and ;

\ A hex digit's value, -1 for any other byte.
: HEX ( n -- n ) {: b:n :}
   b DIGIT? if b [char] 0 - exit then
   b $20 or {: c:n :}
   c [char] a >= c [char] f <= and if c [char] a - 10 + exit then
   -1 ;

: DIGIT-RUN ( ^parser n -- n ) {: p:^ at:n :}
   at begin
      dup p PARSER-LEN @ < if p over TEXT@ DIGIT? else false then
   while 1+ repeat
   at - ;

: HEX-RUN ( ^parser n -- n ) {: p:^ at:n :}
   at begin
      dup p PARSER-LEN @ < if p over TEXT@ HEX 0 >= else false then
   while 1+ repeat
   at - ;

\ v * 10 + d, and false when a cell does not hold it.
: SHIFT-IN ( n n -- n bool ) {: v:n d:n :}
   v MEM-MAX-N d - 10 / > if 0 false exit then
   v 10 * d + true ;

: DIGITS>N ( ^parser n n -- n ) {: p:^ at:n u:n :}
   0 at u + at ?do
      p i TEXT@ [char] 0 - SHIFT-IN
      0= if drop p at u TOO-BIG then
   loop ;

: READ-COUNT ( ^parser -- n ) {: p:^ :}
   p POS {: at:n :}
   p at DIGIT-RUN {: u:n :}
   u 0= if p NO-COUNT then
   p at u DIGITS>N
   p u SKIP ;


\ ---- escapes ---------------------------------------------------------------

ENUM charset digit word space ;ENUM

ENUM escaped
   VARIANT one FIELD point n ;VARIANT
   VARIANT many FIELD which charset FIELD inverse bool ;VARIANT
;ENUM

: SET-LETTER? ( n -- bool ) $20 or {: c:n :}
   c [char] d = c [char] w = or c [char] s = or ;

\ `\d \w \s` and, upper case, their complements.
: SET-ESCAPE ( n -- escaped ) {: c:n :}
   c $20 or {: lower:n :}
   lower [char] d = if CHARSET-DIGIT else
      lower [char] w = if CHARSET-WORD else CHARSET-SPACE then
   then
   c lower <> ESCAPED-MANY ;

\ `\t \n \v \f \r`'s code points, -1 for any other letter.
: CONTROL-LETTER ( n -- n ) {: c:n :}
   c [char] t = if 9 exit then
   c [char] n = if $A exit then
   c [char] v = if $B exit then
   c [char] f = if $C exit then
   c [char] r = if $D exit then
   -1 ;

: SYNTAX? ( n -- bool ) {: c:n :}
   s" ^$\.*+?()[]{}|/-" {: set:str :}
   set STR:LEN 0 ?do
      set STR:DATA i + c@ c = if unloop true exit then
   loop
   false ;

\ At `0` after the backslash at offset at.
: ZERO ( ^parser n -- n ) {: p:^ at:n :}
   p 1 BYTE-AT DIGIT? if p at ZERO-DIGIT then
   p 1 SKIP 0 ;

: HEX4 ( ^parser -- n ) {: p:^ :}
   0 4 0 ?do
      p PEEK HEX dup 0< if drop p NOT-HEX then
      swap 4 lshift or  p 1 SKIP
   loop ;

: HIGH? ( n -- bool ) dup $D800 >= swap $DBFF <= and ;
: LOW? ( n -- bool ) dup $DC00 >= swap $DFFF <= and ;

\ The low surrogate a `\uXXXX` at the offset read next holds, else -1.
: LOW-FOLLOWS ( ^parser -- n ) {: p:^ :}
   p 0 BYTE-AT BACKSLASH <> if -1 exit then
   p 1 BYTE-AT [char] u <> if -1 exit then
   0 6 2 ?do
      p i BYTE-AT HEX dup 0< if 2drop unloop -1 exit then
      swap 4 lshift or
   loop
   dup LOW? 0= if drop -1 then ;

: PAIR>POINT ( n n -- n ) {: hi:n lo:n :}
   hi $D800 - 10 lshift lo $DC00 - or $10000 + ;

\ After `\u{`: hex digits up to MOST-POINT, then `}`.
: BRACED ( ^parser -- n ) {: p:^ :}
   p POS {: at:n :}
   p at HEX-RUN {: u:n :}
   u 0= if p NOT-HEX then
   0 at u + at ?do
      p i TEXT@ HEX swap 4 lshift or
      dup MOST-POINT > if drop p at u PAST-UNICODE then
   loop
   p u SKIP
   p PEEK [char] } <> if p UNCLOSED-POINT then
   p 1 SKIP ;

\ At `u` after the backslash.
: UNICODE ( ^parser -- n ) {: p:^ :}
   p 1 SKIP
   p PEEK [char] { = if p 1 SKIP p BRACED exit then
   p HEX4 {: hi:n :}
   hi HIGH? 0= if hi exit then
   p LOW-FOLLOWS {: lo:n :}
   lo 0< if hi exit then
   p 6 SKIP
   hi lo PAIR>POINT ;

\ An escape the dialect rejects, c the byte after the backslash at at.
: BAD-ESCAPE ( ^parser n n -- ) {: p:^ at:n c:n :}
   c DIGIT? if p at p at 1+ DIGIT-RUN 1+ BACKREFERENCE then
   c $20 or [char] b = if p at BOUNDARY then
   c $20 or [char] p = if p at PROPERTY then
   c [char] c = if p at CONTROL then
   c RAW? p at 1+ WIDTH 1 = and if p at 1+ 1 OTHER-ESCAPE then
   p at p at 1+ WIDTH 1+ OTHER-ESCAPE ;

\ At a backslash; in-class reads `\b` as U+0008.
: ESCAPE ( ^parser bool -- escaped ) {: p:^ in-class:bool :}
   p POS {: at:n :}
   p 1 SKIP
   p PEEK {: c:n :}
   c 0< if p TRAILING-BACKSLASH then
   c SET-LETTER? if p 1 SKIP c SET-ESCAPE exit then
   c CONTROL-LETTER {: v:n :}
   v 0 >= if p 1 SKIP v ESCAPED-ONE exit then
   c [char] b = in-class and if p 1 SKIP 8 ESCAPED-ONE exit then
   c [char] 0 = if p at ZERO ESCAPED-ONE exit then
   c [char] u = if p UNICODE ESCAPED-ONE exit then
   c SYNTAX? if p 1 SKIP c ESCAPED-ONE exit then
   p at c BAD-ESCAPE ;


\ ---- classes ---------------------------------------------------------------

: CELL@ ( ^n n -- n ) cells + @ ;
: CELL! ( n ^n n -- ) cells + ! ;
: PAIR-AT ( ^n n -- ^n ) 2 * cells + ;
: PAIR@ ( ^n n -- n n ) PAIR-AT dup @ swap cell+ @ ;
: PAIR! ( n n ^n n -- ) PAIR-AT {: q:^ :} q cell+ ! q ! ;

\ Room for the pairs of a class of k bytes, none taken yet.
: OPEN-RANGES ( ^parser n -- ) {: p:^ k:n :}
   p PARSER-SCRATCH @ k PAIRS-PER-BYTE * 2 + 2 * cells ARENA:ALLOC
   drop CELL-VIEW p PARSER-RANGES !
   0 p PARSER-COUNT ! ;

: ADD-RANGE ( ^parser n n -- ) {: p:^ lo:n hi:n :}
   lo hi p PARSER-RANGES @ p PARSER-COUNT @ PAIR!
   1 p PARSER-COUNT +! ;

: DIGIT-RANGES ( ^parser -- ) [char] 0 [char] 9 ADD-RANGE ;

: WORD-RANGES ( ^parser -- ) {: p:^ :}
   p [char] 0 [char] 9 ADD-RANGE
   p [char] A [char] Z ADD-RANGE
   p [char] _ dup ADD-RANGE
   p [char] a [char] z ADD-RANGE ;

: SPACE-RANGES ( ^parser -- ) {: p:^ :}
   p $9 $D ADD-RANGE  p $20 $20 ADD-RANGE  p $A0 $A0 ADD-RANGE
   p $1680 $1680 ADD-RANGE  p $2000 $200A ADD-RANGE
   p $2028 $2029 ADD-RANGE  p $202F $202F ADD-RANGE
   p $205F $205F ADD-RANGE  p $3000 $3000 ADD-RANGE
   p $FEFF $FEFF ADD-RANGE ;

: SET-RANGES ( ^parser charset -- ) {: p:^ which :}
   which MATCH charset
      digit OF p DIGIT-RANGES ENDOF
      word OF p WORD-RANGES ENDOF
      space OF p SPACE-RANGES ENDOF
   ;MATCH ;

\ Adds the gap below lo, from the code point from on; answers hi + 1.
: GAP ( ^parser n n n -- n ) {: p:^ from:n lo:n hi:n :}
   lo from > if p from lo 1- ADD-RANGE then
   hi 1+ ;

\ Replaces the pairs from start, ascending and apart, with the code points
\ they leave out, in place: gap j is written after pair j is read.
: COMPLEMENT ( ^parser n -- ) {: p:^ start:n :}
   p PARSER-COUNT @ {: end:n :}
   start p PARSER-COUNT !
   0 end start ?do
      p swap p PARSER-RANGES @ i PAIR@ GAP
   loop
   {: from:n :}
   from MOST-POINT <= if p from MOST-POINT ADD-RANGE then ;

: ADD-SET ( ^parser charset bool -- ) {: p:^ which inv:bool :}
   p PARSER-COUNT @ {: start:n :}
   p which SET-RANGES
   inv if p start COMPLEMENT then ;

\ Pair j packed into cell j, lo above hi: code points take 21 bits.
: PACK-AT ( ^n n -- ) {: r:^ j:n :}
   r j PAIR@ swap 32 lshift or  r j CELL! ;

: UNPACK-AT ( ^n n -- ) {: r:^ j:n :}
   r j CELL@ {: v:n :}
   v 32 rshift  v $FFFFFFFF and  r j PAIR! ;

\ Unpacks from the last pair, so each packed cell is read before a pair
\ covers it.
: SORT-PAIRS ( ^n n -- ) {: r:^ k:n :}
   k 0 ?do r i PACK-AT loop
   r k [: < ;] SORT:SORT!
   k 0 ?do r k 1- i - UNPACK-AT loop ;

\ Whether a pair starting at lo overlaps or touches the last of kept.
: JOINS? ( ^n n n -- bool ) {: r:^ kept:n lo:n :}
   kept 0 > if r kept 1- PAIR@ nip 1+ lo >= else false then ;

: MERGE-PAIR ( ^n n n -- n ) {: r:^ kept:n i:n :}
   r i PAIR@ {: lo:n hi:n :}
   r kept lo JOINS? if
      r kept 1- PAIR@ hi max r kept 1- PAIR!  kept exit
   then
   lo hi r kept PAIR!  kept 1+ ;

\ Sorts the pairs and joins those that overlap or touch; answers how many
\ are left.
: NORMALIZE ( ^n n -- n ) {: r:^ k:n :}
   r k SORT-PAIRS
   0 k 0 ?do r swap i MERGE-PAIR loop ;


\ ---- the tree's nodes ------------------------------------------------------

: NEW-NODE ( ^parser kind -- ^node ) {: p:^ form :}
   p PARSER-SCRATCH @ NODES-NEW {: nd:^ :}
   form nd NODE-FORM !  nd ;

: ATOM ( ^parser opcode n -- ^node ) {: p:^ op x:n :}
   p KIND-ATOM NEW-NODE {: nd:^ :}
   op nd NODE-OP !  x nd NODE-X !  1 nd NODE-SIZE !  nd ;

: CLASS-NODE ( ^parser bool -- ^node ) {: p:^ neg:bool :}
   p PARSER-RANGES @ p PARSER-COUNT @ NORMALIZE {: k:n :}
   p OPCODE-CLASS k ATOM {: nd:^ :}
   p PARSER-RANGES @ nd NODE-RANGES !
   neg nd NODE-NEGATED !  nd ;

: SET-NODE ( charset bool ^parser -- ^node ) {: which inv:bool p:^ :}
   p 2 OPEN-RANGES  p which inv ADD-SET  p false CLASS-NODE ;

: ESCAPED-NODE ( ^parser escaped -- ^node ) {: p:^ e :}
   e MATCH escaped
      one OF p OPCODE-SINGLE rot ATOM ENDOF
      many OF p SET-NODE ENDOF
   ;MATCH ;

: COPY-NODE ( ^parser ^node -- ^node ) {: p:^ from:^ :}
   p from NODE-FORM @ NEW-NODE {: nd:^ :}
   from NODE-OP @ nd NODE-OP !  from NODE-X @ nd NODE-X !
   from NODE-Y @ nd NODE-Y !  from NODE-RANGES @ nd NODE-RANGES !
   from NODE-NEGATED @ nd NODE-NEGATED !
   from NODE-FIRST @ nd NODE-FIRST !
   from NODE-SIZE @ nd NODE-SIZE !  nd ;

: SAT+ ( n n -- n ) + SIZE-CAP min ;

: SAT* ( n n -- n ) {: a:n b:n :}
   b 0= if 0 exit then
   a SIZE-CAP b / > if SIZE-CAP exit then
   a b * SIZE-CAP min ;

\ The members' sizes added, and how many there are.
: MEMBERS ( ^node -- n n )
   0 0 rot NODE-FIRST @ begin dup NULL-PTR <> while
      >r swap r@ NODE-SIZE @ SAT+ swap 1+ r> NODE-NEXT @
   repeat
   drop ;

: SEAL-SEQ ( ^node -- ) {: nd:^ :}
   nd MEMBERS drop nd NODE-SIZE ! ;

\ Every member but the last takes a split before it and a jump after it.
: SEAL-CHOICE ( ^node -- ) {: nd:^ :}
   nd MEMBERS 1- 2 SAT* SAT+ nd NODE-SIZE ! ;

\ s instructions repeated least to most times.
: COUNTED-SIZE ( n n n -- n ) {: s:n least:n most:n :}
   s least SAT*
   most UNBOUNDED = if s 2 SAT+ else s 1+ most least - SAT* then
   SAT+ ;


\ ---- reading the pattern ---------------------------------------------------

: OPEN-FRAME ( ^parser -- ) {: p:^ :}
   p PARSER-SCRATCH @ FRAMES-NEW {: f:^ :}
   p KIND-CHOICE NEW-NODE f FRAME-CHOICE !
   p KIND-SEQUENCE NEW-NODE f FRAME-SEQ !
   p PARSER-TOP @ f FRAME-OUTER !
   f p PARSER-TOP ! ;

: APPEND ( ^parser ^node bool -- ) {: p:^ nd:^ rep:bool :}
   p PARSER-TOP @ {: f:^ :}
   f FRAME-LAST @ {: l:^ :}
   l NULL-PTR = if
      nd f FRAME-SEQ @ NODE-FIRST !
   else nd l NODE-NEXT ! then
   nd f FRAME-LAST !
   rep f FRAME-REPEATABLE ! ;

\ Seals the sequence being read into the frame's choice, which holds its
\ members last first: their order does not change an answer.
: CLOSE-SEQ ( ^frame -- ) {: f:^ :}
   f FRAME-SEQ @ {: s:^ :}
   s SEAL-SEQ
   f FRAME-CHOICE @ {: c:^ :}
   c NODE-FIRST @ s NODE-NEXT !
   s c NODE-FIRST ! ;

: BAR ( ^parser -- ) {: p:^ :}
   p 1 SKIP
   p PARSER-TOP @ {: f:^ :}
   f CLOSE-SEQ
   p KIND-SEQUENCE NEW-NODE f FRAME-SEQ !
   NULL-PTR f FRAME-LAST !
   false f FRAME-REPEATABLE ! ;

\ At `(?`, at the `(`: `(?:` reads on; every other form is rejected.
: GROUP-FORM ( ^parser -- ) {: p:^ :}
   p POS {: at:n :}
   p 2 BYTE-AT {: c:n :}
   c [char] : = if p 3 SKIP exit then
   c [char] = = c [char] ! = or if p at LOOKAHEAD then
   c [char] < <> if p 2 SKIP p NOT-COLON then
   p 3 BYTE-AT {: d:n :}
   d [char] = = d [char] ! = or if p at LOOKBEHIND then
   p at NAMED ;

: OPEN-GROUP ( ^parser -- ) {: p:^ :}
   p 1 BYTE-AT [char] ? = if p GROUP-FORM else p 1 SKIP then
   p OPEN-FRAME ;

: CLOSE-GROUP ( ^parser -- ) {: p:^ :}
   p PARSER-TOP @ {: f:^ :}
   f FRAME-OUTER @ NULL-PTR = if p STRAY then
   p 1 SKIP
   f CLOSE-SEQ
   f FRAME-CHOICE @ {: c:^ :}
   c SEAL-CHOICE
   f FRAME-OUTER @ p PARSER-TOP !
   p c true APPEND ;

: QUANTIFIER? ( n -- bool ) {: c:n :}
   c [char] * = c [char] + = or c [char] ? = or c [char] { = or ;

\ At `{`: `{n}`, `{n,}` or `{n,m}`.
: BRACES ( ^parser -- n n ) {: p:^ :}
   p 1 SKIP
   p READ-COUNT {: least:n :}
   p PEEK [char] } = if p 1 SKIP least least exit then
   p PEEK [char] , <> if p UNCLOSED-BRACE then
   p 1 SKIP
   p PEEK [char] } = if p 1 SKIP least UNBOUNDED exit then
   p POS {: at:n :}
   p READ-COUNT {: most:n :}
   most least < if p at p POS at - FEWER then
   p PEEK [char] } <> if p UNCLOSED-BRACE then
   p 1 SKIP least most ;

\ A quantifier's least and most counts; a `?` after it is read and changes
\ nothing.
: BOUNDS ( ^parser -- n n ) {: p:^ :}
   p PEEK {: c:n :}
   c [char] { = if p BRACES else
      p 1 SKIP
      c [char] * = if 0 UNBOUNDED else
         c [char] + = if 1 UNBOUNDED else 0 1 then
      then
   then
   p PEEK [char] ? = if p 1 SKIP then ;

\ Turns the frame's last member into a counted node over a copy of itself,
\ so the member list keeps its links.
: WRAP-LAST ( ^parser n n -- ) {: p:^ least:n most:n :}
   p PARSER-TOP @ FRAME-LAST @ {: l:^ :}
   p l COPY-NODE {: e:^ :}
   KIND-COUNTED l NODE-FORM !
   e l NODE-FIRST !
   least l NODE-X !  most l NODE-Y !
   e NODE-SIZE @ least most COUNTED-SIZE l NODE-SIZE ! ;

: QUANTIFY ( ^parser -- ) {: p:^ :}
   p PARSER-TOP @ {: f:^ :}
   f FRAME-REPEATABLE @ 0= if p NOTHING-TO-REPEAT then
   p p BOUNDS WRAP-LAST
   false f FRAME-REPEATABLE ! ;

\ At `[`: the `]` that closes it, which no escaped byte is.
: CLASS-END ( ^parser -- n ) {: p:^ :}
   p POS 1+
   begin
      dup p PARSER-LEN @ >= if p UNCLOSED-CLASS then
      p over TEXT@ dup [char] ] <>
   while
      BACKSLASH = if 2 + else 1+ then
   repeat
   drop ;

: CLASS-ATOM ( ^parser -- escaped ) {: p:^ :}
   p PEEK BACKSLASH = if p true ESCAPE exit then
   p TAKE-POINT ESCAPED-ONE ;

\ Whether a `-` that makes a range is next.
: RANGE-DASH? ( ^parser -- bool ) {: p:^ :}
   p PEEK [char] - = p 1 BYTE-AT [char] ] <> and ;

: RANGE-END ( n ^parser n n -- ) {: hi:n p:^ at:n lo:n :}
   hi lo < if p at p POS at - REVERSED then
   p lo hi ADD-RANGE ;

: ONE-OR-RANGE ( ^parser n -- ) {: p:^ lo:n :}
   p RANGE-DASH? 0= if p lo lo ADD-RANGE exit then
   p 1 SKIP
   p POS {: at:n :}
   p CLASS-ATOM MATCH escaped
      one OF p at lo RANGE-END ENDOF
      many OF 2drop p at p POS at - SET-END ENDOF
   ;MATCH ;

: SET-ITEM ( charset bool ^parser -- ) {: which inv:bool p:^ :}
   p RANGE-DASH? if p SET-START then
   p which inv ADD-SET ;

: CLASS-ITEM ( ^parser -- ) {: p:^ :}
   p CLASS-ATOM MATCH escaped
      one OF p swap ONE-OR-RANGE ENDOF
      many OF p SET-ITEM ENDOF
   ;MATCH ;

: CLASS ( ^parser -- ^node ) {: p:^ :}
   p p CLASS-END p POS - OPEN-RANGES
   p 1 SKIP
   p PEEK [char] ^ = dup if p 1 SKIP then {: neg:bool :}
   begin p PEEK [char] ] <> while p CLASS-ITEM repeat
   p 1 SKIP
   p neg CLASS-NODE ;

: ITEM ( ^parser -- ^node ) {: p:^ :}
   p PEEK {: c:n :}
   c [char] . = if p 1 SKIP p OPCODE-ANY 0 ATOM exit then
   c [char] [ = if p CLASS exit then
   c BACKSLASH = if p p false ESCAPE ESCAPED-NODE exit then
   c [char] ] = c [char] } = or if p STRAY then
   p TAKE-POINT {: cp:n :}
   p OPCODE-SINGLE cp ATOM ;

: ASSERTION ( ^parser opcode -- ) {: p:^ op :}
   p 1 SKIP
   p p op 0 ATOM false APPEND ;

: STEP ( ^parser -- ) {: p:^ :}
   p PEEK {: c:n :}
   c [char] | = if p BAR exit then
   c [char] ( = if p OPEN-GROUP exit then
   c [char] ) = if p CLOSE-GROUP exit then
   c QUANTIFIER? if p QUANTIFY exit then
   c [char] ^ = if p OPCODE-START ASSERTION exit then
   c [char] $ = if p OPCODE-END ASSERTION exit then
   p p ITEM true APPEND ;

: READ-PATTERN ( ^parser -- ^node ) {: p:^ :}
   p OPEN-FRAME
   begin p POS p PARSER-LEN @ < while p STEP repeat
   p PARSER-TOP @ {: f:^ :}
   f FRAME-OUTER @ NULL-PTR <> if p UNCLOSED-GROUP then
   f CLOSE-SEQ
   f FRAME-CHOICE @ {: c:^ :}
   c SEAL-CHOICE  c ;


\ ---- laying out the program ------------------------------------------------

ENUM act lay split jump ;ENUM

\ One piece of work: lay's tree is the node to lay out; dx and dy are a
\ split's or jump's targets less the pc it is put at.
STRUCTURE work DERIVE addr
   FIELD form act
   FIELD tree ^node
   FIELD dx n
   FIELD dy n
   FIELD below ^work
;STRUCTURE

ARENA WORKS work

\ The arena the program goes in, its instructions and the next one's index;
\ the work stack, the node's work being made, newest first, and the records
\ free for reuse, each a chain through below.
STRUCTURE gen DERIVE addr
   FIELD arena ^ARENA:arena
   FIELD code ^inst
   FIELD pc n
   FIELD scratch ^ARENA:arena
   FIELD top ^work
   FIELD pending ^work
   FIELD spare ^work
;STRUCTURE

ARENA GENS gen

: HERE-INST ( ^gen -- ^inst ) dup GEN-CODE @ swap GEN-PC @ INST-AT ;

: PUT-OP ( ^gen opcode n n -- ) {: g:^ op x:n y:n :}
   g HERE-INST {: i:^ :}
   op i INST-OP !  x i INST-X !  y i INST-Y !
   1 g GEN-PC +! ;

\ Moves the first record of one chain onto another.
: MOVE-WORK ( ^^work ^^work -- ) {: from:^ to:^ :}
   from @ {: w:^ :}
   w WORK-BELOW @ from !
   to @ w WORK-BELOW !
   w to ! ;

\ A record put first on the pending chain, a spare one when there is one.
: PEND ( ^gen -- ^work ) {: g:^ :}
   g GEN-SPARE @ NULL-PTR = if
      g GEN-SCRATCH @ WORKS-NEW g GEN-SPARE !
   then
   g GEN-SPARE g GEN-PENDING MOVE-WORK
   g GEN-PENDING @ ;

: PEND-NODE ( ^gen ^node -- ) {: g:^ nd:^ :}
   nd NODE-SIZE @ 0= if exit then
   g PEND {: w:^ :}
   ACT-LAY w WORK-FORM !  nd w WORK-TREE ! ;

: PEND-SPLIT ( ^gen n n -- ) {: g:^ dx:n dy:n :}
   g PEND {: w:^ :}
   ACT-SPLIT w WORK-FORM !  dx w WORK-DX !  dy w WORK-DY ! ;

: PEND-JUMP ( ^gen n -- ) {: g:^ dx:n :}
   g PEND {: w:^ :}
   ACT-JUMP w WORK-FORM !  dx w WORK-DX ! ;

\ Moving the pending chain onto the stack reverses it, so the work that runs
\ first is on top.
: SPILL ( ^gen -- ) {: g:^ :}
   begin g GEN-PENDING @ NULL-PTR <> while
      g GEN-PENDING g GEN-TOP MOVE-WORK
   repeat ;

\ A class's pairs, copied out of the scratch arena; NULL-PTR for no pairs.
: COPY-RANGES ( ^gen ^node -- ^n ) {: g:^ nd:^ :}
   nd NODE-RANGES @ {: r:^ :}
   r NULL-PTR = if r exit then
   nd NODE-X @ 0= if NULL-PTR exit then
   nd NODE-X @ 2 * {: k:n :}
   g GEN-ARENA @ k cells ARENA:ALLOC drop CELL-VIEW {: dst:^ :}
   k 0 ?do r i CELL@ dst i CELL! loop
   dst ;

: EMIT-ATOM ( ^gen ^node -- ) {: g:^ nd:^ :}
   g HERE-INST {: i:^ :}
   g nd NODE-OP @ nd NODE-X @ 0 PUT-OP
   nd NODE-NEGATED @ i INST-NEGATED !
   g nd COPY-RANGES i INST-RANGES ! ;

: EMIT-MEMBERS ( ^gen ^node -- )
   NODE-FIRST @ begin dup NULL-PTR <> while
      2dup PEND-NODE NODE-NEXT @
   repeat
   2drop ;

\ A member but the last: a split to it or past it, and a jump to the
\ choice's end, rest instructions past the jump.
: EMIT-ALTERNATIVE ( ^gen ^node n -- ) {: g:^ m:^ rest:n :}
   g 1  m NODE-SIZE @ 2 + PEND-SPLIT
   g m PEND-NODE
   g rest 1+ PEND-JUMP ;

: EMIT-CHOICE ( ^gen ^node -- ) {: g:^ nd:^ :}
   nd NODE-SIZE @ nd NODE-FIRST @
   begin dup NODE-NEXT @ NULL-PTR <> while
      {: left:n m:^ :}
      left m NODE-SIZE @ 2 + - {: rest:n :}
      g m rest EMIT-ALTERNATIVE
      rest m NODE-NEXT @
   repeat
   nip g swap PEND-NODE ;

: EMIT-STAR ( ^gen ^node -- ) {: g:^ e:^ :}
   e NODE-SIZE @ {: s:n :}
   g 1 s 2 + PEND-SPLIT
   g e PEND-NODE
   g s 1+ negate PEND-JUMP ;

\ k optional copies, each a split to it or past the last.
: EMIT-OPTIONALS ( ^gen ^node n -- ) {: g:^ e:^ k:n :}
   e NODE-SIZE @ 1+ {: step:n :}
   k 0 ?do
      g 1  k i - step * PEND-SPLIT
      g e PEND-NODE
   loop ;

\ A count over an operand with instructions is bounded by the program's size,
\ allocated before layout; over an empty one it may near a cell's maximum.
: EMIT-COUNTED ( ^gen ^node -- ) {: g:^ nd:^ :}
   nd NODE-FIRST @ {: e:^ :}
   e NODE-SIZE @ 0<> if nd NODE-X @ 0 ?do g e PEND-NODE loop then
   nd NODE-Y @ UNBOUNDED = if g e EMIT-STAR exit then
   g e nd NODE-Y @ nd NODE-X @ - EMIT-OPTIONALS ;

\ Puts an atom; any other node pends its work.
: EMIT-NODE ( ^gen ^node -- ) {: g:^ nd:^ :}
   g nd  nd NODE-FORM @ MATCH kind
      atom OF EMIT-ATOM ENDOF
      sequence OF EMIT-MEMBERS ENDOF
      choice OF EMIT-CHOICE ENDOF
      counted OF EMIT-COUNTED ENDOF
   ;MATCH ;

\ Reads the record before EMIT-NODE pends: it is spare once taken.
: RUN-WORK ( ^gen ^work -- ) {: g:^ w:^ :}
   g GEN-PC @ {: pc:n :}
   w WORK-TREE @ w WORK-DX @ w WORK-DY @ {: nd:^ dx:n dy:n :}
   w WORK-FORM @ MATCH act
      lay OF g nd EMIT-NODE  g SPILL ENDOF
      split OF g OPCODE-SPLIT pc dx + pc dy + PUT-OP ENDOF
      jump OF g OPCODE-JUMP pc dx + 0 PUT-OP ENDOF
   ;MATCH ;

: TAKE ( ^gen -- ^work ) {: g:^ :}
   g GEN-TOP @ {: w:^ :}
   g GEN-TOP g GEN-SPARE MOVE-WORK  w ;

: LAYOUT ( ^gen ^node -- ) {: g:^ tree:^ :}
   g tree PEND-NODE  g SPILL
   begin g GEN-TOP @ NULL-PTR <> while g g TAKE RUN-WORK repeat ;

: BUILD ( str ^ARENA:arena ^ARENA:arena -- regex )
   {: src:str a:^ s:^ :}
   s PARSERS-NEW {: p:^ :}
   src STR:DATA p PARSER-SRC !  src STR:LEN p PARSER-LEN !  s p PARSER-SCRATCH !
   p READ-PATTERN {: tree:^ :}
   tree NODE-SIZE @ 1+ {: size:n :}
   a size INST-BYTES * ARENA:ALLOC drop >INSTS {: code:^ :}
   s GENS-NEW {: g:^ :}
   a g GEN-ARENA !  code g GEN-CODE !  s g GEN-SCRATCH !
   g tree LAYOUT
   g OPCODE-ACCEPT 0 0 PUT-OP
   code size REGEX-MAKE ;


\ ---- matching --------------------------------------------------------------

\ One MATCH? run, at the start of its scratch. now and was list the threads
\ at offset and the one before; marks holds a generation per instruction.
STRUCTURE vm DERIVE addr
   FIELD code ^inst
   FIELD text ^u8
   FIELD len n
   FIELD offset n
   FIELD now ^n
   FIELD now-count n
   FIELD was ^n
   FIELD was-count n
   FIELD marks ^n
   FIELD stack ^n
   FIELD depth n
;STRUCTURE

CAST: >VM ( ^u8 -- ^vm )

\ Pushes pc for the closure unless generation gen has reached it.
: REACH ( ^vm n n -- ) {: v:^ pc:n gen:n :}
   v VM-MARKS @ pc CELL@ gen = if exit then
   gen v VM-MARKS @ pc CELL!
   pc v VM-STACK @ v VM-DEPTH @ CELL!
   1 v VM-DEPTH +! ;

: POP ( ^vm -- n ) {: v:^ :}
   -1 v VM-DEPTH +!
   v VM-STACK @ v VM-DEPTH @ CELL@ ;

: KEEP-THREAD ( ^vm n -- ) {: v:^ pc:n :}
   pc v VM-NOW @ v VM-NOW-COUNT @ CELL!
   1 v VM-NOW-COUNT +! ;

\ One instruction of the closure at offset at; true at accept.
: FOLLOW ( ^vm n n -- bool ) {: v:^ pc:n at:n :}
   v VM-CODE @ pc INST-AT {: ins:^ :}
   at 1+ {: gen:n :}
   ins INST-OP @ MATCH opcode
      single OF v pc KEEP-THREAD false ENDOF
      class OF v pc KEEP-THREAD false ENDOF
      any OF v pc KEEP-THREAD false ENDOF
      start OF at 0= if v pc 1+ gen REACH then false ENDOF
      end OF at v VM-LEN @ = if v pc 1+ gen REACH then false ENDOF
      split OF
         v ins INST-X @ gen REACH  v ins INST-Y @ gen REACH  false
      ENDOF
      jump OF v ins INST-X @ gen REACH false ENDOF
      accept OF true ENDOF
   ;MATCH ;

\ Adds pc's closure at offset at to the list for at; true when it reaches
\ accept.
: CLOSURE ( ^vm n n -- bool ) {: v:^ pc:n at:n :}
   v pc at 1+ REACH
   begin v VM-DEPTH @ 0 > while
      v v POP at FOLLOW if true exit then
   repeat
   false ;

: TERMINATOR? ( n -- bool ) {: c:n :}
   c $A = c $D = or c $2028 = or c $2029 = or ;

: IN-RANGES? ( ^inst n -- bool ) {: ins:^ c:n :}
   ins INST-RANGES @ {: r:^ :}
   ins INST-X @ 0 ?do
      r i PAIR@ c >= swap c <= and if unloop true exit then
   loop
   false ;

: TAKES? ( ^inst n -- bool ) {: ins:^ c:n :}
   ins INST-OP @ MATCH opcode
      single OF ins INST-X @ c = ENDOF
      class OF
         ins c IN-RANGES? ins INST-NEGATED @ if 0= then
      ENDOF
      any OF c TERMINATOR? 0= ENDOF
      start OF false ENDOF
      end OF false ENDOF
      split OF false ENDOF
      jump OF false ENDOF
      accept OF false ENDOF
   ;MATCH ;

: SWAP-LISTS ( ^vm -- ) {: v:^ :}
   v VM-NOW @ v VM-WAS @ v VM-NOW ! v VM-WAS !
   v VM-NOW-COUNT @ v VM-WAS-COUNT !
   0 v VM-NOW-COUNT ! ;

: MOVE-THREAD ( ^vm n n n -- bool ) {: v:^ pc:n c:n next:n :}
   v VM-CODE @ pc INST-AT c TAKES? 0= if false exit then
   v pc 1+ next CLOSURE ;

\ Moves the threads that take c to the list for offset next; true when one
\ reaches accept.
: STEP-THREADS ( ^vm n n -- bool ) {: v:^ c:n next:n :}
   v SWAP-LISTS
   v VM-WAS-COUNT @ 0 ?do
      v v VM-WAS @ i CELL@ c next MOVE-THREAD
      if unloop true exit then
   loop
   false ;

\ The code point at offset at and the offset after it.
: POINT ( ^vm n -- n n ) {: v:^ at:n :}
   v VM-TEXT @ v VM-LEN @ >str at UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF nip REPLACEMENT swap ENDOF
   ;MATCH ;

\ One offset: a new thread starts, then the threads step over its code
\ point. Answers whether the run is done, then the answer.
: VISIT ( ^vm -- bool bool ) {: v:^ :}
   v VM-OFFSET @ {: at:n :}
   v 0 at CLOSURE if true true exit then
   at v VM-LEN @ >= if true false exit then
   v at POINT {: c:n next:n :}
   next v VM-OFFSET !
   v c next STEP-THREADS dup ;

: RUN ( ^vm -- bool ) {: v:^ :}
   begin v VISIT swap 0= while drop repeat ;

\ The scratch: the vm record, then four rows of a cell per instruction.
: SCAN ( str regex ^u8 NUM:alloc-byte-len -- bool )
   drop {: t:str re base:^ :}
   re REGEX-UNMAKE {: code:^ size:n :}
   base >VM {: v:^ :}
   base VM-BYTES + CELL-VIEW {: row:^ :}
   code v VM-CODE !
   t STR:DATA v VM-TEXT !  t STR:LEN v VM-LEN !
   row v VM-NOW !
   row size cells + v VM-WAS !
   row size 2 * cells + v VM-MARKS !
   row size 3 * cells + v VM-STACK !
   v RUN ;

public

\ The pattern's program, in arena a; a pattern outside the dialect throws
\ E-PATTERN with its text.
: COMPILE ( str ^ARENA:arena -- regex )
   {: src:str a:^ :}
   src a [: BUILD ;] ARENA:WITH ;

\ Whether the program matches anywhere in the text.
: MATCH? ( str regex -- bool ) {: s:str re :}
   re REGEX-UNMAKE nip {: size:n :}
   s re  VM-BYTES size 4 * cells + MEM:BYTES-ALLOC-LEN
   [: SCAN ;] MEM:WITH-BYTES ;

;package
