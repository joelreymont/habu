\ regex-test.f - patterns through REGEX:COMPILE and REGEX:MATCH?: the ones
\ consumers state, written as their JSON strings decode, the dialect's
\ parts, the time bound, a nesting too deep to recurse through and each
\ rejected construct's text.
\ Run: bin/hb --load src/lib/regex-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/arena.f
require src/lib/reason.f
require src/lib/regex.f

package REGEX-TEST

: RUN-IN ( str str ^ARENA:arena -- bool )
   {: p:str t:str a:^ :}
   p a REGEX:COMPILE {: re :}
   t re REGEX:MATCH? ;

\ Whether the pattern matches the text.
: MATCHES? ( str str -- bool ) [: RUN-IN ;] ARENA:WITH ;

: YES ( str str -- ) MATCHES? TTRUE ;
: NO ( str str -- ) MATCHES? TFALSE ;

: COMPILE-ONLY ( str -- )
   [: REGEX:COMPILE drop ;] ARENA:WITH ;

\ The pattern is rejected with exactly the text given.
: REJECTS ( str str -- ) {: p:str w:str :}
   [: p COMPILE-ONLY ;] REGEX:E-PATTERN TTHROWSQ
   REGEX:E-PATTERN REASON:TEXT$ w T$=
   REASON:RELEASE ;

: MAKI ( -- )
   s" Maki's tool patterns" T-LABEL
   s" ^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
   dup s" 123e4567-e89b-12d3-a456-426614174000" YES
   s" 123e4567-e89b-12d3-a456-42661417400" NO
   s" ^[0-9a-f]{32}$"
   dup s" 0123456789abcdef0123456789abcdef" YES
   s" 0123456789abcdef0123456789abcdeF" NO
   s" ^[^\u0000]*$" dup s" project" YES S\" pro\x00ject" NO
   s" ^[a-z0-9][a-z0-9-]*$" dup s" my-project1" YES s" -lead" NO
   s" ^[^/]+$" dup s" file.txt" YES s" dir/file.txt" NO
   s" ^[0-9a-f]{64}$"
   dup s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
   YES
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde"
   NO
   s" \S" dup s"  x " YES S\"  \t\n " NO ;

\ Maki's name rule: not empty or only white space, the class spelling it
\ in `\t-\r` and `\u` escapes.
: MAKI-NAME$ ( -- str )
   s" ^[^\u0000]*[^\u0000\t-\r \u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000][^\u0000]*$" ;

: MAKI-NAME ( -- )
   s" Maki's file name, a class of white space escapes" T-LABEL
   MAKI-NAME$ s"  a " YES
   MAKI-NAME$ S\"  \xE3\x80\x80 " NO ;

: ZEN ( -- )
   s" Zen's date and digest patterns" T-LABEL
   s" ^[0-9]{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?$"
   dup s" 2026-11-17" YES s" 2026-13" NO
   s" ^[0-9a-f]{64}$"
   dup s" 89abcdef0123456789abcdef0123456789abcdef0123456789abcdef01234567"
   YES
   s" 89abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456" NO ;

: DIALECT ( -- )
   s" an unanchored search, `.`, a pair escape, classes, counts, groups"
   T-LABEL
   s" b+c" dup s" aabbbcd" YES s" aabd" NO
   s" ^b" s" ab" NO
   s" a.c" dup s" a-c" YES S\" a\nc" NO
   s" a.c" dup S\" a\rc" NO S\" a\xE2\x80\xA8c" NO
   s" ^\uD83D\uDE00$" dup S\" \xF0\x9F\x98\x80" YES s" a" NO
   s" ^[α-ω]+$" dup s" λογος" YES s" Λ" NO
   s" ^a{2,3}$" dup s" a" NO dup s" aa" YES
   dup s" aaa" YES s" aaaa" NO
   s" ^(cat|dog)s?$" dup s" dogs" YES s" cow" NO ;

100000 constant AS-LEN
AS-LEN BUFFER: AS

\ A backtracking matcher takes time exponential in the text here.
: LINEAR ( -- )
   s" `(a*)*b` over 100000 `a`s answers false" T-LABEL
   AS-LEN 0 ?do [char] a AS i + c! loop
   s" (a*)*b" AS AS-LEN >str NO ;

100000 constant DEPTH
DEPTH 2 * 1+ constant NESTED-LEN
NESTED-LEN BUFFER: NESTED

\ DEPTH `(`, then `a`, then DEPTH `)`.
: NEST ( -- )
   DEPTH 0 ?do [char] ( NESTED i + c! loop
   [char] a NESTED DEPTH + c!
   DEPTH 0 ?do [char] ) NESTED DEPTH 1+ i + + c! loop ;

: DEEP ( -- )
   s" `a` in 100000 nested groups compiles and matches `a`" T-LABEL
   NEST NESTED NESTED-LEN >str s" a" YES ;

: EMPTY-COUNT ( -- )
   s" an empty group counted at least 10^12 times compiles and matches `ab`" T-LABEL
   s" a(){1000000000000,}b" s" ab" YES ;

: REJECTED ( -- )
   s" each rejected construct's text" T-LABEL
   s" (a)\1"
   s" 4: expected an escape other than a backreference, found `\1`; repeat the group's pattern in place of the reference"
   REJECTS
   S\" a\\\xE2\x80\xAEb"
   s" 2: expected an escape the dialect takes, found `\U+202E`; write the character itself or as `\uXXXX`"
   REJECTS
   s" a(?=b)"
   s" 2: expected a group other than a lookahead, found `(?=`; rewrite the pattern without it"
   REJECTS
   s" \bword"
   s" 1: expected an escape other than a word boundary, found `\b`; rewrite the pattern without it"
   REJECTS
   s" \p{L}"
   s" 1: expected an escape other than a Unicode property, found `\p`; write the property's characters as a class of ranges"
   REJECTS
   s" (ab"
   s" 4: expected `)` to close a group, found the end of the pattern; close each `(` with `)`"
   REJECTS
   s" *a"
   s" 1: expected a character, class or group before the quantifier, found `*`; put `\` before it to match the character"
   REJECTS
   s" a{3,2}"
   s" 5: expected a most count no less than the least, found `2`; write the smaller count first"
   REJECTS
   s" [z-a]"
   s" 4: expected a range end no less than its start, found `a`; write the range's lower end first"
   REJECTS ;

: RUN ( -- )
   T-RESET
   MAKI MAKI-NAME ZEN DIALECT LINEAR DEEP EMPTY-COUNT REJECTED
   T-REPORT ;

RUN

;package
