\ source.f - checked source materialization helpers.
\
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f

package SOURCE
private

1 constant SOURCE-PROBE-CAP
0 constant SOURCE-NUL
9 constant SOURCE-TAB
10 constant SOURCE-LF
13 constant SOURCE-CR
32 constant SOURCE-SPACE
34 constant SOURCE-DQ
92 constant SOURCE-BACKSLASH

create SOURCE-PROBE SOURCE-PROBE-CAP allot

variable SOURCE-BUF-A
variable SOURCE-LEN
variable SOURCE-RD
variable SOURCE-I
variable SOURCE-J
variable SOURCE-SKIP
variable SOURCE-END

\ Bytes behind SOURCE-BUF-A. There is no capacity CONSTANT here any more: a
\ build's source is not bounded by any number this file could name, so the
\ commented-source buffer is sized from the source on every call.
variable BUF-CAP

: SOURCE-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

public

: SOURCE-PTR-U8@ ( ptr a -- ptr u8 )
   SOURCE-PTR-U8-FIELD @ ;

: SOURCE-PTR-U8! ( ptr u8 ptr a -- )
   SOURCE-PTR-U8-FIELD ! ;

\ One-shot allocation of a caller-owned span. MEM:BYTES-ALLOC-LEN narrows the
\ raw size to the validated alloc role before MEM:ALLOC-BYTES, throwing
\ E-MEM-SIZE on any refusal (zero, negative, or overflow).
: SOURCE-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

private

: SOURCE-BUF ( -- ptr u8 )
   SOURCE-BUF-A SOURCE-PTR-U8@ ;

\ Capacity exits name the need and the have, so a refusal reads as a size and
\ not as a mystery (LESSONS 2026-08-22: a die that prints only the offending
\ token costs a day per break).
: GROW-REFUSED ( n n -- ) {: need:n have:n :}
   s" source: cannot grow the commented-source buffer" type cr
   s" source: needed " type need .
   s" source: have " type have .
   E-FS-CAPACITY throw ;

\ Install the new mapping, then release the prior one. Release is LAST, so a
\ refused allocation never reaches it and leaves the old span owned and intact
\ (the shape lib/vector.f VEC-INSTALL-RESIZE and lib/byte-buffer.f
\ INSTALL-RESIZE already carry). No copy: this buffer is scratch that every use
\ refills from its source before reading it.
\
\ INVARIANT, and the reason releasing is safe: no caller holds a pointer into
\ this buffer across a grow. SOURCE-BUF is fetched at the point of use and the
\ only word that reaches it, COMMENT-EXPORTS$, fetches AFTER its BUF-ENSURE.
: BUF-GROW ( n -- ) {: need:n :}
   need MEM-ALLOC-64K-SPAN {: buf:ptr got:n :}
   got need < if need got GROW-REFUSED then
   SOURCE-BUF {: old:ptr :}
   BUF-CAP @ {: oldcap:n :}
   buf SOURCE-BUF-A SOURCE-PTR-U8!
   got BUF-CAP !
   oldcap 0 > if old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES then ;

\ Grows only, and only when the request does not already fit: the buffer
\ outlives one call so a build that comments two sources reuses the larger span.
\ An empty source still gets a real span, so the answer is always a pointer a
\ caller may write through rather than a null one caller in three checks for.
: BUF-ENSURE ( n -- ) {: need:n :}
   SOURCE-BUF-A @ 0= if need 1 max BUF-GROW exit then
   need BUF-CAP @ <= if exit then
   need BUF-GROW ;

: SOURCE-READ-PROBE ( -- )
   0 SOURCE-PROBE SOURCE-PROBE-CAP read SOURCE-RD !
   SOURCE-RD @ 0 < if E-FS-IO throw then
   SOURCE-RD @ 0 > if E-FS-CAPACITY throw then ;

public

: READ-STDIN-ALL ( ptr u8 len -- len ) {: dst:ptr cap :}
   cap LEN>N 0 < if E-FS-CAPACITY throw then
   0 >LEN SOURCE-LEN !
   begin
      cap LEN>N SOURCE-LEN @ LEN>N - dup 0 <= if
         drop SOURCE-READ-PROBE SOURCE-LEN @ exit
      then
      0 dst SOURCE-LEN @ LEN>N + rot read SOURCE-RD !
      SOURCE-RD @ 0 < if E-FS-IO throw then
      SOURCE-RD @ 0= if SOURCE-LEN @ exit then
      SOURCE-LEN @ LEN>N SOURCE-RD @ + >LEN SOURCE-LEN !
   again ;

private

\ A WHOLE SOURCE IS READ TO ITS END, into storage that grows as it fills: room
\ gives room for n bytes and answers where they start, and the storage may move
\ when it grows, so each read goes to its latest answer. The first room is the
\ caller's size sample and one byte more, at least SOURCE-ROOM-MIN, so a source
\ that has not changed fills it short and the next read finds its end; a full
\ room doubles. A sample only starts the read: bytes past it are read too.
4096 constant SOURCE-ROOM-MIN

\ One read after the held bytes; answers the bytes now held, the room for the
\ next read, doubled once the held bytes fill it, and whether the read found
\ the end.
: SOURCE-READ-STEP ( n [ n -- ptr u8 ] n n -- n n bool )
   {: fd:n room held:n want:n :}
   want room execute {: dst:ptr :}
   fd dst held + want held - read {: got:n :}
   got 0 < if E-FS-IO throw then
   got want held - > if E-FS-IO throw then
   held got + {: now:n :}
   now  now want = if want 2 * else want then  got 0= ;

public

\ The descriptor to its end; the sample is 0 when there is none.
: READ-WHOLE-FD ( n n [ n -- ptr u8 ] -- n )
   {: fd:n first:n room :}
   0  first 1 + SOURCE-ROOM-MIN max
   begin fd room 2swap SOURCE-READ-STEP until
   drop ;

private

\ Stack-preserving under `catch`: the descriptor, sample and room ride through
\ and the count comes back where a zero went in, so READ-WHOLE-FILE closes the
\ descriptor on every way out.
: SOURCE-READ-RUN ( n n [ n -- ptr u8 ] n -- n n [ n -- ptr u8 ] n )
   drop
   {: fd:n first:n room :}
   fd first room  fd first room READ-WHOLE-FD ;

public

\ The file at a path, whole, from a size the caller sampled, which is only the
\ first room. The open or a read refuses what cannot be read (E-FS-OPEN,
\ E-FS-IO); a caller whose refusal of a missing file is the stat's samples with
\ FILE-SIZE.
: READ-WHOLE-SAMPLED ( ptr u8 n n [ n -- ptr u8 ] -- n )
   {: path:ptr pathu:n first:n room :}
   path pathu FS-PATHZ open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd first room 0 [: SOURCE-READ-RUN ;] catch
   {: rc:n :}
   fd close
   rc 0= if nip nip nip else rc throw then ;

\ The file at a path, whole: its size is only the first room, and a path that
\ names no regular file gives no sample.
: READ-WHOLE-FILE ( ptr u8 n [ n -- ptr u8 ] -- n )
   {: path:ptr pathu:n room :}
   path pathu FILE? if path pathu FILE-SIZE else 0 then {: first:n :}
   path pathu first room READ-WHOLE-SAMPLED ;

: SOURCE-APPEND-BYTES ( ptr u8 len ptr u8 len ptr len -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   u LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N cap LEN>N > if E-FS-CAPACITY throw then
   u LEN>N cap LEN>N lenp @ LEN>N - > if E-FS-CAPACITY throw then
   src dst lenp @ LEN>N + u LEN>N BYTE-COPY
   lenp @ LEN>N u LEN>N + >LEN lenp ! ;

: SOURCE-APPEND-C ( n ptr u8 len ptr len -- )
   {: c:n dst:ptr cap:len lenp:ptr :}
   lenp @ LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N cap LEN>N >= if E-FS-CAPACITY throw then
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1 + >LEN lenp ! ;

private

\ One shared checked path-string emitter. Path bytes that would change source
\ structure (`"`, `\`, LF, CR) are rejected fail-closed so materialized loader
\ lines and diagnostic prefix labels cannot be broken or injected by a path. A
\ NUL is rejected too: no path holding one names a file, and a label holding one
\ would carry it into a diagnostic.
: SOURCE-PATH-BYTE-SAFE? ( n -- bool ) {: c:n :}
   c SOURCE-NUL = if STR-FALSE exit then
   c SOURCE-DQ = if STR-FALSE exit then
   c SOURCE-BACKSLASH = if STR-FALSE exit then
   c SOURCE-LF = if STR-FALSE exit then
   c SOURCE-CR = if STR-FALSE exit then
   STR-TRUE ;

: SOURCE-PATH-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ SOURCE-PATH-BYTE-SAFE? 0= if drop STR-FALSE exit then
      1+
   repeat drop STR-TRUE ;

public

\ E-FS-PATH-UNSAFE for a path SOURCE-APPEND-QPATH cannot quote. A caller that
\ quotes a spelling only late judges it early with this.
: SOURCE-QPATH-CHECK ( ptr u8 n -- )
   SOURCE-PATH-SAFE? 0= if E-FS-PATH-UNSAFE throw then ;

: SOURCE-APPEND-QPATH ( ptr u8 len ptr u8 len ptr len -- )
   {: path:ptr pathu:len dst:ptr cap:len lenp:ptr :}
   path pathu LEN>N SOURCE-QPATH-CHECK
   s" s" >LEN dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-DQ dst cap lenp SOURCE-APPEND-C
   SOURCE-SPACE dst cap lenp SOURCE-APPEND-C
   path pathu dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-DQ dst cap lenp SOURCE-APPEND-C ;

\ The bytes SOURCE-APPEND-QPATH writes for a path of n bytes: `s" PATH"`.
: SOURCE-QPATH-BYTES ( n -- n )
   4 + ;

private

: SOURCE-LINE-END ( ptr u8 len off -- off ) {: src:ptr u start :}
   start SOURCE-J !
   begin SOURCE-J @ OFF>N u LEN>N < while
      src SOURCE-J @ OFF>N + c@ SOURCE-LF = if SOURCE-J @ OFF>N 1 + >OFF exit then
      SOURCE-J @ OFF>N 1 + >OFF SOURCE-J !
   repeat
   u LEN>N >OFF ;

: SOURCE-LINE-SKIP-WS ( ptr u8 len -- off ) {: src:ptr u :}
   0 >OFF SOURCE-J !
   begin SOURCE-J @ OFF>N u LEN>N < while
      src SOURCE-J @ OFF>N + c@ dup SOURCE-SPACE = swap SOURCE-TAB = or if
         SOURCE-J @ OFF>N 1 + >OFF SOURCE-J !
      else
         SOURCE-J @ exit
      then
   repeat
   SOURCE-J @ ;

: SOURCE-EXPORT-LINE? ( ptr u8 len -- bool ) {: line:ptr lineu :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - s" EXPORT " STARTS-WITH? ;

\ Package-context tracking for the directive strip (dot
\ habu-compiler-pkg-re-688212c1): `EXPORT NAME` INSIDE an open package block is
\ the re-export declaration and must reach the compiler; only TOP-LEVEL
\ `EXPORT ` lines are the hb-build --repl directive to comment out. The
\ tracker is line-based like the strip itself (canonical line-leading
\ `package NAME` openers and `;package` closers). Both miss
\ modes fail safe: an uncommented top-level directive compiles as the engine
\ keyword's no-op, and a wrongly-commented in-package re-export leaves the
\ alias undefined so the build rejects loudly.
variable SOURCE-PKG-DEPTH

: SOURCE-LINE-LEAD$ ( ptr u8 len -- ptr u8 n ) {: line:ptr lineu:len :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - ;

: SOURCE-PACKAGE-OPEN-LINE? ( ptr u8 len -- bool )
   SOURCE-LINE-LEAD$ s" package " STARTS-WITH? ;

: SOURCE-PACKAGE-CLOSE-LINE? ( ptr u8 len -- bool )
   SOURCE-LINE-LEAD$ {: a:ptr u:n :}
   a u s" ;package" STARTS-WITH? ;

: SOURCE-LINE-PKG-TRACK ( ptr u8 len -- ) {: line:ptr lineu:len :}
   line lineu SOURCE-PACKAGE-OPEN-LINE? if
      SOURCE-PKG-DEPTH @ 1 + SOURCE-PKG-DEPTH ! exit
   then
   line lineu SOURCE-PACKAGE-CLOSE-LINE? if
      SOURCE-PKG-DEPTH @ 0 > if SOURCE-PKG-DEPTH @ 1 - SOURCE-PKG-DEPTH ! then
   then ;

\ The only spelling of the comment marker. The emitter below writes it and
\ COMMENT-NEED sizes with it, so the destination bound cannot drift from the
\ bytes that are actually emitted.
: COMMENT-PREFIX$ ( -- ptr u8 n )
   s" \ " ;

: SOURCE-APPEND-COMMENTED-EXPORT ( ptr u8 len ptr u8 len ptr len -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   COMMENT-PREFIX$ >LEN dst cap lenp SOURCE-APPEND-BYTES
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - >LEN dst cap lenp SOURCE-APPEND-BYTES ;

: SOURCE-APPEND-COMMENT-LINE ( ptr u8 len ptr u8 len ptr len -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-LINE-PKG-TRACK
   line lineu SOURCE-EXPORT-LINE? SOURCE-PKG-DEPTH @ 0 = and if
      line lineu dst cap lenp SOURCE-APPEND-COMMENTED-EXPORT
   else
      line lineu dst cap lenp SOURCE-APPEND-BYTES
   then ;

: COMMENT-EXPORTS ( ptr u8 len ptr u8 len -- len ) {: src:ptr u dst:ptr cap :}
   0 >LEN SOURCE-LEN !
   0 >OFF SOURCE-I !
   0 SOURCE-PKG-DEPTH !
   begin SOURCE-I @ OFF>N u LEN>N < while
      src u SOURCE-I @ SOURCE-LINE-END SOURCE-END !
      src SOURCE-I @ OFF>N + SOURCE-END @ OFF>N SOURCE-I @ OFF>N - >LEN dst cap SOURCE-LEN SOURCE-APPEND-COMMENT-LINE
      SOURCE-END @ SOURCE-I !
   repeat
   SOURCE-LEN @ ;

\ Lines the strip will walk, counting a final unterminated one.
: LINE-COUNT ( ptr u8 len -- n ) {: src:ptr u:len :}
   0 >OFF SOURCE-I !
   0 begin SOURCE-I @ OFF>N u LEN>N < while
      src u SOURCE-I @ SOURCE-LINE-END SOURCE-I !
      1+
   repeat ;

\ The exact destination bound. A commented line loses its leading whitespace and
\ gains the comment prefix, so no line grows by more than the prefix, and every
\ other line is copied byte for byte.
: COMMENT-NEED ( ptr u8 len -- n ) {: src:ptr u:len :}
   u LEN>N  src u LINE-COUNT COMMENT-PREFIX$ nip *  + ;

public

\ COMMENT-EXPORTS into this module's own buffer, sized from the source. The
\ caller gets a span, not a buffer and a capacity: the size question belongs to
\ the code that owns the emission rule, and a caller that had to guess it is how
\ a 128 KiB constant silently capped hb-build (dot habu-hb-build-cannot-d09df17e).
: COMMENT-EXPORTS$ ( ptr u8 len -- ptr u8 len ) {: src:ptr u:len :}
   src u COMMENT-NEED BUF-ENSURE
   src u SOURCE-BUF BUF-CAP @ >LEN COMMENT-EXPORTS {: outu:len :}
   SOURCE-BUF outu ;

\ A parsing keyword takes the next whitespace-delimited token raw, as data,
\ whatever it spells: `'` and `char` at top level, `[']` and `[char]` in a body
\ (src/habu/habu2.f EM-INTERPRET-DEFINE-KEYWORDS, EM-COMPILE-META-KEYWORDS),
\ matched case-folded as the engine's keyword compare folds. The engine refuses
\ each outside its own state, so a scanner that does not track the state takes
\ all four. In a body a local of the keyword's name is that local and takes
\ nothing, so a scanner that knows the locals asks this after its local lookup.
: PARSING-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" char" STR=CI if STR-TRUE exit then
   a u s" [char]" STR=CI if STR-TRUE exit then
   a u s" '" STR= if STR-TRUE exit then
   a u s" [']" STR= ;

\ The control words that open and close a block in a body, matched case-folded.
\ The engine and the checker keep the live locals count on their control-flow
\ stack (src/habu/habu2.f LCFPUSH and LCFPOP, src/core/checker.f CF-PUSH), so a
\ local a block declares ends with the block; `else` ends the true arm's. A
\ quotation is a block too. A scanner that knows the locals asks these after its
\ local lookup, since a local of a control word's name is that local.
: BLOCK-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" STR=CI if STR-TRUE exit then
   a u s" begin" STR=CI if STR-TRUE exit then
   a u s" do" STR=CI if STR-TRUE exit then
   a u s" ?do" STR=CI if STR-TRUE exit then
   a u s" case" STR=CI if STR-TRUE exit then
   a u s" of" STR=CI if STR-TRUE exit then
   a u s" match" STR=CI if STR-TRUE exit then
   a u s" [:" STR= ;

: BLOCK-CLOSER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" then" STR=CI if STR-TRUE exit then
   a u s" until" STR=CI if STR-TRUE exit then
   a u s" repeat" STR=CI if STR-TRUE exit then
   a u s" again" STR=CI if STR-TRUE exit then
   a u s" loop" STR=CI if STR-TRUE exit then
   a u s" +loop" STR=CI if STR-TRUE exit then
   a u s" endof" STR=CI if STR-TRUE exit then
   a u s" endcase" STR=CI if STR-TRUE exit then
   a u s" ;match" STR=CI if STR-TRUE exit then
   a u s" ;]" STR= ;

private

\ The first byte of a string opener: `s` or `c` in either case, or `.`.
: STRING-LEAD? ( n -- bool )
   ASCII-LOWER {: c:n :}
   c 115 = c 99 = or c $2E = or ;

public

\ The words that open a string literal, matched case-folded as the dictionary
\ is: `s"`, `c"` and `."` take the bytes up to the next `"`, and `s\"`, `c\"`
\ and `.\"` the same with a backslash escaping the byte after it.
: NORMAL-STRING-OPENER? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 2 <> if STR-FALSE exit then
   a 1 BYTE@ $22 <> if STR-FALSE exit then
   a 0 BYTE@ STRING-LEAD? ;

: ESC-STRING-OPENER? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 3 <> if STR-FALSE exit then
   a 1 BYTE@ $5C <> if STR-FALSE exit then
   a 2 BYTE@ $22 <> if STR-FALSE exit then
   a 0 BYTE@ STRING-LEAD? ;

;package
