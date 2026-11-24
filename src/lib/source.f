split-str
\ source.f - checked source materialization helpers. Callers supply output
\ buffers: appending past one throws E-FS-CAPACITY, and file or stdin failures
\ throw named filesystem errors. READ-STDIN-ALL appends all of standard input to
\ the vector; growth leaves earlier blocks unwiped, and a full vector grows even
\ for the read that finds the end, so a caller reading a secret first reserves
\ more room than the secret takes.
\ A whole read fills storage that room gives and may move, so each read goes to
\ room's latest answer. The first room is the size sample and one byte more, at
\ least SOURCE-ROOM-MIN, so an unchanged file fills it short and the next read
\ finds its end; a sampled read that fills it holds more than the sample, which
\ is E-FS-CHANGED. A read with no sample doubles a full room.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

\ LINT-LEX (src/lib/source-lex.f) and the loader share the token-class words.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/vector.f
require src/lib/source-syntax.f
require src/core/compile-reads.f

package SOURCE
private

0 constant SOURCE-NUL
10 constant SOURCE-LF
13 constant SOURCE-CR
32 constant SOURCE-SPACE
34 constant SOURCE-DQ
92 constant SOURCE-BACKSLASH
$7FFFFFFFFFFFFFFF constant SOURCE-NO-CAP    \ more than any read holds
4096 constant SOURCE-ROOM-MIN

\ Answers the bytes now held, the room for the next read (doubled once the held
\ bytes fill it) and whether the read found the end; past CAP it is E-FS-CHANGED.
: SOURCE-READ-STEP ( n [ n -- ^u8 ] n n n -- n n bool )
   {: fd:n room held:n want:n cap:n :}
   want room execute {: dst:^ :}
   fd dst held + want held - read {: got:n :}
   got 0 < if E-FS-IO throw then
   got want held - > if E-FS-IO throw then
   held got + {: now:n :}
   now cap > if E-FS-CHANGED throw then
   now  now want = if want 2 * else want then  got 0= ;

\ The descriptor to its end; FIRST is the size sample, or 0 for none.
: SOURCE-READ-LOOP ( n n n [ n -- ^u8 ] -- n )
   {: fd:n first:n cap:n room :}
   0  first 1 + SOURCE-ROOM-MIN max
   begin fd room 2swap cap SOURCE-READ-STEP until
   drop ;

public

\ The descriptor to its end.
: READ-WHOLE-FD ( n [ n -- ^u8 ] -- n )
   {: fd:n room :}
   fd 0 SOURCE-NO-CAP room SOURCE-READ-LOOP ;

private

: STDIN-ROOM ( ^VEC:vector<u8> -- )
   {: v:^ :}
   v VEC:COUNT v VEC:CAP < if exit then
   v  v VEC:CAP 2 * SOURCE-ROOM-MIN max  VEC:RESERVE ;

\ One read into the spare room, appended; answers whether it found the end.
: STDIN-STEP ( ^VEC:vector<u8> -- bool )
   {: v:^ :}
   v STDIN-ROOM
   v VEC:COUNT {: old:n :}
   v VEC:SPAN SPAN:$ drop old + {: at:^ :}
   0 at v VEC:CAP old - read {: got:n :}
   got 0 < if E-FS-IO throw then
   v old got + VEC:ENSURE
   got 0= ;

public

: READ-STDIN-ALL ( ^VEC:vector<u8> -- )
   {: v:^ :}
   begin v STDIN-STEP until ;

private

\ The file at a path opened for a whole read, recorded as one the compile read.
: SOURCE-OPEN ( str -- n )
   {: path:^ pathu:n :}
   path pathu FS:>PATHZ {: z:^ :}
   path pathu z COMPILE-READS:FILE+
   z open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

\ The file at a path, read as SOURCE-READ-LOOP reads.
: SOURCE-READ-PATH ( str n n [ n -- ^u8 ] -- n )
   {: path:^ pathu:n first:n cap:n room :}
   path pathu SOURCE-OPEN {: fd:n :}
   [: fd first cap room SOURCE-READ-LOOP ;] catch {: rc:n :}
   fd close
   rc 0<> if rc throw then ;

public

\ The file at a path, whole, from a size the caller sampled (FS:FILE-SIZE makes a
\ missing file E-FS-STAT); a file holding more bytes than the sample is E-FS-CHANGED.
: READ-WHOLE-SAMPLED ( str n [ n -- ^u8 ] -- n )
   {: path:^ pathu:n first:n room :}
   path pathu first first room SOURCE-READ-PATH ;

\ The file at a path, whole: a regular file's size bounds the read, and a path
\ that names no regular file gives no sample and is read to its end.
: READ-WHOLE-FILE ( str [ n -- ^u8 ] -- n )
   {: path:^ pathu:n room :}
   path pathu FS:FILE? if
      path pathu  path pathu FS:FILE-SIZE  room READ-WHOLE-SAMPLED exit
   then
   path pathu 0 SOURCE-NO-CAP room SOURCE-READ-PATH ;

: SOURCE-APPEND-BYTES ( str ^u8 len ^len -- )
   {: src:^ u dst:^ cap lenp:^ :}
   u 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N cap LEN>N > if E-FS-CAPACITY throw then
   u cap LEN>N lenp @ LEN>N - > if E-FS-CAPACITY throw then
   src dst lenp @ LEN>N + u BYTE-COPY
   lenp @ LEN>N u + >LEN lenp ! ;

: SOURCE-APPEND-C ( n ^u8 len ^len -- )
   {: c:n dst:^ cap:len lenp:^ :}
   lenp @ LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N cap LEN>N >= if E-FS-CAPACITY throw then
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1 + >LEN lenp ! ;

private

\ Path bytes that would change source structure (`"`, `\`, LF, CR) are rejected so
\ materialized loader lines and diagnostic labels cannot be broken or injected by a
\ path. A NUL is rejected too: no path holding one names a file, and a label holding
\ one would carry it into a diagnostic.
: SOURCE-PATH-BYTE-SAFE? ( n -- bool ) {: c:n :}
   c SOURCE-NUL = if STR:FALSE exit then
   c SOURCE-DQ = if STR:FALSE exit then
   c SOURCE-BACKSLASH = if STR:FALSE exit then
   c SOURCE-LF = if STR:FALSE exit then
   c SOURCE-CR = if STR:FALSE exit then
   STR:TRUE ;

: SOURCE-PATH-SAFE? ( str -- bool ) {: a:^ u:n :}
   0 begin dup u < while
      a over + c@ SOURCE-PATH-BYTE-SAFE? 0= if drop STR:FALSE exit then
      1+
   repeat drop STR:TRUE ;

public

\ E-FS-PATH-UNSAFE for a path SOURCE-APPEND-QPATH cannot quote. A caller that
\ quotes a spelling only late judges it early with this.
: SOURCE-QPATH-CHECK ( str -- )
   SOURCE-PATH-SAFE? 0= if E-FS-PATH-UNSAFE throw then ;

: SOURCE-APPEND-QPATH ( str ^u8 len ^len -- )
   {: path:^ pathu:n dst:^ cap:len lenp:^ :}
   path pathu SOURCE-QPATH-CHECK
   s" s" dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-DQ dst cap lenp SOURCE-APPEND-C
   SOURCE-SPACE dst cap lenp SOURCE-APPEND-C
   path pathu dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-DQ dst cap lenp SOURCE-APPEND-C ;

\ The bytes SOURCE-APPEND-QPATH writes for a path of n bytes: `s" PATH"`.
: SOURCE-QPATH-BYTES ( n -- n )
   4 + ;

\ At top level `char` and `'` parse, and `[char]` and `[']`, E-UNDEFINED there,
\ take their token too; a body asks SOURCE-SYNTAX:BODY-PARSING-KEYWORD?.
: PARSING-KEYWORD? ( str -- bool ) {: a:^ u:n :}
   a u SOURCE-SYNTAX:PARSING-KEYWORD? ;

: PACKAGE-KEYWORD? ( str -- bool ) {: a:^ u:n :}
   a u s" package" STR:=CI if STR:TRUE exit then
   a u s" using" STR:=CI if STR:TRUE exit then
   a u s" export" STR:=CI ;

\ A local a block declares ends with the block, and `else` ends the true arm's.
\ A local of a control word's name is that local: ask after the local lookup.
: BLOCK-OPENER? ( str -- bool ) {: a:^ u:n :}
   a u SOURCE-SYNTAX:BLOCK-OPENER? ;

: BLOCK-CLOSER? ( str -- bool ) {: a:^ u:n :}
   a u SOURCE-SYNTAX:BLOCK-CLOSER? ;

private

: STRING-LEAD? ( n -- bool )
   STR:ASCII-LOWER {: c:n :}
   c 115 = c 99 = or c $2E = or ;

public

\ The words that open a string literal, matched case-folded as the dictionary
\ is: `s"`, `c"` and `."` take the bytes up to the next `"`, and `s\"`, `c\"`
\ and `.\"` the same with a backslash escaping the byte after it.
: NORMAL-STRING-OPENER? ( str -- bool )
   {: a:^ u:n :}
   u 2 <> if STR:FALSE exit then
   a 1 BYTE@ $22 <> if STR:FALSE exit then
   a 0 BYTE@ STRING-LEAD? ;

: ESC-STRING-OPENER? ( str -- bool )
   {: a:^ u:n :}
   u 3 <> if STR:FALSE exit then
   a 1 BYTE@ $5C <> if STR:FALSE exit then
   a 2 BYTE@ $22 <> if STR:FALSE exit then
   a 0 BYTE@ STRING-LEAD? ;

;package
