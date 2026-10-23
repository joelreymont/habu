\ source.f - checked source materialization helpers.
\
\ Callers supply output buffers and capacities: appending past one throws
\ E-FS-CAPACITY, and file or stdin I/O failures throw named filesystem errors.
\ READ-STDIN-ALL probes one byte past a full buffer, so overflow fails closed
\ instead of truncating. READ-WHOLE-FILE, READ-WHOLE-SAMPLED and READ-WHOLE-FD
\ read to the end into the caller's growing storage, so a source of any size,
\ or one that grows while read, is read whole. SOURCE-APPEND-QPATH refuses a
\ path it cannot quote with E-FS-PATH-UNSAFE. The token-class words are the
\ ones LINT-LEX (src/lib/source-lex.f) and the loader share.
\
\ STORAGE CLASS. PROCESS-WIDE: the stdin probe byte and READ-STDIN-ALL's
\ SOURCE-LEN/SOURCE-RD are module variables; single-task.
\
require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/source-syntax.f

package SOURCE
private

1 constant SOURCE-PROBE-CAP
0 constant SOURCE-NUL
10 constant SOURCE-LF
13 constant SOURCE-CR
32 constant SOURCE-SPACE
34 constant SOURCE-DQ
92 constant SOURCE-BACKSLASH

create SOURCE-PROBE SOURCE-PROBE-CAP allot

variable SOURCE-LEN
variable SOURCE-RD

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

\ A parsing keyword takes the next whitespace-delimited token raw, as data,
\ whatever it spells: `'` and `char` at top level, `[']` and `[char]` in a body
\ (src/habu/habu2.f EM-INTERPRET-DEFINE-KEYWORDS, EM-COMPILE-META-KEYWORDS),
\ matched case-folded as the engine's keyword compare folds. The engine refuses
\ each outside its own state, so a scanner that does not track the state takes
\ all four. In a body a local of the keyword's name is that local and takes
\ nothing, so a scanner that knows the locals asks this after its local lookup.
: PARSING-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SOURCE-SYNTAX:PARSING-KEYWORD? ;

\ The interpret keywords of src/habu/habu2.f EM-INTERPRET-DEFINE-KEYWORDS that
\ read a name take the next whitespace-delimited token raw, whatever it spells,
\ and open no definition, but for `trusted:`, which opens one as `:` does, and
\ the parsing keywords above. An engine definer makes a word of that name; a
\ package keyword makes none: `package` and `using` take a package's name, and
\ `export` an existing word's, which it re-exports as that word. Matched
\ case-folded as the engine's keyword compare folds. They are keywords only
\ while the engine interprets: in a body `create` is a call that reads its name
\ when the word runs and the others are undefined, so a scanner asks these
\ outside a definition.
: DEFINER-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" cast:" STR=CI if STR-TRUE exit then
   a u s" linear:" STR=CI if STR-TRUE exit then
   a u s" defer" STR=CI if STR-TRUE exit then
   a u s" create" STR=CI if STR-TRUE exit then
   a u s" variable" STR=CI if STR-TRUE exit then
   a u s" constant" STR=CI ;

: PACKAGE-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" STR=CI if STR-TRUE exit then
   a u s" using" STR=CI if STR-TRUE exit then
   a u s" export" STR=CI ;

\ The control words that open and close a block in a body, matched case-folded.
\ The engine and the checker keep the live locals count on their control-flow
\ stack (src/habu/habu2.f LCFPUSH and LCFPOP, src/core/checker.f CF-PUSH), so a
\ local a block declares ends with the block; `else` ends the true arm's. A
\ quotation is a block too. A scanner that knows the locals asks these after its
\ local lookup, since a local of a control word's name is that local.
: BLOCK-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SOURCE-SYNTAX:BLOCK-OPENER? ;

: BLOCK-CLOSER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SOURCE-SYNTAX:BLOCK-CLOSER? ;

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
