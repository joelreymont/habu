\ check-verify-core.f - check a subject's bytes as its file, in its load context, without running it.
\
\ CHECK:VERIFY-BYTES is the check a language server makes of an open buffer and
\ `tools/check.f --verify-only` makes of a file, or of stdin under a path. It
\ takes the subject's bytes and the path they stand for and answers
\ what `bin/hb --load PATH` would refuse of them, and runs none of it:
\
\ - The closure is discovered over the bytes with PATH's directory as root and
\   PATH as the subject's identity, so a dependency that requires PATH back
\   meets the bytes, never the copy on disk.
\ - The verification runs in a short-lived child, tools/check-verify-child.f,
\   whose image is the engine's boot prefix plus the verifier: neither this
\   process's words nor an earlier check's can stand in for, or collide with, a
\   word of the subject. As check.f's run stage does, the child runs on the
\   engine lib/engine-candidate.f names, a gate's HABU_UNDER_TEST or else the
\   engine running this process, never a bin/hb of the working directory. The
\   child is the tools/check-verify-child.f of the tree this file was loaded
\   from, named absolutely, and runs in this process's working directory,
\   whatever tree that holds.
\ - The verdict is the child's own result line. A child that ends without one -
\   an exit, a signal, the deadline - is `incomplete` and carries that status,
\   with the packets it wrote before; its exit status alone is never a verdict.
\
\ VERIFY-OUT$ is the checker's schema-1 packets, one JSON object per line: the
\ subject's name PATH's canonical absolute path and count positions in the
\ bytes; a dependency's name the dependency and count in its file. A duplicate
\ definition, which the checker writes no packet for, is the record
\ --all-errors writes for it (CHECK-ALL-ERRORS:DUP-RECORD$), and a closure that
\ cannot be discovered is a packet at the form discovery refused or at the
\ loader word naming a file that is not there, but for a string or a locals
\ group never closed, a stop at its opener (VERIFY-STOP). VERIFY-BYTES's stop
\ is the last line, its record (STOP-RECORD$) in JSON. VERIFY-FILES$ is the
\ files the verifier read, the subject and its dependencies, and VERIFY-DEFS$
\ the definitions it retained in them, each one JSON object per line, never
\ among the packets. VERIFY-LOG$ is the child's stderr, or for a closure the
\ walk cannot follow, which runs no child, the status line naming the file that
\ ended it and why. All four hold until the next call.
\
\ CHECK:PREVERIFY-BYTES is check.f's pre-pass, on the same child and image: the
\ first refused definition stops it, as it stops the load, and the subject's
\ packets carry the label check.f reports the subject by.
\
\ The require closure, discovered for the command line's named files as well,
\ is kept here, so a caller of the operation loads none of check.f's lints.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/adt/result.f
require lib/fs.f
require lib/source.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require tools/dynamic-tail-manifest.f
require tools/source-discovery.f
require tools/check-all-errors-core.f

package CHECK
using SOURCE-ROOT

\ check.f's exit statuses. The closure walk refuses a source that does not exist
\ with CHK-E-NOINPUT, and one the file system will not read with CHK-E-IOERR.
64 constant CHK-E-USAGE
66 constant CHK-E-NOINPUT
69 constant CHK-E-UNAVAILABLE
70 constant CHK-E-CHECK
74 constant CHK-E-IOERR

\ The closure's files, as many as it holds: each one's path and the root that
\ resolved it, FS-PATH-CAP bytes apiece, their lengths and its walk state (0
\ not walked, 1 walking, 2 walked); the direct dependencies found and not yet
\ followed, each with the file whose loader word names it, where that word
\ starts there and its length; and the files walked, dependencies first.
\ Growth moves a buffer, so a path is read through its id, never kept.
DYNAMIC-BUFFER CHK-DEP-PATHS u8
DYNAMIC-BUFFER CHK-DEP-US n
DYNAMIC-BUFFER CHK-DEP-ROOTS u8
DYNAMIC-BUFFER CHK-DEP-ROOT-US n
DYNAMIC-BUFFER CHK-DEP-STATES n
DYNAMIC-BUFFER CHK-DIR-IDS n
DYNAMIC-BUFFER CHK-DIR-FROM n
DYNAMIC-BUFFER CHK-DIR-AT n
DYNAMIC-BUFFER CHK-DIR-LEN n
DYNAMIC-BUFFER CHK-DEP-ORDER n

variable CHK-DEP-N
variable CHK-DIR-N
variable CHK-DEP-ORDER-N
variable CHK-DISC-ID
variable CHK-EDGE                       \ the dependency followed last, -1 for none
variable CHK-BYTES-ID
TYPED-VARIABLE CHK-BYTES-A ptr u8
variable CHK-BYTES-U
TYPED-VARIABLE CHK-BYTES-LABEL-A ptr u8
variable CHK-BYTES-LABEL-U
variable CHK-EXPAND-TOP


: CHK-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;


: CHK-DEP-CHECK ( n -- )
   {: id:n :}
   id 0 < id CHK-DEP-N @ >= or if E-TBL-BOUNDS throw then ;

: CHK-DEP-PATH ( n -- ptr u8 )
   {: id:n :}
   id CHK-DEP-CHECK
   id FS-PATH-CAP * CHK-DEP-PATHS ;

: CHK-DEP-U ( n -- ptr n )
   {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-US ;

: CHK-DEP-STATE ( n -- ptr n )
   {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-STATES ;

: CHK-DEP$ ( n -- ptr u8 n ) {: id:n :}
   id CHK-DEP-PATH
   id CHK-DEP-U @ ;

: CHK-DEP-ROOT$ ( n -- ptr u8 n )
   {: id:n :}
   id CHK-DEP-CHECK
   id FS-PATH-CAP * CHK-DEP-ROOTS
   id CHK-DEP-ROOT-US @ ;

: CHK-DEP-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   a u id CHK-DEP$ STR= ;

: CHK-DEP-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup CHK-DEP-N @ < while
      dup a u rot CHK-DEP-MATCH? if exit then
      1+
   repeat drop -1 ;

: CHK-DEP-NEW ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n root:ptr rootu:n :}
   u FS-PATH-CAP > rootu FS-PATH-CAP > or if E-FS-CAPACITY throw then
   CHK-DEP-N @ {: id:n :}
   id 1+ FS-PATH-CAP * dup CHK-DEP-PATHS-RESERVE CHK-DEP-ROOTS-RESERVE
   id 1+ dup CHK-DEP-US-RESERVE dup CHK-DEP-ROOT-US-RESERVE CHK-DEP-STATES-RESERVE
   id 1+ CHK-DEP-N !
   a id CHK-DEP-PATH u BYTE-COPY
   u id CHK-DEP-U !
   root id FS-PATH-CAP * CHK-DEP-ROOTS rootu BYTE-COPY
   rootu id CHK-DEP-ROOT-US !
   0 id CHK-DEP-STATE !
   id ;

: CHK-DEP-ID ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n root:ptr rootu:n :}
   a u CHK-DEP-FIND dup 0 >= if exit then
   drop a u root rootu CHK-DEP-NEW ;

\ A direct dependency of the file the walk is in, CHK-DISC-ID, named by the
\ loader word that starts at AT and has length LEN there.
: CHK-DIR-PUSH ( n n n -- )
   {: id:n at:n len:n :}
   CHK-DIR-N @ {: ix:n :}
   ix 1+ dup CHK-DIR-IDS-RESERVE dup CHK-DIR-FROM-RESERVE
   dup CHK-DIR-AT-RESERVE CHK-DIR-LEN-RESERVE
   id ix CHK-DIR-IDS !
   CHK-DISC-ID @ ix CHK-DIR-FROM !
   at ix CHK-DIR-AT !
   len ix CHK-DIR-LEN !
   ix 1+ CHK-DIR-N ! ;

: CHK-DEP-ORDER-PUSH ( n -- )
   {: id:n :}
   CHK-DEP-ORDER-N @ 1+ CHK-DEP-ORDER-RESERVE
   id CHK-DEP-ORDER-N @ CHK-DEP-ORDER !
   CHK-DEP-ORDER-N @ 1+ CHK-DEP-ORDER-N ! ;

: CHK-DEP-DIRECT+ ( ptr u8 n ptr u8 n n n -- )
   {: a:ptr u:n root:ptr rootu:n at:n len:n :}
   a u root rootu CHK-DEP-ID at len CHK-DIR-PUSH ;

\ Dependency closure: the shared whole-file ordered-event producer
\ (tools/source-discovery.f) scans every token of a file - colon bodies
\ included - and records one event per literal loader form
\ (include/included/require/required/provided). Every event path is a direct
\ dep, so the closure is a superset of the runtime load set; dynamic or
\ retired loader forms reject fail-closed unless manifested.
\
\ A walk ends at the first file it cannot follow: discovery refuses it, with an
\ E-DISC-* code, it does not exist, CHK-E-NOINPUT, or the file system will not
\ read it, CHK-E-IOERR. CHK-DISC-ID names it. The file CHK-BYTES-ID names is
\ read from the caller's bytes, never from disk.

: CHK-DISC-RC? ( n -- bool ) {: rc:n :}
   rc E-DISC-FIRST <= rc E-DISC-LAST >= and ;

: CHK-DISC-MSG$ ( n -- ptr u8 n ) {: rc:n :}
   rc E-DISC-SHADOW = if s" discovery rejected: loader word shadowed or undefined" exit then
   rc E-DISC-DYNAMIC = if s" discovery rejected: dynamic (non-literal) loader path" exit then
   rc E-DISC-OPENER = if s" discovery rejected: unsupported string opener before a loader word" exit then
   rc E-DISC-RETIRE = if s" discovery rejected: loader word retired (UNDEFINE-IF-DEFINED)" exit then
   rc E-DISC-UNTERM = if s" discovery rejected: unterminated string or locals group" exit then
   s" discovery rejected: capacity exceeded" ;

: CHK-READ-ACT ( -- )
   CHK-DISC-ID @ {: id:n :}
   id CHK-DEP$ id CHK-DEP-ROOT$ DISCOVER:READ-IN ;

: CHK-FS-RC? ( n -- bool )
   {: rc:n :}
   rc E-FS-FIRST <= rc E-FS-LAST >= and ;

\ How Q, a read of a source, ended: 0, or CHK-E-IOERR for a file the file
\ system refuses to read (a code of its block, such as E-FS-OPEN for a file
\ whose mode forbids it). Any other failure of the read goes on.
: CHK-READ-RC ( [ -- ] -- n ) {: q :}
   q catch {: rc:n :}
   rc CHK-FS-RC? if CHK-E-IOERR exit then
   rc 0<> if rc throw then
   0 ;

\ A file the file system refuses to read is the walk's CHK-E-IOERR, placed at
\ the loader word that names it.
: CHK-DISCOVER-ACT ( -- )
   CHK-DISC-ID @ {: id:n :}
   id CHK-BYTES-ID @ = if
      id CHK-DEP$ id CHK-DEP-ROOT$ CHK-BYTES-A @ CHK-BYTES-U @ DISCOVER:RUN-BYTES exit
   then
   [: CHK-READ-ACT ;] CHK-READ-RC {: rc:n :}
   rc 0<> if rc throw then
   DISCOVER:RUN-READ ;

: CHK-EVENT-DEP+ ( n -- ) {: ix:n :}
   ix EVENT-PATH@ ix SOURCE-EVENT:ROOT@ ix EVENT-TOK@ CHK-DEP-DIRECT+ ;

: CHK-EVENTS>DEPS ( -- )
   0 begin dup EVENT-COUNT < while
      dup CHK-EVENT-DEP+
      1+
   repeat drop ;

: CHK-EXPAND-ID ( n -- ) {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-STATE @ 2 = if exit then
   id CHK-DEP-STATE @ 1 = if exit then
   1 id CHK-DEP-STATE !
   id CHK-DISC-ID !
   CHK-DIR-N @
   id CHK-BYTES-ID @ <> if
      id CHK-DEP$ FILE? 0= if CHK-E-NOINPUT throw then
   then
   CHK-DISCOVER-ACT
   CHK-EVENTS>DEPS
   dup CHK-DIR-N @
   begin 2dup < while
      over CHK-EDGE !
      over CHK-DIR-IDS @ RECURSE
      swap 1+ swap
   repeat
   2drop CHK-DIR-N !
   id CHK-DEP-ORDER-PUSH
   2 id CHK-DEP-STATE ! ;

: CHK-EXPAND-RESET ( -- )
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-DEP-ORDER-N !
   -1 CHK-BYTES-ID ! ;

: CHK-EXPAND-TOP-ACT ( -- )
   CHK-EXPAND-TOP @ CHK-EXPAND-ID ;

\ Walk the closure below ID into the dependency order: 0, or the code of the
\ file that ended the walk. Any other throw goes on. The walk starts with no
\ dependency followed, whatever an earlier walk of the run followed, so a
\ fault in ID itself has no loader word.
: CHK-EXPAND ( n -- n )
   CHK-EXPAND-TOP !
   -1 CHK-EDGE !
   [: CHK-EXPAND-TOP-ACT ;] catch {: rc:n :}
   rc CHK-DISC-RC? rc CHK-E-NOINPUT = or rc CHK-E-IOERR = or rc 0= or 0= if rc throw then
   rc ;

\ Walk the closure over SRC, bytes that stand for the file at PATH with its
\ directory the root, as CHK-EXPAND walks a named file's: 0, or the code of the
\ file that ended the walk. A packet names the bytes' own file LABEL, the name
\ the caller's other reports give it. Discovery takes PATH as loading
\ meanwhile, so a require of an absent PATH meets the bytes too.
: CHK-EXPAND-BYTES ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n :}
   CHK-EXPAND-RESET
   path pathu path pathu DIRNAME CHK-DEP-ID {: id:n :}
   src CHK-BYTES-A !
   srcu CHK-BYTES-U !
   label CHK-BYTES-LABEL-A !
   labelu CHK-BYTES-LABEL-U !
   id CHK-BYTES-ID !
   path pathu DISCOVER:LOADING!
   id [: CHK-EXPAND ;] [: NULL$ DISCOVER:LOADING! ;] finally ;

DYNAMIC-BUFFER CHK-FILE-SRC u8          \ a closure file's bytes, read for a record

: CHK-FILE-ROOM ( n -- ptr u8 )
   CHK-FILE-SRC-RESERVE 0 CHK-FILE-SRC ;

\ The bytes of the file at PATH, read to its end however it grows while it is
\ read. Its size, whose refusal (E-FS-STAT) stays a missing or irregular file's,
\ is only the first room (lib/source.f READ-WHOLE-SAMPLED), and the bytes are
\ taken after the read, which may move the storage.
: CHK-FILE-READ ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu  path pathu FILE-SIZE  [: CHK-FILE-ROOM ;] SOURCE:READ-WHOLE-SAMPLED
   {: u:n :}
   0 CHK-FILE-SRC u ;

\ The bytes of the file with this id: the caller's for CHK-BYTES-ID, else the
\ file's own.
: CHK-FILE-BYTES ( n -- ptr u8 n )
   {: id:n :}
   id CHK-BYTES-ID @ = if CHK-BYTES-A @ CHK-BYTES-U @ exit then
   id CHK-DEP$ CHK-FILE-READ ;

\ The name a packet gives the file with this id: the caller's label for
\ CHK-BYTES-ID, else its path.
: CHK-FILE-NAME$ ( n -- ptr u8 n )
   {: id:n :}
   id CHK-BYTES-ID @ = if CHK-BYTES-LABEL-A @ CHK-BYTES-LABEL-U @ exit then
   id CHK-DEP$ ;

\ A fault's code, repair class and suggestion: a loader word naming a file that
\ is not there or that cannot be read, or a loader form discovery cannot follow.
: CHK-FAULT-CODE ( n -- ptr u8 n ptr u8 n ptr u8 n )
   {: rc:n :}
   rc CHK-E-NOINPUT = if
      s" E-MISSING-SOURCE" s" fix_load_path"
      s" No file is at the path this loader word names. Correct the path, or create the file." exit
   then
   rc CHK-E-IOERR = if
      s" E-UNREADABLE-SOURCE" s" make_source_readable"
      s" The file this loader word names cannot be read. Make it readable, or correct the path." exit
   then
   s" E-LOADER-FORM" s" literal_loader_form"
   s" Load a file by a literal path of at most 1024 bytes, as written and as resolved, through a loader word no definition redefines or retires, or list this file in tools/dynamic-tail-manifest.f." ;

\ The packet for fault RC at the token that starts at AT with length LEN in the
\ file with id ID, whose bytes are A U: one JSON object, no line feed.
: CHK-FAULT-JSON ( n n n n ptr u8 n -- ptr u8 n )
   {: rc:n id:n at:n len:n a:ptr u:n :}
   a at CHECK-ALL-ERRORS:BYTE-ORIGIN {: line:n col:n :}
   rc CHK-FAULT-CODE {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY code codeu LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY class classu LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY a at + len LJW-STRING LJW-COMMA
   s" file" LJW-KEY id CHK-FILE-NAME$ LJW-STRING LJW-COMMA
   s" line" LJW-KEY line LJW-U LJW-COMMA
   s" column" LJW-KEY col LJW-U LJW-COMMA
   s" byte_start" LJW-KEY at LJW-U LJW-COMMA
   s" byte_end" LJW-KEY at len + LJW-U LJW-COMMA
   s" suggestion" LJW-KEY sug sugu LJW-STRING
   LJW-OBJECT-END
   LJW$ ;

\ The packet for the fault RC that ended the walk, CHK-EXPAND's code, at the
\ token that shows it: the form discovery refused in the file it read, or the
\ loader word that names a file that is not there or cannot be read. Empty for
\ a walk that ended at its own top, which no loader word names.
: CHK-FAULT$ ( n -- ptr u8 n )
   {: rc:n :}
   rc CHK-DISC-RC? if
      rc CHK-DISC-ID @ DISCOVER:LAST-TOKEN DISCOVER:BYTES$ CHK-FAULT-JSON exit
   then
   CHK-EDGE @ {: edge:n :}
   edge 0 < if NULL$ exit then
   edge CHK-DIR-FROM @ {: from:n :}
   rc from edge CHK-DIR-AT @ edge CHK-DIR-LEN @ from CHK-FILE-BYTES CHK-FAULT-JSON ;

: CHK-TOK-END ( n -- n ) {: k:n :}
   k LINT-LEX:BYTE@ k LINT-LEX:TOKEN nip + ;

\ The declaration from token def to the end of token name, in the bytes the
\ lex read.
: CHK-NOM-SRC$ ( n n -- ptr u8 n ) {: def:n name:n :}
   def LINT-LEX:TOKEN drop
   name CHK-TOK-END def LINT-LEX:BYTE@ - ;

: CHK-NOM-JSTR ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n :}
   key keyu LJW-KEY val valu LJW-STRING LJW-COMMA ;

: CHK-NOM-JU ( n ptr u8 n -- )
   LJW-KEY LJW-U LJW-COMMA ;

\ A declaration packet up to its suggestion: the declaration from def to token
\ tok of the lex of the file the label names, which the packet names and
\ locates.
: CHK-PACKET-START ( n n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: def:n tok:n code:ptr codeu:n class:ptr classu:n word:ptr wordu:n label:ptr labelu:n :}
   LJW-RESET
   LJW-OBJECT-START
   1 s" schema_version" CHK-NOM-JU
   s" code" code codeu CHK-NOM-JSTR
   s" repair_class" class classu CHK-NOM-JSTR
   s" verdict" s" rejected" CHK-NOM-JSTR
   s" word" word wordu CHK-NOM-JSTR
   s" token" LJW-KEY tok LINT-LEX:TOKEN LJW-STRING LJW-COMMA
   tok s" token_index" CHK-NOM-JU
   s" file" LJW-KEY label labelu LJW-STRING LJW-COMMA
   tok LINT-LEX:LINE@ s" line" CHK-NOM-JU
   tok LINT-LEX:COL@ s" column" CHK-NOM-JU
   tok LINT-LEX:BYTE@ s" byte_start" CHK-NOM-JU
   tok CHK-TOK-END s" byte_end" CHK-NOM-JU
   s" definition_source" LJW-KEY def tok CHK-NOM-SRC$ LJW-STRING LJW-COMMA
   s" declared_effect" s" unknown " CHK-NOM-JSTR
   s" declared_effect_source" s" unknown" CHK-NOM-JSTR
   s" inferred_effect" s" unknown " CHK-NOM-JSTR
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" LJW-KEY s" " LJW-STRING LJW-COMMA
   s" actual" LJW-KEY s" " LJW-STRING
   LJW-OBJECT-END
   LJW-COMMA ;

\ The packet CHK-PACKET-START began, ended by its suggestion: one JSON object,
\ no line feed.
: CHK-PACKET$ ( ptr u8 n -- ptr u8 n ) {: sug:ptr sugu:n :}
   s" suggestion" LJW-KEY sug sugu LJW-STRING
   LJW-OBJECT-END
   LJW$ ;

: CHK-NONAME-SUG$ ( -- ptr u8 n )
   s" Give the definer a name: the next whitespace-delimited token." ;

\ The token of the source LINT-LEX read last that starts at the given byte, or
\ -1 when none does.
: CHK-TOKEN-AT ( n -- n )
   {: at:n :}
   0 begin dup LINT-LEX:COUNT < while
      dup LINT-LEX:BYTE@ at = if exit then
      1+
   repeat drop -1 ;

\ Definer k of the lex has nothing after it, so it has no name to read: the
\ loader refuses it, and the check does at the definer, in the file the label
\ names. The record is a packet when json, else a prose line; no line feed.
: CHK-NONAME-RECORD$ ( n ptr u8 n bool -- ptr u8 n )
   {: k:n label:ptr labelu:n json:bool :}
   json if
      k k s" E-MISSING-NAME" s" fix_missing_name" k LINT-LEX:TOKEN label labelu
      CHK-PACKET-START
      CHK-NONAME-SUG$ CHK-PACKET$ exit
   then
   LJW-RESET
   s" check.f: " LJW-RAW label labelu LJW-RAW
   58 LJW-C k LINT-LEX:LINE@ LJW-U
   58 LJW-C k LINT-LEX:COL@ LJW-U
   s" : missing name after '" LJW-RAW k LINT-LEX:TOKEN LJW-RAW 39 LJW-C
   LJW$ ;

\ The record of a reader with no name after it, which starts at the given byte
\ of the given bytes of the file the label names; empty when their lex has no
\ token there.
: CHK-NONAME-AT$ ( n ptr u8 n ptr u8 n bool -- ptr u8 n )
   {: at:n label:ptr labelu:n src:ptr srcu:n json:bool :}
   src srcu LINT-LEX:SOURCE
   at CHK-TOKEN-AT {: k:n :}
   k 0< if NULL$ exit then
   k label labelu json CHK-NONAME-RECORD$ ;

\ The record of what stopped a check with the given code, at the given byte of
\ the given bytes of the file the label names, a packet when json, else a prose
\ line, with no line feed, and whether it refuses the source by itself. A reader
\ with no name after it is the record the nominal pass writes where it stands,
\ a refusal. Any other throw out of a statement is the lexer's record for an
\ open string or row, and for a reader whose token the lexer reads into another
\ one, a refusal; else the record --all-errors writes for a statement that
\ throws, at the token that starts at that byte. Any other code has no record.
: STOP-RECORD$ ( n n ptr u8 n ptr u8 n bool -- ptr u8 n bool )
   {: rc:n at:n label:ptr labelu:n src:ptr srcu:n json:bool :}
   rc VERIFY:E-MISSING-NAME = if
      at label labelu src srcu json CHK-NONAME-AT$
      dup 0<> if true exit then
      2drop
   then
   rc CHECK-ALL-ERRORS:THREW? 0= if NULL$ false exit then
   json CHECK-ALL-ERRORS:JSON!
   rc CHECK-ALL-ERRORS:LEX-STOP? rc VERIFY:E-MISSING-NAME = or if
      label labelu src srcu CHECK-ALL-ERRORS:LEX-RECORD$
      dup 0<> if true exit then
      2drop
   then
   rc at label labelu src srcu CHECK-ALL-ERRORS:THROW-RECORD$ false ;

\ The files walked, dependencies first: the id at this place.
: CHK-DEP-ORDER@ ( n -- n )
   CHK-DEP-ORDER @ ;

public

\ What one CHECK:VERIFY-BYTES found. verified and refused are the checker's
\ verdict on PATH in its load context, refused whenever a file of the closure,
\ or the closure itself, is refused. engine-provided: the engine provides PATH,
\ so nothing is verified, whatever the bytes hold. held: the verifier's own
\ image holds PATH though the engine does not, so it cannot be verified there.
\ incomplete: the child ended without a result line; status is how it ended.
\ deferred: nothing is refused, but a stretch of top-level source or a definition
\ was deferred to the run, so the tokens a W-CHECK-DEFERRED packet locates are
\ not verified.
ENUM verdict 0
   VARIANT verified ;VARIANT
   VARIANT refused ;VARIANT
   VARIANT engine-provided ;VARIANT
   VARIANT held ;VARIANT
   VARIANT incomplete FIELD status outcome ;VARIANT
   VARIANT deferred ;VARIANT
;ENUM

private

$40000 constant VFY-ERR-CAP             \ the child's stderr
$0A constant VFY-LF

\ The child's answer, from its result line.
0 constant VFY-NONE
1 constant VFY-VERIFIED
2 constant VFY-REFUSED
3 constant VFY-HELD
4 constant VFY-STOPPED
5 constant VFY-DEFERRED

DYNAMIC-BUFFER VFY-OUT u8               \ the child's stdout, then VERIFY-OUT$
DYNAMIC-BUFFER VFY-LOG u8               \ VERIFY-LOG$
DYNAMIC-BUFFER VFY-REC u8               \ VFY-OUT, each duplicate line its record
DYNAMIC-BUFFER VFY-STOP-PATH u8         \ preserve the final stop across duplicate records
DYNAMIC-BUFFER VFY-DEFS u8              \ VERIFY-DEFS$
DYNAMIC-BUFFER VFY-FILES u8             \ VERIFY-FILES$
variable VFY-REC-U
variable VFY-DEFS-U
variable VFY-FILES-U
variable VFY-OUT-U
variable VFY-LOG-U
variable VFY-ANSWER
TYPED-VARIABLE VFY-DEADLINE ms          \ the child's, for VFY-CAPTURE
create VFY-PATH FS-PATH-CAP allot
variable VFY-PATH-U
create VFY-CHILD FS-PATH-CAP allot
variable VFY-CHILD-U

\ While this file loads, SOURCE-ROOT:CURRENT$ is the root that resolved it.
\ The child beside it is fixed here before the working directory can name another tree.
SOURCE-ROOT:CURRENT$ s" tools/check-verify-child.f" VFY-CHILD JOIN-PATH VFY-CHILD-U !

TYPED-VARIABLE VFY-PREPASS bool         \ the child runs check.f's pre-pass ...
TYPED-VARIABLE VFY-LABEL-A ptr u8       \ ... naming the subject by this label
variable VFY-LABEL-U
variable VFY-STOP-RC                    \ a stopped child: its code, 0 for none,
variable VFY-STOP-AT                    \ the byte of the token it read last,
variable VFY-STOP-DUP-AT                \ where the name it refused as a duplicate
variable VFY-STOP-DUP-U                 \ starts and its length, 0 for none,
TYPED-VARIABLE VFY-STOP-SUBJ bool       \ whether that is in the subject's bytes,
variable VFY-STOP-OFF                   \ and the file it is in, in VFY-OUT,
variable VFY-STOP-U
TYPED-VARIABLE VFY-STOP-DISC bool       \ or the one discovery stopped in


: VFY-CHILD$ ( -- ptr u8 n )
   VFY-CHILD VFY-CHILD-U @ ;


: VFY-RESET ( -- )
   0 VFY-OUT-U !
   0 VFY-LOG-U !
   0 VFY-DEFS-U !
   0 VFY-FILES-U !
   VFY-NONE VFY-ANSWER !
   0 VFY-STOP-RC !
   false VFY-STOP-DISC !
   false VFY-PREPASS ! ;


\ PATH's canonical absolute spelling, a relative PATH read from the working
\ directory: the identity the loader keys it by and the name its packets carry,
\ whether or not a file is there yet.
: VFY-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-FS-PATH throw then
   a u CANONICAL drop {: c:ptr cu:n :}
   cu FS-PATH-CAP > if E-FS-CAPACITY throw then
   c VFY-PATH cu BYTE-COPY
   cu VFY-PATH-U ! ;


: VFY-PATH$ ( -- ptr u8 n )
   VFY-PATH VFY-PATH-U @ ;


: VFY-OUT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   VFY-OUT-U @ u + VFY-OUT-RESERVE
   a VFY-OUT-U @ VFY-OUT u BYTE-COPY
   VFY-OUT-U @ u + VFY-OUT-U ! ;


: VFY-LOG+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   VFY-LOG-U @ u + VFY-LOG-RESERVE
   a VFY-LOG-U @ VFY-LOG u BYTE-COPY
   VFY-LOG-U @ u + VFY-LOG-U ! ;


: VFY-LOG-LN ( ptr u8 n -- )
   VFY-LOG+ s\" \n" VFY-LOG+ ;


\ The status line of the fault RC that ended the walk, in the file CHK-DISC-ID
\ names: `cannot read` that file, or the file and why: no such source, or
\ discovery's reason.
: VFY-CLOSURE-LOG ( n -- )
   {: rc:n :}
   rc CHK-E-IOERR = if
      s" cannot read " VFY-LOG+
      CHK-DISC-ID @ CHK-DEP$ VFY-LOG-LN exit
   then
   CHK-DISC-ID @ CHK-DEP$ VFY-LOG+
   s" : " VFY-LOG+
   rc CHK-E-NOINPUT = if s" no such source" VFY-LOG-LN exit then
   rc CHK-DISC-MSG$ VFY-LOG-LN ;


\ The walk's fault, as its packet and its status line: a walk over the caller's
\ bytes enters every other file through a loader word, so the fault is always
\ placed.
: VFY-CLOSURE-REPORT ( n -- )
   {: rc:n :}
   rc CHK-FAULT$ VFY-OUT+
   s\" \n" VFY-OUT+
   rc VFY-CLOSURE-LOG ;


\ A line of VERIFY-OUT$, when there is one.
: VFY-LINE+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u VFY-OUT+
   s\" \n" VFY-OUT+ ;


\ The stop's record, a packet, in the file the label names, whose bytes are
\ given (STOP-RECORD$).
: VFY-STOP-RECORD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: label:ptr labelu:n src:ptr srcu:n :}
   VFY-STOP-RC @ VFY-STOP-AT @ label labelu src srcu true STOP-RECORD$ drop ;


\ Discovery that ended the walk at a string or a locals group a file never
\ closes stops the verification at its opener, in the file CHK-DISC-ID names,
\ whose record ends the packets. Its status line is the log.
: VFY-DISC-STOP ( -- )
   E-DISC-UNTERM VFY-STOP-RC !
   DISCOVER:OPENER-AT VFY-STOP-AT !
   CHK-DISC-ID @ CHK-BYTES-ID @ = VFY-STOP-SUBJ !
   true VFY-STOP-DISC !
   CHK-DISC-ID @ CHK-DEP$ CHK-DISC-ID @ CHK-FILE-BYTES VFY-STOP-RECORD$ VFY-LINE+
   E-DISC-UNTERM VFY-CLOSURE-LOG ;


: VFY-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" CHK-ARG+
   VFY-CHILD$ CHK-ARG+
   s" --" CHK-ARG+
   VFY-PATH$ CHK-ARG+
   VFY-PREPASS @ if VFY-LABEL-A @ VFY-LABEL-U @ CHK-ARG+ then
   PROC-ENV-INHERIT-MISSING ;


TYPED-VARIABLE CHK-ENGINE-REFUSED bool  \ a child's engine selection ended the check

\ The engine a child of the check runs on, ENGINE-CANDIDATE:PATH$. A selection
\ the resolver refuses goes on as the resolver's own throw, and
\ CHK-ENGINE-REFUSED, held while the resolver runs, stays set: the check ended
\ there, where a failure of the source may share that code, and check.f's
\ command line reports it apart.
: CHK-ENGINE$ ( -- ptr u8 n )
   true CHK-ENGINE-REFUSED !
   ENGINE-CANDIDATE:PATH$
   false CHK-ENGINE-REFUSED ! ;


\ VFY-OUT holding at least N bytes: its first.
: VFY-OUT-ROOM ( n -- ptr u8 )
   VFY-OUT-RESERVE 0 VFY-OUT ;


\ The child's run on the subject's bytes, CHK-BYTES-A and CHK-BYTES-U: its
\ stdout into VFY-OUT, which grows to what the child writes, and its stderr
\ into VFY-LOG, their lengths and its end left where PROC-CAPTURE-OUTCOME@
\ reads them. The end is read there, so the one returned here is dropped
\ through the conversion that reads a deadline as a status rather than
\ throwing it.
: VFY-CAPTURE ( -- )
   VFY-ARGV
   VFY-ERR-CAP VFY-LOG-RESERVE
   CHK-ENGINE$ >LEN CHK-BYTES-A @ CHK-BYTES-U @ >LEN
   [: VFY-OUT-ROOM ;] 0 VFY-LOG VFY-ERR-CAP >LEN
   VFY-DEADLINE @ RUN-ARGV-ENV-STDIN-GROWING-CAPTURE-OUTCOME
   PROC-OUTCOME>DEADLINE-RC drop 2drop ;


: VFY-STOPPED$ ( -- ptr u8 n )
   s" check-verify: stopped " ;

: VFY-DUPLICATE$ ( -- ptr u8 n )
   s" check-verify: duplicate " ;

: VFY-DEFINITION$ ( -- ptr u8 n )
   s" check-verify: definition " ;

: VFY-FILE$ ( -- ptr u8 n )
   s" check-verify: file " ;


\ Where the field of VFY-OUT that starts at AT ends: at its space, or at END.
: VFY-FIELD-END ( n n -- n ) {: at:n end:n :}
   at begin
      dup end < if dup VFY-OUT c@ $20 <> else false then
   while 1+ repeat ;


\ The number VFY-OUT spells from AT to STOP, and whether it spells one.
: VFY-FIELD-N ( n n -- n bool ) {: at:n stop:n :}
   at VFY-OUT stop at - STR>NUMBER? MATCH option
      some OF true ENDOF
      none OF 0 false ENDOF
   ;MATCH ;


\ The fields after a stop or duplicate tag: RC BYTE DUP-AT DUP-LEN IN-SUBJECT
\ FILE. VFY-STOPPED with them kept, or VFY-NONE for an invalid line; a stop is
\ never code 0, and IN-SUBJECT is 1 or 0.
: VFY-STOP-PARSE ( n n -- n ) {: at:n end:n :}
   at end VFY-FIELD-END {: e1:n :}
   at e1 VFY-FIELD-N {: rc:n rc-ok:bool :}
   e1 1+ end VFY-FIELD-END {: e2:n :}
   e1 1+ e2 VFY-FIELD-N {: byte:n byte-ok:bool :}
   e2 1+ end VFY-FIELD-END {: e3:n :}
   e2 1+ e3 VFY-FIELD-N {: name-at:n name-at-ok:bool :}
   e3 1+ end VFY-FIELD-END {: e4:n :}
   e3 1+ e4 VFY-FIELD-N {: name-u:n name-u-ok:bool :}
   e4 1+ end VFY-FIELD-END {: e5:n :}
   e4 1+ e5 VFY-FIELD-N {: subj:n subj-ok:bool :}
   rc-ok byte-ok and name-at-ok and name-u-ok and subj-ok and rc 0<> and
   subj 0 = subj 1 = or and
   e5 1+ end < and 0= if VFY-NONE exit then
   rc VFY-STOP-RC !
   byte VFY-STOP-AT !
   name-at VFY-STOP-DUP-AT !
   name-u VFY-STOP-DUP-U !
   subj 1 = VFY-STOP-SUBJ !
   e5 1+ VFY-STOP-OFF !
   end e5 1+ - VFY-STOP-U !
   VFY-STOPPED ;


\ A result line grants a verdict only after a clean child exit. A complete
\ result line is still framing, even when the process ends uncleanly.
: VFY-CLEAN-EXIT? ( outcome -- bool )
   MATCH outcome
      exited OF 0= ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH ;


\ The answer the final line from AT to END in VFY-OUT gives, its line feed
\ excluded. Diagnostic duplicates have a different tag.
: VFY-ANSWER-AT ( n n -- n ) {: at:n end:n :}
   at VFY-OUT end at - {: a:ptr u:n :}
   a u s" check-verify: verified" STR= if VFY-VERIFIED exit then
   a u s" check-verify: refused" STR= if VFY-REFUSED exit then
   a u s" check-verify: deferred" STR= if VFY-DEFERRED exit then
   a u s" check-verify: held" STR= if VFY-HELD exit then
   a u VFY-STOPPED$ STARTS-WITH? if at VFY-STOPPED$ nip + end VFY-STOP-PARSE exit then
   VFY-NONE ;


\ Where the line that ends at END in VFY-OUT starts.
: VFY-LINE-START ( n -- n )
   begin dup 0 > while
      dup 1- VFY-OUT c@ VFY-LF = if exit then
      1-
   repeat ;


\ Keep the complete lines of the child's OUTU bytes of stdout: the packets, and
\ the answer of a result line that ends them. The rest of a line the deadline or
\ the capture cut is dropped.
: VFY-TAKE ( n -- )
   {: outu:n :}
   outu VFY-LINE-START {: end:n :}
   end VFY-OUT-U !
   end 0= if exit then
   end 1- VFY-LINE-START {: at:n :}
   at end 1- VFY-ANSWER-AT VFY-ANSWER !
   VFY-ANSWER @ VFY-NONE = if exit then
   at VFY-OUT-U ! ;


\ The file the last stopped line read names, in VFY-OUT.
: VFY-STOP-FILE$ ( -- ptr u8 n )
   VFY-STOP-OFF @ VFY-OUT VFY-STOP-U @ ;


\ That file's bytes: the subject's, or the file's own (CHK-FILE-READ).
: VFY-STOP-SOURCE ( -- ptr u8 n )
   VFY-STOP-SUBJ @ if CHK-BYTES-A @ CHK-BYTES-U @ exit then
   VFY-STOP-FILE$ CHK-FILE-READ ;


\ The child's stop: its record ends the packets, and the file the stopped line
\ named stays after them, where VFY-STOP-FILE$ reads it.
: VFY-STOP-LINE ( -- )
   VFY-STOP-FILE$ VFY-STOP-SOURCE VFY-STOP-RECORD$ {: a:ptr u:n :}
   VFY-STOP-U @ {: fileu:n :}
   fileu VFY-STOP-PATH-RESERVE
   VFY-STOP-FILE$ drop 0 VFY-STOP-PATH fileu BYTE-COPY
   a u VFY-LINE+
   VFY-OUT-U @ fileu + VFY-OUT-RESERVE
   VFY-OUT-U @ VFY-STOP-OFF !
   0 VFY-STOP-PATH VFY-STOP-OFF @ VFY-OUT fileu BYTE-COPY ;


: VFY-REC+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   VFY-REC-U @ u + VFY-REC-RESERVE
   a VFY-REC-U @ VFY-REC u BYTE-COPY
   VFY-REC-U @ u + VFY-REC-U ! ;


: VFY-DEFS+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   VFY-DEFS-U @ u + VFY-DEFS-RESERVE
   a VFY-DEFS-U @ VFY-DEFS u BYTE-COPY
   VFY-DEFS-U @ u + VFY-DEFS-U ! ;


\ The JSON object of the definition line from AT to END in VFY-OUT, to
\ VFY-DEFS.
: VFY-DEF-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-DEFINITION$ nip +
   {: obj:n :}
   obj VFY-OUT end obj - VFY-DEFS+
   s\" \n" VFY-DEFS+ ;


: VFY-FILES+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   VFY-FILES-U @ u + VFY-FILES-RESERVE
   a VFY-FILES-U @ VFY-FILES u BYTE-COPY
   VFY-FILES-U @ u + VFY-FILES-U ! ;


\ The JSON object of the file line from AT to END in VFY-OUT, to VFY-FILES.
: VFY-FILE-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-FILE$ nip +
   {: obj:n :}
   obj VFY-OUT end obj - VFY-FILES+
   s\" \n" VFY-FILES+ ;


\ Where the line of VFY-OUT that starts at AT ends: at its line feed.
: VFY-LINE-END ( n -- n )
   begin
      dup VFY-OUT-U @ < if dup VFY-OUT c@ VFY-LF <> else false then
   while 1+ repeat ;


\ The line of VFY-OUT that starts at AT, to VFY-REC, a duplicate line
\ as the record --all-errors writes for it, a file line to VFY-FILES and a
\ definition line to VFY-DEFS: where the next line starts.
: VFY-REC-LINE ( n -- n )
   {: at:n :}
   at VFY-LINE-END
   {: end:n :}
   at VFY-OUT end at - VFY-FILE$ STARTS-WITH? if
      at end VFY-FILE-LINE
      end 1+ exit
   then
   at VFY-OUT end at - VFY-DEFINITION$ STARTS-WITH? if
      at end VFY-DEF-LINE
      end 1+ exit
   then
   at VFY-OUT end at - VFY-DUPLICATE$ STARTS-WITH?
   if at VFY-DUPLICATE$ nip + end VFY-STOP-PARSE else VFY-NONE then
   VFY-STOPPED = if
      VFY-STOP-RC @ E-DUP-DEFINITION = if
         true CHECK-ALL-ERRORS:JSON!
         VFY-STOP-DUP-AT @ VFY-STOP-DUP-U @ VFY-STOP-FILE$ VFY-STOP-SOURCE
         CHECK-ALL-ERRORS:DUP-RECORD$ VFY-REC+
         s\" \n" VFY-REC+
      then
   else
      at VFY-OUT end at - VFY-REC+
      s\" \n" VFY-REC+
   then
   end 1+ ;


\ The packets VFY-OUT holds, each duplicate line the first form writes among them
\ for a duplicate made its record, each file line moved to VFY-FILES and each
\ definition line to VFY-DEFS.
: VFY-DUP-RECORDS ( -- )
   VFY-PREPASS @ if exit then
   VFY-OUT-U @ 0= if exit then
   VFY-ANSWER @ VFY-STOPPED =
   VFY-STOP-RC @ VFY-STOP-AT @ VFY-STOP-DUP-AT @ VFY-STOP-DUP-U @
   VFY-STOP-SUBJ @ VFY-STOP-U @
   {: stopped:bool rc:n byte:n at:n u:n subj:bool fileu:n :}
   stopped if
      fileu VFY-STOP-PATH-RESERVE
      VFY-STOP-FILE$ drop 0 VFY-STOP-PATH fileu BYTE-COPY
   then
   0 VFY-REC-U !
   0 begin dup VFY-OUT-U @ < while VFY-REC-LINE repeat drop
   stopped if VFY-REC-U @ fileu + else VFY-REC-U @ then VFY-OUT-RESERVE
   \ Definition lines alone leave no record, and VFY-REC no storage to copy.
   VFY-REC-U @ 0<> if 0 VFY-REC 0 VFY-OUT VFY-REC-U @ BYTE-COPY then
   VFY-REC-U @ VFY-OUT-U !
   stopped if
      rc VFY-STOP-RC !  byte VFY-STOP-AT !
      at VFY-STOP-DUP-AT !  u VFY-STOP-DUP-U !
      subj VFY-STOP-SUBJ !  fileu VFY-STOP-U !
      VFY-OUT-U @ VFY-STOP-OFF !
      0 VFY-STOP-PATH VFY-STOP-OFF @ VFY-OUT fileu BYTE-COPY
   else
      0 VFY-STOP-RC !
   then ;


\ How the child ended. More stderr than the capture holds kills it, and its
\ E-PROC-TRUNCATED is thrown on once what was captured is kept.
: VFY-RUN ( -- outcome )
   [: VFY-CAPTURE ;] catch {: rc:n :}
   rc 0<> rc E-PROC-TRUNCATED <> and if rc throw then
   PROC-CAPTURE-OUTCOME@ {: outu:len erru:len o :}
   erru LEN>N VFY-LOG-U !
   outu LEN>N VFY-TAKE
   VFY-DUP-RECORDS
   rc 0<> if rc throw then
   o ;


\ The child's answer once it has given one of the first form's and exited
\ clean, a stop refused; any other end is incomplete.
: VFY-VERDICT ( outcome -- verdict ) {: o :}
   o VFY-CLEAN-EXIT? if
      VFY-ANSWER @ VFY-VERIFIED = if CHECK-VERDICT:verified exit then
      VFY-ANSWER @ VFY-REFUSED = if CHECK-VERDICT:refused exit then
      VFY-ANSWER @ VFY-STOPPED = if CHECK-VERDICT:refused exit then
      VFY-ANSWER @ VFY-DEFERRED = if CHECK-VERDICT:deferred exit then
      VFY-ANSWER @ VFY-HELD = if CHECK-VERDICT:held exit then
   then
   o CHECK-VERDICT:incomplete ;


\ The pre-pass's answer once it has given one and exited clean: 0 verified,
\ else the code it stopped with. Any other end is the error.
: VFY-PREVERDICT ( outcome -- result<n,outcome> ) {: o :}
   o VFY-CLEAN-EXIT? if
      VFY-ANSWER @ VFY-VERIFIED = if 0 RESULT:OK exit then
      VFY-ANSWER @ VFY-STOPPED = if VFY-STOP-RC @ RESULT:OK exit then
   then
   o RESULT:ERR ;

public

\ The packets of the last VERIFY-BYTES or PREVERIFY-BYTES, one JSON object per
\ line.
: VERIFY-OUT$ ( -- ptr u8 n )
   VFY-OUT-U @ 0= if NULL$ exit then
   0 VFY-OUT VFY-OUT-U @ ;

\ The prose of the last VERIFY-BYTES or PREVERIFY-BYTES.
: VERIFY-LOG$ ( -- ptr u8 n )
   VFY-LOG-U @ 0= if NULL$ exit then
   0 VFY-LOG VFY-LOG-U @ ;

\ The definitions the last VERIFY-BYTES retained, one JSON object per line as
\ tools/check-verify-child.f's definition line states it, in the order the
\ verifier read them.
: VERIFY-DEFS$ ( -- ptr u8 n )
   VFY-DEFS-U @ 0= if NULL$ exit then
   0 VFY-DEFS VFY-DEFS-U @ ;

\ The files the last VERIFY-BYTES read, one JSON object per line as
\ tools/check-verify-child.f's file line states it, each once, in the order
\ the verifier started them.
: VERIFY-FILES$ ( -- ptr u8 n )
   VFY-FILES-U @ 0= if NULL$ exit then
   0 VFY-FILES VFY-FILES-U @ ;

\ Check the bytes as the file at PATH, the child given DEADLINE. A throw that
\ ends the verification refuses it, as does a string or a locals group a file
\ of the closure never closes: the stop's record ends VERIFY-OUT$, and
\ VERIFY-STOP and the words after it say with what and where. Any other closure
\ the walk cannot follow refuses it with the walk's packet in VERIFY-OUT$.
\ Either way VERIFY-LOG$ is the walk's status line. An empty PATH is
\ E-FS-PATH; an engine lib/engine-candidate.f refuses (E-FS-OPEN) or a failed
\ spawn throws as well. stdout is kept whole; more than 256 KiB of the child's
\ stderr is E-PROC-TRUNCATED, VERIFY-OUT$ then holding every complete packet
\ received before it. A file of the closure that a stop or a duplicate
\ definition is in and can no longer be read throws as reading it does.
: VERIFY-BYTES ( ptr u8 n ptr u8 n ms -- verdict )
   {: src:ptr srcu:n path:ptr pathu:n deadline :}
   VFY-RESET
   path pathu VFY-PATH!
   VFY-PATH$ ENGINE-PROVIDES? if CHECK-VERDICT:engine-provided exit then
   src srcu VFY-PATH$ VFY-PATH$ CHK-EXPAND-BYTES {: rc:n :}
   rc E-DISC-UNTERM = if VFY-DISC-STOP CHECK-VERDICT:refused exit then
   rc 0<> if rc VFY-CLOSURE-REPORT CHECK-VERDICT:refused exit then
   deadline VFY-DEADLINE !
   VFY-RUN {: o :}
   o VFY-CLEAN-EXIT? VFY-ANSWER @ VFY-STOPPED = and if VFY-STOP-LINE then
   o VFY-VERDICT ;

\ check.f's pre-pass of the bytes as the file at PATH, the subject named LABEL
\ in its packets, the child given DEADLINE. The child's image is the one
\ VERIFY-BYTES verifies on, so the pre-pass resolves the engine's words and the
\ subject's loads, as the run does, never a word only this process loaded. It
\ stops at the first definition the checker refuses, as the load does: ok 0 when
\ nothing stopped it, else ok the code it stopped with, and VERIFY-STOP-AT,
\ PREVERIFY-DUPLICATE, VERIFY-STOP-SUBJECT? and VERIFY-STOPPED$ say where.
\ err is how a child ended that gave no answer. An empty PATH is E-FS-PATH; a
\ failed spawn throws, and more child stderr than the capture holds is
\ E-PROC-TRUNCATED, as for VERIFY-BYTES.
: PREVERIFY-BYTES ( ptr u8 n ptr u8 n ptr u8 n ms -- result<n,outcome> )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n deadline :}
   VFY-RESET
   path pathu VFY-PATH!
   true VFY-PREPASS !
   label VFY-LABEL-A !
   labelu VFY-LABEL-U !
   src CHK-BYTES-A !
   srcu CHK-BYTES-U !
   deadline VFY-DEADLINE !
   VFY-RUN VFY-PREVERDICT ;

\ The code a throw stopped the last VERIFY-BYTES or PREVERIFY-BYTES with,
\ E-DISC-UNTERM for discovery's stop at a string or group, 0 when none did.
: VERIFY-STOP ( -- n )
   VFY-STOP-RC @ ;

\ Where it stopped: the byte where the token it stopped at starts, the one the
\ verifier read last or the opener of the statement it was in, whether that is
\ in the bytes it was given, and the file it is in, PATH's canonical spelling or
\ the LABEL for those bytes.
: VERIFY-STOP-AT ( -- n )
   VFY-STOP-AT @ ;

\ The name the last PREVERIFY-BYTES refused as a duplicate, in the same file:
\ the byte where it starts and its length, 0 when it kept no name
\ (VERIFY:DUPLICATE).
: PREVERIFY-DUPLICATE ( -- n n )
   VFY-STOP-DUP-AT @ VFY-STOP-DUP-U @ ;

: VERIFY-STOP-SUBJECT? ( -- bool )
   VFY-STOP-SUBJ @ ;

: VERIFY-STOPPED$ ( -- ptr u8 n )
   VFY-STOP-DISC @ if CHK-DISC-ID @ CHK-DEP$ exit then
   VFY-STOP-FILE$ ;

;using
;package
