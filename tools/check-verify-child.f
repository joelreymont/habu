\ check-verify-child.f - the verifier child LSP-CHECK:VERIFY-BYTES,
\ LSP-CHECK:VERIFY-BYTES-AT and LSP-CHECK:PREVERIFY-BYTES run. Only
\ tools/check-verify-core.f spawns it, on the engine src/lib/engine-candidate.f
\ names, by its absolute path in that file's tree, in the caller's working
\ directory.

\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT [--prose] < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT (--label LABEL | --entry) [--prose] < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --at N < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --of TAIL < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --index < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- --batch TAIL < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- --batch-ref TAIL < STDIN

\ STDIN is the open documents, each a line `PU U` then PU bytes of PATH and U
\ bytes of TEXT, the counts in decimal; then an empty line; then BYTES, to the
\ end. BYTES is the subject's text and SUBJECT the canonical absolute path it is
\ checked as. PATH is a file's canonical absolute path, and a load of that file
\ other than SUBJECT scans TEXT in place of its bytes on disk
\ (VERIFY:SUPPLIED); of two entries for one PATH the first holds. The file
\ must still be on disk: the loader resolves it there, and one that is not is
\ no such source. check.f's pre-pass, the second form, sends none.
\ Under --batch and --batch-ref, STDIN is frames instead, each a line `PU SU`
\ then PU bytes of a subject's path and SU bytes of its text, then its open
\ documents as above and an empty line; a `0 0` line ends the stream, and
\ nothing follows it.
\
\ The child is the engine, which carries the verifier, with this file and
\ tools/check-verify-child-lib.f, its body, so no tool's word stands in for, or
\ collides with, a word of the subject. Nothing of SUBJECT or its closure runs:
\ the verifier scans source and records what it declares.

\ The verifier follows top-level loader acts in one neutral checker scope, with
\ each file's path and positions while it is open. `require` skips held paths;
\ `include` verifies every occurrence. The first four forms compose:
\ the composition reads each file a loader loads and rejects a bad loader form
\ where it meets it.
\ `--index` scans only the supplied subject bytes, including for an
\ engine-provided path. It emits surviving declarations after a completed
\ scan, while their checker scopes are still open. A top-level dictionary call
\ defers the whole index; the parent consumes no rows from a deferred result.
\ A scan a throw ends emits no declarations, even if earlier ones survived.
\
\ stdout is the records, one per rejection in verification order, each written
\ where the rejection is made, so a child that dies has passed on every record
\ made before; then one result line. A record is a schema-1 JSON object on a
\ line, or with `--prose` a prose record. The first form verifies with all
\ errors, going past a duplicate definition as past any rejected definition.
\ Among the records it writes one JSON object per line: a file line for each
\ file it reads, once however often it reads the same bytes and before anything
\ in them; a definition line for each definition it retains; a use line for each
\ use in the subject the checker binds to a source target or identifies as an
\ engine primitive; a load line for each top-level loader of the subject whose
\ load reached a file. It answers

\    check-verify: verified | rejected | deferred | stopped | held | full
\                | missing TARGET | unread TARGET
\    check-verify: file {"file":F,"source":"subject|document|disk",
\       "size":S,"mtime_sec":MS,"mtime_ns":MN,
\       "ctime_sec":CS,"ctime_ns":CN}
\    check-verify: definition {"kind":K,"class":C,"word":W,
\       "decl_visit":D,"decl_name":T,"package":P,"visibility":V,
\       "effect":E,"file":F,"byte_start":S,"byte_end":N}
\    check-verify: use {"byte_start":S,"byte_end":N,"in":I,"file":F,
\       "target_start":TS,"target_end":TN,"decl_visit":D,
\       "decl_name":T,"package":P,"visibility":V}
\    check-verify: load {"byte_start":S,"byte_end":N,"path":P,"outcome":O}
\    check-verify: candidate {"word":W,"file":F,"target_start":TS,
\       "target_end":TN}
\
\ A throw that ends the composition after its rejection wrote its record
\ answers rejected, missing, unread or full. missing is a file a loader names
\ that is not there, unread one found that cannot be read, and full a
\ dictionary with no record left; TARGET, the rest of the line after a space
\ and absent when empty, is that file as it was resolved
\ (VERIFY:FAULT-TARGET$). A throw of any other code ends the child with no
\ result line.
\
\ A file line names each file the composition visits. Subject and open
\ document bytes have an editor source; disk bytes carry the metadata captured
\ around their acquisition by the quiet reader. Different stamps of the same
\ path in one composition make its locations unusable. A definition line names
\ each definition the checker records:
\ K the statement token that declared it, C its class as that token's path
\ knows it (word, constant, storage, export or a generated SUMTYPE constructor),
\ W its name as the source writes it
\ (a name its definer generates, as spelled), P and V the package (empty for a
\ global) and visibility (global, private or public) the checker recorded it
\ under, T its recorded canonical tail, D its local file visit, E its declared
\ effect, absent when it declared none, F the file as its records name it, and
\ S and N where the token that declared it starts and ends there.
\
\ A use line names each use the checker bound: S and N where the use starts and
\ ends in I, the file it is in as its records name it, a token, or the bytes
\ between the quotes of a string literal naming a word before a call of a word
\ a `names:` row marks, F the path the
\ declaration's file was resolved to, TS and TN where the token that declared
\ it starts and ends there, and D, T, P and V the selected binding's local
\ visit, canonical tail, package and visibility. For an engine primitive in
\ PRIM-SPEC, D is -2 minus its selected row. F names src/habu/prims.f when
\ its path is known. TS and TN span its name only when that source matches
\ the running table; otherwise they are zero while D stays distinct.
\ A word without a source target or primitive row has D of -1. An accepted
\ engine definition can also have D of -1 with F, TS and TN naming its source.
\
\ A load line names what src/habu/verify-source.f ON-LOADER reports, a
\ top-level loader of the subject whose load reached a file, in the subject's
\ order and before anything in that file: S and N where its operand starts and
\ ends in the subject (the token after include or require, a string loader's
\ literal between its quotes as written), P the canonical path, and O read,
\ the file's bytes were acquired for this load, or held, a require of a path
\ the engine provides or that was recorded before, which read nothing.
\
\ The third form is the first with a completion cursor at byte N of BYTES. It
\ writes as well, for each spelling the checker offers at the cursor's place, a
\ candidate line naming what src/habu/verify-source.f ON-CANDIDATE reports: W
\ the spelling, F the path the file of the declaration it binds was resolved
\ to, and TS and TN where the token that declared it starts and ends there; F,
\ TS and TN are absent for a declaration in no file the composition read, or
\ with no location. A cursor the scan passes in a comment, a string or a
\ signature, or never reaches, has none.
\
\ The fourth form is the first that writes as well a use line for each use
\ the checker binds to a located declaration in a file a load reads, each time
\ it is read, when the declaration's canonical tail T is TAIL, byte for byte:
\ every use outside the subject of a word whose tail is TAIL.
\ The other forms write the subject's uses alone.
\ The batch-ref form is for primitive references: it writes uses of TAIL in
\ the subject and loaded files, including unlocated bindings, and omits
\ definition lines. File lines and result lines describe each check as in the
\ first form.
\
\ deferred: nothing is rejected, but a stretch of top-level source or a
\ definition was deferred to the run, and a W-CHECK-DEFERRED record locates
\ each: those tokens are not verified. stopped: nothing is rejected, but the
\ check stopped at a W-CHECK-STOPPED record and verified nothing after it.
\ held is answered before anything is verified: this child holds SUBJECT though
\ the engine does not provide it (this file, tools/check-verify-child-lib.f,
\ and the verifier's source on an engine that does not carry it), so it
\ cannot be verified here. The parent answers engine-provided itself.
\
\ The second form is check.f's pre-pass. The first definition the checker
\ rejects stops the composition, as it stops the load, and a record that names
\ the subject names it LABEL. SUBJECT is check.f's own copy of the text, which
\ nothing holds. With `--entry` BYTES are entry text. It answers
\
\    check-verify: verified | rejected | full | missing TARGET | unread TARGET
\
\ With `--prose`, for a check.f run without --json-errors, the first and second
\ forms write each record as prose where the other forms write it as JSON.
\
\ stderr is whatever the engine writes there, a `die`'s message among it. A
\ child that ends any other way, the verifier's own `die` included, writes no
\ result line.

\ The child runs in the client's working directory, which can contain a
\ different Habu revision. Its body belongs to this entry's tree.
package CHECK-VERIFY-LOAD
create ROOT PATH-CAP allot   variable ROOT-U
create OLD PATH-CAP allot    variable OLD-U

: SAVE ( -- )
   SOURCE-ROOT:ENGINE$ {: e:str :}
   e STR:DATA OLD e STR:LEN BYTE-COPY  e STR:LEN OLD-U !
   SOURCE-ROOT:ACTIVE-SOURCE drop drop
   SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME {: root:str :}
   root STR:DATA ROOT root STR:LEN BYTE-COPY  root STR:LEN ROOT-U ! ;

: LOAD ( -- ) s" tools/check-verify-child-lib.f" required ;
: LOAD-ROOT ( -- ) ROOT ROOT-U @ >str [: LOAD ;] SOURCE-ROOT:WITH ;

public
: START ( -- )
   SAVE
   ROOT ROOT-U @ >str SOURCE-ROOT:ENGINE!
   [: LOAD-ROOT ;] catch {: rc:n :}
   OLD OLD-U @ >str SOURCE-ROOT:ENGINE!

   rc 0<> if rc throw then ;
;package

CHECK-VERIFY-LOAD:START

CHECK-VERIFY-CHILD:MAIN
