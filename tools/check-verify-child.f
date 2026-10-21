\ check-verify-child.f - the verifier child CHECK:VERIFY-BYTES,
\ CHECK:VERIFY-BYTES-AT and CHECK:PREVERIFY-BYTES run.
\
\ tools/check-verify-core.f spawns it on the engine src/lib/engine-candidate.f
\ names, by its absolute path in the tree that file was loaded from, in the
\ caller's working directory; nothing else runs it:
\
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT LABEL < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --at N < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --of TAIL < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --index < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- --batch TAIL < STDIN
\    ENGINE --load ROOT/tools/check-verify-child.f -- --batch-ref TAIL < STDIN
\    IMAGE -- SUBJECT < STDIN
\    IMAGE -- SUBJECT LABEL < STDIN
\    IMAGE -- SUBJECT --at N < STDIN
\    IMAGE -- SUBJECT --of TAIL < STDIN
\    IMAGE -- SUBJECT --index < STDIN
\    IMAGE -- --batch TAIL < STDIN
\    IMAGE -- --batch-ref TAIL < STDIN
\
\ IMAGE is the saved verifier image (tools/check-verify-image.f, built by
\ tools/check-verify-save.f) core runs when the engine is its own and can save
\ one; this file is the child on any other engine.
\
\ STDIN is the open documents, each a line `PU U` then PU bytes of PATH and U
\ bytes of TEXT, the counts in decimal; then an empty line; then BYTES, to the
\ end. BYTES is the subject's text and SUBJECT the canonical absolute path it is
\ checked as. PATH is a file's canonical absolute path, and a load of that file
\ other than SUBJECT scans TEXT in place of its bytes on disk
\ (VERIFY:SUPPLIED); of two entries for one PATH the first holds. The file
\ must still be on disk: the loader resolves it there, and one that is not is
\ no such source. check.f's pre-pass, the second form, sends none.
\
\ The image is the engine's boot prefix, the verifier and
\ tools/check-verify-child-lib.f, its body, with this file or, in the saved
\ image, tools/check-verify-save.f, so no tool's word stands in for, or
\ collides with, a word of the subject. Nothing of SUBJECT or its closure runs:
\ the verifier scans source and records what it declares.
\
\ The verifier follows top-level loader acts in one neutral checker scope, with
\ each file's path and positions while it is open. `require` skips held paths;
\ `include` verifies every occurrence. The first, third and fourth forms compose
\ quietly (VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE), so no walk of the closure
\ runs before them: the composition reads each file a loader loads and refuses
\ the loader forms tools/source-discovery.f refuses where it meets them.
\ `--index` scans only the supplied subject bytes, including for an
\ engine-provided path. It emits surviving declarations after a completed
\ scan, while their checker scopes are still open. A top-level dictionary call
\ defers the whole index; the parent consumes no rows from a deferred result.
\ A thrown scan has no result line, even if earlier declarations survived.
\
\ stdout is the schema-1 JSON packets, one per line in verification order, each
\ written as the checker makes it, so a child that dies has passed on every
\ packet made before; then one result line. The first form verifies with all
\ errors, going past a duplicate definition as past any refused definition,
\ and writes for each duplicate, which the checker writes no packet for, a
\ diagnostic line among the packets, for each file it reads a file line for
\ the bytes it reads there, once however often it reads the same bytes and
\ before anything in them, for each definition it retains a definition line,
\ a use line for each use in the subject that the checker binds to a source
\ target or identifies as an engine primitive, and for each top-level loader
\ of the subject whose load reached
\ a file a load line, one JSON object each. It answers
\
\    check-verify: verified | refused | deferred | held
\                | stopped RC BYTE DUP-AT DUP-LEN IN-SUBJECT FILE
\    check-verify: duplicate 78 BYTE DUP-AT DUP-LEN IN-SUBJECT FILE
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
\    check-verify: loader LEN TARGET
\
\ A loader line comes right before the stopped line of a stop at a loader's
\ fault (VERIFY:LOADER-FAULT?): LEN is the length of the loader token at the
\ stopped line's BYTE, 0 for a string or locals group a file never closes, and
\ TARGET, the rest of the line and absent when empty, the file the loader could
\ not read, as it was resolved.
\
\ A file line names each file visit reported by ON-FILE. Subject and open
\ document bytes have an editor source; disk bytes carry the metadata captured
\ around their acquisition by the quiet reader. Different stamps of the same
\ path in one composition make its locations unusable. A definition line names
\ what ON-DEFINITION reports:
\ K the statement token that declared it, C its class as that token's path
\ knows it (word, constant, storage, export or a generated SUMTYPE constructor),
\ W its name as the source writes it
\ (a name its definer generates, as spelled), P and V the package (empty for a
\ global) and visibility (global, private or public) the checker recorded it
\ under, T its recorded canonical tail, D its local file visit, E its declared
\ effect, absent when it declared none, F the file as its packets name it, and
\ S and N where the token that declared it starts and ends there.
\
\ A use line names what src/habu/verify-source.f ON-USE reports, a use that
\ the checker bound: S and N where the use starts and
\ ends in I, the file it is in as its packets name it, a token, or the bytes
\ between the quotes of a string literal naming a word before a call of a word
\ a `names:` row marks (src/core/checker.f CHECKER-NAMES-ROW), F the path the
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
\ every use outside the subject (VERIFY:SUBJECT?) of a word whose tail is TAIL.
\ The other forms write the subject's uses alone.
\ The batch-ref form is for primitive references: it writes uses of TAIL in
\ the subject and loaded files, including unlocated bindings, and omits
\ definition lines. File visits and result packets still describe each check.
\
\ deferred: nothing is refused, but a stretch of top-level source or a
\ definition was deferred to the run, and a W-CHECK-DEFERRED packet locates each
\ (src/habu/verify-source.f TOP-TOKEN, RENDER-GAP, REPORT-DEFERRED): those
\ tokens are not verified.
\ held is answered before anything is verified: this image holds SUBJECT though
\ the engine does not provide it (the verifier's source, this file and
\ tools/check-verify-child-lib.f; in the saved image,
\ tools/check-verify-save.f), so it cannot be verified here. The parent
\ answers engine-provided itself. stopped is a throw that ended the
\ composition, whatever was refused before it.
\
\ The second form is check.f's pre-pass. The first definition the checker
\ refuses stops the composition, as it stops the load, and the subject's packets
\ name it LABEL. SUBJECT is check.f's own copy of the text, which nothing holds.
\ It answers
\
\    check-verify: verified | stopped RC BYTE DUP-AT DUP-LEN IN-SUBJECT FILE
\
\ RC is the code the composition stopped with, 70 for a definition or top-level
\ token the checker refused by the throw that rendered its packet, BYTE where
\ the token the verifier read last starts, DUP-AT and DUP-LEN where the name it
\ refused as a duplicate starts and its length, DUP-LEN 0 when it kept no name
\ (VERIFY:DUPLICATE), IN-SUBJECT 1 when the stop is in BYTES and 0 when it is
\ in a file they load, and FILE, the rest of the line, the name of the file it
\ is in: for BYTES, SUBJECT in the first form and LABEL in the second.
\
\ stderr is prose: for the first form, a line for each file whose verification
\ a throw other than a loader's fault stopped, and whatever else the engine
\ writes there, a `die`'s message among it. A child that ends any other way, the verifier's own `die` included,
\ writes no result line.

\ The child runs in the client's working directory, which can contain a
\ different Habu revision. Its body belongs to this entry's tree.
package CHECK-VERIFY-LOAD
create ROOT PATH-CAP allot   variable ROOT-U
create OLD PATH-CAP allot    variable OLD-U

: SAVE ( -- )
   SOURCE-ROOT:ENGINE$ {: a:ptr u:n :}
   a OLD u BYTE-COPY  u OLD-U !
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop
   SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME {: root:ptr rootu:n :}
   root ROOT rootu BYTE-COPY  rootu ROOT-U ! ;

: LOAD ( -- ) s" tools/check-verify-child-lib.f" required ;
: LOAD-ROOT ( -- ) ROOT ROOT-U @ [: LOAD ;] SOURCE-ROOT:WITH ;

public
: START ( -- )
   SAVE
   ROOT ROOT-U @ SOURCE-ROOT:ENGINE!
   [: LOAD-ROOT ;] catch {: rc:n :}
   OLD OLD-U @ SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;
;package

CHECK-VERIFY-LOAD:START

CHECK-VERIFY-CHILD:MAIN
