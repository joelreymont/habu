\ string-roles.f - the string words over CAD-NUM's numeric roles.
\
\ WHY IT IS ITS OWN FILE. The engine bakes lib/string.f, because the compiler,
\ the JIT and the REPL use the raw byte-string words. They do not use the ROLES,
\ and while package STR sat in lib/string.f its `require` of
\ lib/cad-num-arithmetic.f put CAD-NUM's types and its whole arithmetic table
\ into every image for callers that are all in lib/, tools/ and test/ - eighteen
\ files, none of them under src/compiler or src/habu (dot
\ habu-bake-only-what-4039004a: only what the compiler, JIT and REPL require is
\ baked). A file of its own is what keeps it out of the image: lib/string.f keeps
\ exactly the raw words it had, and a caller that wants STR: requires this.
\ Every word below is the one that was in lib/string.f, with its effect, its
\ checks and its comments unchanged.

require lib/string.f
require lib/cad-num-arithmetic.f

\ ---- typed STR surface over CAD-NUM roles -------------------------------------
\
\ The raw string words in lib/string.f conflate byte lengths, byte offsets, and logical
\ counts on one interchangeable `n`. Package STR re-states the same helpers over
\ CAD-NUM roles: a string byte length is a `CAD-NUM:byte-len`, a scan offset is a
\ `CAD-NUM:byte-off`, a character count is a `CAD-NUM:item-count`, and a found
\ position is a `CAD-NUM:index` carried by the shared cell-polymorphic option
\ (option<CAD-NUM:index>). A length/offset swap, an index/offset swap, or a raw
\ `n` where a role is required is a checker reject. Positive behavior is
\ byte-identical to the raw words - same scan, same E-STR-BOUNDS/E-STR-CAPACITY
\ throws, same empty-string/empty-needle zeros and not-found NONE - so the raw
\ words stay untouched for their not-yet-migrated fleet callers; this wave moves
\ only owner-internal callers (none survive as private) and lib/string-test.f.
\
\ STR owns exactly two audited representation projections (BYTE-LEN>N, BYTE-OFF>N),
\ mirroring VEC's ITEM-COUNT>N/INDEX>N and MEM's ALLOC-*>N: the raw scan words and
\ their byte pointers still consume a bare `n`, so the typed boundary reads a
\ validated role's cell to drive them. Private, no public export, confined to
\ lib/string.f; retire when TVK-RAW (habu-nominal-storage-raw-a3430ef2) lets the
\ byte-scan primitives take the nominal role directly. The typed layer wraps the
\ raw words as black boxes (RAW-* aliases captured before the package words shadow
\ their global names) and only STR:SPLIT-NEXT adds a check the raw word cannot
\ express: the largest field boundary it can report is one past the string end, so
\ the offset advance (end + 1) is proven through the B5.2 CAD-NUM:ADVANCE-BYTE-OFF
\ BEFORE the scan, turning an offset-space overflow into an E-STR-BOUNDS throw
\ instead of a silent wrap.
\ Retirement owner: habu-epic-model-cad-70b629a9.

package STR
private

\ ---- representation projections (STR's only retypes, and both are checked) -----
CAST: BYTE-LEN>N ( CAD-NUM:byte-len -- n )
CAST: BYTE-OFF>N ( CAD-NUM:byte-off -- n )

\ ---- raw-word aliases: bind the legacy globals before the package words shadow --
\ their names. Defined here (before the public STR:<name> exist) so a bare
\ FIND-SUB / INDEX-OF / SPLIT-NEXT / BUF-* resolves to the global raw word.
: RAW-FIND-SUB ( ptr u8 n ptr u8 n -- option<idx> )    FIND-SUB ;
: RAW-INDEX-OF ( ptr u8 n n -- option<idx> )           INDEX-OF ;
: RAW-SPLIT-NEXT ( ptr u8 n n n -- ptr u8 n n bool )   SPLIT-NEXT ;
: RAW-BUF-RESET ( ptr len -- )                         BUF-RESET ;
: RAW-BUF-LEN@ ( ptr len -- n )                        BUF-LEN@ ;
: RAW-BUF-APPEND ( ptr u8 n ptr u8 n ptr len -- )      BUF-APPEND ;
: RAW-BUF-APPEND-C ( n ptr u8 n ptr len -- )           BUF-APPEND-C ;

\ ---- ok extractors: a validated/derived raw cell that fails a role predicate is -
\ a string bounds violation. The reachable arm is the caller-facing negative for
\ LENGTH/OFFSET/COUNT (byte-identical to the raw STR-LEN/STR-OFF/STR-COUNT
\ E-STR-BOUNDS) and the SPLIT-NEXT offset-advance overflow; the rest are
\ unreachable invariants over cells the scan proves nonnegative.
: OK-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-BYTE-OFF ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- CAD-NUM:byte-off )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-ITEM-COUNT ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- CAD-NUM:item-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-INDEX ( CAD-NUM:numeric-result<CAD-NUM:index> -- CAD-NUM:index )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- raw cell -> validated role bridges ---------------------------------------
: N>BYTE-LEN ( n -- CAD-NUM:byte-len )       CAD-NUM:BYTE-LEN OK-BYTE-LEN ;
: N>BYTE-OFF ( n -- CAD-NUM:byte-off )       CAD-NUM:BYTE-OFF OK-BYTE-OFF ;
: N>ITEM-COUNT ( n -- CAD-NUM:item-count )   CAD-NUM:ITEM-COUNT OK-ITEM-COUNT ;
: N>INDEX ( n -- CAD-NUM:index )             CAD-NUM:INDEX OK-INDEX ;

\ ---- option<idx> (raw find result) -> option<CAD-NUM:index> --------------------
: IDX-OPT>INDEX-OPT ( option<idx> -- option<CAD-NUM:index> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF IDX>N N>INDEX OPTION:SOME ENDOF
   ;MATCH ;

\ ---- one-past-end offset advance guard (B5.2 offset algebra) -------------------
: ONE-BYTE-LEN ( -- CAD-NUM:byte-len )   1 CAD-NUM:BYTE-LEN OK-BYTE-LEN ;
: SPLIT-SENTINEL-OK ( CAD-NUM:byte-len -- )
   \ the raw scan's largest reported boundary is (length + 1); prove that offset
   \ advance is representable (overflow -> E-STR-BOUNDS) before scanning.
   BYTE-LEN>N N>BYTE-OFF  ONE-BYTE-LEN  CAD-NUM:ADVANCE-BYTE-OFF  OK-BYTE-OFF drop ;

public

\ ---- role validators (raw n -> role; negative throws E-STR-BOUNDS) -------------
: LENGTH ( n -- CAD-NUM:byte-len )      N>BYTE-LEN ;
: OFFSET ( n -- CAD-NUM:byte-off )      N>BYTE-OFF ;
: COUNT ( n -- CAD-NUM:item-count )     N>ITEM-COUNT ;

\ ---- substring / byte search: found position as option<CAD-NUM:index> ----------
: FIND-SUB ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len -- option<CAD-NUM:index> )
   {: a:ptr u:CAD-NUM:byte-len b:ptr v:CAD-NUM:byte-len :}
   a u BYTE-LEN>N b v BYTE-LEN>N RAW-FIND-SUB IDX-OPT>INDEX-OPT ;
: INDEX-OF ( ptr u8 CAD-NUM:byte-len n -- option<CAD-NUM:index> )
   {: a:ptr u:CAD-NUM:byte-len c:n :}
   a u BYTE-LEN>N c RAW-INDEX-OF IDX-OPT>INDEX-OPT ;

\ ---- next separator-delimited field (offset advance proven, then raw scan) ------
\ A split step yields the next separator-delimited field slice plus the advance
\ cursor (the offset one past the separator, or one past the string end on the
\ final field). STR:SPLIT-NEXT returns option<str:split>: SOME(slice,next) while
\ a field remains, NONE once the start cursor is past the string end - the checked
\ replacement for the former (ptr u8 byte-len byte-off bool) value+flag return
\ (switchover wave B), so a caller cannot read the slice or cursor on the
\ no-field path.
STRUCTURE split 0
  FIELD ptr ptr u8
  FIELD len CAD-NUM:byte-len
  FIELD next CAD-NUM:byte-off
;STRUCTURE

: SPLIT-NEXT ( ptr u8 CAD-NUM:byte-len n CAD-NUM:byte-off -- option<str:split> )   \ SOME next field slice + advance cursor, else NONE
   {: a:ptr u:CAD-NUM:byte-len sep:n start:CAD-NUM:byte-off :}
   u SPLIT-SENTINEL-OK
   a u BYTE-LEN>N sep start BYTE-OFF>N RAW-SPLIT-NEXT   \ ( sub-ptr sub-rawlen raw-next more? )
   {: rn:n more?:bool :}                                \ sub-ptr sub-rawlen remain on the stack
   more? if
      N>BYTE-LEN  rn N>BYTE-OFF  STR-SPLIT:MAKE OPTION:SOME
   else
      2drop OPTION:NONE
   then ;

\ ---- caller-owned bounded byte buffer (length cell as byte-len role) ------------
: BUF-RESET ( ptr len -- )                    RAW-BUF-RESET ;
: BUF-LEN@ ( ptr len -- CAD-NUM:byte-len )    RAW-BUF-LEN@ N>BYTE-LEN ;
: BUF-APPEND ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len ptr len -- )
   {: src:ptr srclen:CAD-NUM:byte-len dst:ptr cap:CAD-NUM:byte-len lenp:ptr :}
   src srclen BYTE-LEN>N dst cap BYTE-LEN>N lenp RAW-BUF-APPEND ;
: BUF-APPEND-C ( n ptr u8 CAD-NUM:byte-len ptr len -- )
   {: c:n dst:ptr cap:CAD-NUM:byte-len lenp:ptr :}
   c dst cap BYTE-LEN>N lenp RAW-BUF-APPEND-C ;

;package
