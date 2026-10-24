\ codegen.f - a shared bounded byte buffer for building generated Forth source.
\
\ A definer that builds source text and hands it to an audited `evaluate`
\ (DEFTYPE in src/lib/type/deftype.f, task.f's SLOT-GEN, generator tools) uses
\ one of these buffers for the untrusted text-building and keeps only its own
\ `evaluate` wrapper, so the trust boundary stays per package.
\
\ A buffer is a descriptor minted by the `CODEGEN:BUFFER` defining word. The
\ descriptor word pushes the address of a fixed header: three cells, then a
\ dynamic control record (src/core/dynamic-storage.f: the head cell holding the
\ mapping and its CONTROL-CELLS count cells):
\
\     [ cap ][ cap-err ][ val-err ][ head ][ control ]
\
\ `cap` is the byte capacity, `cap-err` is the throw code raised when an append
\ would overflow and `val-err` is the throw code raised when APPEND-DECIMAL is
\ handed a negative value. The text is scratch that outlives no generation, so
\ it lives in the mapping, reserved at first use: the content length in its
\ first cell, then the cap content bytes. Image capture releases the mapping, so
\ a build that generated text leaves the same DATA as one that did not.
\
\ The definer has no trailing colon because a package word
\ ending in `:` cannot be resolved through a `CODEGEN:` qualifier - the trailing colon
\ is read as a name edge, not a package separator. It reads like `create`/`variable`:
\ `256 CODEGEN:BUFFER NAME` defines NAME as the descriptor word.
\
\ CAPACITY ERROR CODES. The module owns one named capacity error, E-CG-CAP, and one
\ named decimal-value error, E-CG-VALUE; the plain `CODEGEN:BUFFER` definer stamps
\ those into a new buffer. `CODEGEN:BUFFER-E` takes explicit `cap-err`/`val-err`
\ codes instead, so a package keeps the exact throw code its callers already
\ see (deftype.f keeps E-VNOM-CAP) without a catch-and-remap wrapper. New
\ codegen buffers should use the plain definer.
\
\ STORAGE CLASS. CALLER-OWNED: each CODEGEN:BUFFER's header is its definer's
\ dictionary storage and its text a dynamic mapping; the module keeps no state.

require src/lib/prelude.f
require src/core/dynamic-storage.f

\ Codegen-buffer throw codes. src/lib/errors.f owns the canonical stdlib blocks up to
\ -4499; the -4700..-4799 block is unclaimed there, so it is declared at the
\ owning module instead. error-code-lint enforces global uniqueness across every
\ source tree, so these stay distinct from every other E-*.
-4700 constant E-CG-CAP     \ an append would exceed a codegen buffer's capacity
-4701 constant E-CG-VALUE   \ APPEND-DECIMAL was handed a negative value

package CODEGEN

private

\ header cell layout: [cap][cap-err][val-err] then the control record.
3 cells constant CB-RECORD
CB-RECORD CELL / 1+ DYNAMIC-STORAGE:CONTROL-CELLS + constant CB-CELLS
: CB-CAP@  ( ptr n -- n )  @ ;
: CB-CERR@ ( ptr n -- n )  cell+ @ ;
: CB-VERR@ ( ptr n -- n )  2 cells + @ ;
CAST: CB-HEAD-CELL ( ptr n -- ptr ptr u8 )
: CB-HEAD  ( ptr n -- ptr ptr u8 )  CB-RECORD + CB-HEAD-CELL ;
: CB-CTL   ( ptr n -- ptr n )  CB-RECORD CELL + + ;

\ The mapping, reserved whole at first use: the length cell and cap bytes.
: CB-MAP ( ptr n -- ptr u8 ) {: d:ptr :}
   d CB-CTL @ 0= if
      CELL d CB-CAP@ + d CB-HEAD d CB-CTL 1 DYNAMIC-STORAGE:RESERVE
   then
   d CB-HEAD @ ;
: CB-LEN@  ( ptr n -- n )  CB-MAP CELL-VIEW @ ;
: CB-LEN!  ( n ptr n -- )  CB-MAP CELL-VIEW ! ;
: CB-DATA  ( ptr n -- ptr u8 )  CB-MAP CELL + ;   \ content bytes past the length

\ Compile the three header cells and the zeroed control record.
: CB-HEADER, ( n n n -- )   \ cap cap-err val-err --
   >r >r , r> , r> ,  CB-CELLS CB-RECORD CELL / - 0 ?do 0 , loop ;

public

\ Mint a codegen buffer with explicit per-buffer error codes. Defines NAME as a
\ word pushing the buffer descriptor. Use this only to preserve a legacy caller's
\ existing throw code while migrating an existing buffer onto this module.
: BUFFER-E ( n n n -- )   \ cap cap-err val-err --
   create CB-HEADER,  does> ( -- ptr n ) ;

\ Mint a codegen buffer using the module's own named error codes (E-CG-CAP on an
\ overflowing append, E-CG-VALUE on a negative decimal). The form new code should use.
: BUFFER ( n -- )   \ cap --
   E-CG-CAP E-CG-VALUE BUFFER-E ;

\ Discard a buffer's contents, keeping its capacity.
: RESET ( ptr n -- )  0 swap CB-LEN! ;

\ Append one byte; overflow throws the buffer's capacity error code.
: APPEND-BYTE ( n ptr n -- ) {: c:n d:ptr :}
   d CB-LEN@ 1 + d CB-CAP@ > if d CB-CERR@ throw then
   c  d CB-DATA d CB-LEN@ +  c!
   d CB-LEN@ 1 + d CB-LEN! ;

\ Append a counted byte string.
: APPEND-STRING ( ptr u8 n ptr n -- ) {: a:ptr u:n d:ptr :}
   0 begin dup u < while  dup a + c@ d APPEND-BYTE  1 +  repeat drop ;

\ Append a non-negative decimal; a negative value throws the buffer's value error
\ code (fail closed - never emit garbage digits for a negative).
: APPEND-DECIMAL ( n ptr n -- ) {: v:n d:ptr :}
   v 0 < if d CB-VERR@ throw then
   v 10 >= if v 10 / d recurse then  v 10 mod [char] 0 + d APPEND-BYTE ;

\ The buffer's current contents as a counted byte string.
: CONTENTS ( ptr n -- ptr u8 n ) {: d:ptr :}  d CB-DATA d CB-LEN@ ;

\ A descriptor is one cell span: the header and the control record. Its text
\ is a mapping capture releases, outside DATA.
: SCAN-SPANS ( ptr n [ ptr u8 n -- ] -- )
   {: d:ptr cells :}
   d BYTE-VIEW CB-CELLS CELL * cells execute ;

;package
