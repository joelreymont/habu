\ codegen.f - a shared bounded byte buffer for building generated Forth source.
\
\ A buffer is a descriptor minted by the `CODEGEN:BUFFER` defining word. The
\ descriptor word pushes the address of a fixed header laid out as two cells
\ followed by the raw byte storage:
\
\     [ cap ][ len ][ ...cap raw bytes... ]
\
\ `cap` is the byte capacity and `len` is the current content length. The header
\ is ordinary dictionary storage, so nothing here is trusted: cells are read with
\ `@`, and the byte storage is read/written through the core `BYTE-VIEW` byte
\ pointer.
\
\ The definer has no trailing colon (unlike EXTENT:) because a package word
\ ending in `:` cannot be resolved through a `CODEGEN:` qualifier - the trailing colon
\ is read as a name edge, not a package separator. It reads like `create`/`variable`:
\ `256 CODEGEN:BUFFER NAME` defines NAME as the descriptor word.

require lib/prelude.f

\ Codegen-buffer throw codes. lib/errors.f owns the canonical stdlib blocks up to
\ -4499; the -4700..-4799 block is unclaimed there, so it is declared at the
\ owning module instead. error-code-lint enforces global uniqueness across every
\ source tree, so these stay distinct from every other E-*.
-4700 constant E-CG-CAP     \ invalid buffer capacity or overflowing append
-4701 constant E-CG-VALUE   \ APPEND-DECIMAL was handed a negative value

package CODEGEN

private

\ header cell layout: [cap][len] then cap raw bytes.
: CB-CAP@ ( ptr n -- n )  @ ;
: CB-LEN@ ( ptr n -- n )  cell+ @ ;
: CB-LEN! ( n ptr n -- )  cell+ ! ;
: CB-DATA ( ptr n -- ptr u8 )  2 cells + BYTE-VIEW ;

public

\ Mint a codegen buffer. Defines NAME as a word pushing its descriptor.
: BUFFER ( n -- )   \ cap --
   dup 0 < if E-CG-CAP throw then
   create dup , 0 , allot  does> ( -- ptr n ) ;

\ Discard a buffer's contents, keeping its capacity.
: RESET ( ptr n -- )  0 swap CB-LEN! ;

\ Append one byte; overflow throws E-CG-CAP.
: APPEND-BYTE ( n ptr n -- ) {: c:n d:ptr :}
   d CB-LEN@ 1 + d CB-CAP@ > if E-CG-CAP throw then
   c  d CB-DATA d CB-LEN@ +  c!
   d CB-LEN@ 1 + d CB-LEN! ;

\ Append a counted byte string.
: APPEND-STRING ( ptr u8 n ptr n -- ) {: a:ptr u:n d:ptr :}
   0 begin dup u < while  dup a + c@ d APPEND-BYTE  1 +  repeat drop ;

\ Append a non-negative decimal; a negative value throws E-CG-VALUE.
: APPEND-DECIMAL ( n ptr n -- ) {: v:n d:ptr :}
   v 0 < if E-CG-VALUE throw then
   v 10 >= if v 10 / d recurse then  v 10 mod [char] 0 + d APPEND-BYTE ;

\ The buffer's current contents as a counted byte string.
: CONTENTS ( ptr n -- ptr u8 n ) {: d:ptr :}  d CB-DATA d CB-LEN@ ;

;package
