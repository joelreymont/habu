\ codegen.f - a shared bounded byte buffer for building generated Forth source.
\
\ Several definers in the tree build a "( ... ) ;" / "TRUSTED: ... ;" text and
\ hand it to an audited `evaluate`: EXTENT:/TENSOR:/SPEC: in maki (the XG-* buffer)
\ and NOMINAL: in lib/type/value-nominal.f (the NG-* buffer). Each grew its own
\ near-identical private byte-append buffer (reset / append-byte / append-string /
\ append-decimal / contents) plus a capacity guard. Package CODEGEN factors that
\ mechanism out once so each definer keeps only its own audited `evaluate` wrapper
\ (the trust boundary stays per package) and shares the untrusted text-building.
\
\ A buffer is a descriptor minted by the `CODEGEN:BUFFER` defining word. The
\ descriptor word pushes the address of a fixed header laid out as four cells
\ followed by the raw byte storage:
\
\     [ cap ][ cap-err ][ val-err ][ len ][ ...cap raw bytes... ]
\
\ `cap` is the byte capacity, `cap-err` is the throw code raised when an append
\ would overflow, `val-err` is the throw code raised when APPEND-DECIMAL is handed
\ a negative value, and `len` is the current content length. The header is ordinary
\ dictionary storage, so nothing here is trusted: cells are read with `@`, and the
\ byte storage is read/written through the core `BYTE-VIEW` byte pointer.
\
\ The definer has no trailing colon (unlike EXTENT:/NOMINAL:) because a package word
\ ending in `:` cannot be resolved through a `CODEGEN:` qualifier - the trailing colon
\ is read as a name edge, not a package separator. It reads like `create`/`variable`:
\ `256 CODEGEN:BUFFER NAME` defines NAME as the descriptor word.
\
\ CAPACITY ERROR CODES. The module owns one named capacity error, E-CG-CAP, and one
\ named decimal-value error, E-CG-VALUE; the plain `CODEGEN:BUFFER` definer stamps
\ those into a new buffer. `CODEGEN:BUFFER-E` takes explicit `cap-err`/`val-err`
\ codes instead, so a package migrating an existing hand-rolled buffer onto this
\ module preserves the exact throw code its callers already see (maki keeps
\ E-EXT-CAP / E-EXT-NAME / E-EXT-VALUE; value-nominal keeps E-VNOM-CAP) without a
\ catch-and-remap wrapper. New codegen buffers should use the plain definer.

require lib/prelude.f

\ Codegen-buffer throw codes. lib/errors.f owns the canonical stdlib blocks up to
\ -4499; the -4700..-4799 block is unclaimed there, so - like test/perf-verdict.f's
\ -4500 block - it is declared at the owning module. error-code-lint enforces global
\ uniqueness across every source tree, so these stay distinct from every other E-*.
-4700 constant E-CG-CAP     \ an append would exceed a codegen buffer's capacity
-4701 constant E-CG-VALUE   \ APPEND-DECIMAL was handed a negative value

package CODEGEN

private

\ header cell layout: [cap][cap-err][val-err][len] then cap raw bytes.
: CB-CAP@  ( ptr a -- n )  @ ;
: CB-CERR@ ( ptr a -- n )  cell+ @ ;
: CB-VERR@ ( ptr a -- n )  2 cells + @ ;
: CB-LEN@  ( ptr a -- n )  3 cells + @ ;
: CB-LEN!  ( n ptr a -- )  3 cells + ! ;
: CB-DATA  ( ptr a -- ptr u8 )  4 cells + BYTE-VIEW ;   \ byte storage past the header

\ Compile the four header cells and leave cap for the trailing `allot`. A `{: :}`
\ locals group inside a `create ... does>` definer body wedges the definer, so the
\ three inputs are shuffled on the stack (cap kept underneath for allot) rather than
\ bound to locals.
: CB-HEADER, ( n n n -- n )   \ cap cap-err val-err -- cap
   >r >r dup , r> , r> , 0 , ;

public

\ Mint a codegen buffer with explicit per-buffer error codes. Defines NAME as a
\ word pushing the buffer descriptor. Use this only to preserve a legacy caller's
\ existing throw code while migrating an existing buffer onto this module.
: BUFFER-E ( n n n -- )   \ cap cap-err val-err --
   create CB-HEADER, allot  does> ( -- ptr a ) ;

\ Mint a codegen buffer using the module's own named error codes (E-CG-CAP on an
\ overflowing append, E-CG-VALUE on a negative decimal). The form new code should use.
: BUFFER ( n -- )   \ cap --
   E-CG-CAP E-CG-VALUE BUFFER-E ;

\ Discard a buffer's contents, keeping its capacity.
: RESET ( ptr a -- )  0 swap CB-LEN! ;

\ Append one byte; overflow throws the buffer's capacity error code.
: APPEND-BYTE ( n ptr a -- ) {: c:n d:ptr :}
   d CB-LEN@ 1 + d CB-CAP@ > if d CB-CERR@ throw then
   c  d CB-DATA d CB-LEN@ +  c!
   d CB-LEN@ 1 + d CB-LEN! ;

\ Append a counted byte string.
: APPEND-STRING ( ptr u8 n ptr a -- ) {: a:ptr u:n d:ptr :}
   0 begin dup u < while  dup a + c@ d APPEND-BYTE  1 +  repeat drop ;

\ Append a non-negative decimal; a negative value throws the buffer's value error
\ code (fail closed - never emit garbage digits for a negative).
: APPEND-DECIMAL ( n ptr a -- ) {: v:n d:ptr :}
   v 0 < if d CB-VERR@ throw then
   v 10 >= if v 10 / d recurse then  v 10 mod [char] 0 + d APPEND-BYTE ;

\ The buffer's current contents as a counted byte string.
: CONTENTS ( ptr a -- ptr u8 n ) {: d:ptr :}  d CB-DATA d CB-LEN@ ;

;package
