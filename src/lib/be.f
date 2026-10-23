\ be.f - fixed-width big-endian integers in a caller's bytes.
\
\ The counterpart of src/lib/le.f for contracts that store an integer most
\ significant byte first, such as a SolidWorks native container's key and
\ version. Reading a byte at a time keeps the width and the alignment the
\ contract's rather than the host's.
\
\ STORAGE CLASS. CALLER-OWNED: every word reads the caller's bytes and keeps
\ nothing of its own.

package BE
public

: U32@ ( ptr u8 -- n ) {: p :}
   p c@ $18 lshift  p 1 + c@ $10 lshift or  p 2 + c@ 8 lshift or  p 3 + c@ or ;

;package
