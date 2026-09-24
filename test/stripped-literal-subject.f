\ Derive pointer-shaped text from this builder's own code, on every host.
require lib/codegen.f

package STRIPPED-LITERAL

4096 CODEGEN:BUFFER SOURCE
create LIT-CELL 0 ,
public
: ANCHOR ( -- ) ;
private

: TEXT+ ( ptr u8 n -- ) SOURCE CODEGEN:APPEND-STRING ;
: BYTE+ ( n -- ) SOURCE CODEGEN:APPEND-BYTE ;
: DEC+ ( n -- ) SOURCE CODEGEN:APPEND-DECIMAL ;
: HEX+ ( n -- ) $F and s" 0123456789abcdef" drop + c@ BYTE+ ;
: ESCAPE+ ( n -- )
   S\" \\x" TEXT+ dup 4 rshift HEX+ HEX+ ;

public
\ The untyped top-level fixture passes ' ANCHOR as raw bits deliberately.
: RAW ( n -- ) LIT-CELL ! ;
: VALUE ( -- n ) LIT-CELL @ ;

\ Repeat the pointer at all eight alignments. Exactly one block puts it in
\ an aligned DATA cell regardless of where the literal pool places the body.
\ Expected bytes are separate small immediates, so the image verifies that
\ literal storage survives capture without relocating its pointer-shaped text.
: BODY-SOURCE ( n -- ptr u8 n )
   {: value:n :}
   SOURCE CODEGEN:RESET
   S\" : LIT$ ( -- ptr u8 n ) S\\\q " TEXT+
   8 0 ?do
      i 0 ?do 0 ESCAPE+ loop
      8 0 ?do value i 8 * rshift $FF and ESCAPE+ loop
      8 i - 0 ?do 0 ESCAPE+ loop
   loop
   S\" \q ;\n: MAIN ( -- ) LIT$ {: a:ptr u:n :} u 128 <> if 79 throw then\n" TEXT+
   8 0 ?do
      s" a " TEXT+ i DEC+ s"  + c@ " TEXT+
      value i 8 * rshift $FF and DEC+
      S\"  <> if 79 throw then\n" TEXT+
   loop
   S\" s\q literal\q type cr ;\n" TEXT+
   SOURCE CODEGEN:CONTENTS ;

;package
