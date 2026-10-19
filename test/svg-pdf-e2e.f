\ SVG bytes -> native librsvg/Cairo -> staged PDF -> independent PDF readers.
\ Run: bin/hb --load test/svg-pdf-e2e.f
require src/lib/svg-pdf.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/ffi-abi.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/string.f
require src/lib/f64-text.f
require src/lib/le.f
require src/lib/fmt.f
require src/lib/span.f

package SVG-PDF-TEST
private

$10000 constant CAP
CAP BUFFER: OUTPUT
CAP BUFFER: ERRORS
CAP BUFFER: PDF
FS-PATH-CAP BUFFER: EXE
FS-PATH-CAP BUFFER: ROOT
FS-PATH-CAP BUFFER: PATH
variable ROOT-U
variable PDF-U
variable FILES
CAP SPAN-BUFFER: FIRST
CAP SPAN-BUFFER: SECOND
DYNAMIC-BUFFER PAGES SPAN:span<u8>

\ A soft file-size limit makes the real PDF sink fail during finalization.
\ These POSIX resources belong to this test process and are restored even
\ when an assertion throws. No renderer hook or replacement C function exists.
16 BUFFER: OLD-LIMIT
16 BUFFER: NEW-LIMIT
variable OLD-SIGNAL
variable FINISH-LIMIT
1 constant FILE-SIZE-LIMIT
25 constant FILE-SIZE-SIGNAL

PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ptr u8 -- i32 )
   1 16 WRITES-BYTES
;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: SIGNAL-CALL signal ( n n -- n ) ;FUNCTION

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr size:n :}
   ROOT$ name size PATH JOIN-PATH PATH swap ;
: DEST$ ( -- ptr u8 n ) s" pages.pdf" AT ;
: FIRST-QDF$ ( -- ptr u8 n ) s" first-qdf.pdf" AT ;
: SECOND-QDF$ ( -- ptr u8 n ) s" second-qdf.pdf" AT ;
: THIRD-QDF$ ( -- ptr u8 n ) s" third-qdf.pdf" AT ;
: SINGLE$ ( -- ptr u8 n ) s" fractional.pdf" AT ;
: SINGLE-QDF$ ( -- ptr u8 n ) s" fractional-qdf.pdf" AT ;
: PHYSICAL$ ( -- ptr u8 n ) s" physical.pdf" AT ;
: PHYSICAL-QDF$ ( -- ptr u8 n ) s" physical-qdf.pdf" AT ;
: MAX-PAGE$ ( -- ptr u8 n ) s" max-page.pdf" AT ;
: MAX-PAGE-QDF$ ( -- ptr u8 n ) s" max-page-qdf.pdf" AT ;

: SVG$ ( -- ptr u8 n )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='96.375' height='48.625' overflow='visible'><path d='M1.125 2.375L95.25 47.5M-10 -10L110 60' fill='none' stroke='black' stroke-width='.25'/></svg>" ;

: SECOND-SVG$ ( -- ptr u8 n )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='144.5' height='72.25'><path d='M8.5 4.25L140.125 70.75' fill='none' stroke='red' stroke-width='.5'/></svg>" ;
: SECOND! ( ptr u8 n -- )
   {: data:ptr size:n :}
   data size SECOND SPAN:COPY SECOND size SPAN:TAKE 1 PAGES ! ;
: WRITE-PAGES ( -- ) DEST$ 0 PAGES 2 SVG-PDF:WRITE-PAGES ;
: WRITE-VALID ( -- ) SECOND-SVG$ SECOND! WRITE-PAGES ;
: WRITE-SINGLE ( -- ) SINGLE$ SVG$ SVG-PDF:WRITE ;
: EMPTY ( -- ) DEST$ 0 PAGES 0 SVG-PDF:WRITE-PAGES ;
: NEGATIVE ( -- ) DEST$ 0 PAGES -1 SVG-PDF:WRITE-PAGES ;
: MALFORMED ( -- ) s" <svg>" SECOND! WRITE-PAGES ;
: UNRESOLVED ( -- )
   s" <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 96 48'/>" SECOND! WRITE-PAGES ;
: ZERO-SIZE ( -- )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='0' height='48'/>" SECOND! WRITE-PAGES ;
: NONFINITE-SIZE ( -- )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='1e300' height='48'><path d='M0 0L10 10' stroke='black'/></svg>" SECOND! WRITE-PAGES ;
: OVERSIZE-WIDTH ( -- )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='1e20' height='48'/>" SECOND! WRITE-PAGES ;
: OVERSIZE-HEIGHT ( -- )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='96' height='19201'/>" SECOND! WRITE-PAGES ;
: BAD-RENDER ( -- )
   \ Twenty levels of doubled uses exceed librsvg's real instancing limit.
   SB-RESET
   s" <svg xmlns='http://www.w3.org/2000/svg' width='96' height='48'><defs><g id='a'><rect width='1' height='1'/></g>" SB-APPEND
   20 1 do
      s" <g id='" SB-APPEND i 97 + SB-APPEND-C s" '>" SB-APPEND
      2 0 do s" <use href='#" SB-APPEND j 96 + SB-APPEND-C s" '/>" SB-APPEND loop
      s" </g>" SB-APPEND
   loop
   s" </defs><use href='#t'/></svg>" SB-APPEND
   SB$ SECOND! WRITE-PAGES ;
: RESTORE-DIR ( -- ) ROOT$ 448 CHMOD-MODE ;
: BAD-DEST ( -- )
   ROOT$ 365 CHMOD-MODE
   [: WRITE-VALID ;] [: RESTORE-DIR ;] finally ;
: BAD-RENAME ( -- )
   s" directory" AT 0 PAGES 2 SVG-PDF:WRITE-PAGES ;

: PDF-READ ( ptr u8 n -- ptr u8 n )
   PDF CAP READ-ALL dup PDF-U ! PDF swap ;

: LIMIT-ON ( -- )
   FILE-SIZE-LIMIT OLD-LIMIT GET-LIMIT 0 T=
   OLD-LIMIT NEW-LIMIT 16 BYTE-COPY
   FINISH-LIMIT @ NEW-LIMIT LE:U64!
   FILE-SIZE-SIGNAL 1 SIGNAL-CALL OLD-SIGNAL !
   FILE-SIZE-LIMIT NEW-LIMIT SET-LIMIT 0 T= ;
: LIMIT-OFF ( -- )
   FILE-SIZE-LIMIT OLD-LIMIT SET-LIMIT 0 T=
   FILE-SIZE-SIGNAL OLD-SIGNAL @ SIGNAL-CALL -1 T<> ;
: FINISH-FAILURE ( -- )
   LIMIT-ON
   [: WRITE-VALID ;] [: LIMIT-OFF ;] finally ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: COMMAND ( ptr u8 n -- ptr u8 n )
   {: name:ptr size:n :}
   name size >LEN EXE RESOLVE-EXECUTABLE {: exeu:len :}
   EXE exeu s" " >LEN OUTPUT CAP >LEN ERRORS CAP >LEN 30000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len code:n :}
   code 0<> if OUTPUT outu LEN>N type ERRORS erru LEN>N type then
   code 0 T=
   OUTPUT outu LEN>N ;
: RESET-COMMAND ( -- )
   PROC-ARGV-ENV-RESET
   s" LC_ALL" >LEN s" C" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;
: RETRY ( -- )
   WRITE-VALID
   \ All page data fits; the final byte of a complete PDF does not. This
   \ exercises the real finalization sink after earlier pages can be emitted.
   DEST$ PDF-READ nip 1- FINISH-LIMIT !
   RESET-COMMAND s" --check" ARG DEST$ ARG s" qpdf" COMMAND 2drop ;
: COUNT-FILE ( ptr u8 n -- ) 2drop 1 FILES +! ;
: FILES# ( -- n )
   0 FILES ! ROOT$ [: COUNT-FILE ;] WALK-FILES FILES @ ;
: PRESERVED ( [ -- ] n -- )
   {: run code:n :}
   DEST$ s" previous destination" WRITE-ALL
   FILES# {: before:n :}
   run code TTHROWSQ
   DEST$ PDF-READ s" previous destination" T$=
   FILES# before T=
   RETRY ;

: AFTER ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: data:ptr size:n key:ptr keyu:n :}
   data size key keyu FIND-SUB MATCH option
      none OF s" PDF field missing" 1 die ENDOF
      some OF IDX>N keyu + {: off:n :} data off + size off - ENDOF
   ;MATCH ;
: SPACE? ( n -- bool ) dup 32 <= swap 0 >= and ;
: SKIP-SPACE ( ptr u8 n -- ptr u8 n )
   {: text:ptr size:n :}
   0 begin dup size < while
      text over + c@ SPACE? 0= if text over + swap size swap - exit then
      1+
   repeat text swap size swap - ;
: TOKEN-SIZE ( ptr u8 n -- n )
   {: text:ptr size:n :}
   0 begin dup size < while
      text over + c@ SPACE? if exit then 1+
   repeat ;
: NUMBER ( ptr u8 n -- ptr u8 n r )
   SKIP-SPACE {: text:ptr size:n :}
   text size TOKEN-SIZE {: digits:n :}
   text digits F64-TEXT:PARSE MATCH result
      ok OF {: value:r :} text digits + size digits - value ENDOF
      err OF drop s" PDF number malformed" 1 die ENDOF
   ;MATCH ;
: NEAR ( r r r -- )
   {: actual:r expected:r epsilon:r :}
   actual expected f- fabs epsilon f<= TTRUE ;
: PAGE-SIZE ( ptr u8 n r r -- )
   {: data:ptr size:n width:r height:r :}
   data size s" /MediaBox [" AFTER
   NUMBER 0.0 0.0 NEAR NUMBER 0.0 0.0 NEAR
   NUMBER width 0.000001 NEAR NUMBER height 0.000001 NEAR 2drop ;

: LINE-AT ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: data:ptr size:n key:ptr keyu:n :}
   data size key keyu FIND-SUB MATCH option
      none OF s" PDF path missing" 1 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH
   begin dup 0 > while
      data over 1- + c@ 10 = if data over + swap size swap - exit then
      1-
   repeat data swap size swap - ;
: DRAWING ( r r r r r -- )
   {: height:r x1:r y1:r x2:r y2:r :}
   PDF PDF-U @ s\" \nstream\n" AFTER
   NUMBER 1.0 0.0 NEAR NUMBER 0.0 0.0 NEAR NUMBER 0.0 0.0 NEAR
   NUMBER -1.0 0.0 NEAR NUMBER 0.0 0.0 NEAR NUMBER height 0.000001 NEAR
   2drop
   \ Cairo's PDF path coordinates are quantized; allow 0.003 pt, not a pixel.
   PDF PDF-U @ s"  m " LINE-AT
   NUMBER x1 0.003 NEAR NUMBER y1 0.003 NEAR
   s" m " AFTER NUMBER x2 0.003 NEAR NUMBER y2 0.003 NEAR 2drop ;

: RENAME-FAILURE ( -- )
   s" directory/keep" AT s" existing contents" WRITE-ALL
   FILES# {: before:n :}
   [: BAD-RENAME ;] E-FS-IO TTHROWSQ
   s" directory" AT DIR? TTRUE
   s" directory/keep" AT PDF-READ s" existing contents" T$=
   FILES# before T=
   RETRY ;

: VECTOR-ONLY ( ptr u8 n -- )
   {: path:ptr size:n :}
   RESET-COMMAND s" -list" ARG path size ARG
   s" pdfimages" COMMAND s\" \n" AFTER s\" \n" AFTER nip 0 T= ;
: PAGE-QDF ( ptr u8 n [ -- ptr u8 n ] -- )
   {: page:ptr pageu:n output :}
   RESET-COMMAND s" --qdf" ARG s" --object-streams=disable" ARG
   s" --stream-data=uncompress" ARG DEST$ ARG
   s" --pages" ARG s" ." ARG page pageu ARG s" --" ARG output execute ARG
   s" qpdf" COMMAND 2drop ;
: INSPECT-PAGES ( -- )
   RESET-COMMAND s" -box" ARG DEST$ ARG
   s" pdfinfo" COMMAND s" Pages:           2" CONTAINS? TTRUE
   DEST$ VECTOR-ONLY
   s" 1" [: FIRST-QDF$ ;] PAGE-QDF
   FIRST-QDF$ PDF-READ 72.28125 36.46875 PAGE-SIZE
   36.46875 0.84375 1.78125 71.4375 35.625 DRAWING
   s" 2" [: SECOND-QDF$ ;] PAGE-QDF
   SECOND-QDF$ PDF-READ 108.375 54.1875 PAGE-SIZE
   54.1875 6.375 3.1875 105.09375 53.0625 DRAWING ;
: INSPECT-EMPTY-TAIL ( -- )
   s" <svg xmlns='http://www.w3.org/2000/svg' width='228.75' height='144.5'/>"
      SPAN:MAKE 2 PAGES !
   DEST$ 0 PAGES 3 SVG-PDF:WRITE-PAGES
   RESET-COMMAND s" -box" ARG DEST$ ARG
   s" pdfinfo" COMMAND s" Pages:           3" CONTAINS? TTRUE
   s" 3" [: THIRD-QDF$ ;] PAGE-QDF
   THIRD-QDF$ PDF-READ 171.5625 108.375 PAGE-SIZE ;
: INSPECT-SINGLE ( -- )
   WRITE-SINGLE
   RESET-COMMAND s" -box" ARG SINGLE$ ARG
   s" pdfinfo" COMMAND s" Pages:           1" CONTAINS? TTRUE
   SINGLE$ VECTOR-ONLY
   RESET-COMMAND s" --qdf" ARG s" --object-streams=disable" ARG
   s" --stream-data=uncompress" ARG SINGLE$ ARG SINGLE-QDF$ ARG
   s" qpdf" COMMAND 2drop
   SINGLE-QDF$ PDF-READ 72.28125 36.46875 PAGE-SIZE
   36.46875 0.84375 1.78125 71.4375 35.625 DRAWING ;

: RUN ( -- )
   T-RESET
   s" build" MAKE-DIRS
   s" build" s" svg-pdf-e2e" MAKE-TEMP-DIR {: root:ptr size:n :}
   root ROOT size BYTE-COPY size ROOT-U !
   3 PAGES-RESERVE
   SVG$ {: data:ptr datau:n :}
   data datau FIRST SPAN:COPY FIRST datau SPAN:TAKE 0 PAGES !
   s" directory" AT MAKE-DIRS
   s" parser, dimensions, render, destination and finish failures preserve output" T-LABEL
   [: EMPTY ;] SVG-PDF:E-OPEN PRESERVED
   [: NEGATIVE ;] SVG-PDF:E-OPEN PRESERVED
   [: MALFORMED ;] SVG-PDF:E-OPEN PRESERVED
   [: UNRESOLVED ;] SVG-PDF:E-SIZE PRESERVED
   [: ZERO-SIZE ;] SVG-PDF:E-SIZE PRESERVED
   [: NONFINITE-SIZE ;] SVG-PDF:E-SIZE PRESERVED
   [: OVERSIZE-WIDTH ;] SVG-PDF:E-SIZE PRESERVED
   [: OVERSIZE-HEIGHT ;] SVG-PDF:E-SIZE PRESERVED
   [: BAD-RENDER ;] SVG-PDF:E-RENDER PRESERVED
   [: BAD-DEST ;] E-FS-OPEN PRESERVED
   RENAME-FAILURE
   [: FINISH-FAILURE ;] SVG-PDF:E-FINISH PRESERVED
   s" ordered pages retain independent dimensions and vectors" T-LABEL
   INSPECT-PAGES
   s" an empty final page retains its place and dimensions" T-LABEL
   INSPECT-EMPTY-TAIL
   s" single fractional page and vector geometry" T-LABEL
   INSPECT-SINGLE
   PHYSICAL$ s" <svg xmlns='http://www.w3.org/2000/svg' width='25.4mm' height='12.7mm'/>" SVG-PDF:WRITE
   RESET-COMMAND s" --qdf" ARG s" --object-streams=disable" ARG
   PHYSICAL$ ARG PHYSICAL-QDF$ ARG s" qpdf" COMMAND 2drop
   PHYSICAL-QDF$ PDF-READ 72.0 36.0 PAGE-SIZE
   s" default-unit page limit" T-LABEL
   MAX-PAGE$ s" <svg xmlns='http://www.w3.org/2000/svg' width='200in' height='200in'/>" SVG-PDF:WRITE
   RESET-COMMAND s" --qdf" ARG s" --object-streams=disable" ARG
   MAX-PAGE$ ARG MAX-PAGE-QDF$ ARG s" qpdf" COMMAND 2drop
   MAX-PAGE-QDF$ PDF-READ 14400.0 14400.0 PAGE-SIZE
   PAGES-RELEASE
   s" svg-pdf artifacts: " type ROOT$ type cr
   T-REPORT ;

RUN
;package
