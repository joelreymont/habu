\ svg-pdf.f - completed in-memory SVGs become PDF pages through native
\ librsvg 2.52+/Cairo, in `package SVG-PDF`. Each SVG loads with no base URI:
\ data: resources resolve, external files do not, and librsvg may omit a missing
\ one silently. The destination is replaced by one FS:REPLACE-STAGED only after
\ every page rendered, so a failure leaves it intact. Test: test/svg-pdf-e2e.f.
\ STORAGE CLASS. TASK-LOCAL: foreign handles and the viewport live in STATE.
require src/lib/ffi-abi.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/ieee754.f
require src/lib/le.f
require src/lib/span.f

package SVG-PDF
public

-9490 constant E-SVG-PDF-FIRST
-9499 constant E-SVG-PDF-LAST
\ E-OPEN: malformed input or a nonpositive page count. E-SIZE: an unresolved,
\ nonfinite, nonpositive or oversized dimension. E-RENDER: a librsvg or Cairo
\ rendering failure. E-FINISH: page emission, finalization or output failure.
-9490 constant E-OPEN
-9491 constant E-SIZE
-9492 constant E-RENDER
-9493 constant E-FINISH

private

\ Pages are CSS px * 72/96 points (96 DPI), no rounding or crop; a side over
\ 14,400 pt (200 in, the PDF 1.7 Appendix C limit, no UserUnit) is E-SIZE before
\ that page draws, and both intrinsic dimensions must resolve to finite positive
\ CSS pixels (a percentage or a lone viewBox does not). Nothing is clamped.
14400 constant MAX-PAGE-PT

VERSIONED-LIBRARY rsvg-2 2
FUNCTION: SVG-OPEN rsvg_handle_new_from_data
   ( ptr u8 n ptr u8 -- ptr u8 )
   2 CELL WRITES-BYTES
;FUNCTION
FUNCTION: SVG-DPI rsvg_handle_set_dpi ( ptr u8 r -- ) ;FUNCTION
FUNCTION: SVG-SIZE rsvg_handle_get_intrinsic_size_in_pixels
   ( ptr u8 ptr u8 ptr u8 -- i32 )
   1 8 WRITES-BYTES 2 8 WRITES-BYTES
;FUNCTION
FUNCTION: SVG-RENDER rsvg_handle_render_document
   ( ptr u8 ptr u8 ptr u8 ptr u8 -- i32 )
   3 CELL WRITES-BYTES
;FUNCTION

VERSIONED-LIBRARY glib-2.0 0
FUNCTION: ERROR-FREE g_error_free ( ptr u8 -- ) ;FUNCTION

VERSIONED-LIBRARY gobject-2.0 0
FUNCTION: SVG-FREE g_object_unref ( ptr u8 -- ) ;FUNCTION

VERSIONED-LIBRARY cairo 2
FUNCTION: PDF-OPEN cairo_pdf_surface_create ( ptr u8 r r -- ptr u8 ) ;FUNCTION
FUNCTION: PDF-SIZE cairo_pdf_surface_set_size ( ptr u8 r r -- ) ;FUNCTION
FUNCTION: SURFACE-STATUS cairo_surface_status ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: SURFACE-FINISH cairo_surface_finish ( ptr u8 -- ) ;FUNCTION
FUNCTION: SURFACE-FREE cairo_surface_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: CONTEXT-OPEN cairo_create ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: CONTEXT-STATUS cairo_status ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: CONTEXT-FREE cairo_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: SHOW-PAGE cairo_show_page ( ptr u8 -- ) ;FUNCTION
FUNCTION: SCALE cairo_scale ( ptr u8 r r -- ) ;FUNCTION
FUNCTION: RECTANGLE cairo_rectangle ( ptr u8 r r r r -- ) ;FUNCTION
FUNCTION: CLIP cairo_clip ( ptr u8 -- ) ;FUNCTION

\ Four owned foreign handles and the four-double RsvgRectangle are one
\ task-local row. Both writers are synchronous and expose no handles.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 8 cells TASK:+USER STATE drop
: ERROR-CELL ( -- ptr ptr u8 ) STATE 0 ptr-field ;
: SVG-CELL ( -- ptr ptr u8 ) STATE 1 ptr-field ;
: SURFACE-CELL ( -- ptr ptr u8 ) STATE 2 ptr-field ;
: CONTEXT-CELL ( -- ptr ptr u8 ) STATE 3 ptr-field ;
: VIEWPORT ( -- ptr u8 ) STATE BYTE-VIEW 4 cells + ;
: WIDTH-PTR ( -- ptr u8 ) VIEWPORT 16 + ;
: HEIGHT-PTR ( -- ptr u8 ) VIEWPORT 24 + ;
: WIDTH ( -- r ) WIDTH-PTR LE:U64@ IEEE754:BITS>F64 ;
: HEIGHT ( -- r ) HEIGHT-PTR LE:U64@ IEEE754:BITS>F64 ;
: NULL? ( ptr a -- bool ) NULL-PTR = ;

: CLEAR ( -- )
   NULL-PTR ERROR-CELL ! NULL-PTR SVG-CELL !
   NULL-PTR SURFACE-CELL ! NULL-PTR CONTEXT-CELL !
   0 VIEWPORT LE:U64! 0 VIEWPORT 8 + LE:U64! ;

: RELEASE-PAGE ( -- )
   ERROR-CELL @ dup NULL? 0= if ERROR-FREE else drop then
   CONTEXT-CELL @ dup NULL? 0= if CONTEXT-FREE else drop then
   SVG-CELL @ dup NULL? 0= if SVG-FREE else drop then
   NULL-PTR ERROR-CELL ! NULL-PTR CONTEXT-CELL ! NULL-PTR SVG-CELL ! ;

: RELEASE ( -- )
   RELEASE-PAGE
   SURFACE-CELL @ dup NULL? 0= if SURFACE-FREE else drop then
   CLEAR ;

: POSITIVE-FINITE? ( r -- bool )
   {: value:r :}
   value IEEE754:F64>BITS $7FF0000000000000 and $7FF0000000000000 <>
   value 0.0 f> and ;

: OPEN-SVG ( ptr u8 n -- )
   {: data:ptr size:n :}
   size 0 < if E-OPEN throw then
   data size ERROR-CELL BYTE-VIEW SVG-OPEN SVG-CELL !
   SVG-CELL @ NULL? if E-OPEN throw then
   SVG-CELL @ 96.0 SVG-DPI
   SVG-CELL @ WIDTH-PTR HEIGHT-PTR SVG-SIZE 0= if E-SIZE throw then
   WIDTH POSITIVE-FINITE? HEIGHT POSITIVE-FINITE? and 0= if E-SIZE throw then
   WIDTH 0.75 f* MAX-PAGE-PT s>f f>
   HEIGHT 0.75 f* MAX-PAGE-PT s>f f> or if E-SIZE throw then ;

: OPEN-PDF ( ptr u8 n -- )
   FS:>PATHZ WIDTH 0.75 f* HEIGHT 0.75 f* PDF-OPEN SURFACE-CELL !
   SURFACE-CELL @ SURFACE-STATUS 0<> if E-RENDER throw then ;

: RENDER-PAGE ( -- )
   SURFACE-CELL @ CONTEXT-OPEN CONTEXT-CELL !
   CONTEXT-CELL @ 0.75 0.75 SCALE
   \ Clip document overflow to the authored page before rendering.
   CONTEXT-CELL @ 0.0 0.0 WIDTH HEIGHT RECTANGLE
   CONTEXT-CELL @ CLIP
   SVG-CELL @ CONTEXT-CELL @ VIEWPORT ERROR-CELL BYTE-VIEW SVG-RENDER
   0= if E-RENDER throw then
   CONTEXT-CELL @ CONTEXT-STATUS 0<> if E-RENDER throw then ;

: FINISH-PDF ( -- )
   SURFACE-CELL @ SURFACE-FINISH
   SURFACE-CELL @ SURFACE-STATUS 0<> if E-FINISH throw then ;

: RENDER-PDF ( ptr u8 n -- ) OPEN-PDF RENDER-PAGE FINISH-PDF ;

: PAGE-AT ( ptr SPAN:span<u8> n -- SPAN:span<u8> ) 2 * record-at @ ;

: EMIT-PAGE ( -- )
   CONTEXT-CELL @ SHOW-PAGE
   CONTEXT-CELL @ CONTEXT-STATUS 0<>
   SURFACE-CELL @ SURFACE-STATUS 0<> or if E-FINISH throw then ;

: RENDER-PAGES ( ptr SPAN:span<u8> n ptr u8 n -- ptr SPAN:span<u8> n )
   {: pages:ptr count:n path:ptr pathu:n :}
   path pathu OPEN-PDF
   count 0 do
      i 0 > if
         pages i PAGE-AT SPAN:$ OPEN-SVG
         \ set_size applies to the current page, before any drawing on it.
         SURFACE-CELL @ WIDTH 0.75 f* HEIGHT 0.75 f* PDF-SIZE
         SURFACE-CELL @ SURFACE-STATUS 0<> if E-RENDER throw then
      then
      RENDER-PAGE
      \ Finish omits an unpainted trailing page; emit every supplied page.
      EMIT-PAGE
      RELEASE-PAGE
   loop
   FINISH-PDF
   pages count ;

: WRITE-BODY ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n data:ptr size:n :}
   data size OPEN-SVG
   path pathu [: RENDER-PDF ;] FS:REPLACE-STAGED ;

: WRITE-PAGES-BODY ( ptr u8 n ptr SPAN:span<u8> n -- )
   {: path:ptr pathu:n pages:ptr count:n :}
   count 0 <= if E-OPEN throw then
   pages 0 PAGE-AT SPAN:$ OPEN-SVG
   pages count path pathu [: RENDER-PAGES ;] FS:REPLACE-STAGED 2drop ;

public

: WRITE ( ptr u8 n ptr u8 n -- )
   CLEAR
   [: WRITE-BODY ;] [: RELEASE ;] finally ;

: WRITE-PAGES ( ptr u8 n ptr SPAN:span<u8> n -- )
   CLEAR
   [: WRITE-PAGES-BODY ;] [: RELEASE ;] finally ;

;package
