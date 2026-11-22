split-str
\ svg-pdf.f - completed in-memory SVGs become PDF pages through native
\ librsvg 2.52+/Cairo, in `package SVG-PDF`. Each SVG loads with no base URI:
\ data: resources resolve, external files do not, and librsvg may omit a missing
\ one silently. The destination is replaced by one FS:REPLACE-STAGED only after
\ every page rendered, so a failure leaves it intact. Test: test/svg-pdf-e2e.f.
\ STORAGE CLASS. TASK-LOCAL: the foreign handles and the viewport are TASK:+USER
\ rows.
require src/lib/ffi-abi.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/ieee754.f
require src/lib/fmath.f
require src/lib/span.f

package SVG-PDF
public

-9490 constant E-SVG-PDF-FIRST
-9499 constant E-SVG-PDF-LAST
-9490 s" malformed SVG input or a nonpositive page count" ERROR:CODE E-OPEN
-9491 s" an unresolved, nonfinite, nonpositive or oversized dimension" ERROR:CODE E-SIZE
-9492 s" librsvg or Cairo failed to render" ERROR:CODE E-RENDER
-9493 s" page emission, finalization or output failed" ERROR:CODE E-FINISH

private

\ Pages are CSS px * 72/96 points (96 DPI), no rounding or crop; a side over
\ 14,400 pt (200 in, the PDF 1.7 Appendix C limit, no UserUnit) is E-SIZE before
\ that page draws, and both intrinsic dimensions must resolve to finite positive
\ CSS pixels (a percentage or a lone viewBox does not). Nothing is clamped.
14400 constant MAX-PAGE-PT

VERSIONED-LIBRARY rsvg-2 2
FUNCTION: SVG-OPEN rsvg_handle_new_from_data
   ( ^u8 n ^u8 -- ^u8 )
;FUNCTION
FUNCTION: SVG-DPI rsvg_handle_set_dpi ( ^u8 r -- ) ;FUNCTION
FUNCTION: SVG-SIZE rsvg_handle_get_intrinsic_size_in_pixels
   ( ^u8 ^u8 ^u8 -- i32 )
;FUNCTION
FUNCTION: SVG-RENDER rsvg_handle_render_document
   ( ^u8 ^u8 ^u8 ^u8 -- i32 )
;FUNCTION

VERSIONED-LIBRARY glib-2.0 0
FUNCTION: ERROR-FREE g_error_free ( ^u8 -- ) ;FUNCTION

VERSIONED-LIBRARY gobject-2.0 0
FUNCTION: SVG-FREE g_object_unref ( ^u8 -- ) ;FUNCTION

VERSIONED-LIBRARY cairo 2
FUNCTION: PDF-OPEN cairo_pdf_surface_create ( ^u8 r r -- ^u8 ) ;FUNCTION
FUNCTION: PDF-SIZE cairo_pdf_surface_set_size ( ^u8 r r -- ) ;FUNCTION
FUNCTION: SURFACE-STATUS cairo_surface_status ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: SURFACE-FINISH cairo_surface_finish ( ^u8 -- ) ;FUNCTION
FUNCTION: SURFACE-FREE cairo_surface_destroy ( ^u8 -- ) ;FUNCTION
FUNCTION: CONTEXT-OPEN cairo_create ( ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: CONTEXT-STATUS cairo_status ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: CONTEXT-FREE cairo_destroy ( ^u8 -- ) ;FUNCTION
FUNCTION: SHOW-PAGE cairo_show_page ( ^u8 -- ) ;FUNCTION
FUNCTION: SCALE cairo_scale ( ^u8 r r -- ) ;FUNCTION
FUNCTION: RECTANGLE cairo_rectangle ( ^u8 r r r r -- ) ;FUNCTION
FUNCTION: CLIP cairo_clip ( ^u8 -- ) ;FUNCTION

\ The four owned foreign handles and the RsvgRectangle are task-local rows; the
\ rectangle holds the bits of the doubles librsvg reads and fills. Both writers
\ are synchronous and expose no handles.
TASK:+USER ERROR-CELL ^u8
TASK:+USER SVG-CELL ^u8
TASK:+USER SURFACE-CELL ^u8
TASK:+USER CONTEXT-CELL ^u8
STRUCTURE rect DERIVE addr
   FIELD x n
   FIELD y n
   FIELD width n
   FIELD height n
;STRUCTURE
TASK:+USER VIEW rect
: WIDTH ( -- r ) VIEW RECT-WIDTH @ IEEE754:BITS>F64 ;
: HEIGHT ( -- r ) VIEW RECT-HEIGHT @ IEEE754:BITS>F64 ;
: NULL? ( ^a -- bool ) NULL-PTR = ;

: CLEAR ( -- )
   NULL-PTR ERROR-CELL ! NULL-PTR SVG-CELL !
   NULL-PTR SURFACE-CELL ! NULL-PTR CONTEXT-CELL !
   0 VIEW RECT-X ! 0 VIEW RECT-Y ! ;

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
   value FMATH:FINITE?
   value 0.0 f> and ;

: OPEN-SVG ( str -- )
   {: data:^ size:n :}
   size 0 < if E-OPEN throw then
   data size ERROR-CELL BYTE-VIEW SVG-OPEN SVG-CELL !
   SVG-CELL @ NULL? if E-OPEN throw then
   SVG-CELL @ 96.0 SVG-DPI
   SVG-CELL @ VIEW RECT-WIDTH BYTE-VIEW
   VIEW RECT-HEIGHT BYTE-VIEW SVG-SIZE 0= if E-SIZE throw then
   WIDTH POSITIVE-FINITE? HEIGHT POSITIVE-FINITE? and 0= if E-SIZE throw then
   WIDTH 0.75 f* MAX-PAGE-PT s>f f>
   HEIGHT 0.75 f* MAX-PAGE-PT s>f f> or if E-SIZE throw then ;

: OPEN-PDF ( str -- )
   FS:>PATHZ WIDTH 0.75 f* HEIGHT 0.75 f* PDF-OPEN SURFACE-CELL !
   SURFACE-CELL @ SURFACE-STATUS 0<> if E-RENDER throw then ;

: RENDER-PAGE ( -- )
   SURFACE-CELL @ CONTEXT-OPEN CONTEXT-CELL !
   CONTEXT-CELL @ 0.75 0.75 SCALE
   \ Clip document overflow to the authored page before rendering.
   CONTEXT-CELL @ 0.0 0.0 WIDTH HEIGHT RECTANGLE
   CONTEXT-CELL @ CLIP
   SVG-CELL @ CONTEXT-CELL @ VIEW BYTE-VIEW ERROR-CELL BYTE-VIEW
   SVG-RENDER
   0= if E-RENDER throw then
   CONTEXT-CELL @ CONTEXT-STATUS 0<> if E-RENDER throw then ;

: FINISH-PDF ( -- )
   SURFACE-CELL @ SURFACE-FINISH
   SURFACE-CELL @ SURFACE-STATUS 0<> if E-FINISH throw then ;

: RENDER-PDF ( str -- ) OPEN-PDF RENDER-PAGE FINISH-PDF ;

: PAGE-AT ( ^SPAN:span<u8> n -- SPAN:span<u8> ) 2 * record-at @ ;

: EMIT-PAGE ( -- )
   CONTEXT-CELL @ SHOW-PAGE
   CONTEXT-CELL @ CONTEXT-STATUS 0<>
   SURFACE-CELL @ SURFACE-STATUS 0<> or if E-FINISH throw then ;

: RENDER-PAGES ( ^SPAN:span<u8> n str -- ^SPAN:span<u8> n )
   {: pages:^ count:n path:^ pathu:n :}
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

: WRITE-BODY ( str str -- )
   {: path:^ pathu:n data:^ size:n :}
   data size OPEN-SVG
   path pathu [: RENDER-PDF ;] FS:REPLACE-STAGED ;

: WRITE-PAGES-BODY ( str ^SPAN:span<u8> n -- )
   {: path:^ pathu:n pages:^ count:n :}
   count 0 <= if E-OPEN throw then
   pages 0 PAGE-AT SPAN:$ OPEN-SVG
   pages count path pathu [: RENDER-PAGES ;] FS:REPLACE-STAGED 2drop ;

public

: WRITE ( ^u8 n ^u8 n -- )
   CLEAR
   [: WRITE-BODY ;] [: RELEASE ;] finally ;

: WRITE-PAGES ( str ^SPAN:span<u8> n -- )
   CLEAR
   [: WRITE-PAGES-BODY ;] [: RELEASE ;] finally ;

;package
