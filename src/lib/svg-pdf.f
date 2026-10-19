\ A completed in-memory SVG becomes one PDF page through native librsvg/Cairo.
\ STORAGE CLASS. TASK-LOCAL: foreign handles and the viewport live in STATE.
require src/lib/ffi-abi.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/ieee754.f
require src/lib/le.f

package SVG-PDF
public

-9490 constant E-SVG-PDF-FIRST
-9499 constant E-SVG-PDF-LAST
-9490 constant E-OPEN
-9491 constant E-SIZE
-9492 constant E-RENDER
-9493 constant E-FINISH

private

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
FUNCTION: SURFACE-STATUS cairo_surface_status ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: SURFACE-FINISH cairo_surface_finish ( ptr u8 -- ) ;FUNCTION
FUNCTION: SURFACE-FREE cairo_surface_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: CONTEXT-OPEN cairo_create ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: CONTEXT-STATUS cairo_status ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: CONTEXT-FREE cairo_destroy ( ptr u8 -- ) ;FUNCTION
FUNCTION: SCALE cairo_scale ( ptr u8 r r -- ) ;FUNCTION
FUNCTION: RECTANGLE cairo_rectangle ( ptr u8 r r r r -- ) ;FUNCTION
FUNCTION: CLIP cairo_clip ( ptr u8 -- ) ;FUNCTION

\ Four owned foreign handles and the four-double RsvgRectangle are one
\ task-local row. WRITE is synchronous and exposes no callbacks or handles.
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

: RELEASE ( -- )
   ERROR-CELL @ dup NULL? 0= if ERROR-FREE else drop then
   CONTEXT-CELL @ dup NULL? 0= if CONTEXT-FREE else drop then
   SURFACE-CELL @ dup NULL? 0= if SURFACE-FREE else drop then
   SVG-CELL @ dup NULL? 0= if SVG-FREE else drop then
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

: RENDER-PDF ( ptr u8 n -- )
   FS-PATHZ WIDTH 0.75 f* HEIGHT 0.75 f* PDF-OPEN SURFACE-CELL !
   SURFACE-CELL @ SURFACE-STATUS 0<> if E-RENDER throw then
   SURFACE-CELL @ CONTEXT-OPEN CONTEXT-CELL !
   CONTEXT-CELL @ 0.75 0.75 SCALE
   \ Clip document overflow to the authored page before rendering.
   CONTEXT-CELL @ 0.0 0.0 WIDTH HEIGHT RECTANGLE
   CONTEXT-CELL @ CLIP
   SVG-CELL @ CONTEXT-CELL @ VIEWPORT ERROR-CELL BYTE-VIEW SVG-RENDER
   0= if E-RENDER throw then
   CONTEXT-CELL @ CONTEXT-STATUS 0<> if E-RENDER throw then
   SURFACE-CELL @ SURFACE-FINISH
   SURFACE-CELL @ SURFACE-STATUS 0<> if E-FINISH throw then ;

: WRITE-BODY ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n data:ptr size:n :}
   data size OPEN-SVG
   path pathu [: RENDER-PDF ;] REPLACE-STAGED ;

public

: WRITE ( ptr u8 n ptr u8 n -- )
   CLEAR
   [: WRITE-BODY ;] [: RELEASE ;] finally ;

;package
