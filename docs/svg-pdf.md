# SVG to PDF

`src/lib/svg-pdf.f` renders a completed SVG byte span into a one-page PDF:

```forth
require src/lib/svg-pdf.f

: SAVE ( ptr u8 n -- )
   s" drawing.pdf" 2swap SVG-PDF:WRITE ;
```

`SVG-PDF:WRITE ( ptr u8 n ptr u8 n -- )` takes the destination path first,
then SVG bytes. It calls native librsvg and Cairo directly through declared
FFI functions. Runtime dependencies are librsvg 2.52 or newer, Cairo with PDF
support, GLib and GObject. The versioned library names are `rsvg-2`/2,
`cairo`/2, `glib-2.0`/0 and `gobject-2.0`/0: for example,
`librsvg-2.2.dylib` on macOS and `librsvg-2.so.2` on Linux.

The SVG is loaded with `rsvg_handle_new_from_data` and no base URI. Supply
self-contained SVG; librsvg's resource policy permits embedded `data:`
resources but cannot resolve external files without a base URI. Missing
resources can be omitted by librsvg rather than reported as a failure. See
[librsvg's resource policy](https://gnome.pages.gitlab.gnome.org/librsvg/Rsvg-2.0/class.Handle.html#security-and-locations-of-referenced-files).

The handle's DPI is explicitly 96. Both intrinsic dimensions must resolve to
finite positive CSS pixel sizes. Percentage dimensions and a `viewBox` alone
do not establish a page size. Fractional dimensions remain binary64 values;
the PDF page size is CSS pixels multiplied by 72/96, with no integer rounding
or content-based crop. See
[intrinsic pixel dimensions](https://gnome.pages.gitlab.gnome.org/librsvg/Rsvg-2.0/method.Handle.get_intrinsic_size_in_pixels.html).

This writer uses default 1/72-inch PDF user units without `UserUnit` scaling.
Each page dimension is limited to 14,400 points (200 inches), the Acrobat
compatibility boundary in [PDF 1.7, Appendix C, page 993](https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/pdfreference1.7old.pdf#page=993).
Larger dimensions throw `E-SIZE` before staged output begins; dimensions are
never clamped or rescaled to fit.

Cairo's coordinate system is scaled by 72/96, explicitly clipped to the page,
and passed to
[render_document](https://gnome.pages.gitlab.gnome.org/librsvg/Rsvg-2.0/method.Handle.render_document.html)
with the original fractional pixel viewport. Cairo's
[PDF surface](https://www.cairographics.org/manual/cairo-PDF-Surfaces.html)
uses points, where 72 points equal one inch. Vector paths remain vectors when
supported by Cairo; SVG raster images and effects can produce PDF images.
Cairo quantizes exported path coordinates: the E2E fixture checks its vector
coordinates within 0.003 point, and checks its page dimensions separately
within 0.000001 point.

The destination is replaced through `REPLACE-STAGED` only after rendering,
checking the context status, explicitly
[finishing the surface and checking its status](https://cairographics.org/manual/cairo-cairo-surface-t.html#cairo-surface-finish).
Failures leave the existing destination intact and remove the staged sibling.
Every call releases its GError, SVG handle, context and surface on success or
throw. Mutable scratch belongs to the current task; no foreign handle survives
the call.

Public failures are `SVG-PDF:E-OPEN` (-9490) for malformed input,
`SVG-PDF:E-SIZE` (-9491) for unresolved, nonfinite, nonpositive or oversized
dimensions, `SVG-PDF:E-RENDER` (-9492) for a Cairo or librsvg rendering failure, and
`SVG-PDF:E-FINISH` (-9493) for finalization/output failure. Filesystem and FFI
errors retain their existing named codes.

Run `bin/hb --load test/svg-pdf-e2e.f`. The test needs `qpdf`, `pdfinfo` and
`pdfimages` on PATH. It writes repeatable fractional and physical-unit PDF
fixtures, including a 200-inch-square page, plus decoded QDF files, under the
printed `build/svg-pdf-e2e-*` path. Each process owns its fixture directory.
Independent tools
check structure, page count, MediaBox, absence of raster images in the vector
fixture, and the scaled line geometry. Parser, sizing, actual librsvg
instancing-limit, destination and actual file-size-limit failures each precede
a valid retry. The file-size limit and signal disposition are restored.
