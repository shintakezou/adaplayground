/*
 * Mostly a pick of:
 *    http://www.labbookpages.co.uk/software/imgProc/libPNG.html
 *
 * But also check examples here:
 *    http://www.libpng.org/pub/png/libpng-manual.txt
 *
 * And read:
 *    http://www.libpng.org/pub/png/book/toc.html
 *    http://www.libpng.org/pub/png/spec/1.1/PNG-Chunks.html
 */

#include "pngfunc.h"

//#include <stdio.h>
#include <stdint.h>

#include <png.h>


int create_image(const char *filename, int w, int h, uint8_t *data)
{
    int r = -1;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;

    //printf("(%d, %d)\n", w, h);

    if (!data) return -1;

#ifdef RAW_DUMP 
    FILE *rawdump = fopen("out.raw", "wb");
    if (rawdump) {
        fwrite(data, sizeof (uint8_t), 3 * w * h, rawdump);
        fclose(rawdump);
    }
#endif
    
    FILE *fh = fopen(filename, "wb");
    if (!fh) return -1;

    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                      NULL,
                                      NULL,
                                      NULL);
    if (!png_ptr) goto quickdepart;
    
    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) goto quickdepart;
    /* does this "style" embarass you? Well, it reminds me of the days
       I used to write 68k assembly; you may do a lot of things like
       this (*) in asm, and no-one cares :-) For clean-ups it is
       effective in C, too, I think */

    if (setjmp(png_jmpbuf(png_ptr))) goto quickdepart;

    png_init_io(png_ptr, fh);
    
    png_set_IHDR(png_ptr, info_ptr,
                 w, h,
                 8,
                 PNG_COLOR_TYPE_RGB,
                 PNG_INTERLACE_NONE,
                 PNG_COMPRESSION_TYPE_BASE,
                 PNG_FILTER_TYPE_BASE);

    png_write_info(png_ptr, info_ptr);

    for (int y = 0; y < h; ++y) {
        png_write_row(png_ptr, data + 3*w*y);
    }

    png_write_end(png_ptr, NULL);
    r = 0;

  quickdepart: /* (*) */
    if (info_ptr) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
    if (png_ptr) png_destroy_write_struct(&png_ptr, NULL);
    if (fh) fclose(fh);
    return r;
}
