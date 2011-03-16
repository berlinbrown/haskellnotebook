/* 

Copyright (C) 2000 Matthew Danish

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include<GL/gl.h>
#include<stdio.h>
#include"defines.h"

#ifdef PNG_SCREENSHOT
#include<png.h>
#endif

/*
 * Thanks go to Andreas Umbach and the creators of glTron for the code
 * to write a PNG screenshot in SDL/OpenGL
 *
 */

#ifdef PNG_SCREENSHOT
static FILE *_fp;
#endif

bool PNGScreenShot(char *filename) {
#ifdef PNG_SCREENSHOT
  void user_write_data(png_structp png_ptr,png_bytep data, png_size_t length);
  void user_flush_data(png_structp png_ptr);
  unsigned char *data;

  png_structp png_ptr;
  png_infop info_ptr;
  png_byte **row_pointers;
  int colortype;
  int width, height;
  int i;

  width = SCREEN_W;
  height = SCREEN_H;
  data = new unsigned char[width * height * 3];
  
  glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, data);
  
  _fp = fopen(filename, "wb");
  if(!_fp) {
    fprintf(stderr, "can't open %s for writing\n", filename);
    return 0;

  }
  
  png_ptr = png_create_write_struct
    /*     (PNG_LIBPNG_VER_STRING, (png_voidp)user_error_ptr,
           user_error_fn, user_warning_fn); */
    (PNG_LIBPNG_VER_STRING, 0, 0, 0);

  if (!png_ptr)
    return 0;

  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
      png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
      return 0;
  }
  /* png_init_io(png_ptr, _fp); */
  png_set_write_fn(png_ptr, 0, user_write_data, user_flush_data);

  colortype = PNG_COLOR_TYPE_RGB;
  png_set_IHDR(png_ptr, info_ptr, width, height,
               8, colortype, PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png_ptr, info_ptr);

  /* get pointers */
  row_pointers = (png_byte**) new (png_byte*)[height];
  for(i = 0; i < height; i++)
    row_pointers[i] = data + (height - i - 1) 
      * 3 * width;

  png_write_image(png_ptr, row_pointers);
  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);
  
  delete row_pointers;
  delete data;
  fclose(_fp);
  
  fprintf(stderr, "Wrote screenshot to %s\n", filename);
#endif
  return 1;
}

#ifdef PNG_SCREENSHOT
void user_write_data(png_structp png_ptr,
                     png_bytep data, png_size_t length) {
  fwrite(data, length, 1, _fp);
}

void user_flush_data(png_structp png_ptr) {
  fflush(_fp);
}
#endif

