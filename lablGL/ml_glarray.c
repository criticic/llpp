
#ifdef _WIN32
#include <wtypes.h>
#endif
#include <string.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "ml_gl.h"
#include "gl_tags.h"
#include "raw_tags.h"
#include "ml_raw.h"

int ml_glSizeOfValue(value v) {
   switch(v) {
   case MLTAG_one: return(1);
   case MLTAG_two: return(2);
   case MLTAG_three: return(3);
   case MLTAG_four: return(4);
   default: ml_raise_gl("ml_glSizeOfValue: invalid size");
   }
}


CAMLprim value ml_glEdgeFlagPointer(value raw)
{
  glEdgeFlagPointer(0, (GLboolean*)Addr_raw(raw));
  return Val_unit;
}

CAMLprim value ml_glTexCoordPointer(value size, value raw)
{
  glTexCoordPointer (ml_glSizeOfValue(size), 
		     GLenum_val(Kind_raw(raw)), 0, Void_raw(raw));
  return Val_unit;
}

CAMLprim value ml_glColorPointer(value size, value raw)
{
  glColorPointer (ml_glSizeOfValue(size), 
		  GLenum_val(Kind_raw(raw)), 0, Void_raw(raw));
  return Val_unit;
}

CAMLprim value ml_glIndexPointer(value raw)
{
  glIndexPointer (GLenum_val(Kind_raw(raw)), 0, Void_raw(raw));
  return Val_unit;
}

CAMLprim value ml_glNormalPointer(value raw)
{
  glNormalPointer (GLenum_val(Kind_raw(raw)), 0, Void_raw(raw));
  return Val_unit;
}

CAMLprim value ml_glVertexPointer(value size, value raw)
{
  glVertexPointer (ml_glSizeOfValue(size), 
		   GLenum_val(Kind_raw(raw)), 0, Void_raw(raw));
  return Val_unit;
}

CAMLprim value ml_glEnableClientState(value kl)
{
   GLenum a;

   switch(kl) {
   case MLTAG_edge_flag: a = GL_EDGE_FLAG_ARRAY; break;
   case MLTAG_texture_coord: a = GL_TEXTURE_COORD_ARRAY; break;
   case MLTAG_color: a = GL_COLOR_ARRAY; break;
   case MLTAG_index: a = GL_INDEX_ARRAY; break;
   case MLTAG_normal: a = GL_NORMAL_ARRAY; break;
   case MLTAG_vertex: a = GL_VERTEX_ARRAY; break;
   default: ml_raise_gl("ml_glEnableClientState: invalid array");
   }
   glEnableClientState(a);
   return Val_unit;
}

CAMLprim value ml_glDisableClientState(value kl)
{
   GLenum a;

   switch(kl) {
   case MLTAG_edge_flag: a = GL_EDGE_FLAG_ARRAY; break;
   case MLTAG_texture_coord: a = GL_TEXTURE_COORD_ARRAY; break;
   case MLTAG_color: a = GL_COLOR_ARRAY; break;
   case MLTAG_index: a = GL_INDEX_ARRAY; break;
   case MLTAG_normal: a = GL_NORMAL_ARRAY; break;
   case MLTAG_vertex: a = GL_VERTEX_ARRAY; break;
   default: ml_raise_gl("ml_glDisableClientState: invalid array");
   }
   glDisableClientState(a);
   return Val_unit;
}

ML_1 (glArrayElement, Int_val);
ML_3 (glDrawArrays, GLenum_val, Int_val, Int_val);

CAMLprim value ml_glDrawElements(value mode, value count, value raw) 
{
  glDrawElements (GLenum_val(mode), Int_val(count),
                  GLenum_val(Kind_raw(raw)), Void_raw(raw));
  return Val_unit;
}
