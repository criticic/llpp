/* $Id: ml_gl.h,v 1.21 2003-10-03 04:27:19 garrigue Exp $ */

#ifndef _ml_gl_
#define _ml_gl_

#include "ml_raw.h"

void ml_raise_gl (const char *errmsg) Noreturn;
#define copy_string_check lablgl_copy_string_check
value copy_string_check (const char *str);

GLenum GLenum_val (value);

#define Float_val(dbl) ((GLfloat) Double_val(dbl))
#define Addr_val(addr) ((GLvoid *) addr)
#define Val_addr(addr) ((value) addr)
#define Type_raw(raw) (GLenum_val(Kind_raw(raw)))
#define Type_void_raw(raw) Type_raw(raw), Void_raw(raw)

#define ML_0(cname) \
CAMLprim value ml_##cname (value unit) \
{ cname (); return Val_unit; }
#define ML_1(cname, conv1) \
CAMLprim value ml_##cname (value arg1) \
{ cname (conv1(arg1)); return Val_unit; }
#define ML_2(cname, conv1, conv2) \
CAMLprim value ml_##cname (value arg1, value arg2) \
{ cname (conv1(arg1), conv2(arg2)); return Val_unit; }
#define ML_3(cname, conv1, conv2, conv3) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3) \
{ cname (conv1(arg1), conv2(arg2), conv3(arg3)); return Val_unit; }
#define ML_4(cname, conv1, conv2, conv3, conv4) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4) \
{ cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4)); \
  return Val_unit; }
#define ML_5(cname, conv1, conv2, conv3, conv4, conv5) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5) \
{ cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5)); \
  return Val_unit; }
#define ML_6(cname, conv1, conv2, conv3, conv4, conv5, conv6) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6) \
{ cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5), \
         conv6(arg6)); \
  return Val_unit; }
#define ML_7(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7) \
{ cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5), \
         conv6(arg6), conv7(arg7)); \
  return Val_unit; }
#define ML_8(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8) \
{ cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5), \
         conv6(arg6), conv7(arg7), conv8(arg8)); \
  return Val_unit; }

#define ML_0_(cname, conv) \
CAMLprim value ml_##cname (value unit) \
{ return conv (cname ()); }
#define ML_1_(cname, conv1, conv) \
CAMLprim value ml_##cname (value arg1) \
{ return conv (cname (conv1(arg1))); }
#define ML_2_(cname, conv1, conv2, conv) \
CAMLprim value ml_##cname (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define ML_3_(cname, conv1, conv2, conv3, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define ML_4_(cname, conv1, conv2, conv3, conv4, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define ML_5_(cname, conv1, conv2, conv3, conv4, conv5, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5))); }
#define ML_6_(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6))); }
#define ML_7_(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7))); }
#define ML_8_(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
             conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8))); }

/* Use with care: needs the argument index */
#define Ignore(x)
#define Split(x,f,g) f(x), g(x) Ignore
#define Split3(x,f,g,h) f(x), g(x), h(x) Ignore
#define Pair(x,f,g) f(Field(x,0)), g(Field(x,1)) Ignore
#define Triple(x,f,g,h) f(Field(x,0)), g(Field(x,1)), h(Field(x,2)) Ignore

/* For more than 5 arguments */
#define ML_bc6(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]); }

#define ML_bc7(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6]); }

#define ML_bc8(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7]); }


/* subtleties of openGL 1.1 vs 1.2 */
#if !defined(GL_DOUBLE) && defined(GL_DOUBLE_EXT)
#define GL_DOUBLE GL_DOUBLE_EXT
#endif
#if !defined(GL_TEXTURE_PRIORITY) && defined(GL_TEXTURE_PRIORITY_EXT)
#define GL_TEXTURE_PRIORITY GL_TEXTURE_PRIORITY_EXT
#endif
#if !defined(GL_PROXY_TEXTURE_1D) && defined(GL_PROXY_TEXTURE_1D_EXT)
#define GL_PROXY_TEXTURE_1D GL_PROXY_TEXTURE_1D_EXT
#endif
#if !defined(GL_PROXY_TEXTURE_2D) && defined(GL_PROXY_TEXTURE_2D_EXT)
#define GL_PROXY_TEXTURE_2D GL_PROXY_TEXTURE_2D_EXT
#endif

#endif
