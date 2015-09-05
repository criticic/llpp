/* $Id: ml_gl.c,v 1.51 2007-04-13 02:48:43 garrigue Exp $ */

#ifdef _WIN32
#include <wtypes.h>
#endif
#include <string.h>
#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#ifdef HAS_GLEXT_H
#include <GL/glext.h>
#undef GL_VERSION_1_3
#endif
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "ml_raw.h"
#include "gl_tags.h"
#include "ml_gl.h"

#if !defined(GL_VERSION_1_4)
#define GL_GENERATE_MIPMAP 0x8191
#endif

/* #include <stdio.h> */

void ml_raise_gl(const char *errmsg)
{
  static value * gl_exn = NULL;
  if (gl_exn == NULL)
      gl_exn = caml_named_value("glerror");
  raise_with_string(*gl_exn, (char*)errmsg);
}

value copy_string_check (const char *str)
{
    if (!str) ml_raise_gl("Null string");
    return copy_string ((char*) str);
}

struct record {
    value key; 
    GLenum data;
};

static struct record input_table[] = {
#include "gl_tags.c"
};

static struct record *tag_table = NULL;

#define TABLE_SIZE (TAG_NUMBER*2+1)

CAMLprim value ml_gl_make_table (value unit)
{
    int i;
    unsigned int hash;

    tag_table = stat_alloc (TABLE_SIZE * sizeof(struct record));
    memset ((char *) tag_table, 0, TABLE_SIZE * sizeof(struct record));
    for (i = 0; i < TAG_NUMBER; i++) {
	hash = (unsigned long) input_table[i].key % TABLE_SIZE;
	while (tag_table[hash].key != 0) {
	    hash ++;
	    if (hash == TABLE_SIZE) hash = 0;
	}
	tag_table[hash].key = input_table[i].key;
	tag_table[hash].data = input_table[i].data;
    }
    return Val_unit;
}

GLenum GLenum_val(value tag)
{
    unsigned int hash = (unsigned long) tag % TABLE_SIZE;

    if (!tag_table) ml_gl_make_table (Val_unit);
    while (tag_table[hash].key != tag) {
	if (tag_table[hash].key == 0) ml_raise_gl ("Unknown tag");
	hash++;
	if (hash == TABLE_SIZE) hash = 0;
    }
    /*
    fprintf(stderr, "Converted %ld to %d", Int_val(tag), tag_table[hash].data);
    */
    return tag_table[hash].data;
}

/*
GLenum GLenum_val(value tag)
{
    switch(tag)
    {
#include "gl_tags.c"
    }
    ml_raise_gl("Unknown tag");
}
*/

ML_2 (glAccum, GLenum_val, Float_val)
ML_2 (glAlphaFunc, GLenum_val, Float_val)

ML_1 (glBegin, GLenum_val)

ML_5 (glBitmap, Int_val, Int_val, Pair(arg3,Float_val,Float_val),
      Pair(arg4,Float_val,Float_val), Void_raw)

ML_2 (glBlendFunc, GLenum_val, GLenum_val)

CAMLprim value ml_glClipPlane(value plane, value equation)  /* ML */
{
    double eq[4];
    int i;

    for (i = 0; i < 4; i++)
	eq[i] = Double_val (Field(equation,i));
    glClipPlane (GL_CLIP_PLANE0 + Int_val(plane), eq);
    return Val_unit;
}

CAMLprim value ml_glClear(value bit_list)  /* ML */
{
    GLbitfield accu = 0;

    while (bit_list != Val_int(0)) {
	switch (Field (bit_list, 0)) {
	case MLTAG_color:
	    accu |= GL_COLOR_BUFFER_BIT; break;
	case MLTAG_depth:
	    accu |= GL_DEPTH_BUFFER_BIT; break;
	case MLTAG_accum:
	    accu |= GL_ACCUM_BUFFER_BIT; break;
	case MLTAG_stencil:
	    accu |= GL_STENCIL_BUFFER_BIT; break;
	}
	bit_list = Field (bit_list, 1);
    }
    glClear (accu);
    return Val_unit;
}
ML_4 (glClearAccum, Float_val, Float_val, Float_val, Float_val)
ML_4 (glClearColor, Double_val, Double_val, Double_val, Double_val)
ML_1 (glClearDepth, Double_val)
ML_1 (glClearIndex, Float_val)
ML_1 (glClearStencil, Int_val)
ML_4 (glColor4d, Double_val, Double_val, Double_val, Double_val)
ML_4 (glColorMask, Int_val, Int_val, Int_val, Int_val)
ML_2 (glColorMaterial, GLenum_val, GLenum_val)
ML_5 (glCopyPixels, Int_val, Int_val, Int_val, Int_val, GLenum_val)
ML_1 (glCullFace, GLenum_val)

ML_1 (glDisable, GLenum_val)
ML_1 (glDepthFunc, GLenum_val)
ML_1 (glDepthMask, Int_val)
ML_2 (glDepthRange, Double_val, Double_val)

CAMLprim value ml_glDrawBuffer (value buffer)
{
    if (Is_block(buffer)) {
	int n = Int_val (Field(buffer,1));
	if (n >= GL_AUX_BUFFERS)
	    ml_raise_gl ("GlFunc.draw_buffer : no such auxiliary buffer");
	glDrawBuffer (GL_AUX0 + n);
    }
    else glDrawBuffer (GLenum_val(buffer));
    return Val_unit;
}

ML_4 (glDrawPixels, Int_val, Int_val, GLenum_val, Type_void_raw)

ML_1 (glEdgeFlag, Int_val)
ML_1 (glEnable, GLenum_val)
ML_0 (glEnd)
ML_1 (glEvalCoord1d, Double_val)
ML_2 (glEvalCoord2d, Double_val, Double_val)
ML_3 (glEvalMesh1, GLenum_val, Int_val, Int_val)
ML_5 (glEvalMesh2, GLenum_val, Int_val, Int_val, Int_val, Int_val)
ML_1 (glEvalPoint1, Int_val)
ML_2 (glEvalPoint2, Int_val, Int_val)


ML_3 (glFeedbackBuffer, Int_val, GLenum_val, (GLfloat*)Addr_raw)

CAMLprim value ml_glFog (value param) /* ML */
{
    float params[4];
    int i;

    switch (Field(param,0))
    {
    case MLTAG_mode:
	glFogi(GL_FOG_MODE, GLenum_val(Field(param,1)));
	break;
    case MLTAG_density:
	glFogf(GL_FOG_DENSITY, Float_val(Field(param,1)));
	break;
    case MLTAG_start:
	glFogf(GL_FOG_START, Float_val(Field(param,1)));
	break;
    case MLTAG_End:
	glFogf(GL_FOG_END, Float_val(Field(param,1)));
	break;
    case MLTAG_index:
	glFogf(GL_FOG_INDEX, Float_val(Field(param,1)));
	break;
    case MLTAG_color:
      for (i = 0; i < 4; i++) params[i] = Float_val(Field(Field(param,1),i));
	glFogfv(GL_FOG_COLOR, params);
	break;
    }
    return Val_unit;
}

ML_0 (glFlush)
ML_0 (glFinish)
ML_1 (glFrontFace, GLenum_val)
ML_3 (glFrustum, Pair(arg1,Double_val,Double_val),
      Pair(arg2,Double_val,Double_val), Pair(arg3,Double_val,Double_val))

ML_1_ (glGetString, GLenum_val, copy_string_check)
ML_2 (glGetDoublev, GLenum_val, Double_raw)

CAMLprim value ml_glGetError(value unit)
{
    switch (glGetError()) {
    case GL_NO_ERROR:       return MLTAG_no_error;
    case GL_INVALID_ENUM:   return MLTAG_invalid_enum;
    case GL_INVALID_VALUE:  return MLTAG_invalid_value;
    case GL_INVALID_OPERATION:  return MLTAG_invalid_operation;
    case GL_STACK_OVERFLOW: return MLTAG_stack_overflow;
    case GL_STACK_UNDERFLOW: return MLTAG_stack_underflow;
    case GL_OUT_OF_MEMORY:  return MLTAG_out_of_memory;
#if defined(GL_VERSION_1_2) || defined(GL_TABLE_TOO_LARGE)
    case GL_TABLE_TOO_LARGE: return MLTAG_table_too_large;
#endif
    default: ml_raise_gl("glGetError: unknown error");
    }
}
	
CAMLprim value ml_glHint (value target, value hint)
{
    GLenum targ = 0U;

    switch (target) {
    case MLTAG_fog:	targ = GL_FOG_HINT; break;
    case MLTAG_line_smooth:	targ = GL_LINE_SMOOTH_HINT; break;
    case MLTAG_perspective_correction:
	targ = GL_PERSPECTIVE_CORRECTION_HINT; break;
    case MLTAG_point_smooth:	targ = GL_POINT_SMOOTH_HINT; break;
    case MLTAG_polygon_smooth:	targ = GL_POLYGON_SMOOTH_HINT; break;
    }
    glHint (targ, GLenum_val(hint));
    return Val_unit;
}

ML_1 (glIndexMask, Int_val)
ML_1 (glIndexd, Double_val)
ML_0 (glInitNames)
ML_1_ (glIsEnabled, GLenum_val, Val_int)

CAMLprim value ml_glLight (value n, value param)  /* ML */
{
    float params[4];
    int i;

    if (Int_val(n) >= GL_MAX_LIGHTS) invalid_argument ("Gl.light");
    switch (Field(param,0))
    {
    case MLTAG_ambient:
    case MLTAG_diffuse:
    case MLTAG_specular:
    case MLTAG_position:
	for (i = 0; i < 4; i++)
	    params[i] = Float_val (Field(Field(param, 1), i));
	break;
    case MLTAG_spot_direction:
	for (i = 0; i < 3; i++)
	    params[i] = Float_val (Field(Field(param, 1), i));
	break;
    default:
	params[0] = Float_val (Field(param, 1));
    }
    glLightfv (GL_LIGHT0 + Int_val(n), GLenum_val(Field(param,0)), params);
    return Val_unit;
}

CAMLprim value ml_glLightModel (value param)  /* ML */
{
    float params[4];
    int i;

    switch (Field(param,0))
    {
    case MLTAG_ambient:
	for (i = 0; i < 4; i++)
	    params[i] = Float_val (Field(Field(param,1),i));
	glLightModelfv (GL_LIGHT_MODEL_AMBIENT, params);
	break;
    case MLTAG_local_viewer:
	glLightModelf (GL_LIGHT_MODEL_LOCAL_VIEWER,
		       Int_val(Field(param,1)));
	break;
    case MLTAG_two_side:
	glLightModeli (GL_LIGHT_MODEL_TWO_SIDE,
		       Int_val(Field(param,1)));
	break;
    case MLTAG_color_control:
#ifdef GL_VERSION_1_2
	switch (Field(param,1))
        {
          case MLTAG_separate_specular_color:
        	glLightModeli (GL_LIGHT_MODEL_COLOR_CONTROL,
		               GL_SEPARATE_SPECULAR_COLOR);
                break;
           case MLTAG_single_color:
        	glLightModeli (GL_LIGHT_MODEL_COLOR_CONTROL,
		               GL_SINGLE_COLOR);
                break;
        }
#else
        ml_raise_gl ("Parameter: GL_LIGHT_MODEL_COLOR_CONTROL not available");
#endif
	break;
    }
    return Val_unit;
}

ML_1 (glLineWidth, Float_val)
ML_2 (glLineStipple, Int_val, Int_val)
ML_1 (glLoadName, Int_val)
ML_0 (glLoadIdentity)
ML_1 (glLoadMatrixd, Double_raw)

#ifdef GL_VERSION_1_3
ML_1 (glLoadTransposeMatrixd, Double_raw)
#else
CAMLprim void ml_glLoadTransposeMatrixd (value raw)
{
    ml_raise_gl ("Function: glLoadTransposeMatrixd not available");
}
#endif
ML_1 (glLogicOp, GLenum_val)

CAMLprim value ml_glMap1d (value target, value *u, value order, value raw)
{
    int ustride = 0;
    GLenum targ = 0U;

    switch (target) {
    case MLTAG_vertex_3:
	targ = GL_MAP1_VERTEX_3; ustride = 3; break;
    case MLTAG_vertex_4:
	targ = GL_MAP1_VERTEX_4; ustride = 4; break;
    case MLTAG_index:
	targ = GL_MAP1_INDEX; ustride = 1; break;
    case MLTAG_color_4:
	targ = GL_MAP1_COLOR_4; ustride = 4; break;
    case MLTAG_normal:
	targ = GL_MAP1_NORMAL; ustride = 3; break;
    case MLTAG_texture_coord_1:
	targ = GL_MAP1_TEXTURE_COORD_1; ustride = 1; break;
    case MLTAG_texture_coord_2:
	targ = GL_MAP1_TEXTURE_COORD_2; ustride = 2; break;
    case MLTAG_texture_coord_3:
	targ = GL_MAP1_TEXTURE_COORD_3; ustride = 3; break;
    case MLTAG_texture_coord_4:
	targ = GL_MAP1_TEXTURE_COORD_4; ustride = 4; break;
    }
    glMap1d (targ, Double_val(u[0]), Double_val(u[1]),
	     ustride, Int_val(order), Double_raw(raw));
    return Val_unit;
}

CAMLprim value ml_glMap2d (value target, value u, value uorder,
                           value v, value vorder, value raw)
{
    int ustride = 0;
    GLenum targ = 0U;

    switch (target) {
    case MLTAG_vertex_3:
	targ = GL_MAP2_VERTEX_3; ustride = 3; break;
    case MLTAG_vertex_4:
	targ = GL_MAP2_VERTEX_4; ustride = 4; break;
    case MLTAG_index:
	targ = GL_MAP2_INDEX; ustride = 1; break;
    case MLTAG_color_4:
	targ = GL_MAP2_COLOR_4; ustride = 4; break;
    case MLTAG_normal:
	targ = GL_MAP2_NORMAL; ustride = 3; break;
    case MLTAG_texture_coord_1:
	targ = GL_MAP2_TEXTURE_COORD_1; ustride = 1; break;
    case MLTAG_texture_coord_2:
	targ = GL_MAP2_TEXTURE_COORD_2; ustride = 2; break;
    case MLTAG_texture_coord_3:
	targ = GL_MAP2_TEXTURE_COORD_3; ustride = 3; break;
    case MLTAG_texture_coord_4:
	targ = GL_MAP2_TEXTURE_COORD_4; ustride = 4; break;
    }
    glMap2d (targ, Double_val(Field(u,0)), Double_val(Field(u,1)), ustride,
	     Int_val(uorder), Double_val(Field(v,0)), Double_val(Field(v,1)),
	     Int_val(uorder)*ustride, Int_val(vorder), Double_raw(raw));
    return Val_unit;
}

ML_bc6 (ml_glMap2d)

ML_2 (glMapGrid1d, Int_val, Pair(arg2,Double_val,Double_val))
ML_4 (glMapGrid2d, Int_val, Pair(arg2,Double_val,Double_val),
      Int_val, Pair(arg4,Double_val,Double_val))

CAMLprim value ml_glMaterial (value face, value param)  /* ML */
{
    float params[4];
    int i;

    switch (Field(param,0))
    {
    case MLTAG_shininess:
	params[0] = Float_val (Field(param, 1));
	break;
    case MLTAG_color_indexes:
	for (i = 0; i < 3; i++)
	    params[i] = Float_val (Field(Field(param, 1), i));
	break;
    default:
	for (i = 0; i < 4; i++)
	    params[i] = Float_val (Field(Field(param, 1), i));
	break;
    }
    glMaterialfv (GLenum_val(face), GLenum_val(Field(param,0)), params);
    return Val_unit;
}

ML_1 (glMatrixMode, GLenum_val)
ML_1 (glMultMatrixd, Double_raw)

#ifdef GL_VERSION_1_3
ML_1 (glMultTransposeMatrixd, Double_raw)
#else
CAMLprim void ml_glMultTransposeMatrixd (value raw)
{
  ml_raise_gl ("Function: glMultTransposeMatrixd not available");
}
#endif 

ML_3 (glNormal3d, Double_val, Double_val, Double_val)

ML_1 (glPassThrough, Float_val)

CAMLprim value ml_glPixelMapfv (value map, value raw)
{
    glPixelMapfv (GLenum_val(map), Int_val(Size_raw(raw))/sizeof(GLfloat),
		  Float_raw(raw));
    return Val_unit;
}

ML_3 (glOrtho, Pair(arg1,Double_val,Double_val),
      Pair(arg2,Double_val,Double_val), Pair(arg3,Double_val,Double_val))

ML_1 (glPixelStorei, Pair(arg1,GLenum_val,Int_val))

CAMLprim value ml_glPixelTransfer (value param)
{
    GLenum pname = GLenum_val (Field(param,0));

    switch (pname) {
    case GL_MAP_COLOR:
    case GL_MAP_STENCIL:
    case GL_INDEX_SHIFT:
    case GL_INDEX_OFFSET:
	glPixelTransferi (pname, Int_val (Field(param,1)));
	break;
    default:
	glPixelTransferf (pname, Float_val (Field(param,1)));
    }
    return Val_unit;
}

ML_2 (glPixelZoom, Float_val, Float_val)
ML_1 (glPointSize, Float_val)
ML_2 (glPolygonOffset, Float_val, Float_val)
ML_2 (glPolygonMode, GLenum_val, GLenum_val)
ML_1 (glPolygonStipple, (unsigned char *)Byte_raw)
ML_0 (glPopAttrib)
ML_0 (glPopMatrix)
ML_0 (glPopName)

CAMLprim value ml_glPushAttrib (value list)
{
    GLbitfield mask = 0;

    while (list != Val_int(0)) {
	switch (Field(list,0)) {
	case MLTAG_accum_buffer:mask |= GL_ACCUM_BUFFER_BIT; break;
	case MLTAG_color_buffer:mask |= GL_COLOR_BUFFER_BIT; break;
	case MLTAG_current:	mask |= GL_CURRENT_BIT; break;
	case MLTAG_depth_buffer:mask |= GL_DEPTH_BUFFER_BIT; break;
	case MLTAG_enable:	mask |= GL_ENABLE_BIT; break;
	case MLTAG_eval:	mask |= GL_EVAL_BIT; break;
	case MLTAG_fog:		mask |= GL_FOG_BIT; break;
	case MLTAG_hint:	mask |= GL_HINT_BIT; break;
	case MLTAG_lighting:	mask |= GL_LIGHTING_BIT; break;
	case MLTAG_line:	mask |= GL_LINE_BIT; break;
	case MLTAG_list:	mask |= GL_LIST_BIT; break;
	case MLTAG_pixel_mode:	mask |= GL_PIXEL_MODE_BIT; break;
	case MLTAG_point:	mask |= GL_POINT_BIT; break;
	case MLTAG_polygon:	mask |= GL_POLYGON_BIT; break;
	case MLTAG_polygon_stipple:mask |= GL_POLYGON_STIPPLE_BIT; break;
	case MLTAG_scissor:	mask |= GL_SCISSOR_BIT; break;
	case MLTAG_stencil_buffer:mask |= GL_STENCIL_BUFFER_BIT; break;
	case MLTAG_texture:	mask |= GL_TEXTURE_BIT; break;
	case MLTAG_transform:	mask |= GL_TRANSFORM_BIT; break;
	case MLTAG_viewport:	mask |= GL_VIEWPORT_BIT; break;
	}
	list = Field(list,1);
    }
    glPushAttrib (mask);
    return Val_unit;
}

ML_0 (glPushMatrix)
ML_1 (glPushName, Int_val)

CAMLprim value ml_glRasterPos(value x, value y, value z, value w)  /* ML */
{
    if (z == Val_int(0)) glRasterPos2d (Double_val(x), Double_val(y));
    else if (w == Val_int(0))
	glRasterPos3d (Double_val(x), Double_val(y), Double_val(Field(z, 0)));
    else
	glRasterPos4d (Double_val(x), Double_val(y), Double_val(Field(z, 0)),
		    Double_val(Field(w, 0)));
    return Val_unit;
}

CAMLprim value ml_glReadBuffer (value buffer)
{
    if (Is_block(buffer)) {
	int n = Int_val (Field(buffer,1));
	if (n >= GL_AUX_BUFFERS)
	    ml_raise_gl ("GlFunc.read_buffer : no such auxiliary buffer");
	glReadBuffer (GL_AUX0 + n);
    }
    else glReadBuffer (GLenum_val(buffer));
    return Val_unit;
}

CAMLprim value ml_glReadPixels(value x, value y, value w, value h, value format , value raw)  /* ML */
{
  glPixelStorei(GL_PACK_SWAP_BYTES, 0);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glReadPixels(Int_val(x),Int_val(y),Int_val(w),Int_val(h),GLenum_val(format),
	       Type_void_raw(raw));
  return Val_unit;
}

ML_bc6 (ml_glReadPixels)
ML_2 (glRectd, Pair(arg1,Double_val,Double_val),
      Pair(arg2,Double_val,Double_val))
ML_1_ (glRenderMode, GLenum_val, Val_int)
ML_4 (glRotated, Double_val, Double_val, Double_val, Double_val)
ML_3 (glScaled, Double_val, Double_val, Double_val)

ML_4 (glScissor, Int_val, Int_val, Int_val, Int_val)
ML_2 (glSelectBuffer, Int_val, (GLuint*)Addr_raw)
ML_1 (glShadeModel, GLenum_val)
ML_3 (glStencilFunc, GLenum_val, Int_val, Int_val)
ML_1 (glStencilMask, Int_val)
ML_3 (glStencilOp, GLenum_val, GLenum_val, GLenum_val)

ML_1 (glTexCoord1d, Double_val)
ML_2 (glTexCoord2d, Double_val, Double_val)
ML_3 (glTexCoord3d, Double_val, Double_val, Double_val)
ML_4 (glTexCoord4d, Double_val, Double_val, Double_val, Double_val)

CAMLprim value ml_glTexEnv (value param)
{
    value params = Field(param,1);
    GLfloat color[4];
    int i;

    switch (Field(param,0)) {
    case MLTAG_mode:
	glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GLenum_val(params));
	break;
    case MLTAG_color:
	for (i = 0; i < 4; i++) color[i] = Float_val(Field(params,i));
	glTexEnvfv (GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, color);
	break;
    }
    return Val_unit;
}

CAMLprim value ml_glTexGen (value coord, value param)
{
    value params = Field(param,1);
    GLdouble point[4];
    int i;

    if (Field(param,0) == MLTAG_mode)
	glTexGeni (GLenum_val(coord), GL_TEXTURE_GEN_MODE, GLenum_val(params));
    else {
	for (i = 0; i < 4; i++) point[i] = Double_val(Field(params,i));
	glTexGendv (GLenum_val(coord), GLenum_val(Field(param,0)), point);
    }
    return Val_unit;
}

CAMLprim value ml_glTexImage1D (value proxy, value level, value internal,
                                value width, value border, value format,
                                value data)
{
    glTexImage1D (proxy == Val_int(1)
		  ? GL_PROXY_TEXTURE_1D : GL_TEXTURE_1D,
		  Int_val(level), Int_val(internal), Int_val(width),
		  Int_val(border), GLenum_val(format),
		  Type_raw(data), Void_raw(data));
    return Val_unit;
}

ML_bc7 (ml_glTexImage1D)

CAMLprim value ml_glTexImage2D (value proxy, value level, value internal,
                                value width, value height, value border,
                                value format, value data)
{
    /* printf("p=%x,l=%d,i=%d,w=%d,h=%d,b=%d,f=%x,t=%x,d=%x\n", */
    glTexImage2D (proxy == Val_int(1)
		  ? GL_PROXY_TEXTURE_2D : GL_TEXTURE_2D,
		  Int_val(level), Int_val(internal), Int_val(width),
		  Int_val(height), Int_val(border), GLenum_val(format),
		  Type_raw(data), Void_raw(data));
    /*  flush(stdout); */
    return Val_unit;
}

ML_bc8 (ml_glTexImage2D)

CAMLprim value ml_glTexParameter (value target, value param)
{
    GLenum targ = GLenum_val(target);
    GLenum pname = GLenum_val(Field(param,0));
    value params = Field(param,1);
    GLfloat color[4];
    int i;

    switch (pname) {
    case GL_TEXTURE_BORDER_COLOR:
	for (i = 0; i < 4; i++) color[i] = Float_val(Field(params,i));
	glTexParameterfv (targ, pname, color);
	break;
    case GL_TEXTURE_PRIORITY:
	glTexParameterf (targ, pname, Float_val(params));
	break;
    case GL_GENERATE_MIPMAP:
#ifdef GL_VERSION_1_4
        glTexParameteri (targ, pname, Int_val(params));
#else
        ml_raise_gl ("Parameter: GL_GENERATE_MIPMAP not available"); 
#endif
        break;
    default:
	glTexParameteri (targ, pname, GLenum_val(params));
	break;
    }
    return Val_unit;
}

ML_2 (glGenTextures, Int_val, Int_raw)
ML_2 (glBindTexture, GLenum_val, Nativeint_val)

CAMLprim value ml_glDeleteTexture (value texture_id)
{
    GLuint id = Nativeint_val(texture_id);
    glDeleteTextures(1,&id);
    return Val_unit;
}

ML_3 (glTranslated, Double_val, Double_val, Double_val)

CAMLprim value ml_glVertex(value x, value y, value z, value w)  /* ML */
{
    if (z == Val_int(0)) glVertex2d (Double_val(x), Double_val(y));
    else if (w == Val_int(0))
	glVertex3d (Double_val(x), Double_val(y), Double_val(Field(z, 0)));
    else
	glVertex4d (Double_val(x), Double_val(y), Double_val(Field(z, 0)),
		    Double_val(Field(w, 0)));
    return Val_unit;
}

ML_4 (glViewport, Int_val, Int_val, Int_val, Int_val)


/* List functions */

ML_1_ (glIsList, Int_val, Val_int)
ML_2 (glDeleteLists, Int_val, Int_val)
ML_1_ (glGenLists, Int_val, Val_int)
ML_2 (glNewList, Int_val, GLenum_val)
ML_0 (glEndList)
ML_1 (glCallList, Int_val)
ML_1 (glListBase, Int_val)

CAMLprim value ml_glCallLists (value indexes)  /* ML */
{
    int len,i;
    int * table;

    switch (Field(indexes,0)) {
    case MLTAG_byte:
	glCallLists (string_length(Field(indexes,1)),
		     GL_UNSIGNED_BYTE,
		     String_val(Field(indexes,1)));
	break;
    case MLTAG_int:
	len = Wosize_val (indexes);
	table = calloc (len, sizeof (GLint));
	for (i = 0; i < len; i++) table[i] = Int_val (Field(indexes,i));
	glCallLists (len, GL_INT, table);
	free (table);
	break;
    }
    return Val_unit;
}
