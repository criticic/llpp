/* $Id: ml_raw.c,v 1.16 2007-04-13 02:48:43 garrigue Exp $ */

#include <string.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include "raw_tags.h"
#include "ml_raw.h"

#define SIZE_BYTE	sizeof(char)
#define SIZE_SHORT	sizeof(short)
#define SIZE_INT	sizeof(int)
#define SIZE_LONG	sizeof(long)
#define SIZE_FLOAT	sizeof(float)
#define SIZE_DOUBLE	sizeof(double)

extern void invalid_argument (char *) Noreturn;
extern void raise_out_of_memory (void) Noreturn;

static int raw_sizeof (value kind)
{
    switch (kind) {
    case MLTAG_bitmap:
    case MLTAG_byte:
    case MLTAG_ubyte:
	return SIZE_BYTE;
    case MLTAG_short:
    case MLTAG_ushort:
	return SIZE_SHORT;
    case MLTAG_int:
    case MLTAG_uint:
	return SIZE_INT;
    case MLTAG_long:
    case MLTAG_ulong:
	return SIZE_LONG;
    case MLTAG_float:
	return SIZE_FLOAT;
    case MLTAG_double:
	return SIZE_DOUBLE;
    }
    return 0;
}

CAMLprim value ml_raw_sizeof (value kind)  /* ML */
{
    return Val_int(raw_sizeof(kind));
}

static void check_size (value raw, long pos, char *msg)
{
    if (pos < 0 ||
	(pos+1) * raw_sizeof(Kind_raw(raw)) > Int_val(Size_raw(raw)))
	invalid_argument (msg);
}

CAMLprim value ml_raw_get (value raw, value pos)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.get");
    switch (Kind_raw(raw)) {
    case MLTAG_bitmap:
    case MLTAG_ubyte:
	return Val_long ((unsigned char) Byte_raw(raw)[i]);
    case MLTAG_byte:
	return Val_long (Byte_raw(raw)[i]);
    case MLTAG_short:
	return Val_long (Short_raw(raw)[i]);
    case MLTAG_ushort:
	return Val_long ((unsigned short) Short_raw(raw)[i]);
    case MLTAG_int:
	return Val_long (Int_raw(raw)[i]);
    case MLTAG_uint:
	return Val_long ((unsigned int) Int_raw(raw)[i]);
    case MLTAG_long:
	return Val_long (Long_raw(raw)[i]);
    case MLTAG_ulong:
	return Val_long ((unsigned long) Long_raw(raw)[i]);
    }
    return Val_unit;
}

CAMLprim value ml_raw_read (value raw, value pos, value len)  /* ML */
{
    int s = Int_val(pos);
    int i, l = Int_val(len);
    value ret;

    check_size (raw,s+l-1,"Raw.read");
    if (l<0 || s<0) invalid_argument("Raw.read");
    ret = alloc_shr (l, 0);
    switch (Kind_raw(raw)) {
    case MLTAG_bitmap:
    case MLTAG_ubyte:
    {
	unsigned char *byte_raw = (unsigned char *)Byte_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*byte_raw++);
	break;
    }
    case MLTAG_byte:
    {
	char *byte_raw = Byte_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*byte_raw++);
	break;
    }
    case MLTAG_short:
    {
	short *short_raw = Short_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*short_raw++);
	break;
    }
    case MLTAG_ushort:
    {
	unsigned short *short_raw = (unsigned short *)Short_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*short_raw++);
	break;
    }
    case MLTAG_int:
    {
	int *int_raw = Int_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*int_raw++);
	break;
    }
    case MLTAG_uint:
    {
	unsigned int *int_raw = (unsigned int *)Int_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*int_raw++);
	break;
    }
    case MLTAG_long:
    {
	long *long_raw = Long_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*long_raw++);
	break;
    }
    case MLTAG_ulong:
    {
	unsigned long *long_raw = (unsigned long *)Long_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Field(ret,i) = Val_long (*long_raw++);
	break;
    }
    }
    return ret;
}

CAMLprim value ml_raw_read_string (value raw, value pos, value len)  /* ML */
{
    CAMLparam1(raw);
    int s = Int_val(pos);
    int l = Int_val(len);
    value ret;

    if (l<0 || s<0 || s+l > Int_val(Size_raw(raw)))
	invalid_argument("Raw.read_string");
    ret = alloc_string (l);
    memcpy (String_val(ret), Bp_val(Addr_raw(raw))+s, l);
    CAMLreturn(ret);
}

CAMLprim value ml_raw_write_string (value raw, value pos, value data)  /* ML */
{
    int s = Int_val(pos);
    int l = string_length(data);

    if (s<0 || s+l > Int_val(Size_raw(raw)))
	invalid_argument("Raw.write_string");
    memcpy (Bp_val(Addr_raw(raw))+s, String_val(data), l);
    return Val_unit;
}

CAMLprim value ml_raw_set (value raw, value pos, value data)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.set");
    switch (Kind_raw(raw)) {
    case MLTAG_bitmap:
    case MLTAG_ubyte:
    case MLTAG_byte:
	Byte_raw(raw)[i] = Long_val(data);
	break;
    case MLTAG_short:
    case MLTAG_ushort:
	Short_raw(raw)[i] = Long_val(data);
	break;
    case MLTAG_int:
	Int_raw(raw)[i] = Long_val(data);
	break;
    case MLTAG_uint:
	Int_raw(raw)[i] = Long_val((unsigned long) data);
	break;
    case MLTAG_long:
	Long_raw(raw)[i] = Long_val(data);
	break;
    case MLTAG_ulong:
	Long_raw(raw)[i] = Long_val((unsigned long) data);
	break;
    }
    return Val_unit;
}

CAMLprim value ml_raw_write (value raw, value pos, value data)  /* ML */
{
    int s = Int_val(pos);
    int i, l = Wosize_val(data);

    check_size (raw,s+l-1,"Raw.write");
    if (s<0) invalid_argument("Raw.write");

    switch (Kind_raw(raw)) {
    case MLTAG_bitmap:
    case MLTAG_ubyte:
    case MLTAG_byte:
    {
	char *byte_raw = Byte_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *byte_raw++ = Long_val(Field(data,i));
	break;
    }
    case MLTAG_short:
    case MLTAG_ushort:
    {
	short *short_raw = Short_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *short_raw++ = Long_val(Field(data,i));
	break;
    }
    case MLTAG_int:
    {
	int *int_raw = Int_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *int_raw++ = Long_val(Field(data,i));
	break;
    }
    case MLTAG_uint:
    {
	int *int_raw = Int_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *int_raw++ = Long_val((unsigned long) Field(data,i));
	break;
    }
    case MLTAG_long:
    {
	long *long_raw = Long_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *long_raw++ = Long_val(Field(data,i));
	break;
    }
    case MLTAG_ulong:
    {
	long *long_raw = Long_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *long_raw++ = Long_val((unsigned long) Field(data,i));
	break;
    }
    }
    return Val_unit;
}

CAMLprim value ml_raw_get_float (value raw, value pos)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.get_float");
    if (Kind_raw(raw) == MLTAG_float)
	return copy_double ((double) Float_raw(raw)[i]);
    else
	return copy_double (Double_raw(raw)[i]);
}

CAMLprim value ml_raw_read_float (value raw, value pos, value len)  /* ML */
{
    int s = Int_val(pos);
    int i, l = Int_val(len);
    value ret = Val_unit;

    check_size (raw,s+l-1,"Raw.read_float");
    if (l<0 || s<0) invalid_argument("Raw.read_float");
    ret = alloc_shr (l*sizeof(double)/sizeof(value), Double_array_tag);
    if (Kind_raw(raw) == MLTAG_float) {
	float *float_raw = Float_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Store_double_field(ret, i, (double) *float_raw++);
    } else {
	double *double_raw = Double_raw(raw)+s;
	for (i = 0; i < l; i++)
	    Store_double_field(ret, i, *double_raw++);
    }
    return ret;
}

CAMLprim value ml_raw_set_float (value raw, value pos, value data)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.set_float");
    if (Kind_raw(raw) == MLTAG_float)
	Float_raw(raw)[i] = (float) Double_val(data);
    else
	Double_raw(raw)[i] = Double_val(data);
    return Val_unit;
}

CAMLprim value ml_raw_write_float (value raw, value pos, value data)  /* ML */
{
    int s = Int_val(pos);
    int i, l = Wosize_val(data)*sizeof(value)/sizeof(double);

    check_size (raw,s+l-1,"Raw.write_float");
    if (s<0) invalid_argument("Raw.write_float");
    if (Kind_raw(raw) == MLTAG_float) {
	float *float_raw = Float_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *float_raw++ = (float) Double_field(data,i);
    } else {
	double *double_raw = Double_raw(raw)+s;
	for (i = 0; i < l; i++)
	    *double_raw++ = Double_field(data,i);
    }
    return Val_unit;
}

#ifdef ARCH_BIG_ENDIAN
#define HI_OFFSET 1
#define LO_OFFSET 0
#else
#define HI_OFFSET 0
#define LO_OFFSET 1
#endif

/* Here we suppose that:
 *    sizeof(int)  == 2*sizeof(short)
 *    sizeof(long) == 2*sizeof(int)   (64-bit architectures)
 *    sizeof(long) == 2*sizeof(short) (otherwise)
 */

#define Hint_raw(raw) ((unsigned short *) Short_raw(raw))

#ifdef ARCH_SIXTYFOUR
#define Hlong_raw(raw) ((unsigned int *) Int_raw(raw))
#else
#define Hlong_raw(raw) ((unsigned short *) Short_raw(raw))
#endif

CAMLprim value ml_raw_get_hi (value raw, value pos)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.get_hi");
    switch (Kind_raw(raw)) {
    case MLTAG_int:
    case MLTAG_uint:
	return Val_long (Hint_raw(raw)[2*i+HI_OFFSET]);
    case MLTAG_long:
    case MLTAG_ulong:
	return Val_long (Hlong_raw(raw)[2*i+HI_OFFSET]);
    }
    return Val_unit;
}

CAMLprim value ml_raw_get_lo (value raw, value pos)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.get_lo");
    switch (Kind_raw(raw)) {
    case MLTAG_int:
    case MLTAG_uint:
	return Val_long ((unsigned long) Hint_raw(raw)[2*i+LO_OFFSET]);
    case MLTAG_long:
    case MLTAG_ulong:
	return Val_long ((unsigned long) Hlong_raw(raw)[2*i+LO_OFFSET]);
    }
    return Val_unit;
}

CAMLprim value ml_raw_set_hi (value raw, value pos, value data)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.set_hi");
    switch (Kind_raw(raw)) {
    case MLTAG_int:
    case MLTAG_uint:
	Hint_raw(raw)[2*i+HI_OFFSET] = Long_val(data);
	break;
    case MLTAG_long:
    case MLTAG_ulong:
	Hlong_raw(raw)[2*i+HI_OFFSET] = Long_val(data);
	break;
    }
    return Val_unit;
}

CAMLprim value ml_raw_set_lo (value raw, value pos, value data)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.set_lo");
    switch (Kind_raw(raw)) {
    case MLTAG_int:
    case MLTAG_uint:
	Hint_raw(raw)[2*i+LO_OFFSET] = Long_val(data);
	break;
    case MLTAG_long:
    case MLTAG_ulong:
	Hlong_raw(raw)[2*i+LO_OFFSET] = Long_val(data);
	break;
    }
    return Val_unit;
}

CAMLprim value ml_raw_get_long (value raw, value pos)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.get_long");
    switch (Kind_raw(raw)) {
    case MLTAG_int:
    case MLTAG_uint:
        return copy_nativeint (Int_raw(raw)[i]);
    case MLTAG_long:
    case MLTAG_ulong:
        return copy_nativeint (Long_raw(raw)[i]);
    }
    return Val_unit;
}

CAMLprim value ml_raw_set_long (value raw, value pos, value data)  /* ML */
{
    long i = Long_val(pos);

    check_size (raw,i,"Raw.set_long");
    switch (Kind_raw(raw)) {
    case MLTAG_int:
    case MLTAG_uint:
	Int_raw(raw)[i] = Nativeint_val(data);
	break;
    case MLTAG_long:
    case MLTAG_ulong:
	Long_raw(raw)[i] = Nativeint_val(data);
	break;
    }
    return Val_unit;
}

CAMLprim value ml_raw_alloc (value kind, value len)  /* ML */
{
    CAMLparam0();
    CAMLlocal1(data);
    value raw;
    int size = raw_sizeof(kind) * Int_val(len);
    int offset = 0;

    if (kind == MLTAG_double && sizeof(double) > sizeof(value)) {
	data = alloc_shr ((size-1)/sizeof(value)+2, Abstract_tag);
	offset = (data % sizeof(double) ? sizeof(value) : 0);
    } else data = alloc_shr ((size-1)/sizeof(value)+1, Abstract_tag);
    raw = alloc_small (SIZE_RAW,0);
    Kind_raw(raw) = kind;
    Size_raw(raw) = Val_int(size);
    Base_raw(raw) = data;
    Offset_raw(raw) = Val_int(offset);
    Static_raw(raw) = Val_false;
    CAMLreturn(raw);
}

CAMLprim value ml_raw_alloc_static (value kind, value len)  /* ML */
{
    value raw;
    void  *data;
    int size = raw_sizeof(kind) * Int_val(len);
    int offset = 0;

    if (kind == MLTAG_double && sizeof(double) > sizeof(long)) {
	data = stat_alloc (size+sizeof(long));
	offset = ((long)data % sizeof(double) ? sizeof(value) : 0);
    } else data = stat_alloc (size);
    raw = alloc_small (SIZE_RAW, 0);
    Kind_raw(raw) = kind;
    Size_raw(raw) = Val_int(size);
    Base_raw(raw) = (value) data;
    Offset_raw(raw) = Val_int(offset);
    Static_raw(raw) = Val_true;
    return raw;
}

CAMLprim value ml_raw_free_static (value raw)  /* ML */
{
    if (Static_raw(raw) != Val_int(1)) invalid_argument ("Raw.free_static");
    stat_free (Void_raw(raw));
    Base_raw(raw) = Val_unit;
    Size_raw(raw) = Val_unit;
    Offset_raw(raw) = Val_unit;
    Static_raw(raw) = Val_false;
    return Val_unit;
}
