/* $Id: ml_raw.h,v 1.3 1999-04-14 14:05:52 garrigue Exp $ */

#ifndef _ml_raw_
#define _ml_raw_

#define SIZE_RAW 5
#define Kind_raw(raw) (Field(raw,0))
#define Base_raw(raw) (Field(raw,1))
#define Offset_raw(raw) (Field(raw,2))
#define Size_raw(raw) (Field(raw,3))
#define Static_raw(raw) (Field(raw,4))

#define Addr_raw(raw) (Base_raw(raw)+Long_val(Offset_raw(raw)))

#define Void_raw(raw) ((void *) Addr_raw(raw))
#define Byte_raw(raw) ((char *) Addr_raw(raw))
#define Short_raw(raw) ((short *) Addr_raw(raw))
#define Int_raw(raw) ((int *) Addr_raw(raw))
#define Long_raw(raw) ((long *) Addr_raw(raw))
#define Float_raw(raw) ((float *) Addr_raw(raw))
#define Double_raw(raw) ((double *) Addr_raw(raw))

#endif
