#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>

#define stringify(x) #x
#define ver stringify (LLPP_VERSION)

CAMLprim value ml_llpp_version (void)
{
    return caml_copy_string (ver);
}
