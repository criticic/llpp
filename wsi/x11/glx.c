#include <GL/glx.h>

void (*wsigladdr (const char *name)) (void)
{
    return glXGetProcAddress (name);
}
