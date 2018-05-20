#define stringify1(x) #x
#define stringify(x) stringify1 (x)
const char llpp_version[] = stringify (LLPP_VERSION);
