// List of test modules to register

#ifdef ARCH_HOST
#define TEST_MAP(XX) XX(basic)
#else
#define TEST_MAP(XX) XX(sdcard)
#endif
