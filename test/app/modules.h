// List of test modules to register

#ifdef ARCH_HOST
#define ARCH_TEST_MAP(XX) XX(basic)
#else
#define ARCH_TEST_MAP(XX)
#endif

#define TEST_MAP(XX)                                                                                                   \
	ARCH_TEST_MAP(XX)                                                                                                  \
	XX(sdcard)
