COMPONENT_DEPENDS := Storage FatIFS
HWCONFIG := basic-$(if $(findstring Esp32,$(SMING_ARCH)),esp32,std)
DISABLE_NETWORK := 1
