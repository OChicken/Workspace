ROOT ?= ..
CFLAGS += \
	-I$(ROOT)/include \
	-I$(ROOT)/build

SOURCES = main.c
#LDFLAGS += -L$(ROOT)/build -lmylib  # The linker uses *.so by default
LDFLAGS += $(ROOT)/build/libmylib.a  # So force it to use *.a in this way
BUILDDIR = $(ROOT)/build

hello: $(SOURCES)
	$(CC) $(CFLAGS) $(SOURCES) -o $(BUILDDIR)/$@ $(LDFLAGS)

include $(ROOT)/Makefile
