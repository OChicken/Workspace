ROOT ?= ..
CFLAGS += \
	-I$(ROOT)/include \
	-I$(ROOT)/build

SOURCES = mylib.c
BUILDDIR = $(ROOT)/build

all-src: libmylib.so libmylib.a

libmylib.so: $(SOURCES)
	$(CC) $(CFLAGS) $(SOURCES) -fPIC -shared $(LDFLAGS) -o $(BUILDDIR)/$@

libmylib.a: $(SOURCES)
	$(CC) $(CFLAGS) -c $(SOURCES) -o mylib.o $(LDFLAGS)
	ar crv $(BUILDDIR)/$@ mylib.o $(LDFLAGS)
	rm mylib.o

include $(ROOT)/Makefile
