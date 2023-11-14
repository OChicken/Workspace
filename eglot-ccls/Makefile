CC		 ?= /usr/bin/gcc
CFLAGS += -Wall -Wextra -Wpedantic -Wredundant-decls -Wshadow -Wpointer-arith -Og -fomit-frame-pointer
LDFLAGS += -lgcrypt

all:
	cd src && $(MAKE) || exit 1
	cd tests && $(MAKE) || exit 1

clean:
	cd build && $(MAKE) || exit 1
