
R_INCLUDE ?= $(shell Rscript -e 'cat(R.home("include"))')
R_LIB ?= $(shell Rscript -e 'cat(R.home("lib"))')
R_CFLAGS ?= $(shell R CMD config CFLAGS)
PKG_CFLAGS = -I$(R_INCLUDE) $(R_CFLAGS)
PKG_LIBS = -L$(R_LIB) -lR -lpthread -lblas

SRCDIR = $(dir $(firstword $(MAKEFILE_LIST)))
SRC = $(addprefix $(SRCDIR), tasks.c register.c)
OUT = $(SRCDIR)bench.so

all: $(OUT)

$(OUT): $(SRC)
	$(CC) $(PKG_CFLAGS) -fPIC -shared -o $@ $(SRC) $(PKG_LIBS)

clean:
	rm -f $(OUT)
