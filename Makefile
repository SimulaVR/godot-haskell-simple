LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-simple.so
PROJECTROOT = $(shell stack path --project-root)
all:
	stack clean godot-haskell-simple
	stack build
	cp $(LIBFILE) $(PROJECTROOT)
