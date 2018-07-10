#include "HsFFI.h"

static void simple_init() __attribute__((constructor));
static void simple_init() {
  static char *argv[] = { "libGodotHaskellPlugin.so", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}

static void simple_fini() __attribute__((destructor));
static void simple_fini() {
  hs_exit();
}
