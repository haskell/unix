/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions for package `unix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#include "HsUnix.h"

char **__hsunix_get_environ (void) {return environ;}

#ifdef HAVE_RTLDNEXT
void *__hsunix_rtldNext (void) {return RTLD_NEXT;}
#endif

#ifdef HAVE_RTLDDEFAULT
void *__hsunix_rtldDefault (void) {return RTLD_DEFAULT;}
#endif

// push a SVR4 STREAMS module; do nothing if STREAMS not available
int __hsunix_push_module(int fd, const char *module)
{
#if defined(I_PUSH) && !defined(HAVE_DEV_PTC)
    return ioctl(fd, I_PUSH, module);
#else
    return 0;
#endif
}

#ifdef HAVE_CLOCKS_PER_SEC
clock_t __hsunix_clocks_per_second (void) {return CLOCKS_PER_SEC;}
#endif

char *__hscore_d_name( struct dirent* d )
{
  return (d->d_name);
}

char __hscore_d_type( struct dirent* d )
{
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
  return (d->d_type);
#else
  return CONST_DT_UNKNOWN;
#endif
}
