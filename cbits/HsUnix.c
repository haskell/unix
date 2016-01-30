/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions for package `unix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#include "HsUnix.h"

#ifdef HAVE_RTLDNEXT
void *__hsunix_rtldNext (void) {return RTLD_NEXT;} 
#endif

#ifdef HAVE_RTLDDEFAULT
void *__hsunix_rtldDefault (void) {return RTLD_DEFAULT;}
#endif

#ifdef HAVE_PTSNAME
// On Linux (and others), <stdlib.h> needs to be included while
// `_XOPEN_SOURCE` is already defined. However, GHCs before GHC 8.0
// didn't do that yet for CApiFFI, so we need this workaround here.

char *__hsunix_ptsname(int fd)
{
    return ptsname(fd);
}

int __hsunix_grantpt(int fd)
{
    return grantpt(fd);
}

int __hsunix_unlockpt(int fd)
{
    return unlockpt(fd);
}
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

#ifdef HAVE_UNSETENV
int __hsunix_unsetenv(const char *name)
{
#ifdef UNSETENV_RETURNS_VOID
    unsetenv(name);
    return 0;
#else
    return unsetenv(name);
#endif
}
#endif

/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
HsInt __hsunix_long_path_size(void) {
#ifdef PATH_MAX
    return PATH_MAX;
#else
    return 4096;
#endif
}

