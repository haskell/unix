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

/*
 * GNU glibc 2.23 and later deprecate `readdir_r` in favour of plain old
 * `readdir` which in some upcoming POSIX standard is going to required to be
 * re-entrant.
 * Eventually we want to drop `readder_r` all together, but want to be
 * compatible with older unixen which may not have a re-entrant `readdir`.
 * Solution is to make systems with *known* re-entrant `readir` use that and use
 * `readdir_r` wherever we have it and don't *know* that `readdir` is
 * re-entrant.
 */

#if defined (__GLIBC__) && ((__GLIBC__ > 2) || (__GLIBC__ == 2) && (__GLIBC_MINOR__ >= 23))
#define USE_READDIR_R 0
#else
#define USE_READDIR_R 1
#endif

/*
 * read an entry from the directory stream; opt for the
 * re-entrant friendly way of doing this, if available.
 */
int __hscore_readdir( DIR *dirPtr, struct dirent **pDirEnt )
{
#if HAVE_READDIR_R && USE_READDIR_R
  struct dirent* p;
  int res;
  static unsigned int nm_max = (unsigned int)-1;

  if (pDirEnt == NULL) {
    return -1;
  }
  if (nm_max == (unsigned int)-1) {
#ifdef NAME_MAX
    nm_max = NAME_MAX + 1;
#else
    nm_max = pathconf(".", _PC_NAME_MAX);
    if (nm_max == -1) { nm_max = 255; }
    nm_max++;
#endif
  }
  p = (struct dirent*)malloc(sizeof(struct dirent) + nm_max);
  if (p == NULL) return -1;
  res = readdir_r(dirPtr, p, pDirEnt);
  if (res != 0) {
      *pDirEnt = NULL;
      free(p);
  }
  else if (*pDirEnt == NULL) {
    // end of stream
    free(p);
  }
  return res;
#else

  if (pDirEnt == NULL) {
    return -1;
  }

  *pDirEnt = readdir(dirPtr);
  if (*pDirEnt == NULL) {
    return -1;
  } else {
    return 0;
  }
#endif
}

char *__hscore_d_name( struct dirent* d )
{
  return (d->d_name);
}

void __hscore_free_dirent(struct dirent *dEnt)
{
#if HAVE_READDIR_R && USE_READDIR_R
  free(dEnt);
#endif
}
