/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions for package `unix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSUNIX_H
#define HSUNIX_H

#include "HsUnixConfig.h"

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#ifdef solaris2_HOST_OS
#define _POSIX_PTHREAD_SEMANTICS
#endif

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_UTIME_H
#include <utime.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#include <dlfcn.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

extern char **environ;

#ifndef INLINE
# if defined(__GNUC__)
#  define INLINE extern inline
# else
#  define INLINE inline
# endif
#endif

INLINE int __hsunix_wifexited   (int stat) { return WIFEXITED(stat); }
INLINE int __hsunix_wexitstatus (int stat) { return WEXITSTATUS(stat); }
INLINE int __hsunix_wifsignaled (int stat) { return WIFSIGNALED(stat); }
INLINE int __hsunix_wtermsig    (int stat) { return WTERMSIG(stat); }
INLINE int __hsunix_wifstopped  (int stat) { return WIFSTOPPED(stat); }
INLINE int __hsunix_wstopsig    (int stat) { return WSTOPSIG(stat); }

#ifdef HAVE_RTLDNEXT
INLINE void *__hsunix_rtldNext (void) {return RTLD_NEXT;} 
#endif

#ifdef HAVE_RTLDDEFAULT
INLINE void *__hsunix_rtldDefault (void) {return RTLD_DEFAULT;} 
#endif

/* O_SYNC doesn't exist on Mac OS X and (at least some versions of) FreeBSD,
fall back to O_FSYNC, which should be the same */
#ifndef O_SYNC
#define O_SYNC O_FSYNC
#endif

#ifdef SIGINFO
INLINE int __hsunix_SIGINFO()	{ return SIGINFO; }
#endif
#ifdef SIGWINCH
INLINE int __hsunix_SIGWINCH()	{ return SIGWINCH; }
#endif

#endif
