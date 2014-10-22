/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in execvpe.c
   ------------------------------------------------------------------------- */

#include "HsUnixConfig.h"
// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#include <errno.h>
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
#ifndef __QNXNTO__
#ifdef __OpenBSD__
extern int execvpe(const char *, char *const *, char *const *);
#else
extern int execvpe(char *name, char *const argv[], char **envp);
#endif
#endif
extern void pPrPr_disableITimers (void);
#endif

