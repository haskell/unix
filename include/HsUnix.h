/* -----------------------------------------------------------------------------
 * $Id: HsUnix.h,v 1.1 2002/09/12 16:38:22 simonmar Exp $
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions for package `unix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSUNIX_H
#define HSUNIX_H

#include "config.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

extern int execvpe(char *name, char **argv, char **envp);

#ifndef INLINE
#define INLINE extern inline
#endif

INLINE int __hsunix_wifexited   (int stat) { return WIFEXITED(stat); }
INLINE int __hsunix_wexitstatus (int stat) { return WEXITSTATUS(stat); }
INLINE int __hsunix_wifsignaled (int stat) { return WIFSIGNALED(stat); }
INLINE int __hsunix_wtermsig    (int stat) { return WTERMSIG(stat); }
INLINE int __hsunix_wifstopped  (int stat) { return WIFSTOPPED(stat); }
INLINE int __hsunix_wstopsig    (int stat) { return WSTOPSIG(stat); }

#endif
