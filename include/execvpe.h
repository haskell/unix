/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in cbits/execvpe.c
   ------------------------------------------------------------------------- */

#ifndef HSUNIX_EXECVPE_H
#define HSUNIX_EXECVPE_H

#include "HsUnixConfig.h"

extern int
__hsunix_execvpe(const char *name, char *const argv[], char *const envp[]);

// this hack is needed for `process`; to be removed in unix-2.8
#if HAVE_EXECVPE
# define _GNU_SOURCE
# include <unistd.h>
#else
# define execvpe(name,argv,envp) __hsunix_execvpe(name,argv,envp)
#endif

// implemented in cbits/ghcrts.c
extern void pPrPr_disableITimers (void);

#endif
