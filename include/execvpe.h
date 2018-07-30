/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in cbits/execvpe.c
   ------------------------------------------------------------------------- */

#ifndef HSUNIX_EXECVPE_H
#define HSUNIX_EXECVPE_H

extern int
__hsunix_execvpe(const char *name, char *const argv[], char *const envp[]);

#endif
