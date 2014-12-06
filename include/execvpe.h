/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in cbits/execvpe.c
   ------------------------------------------------------------------------- */

extern int
__hsunix_execvpe(const char *name, char *const argv[], char *const envp[]);

// implemented in cbits/ghcrts.c
extern void pPrPr_disableITimers (void);
