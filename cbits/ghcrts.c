#include "execvpe.h"

#ifdef __GLASGOW_HASKELL__
// for 'void StopTimer(void)' prototype
# include "Rts.h"
#endif

/* Copied verbatim from ghc/lib/std/cbits/system.c. */
void pPrPr_disableITimers (void)
{
#ifdef __GLASGOW_HASKELL__
    stopTimer();
#endif
}
