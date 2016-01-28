/* Provide ctermid() if it is missing (as on some Android versions). */

#include "HsUnixConfig.h"
#ifndef HAVE_CTERMID

#include "ghcplatform.h"

#if defined(linux_HOST_OS) || defined(linux_android_HOST_OS)
#include <string.h>

char *ctermid(char *s)
{
  static const char devtty[] = "/dev/tty";
  if (s)
    return strcpy(s, devtty);
  else
    return (char *)devtty;
}
#endif /* linux or linux_android */

#endif /* HAVE_CTERMID */
