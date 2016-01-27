/* Provide tcdrain() if it is missing (as on some Android versions). */

#include "HsUnixConfig.h"
#ifndef HAVE_TCDRAIN

#include "ghcplatform.h"

#ifdef linux_android_HOST_OS
#include <sys/ioctl.h>

int tcdrain(int fd)
{
  /* POSIX says tcdrain is a cancellation point, so we should
     technically set the cancelability type to asynchronous here. But
     Android doesn't implement pthread_cancel anyways, so meh. */

  return ioctl(fd, TCSBRK, 1);
}
#endif /* linux_android_HOST_OS */

#endif /* HAVE_TCDRAIN */
