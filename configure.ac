AC_INIT([Haskell unix package], [1.0], [libraries@haskell.org], [unix])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsUnix.h])

AC_CONFIG_HEADERS([include/HsUnixConfig.h])

AC_MSG_CHECKING([for _SC_GETGR_R_SIZE_MAX])
AC_EGREP_CPP(we_have_that_sysconf_thing,
[
#include <unistd.h>
#ifdef _SC_GETGR_R_SIZE_MAX
we_have_that_sysconf_thing
#endif
],
[AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_SC_GETGR_R_SIZE_MAX], [1], [Define to 1 if <unistd.h> defines _SC_GETGR_R_SIZE_MAX.])],
[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for _SC_GETPW_R_SIZE_MAX])
AC_EGREP_CPP(we_have_that_sysconf_thing,
[
#include <unistd.h>
#ifdef _SC_GETPW_R_SIZE_MAX
we_have_that_sysconf_thing
#endif
],
[AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_SC_GETPW_R_SIZE_MAX], [1], [Define to 1 if <unistd.h> defines _SC_GETPW_R_SIZE_MAX.])],
[AC_MSG_RESULT([no])])

AC_OUTPUT
