AC_INIT([Haskell unix package], [1.0], [libraries@haskell.org], [unix])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsUnix.h])

AC_CONFIG_HEADERS([include/HsUnixConfig.h])

AC_CHECK_FUNCS([getgrgid_r getgrnam_r getpwnam_r getpwuid_r getpwnam getpwuid])

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

dnl ---------- usleep ----------
dnl --- stolen from guile configure ---
dnl --- FIXME: /usr/include/unistd.h can't be right?

### On some systems usleep has no return value.  If it does have one,
### we'd like to return it; otherwise, we'll fake it.
AC_CACHE_CHECK([return type of usleep], cv_func_usleep_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+usleep>changequote([, ]),
                   /usr/include/unistd.h,
                   [cv_func_usleep_return_type=void],
                   [cv_func_usleep_return_type=int])])
case "$cv_func_usleep_return_type" in
  "void" )
    AC_DEFINE([USLEEP_RETURNS_VOID], [1], [Define if the system headers declare usleep to return void.])
  ;;
esac

AC_OUTPUT
