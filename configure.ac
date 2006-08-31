AC_INIT([Haskell unix package], [2.0], [libraries@haskell.org], [unix])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsUnix.h])

AC_CONFIG_HEADERS([include/HsUnixConfig.h])

# Is this a Unix system?
AC_CHECK_HEADER([dlfcn.h], [BUILD_PACKAGE_BOOL=True], [BUILD_PACKAGE_BOOL=False])
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_C_CONST

AC_CHECK_HEADERS([dirent.h fcntl.h grp.h limits.h pwd.h signal.h string.h])
AC_CHECK_HEADERS([sys/resource.h sys/stat.h sys/times.h sys/time.h])
AC_CHECK_HEADERS([sys/utsname.h sys/wait.h])
AC_CHECK_HEADERS([termios.h time.h unistd.h utime.h])

AC_CHECK_FUNCS([getgrgid_r getgrnam_r getpwnam_r getpwuid_r getpwnam getpwuid])
AC_CHECK_FUNCS([getpwent getgrent])
AC_CHECK_FUNCS([lchown setenv sysconf unsetenv])

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

dnl ** sometimes RTLD_NEXT is hidden in #ifdefs we really don't wan to set
AC_MSG_CHECKING(for RTLD_NEXT from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_NEXT
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDNEXT], [1], [Define to 1 if we can see RTLD_NEXT in dlfcn.h.])
], [
  AC_MSG_RESULT(no)
  ])    

dnl ** RTLD_DEFAULT isn't available on cygwin
AC_MSG_CHECKING(for RTLD_DEFAULT from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_DEFAULT
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDDEFAULT], [1], [Define to 1 if RTLD_DEFAULT is available.])
], [
  AC_MSG_RESULT(no)
  ])    

dnl ** RTLD_LOCAL isn't available on cygwin or openbsd
AC_MSG_CHECKING(for RTLD_LOCAL from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_LOCAL
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDLOCAL], [1], [Define to 1 if RTLD_LOCAL is available.])
], [
  AC_MSG_RESULT(no)
  ])    

dnl ** RTLD_GLOBAL isn't available on openbsd
AC_MSG_CHECKING(for RTLD_GLOBAL from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_GLOBAL
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDGLOBAL], [1], [Define to 1 if RTLD_GLOBAL is available.])
], [
  AC_MSG_RESULT(no)
  ])    

dnl ** RTLD_NOW isn't available on openbsd
AC_MSG_CHECKING(for RTLD_NOW from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_NOW
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDNOW], [1], [Define to 1 if we can see RTLD_NOW in dlfcn.h])
], [
  AC_MSG_RESULT(no)
  ])    

# Avoid adding dl if absent or unneeded
AC_CHECK_LIB(dl, dlopen, [EXTRA_LIBS=dl], [EXTRA_LIBS=])
AC_SUBST([EXTRA_LIBS])

AC_CONFIG_FILES([unix.buildinfo])

AC_OUTPUT
