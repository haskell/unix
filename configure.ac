AC_INIT([Haskell unix package], [2.0], [libraries@haskell.org], [unix])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsUnix.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_CONFIG_HEADERS([include/HsUnixConfig.h])

# Is this a Unix system?
AC_CHECK_HEADER([dlfcn.h], [BUILD_PACKAGE_BOOL=True], [BUILD_PACKAGE_BOOL=False])
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_C_CONST

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

AC_CHECK_HEADERS([dirent.h fcntl.h grp.h limits.h pwd.h signal.h string.h])
AC_CHECK_HEADERS([sys/resource.h sys/stat.h sys/times.h sys/time.h])
AC_CHECK_HEADERS([sys/utsname.h sys/wait.h])
AC_CHECK_HEADERS([bsd/libutil.h libutil.h pty.h utmp.h])
AC_CHECK_HEADERS([termios.h time.h unistd.h utime.h])

AC_CHECK_FUNCS([getgrgid_r getgrnam_r getpwnam_r getpwuid_r getpwnam getpwuid])
AC_CHECK_FUNCS([getpwent getgrent])
AC_CHECK_FUNCS([lchown setenv sysconf unsetenv])
AC_CHECK_FUNCS([nanosleep])
AC_CHECK_FUNCS([ptsname])
AC_CHECK_FUNCS([setitimer])
AC_CHECK_FUNCS([readdir_r])

# Avoid adding rt if absent or unneeded
AC_CHECK_LIB(rt, shm_open, [EXTRA_LIBS="$EXTRA_LIBS rt" CFLAGS="$CFLAGS -lrt"])

# needs -lrt on linux
AC_CHECK_FUNCS([shm_open shm_unlink])

FP_CHECK_CONSTS([SIGABRT SIGALRM SIGBUS SIGCHLD SIGCONT SIGFPE SIGHUP SIGILL SIGINT SIGKILL SIGPIPE SIGQUIT SIGSEGV SIGSTOP SIGTERM SIGTSTP SIGTTIN SIGTTOU SIGUSR1 SIGUSR2 SIGPOLL SIGPROF SIGSYS SIGTRAP SIGURG SIGVTALRM SIGXCPU SIGXFSZ SIG_BLOCK SIG_SETMASK SIG_UNBLOCK], [
#if HAVE_SIGNAL_H
#include <signal.h>
#endif])

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

### On some systems usleep has no return value.  If it does have one,
### we'd like to return it; otherwise, we'll fake it.
AC_CACHE_CHECK([return type of usleep], fptools_cv_func_usleep_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+usleep>changequote([, ]),
                   unistd.h,
                   [fptools_cv_func_usleep_return_type=void],
                   [fptools_cv_func_usleep_return_type=int])])
case "$fptools_cv_func_usleep_return_type" in
  "void" )
    AC_DEFINE([USLEEP_RETURNS_VOID], [1], [Define if the system headers declare usleep to return void.])
  ;;
esac

###  POSIX.1003.1 unsetenv returns 0 or -1 (EINVAL), but older implementations
###  in common use return void.
AC_CACHE_CHECK([return type of unsetenv], fptools_cv_func_unsetenv_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+unsetenv>changequote([, ]),
                   stdlib.h,
                   [fptools_cv_func_unsetenv_return_type=void],
                   [fptools_cv_func_unsetenv_return_type=int])])
case "$fptools_cv_func_unsetenv_return_type" in
  "void" )
    AC_DEFINE([UNSETENV_RETURNS_VOID], [1], [Define if stdlib.h declares unsetenv to return void.])
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

AC_CHECK_FUNCS(openpty,,
   AC_CHECK_LIB(util,openpty,
     [AC_DEFINE(HAVE_OPENPTY) EXTRA_LIBS="$EXTRA_LIBS util"],
     AC_CHECK_LIB(bsd,openpty, [AC_DEFINE(HAVE_OPENPTY) EXTRA_LIBS="$EXTRA_LIBS bsd"])
   )
)

AC_MSG_CHECKING(for /dev/ptmx)
if test -r /dev/ptmx
then
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_DEV_PTMX, 1,
  [Define if we have /dev/ptmx.])
else
  AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING(for /dev/ptc)
if test -r /dev/ptc
then
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_DEV_PTC, 1,
  [Define if we have /dev/ptc.])
else
  AC_MSG_RESULT(no)
fi

# Avoid adding dl if absent or unneeded
AC_CHECK_LIB(dl, dlopen, [EXTRA_LIBS="$EXTRA_LIBS dl"])
AC_SUBST([EXTRA_LIBS])

# -{l,}pthread goo
AC_CANONICAL_TARGET

AC_SEARCH_LIBS(sem_close, pthread,
    [EXTRA_LIBS="$EXTRA_LIBS $ac_lib"],
    [AC_MSG_NOTICE([Not found])])

AC_CONFIG_FILES([unix.buildinfo])

AC_OUTPUT
