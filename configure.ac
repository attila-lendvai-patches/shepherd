dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.69)

dnl Initialization
AC_INIT([GNU Shepherd], [0.10.2], [bug-guix@gnu.org])
AC_CONFIG_SRCDIR([modules/shepherd.scm])
AC_CONFIG_AUX_DIR([build-aux])

dnl We're fine with GNU make constructs, hence '-Wno-portability'.
AM_INIT_AUTOMAKE([1.11 gnu silent-rules -Wall -Wno-portability
  std-options color-tests])

dnl Enable silent rules by default.
AM_SILENT_RULES([yes])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19])

AC_CANONICAL_HOST
AC_PROG_MKDIR_P
AC_PROG_SED

dnl The 'timeout' program, introduced in GNU Coreutils 7.0 (2008).
AC_PATH_PROG([TIMEOUT], [timeout], [not-found])
AM_CONDITIONAL([HAVE_TIMEOUT], [test "x$TIMEOUT" != "xnot-found"])
AC_SUBST([TIMEOUT])

dnl Check for Guile 2.2+.
m4_pattern_forbid([^GUILE_PKG$])
GUILE_PKG([3.0 2.2])

dnl Checks for programs.
GUILE_PROGS

guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

dnl Check for extra dependencies.
GUILE_MODULE_AVAILABLE([have_fibers], [(fibers)])
if test "x$have_fibers" != "xyes"; then
  AC_MSG_ERROR([Fibers is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([have_recent_fibers], [(fibers scheduler)])
if test "x$have_recent_fibers" != "xyes"; then
  AC_MSG_ERROR([Fibers appears to be too old; please install version 1.1.0 or later.])
fi

dnl Make sure Fibers does not create POSIX threads: since shepherd
dnl forks, it must be single-threaded.
AC_CACHE_CHECK([whether Fibers might create POSIX threads],
  [ac_cv_fibers_creates_pthreads],
  [GUILE_CHECK([retval],
    [(use-modules (fibers))
     (set! (@ (ice-9 threads) call-with-new-thread)
       (lambda _ (throw 'new-thread!)))
     (run-fibers (lambda () (spawn-fiber (lambda () 1)))
                 #:parallelism 1 #:hz 0)])
   if test "$retval" = 0; then
     ac_cv_fibers_creates_pthreads="no"
   else
     ac_cv_fibers_creates_pthreads="yes"
   fi])
if test "x$ac_cv_fibers_creates_pthreads" = "xyes"; then
  AC_MSG_ERROR([Fibers creates POSIX threads behind our back; aborting.])
fi

dnl Capture the location of Fibers' modules.
FIBERS_SOURCE_DIRECTORY="`\
  "$GUILE" -c '(display (dirname (search-path %load-path "fibers.scm")))'`"
FIBERS_OBJECT_DIRECTORY="`\
  "$GUILE" -c '(display (dirname (search-path %load-compiled-path "fibers.go")))'`"
AC_SUBST([FIBERS_SOURCE_DIRECTORY])
AC_SUBST([FIBERS_OBJECT_DIRECTORY])

dnl
dnl Low-level operating system interface.
dnl

AC_PROG_CC

dnl In practice that value is fixed for all platforms in glibc, but better
dnl check it.
AC_MSG_CHECKING([the '_SC_OPEN_MAX' value])
AC_COMPUTE_INT([_SC_OPEN_MAX], [_SC_OPEN_MAX], [#include <unistd.h>])
AC_MSG_RESULT([$_SC_OPEN_MAX])
AC_SUBST([_SC_OPEN_MAX])

dnl Guile 2.2.7 lacks a definition for O_CLOEXEC.
AC_MSG_CHECKING([the 'O_CLOEXEC' value])
AC_COMPUTE_INT([O_CLOEXEC], [O_CLOEXEC], [#include <fcntl.h>])
AC_MSG_RESULT([$O_CLOEXEC])
AC_SUBST([O_CLOEXEC])

AC_MSG_CHECKING([<sys/reboot.h> constants])
AC_COMPUTE_INT([RB_AUTOBOOT], [RB_AUTOBOOT], [#include <sys/reboot.h>])
case "$host_os" in
  linux-gnu*)
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_ENABLE_CAD], [RB_ENABLE_CAD], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_DISABLE_CAD], [RB_DISABLE_CAD], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_POWER_OFF], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_SW_SUSPEND], [RB_SW_SUSPEND], [#include <sys/reboot.h>])
    ;;
  gnu*)
    # On GNU/Hurd, the Mach-derived reboot.h uses different names, and
    # has a different set of features and flags.
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_HALT], [#include <sys/reboot.h>])
    RB_DISABLE_CAD="#f"
    ;;
  *)
    # What is this?  GNU/kFreeBSD?
    AC_MSG_WARN([not sure how to halt the system on '$host_os'])
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    RB_DISABLE_CAD="#f"
    ;;
esac

AC_SUBST([RB_DISABLE_CAD])
AC_SUBST([RB_AUTOBOOT])
AC_SUBST([RB_HALT_SYSTEM])
AC_SUBST([RB_POWER_OFF])
AC_MSG_RESULT([done])

AC_MSG_CHECKING([<sys/prctl.h> constants])
AC_COMPUTE_INT([PR_SET_CHILD_SUBREAPER], [PR_SET_CHILD_SUBREAPER], [#include <sys/prctl.h>])
AC_SUBST([PR_SET_CHILD_SUBREAPER])
AC_MSG_RESULT([done])

dnl Check the size of 'signalfd_siginfo'.  If it's undefined, returns zero.
AC_CHECK_SIZEOF([struct signalfd_siginfo], [], [#include <sys/signalfd.h>])
AC_CHECK_SIZEOF([sigset_t], [], [#include <signal.h>])

AC_MSG_CHECKING([<sys/signalfd.h> and <sys/signal.h> constants])
AC_COMPUTE_INT([SFD_CLOEXEC], [SFD_CLOEXEC], [#include <sys/signalfd.h>])
AC_COMPUTE_INT([SFD_NONBLOCK], [SFD_NONBLOCK], [#include <sys/signalfd.h>])
AC_COMPUTE_INT([SIG_BLOCK], [SIG_BLOCK], [#include <sys/signal.h>])
AC_COMPUTE_INT([SIG_UNBLOCK], [SIG_UNBLOCK], [#include <sys/signal.h>])
AC_COMPUTE_INT([SIG_SETMASK], [SIG_SETMASK], [#include <sys/signal.h>])
AC_MSG_RESULT([done])

dnl These constants are undefined on non-Linux systems; set them to zero
dnl instead of the empty string.
if test -z "$SFD_CLOEXEC"; then
  SFD_CLOEXEC=0
fi
if test -z "$SFD_NONBLOCK"; then
  SFD_NONBLOCK=0
fi

SIZEOF_STRUCT_SIGNALFD_SIGINFO="$ac_cv_sizeof_struct_signalfd_siginfo"
SIZEOF_SIGSET_T="$ac_cv_sizeof_sigset_t"
AC_SUBST([SIZEOF_STRUCT_SIGNALFD_SIGINFO])
AC_SUBST([SIZEOF_SIGSET_T])
AC_SUBST([SFD_CLOEXEC])
AC_SUBST([SFD_NONBLOCK])
AC_SUBST([SIG_BLOCK])
AC_SUBST([SIG_UNBLOCK])
AC_SUBST([SIG_SETMASK])

dnl Check for constants not exported by Guile as of 3.0.8.
AC_MSG_CHECKING([<netinet/in.h> constants])
AC_COMPUTE_INT([IPPROTO_IPV6], [IPPROTO_IPV6], [
  #include <sys/socket.h>
  #include <netinet/in.h>])
AC_COMPUTE_INT([IPV6_V6ONLY], [IPV6_V6ONLY], [
  #include <sys/socket.h>
  #include <netinet/in.h>])
AC_MSG_RESULT([done])
AC_SUBST([IPPROTO_IPV6])
AC_SUBST([IPV6_V6ONLY])

AC_MSG_CHECKING([whether to build crash handler])
case "$host_os" in
  linux-gnu*)  build_crash_handler=yes;;
  *)           build_crash_handler=no;;
esac
AC_MSG_RESULT([$build_crash_handler])
AM_CONDITIONAL([BUILD_CRASH_HANDLER], [test "x$build_crash_handler" = "xyes"])

dnl Bash completion file.

AC_ARG_WITH([bash-completion-dir],
  AS_HELP_STRING([--with-bash-completion-dir=DIR],
    [name of the Bash completion directory]),
  [bashcompletiondir="$withval"],
  [bashcompletiondir='${sysconfdir}/bash_completion.d'])
AC_SUBST([bashcompletiondir])

dnl Manual pages.
AM_MISSING_PROG([HELP2MAN], [help2man])

dnl Finish.
AC_CONFIG_FILES([Makefile po/Makefile.in
                 modules/shepherd/system.scm])

AC_OUTPUT
