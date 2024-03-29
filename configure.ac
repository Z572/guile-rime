
dnl -*- Autoconf -*-

AC_INIT(guile-rime, 0.1)
AC_SUBST(HVERSION, "\"0.1\"")
AC_SUBST(AUTHOR, "\"Zheng Junjie\"")
AC_SUBST(COPYRIGHT, "'(2021)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(rime)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([rime/configuration.scm])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

m4_pattern_forbid([PKG_CHECK_MODULES])
PKG_CHECK_MODULES([RIME], [rime >= 1.6.0])
PKG_CHECK_VAR([RIME_LIBDIR], [rime],[libdir])

PKG_CHECK_MODULES([RIME_1_8],
    [rime >= 1.8.0],
    [RIME_1_8_P="#true"],
    [RIME_1_8_P="#false"])
AC_SUBST([RIME_1_8_P])
AS_IF([test "x$RIME_LIBDIR" = "x"], [
  AC_MSG_FAILURE([Unable to identify rime lib path.])
])
AC_SUBST([RIME_LIBDIR])
AC_ARG_WITH([rime-data-dir],
    [AS_HELP_STRING([--with-rime-data-dir=DIR],
      [name of the default Rime data directory])],
    [RIME_DATA="$withval"],
    [RIME_DATA="/usr/share/rime-data"])

AC_MSG_CHECKING([Rime data directory])
AC_MSG_RESULT([$RIME_DATA])
AC_SUBST([RIME_DATA])

AC_ARG_ENABLE([debug-mode],
    [AS_HELP_STRING([--enable-debug-mode],
        [enable debug mode])],
        [DEBUG_MODE="#true"],
        [DEBUG_MODE="#false"])
AC_SUBST([DEBUG_MODE])
dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi
GUILE_MODULE_REQUIRED([bytestructures guile])
GUILE_MODULE_REQUIRED([ncurses curses])
GUILE_MODULE_REQUIRED([ncurses menu])
dnl Hall auto-generated guile-module dependencies
if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
