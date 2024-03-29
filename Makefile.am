# Makefile.am -- How to build and install the Shepherd.
# Copyright © 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
# Copyright © 2013-2016, 2018-2020, 2022-2023 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
# Copyright © 2023 Oleg Pykhalov <go.wigust@gmail.com>
#
# This file is part of the GNU Shepherd.
#
# The GNU Shepherd is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# The GNU Shepherd is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.

# The main programs.
bin_SCRIPTS = shepherd herd
sbin_SCRIPTS = halt reboot
templates =					\
  shepherd.in herd.in halt.in reboot.in		\
  modules/shepherd/config.scm.in		\
  modules/shepherd/system.scm.in

# Build this module first.
BUILT_SOURCES = modules/shepherd/config.scm

# The source files.
# XXX: we can't use 'nobase_' because of the modules/ subdirectory.
dist_guilemodule_DATA =				\
  modules/shepherd.scm
nodist_guileobject_DATA = $(dist_guilemodule_DATA:%.scm=%.go)

shepherdsubdir = $(guilemoduledir)/shepherd
servicesubdir = $(guilemoduledir)/shepherd/service
dist_shepherdsub_DATA =				\
  modules/shepherd/args.scm			\
  modules/shepherd/colors.scm			\
  modules/shepherd/service.scm			\
  modules/shepherd/support.scm			\
  modules/shepherd/comm.scm
nodist_shepherdsub_DATA =			\
  modules/shepherd/config.scm			\
  modules/shepherd/system.scm
dist_servicesub_DATA =				\
  modules/shepherd/service/monitoring.scm	\
  modules/shepherd/service/repl.scm

shepherdgosubdir = $(guileobjectdir)/shepherd
servicegosubdir = $(guileobjectdir)/shepherd/service
nodist_shepherdgosub_DATA =			\
  modules/shepherd/config.go			\
  modules/shepherd/system.go			\
  $(dist_shepherdsub_DATA:%.scm=%.go)
nodist_servicegosub_DATA =			\
  $(dist_servicesub_DATA:%.scm=%.go)

scriptsdir = $(shepherdsubdir)/scripts
dist_scripts_DATA =				\
  modules/shepherd/scripts/halt.scm		\
  modules/shepherd/scripts/herd.scm		\
  modules/shepherd/scripts/reboot.scm

scriptsgodir = $(shepherdgosubdir)/scripts
nodist_scriptsgo_DATA =				\
  $(dist_scripts_DATA:%.scm=%.go)

CLEANFILES =						\
  $(nodist_guilemodule_DATA) $(nodist_shepherdsub_DATA)	\
  $(nodist_guileobject_DATA)				\
  $(nodist_shepherdgosub_DATA)				\
  $(nodist_servicegosub_DATA)				\
  $(nodist_scriptsgo_DATA)				\
  $(bin_SCRIPTS) $(sbin_SCRIPTS)


# Crash handler.

EXTRA_DIST =					\
  etc/crash-handler.c				\
  guix.scm					\
  .guix/modules/shepherd-package.scm		\
  .guix/manifest.scm

if BUILD_CRASH_HANDLER

# Build the crash handler "manually" rather than with Libtool.

all-local: etc/crash-handler.so
install-crash-handler:
	$(MKDIR_P) $(DESTDIR)$(pkglibdir)
	$(INSTALL) -m 755 etc/crash-handler.so \
	           $(DESTDIR)$(pkglibdir)/crash-handler.so

etc/crash-handler.so: etc/crash-handler.c
	$(AM_V_GEN)$(MKDIR_P) etc;			\
	$(CC) -O2 -g -Wall -shared -fPIC -o "$@" "$^"

CLEANFILES += etc/crash-handler.so

else !BUILD_CRASH_HANDLER

install-crash-handler:

endif !BUILD_CRASH_HANDLER

.PHONY: install-crash-handler


# Documentation.
info_TEXINFOS = doc/shepherd.texi
doc_shepherd_TEXINFOS = doc/fdl-1.3.texi

AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;

HELP2MANFLAGS = --source=GNU --info-page=$(PACKAGE_TARNAME)
gen_man = $(AM_V_HELP2MAN)LANGUAGE= $(HELP2MAN) $(HELP2MANFLAGS)

doc/shepherd.1: modules/shepherd.scm configure.ac
	$(gen_man) --output="$@" "$(top_builddir)/`basename "$@" .1`"

doc/%.1: modules/shepherd/scripts/%.scm configure.ac
	$(gen_man) --output="$@" "$(top_builddir)/`basename "$@" .1`"

doc/%.8: modules/shepherd/scripts/%.scm configure.ac
	$(gen_man) --section=8 --output="$@"	\
	  "$(top_builddir)/`basename "$@" .8`"

dist_man1_MANS = doc/shepherd.1 doc/herd.1
dist_man8_MANS = doc/halt.8 doc/reboot.8

# Bash completion file.
dist_bashcompletion_DATA = etc/completion/bash/herd

# Things not automatically included in the distribution.
EXTRA_DIST +=					\
  build-aux/config.rpath			\
  ChangeLog-2003				\
  $(templates)					\
  doc/examples/README				\
  doc/examples/wolfgangj.scm

# Create the socket directory (aka. 'default-socket-dir'.)  Make it
# accessible only by its owner since otherwise 'shepherd' suggests using
# '--insecure'.  This is just a convenience, so allow it to fail.
install-data-local:
	-$(MKDIR_P) $(DESTDIR)$(localstatedir)/run/shepherd
	-chmod 700 $(DESTDIR)$(localstatedir)/run/shepherd

# Relocate the script---i.e., have them refer to the installed module
# directory.
install-exec-hook: install-executable-symlinks install-crash-handler
	for script in							\
	   $(bin_SCRIPTS:%=$(DESTDIR)$(bindir)/%)			\
	   $(sbin_SCRIPTS:%=$(DESTDIR)$(sbindir)/%) ;			\
	do								\
	  $(SED) -e's|$(abs_top_srcdir)/modules\(.*\)%load-path|$(guilemoduledir)\1%load-path|g' \
	         -e's|$(abs_top_builddir)/modules\(.*\)%load-compiled-path|$(guileobjectdir)\1%load-compiled-path|g' \
	         -i $$script ;						\
	done

# Download PO files (translations).
download-po-files:
	for lang in `grep -v '^[[:blank:]]*#' < "$(top_srcdir)/po/LINGUAS"`; do	\
	  if wget -nv -O "$(top_srcdir)/po/$$lang.po.tmp"			\
	    "https://translationproject.org/latest/shepherd/$$lang.po" ;	\
	  then									\
	    mv "$(top_srcdir)/po/$$lang.po"{.tmp,} ;				\
	  else									\
	    rm "$(top_srcdir)/po/$$lang.po.tmp"	;				\
	  fi ;									\
	done

.PHONY: download-po-files

# Install 'shutdown' as a symlink to 'halt; XXX: someday we'll do
# better.
install-executable-symlinks:
	cd $(DESTDIR)$(sbindir); ln -s halt shutdown

# Remove the 'shutdown' symlink and 'crash-handler.so'.
uninstall-hook:
	cd $(DESTDIR)$(sbindir); rm -f shutdown
	cd $(DESTDIR)$(pkglibdir); rm -f crash-handler.so

# 'sed' expression to instantiate templates.
instantiate =						\
  -e 's,%PREFIX%,${prefix},g'				\
  -e 's,%modsrcdir%,${abs_top_srcdir}/modules,g'	\
  -e 's,%modbuilddir%,${abs_top_builddir}/modules,g'	\
  -e 's,%localstatedir%,${localstatedir},g'		\
  -e 's,%runstatedir%,${runstatedir},g'			\
  -e 's,%pkglibdir%,${pkglibdir},g'			\
  -e 's,%sysconfdir%,${sysconfdir},g'			\
  -e 's,%localedir%,${localedir},g'			\
  -e 's,%VERSION%,@VERSION@,g'				\
  -e 's,%PACKAGE_BUGREPORT%,@PACKAGE_BUGREPORT@,g'	\
  -e 's,%PACKAGE_NAME%,@PACKAGE_NAME@,g'		\
  -e 's,%PACKAGE_URL%,@PACKAGE_URL@,g'			\
  -e 's,%FIBERS_SOURCE_DIRECTORY%,@FIBERS_SOURCE_DIRECTORY@,g' \
  -e 's,%FIBERS_OBJECT_DIRECTORY%,@FIBERS_OBJECT_DIRECTORY@,g' \
  -e 's,%GUILE%,$(GUILE),g'

# Instantiate templates.

# Emit a 'chmod +x' command only if $@ matches one of the scripts.
maybe_executable =					\
   $(subst $@,chmod +x $@,				\
      $(findstring $@,$(bin_SCRIPTS) $(sbin_SCRIPTS)))

%: %.in Makefile
	$(AM_V_GEN)$(MKDIR_P) "`dirname $@`" ;		\
	$(SED) $(instantiate) < $< >$@ ;		\
	$(maybe_executable)

modules/shepherd/config.scm: modules/shepherd/config.scm.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC  " $@;

# Make sure 'modules/shepherd/{config,system.scm}' are built first.
# Unset 'GUILE_LOAD_COMPILED_PATH' so we can be sure that any .go file that we
# load comes from the build directory.
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
%.go: %.scm $(templates:%.in=%)
	$(AM_V_GUILEC)$(MKDIR_P) "`dirname "$@"`" ;	\
	$(AM_V_P) && out=1 || out=- ;			\
	unset GUILE_LOAD_COMPILED_PATH ; LC_ALL=C	\
	$(GUILD) compile --target="$(host)"		\
	  -L "$(top_builddir)/modules"			\
	  -L "$(top_srcdir)/modules"			\
	  -Wformat -Wunbound-variable -Warity-mismatch	\
	  -o "$@" "$<" >&$$out

SUFFIXES = .go

TESTS =						\
  tests/basic.sh				\
  tests/config-failure.sh			\
  tests/starting-status.sh			\
  tests/stopping-status.sh			\
  tests/startup-failure.sh			\
  tests/startup-error.sh			\
  tests/replacement.sh				\
  tests/respawn.sh				\
  tests/respawn-throttling.sh			\
  tests/restart.sh				\
  tests/misbehaved-client.sh			\
  tests/no-home.sh				\
  tests/pid-file.sh				\
  tests/logging.sh				\
  tests/file-creation-mask.sh			\
  tests/status-sexp.sh				\
  tests/forking-service.sh			\
  tests/one-shot.sh				\
  tests/transient.sh				\
  tests/inetd.sh				\
  tests/systemd.sh				\
  tests/signals.sh				\
  tests/system-star.sh				\
  tests/close-on-exec.sh			\
  tests/daemonize.sh				\
  tests/eval-load.sh				\
  tests/services/monitoring.sh			\
  tests/services/repl.sh

TEST_EXTENSIONS = .sh
EXTRA_DIST += $(TESTS)

AM_TESTS_ENVIRONMENT =				\
  unset XDG_CONFIG_HOME; unset LANGUAGE;	\
  LC_ALL=C LC_MESSAGES=C			\
  PATH="$(abs_top_builddir):$$PATH"		\
  SHELL="$(SHELL)" GUILE="$(GUILE)" CC="$(CC)"	\
  GUILE_LOAD_PATH="$(abs_top_srcdir)/modules:$(abs_top_builddir)/modules:$$GUILE_LOAD_PATH" \
  GUILE_LOAD_COMPILED_PATH="$(abs_top_srcdir)/modules:$(abs_top_builddir)/modules:$$GUILE_LOAD_COMPILED_PATH"

if HAVE_TIMEOUT
# Tests should never take more than a few seconds on modern machines.
SH_LOG_COMPILER = $(TIMEOUT) 3m $(SHELL)
else
SH_LOG_COMPILER = $(SHELL)
endif

AM_SH_LOG_FLAGS = -x -e

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
shepherd_install_go_files = install-nodist_guileobjectDATA
$(shepherd_install_go_files): install-dist_guilemoduleDATA

shepherdsub_install_go_files = install-nodist_shepherdgosubDATA
$(shepherdsub_install_go_files): install-dist_shepherdsubDATA install-nodist_shepherdsubDATA

scripts_install_go_files = install-nodist_scriptsgoDATA
$(scripts_install_go_files): install-dist_scriptsDATA



gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    8da9cf2..HEAD > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

dist-hook: gen-ChangeLog

.PHONY: gen-ChangeLog

SUBDIRS = po

ACLOCAL_AMFLAGS = -I m4
