# GNU Shepherd --- Ensure file descriptors are not leaked to children.
# Copyright © 2022, 2023 Ludovic Courtès <ludo@gnu.org>
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

shepherd --version
herd --version

socket="t-socket-$$"
conf="t-conf-$$"
log="t-log-$$"
pid="t-pid-$$"
c_file="t-count-file-descriptors-$$.c"
exe="$PWD/t-count-file-descriptors-$$"
fd_count="$PWD/t-fd-count-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $log $fd_count $c_file $exe;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$c_file" <<EOF
/* This program counts its own open file descriptors and writes
   that number to $fd_count.  It's more reliable than using the
   shell or Guile since those may open additional file descriptors.  */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <sys/socket.h>
#include <assert.h>

int
main (int argc, char *argv[])
{
  DIR *dir;
  struct dirent *ent;
  size_t count;
  FILE *log;

  if (getenv ("LISTEN_FDS") != NULL)  /* systemd */
    {
      struct sockaddr_storage address;
      socklen_t len;
      int fd = accept (3, (struct sockaddr *) &address, &len);
      assert (fd >= 0);
    }

  dir = opendir ("/proc/self/fd");
  chdir ("/proc/self/fd");
  for (count = 0, ent = NULL; ent = readdir (dir), ent != NULL; )
    {
      if (strcmp (ent->d_name, ".") == 0 || strcmp (ent->d_name, "..") == 0)
        continue;

      char target[1024];
      ssize_t size;
      size = readlink (ent->d_name, target, sizeof target);
      target[size < 0 ? 0 : size] = '\0';
      printf ("%s -> %s\n", ent->d_name, target);

      count++;
    }
  closedir (dir);

  log = fopen ("$fd_count", "w");
  fprintf (log, "%zi\n", count);
  fclose (log);

  return EXIT_SUCCESS;
}
EOF

"${CC:-gcc}" -Wall "$c_file" -o "$exe"
"$exe"				# try it out

cat > "$conf" <<EOF
(register-services
 (list (service
	 '(system-ctor)
	 #:start (make-system-constructor "$exe")
	 #:stop  (const #f)
	 #:one-shot? #t)
       (service
	 '(forkexec-ctor)
	 #:start (make-forkexec-constructor '("$(type -P sleep)" "100"))
	 #:stop (make-kill-destructor))
       (service
	 '(inetd-ctor)
	 #:start (make-inetd-constructor '("$exe")
					 (list
					  (endpoint (make-socket-address
						     AF_INET
						     INADDR_LOOPBACK
						     5555))))
	 #:stop  (make-inetd-destructor))
       (service
	 '(systemd-ctor)
	 #:start (make-systemd-constructor '("$exe")
					   (list
					    (endpoint (make-socket-address
						       AF_INET
						       INADDR_LOOPBACK
						       5556))))
	 #:stop  (make-systemd-destructor))))
EOF

rm -f "$pid" "$fd_count"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"
kill -0 $shepherd_pid

# Open listening sockets, which should all be SOCK_CLOEXEC.
$herd start inetd-ctor

ls -l /proc/$shepherd_pid/fd

# Start 'system-ctor' and check how many open file descriptors it sees.
$herd start system-ctor
$herd status system-ctor
while ! test -f "$fd_count" ; do sleep 0.3 ; done

# The process running $exe must have seen the three standard file descriptors
# plus an open descriptor on /proc/self/fd.  Note that $exe is executed by
# 'system' so whether file descriptors are closed depends entirely on properly
# mapping all of shepherd's internal-use file descriptors as O_CLOEXEC.
test $(cat "$fd_count") -eq 4

# Same test, this time with a process started with 'make-forkexec-constructor'
# and thus 'exec-command'.
$herd start forkexec-ctor
$herd status forkexec-ctor

pid="$($herd status forkexec-ctor | grep "Running value" \
  | sed -e's/^.* \([0-9]\+\)\.$/\1/g')"
kill -0 "$pid"

ls -l "/proc/$pid/fd"
test "$(cd "/proc/$pid/fd"; echo *)" = "0 1 2"
$herd stop forkexec-ctor

# Likewise for inetd and systemd services.

connect_to_server ()
{
    rm -f "$fd_count"
    guile -c "(use-modules (ice-9 match))
      (define IN6ADDR_LOOPBACK 1)
      (define address (make-socket-address AF_INET INADDR_LOOPBACK $1))
      (define sock (socket (sockaddr:fam address) SOCK_STREAM 0))
      (connect sock address)"
    while ! test -f "$fd_count" ; do sleep 0.3 ; done
}

for i in $(seq 1 3)
do
    # Spawn the inetd service process by connecting to the endpoint.  It must
    # have nothing but the 3 standard file descriptors open (plus one for
    # /proc/self/fd).
    connect_to_server 5555
    test $(cat "$fd_count") -eq 4

    # Spawn the systemd service by starting it.  This one must have
    # 5 open file descriptors: one for the listening socket, and one for the
    # accepted client connection (plus one for /proc/self/fd).
    $herd enable systemd-ctor
    $herd start systemd-ctor
    connect_to_server 5556
    test $(cat "$fd_count") -eq 6
    $herd stop systemd-ctor

    $herd restart forkexec-ctor
done
