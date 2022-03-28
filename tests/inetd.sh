# GNU Shepherd --- Test transient services.
# Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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
service_socket="t-service-socket-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $service_socket $socket $conf $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT


PORT=4444			# port of the echo server

cat > "$conf" <<EOF
(define %command
  ;; Simple echo server.
  '("$SHELL" "-c" "echo hello; read line; echo \$line; echo done"))

(register-services
 (make <service>
   #:provides '(test-inetd)
   #:start (make-inetd-constructor %command
                                   (make-socket-address AF_INET
                                                        INADDR_LOOPBACK
                                                        $PORT))
   #:stop  (make-inetd-destructor))
 (make <service>
   #:provides '(test-inetd-unix)
   #:start (make-inetd-constructor %command
                                   (make-socket-address AF_UNIX
                                                        "$service_socket")
                                   #:max-connections 5)
   #:stop  (make-inetd-destructor)))

(start 'test-inetd)
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

file_descriptor_count ()
{
    ls -l /proc/$shepherd_pid/fd/[0-9]* | wc -l
}

initial_fd_count=$(file_descriptor_count)

$herd status test-inetd | grep started
test $($herd status | grep '\+' | wc -l) -eq 2

converse_with_echo_server ()
{
    guile -c "(use-modules (ice-9 match) (ice-9 rdelim))
      (define address $1)
      (define sock (socket (sockaddr:fam address) SOCK_STREAM 0))
      (connect sock address)
      (match (read-line sock) (\"hello\" #t))
      (display \"bye\n\" sock)
      (match (read-line sock) (\"bye\" #t))
      (match (read-line sock) (\"done\" #t))"
}


for i in $(seq 1 3)
do
    test $($herd status | grep '\+' | wc -l) -eq 2
    converse_with_echo_server \
	"(make-socket-address AF_INET INADDR_LOOPBACK $PORT)"
done

$herd stop test-inetd
! converse_with_echo_server \
  "(make-socket-address AF_INET INADDR_LOOPBACK $PORT)"

# Now test inetd on a Unix-domain socket.

$herd start test-inetd-unix
for i in $(seq 1 3)
do
    test $($herd status | grep '\+' | wc -l) -eq 2
    converse_with_echo_server \
	"(make-socket-address AF_UNIX \"$service_socket\")"
done

$herd stop test-inetd-unix
! converse_with_echo_server \
  "(make-socket-address AF_UNIX \"$service_socket\")"

# Check the maximum connection limit.
$herd start test-inetd-unix
guile -c "
  (use-modules (ice-9 rdelim) (ice-9 match))
  (define address (make-socket-address AF_UNIX \"$service_socket\"))
  (let loop ((i 10)
             (sockets '()))
    (if (zero? i)
        ;; shepherd should close the extra sockets immediately.
        (unless (equal? (append (make-list 5 the-eof-object)
                                (make-list 5 \"hello\"))
                        (pk 'read (map read-line sockets)))
          (exit 1))
        (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
          (connect sock address)
          (loop (- i 1) (cons sock sockets)))))"

converse_with_echo_server \
  "(make-socket-address AF_UNIX \"$service_socket\")"

$herd stop test-inetd-unix
$herd status

# At this point, shepherd should have INITIAL_FD_COUNT - 1 file descriptors
# opened.
test $(file_descriptor_count) -lt $initial_fd_count
