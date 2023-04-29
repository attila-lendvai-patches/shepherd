# GNU Shepherd --- Test transient services.
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
 (list
  (service
    '(test-inetd)
    #:start (make-inetd-constructor %command
				    (list
				     (endpoint (make-socket-address
						AF_INET
						INADDR_LOOPBACK
						$PORT))))
    #:stop  (make-inetd-destructor))
  (service
    '(test-inetd6)
    #:start (make-inetd-constructor %command
				    (list
				     (endpoint (make-socket-address
						AF_INET
						INADDR_LOOPBACK
						$PORT))
				     (endpoint (make-socket-address
						AF_INET6
						IN6ADDR_LOOPBACK
						$PORT))))
    #:stop  (make-inetd-destructor))
  (service
    '(test-inetd-v6-only)
    #:start (make-inetd-constructor %command
				    (list
				     (endpoint (make-socket-address
						AF_INET6
						IN6ADDR_LOOPBACK
						$PORT))))
    #:stop  (make-inetd-destructor))
  (service
    '(test-inetd-unix)
    #:start (make-inetd-constructor %command
				    (list
				     (endpoint (make-socket-address
						AF_UNIX "$service_socket")))
				    #:max-connections 5)
    #:stop  (make-inetd-destructor))))

(start-service (lookup-service 'test-inetd))
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

# Trigger startup of the finalizer thread, which creates a couple of pipes.
# That way, those extra file descriptors won't influence the comparison with
# INITIAL_FD_COUNT done at the end.
$herd eval root '(gc)'

initial_fd_count=$(file_descriptor_count)

$herd status test-inetd | grep running
test $($herd status | grep '\+' | wc -l) -eq 2

converse_with_echo_server ()
{
    guile -c "(use-modules (ice-9 match) (ice-9 rdelim))
      (define IN6ADDR_LOOPBACK 1)
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

# Unavailable on IPv6.
converse_with_echo_server \
    "(make-socket-address AF_INET6 IN6ADDR_LOOPBACK $PORT)" \
     && false

$herd stop test-inetd
converse_with_echo_server \
  "(make-socket-address AF_INET INADDR_LOOPBACK $PORT)" \
   && false

if guile -c '(socket AF_INET6 SOCK_STREAM 0)'; then
    # Test IPv6 support.
    $herd start test-inetd6

    converse_with_echo_server \
	"(make-socket-address AF_INET6 IN6ADDR_LOOPBACK $PORT)"
    converse_with_echo_server \
	"(make-socket-address AF_INET INADDR_LOOPBACK $PORT)"

    $herd stop test-inetd6

    ! converse_with_echo_server \
	"(make-socket-address AF_INET6 IN6ADDR_LOOPBACK $PORT)"
    ! converse_with_echo_server \
	"(make-socket-address AF_INET INADDR_LOOPBACK $PORT)"

    $herd start test-inetd-v6-only

    converse_with_echo_server \
	"(make-socket-address AF_INET6 IN6ADDR_LOOPBACK $PORT)"
    ! converse_with_echo_server \
	"(make-socket-address AF_INET INADDR_LOOPBACK $PORT)"

    $herd stop test-inetd-v6-only

    ! converse_with_echo_server \
	"(make-socket-address AF_INET6 IN6ADDR_LOOPBACK $PORT)"
    ! converse_with_echo_server \
	"(make-socket-address AF_INET INADDR_LOOPBACK $PORT)"

    # Note: The following test below would hang with Fibers 1.1.0, due to
    # <https://github.com/wingo/fibers/pull/57>.
fi

# Now test inetd on a Unix-domain socket.

$herd start test-inetd-unix
for i in $(seq 1 3)
do
    test $($herd status | grep '\+' | wc -l) -eq 2
    converse_with_echo_server \
	"(make-socket-address AF_UNIX \"$service_socket\")"
done

$herd stop test-inetd-unix
converse_with_echo_server \
  "(make-socket-address AF_UNIX \"$service_socket\")" \
   && false

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

# Simulate EADDRINUSE.
$herd eval root "
  (let ((real-bind bind)
        (failures 0))
    (set! bind (lambda (sock address)
                 (when (< failures 2)
                   (set! failures (+ failures 1))
                   (throw 'system-error \"bind\" \"Oh!\" '()
                          (list EADDRINUSE)))
                 (set! bind real-bind)
                 (real-bind sock address))))"

$herd start test-inetd-unix
$herd status test-inetd-unix | grep running
$herd stop test-inetd-unix

grep "is in use" "$log"
$herd status

# At this point, shepherd should have INITIAL_FD_COUNT - 1 file descriptors
# opened.
test $(file_descriptor_count) -lt $initial_fd_count
