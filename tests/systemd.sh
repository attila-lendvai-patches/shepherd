# GNU Shepherd --- Test transient services.
# Copyright © 2022-2023 Ludovic Courtès <ludo@gnu.org>
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

cat > "$conf" <<EOF
(define %command
  ;; Simple echo server.
  (quasiquote ("guile" "-c"
    ,(object->string
      '(begin
         (use-modules (ice-9 match) (ice-9 rdelim))

         (display "starting\n")
         (unless (= (string->number (getenv "LISTEN_PID")) (getpid))
           (error "wrong pid!" (getenv "LISTEN_PID")))
         (unless (= (string->number (getenv "LISTEN_FDS")) 1)
           (error "wrong LISTEN_FDS!" (getenv "LISTEN_FDS")))
         (let ((sock (fdopen 3 "r+0")))
           (match (accept sock)
             ((connection . peer)
              (format #t "accepting connection from ~s~%" peer)
              (display "hello\n" connection)
              (display (read-line connection) connection)
              (newline connection)
              (display "done\n" connection)
              (display "exiting!\n")
              (close-port connection)
              (close-port sock)))))))))

(define %endpoints
  (list (endpoint (make-socket-address AF_UNIX "$service_socket"))))

(register-services
 (list (service
	 '(test-systemd-unix)
	 #:start (make-systemd-constructor %command %endpoints)
	 #:stop  (make-systemd-destructor)
	 #:respawn? #t)
       (service
	 '(test-systemd-unix-eager)
	 #:start (make-systemd-constructor %command %endpoints
					   #:lazy-start? #f)
	 #:stop  (make-systemd-destructor))))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

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


$herd start test-systemd-unix
$herd status test-systemd-unix | grep running
test $($herd status | grep '\+' | wc -l) -eq 2

for i in $(seq 1 3)
do
    converse_with_echo_server \
	"(make-socket-address AF_UNIX \"$service_socket\")"
done

$herd stop test-systemd-unix
if converse_with_echo_server "(make-socket-address AF_UNIX \"$service_socket\")"
then false; else true; fi

# Now test the eager systemd-style service.
$herd start test-systemd-unix-eager
$herd status test-systemd-unix-eager | grep running

# The process should soon be running, before we've tried to connect to it.
while ! $herd status test-systemd-unix-eager | grep -E "Running value is [0-9]+"
do $herd status test-systemd-unix-eager; sleep 0.3; done

child_pid="$($herd status test-systemd-unix-eager | grep Running \
   | sed '-es/.*Running value is \([0-9]\+\)\./\1/g')"
kill -0 "$child_pid"
converse_with_echo_server "(make-socket-address AF_UNIX \"$service_socket\")"
while ! $herd status test-systemd-unix-eager | grep stopped
do sleep 0.3; done
