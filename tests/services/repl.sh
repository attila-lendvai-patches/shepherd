# GNU Shepherd --- Test monitoring service.
# Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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
repl_socket="$PWD/repl-socket-$$"

herd="herd -s $socket"

trap "cat $log || true;
      rm -f $socket $repl_socket $conf $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf" <<EOF
(use-modules (shepherd service repl))

(register-services (list (repl-service "$repl_socket")))
EOF

rm -f "$pid" "$log" "$repl_socket"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

$herd start repl
$herd status repl | grep running

$herd status
test $($herd status | grep '^ ' | wc -l) -eq 2

guile -c '(let ((sock (socket AF_UNIX SOCK_STREAM 0)))
            (connect sock PF_UNIX "'$repl_socket'")
            (sleep 10))' &
child_pid=$!

$herd status
$herd status repl-client-1
$herd status repl-client-1 | grep running
$herd status repl-client-1 | grep "transient"
test $($herd status | grep '^ ' | wc -l) = 3

# Make sure 'repl-client-1' gets stopped as soon as the client disappears.
kill $child_pid
while test $($herd status | grep '^ ' | wc -l) -ne 2; do $herd status && sleep 1 ;done
! $herd status repl-client-1

guile -c '
(use-modules (ice-9 rdelim))

(setvbuf (current-output-port) (string->symbol "none"))
(alarm 10)
(let ((sock (socket AF_UNIX SOCK_STREAM 0)))
  (connect sock PF_UNIX "'$repl_socket'")
  (format #t "connected!~%> ")

  (let loop ()
    (define chr (read-char sock))
    (unless (eof-object? chr)
      (display chr)
      (when (eq? chr #\newline)
	(display "> ")))
    (cond ((eof-object? chr)
           (format #t "done!~%"))
          ((eq? chr #\>)
           (display "(+ 2 3)\n,q\n" sock)
           (loop))
	  (else
	   (loop)))))
'

while test $($herd status | grep '^ ' | wc -l) -ne 2; do $herd status && sleep 1; done

# Register and start a service from the REPL.
guile -c '
(alarm 10)
(let ((sock (socket AF_UNIX SOCK_STREAM 0)))
  (connect sock PF_UNIX "'$repl_socket'")
  (format #t "connected!~%> ")
  (display
   (object->string
    (quote (begin
             (use-modules (shepherd service) (shepherd service monitoring))
             (register-services (list (monitoring-service #:period 2)))
             (start-service (lookup-service (quote monitoring))))))
   sock)
  (display ",q\n" sock)
  (let loop ()
    (define chr (read-char sock))
    (unless (eof-object? chr)
      (display chr)
      (when (eq? chr #\newline)
	(display "> "))
      (loop))))
'

$herd status monitoring
$herd status monitoring | grep running
grep "heap:" "$log"

$herd log monitoring | grep "heap:"

$herd stop repl
$herd status repl | grep "stopped"

# Now we can't connect anymore.
! guile -c '(let ((sock (socket AF_UNIX SOCK_STREAM 0)))
              (connect sock PF_UNIX "'$repl_socket'"))'
