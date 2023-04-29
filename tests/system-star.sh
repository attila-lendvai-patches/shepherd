# GNU Shepherd --- Test whether 'system*' is blocking.
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
stamp="t-stamp-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $stamp $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

script="while [ ! -f $PWD/$stamp ] ; do sleep 0.1 ; done ; exit \$(cat $PWD/$stamp)"

cat > "$conf" <<EOF
(register-services
 (list (service
	 '(test)
	 #:start (lambda _
		   (list 'exit-code
			 (status:exit-val
			  (system* "$SHELL" "-c" "$script"))))
	 #:stop  (lambda _
		   (system* "$SHELL" "-c" "echo STOPPING")
		   (delete-file "$stamp"))
	 #:respawn? #f)
       (service
	 '(test-command-not-found)
	 #:start (lambda _
		   (zero? (system* "this command does not exist")))
	 #:stop  (const #f)
	 #:respawn? #f)
       (service
	 '(test-with-respawn)
	 #:start (make-forkexec-constructor
		   (list "$SHELL" "-cex"
			 "[ ! -f $PWD/$stamp ] ; touch $PWD/$stamp ; sleep 60"))
	 #:stop  (lambda (pid)
		   (and (zero? (system* "$(type -P kill)" (number->string pid)))
			(begin
			  (delete-file "$stamp")
			  #f)))
	 #:respawn? #t)))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

kill -0 $shepherd_pid

# 'herd start' will block until the script exits...
$herd start test &

# ... so at this point the service is starting or about to start.
$herd status test | grep -E "(starting|stopped)"

# Touch $stamp.  The shell script passed to 'system*' should complete shortly
# after that.
echo 123 > "$stamp"

n=0
while [ $n -lt 20 ]
do
    if $herd status test | grep running
    then
	break
    else
	n=$(expr $n + 1)
	sleep 1
    fi
done
$herd status test | grep running
$herd status test | grep "exit-code 123"

$herd stop test
test -f "$stamp" && false
grep "STOPPING" "$log"

# This service uses 'system*' but the command is not found.
$herd start test-command-not-found && false
$herd status test-command-not-found
$herd status test-command-not-found | grep "stopped"

# What about a service with a custom 'stop' procedure that uses 'system*'?
# Stopping the service should not trigger the respawn machinery.
$herd start test-with-respawn
$herd status test-with-respawn | grep running
$herd stop test-with-respawn
$herd status test-with-respawn | grep "stopped"

for i in `seq 1 5`
do
    $herd restart test-with-respawn
    $herd status test-with-respawn | grep running
done
$herd stop test-with-respawn

# What happens when we cause the process monitor to throw an exception while
# trying to fork?  The process monitor fiber should remain alive.
$herd eval root "(setrlimit 'nproc 1 1)"
$herd start test && false
$herd status test
$herd status test | grep "stopped"

$herd stop root

# Make sure 'shutdown-services' did its job.
if test -f "$stamp"; then false; else true; fi
