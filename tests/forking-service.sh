# GNU Shepherd --- Test detecting a forked process' termination
# Copyright © 2016, 2020, 2022, 2023 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
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
service_pid="t-service-pid-$$"
service_nofiles="t-service-nofiles-$$"
service2_pid="t-service2-pid-$$"
service2_started="t-service2-starts-$$"

herd="herd -s $socket"

function cleanup
{
    cat $log || true
    rm -f $socket $conf $log $service2_started $service_nofiles
    test -f $pid && kill "`cat $pid`" || true
    rm -f $pid
    test -f $service_pid && kill "`cat $service_pid`" || true
    rm -f $service_pid
    test -f $service2_pid && kill "`cat $service2_pid`" || true
    rm -f $service2_pid
}

trap cleanup EXIT

cat > "$conf"<<EOF
;; Leave a timeout long enough for slow machines.
(default-pid-file-timeout 10)

(define %command
  '("$SHELL" "-c" "ulimit -n >$PWD/$service_nofiles; sleep 600 & echo \$! > $PWD/$service_pid"))

(register-services
 (list (service
	 ;; A service that forks into a different process.
	 '(test)
	 #:start (make-forkexec-constructor
                   %command
                   #:pid-file "$PWD/$service_pid"
                   #:resource-limits '((nofile 1567 1567)))
	 #:stop  (make-kill-destructor)
	 #:respawn? #f)))

(define %command2
  '("$SHELL" "-c" "echo started >> $PWD/$service2_started; sleep 600 & echo \$! > $PWD/$service2_pid"))

(register-services
 (list (service
	 ;; A service that forks into a different process.
	 '(test2)
	 #:start (make-forkexec-constructor %command2
					    #:pid-file "$PWD/$service2_pid")
	 #:stop  (make-kill-destructor)
	 #:respawn? #t)))

(define %command3
  '("$SHELL" "-c" "sleep 600"))

(register-services
 (list (service
	 ;; A service that forks into a different process.
	 '(test3)
	 #:start (make-forkexec-constructor %command3)
	 #:stop  (make-kill-destructor)
	 #:respawn? #t)))

(define %command4
  '("$SHELL" "-c" "trap 'echo ignoring SIGTERM' SIGTERM; while true ; do : ; done"))

(register-services
 (list (service
	 ;; A service that ignores SIGTERM.
	 '(test4)
	 #:requirement '(test3)
	 #:start (make-forkexec-constructor %command4)
	 #:stop  (make-kill-destructor SIGTERM #:grace-period 3))))
EOF
cat $conf

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

# start both of the services
$herd start test
$herd start test2

# make sure "test" is started
until $herd status test | grep running; do sleep 0.3; done
test -f "$service_pid"
service_pid_value="`cat $service_pid`"
# now kill it
kill "$service_pid_value"
while kill -0 "$service_pid_value"; do sleep 0.3; done
# shepherd should notice that the service has stopped within one second
sleep 1
$herd status test | grep stopped



# make sure "test2" has started
until $herd status test2 | grep running; do sleep 0.3; done
test -f "$service2_pid"
service2_pid_value="`cat $service2_pid`"
test "`cat $PWD/$service2_started`" = "started"
# now kill it
rm -f "$service2_pid"
kill $service2_pid_value
while kill -0 "$service2_pid_value"; do sleep 0.3; done
# shepherd should notice that the service has stopped, and restart it, within one second
sleep 1;
$herd status test2 | grep running
test "`cat $PWD/$service2_started`" = "started
started"



# test if nofiles was set properly
test -f "$service_nofiles"
nofiles_value="`cat $service_nofiles`"
test 1567 -eq $nofiles_value

# Try to trigger potential race conditions, when killing a process, between
# the fork and execv calls.
for i in `seq 1 50`
do
    $herd restart test3
done

# Make sure 'herd stop' eventually terminates processes that ignore SIGTERM.
$herd start test4
$herd status test3 | grep running
child_pid="$($herd status test4 | grep Running | sed '-es/.*Running value is \([0-9]\+\)\./\1/g')"
kill -0 "$child_pid"
$herd stop test3		# this will also stop 'test4'
kill -0 "$child_pid" && false
grep ignoring "$log"
grep SIGKILL "$log"
$herd status test3 | grep stopped

$herd status
