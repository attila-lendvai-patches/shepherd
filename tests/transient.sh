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

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf"<<EOF
(register-services
 (list (service
	 '(transient-test1)
	 #:start (make-forkexec-constructor '("sleep" "600"))
	 #:transient? #t)
       (service
	 '(transient-test2)
	 #:start (make-forkexec-constructor '("sleep" "600"))
	 #:transient? #t)))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

$herd start transient-test1
$herd start transient-test2

# Stop the service and make sure it gets unregistered.
$herd status transient-test1 | grep "transient, running"
$herd stop transient-test1
! $herd status transient-test1

# Terminate the service and make sure it gets unregistered.
$herd status transient-test2 | grep "transient, running"
kill $($herd status transient-test2 | grep Running | sed -e's/^.* \([0-9]\+\).*$/\1/g')
! $herd status transient-test2

test $($herd status | grep transient-test | wc -l) -eq 0
