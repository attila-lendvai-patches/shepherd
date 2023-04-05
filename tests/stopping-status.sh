# GNU Shepherd --- Test the "stopping" status.
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
confdir="t-confdir-$$"
datadir="t-datadir-$$"
log="t-log-$$"
stamp="t-stamp-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $stamp $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf" <<EOF
(register-services
 (service
   '(test)
   #:start (const #t)
   #:stop  (lambda _
             (let loop ((n 30))
               (if (or (file-exists? "$stamp") (zero? n))
                   (begin
                     (delete-file "$stamp")
                     (zero? n))   ;failure if N is zero
                   (begin
                     ((@ (fibers) sleep) 1)
                     (loop (- n 1))))))

   #:respawn? #f))
EOF

rm -f "$pid" "$stamp"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

$herd start test
$herd status test | grep started

$herd stop test &
herd_pid=$!

$herd status
$herd status test
$herd status test | grep "being stopped"

$herd stop test &
herd_pid2=$!
sleep 1
kill -0 "$herd_pid"
kill -0 "$herd_pid2"

# Trigger actual service stop.
touch "$stamp"

# Make sure the service is marked as "stopped" shortly after.
n=0
while : ; do
    if $herd status test | grep stopped
    then break
    else n=$(expr $n + 1)
    fi

    test $n -le 10
    sleep 1
done

# Make sure the 'herd' processes terminated.
n=0
while : ; do
    if kill -0 "$herd_pid" || kill -0 "$herd_pid2"
    then
	n=$(expr $n + 1)
	test $n -le 10
	sleep 1
    else
	break
    fi
done

$herd stop test
! test -f "$stamp"

$herd stop root
! kill -0 $shepherd_pid

test $(grep "Stopping service test" "$log" | wc -l) = 1

rm -rf "$confdir"
rm -rf "$datadir"
