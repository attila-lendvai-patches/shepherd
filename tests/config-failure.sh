# GNU Shepherd --- Test shepherd behavior when config file errors out.
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
child_pid="t-child-pid-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $stamp $log $child_pid;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf" <<EOF
(register-services
 (list (service
	 '(succeeding)
	 #:start (make-forkexec-constructor
                   '("$SHELL" "-c" "echo \$\$ > $PWD/$child_pid; exec sleep 300"))
	 #:stop (make-kill-destructor)
	 #:respawn? #f)
       (service
	 '(failing)
         #:requirement '(succeeding)
	 #:start (lambda _
                   (call-with-output-file "$stamp" (const #t))
                   (error "faileddddd!"))
	 #:stop (const #f)
	 #:respawn? #f)))

(start-service (lookup-service 'failing))
EOF

rm -f "$pid" "$stamp"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# The 'succeeding' service should be up and running.
while ! test -f "$child_pid" ; do sleep 0.3 ; done
kill -0 "$(cat "$child_pid")"

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done
shepherd_pid="$(cat $pid)"

# Then the 'failing' service should fail.
while ! test -f "$stamp" ; do sleep 0.3 ; done

# Despite the failure while loading $conf, shepherd must be up and running.
$herd status failing | grep "stopped"
$herd status succeeding | grep "running"

$herd stop root

while kill -0 "$shepherd_pid" ; do sleep 0.3 ; done
if kill -0 "$(cat "$child_pid")"; then false; else true; fi
