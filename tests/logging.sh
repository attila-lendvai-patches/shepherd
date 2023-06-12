# GNU Shepherd --- Test the logging capabilities of 'make-forkexec-constructor'.
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
service_script="t-service-script-$$"
service_pid="t-service-pid-$$"
service_log="t-service-log-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $service_pid $service_log $service_script $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$service_script" <<EOF
echo STARTING
echo \$\$ > "$PWD/$service_pid"
echo STARTED >&2
echo café anyone?
printf "latin1 garbage: \347a alors !\n"
exec sleep 600
EOF

cat > "$conf"<<EOF
(use-modules (ice-9 match))

(define %command
  '("$SHELL" "$service_script"))

(register-services
 (list (service
	 ;; Service with built-in logging.
	 '(test-builtin-logging)
	 #:start (make-forkexec-constructor %command
					    #:pid-file "$PWD/$service_pid")
	 #:stop  (make-kill-destructor)
	 #:respawn? #f)

       (service
	 ;; Service with built-in logging.
	 '(test-file-logging)
	 #:start (make-forkexec-constructor %command
					    #:log-file "$PWD/$service_log"
					    #:pid-file "$PWD/$service_pid")
	 #:stop  (make-kill-destructor)
	 #:respawn? #f)))

;; Start it upfront to make sure the logging fiber works.
(start-service (lookup-service 'test-file-logging))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

while ! test -f "$service_pid" ; do sleep 0.3 ; done
until $herd status test-file-logging | grep running; do sleep 1; done

cat "$service_log"
for message in "STARTING" "STARTED" "café" "latin1 garbage: .* alors"
do
    grep -E '^2[0-9]{3}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} '"$message" "$service_log"
done

# Return the file descriptor corresponding to the given file.
find_file_descriptor ()
{
    for fd in "/proc/$shepherd_pid/fd"/*
    do
	if [ "$(readlink $fd)" = "$1" ]
	then
	    echo "$fd"
	    break
	fi
    done
}

# At this point, the log file is open.
test -n "$(find_file_descriptor "$PWD/$service_log")"

# Stop the service and ensure the log file has been closed.
$herd stop test-file-logging
test -z "$(find_file_descriptor "$PWD/$service_log")"

rm -f "$service_pid"
$herd start test-builtin-logging
for message in "STARTING" "STARTED" "café" "latin1 garbage: .* alors"
do
    grep -E '^2[0-9]{3}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} .*'"$message" "$log"
done

$herd stop root
