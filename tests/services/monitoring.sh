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

herd="herd -s $socket"

trap "cat $log || true;
      rm -f $socket $conf $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf" <<EOF
(use-modules (shepherd service monitoring))

(register-services (list (monitoring-service)))
EOF

rm -f "$pid" "$log"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

$herd start monitoring
$herd status monitoring | grep running

n=0
while ! grep "heap:" "$log" && test $n -lt 10
do
    sleep 1
    n=$(expr $n + 1)
done
test $n -lt 10

$herd log monitoring
$herd log monitoring | grep "heap:"
$herd log monitoring | grep "service names: 3"
$herd period monitoring 1
! $herd period monitoring not-a-number

$herd stop monitoring
$herd status monitoring | grep "stopped"
