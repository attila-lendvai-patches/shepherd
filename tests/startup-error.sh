# GNU Shepherd --- Check service startup failure reporting
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

trap "rm -f $socket $conf $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf" <<EOF
(register-services
 (list (service
	 '(does-fail)
	 #:start (lambda _
		   (error "This is an intentionally thrown error"))
	 #:respawn? #f)))
EOF

rm -f "$pid" "$socket"
shepherd -I -s "$socket" -c "$conf" --pid="$pid" --log="$log" &

while ! test -f "$pid"; do sleep 0.5 ; done

# When the service fails to start, 'herd status' should display that.
if $herd start does-fail; then false; else true; fi
$herd status does-fail | grep stopped
$herd status does-fail | grep "Failed to start"
$herd status | grep "Failed to start:"

$herd log
$herd log | grep "service does-fail failed to start"

# Once the service has been stopped gracefully, the "Failed to start" message
# should not appear any longer.
$herd stop does-fail
$herd status does-fail | grep stopped
$herd status does-fail | grep -v "Failed to start"
