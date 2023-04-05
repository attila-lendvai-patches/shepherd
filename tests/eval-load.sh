# GNU Shepherd --- Check whether config can be loaded with 'primitive-load'
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
 (service
   '(a)
   #:start (const #t)
   #:respawn? #f)
 (service
   '(b)
   #:start (const #t)
   #:respawn? #f))
EOF

rm -f "$pid" "$stamp" "$socket"
shepherd -I -s "$socket" -c /dev/null --pid="$pid" --log="$log" &

while ! test -f "$pid"; do sleep 0.5 ; done

$herd status

# 'guix system reconfigure' does something similar to what's shown below.  As
# of Guile 3.0.9, 'primitive-load' is in C and thus introduces a continuation
# barrier, which makes it unsuitable in this context.  Check that we're not
# hitting a continuation barrier.
$herd eval root '(primitive-load "'$conf'")'

$herd status
$herd status | grep ' - a'
$herd status | grep ' - b'
$herd start a
$herd start b
