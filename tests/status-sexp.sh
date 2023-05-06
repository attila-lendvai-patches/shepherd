# GNU Shepherd --- Test status sexps.
# Copyright © 2016, 2023 Ludovic Courtès <ludo@gnu.org>
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

trap "rm -f $socket $conf $stamp $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf"<<EOF
(register-services
 (list (service
	 '(foo)
	 #:start (const 'abc)
	 #:stop  (const #f)
	 #:documentation "Foo!"
	 #:respawn? #t)
       (service
	 '(bar)
	 #:requirement '(foo)
	 #:start (const 'up-and-running)
	 #:stop  (const #f)
	 #:documentation "Bar!"
	 #:respawn? #f)))

(start-service (lookup-service 'foo))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

kill -0 $shepherd_pid
test -S "$socket"

# Code to fetch service status info.
fetch_status="
  (let ((sock (open-connection \"$socket\")))
    (write-command (shepherd-command 'status 'root) sock)
    (read sock))"

root_service_sexp="
   (service (version 0)
      (provides (root shepherd))
      (requires ())
      (respawn? #f)
      (docstring \"The root service is used to operate on shepherd itself.\")
      (enabled? #t) (running #t) (conflicts ())
      (last-respawns ())
      (status-changes ((running . 0) (starting . 0)))
      (startup-failures ())
      (status running)
      (one-shot? #f)
      (transient? #f))"

# Define a helper procedure that resets timestamps in the 'status-changes'
# property to make it easier to compare them.
define_reset_timestamps="
(define (reset-timestamps service)
  (match service
    (('service version properties ...)
     (cons* 'service version
	    (map (match-lambda
		   (('status-changes alist)
                    (list 'status-changes
			  (map (match-lambda
				 ((status . _)
				  (cons status 0)))
			       alist)))
                   (prop prop))
		 properties)))))
"

"$GUILE" -c "
(use-modules (shepherd comm) (srfi srfi-1) (ice-9 match))

$define_reset_timestamps

(exit
 (match $fetch_status
   (('reply _ ('result (services)) ('error #f) ('messages ()))
    (lset= equal?
           (pk 'ACTUAL (map reset-timestamps services))
	   '($root_service_sexp
	     (service (version 0)
	       (provides (foo)) (requires ())
	       (respawn? #t) (docstring \"Foo!\")
	       (enabled? #t) (running abc) (conflicts ())
	       (last-respawns ())
               (status-changes ((running . 0) (starting . 0)))
               (startup-failures ())
               (status running)
               (one-shot? #f) (transient? #f))
	     (service (version 0)
	       (provides (bar)) (requires (foo))
	       (respawn? #f) (docstring \"Bar!\")
	       (enabled? #t) (running #f) (conflicts ())
	       (last-respawns ())
               (status-changes ())
               (startup-failures ())
               (status stopped)
               (one-shot? #f) (transient? #f)))))))
"

# Make sure we get an 'error' sexp when querying a nonexistent service.
"$GUILE" -c "
(use-modules (shepherd comm) (ice-9 match))

(match (let ((sock (open-connection \"$socket\")))
         (write-command (shepherd-command 'status 'does-not-exist) sock)
         (read sock))
  (('reply _ ...
    ('error ('error _ 'service-not-found 'does-not-exist))
    ('messages ()))
   #t)
  (x
   (pk 'wrong x)
   (exit 1)))"

# Unload everything and make sure only 'root' is left.
$herd unload root all

"$GUILE" -c "
(use-modules (shepherd comm) (ice-9 match))

$define_reset_timestamps

(exit
  (equal? (match $fetch_status
            (('reply version ('result ((service))) rest ...)
             (cons* 'reply version
                     (list 'result
                            (list (list (reset-timestamps service))))
                     rest)))
          '(reply
            (version 0)
            (result (($root_service_sexp)))
            (error #f) (messages ()))))"

$herd stop root

test -f "$log"
