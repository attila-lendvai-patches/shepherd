;; herd.scm -- The program to herd the Shepherd.
;; Copyright (C) 2013-2014, 2016, 2018-2019, 2021-2023 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;;
;; This file is part of the GNU Shepherd.
;;
;; The GNU Shepherd is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; The GNU Shepherd is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.

(define-module (shepherd scripts herd)
  #:use-module (shepherd config)
  #:use-module (shepherd support)
  #:use-module (shepherd args)
  #:use-module (shepherd comm)
  #:use-module (shepherd colors)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 vlist) (vlist-null vhash-consq vhash-assq)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (main))


;; Information about live services.
(define-record-type <live-service>
  (live-service provision requirement one-shot? transient? respawn?
                enabled? status running
                status-changes last-respawns startup-failures)
  live-service?
  (provision        live-service-provision)       ;list of symbols
  (requirement      live-service-requirement)     ;list of symbols
  (one-shot?        live-service-one-shot?)       ;Boolean
  (transient?       live-service-transient?)      ;Boolean
  (respawn?         live-service-respawn?)        ;Boolean

  (enabled?         live-service-enabled?)         ;Boolean
  (status           live-service-status)           ;symbol
  (running          live-service-running-value)    ;#f | object
  (status-changes   live-service-status-changes)   ;symbol/integer pairs
  (last-respawns    live-service-last-respawns)    ;list of integers
  (startup-failures live-service-startup-failures)) ;list of integers

(define (live-service-canonical-name service)
  "Return the 'canonical name' of @var{service}."
  (first (live-service-provision service)))

(define (live-service-failing? service)
  "Return true if @var{service} failed to start."
  (and (eq? 'stopped (live-service-status service))
       (pair? (live-service-startup-failures service))))

(define (live-service-last-status-change-time service)
  "Return the time @var{service} last changed statuses."
  (match (live-service-status-changes service)
    (((_ . time) . _) time)
    (() #f)))

(define (live-service-status-duration service)
  "Return the duration @var{service} has been in its current status."
  (match (live-service-last-status-change-time service)
    (#f 0)
    (time
     (- (time-second (current-time time-utc)) time))))

(define (live-service-status-predicate status)
  "Return a predicate that returns true when passed a service with the given
@var{status}."
  (lambda (service)
    (eq? status (live-service-status service))))

(define-syntax alist-let*
  (syntax-rules ()
    "Bind the given KEYs in EXP to the corresponding items in ALIST.  ALIST
is assumed to be a list of two-element tuples rather than a traditional list
of pairs."
    ((_ alist (key ...) exp ...)
     (let ((key (and=> (assoc-ref alist 'key) car)) ...)
       exp ...))))

(define (sexp->live-service sexp)
  "Turn @var{sexp}, the wire representation of a service returned by shepherd,
into a @code{live-service} record."
  (match sexp
    (('service ('version 0 _ ...) properties ...)
     (alist-let* properties (provides requires status running respawn? enabled?
                             status-changes last-respawns startup-failures
                             one-shot? transient?)
       (live-service provides requires one-shot?
                     transient? respawn?

                     enabled?
                     (or status (if running 'running 'stopped))
                     running
                     (or status-changes '())
                     (or last-respawns '())
                     (or startup-failures '()))))))

(define (highlight-if-long-transient-status service)
  "Return a procedure to highlight @var{service} if it's been stuck in a
transient status for too long."
  (if (memq (live-service-status service) '(starting stopping))
      (let ((duration (live-service-status-duration service)))
        (cond ((>= duration 30) highlight/warn)
              ((>= duration 60) highlight/error)
              (else identity)))
      identity))

(define (display-status-summary services)
  "Display a summary of the status of all of SERVICES."
  (define (service<? service1 service2)
    (string<? (symbol->string (live-service-canonical-name service1))
              (symbol->string (live-service-canonical-name service2))))

  (define (display-services header bullet services)
    (unless (null? services)
      (display header)
      (for-each (lambda (service)
                  (define highlight
                    (highlight-if-long-transient-status service))

                  (format #t " ~a ~a~%" bullet
                          (highlight
                           (symbol->string
                            (live-service-canonical-name service)))))
                (sort services service<?))))      ;get deterministic output

  (let* ((started  (filter (live-service-status-predicate 'running) services))
         (stopped  (filter (live-service-status-predicate 'stopped) services))
         (starting (filter (live-service-status-predicate 'starting) services))
         (stopping (filter (live-service-status-predicate 'stopping) services))
         (one-shot stopped (partition live-service-one-shot? stopped))
         (failing stopped  (partition live-service-failing? stopped)))
    (display-services (highlight (l10n "Started:\n")) "+"
                      started)
    (display-services (highlight (l10n "Starting:\n")) "^"
                      starting)

    (display-services (highlight (l10n "Stopped:\n")) "-"
                      stopped)
    (display-services (highlight (l10n "Stopping:\n")) "v"
                      stopping)

    ;; TRANSLATORS: Here "one-shot" refers to "one-shot services".  These are
    ;; services that are immediately marked as stopped once their 'start'
    ;; method has completed.
    (display-services (highlight (l10n "One-shot:\n")) "*"
                      one-shot)

    (display-services (highlight/error (l10n "Failed to start:\n")) "!"
                      failing)))

(define (display-detailed-status services)
  "Display the detailed status of SERVICES."
  (for-each display-service-status services))

(define (time->string time)
  "Return a string representing TIME in a concise, human-readable way."
  (define now*
    (current-time time-utc))

  (define now
    (time-second now*))

  (define elapsed
    (- now time))

  (define relative
    (cond ((< elapsed 120)
           (format #f (l10n "~a second ago" "~a seconds ago" elapsed)
                   elapsed))
          ((< elapsed 7200)
           (let ((minutes (inexact->exact
                           (round (/ elapsed 60)))))
             (format #f (l10n "~a minute ago" "~a minutes ago" minutes)
                     minutes)))
          ((< elapsed (* 48 3600))
           (let ((hours (inexact->exact
                         (round (/ elapsed 3600)))))
             (format #f (l10n "~a hour ago" "~a hours ago" hours)
                     hours)))
          (else
           (let ((days (inexact->exact
                        (round (/ elapsed (* 3600 24))))))
             (format #f (l10n "~a day ago" "~a days ago" days)
                     days)))))

  (define absolute
    (let* ((time*   (make-time time-utc 0 time))
           (date    (time-utc->date time*))
           (year    (date-year date))
           (now*    (time-utc->date now*))
           ;; Note: Use 'strftime' rather than 'date->string' to better
           ;; account for locale preferences.
           (format  (if (= year (date-year now*))
                        (if (= (date-day date) (date-day now*))
                            "%X"
                            "%c")
                        "%c")))
      (strftime format (localtime time))))

  ;; TRANSLATORS: The first placeholder is for a date string such as "April 22
  ;; 19:07:46" and the parenthesized placeholder is for the corresponding
  ;; relative date string like "2 hours ago".
  (format #f (l10n "~a (~a)") absolute relative))

(define (display-service-status service)
  "Display the status of SERVICE, an sexp."
  (format #t (highlight (l10n "Status of ~a:~%"))
          (live-service-canonical-name service))

  (match (live-service-status service)
    ('running
     (match (live-service-status-changes service)
       ((('running . time) . _)
        (if (live-service-transient? service)
            (format #t (l10n "  It is transient, running since ~a.~%")
                    (time->string time))
            (format #t (l10n "  It is running since ~a.~%")
                    (time->string time))))
       (_
        ;; Shepherd 0.9.x did not provide status change times.
        (if (live-service-transient? service)
            (format #t (l10n "  It is started and transient.~%"))
            (format #t (l10n "  It is started.~%")))))

     ;; TRANSLATORS: The "~s" bit is most of the time a placeholder
     ;; for the PID (an integer) of the running process, and
     ;; occasionally for another Scheme object.
     (format #t (l10n "  Running value is ~s.~%")
             (live-service-running-value service)))
    ('stopped
     (if (live-service-one-shot? service)
         (format #t (l10n "  It is stopped (one-shot).~%"))
         (if (pair? (live-service-startup-failures service))
             (format #t (highlight/error
                         (l10n "  It is stopped (failing).~%")))
             (match (live-service-status-changes service)
               ((('stopped . time) . _)
                (format #t (highlight/warn
                            (l10n "  It is stopped since ~a.~%"))
                        (time->string time)))
               (_
                (format #t (highlight/warn
                            (l10n "  It is stopped.~%"))))))))
    ('starting
     (let ((highlight (highlight-if-long-transient-status service)))
       (format #t (highlight (l10n "  It is starting.~%")))))
    ('stopping
     (let ((highlight (highlight-if-long-transient-status service)))
       (format #t (highlight (l10n "  It is being stopped.~%")))))
    (x
     (format #t (l10n "  Unknown status '~a'~%.") x)))

  (if (live-service-enabled? service)
      (format #t (l10n "  It is enabled.~%"))
      (let ((highlight (if (null? (live-service-last-respawns service))
                           highlight/warn
                           highlight/error)))
        (format #t (highlight (l10n "  It is disabled.~%")))))
  (format #t (l10n "  Provides ~a.~%") (live-service-provision service))
  (format #t (l10n "  Requires ~a.~%") (live-service-requirement service))
  (if (live-service-respawn? service)
      (format #t (l10n "  Will be respawned.~%"))
      (format #t (l10n "  Will not be respawned.~%")))
  (match (live-service-last-respawns service)
    ((time _ ...)
     (format #t (l10n "  Last respawned on ~a.~%")
             (time->string time)))
    (_ #t))
  (when (eq? (live-service-status service) 'stopped)
    (match (live-service-startup-failures service)
      ((time _ ...)
       (format #t (highlight/error (l10n "  Failed to start at ~a.~%"))
               (time->string time)))
      (_ #t))))

(define (display-event-log services)
  "Display status changes of @var{services} as a chronologically-sorted log."
  (define events
    (map (lambda (service)
           (fold-right
            (lambda (pair result)
              (match pair
                (('stopped . time)
                 (cons (if (live-service-one-shot? service)
                           (list time service 'stopped)
                           (match result
                             (((_ _ 'starting) . _)
                              ;; Transition from "starting" to "stopped"
                              ;; indicates a startup failure.
                              (list time service 'startup-failure))
                             (_
                              (list time service 'stopped))))
                       result))
                ((status . time)
                 (cons (list time service status)
                       result))))
            '()
            (live-service-status-changes service)))
         services))

  (define event>?
    (match-lambda*
      (((time1 . _) (time2 . _))
       (> time1 time2))))

  (define sorted
    ;; Each event list is already sorted, so merge them.  (They cannot be
    ;; resorted based on timestamps because there may be several events with
    ;; the same timestamps so resorting would lose causal ordering.)
    (reduce (lambda (events1 events2)
              (merge events1 events2 event>?))
            '()
            events))

  (when (null? sorted)
    ;; The Shepherd 0.9.x and earlier did not log service status changes.
    (report-error (l10n "event log is missing (shepherd is too old?)"))
    (exit 1))

  (for-each (match-lambda
              ((time service status)
               (let ((name (live-service-canonical-name service)))
                 (format #t "~a\t"
                         (date->string
                          (time-utc->date
                           (make-time time-utc 0 time))
                          "~e ~b ~Y ~H:~M:~S"))
                 (match status
                   ('running
                    (format #t (highlight (l10n "service ~a is running~%"))
                            name))
                   ('stopped
                    (cond ((live-service-one-shot? service)
                           (format #t (l10n "service ~a is done (one-shot)~%")
                                   name))
                          ((live-service-transient? service)
                           (format #t
                                   (highlight/warn
                                    (l10n "service ~a is done (transient)~%"))
                                   name))
                          (else
                           (format #t (highlight/warn
                                       (l10n "service ~a is stopped~%"))
                                   name))))
                   ('startup-failure
                    (format #t (highlight/error
                                (l10n "service ~a failed to start~%"))
                            name))
                   ('starting
                    (format #t (l10n "service ~a is being started~%")
                            name))
                   ('stopping
                    (format #t (l10n "service ~a is being stopped~%")
                            name))
                   (_
                    (format #t (l10n "service ~a is entering state '~a'~%")
                            name status))))))
            (reverse sorted)))

(define (display-service-graph services)
  "Write to the current output port a Graphviz representation of
@var{services}."
  (define registry
    (fold (lambda (service registry)
            (fold (cut vhash-consq <> service <>)
                  registry
                  (live-service-provision service)))
          vlist-null
          services))

  (define (lookup-service name)
    (match (vhash-assq name registry)
      (#f
       (report-error (l10n "inconsistent graph: service '~a' not found~%")
                     name)
       (exit 1))
      ((_ . service) service)))

  (define (emit-graphviz services)
    (define (shape service)
      (if (live-service-one-shot? service)
          "circle"
          "box"))
    (define (style service)
      (if (live-service-transient? service)
          "dashed"
          "solid"))
    (define (text-color service)
      (cond ((live-service-failing? service)
             "red")
            ((eq? (live-service-status service) 'stopped)
             "purple")
            ((eq? (live-service-status service) 'running)
             "black")
            (else
             "gray")))
    (define (color service)
      (cond ((live-service-failing? service)
             "red")
            ((eq? (live-service-status service) 'stopped)
             "purple")
            ((eq? (live-service-status service) 'running)
             "green")
            (else
             "gray")))

    (format #t "digraph ~s {~%" (l10n "Service Graph"))
    (for-each (lambda (service)
                (format #t "  \"~a\" [shape = ~a, color = ~a, \
fontcolor = ~a, style = ~a];~%"
                        (live-service-canonical-name service)
                        (shape service) (color service)
                        (text-color service) (style service))
                (for-each (lambda (dependency)
                            (format #t "  \"~a\" -> \"~a\";~%"
                                    (live-service-canonical-name service)
                                    (live-service-canonical-name
                                     (lookup-service dependency))))
                          (live-service-requirement service)))
              services)
    (format #t "}~%"))

  (emit-graphviz services))

(define root-service?
  ;; XXX: This procedure is written in a surprising way to work around a
  ;; compilation bug in Guile 3.0.5 to 3.0.7: <https://bugs.gnu.org/47172>.
  (let ((names (list 'root 'shepherd)))
    (lambda (service)
      (memq service names))))

(define (run-command socket-file action service args)
  "Perform ACTION with ARGS on SERVICE, and display the result.  Connect to
the daemon via SOCKET-FILE."
  (with-system-error-handling
   (let ((sock    (open-connection socket-file))
         (action* (if (and (memq action '(detailed-status log graph))
                           (root-service? service))
                      'status
                      action)))
     ;; Send the command.
     (write-command (shepherd-command action* service #:arguments args)
                    sock)

     ;; Receive output.  Interpret the command's output when possible and
     ;; format it in a human-readable way.
     (match (read sock)
       (('reply ('version 0 _ ...)                ;no errors
                ('result result) ('error #f)
                ('messages messages))
        ;; First, display raw messages coming from the daemon.  Since they are
        ;; not translated in the user's locale, they should be avoided!
        (for-each display-line messages)

        ;; Then interpret the result
        (match (list action service)
          (('status (or 'root 'shepherd))
           (display-status-summary
            (map sexp->live-service (first result))))
          (('detailed-status (or 'root 'shepherd))
           (display-detailed-status
            (map sexp->live-service (first result))))
          (('log (or 'root 'shepherd))
           (display-event-log
            (map sexp->live-service (first result))))
          (('graph (or 'root 'shepherd))
           (display-service-graph
            (map sexp->live-service (first result))))
          (('help (or 'root 'shepherd))
           (match result
             ((help-text)
              (display (gettext help-text))
              (newline))))
          (('eval (or 'root 'shepherd))
           (match result
             ((value)
              (write value)
              (newline))))
          (('status _)
           ;; We get a list of statuses, in case several services have the
           ;; same name.  FIXME: These days each name maps to exactly one
           ;; service so RESULT is always a singleton.
           (for-each (compose display-service-status sexp->live-service)
                     result))
          (('start _)
           (unless result
             (report-error (l10n "failed to start service ~a")
                           service)
             (exit 1)))
          (_
           ;; For other commands, exit successfully if and only if all the
           ;; values of RESULT are true.
           (unless (every ->bool result)
             (exit 1)))))
       (('reply ('version 0 _ ...)                ;an error
                ('result _) ('error error)
                ('messages messages))
        (for-each display-line messages)
        (report-command-error error)

        ;; Did the user swap ACTION and SERVICE?
        (match (list action service)
          ((_ (or 'start 'stop 'status 'doc))
           (report-error (l10n "Did you mean 'herd ~a ~a'?")
                         service action))
          ((root (or 'help 'halt 'power-off 'load 'eval 'unload 'reload
                     'daemonize 'restart))
           (report-error (l10n "Did you mean 'herd ~a ~a'?")
                         service action))
          ((_ _) *unspecified*))

        (exit 1))
       ((? eof-object?)
        ;; When stopping shepherd, we may get an EOF in lieu of a real reply,
        ;; and that's fine.  In other cases, a premature EOF is an error.
        (unless (and (eq? action 'stop)
                     (memq service '(root shepherd)))
          (report-error (l10n "premature end-of-file while \
talking to shepherd"))
          (exit 1))))

     (close-port sock))))


;; Main program.
(define (main . args)
  (initialize-cli)

  (parameterize ((program-name "herd"))
    (let ((socket-file default-socket-file)
          (command-args '()))
      (process-args (program-name) args
                    (l10n "ACTION SERVICE [ARG...]")
                    (l10n "Apply ACTION (start, stop, status, etc.) on \\
SERVICE with the ARGs.")
                    (lambda (arg)
                      ;; Collect unknown args.
                      (set! command-args (cons arg command-args)))
                    (option
                      #:long-name "socket" #:short-name #\s
                      #:takes-argument? #t #:argument-is-optional? #f
                      #:argument-name (l10n "FILE")
                      #:description (l10n "send commands to FILE")
                      #:action (lambda (file)
                                 (set! socket-file file))))

      (match (reverse command-args)
        (((and action
               (or "status" "detailed-status" "help" "log" "graph"))) ;one argument
         (run-command socket-file (string->symbol action) 'root '()))
        ((action service args ...)
         (run-command socket-file
                      (string->symbol action)
                      (string->symbol service) args))
        (_
         (format (current-error-port)
                 (l10n "Usage: herd ACTION [SERVICE [OPTIONS...]]~%"))
         (exit 1))))))

;; Local Variables:
;; eval: (put 'alist-let* 'scheme-indent-function 2)
;; End:
