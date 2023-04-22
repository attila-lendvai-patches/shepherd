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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-19)
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
                     (if (sloppy-assq 'transient? properties)
                         transient?
                         (and running *unspecified*))
                     respawn?

                     enabled?
                     (or status (if running 'running 'stopped))
                     running
                     (or status-changes '())
                     (or last-respawns '())
                     (or startup-failures '()))))))

(define (display-status-summary services)
  "Display a summary of the status of all of SERVICES."
  (define (service<? service1 service2)
    (string<? (symbol->string (live-service-canonical-name service1))
              (symbol->string (live-service-canonical-name service2))))

  (define (display-services header bullet services)
    (unless (null? services)
      (display header)
      (for-each (lambda (service)
                  (format #t " ~a ~a~%" bullet
                          (live-service-canonical-name service)))
                (sort services service<?))))      ;get deterministic output

  (let* ((started  (filter (live-service-status-predicate 'running) services))
         (stopped  (filter (live-service-status-predicate 'stopped) services))
         (starting (filter (live-service-status-predicate 'starting) services))
         (stopping (filter (live-service-status-predicate 'stopping) services))
         (one-shot stopped (partition live-service-one-shot? stopped))
         (failing stopped
                  (partition (compose pair? live-service-startup-failures)
                             stopped)))
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

(define (display-service-status service)
  "Display the status of SERVICE, an sexp."
  (define (timestamp->string time)
    (date->string
     (time-utc->date (make-time time-utc 0 time))))

  (format #t (highlight (l10n "Status of ~a:~%"))
          (live-service-canonical-name service))

  (match (live-service-status service)
    ('running
     (match (live-service-status-changes service)
       ((('running . time) . _)
        (if (live-service-transient? service)
            (format #t (l10n "  It is transient, running since ~a.~%")
                    (timestamp->string time))
            (format #t (l10n "  It is running since ~a.~%")
                    (timestamp->string time))))
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
                        (timestamp->string time)))
               (_
                (format #t (highlight/warn
                            (l10n "  It is stopped.~%"))))))))
    ('starting
     (format #t (l10n "  It is starting.~%")))
    ('stopping
     (format #t (l10n "  It is being stopped.~%")))
    (x
     (format #t (l10n "  Unknown status '~a'~%.") x)))

  (if (live-service-enabled? service)
      (format #t (l10n "  It is enabled.~%"))
      (format #t (highlight/warn (l10n "  It is disabled.~%"))))
  (format #t (l10n "  Provides ~a.~%") (live-service-provision service))
  (format #t (l10n "  Requires ~a.~%") (live-service-requirement service))
  (if (live-service-respawn? service)
      (format #t (l10n "  Will be respawned.~%"))
      (format #t (l10n "  Will not be respawned.~%")))
  (match (live-service-last-respawns service)
    ((time _ ...)
     (format #t (l10n "  Last respawned on ~a.~%")
             (timestamp->string time)))
    (_ #t))
  (when (eq? (live-service-status service) 'stopped)
    (match (live-service-startup-failures service)
      ((time _ ...)
       (format #t (highlight/error (l10n "  Failed to start at ~a.~%"))
               (timestamp->string time)))
      (_ #t))))

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
         (action* (if (and (eq? action 'detailed-status)
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
        (((and action (or "status" "detailed-status" "help"))) ;one argument
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
