;; service.scm -- Representation of services.
;; Copyright (C) 2013-2022 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Järling <wolfgang@pro-linux.de>
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Copyright (C) 2016 Alex Kost <alezost@gmail.com>
;; Copyright (C) 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
;; Copyright (C) 2019 Ricardo Wurmus <rekado@elephly.net>
;; Copyright (C) 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;; Copyright (C) 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (shepherd service)
  #:use-module ((fibers)
                #:hide (sleep))
  #:use-module (fibers scheduler)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs io ports)
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:autoload   (ice-9 ports internal) (port-read-wait-fd)
  #:autoload   (ice-9 rdelim) (read-line)
  #:autoload   (ice-9 pretty-print) (truncated-print)
  #:use-module (shepherd support)
  #:use-module (shepherd comm)
  #:use-module (shepherd config)
  #:use-module (shepherd system)
  #:export (<service>
            service?
            canonical-name
            running?
            one-shot?
            transient?
            action-list
            lookup-action
            defines-action?

            action?

            enable
            disable
            start
            start-in-the-background
            stop
            action
            enforce
            doc
            conflicts-with
            conflicts-with-running
            depends-resolved?
            launch-service
            first-running
            lookup-running
            lookup-running-or-providing
            make-service-group
            for-each-service
            lookup-services
            respawn-service
            handle-SIGCHLD
            %precious-signals
            register-services
            provided-by
            required-by
            handle-unknown

            default-service-termination-handler
            default-environment-variables
            make-forkexec-constructor
            make-kill-destructor
            exec-command
            fork+exec-command
            default-pid-file-timeout
            read-pid-file
            make-system-constructor
            make-system-destructor
            default-inetd-max-connections
            make-inetd-constructor
            make-inetd-destructor

            endpoint
            endpoint?
            endpoint-name
            endpoint-address
            endpoint-style
            endpoint-backlog
            endpoint-socket-owner
            endpoint-socket-group
            endpoint-socket-directory-permissions
            make-systemd-constructor
            make-systemd-destructor

            check-for-dead-services
            root-service
            make-actions

            &service-error
            service-error?
            &missing-service-error
            missing-service-error?
            missing-service-name

            &unknown-action-error
            unknown-action-error?
            unknown-action-name
            unknown-action-service

            &action-runtime-error
            action-runtime-error?
            action-runtime-error-service
            action-runtime-error-action
            action-runtime-error-key
            action-runtime-error-arguments

            condition->sexp))


(define sleep (@ (fibers) sleep))

;; Keep track of lazy initialization of SIGCHLD handler
(define %sigchld-handler-installed? #f)

;; Type of service actions.
(define-record-type <action>
  (make-action name proc doc)
  action?
  (name action-name)
  (proc action-procedure)
  (doc  action-documentation))

;; Conveniently create a list of <action> objects containing the actions for a
;; <service> object.
(define-syntax make-actions
  (syntax-rules ()
    ((_ (name docstring proc) rest ...)
     (cons (make-action 'name proc docstring)
           (make-actions rest ...)))
    ((_ (name proc) rest ...)
     (cons (make-action 'name proc "[No documentation.]")
           (make-actions rest ...)))
    ((_)
     '())))

;; Respawning CAR times in CDR seconds will disable the service.
;;
;; XXX: The terrible hack in (shepherd) using SIGALRM to work around
;; unreliable SIGCHLD delivery means that it might take up to 1 second for
;; SIGCHLD to be delivered.  Thus, arrange for the car to be lower than the
;; cdr.
(define respawn-limit '(5 . 7))

(define (respawn-limit-hit? respawns times seconds)
  "Return true of RESPAWNS, the list of times at which a given service was
respawned, shows that it has been respawned more than TIMES in SECONDS."
  (define now (current-time))

  ;; Note: This is O(TIMES), but TIMES is typically small.
  (let loop ((times    times)
             (respawns respawns))
    (match respawns
      (()
       #f)
      ((last-respawn rest ...)
       (or (zero? times)
           (and (> (+ last-respawn seconds) now)
                (loop (- times 1) rest)))))))

(define (default-service-termination-handler service status)
  "Handle the termination of @var{service} by respawning it if applicable.
Log abnormal termination reported by @var{status}."
  (unless (zero? status)
    ;; Most likely something went wrong; log it.
    (cond ((status:exit-val status)
           =>
           (lambda (code)
             (local-output (l10n "Service ~a (PID ~a) exited with ~a.")
                           (canonical-name service)
                           (service-running-value service) code)))
          ((status:term-sig status)
           =>
           (lambda (signal)
             (local-output (l10n "Service ~a (PID ~a) terminated with signal ~a.")
                           (canonical-name service)
                           (service-running-value service) signal)))
          ((status:stop-sig status)
           =>
           (lambda (signal)
             (local-output (l10n "Service ~a (PID ~a) stopped with signal ~a.")
                           (canonical-name service)
                           (service-running-value service) signal)))))

  (respawn-service service))

(define-class <service> ()
  ;; List of provided service-symbols.  The first one is also called
  ;; the `canonical name' and must be unique to this service.
  (provides #:init-keyword #:provides
	    #:getter provided-by)
  ;; List of required service-symbols.
  (requires #:init-keyword #:requires
	    #:init-value '()
	    #:getter required-by)
  ;; If true, the service is a "one-shot" service: it becomes marked as
  ;; stopped as soon as its 'start' method as completed, but services that
  ;; depend on it may be started.
  (one-shot? #:init-keyword #:one-shot?
             #:init-value #f
             #:getter one-shot?)
  ;; If true, the service is "transient": it is unregistered as soon as it
  ;; terminates, unless it is respawned.
  (transient? #:init-keyword #:transient?
              #:init-value #f
              #:getter transient?)
  ;; If `#t', then assume the `running' slot specifies a PID and
  ;; respawn it if that process terminates.  Otherwise `#f'.
  (respawn? #:init-keyword #:respawn?
	    #:init-value #f
	    #:getter respawn?)
  ;; The action to perform to start the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but it
  ;; must be possible to call it without any argument.  If the
  ;; starting attempt failed, it must return `#f'.  The return value
  ;; will be stored in the `running' slot.
  (start #:init-keyword #:start
	 #:init-value (lambda () #t))
  ;; The action to perform to stop the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but must
  ;; be callable with exactly one argument, which will be the value of
  ;; the `running' slot.  Whatever the procedure returns will be
  ;; ignored.
  (stop #:init-keyword #:stop
	#:init-value (lambda (running) #f))
  ;; Additional actions that can be performed with the service.  This
  ;; currently is a list with each element (and thus each action)
  ;; being ``(name . (proc . docstring))'', but users should not rely
  ;; on this.
  (actions #:init-keyword #:actions
	   #:init-form (make-actions))
  ;; If this is `#f', it means that the service is not running
  ;; currently.  Otherwise, it is the value that was returned by the
  ;; procedure in the `start' slot when the service was started.
  (running #:init-value #f)
  ;; Procedure called to notify that the process associated with this service
  ;; (whose PID is in the 'running' slot) has terminated.
  (handle-termination #:init-keyword #:handle-termination
                      #:init-value default-service-termination-handler)
  ;; A description of the service.
  (docstring #:init-keyword #:docstring
	     #:init-value "[No description].")
  ;; A service can be disabled if it is respawning too fast; it is
  ;; also possible to enable or disable it manually.
  (enabled? #:init-value #t
	    #:getter enabled?)
  ;; Some services should not be directly stopped, but should not be
  ;; respawned anymore instead.  This field indicates that we are in
  ;; the phase after the stop but before the termination.
  (waiting-for-termination? #:init-value #f)
  ;; This causes the above to be used.  When this is `#t', there is no
  ;; need for a destructor (i.e. no value in the `stop' slot).
  (stop-delay? #:init-keyword #:stop-delay?
	       #:init-value #f)
  ;; The times of the last respawns, most recent first.
  (last-respawns #:init-form '())
  ;; A replacement for when this service is stopped.
  (replacement #:init-keyword #:replacement
               #:init-value #f))

(define (service? obj)
  "Return true if OBJ is a service."
  (is-a? obj <service>))

;; Service errors.
(define-condition-type &service-error &error service-error?)

;; Error raised when looking up a service by name fails.
(define-condition-type &missing-service-error &service-error
  missing-service-error?
  (name missing-service-name))

(define-condition-type &unknown-action-error &service-error
  unknown-action-error?
  (service unknown-action-service)
  (action  unknown-action-name))

;; Report of an action throwing an exception in user code.
(define-condition-type &action-runtime-error &service-error
  action-runtime-error?
  (service   action-runtime-error-service)
  (action    action-runtime-error-action)
  (key       action-runtime-error-key)
  (arguments action-runtime-error-arguments))


(define (report-exception action service key args)
  "Report an exception of type KEY in user code ACTION of SERVICE."
  ;; FIXME: Would be nice to log it without sending the message to the client.
  (raise (condition (&action-runtime-error
                     (service service)
                     (action action)
                     (key key)
                     (arguments args)))))

(define (condition->sexp condition)
  "Turn the SRFI-35 error CONDITION into an sexp that can be sent over the
wire."
  (match condition
    ((? missing-service-error?)
     `(error (version 0) service-not-found
             ,(missing-service-name condition)))
    ((? unknown-action-error?)
     `(error (version 0) action-not-found
             ,(unknown-action-name condition)
             ,(canonical-name (unknown-action-service condition))))
    ((? action-runtime-error?)
     `(error (version 0) action-exception
             ,(action-runtime-error-action condition)
             ,(canonical-name (action-runtime-error-service condition))
             ,(action-runtime-error-key condition)
             ,(map result->sexp (action-runtime-error-arguments condition))))
    ((? service-error?)
     `(error (version 0) service-error))))

;; Return the canonical name of the service.
(define-method (canonical-name (obj <service>))
  (car (provided-by obj)))

;; Return the "running value" of OBJ.
(define-method (service-running-value (obj <service>))
  (match (slot-ref obj 'running)
    ((? procedure? proc) (proc))
    (value value)))

;; Return whether the service is currently running.
(define-method (running? (obj <service>))
  (and (service-running-value obj) #t))

;; Return a list of all actions implemented by OBJ.
(define-method (action-list (obj <service>))
  (map action-name (slot-ref obj 'actions)))

;; Return the action ACTION or #f if none was found.
(define-method (lookup-action (obj <service>) action)
  (find (match-lambda
          (($ <action> name)
           (eq? name action)))
        (slot-ref obj 'actions)))

;; Return whether OBJ implements the action ACTION.
(define-method (defines-action? (obj <service>) action)
  (and (lookup-action obj action) #t))

;; Enable the service, allow it to get started.
(define-method (enable (obj <service>))
  (slot-set! obj 'enabled? #t)
  (local-output (l10n "Enabled service ~a.") (canonical-name obj)))

;; Disable the service, make it unstartable.
(define-method (disable (obj <service>))
  (slot-set! obj 'enabled? #f)
  (local-output (l10n "Disabled service ~a.") (canonical-name obj)))

;; Start the service, including dependencies.
(define-method (start (obj <service>) . args)
  (cond ((running? obj)
	 (local-output (l10n "Service ~a is already running.")
		       (canonical-name obj))
         (service-running-value obj))
	((not (enabled? obj))
	 (local-output (l10n "Service ~a is currently disabled.")
		       (canonical-name obj))
         (service-running-value obj))
	((let ((conflicts (conflicts-with-running obj)))
	   (or (null? conflicts)
	       (local-output (l10n "Service ~a conflicts with running services ~a.")
			     (canonical-name obj)
			     (map canonical-name conflicts)))
	   (not (null? conflicts)))
	 (service-running-value obj))
	(else
	 ;; It is not running and does not conflict with anything
	 ;; that's running, so we can go on and launch it.
	 (let ((problem
		;; Resolve all dependencies.
		(find (negate start) (required-by obj))))
	   (if problem
	       (local-output (l10n "Service ~a depends on ~a.")
			     (canonical-name obj)
			     problem)
               ;; Start the service itself.
               (slot-set! obj 'running (catch #t
                                         (lambda ()
                                           (apply (slot-ref obj 'start)
                                                  args))
                                         (lambda (key . args)
                                           (report-exception 'start obj
                                                             key args)))))

	   ;; Status message.
           (let ((running (service-running-value obj)))
             (when (one-shot? obj)
               (slot-set! obj 'running #f))
             (local-output (if running
			       (l10n "Service ~a has been started.")
                               (l10n "Service ~a could not be started."))
			   (canonical-name obj))

             running)))))

(define (replace-service old-service new-service)
  "Replace OLD-SERVICE with NEW-SERVICE in the services registry.  This
completely removes all references to OLD-SERVICE before registering
NEW-SERVICE."
  (define (remove-service name)
    (let* ((old (hashq-ref %services name))
           (new (delete old-service old)))
      (if (null? new)
          (hashq-remove! %services name)
          (hashq-set! %services name new))))
  (when new-service
    (for-each remove-service (provided-by old-service))
    (register-services new-service)))

(define (required-by? service dependent)
  "Returns #t if DEPENDENT directly requires SERVICE in order to run.  Returns
#f otherwise."
  (and (find (lambda (dependency)
               (memq dependency (provided-by service)))
             (required-by dependent))
       #t))

;; Stop the service, including services that depend on it.  If the
;; latter fails, continue anyway.  Return `#f' if it could be stopped.
(define-method (stop (service <service>) . args)
  "Stop SERVICE, and any services which depend on it.  Returns a list of
canonical names for all of the services which have been stopped (including
transitive dependent services).  This method will print a warning if SERVICE
is not already running, and will return SERVICE's canonical name in a list."
  ;; Block asyncs so the SIGCHLD handler doesn't execute concurrently.
  ;; Notably, that makes sure the handler processes the SIGCHLD for SERVICE's
  ;; process once we're done; otherwise, it could end up respawning SERVICE.
  (call-with-blocked-asyncs
   (lambda ()
     (if (not (running? service))
         (begin
           (local-output (l10n "Service ~a is not running.")
                         (canonical-name service))
           (list (canonical-name service)))
         (if (slot-ref service 'stop-delay?)
             (begin
               (slot-set! service 'waiting-for-termination? #t)
               (local-output (l10n "Service ~a pending to be stopped.")
                             (canonical-name service))
               (list (canonical-name service)))
             (let ((name (canonical-name service))
                   (stopped-dependents (fold-services (lambda (other acc)
                                                        (if (and (running? other)
                                                                 (required-by? service other))
                                                            (append (stop other) acc)
                                                            acc))
                                                      '())))
               ;; Stop the service itself.
               (catch #t
                 (lambda ()
                   (apply (slot-ref service 'stop)
                          (service-running-value service)
                          args))
                 (lambda (key . args)
                   ;; Special case: 'root' may quit.
                   (and (eq? root-service service)
                        (eq? key 'quit)
                        (apply quit args))
                   (caught-error key args)))

               ;; SERVICE is no longer running.
               (slot-set! service 'running #f)

               ;; Reset the list of respawns.
               (slot-set! service 'last-respawns '())

               ;; Replace the service with its replacement, if it has one
               (let ((replacement (slot-ref service 'replacement)))
                 (when replacement
                   (replace-service service replacement)))

               ;; Status message.
               (if (running? service)
                   (local-output (l10n "Service ~a could not be stopped.")
                                 name)
                   (local-output (l10n "Service ~a has been stopped.")
                                 name))

               (when (transient? service)
                 (hashq-remove! %services (canonical-name service))
                 (local-output (l10n "Transient service ~a unregistered.")
                               (canonical-name service)))

               (cons name stopped-dependents)))))))

;; Call action THE-ACTION with ARGS.
(define-method (action (obj <service>) the-action . args)
  (define default-action
    ;; All actions which are handled here might be called even if the
    ;; service is not running, so they have to take this into account.
    (case the-action
      ;; Restarting is done in the obvious way.
      ((restart)
       (lambda (running . args)
         (let ((stopped-services (stop obj)))
           (for-each start stopped-services)
           #t)))
      ((status)
       ;; Return the service itself.  It is automatically converted to an sexp
       ;; via 'result->sexp' and sent to the client.
       (lambda (_) obj))
      ((enable)
       (lambda (_)
         (enable obj)))
      ((disable)
       (lambda (_)
         (disable obj)))
      ((doc)
       (lambda (_ . args)
         (apply doc obj args)))
      (else
       (lambda _
         ;; FIXME: Unknown service.
         (raise (condition (&unknown-action-error
                            (service obj)
                            (action the-action))))))))

  (let ((proc (or (and=> (lookup-action obj the-action)
                         action-procedure)
		  default-action)))
    ;; Invoking THE-ACTION is allowed even when the service is not running, as
    ;; it provides generally useful functionality and information.
    (catch #t
      (lambda ()
        (apply proc (service-running-value obj) args))
      (lambda (key . args)
        ;; Special case: 'root' may quit.
        (and (eq? root-service obj)
             (eq? key 'quit)
             (apply quit args))

        ;; Re-throw SRFI-34 exceptions that the caller will handle.
        (cond ((eq? key 'srfi-34)                 ;Guile 2.x
               (apply throw key args))
              ((eq? key '%exception)              ;Guile 3.x
               (raise-exception (car args)))
              (else
               (report-exception the-action obj key args)))))))

;; Display documentation about the service.
(define-method (doc (obj <service>) . args)
  (if (null? args)
      ;; No further argument given -> Normal level of detail.
      (local-output (slot-ref obj 'docstring))
    (case (string->symbol (car args)) ;; Does not work with strings.
      ((full)
       ;; FIXME
       (local-output (slot-ref obj 'docstring)))
      ((short)
       ;; FIXME
       (local-output (slot-ref obj 'docstring)))
      ((action)
       ;; Display documentation of given actions.
       (for-each
	(lambda (the-action)
          (let ((action-object
                 (lookup-action obj (string->symbol the-action))))
            (unless action-object
              (raise (condition (&unknown-action-error
                                 (action the-action)
                                 (service obj)))))
            (local-output "~a: ~a" the-action
                          (action-documentation action-object))))
        (cdr args)))
      ((list-actions)
       (local-output "~a ~a"
		     (canonical-name obj)
		     (action-list obj)))
      (else
       ;; FIXME: Implement doc-help.
       (local-output (l10n "Unknown keyword.  Try 'doc root help'."))))))

;; Return a list of services that conflict with OBJ.
(define-method (conflicts-with (obj <service>))
  (delete-duplicates
   (append-map (lambda (sym)
                 (filter-map (lambda (service)
                               (and (not (eq? service obj))
                                    service))
                             (lookup-services sym)))
               (provided-by obj))
   eq?))

;; Check if this service provides a symbol that is already provided
;; by any other running services.  If so, return these services.
;; Otherwise, return the empty list.
(define-method (conflicts-with-running (obj <service>))
  (filter running? (conflicts-with obj)))

;; Start OBJ, but first kill all services which conflict with it.
;; FIXME-CRITICAL: Conflicts of indirect dependencies.  For this, we
;; seem to need a similar solution like launch-service.
;; FIXME: This should rather be removed and added cleanly later.
(define-method (enforce (obj <service>) . args)
  (for-each stop (conflicts-with-running obj))
  (apply start obj args))

(define (service->sexp service)
  "Return a representation of SERVICE as an sexp meant to be consumed by
clients."
  `(service (version 0)                           ;protocol version
            (provides ,(provided-by service))
            (requires ,(required-by service))
            (respawn? ,(respawn? service))
            (docstring ,(slot-ref service 'docstring))

            ;; Status.  Use 'result->sexp' for the running value to make sure
            ;; that whole thing is valid read syntax; we do not want things
            ;; like #<undefined> to be sent to the client.
            (enabled? ,(enabled? service))
            (running ,(result->sexp (service-running-value service)))
            (conflicts ,(map canonical-name (conflicts-with service)))
            (last-respawns ,(slot-ref service 'last-respawns))
            ,@(if (slot-ref service 'one-shot?)
                  '((one-shot? #t))
                  '())
            ,@(if (slot-ref service 'transient?)
                  '((transient? #t))
                  '())))

(define-method (result->sexp (service <service>))
  ;; Serialize SERVICE to an sexp.
  (service->sexp service))

;; Return whether OBJ requires something that is not yet running.
(define-method (depends-resolved? (obj <service>))
  (every lookup-running (required-by obj)))



(define (launch-service name proc args)
  "Try to start (with PROC) a service providing NAME; return #f on failure.
Used by `start' and `enforce'."
  (match (lookup-services name)
    (()
     (raise (condition (&missing-service-error (name name)))))
    ((possibilities ...)
     (or (first-running possibilities)

         ;; None running yet, start one.
         (find (lambda (service)
                 (apply proc service args))
               possibilities)

         ;; Failed to start something, try the 'unknown' service.
         (let ((unknown (lookup-running 'unknown)))
           (if (and unknown
                    (defines-action? unknown 'start))
               (apply action unknown 'start name args)
               #f))))))

;; Starting by name.
(define-method (start (obj <symbol>) . args)
  (launch-service obj start args))

;; Enforcing by name.  FIXME: Should be removed and added cleanly later.
(define-method (enforce (obj <symbol>) . args)
  (launch-service obj enforce args))

;; Stopping by name.
(define-method (stop (obj <symbol>) . args)
  (let ((which (lookup-running obj)))
    (if which
	(apply stop which args)
        (let ((unknown (lookup-running 'unknown)))
	  (if (and unknown
		   (defines-action? unknown 'stop))
	      (apply action unknown 'stop obj args)
              ;; Only print an error if the service does not exist.
              (match (lookup-services obj)
                (()
                 (raise (condition (&missing-service-error (name obj)))))
                ((stopped . _)
                 (list))))))))

(define-method (action (obj <symbol>) the-action . args)
  "Perform THE-ACTION on all the services named OBJ.  Return the list of
results."
  (let ((which-services (lookup-running-or-providing obj)))
    (if (null? which-services)
	(let ((unknown (lookup-running 'unknown)))
	  (if (and unknown
		   (defines-action? unknown 'action))
	      (apply action unknown 'action the-action args)
              (raise (condition (&missing-service-error (name obj))))))
        (map (lambda (service)
               (apply action service the-action args))
             which-services))))

(define (start-in-the-background services)
  "Start the services named by @var{services}, a list of symbols, in the
background.  In other words, this procedure returns immediately without
waiting until all of @var{services} have been started.

This procedure can be useful in a configuration file because it lets you
interact right away with shepherd using the @command{herd} command."
  (spawn-fiber
   (lambda ()
     (for-each (lambda (service)
                 ;; Keep going if one of SERVICES fails to start.
                 (guard (c ((service-error? c)
                            (local-output
                             (l10n "Failed to start ~a in the background.")
                             service)))
                   (start service)))
               services)))

  ;; 'spawn-fiber' returns zero values, which can confuse callees; return one.
  *unspecified*)



;; Handling of unprovided service-symbols.  This can be called in
;; either of the following ways (i.e. with either three or four
;; arguments):
;;   handle-unknown SERVICE-SYMBOL [ 'start | 'stop ] ARGS
;;   handle-unknown SERVICE-SYMBOL 'action THE_ACTION ARGS
(define (handle-unknown . args)
  (let ((unknown (lookup-running 'unknown)))
    ;; FIXME: Display message if no unknown service.
    (when unknown
      (apply (case-lambda
	       ;; Start or stop.
	       ((service-symbol start/stop args)
	        (if (defines-action? unknown start/stop)
		    (apply action unknown start/stop service-symbol args)
		    ;; FIXME: Bad message.
		    (local-output "Cannot ~a ~a." start/stop service-symbol)))
	       ;; Action.
	       ((service-symbol action-sym the-action args)
	        (assert (eq? action-sym 'action))
	        (if (defines-action? unknown 'action)
		    (apply action unknown 'action service-symbol
                           the-action args)
		    (local-output (l10n "No service provides ~a.")
                                  service-symbol))))
             args))))

;; Check if any of SERVICES is running.  If this is the case, return
;; it.  If none, return `#f'.  Only the first one found will be
;; returned; this is because this is mainly intended to be applied on
;; the return value of `lookup-services', where no more than one will
;; ever run at the same time.
(define (first-running services)
  (find running? services))

;; Return the running service that provides NAME, or false if none.
(define (lookup-running name)
  (first-running (lookup-services name)))

;; Lookup the running service providing SYM, and return it as a
;; one-element list.  If none is running, return a list of all
;; services which provide SYM.
(define (lookup-running-or-providing sym)
  (match (lookup-running sym)
    ((? service? service)
     (list service))
    (#f
     (lookup-services sym))))


;;;
;;; Starting/stopping services.
;;;

(define (default-service-directory)
  "Return the default current directory from which a service is started."
  (define (ensure-valid directory)
    (if (and (file-exists? directory)
             (file-is-directory? directory))
        directory
        "/"))

  (if (zero? (getuid))
      "/"
      (ensure-valid (or (getenv "HOME")
                        (and=> (catch-system-error (getpw (getuid)))
                               passwd:dir)
                        (getcwd)))))

(define default-environment-variables
  ;; The default list of environment variable name/value pairs that should be
  ;; set when starting a service.
  (make-parameter (environ)))

(define default-pid-file-timeout
  ;; Maximum number of seconds to wait for a PID file to show up.
  (make-parameter 5))

(define* (read-pid-file file #:key (max-delay (default-pid-file-timeout))
                        (validate-pid? #f))
  "Wait for MAX-DELAY seconds for FILE to show up, and read its content as a
number.  Return #f if FILE was not created or does not contain a number;
otherwise return the number that was read (a PID).

When VALIDATE-PID? is true, succeed if and only if the number that was read is
the PID of an existing process in the current PID namespace.  This test cannot
be used if FILE might contain a PID from another PID namespace--i.e., the
daemon writing FILE is running in a separate PID namespace."
  (define start (current-time))

  (define (sleep* n)
    ;; In general we want to use (@ (fibers) sleep) to yield to the scheduler.
    ;; However, this code might be non-suspendable--e.g., if the user calls
    ;; the 'start' method right from their config file, which is loaded with
    ;; 'primitive-load', which is a continuation barrier.  Thus, this variant
    ;; checks whether it can suspend and picks the right 'sleep'.
    (if (yield-current-task)
        (begin
          (set! sleep* (@ (fibers) sleep))
          (sleep n))
        (begin
          (set! sleep* (@ (guile) sleep))
          ((@ (guile) sleep) n))))

  (let loop ()
    (define (try-again)
      (and (< (current-time) (+ start max-delay))
           (begin
             ;; FILE does not exist yet, so wait and try again.
             (sleep* 1)                         ;yield to the Fibers scheduler
             (loop))))

    (catch 'system-error
      (lambda ()
        (match (string->number
                (string-trim-both
                 (call-with-input-file file get-string-all)))
          (#f
           ;; If we didn't get an integer, it may be because the daemon didn't
           ;; create FILE atomically and isn't done writing to it.  Try again.
           (try-again))
          ((? integer? pid)
           ;; It's possible, though unlikely, that PID is not a valid PID, for
           ;; instance because writes to FILE did not complete.  When
           ;; VALIDATE-PID? is true, check that PID is valid in the current
           ;; PID namespace.
           (if (or (not validate-pid?)
                   (catch-system-error (kill pid 0) #t))
               pid
               (try-again)))))
      (lambda args
        (let ((errno (system-error-errno args)))
          (if (= ENOENT errno)
              (try-again)
              (apply throw args)))))))

(define (%service-file-logger file input)
  "Like 'service-file-logger', but doesn't handle the case in which FILE does
not exist."
  (let* ((fd     (open-fdes file (logior O_CREAT O_WRONLY O_APPEND O_CLOEXEC)
                            #o640))
         (output (fdopen fd "al")))
    (set-port-encoding! output "UTF-8")
    (set-port-conversion-strategy! output 'substitute)
    (lambda ()
      (call-with-port output
        (lambda (output)
          (let loop ()
            (match (read-line input)
              ((? eof-object?)
               (close-port input)
               (close-port output))
              (line
               (let ((prefix (strftime default-logfile-date-format
                                       (localtime (current-time)))))
                 (format output "~a~a~%" prefix line)
                 (loop))))))))))

(define (service-file-logger file input)
  "Return a thunk meant to run as a fiber that reads from INPUT and logs it to
FILE."
  (catch 'system-error
    (lambda ()
      (%service-file-logger file input))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          (begin
            (mkdir-p (dirname file))
            (%service-file-logger file input))
          (apply throw args)))))

(define (service-builtin-logger command input)
  "Return a thunk meant to run as a fiber that reads from INPUT and logs to
'log-output-port'."
  (lambda ()
    (let loop ()
      (match (read-line input)
        ((? eof-object?)
         (close-port input))
        (line
         (let ((prefix (strftime (%current-logfile-date-format)
                                 (localtime (current-time)))))
           ;; TODO: Print the PID of COMMAND.  The actual PID is potentially
           ;; not known until after 'read-pid-file' has completed, so it would
           ;; need to be communicated.
           (format (log-output-port) "~a[~a] ~a~%"
                   prefix command line))
         (loop))))))

(define (format-supplementary-groups supplementary-groups)
  (list->vector (map (lambda (group) (group:gid (getgr group)))
                     supplementary-groups)))

(define* (exec-command command
                       #:key
                       (user #f)
                       (group #f)
                       (supplementary-groups '())
                       (log-file #f)
                       (log-port #f)
                       (input-port #f)
                       (extra-ports '())
                       (directory (default-service-directory))
                       (file-creation-mask #f)
                       (create-session? #t)
                       (environment-variables (default-environment-variables))
                       (resource-limits '()))
  "Run COMMAND as the current process from DIRECTORY, with FILE-CREATION-MASK
if it's true, and with ENVIRONMENT-VARIABLES (a list of strings like
\"PATH=/bin\").  File descriptors 1 and 2 are kept as is or redirected to
either LOG-PORT or LOG-FILE if it's true, whereas file descriptor 0 (standard
input) points to INPUT-PORT or /dev/null.

EXTRA-PORTS are made available starting from file descriptor 3 onwards; all
other file descriptors are closed prior to yielding control to COMMAND.  When
CREATE-SESSION? is true, call 'setsid' first.

Guile's SETRLIMIT procedure is applied on the entries in RESOURCE-LIMITS.  For
example, a valid value would be '((nproc 10 100) (nofile 4096 4096)).

By default, COMMAND is run as the current user.  If the USER keyword
argument is present and not false, change to USER immediately before
invoking COMMAND.  USER may be a string, indicating a user name, or a
number, indicating a user ID.  Likewise, COMMAND will be run under the
current group, unless the GROUP keyword argument is present and not
false."
  (match command
    ((program args ...)
     (when create-session?
       ;; Become the leader of a new session and session group.
       ;; Programs such as 'mingetty' expect this.
       (setsid))

     (for-each (cut apply setrlimit <>) resource-limits)

     (chdir directory)
     (environ environment-variables)

     ;; Close all the file descriptors except stdout and stderr.
     (let ((max-fd (max-file-descriptors)))

       ;; Redirect stdin.
       (catch-system-error (close-fdes 0))
       ;; Make sure file descriptor zero is used, so we don't end up reusing
       ;; it for something unrelated, which can confuse some packages.
       (dup2 (if input-port
                 (fileno input-port)
                 (open-fdes "/dev/null" O_RDONLY))
             0)

       (when (or log-port log-file)
         (catch #t
           (lambda ()
             ;; Redirect stout and stderr to use LOG-FILE.
             (catch-system-error (close-fdes 1))
             (catch-system-error (close-fdes 2))
             (dup2 (if log-file
                       (open-fdes log-file (logior O_CREAT O_WRONLY O_APPEND)
                                  #o640)
                       (fileno log-port))
                   1)
             (dup2 1 2)

             ;; Make EXTRA-PORTS available starting from file descriptor 3.
             ;; This clears their FD_CLOEXEC flag.
             (let loop ((fd    3)
                        (ports extra-ports))
               (match ports
                 (() #t)
                 ((port rest ...)
                  (catch-system-error (close-fdes fd))
                  (dup2 (fileno port) fd)
                  (loop (+ 1 fd) rest)))))

           (lambda (key . args)
             (when log-file
               (format (current-error-port)
                       "failed to open log-file ~s:~%" log-file))
             (print-exception (current-error-port) #f key args)
             (primitive-exit 1))))

     ;; setgid must be done *before* setuid, otherwise the user will
     ;; likely no longer have permissions to setgid.
     (when group
       (catch #t
         (lambda ()
           ;; Clear supplementary groups.
           (setgroups (format-supplementary-groups supplementary-groups))
           (setgid (group:gid (getgr group))))
         (lambda (key . args)
           (format (current-error-port)
                   "failed to change to group ~s:~%" group)
           (print-exception (current-error-port) #f key args)
           (primitive-exit 1))))

     (when user
       (catch #t
         (lambda ()
           (setuid (passwd:uid (getpw user))))
         (lambda (key . args)
           (format (current-error-port)
                   "failed to change to user ~s:~%" user)
           (print-exception (current-error-port) #f key args)
           (primitive-exit 1))))

     (when file-creation-mask
       (umask file-creation-mask))

     ;; Last, close all file descriptors.  Do that after shutting down the
     ;; finalization thread since we will close its pipe, leading to
     ;; "error in the finalization thread: Bad file descriptor".
     (without-automatic-finalization
      (let loop ((i (+ 3 (length extra-ports))))
        (when (< i max-fd)
          (catch-system-error (close-fdes i))
          (loop (+ i 1))))

      (catch 'system-error
        (lambda ()
          (apply execlp program program args))
        (lambda args
          (format (current-error-port)
                  "exec of ~s failed: ~a~%"
                  program (strerror (system-error-errno args)))
          (primitive-exit 1))))))))

(define %precious-signals
  ;; Signals that the shepherd process handles.
  (list SIGCHLD SIGINT SIGHUP SIGTERM))

(define* (fork+exec-command command
                            #:key
                            (user #f)
                            (group #f)
                            (supplementary-groups '())
                            (log-file #f)
                            (log-encoding "UTF-8")
                            (extra-ports '())
                            (directory (default-service-directory))
                            (file-creation-mask #f)
                            (create-session? #t)
                            (environment-variables
                             (default-environment-variables))
                            (listen-pid-variable? #f)
                            (resource-limits '()))
  "Spawn a process that executes @var{command} as per @code{exec-command}, and
return its PID.  When @var{listen-pid-variable?} is true, augment
@var{environment-variables} with a definition of the @env{LISTEN_PID}
environment variable used for systemd-style \"socket activation\"."
  ;; Install the SIGCHLD handler if this is the first fork+exec-command call.
  (unless %sigchld-handler-installed?
    (sigaction SIGCHLD handle-SIGCHLD SA_NOCLDSTOP)
    (set! %sigchld-handler-installed? #t))

  ;; Child processes inherit signal handlers until they exec.  If one of
  ;; %PRECIOUS-SIGNALS is received by the child before it execs, the installed
  ;; handler, which stops shepherd, is called.  To avoid this, block signals
  ;; so that the child process never executes those handlers.
  (with-blocked-signals %precious-signals
    (match (pipe2 O_CLOEXEC)
      ((log-input . log-output)
       (let ((pid (primitive-fork)))
         (if (zero? pid)
             (begin
               ;; First restore the default handlers.
               (for-each (cut sigaction <> SIG_DFL) %precious-signals)

               ;; Unblock any signals that have been blocked by the parent
               ;; process.
               (unblock-signals %precious-signals)

               (close-port log-input)
               (exec-command command
                             #:user user
                             #:group group
                             #:supplementary-groups supplementary-groups
                             #:log-port log-output
                             #:extra-ports extra-ports
                             #:directory directory
                             #:file-creation-mask file-creation-mask
                             #:create-session? create-session?
                             #:environment-variables
                             (if listen-pid-variable?
                                 (cons (string-append "LISTEN_PID="
                                                      (number->string (getpid)))
                                       environment-variables)
                                 environment-variables)
                             #:resource-limits resource-limits))
             (let ((log-input (non-blocking-port log-input)))
               (close-port log-output)

               (when log-encoding
                 (set-port-encoding! log-input log-encoding))

               ;; Do not crash when LOG-INPUT contains data that does not
               ;; conform LOG-ENCODING.  XXX: The 'escape strategy would be
               ;; nicer but it's not implemented in (ice-9 suspendable-ports):
               ;; <https://issues.guix.gnu.org/54538>.
               (set-port-conversion-strategy! log-input 'substitute)

               (spawn-fiber
                (if log-file
                    (service-file-logger log-file log-input)
                    (service-builtin-logger (match command
                                              ((command . _)
                                               (basename command)))
                                            log-input)))
               pid)))))))

(define* (make-forkexec-constructor command
                                    #:key
                                    (user #f)
                                    (group #f)
                                    (supplementary-groups '())
                                    (log-file #f)
                                    (directory (default-service-directory))
                                    (file-creation-mask #f)
                                    (create-session? #t)
                                    (environment-variables
                                     (default-environment-variables))
                                    (resource-limits '())
                                    (pid-file #f)
                                    (pid-file-timeout
                                     (default-pid-file-timeout)))
  "Return a procedure that forks a child process, closes all file
descriptors except the standard output and standard error descriptors, sets
the current directory to @var{directory}, sets the umask to
@var{file-creation-mask} unless it is @code{#f}, changes the environment to
@var{environment-variables} (using the @code{environ} procedure), sets the
current user to @var{user} and the current group to @var{group} unless they
are @code{#f}, and executes @var{command} (a list of strings.)  When
@var{create-session?} is true, the child process creates a new session with
'setsid' and becomes its leader.  The result of the procedure will be the
 PID of the child process.

When @var{pid-file} is true, it must be the name of a PID file associated with
the process being launched; the return value is the PID read from that file,
once that file has been created.  If @var{pid-file} does not show up in less
than @var{pid-file-timeout} seconds, the service is considered as failing to
start."
  (lambda args
    (define (clean-up file)
      (when file
        (catch 'system-error
          (lambda ()
            (delete-file file))
          (lambda args
            (unless (= ENOENT (system-error-errno args))
              (apply throw args))))))

    (clean-up pid-file)

    (let ((pid (fork+exec-command command
                                  #:user user
                                  #:group group
                                  #:supplementary-groups supplementary-groups
                                  #:log-file log-file
                                  #:directory directory
                                  #:file-creation-mask file-creation-mask
                                  #:create-session? create-session?
                                  #:environment-variables
                                  environment-variables
                                  #:resource-limits resource-limits)))
      (if pid-file
          (match (read-pid-file pid-file
                                #:max-delay pid-file-timeout
                                #:validate-pid? #t)
            (#f
             ;; Send SIGTERM to the whole process group.
             (catch-system-error (kill (- pid) SIGTERM))
             #f)
            ((? integer? pid)
             pid))
          pid))))

(define* (make-kill-destructor #:optional (signal SIGTERM))
  "Return a procedure that sends SIGNAL to the process group of the PID given
as argument, where SIGNAL defaults to `SIGTERM'."
  (lambda (pid . args)
    ;; Kill the whole process group PID belongs to.  Don't assume that PID is
    ;; a process group ID: that's not the case when using #:pid-file, where
    ;; the process group ID is the PID of the process that "daemonized".  If
    ;; this procedure is called, between the process fork and exec, the PGID
    ;; will still be zero (the Shepherd PGID). In that case, use the PID.
    (let ((pgid (getpgid pid)))
      (if (= (getpgid 0) pgid)
          (kill pid signal) ;don't kill ourself
          (kill (- pgid) signal)))
    #f))

;; Produce a constructor that executes a command.
(define (make-system-constructor . command)
  (lambda args
    (zero? (status:exit-val (system (apply string-append command))))))

;; Produce a destructor that executes a command.
(define (make-system-destructor . command)
  (lambda (ignored . args)
    (not (zero? (status:exit-val (system (apply string-append command)))))))


;;;
;;; Server endpoints.
;;;

;; Endpoint of a systemd-style or inetd-style service.
(define-record-type <endpoint>
  (make-endpoint name address style backlog owner group permissions)
  endpoint?
  (name        endpoint-name)                          ;string
  (address     endpoint-address)                       ;socket address
  (style       endpoint-style)                         ;SOCK_STREAM, etc.
  (backlog     endpoint-backlog)                       ;integer
  (owner       endpoint-socket-owner)                  ;integer
  (group       endpoint-socket-group)                  ;integer
  (permissions endpoint-socket-directory-permissions)) ;integer

(define* (endpoint address
                   #:key (name "unknown") (style SOCK_STREAM)
                   (backlog 128)
                   (socket-owner (getuid)) (socket-group (getgid))
                   (socket-directory-permissions #o755))
  "Return a new endpoint called @var{name} of @var{address}, an address as
return by @code{make-socket-address}, with the given @var{style} and
@var{backlog}.

When @var{address} is of type @code{AF_INET6}, the endpoint is
@emph{IPv6-only}.  Thus, if you want a service available both on IPv4 and
IPv6, you need two endpoints.

When @var{address} is of type @code{AF_UNIX}, @var{socket-owner} and
@var{socket-group} are strings or integers that specify its ownership and that
of its parent directory; @var{socket-directory-permissions} specifies the
permissions for its parent directory."
  (make-endpoint name address style backlog
                 socket-owner socket-group
                 socket-directory-permissions))

(define (endpoint->listening-socket endpoint)
  "Return a listening socket for ENDPOINT."
  (match endpoint
    (($ <endpoint> name address style backlog
                   owner group permissions)
     ;; Make listening sockets SOCK_CLOEXEC: inetd-style services don't pass
     ;; them to the child process, and systemd-style do pass them but call
     ;; 'dup2' right before 'exec', thereby clearing this property.
     (let* ((sock    (socket (sockaddr:fam address)
                             (logior SOCK_NONBLOCK SOCK_CLOEXEC style)
                             0))
            (owner   (if (integer? owner)
                         owner
                         (passwd:uid (getpwnam owner))))
            (group   (if (integer? group)
                         group
                         (group:gid (getgrnam group)))))
       (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
       (when (= AF_INET6 (sockaddr:fam address))
         ;; Interpret AF_INET6 endpoints as IPv6-only.  This is contrary to
         ;; the Linux defaults where listening on an IPv6 address also listens
         ;; on its IPv4 counterpart.
         (ipv6-only sock))
       (when (= AF_UNIX (sockaddr:fam address))
         (mkdir-p (dirname (sockaddr:path address)) permissions)
         (chown (dirname (sockaddr:path address)) owner group)
         (catch-system-error (delete-file (sockaddr:path address))))

       (bind sock address)
       (listen sock backlog)

       (when (= AF_UNIX (sockaddr:fam address))
         (chown sock owner group)
         (chmod sock #o666))

       sock))))

(define (open-sockets endpoints)
  "Return a list of listening sockets corresponding to ENDPOINTS, in the same
order as ENDPOINTS.  If opening of binding one of them fails, an exception is
thrown an previously-opened sockets are closed."
  (let loop ((endpoints endpoints)
             (result   '()))
    (match endpoints
      (()
       (reverse result))
      ((head tail ...)
       (let ((sock (catch 'system-error
                     (lambda ()
                       (endpoint->listening-socket head))
                     (lambda args
                       ;; When opening one socket fails, abort the whole
                       ;; process.
                       (for-each (match-lambda
                                   ((_ . socket) (close-port socket)))
                                 result)
                       (apply throw args)))))
         (loop tail (cons sock result)))))))

(define-syntax-rule (define-as-needed name value)
  (unless (defined? 'name)
    (module-define! (current-module) 'name value)
    (module-export! (current-module) '(name))))

;; These values are not defined as of Guile 3.0.8.  Provide them as a
;; convenience.
(define-as-needed IN6ADDR_LOOPBACK 1)
(define-as-needed IN6ADDR_ANY 0)


;;;
;;; Inetd-style services.
;;;

(define* (make-inetd-forkexec-constructor command connection
                                          #:key
                                          (user #f)
                                          (group #f)
                                          (supplementary-groups '())
                                          (directory (default-service-directory))
                                          (file-creation-mask #f)
                                          (create-session? #t)
                                          (environment-variables
                                           (default-environment-variables))
                                          (resource-limits '()))
  (lambda ()
    ;; XXX: This is partly copied from 'make-forkexec-constructor'.
    ;; Install the SIGCHLD handler if this is the first fork+exec-command call.
    (unless %sigchld-handler-installed?
      (sigaction SIGCHLD handle-SIGCHLD SA_NOCLDSTOP)
      (set! %sigchld-handler-installed? #t))

    (with-blocked-signals %precious-signals
      (let ((pid (primitive-fork)))
        (if (zero? pid)
            (begin
              ;; First restore the default handlers.
              (for-each (cut sigaction <> SIG_DFL) %precious-signals)

              ;; Unblock any signals that have been blocked by the parent
              ;; process.
              (unblock-signals %precious-signals)

              (exec-command command
                            #:input-port connection
                            #:log-port connection
                            #:user user
                            #:group group
                            #:supplementary-groups supplementary-groups
                            #:directory directory
                            #:file-creation-mask file-creation-mask
                            #:create-session? create-session?
                            #:environment-variables
                            environment-variables
                            #:resource-limits resource-limits))
            (begin
              (close-port connection)
              pid))))))

(define (socket-address->string address)
  "Return a human-readable representation of ADDRESS, an object as returned by
'make-socket-address'."
  (let ((family (sockaddr:fam address)))
    (cond ((= AF_INET family)
           (string-append (inet-ntop AF_INET (sockaddr:addr address))
                          ":" (number->string (sockaddr:port address))))
          ((= AF_INET6 family)
           (string-append "[" (inet-ntop AF_INET6 (sockaddr:addr address)) "]"
                          ":" (number->string (sockaddr:port address))))
          ((= AF_UNIX family)
           (sockaddr:path address))
          (else
           (object->string address)))))

(define (inetd-variables server client)
  "Return environment variables that inetd would defined for a connection of
@var{client} to @var{server} (info \"(inetutils) Inetd Environment\")."
  (let ((family (sockaddr:fam server)))
    (if (memv family (list AF_INET AF_INET6))
        (list (string-append "TCPLOCALIP="
                             (inet-ntop family (sockaddr:addr server)))
              (string-append "TCPLOCALPORT="
                             (number->string (sockaddr:port server)))
              (string-append "TCPREMOTEIP="
                             (inet-ntop (sockaddr:fam client)
                                        (sockaddr:addr client)))
              (string-append "TCPREMOTEPORT"
                             (number->string (sockaddr:port client))))
        '())))

(define default-inetd-max-connections
  ;; Default maximum number of simultaneous connections for an inetd-style
  ;; service.
  (make-parameter 100))

(define* (make-inetd-constructor command endpoints
                                 #:key
                                 (service-name-stem
                                  (match command
                                    ((program . _)
                                     (basename program))))
                                 (requirements '())
                                 (max-connections
                                  (default-inetd-max-connections))
                                 (user #f)
                                 (group #f)
                                 (supplementary-groups '())
                                 (directory (default-service-directory))
                                 (file-creation-mask #f)
                                 (create-session? #t)
                                 (environment-variables
                                  (default-environment-variables))
                                 (resource-limits '())

                                 ;; Deprecated.
                                 (socket-style SOCK_STREAM)
                                 (socket-owner (getuid))
                                 (socket-group (getgid))
                                 (socket-directory-permissions #o755)
                                 (listen-backlog 10))
  "Return a procedure that opens sockets listening to @var{endpoints}, a list
of objects as returned by @code{endpoint}, and accepting connections in the
background.

Upon a client connection, a transient service running @var{command} is
spawned.  Only up to @var{max-connections} simultaneous connections are
accepted; when that threshold is reached, new connections are immediately
closed.

The remaining arguments are as for @code{make-forkexec-constructor}."
  (define child-service-name
    (let ((counter 1))
      (lambda ()
        (define name
          (string->symbol
           (string-append service-name-stem "-" (number->string counter))))
        (set! counter (+ 1 counter))
        name)))

  (define connection-count
    ;; Number of active connections.
    0)

  (define (handle-child-termination service status)
    (set! connection-count (- connection-count 1))
    (local-output (l10n "~a connection still in use after ~a termination."
                        "~a connections still in use after ~a termination."
                        connection-count)
                  connection-count (canonical-name service))
    (default-service-termination-handler service status))

  (define (spawn-child-service connection server-address client-address)
    (let* ((name    (child-service-name))
           (service (make <service>
                      #:provides (list name)
                      #:requires requirements
                      #:respawn? #f
                      #:transient? #t
                      #:start (make-inetd-forkexec-constructor
                               command connection
                               #:user user
                               #:group group
                               #:supplementary-groups
                               supplementary-groups
                               #:directory directory
                               #:file-creation-mask file-creation-mask
                               #:create-session? create-session?
                               #:environment-variables
                               (append (inetd-variables server-address
                                                        client-address)
                                   environment-variables)
                               #:resource-limits resource-limits)
                      #:handle-termination handle-child-termination
                      #:stop (make-kill-destructor))))
      (register-services service)
      (start service)))

  (define (accept-clients server-address sock)
    ;; Return a thunk that accepts client connections from SOCK.
    (lambda ()
      (let loop ()
        (match (accept sock)
          ((connection . client-address)
           (if (>= connection-count max-connections)
               (begin
                 (local-output
                  (l10n "Maximum number of ~a clients reached; \
rejecting connection from ~:[~a~;~*local process~].")
                  (socket-address->string server-address)
                  (= AF_UNIX (sockaddr:fam client-address))
                  (socket-address->string client-address))
                 (close-port connection))
               (begin
                 (set! connection-count (+ 1 connection-count))
                 (local-output
                  (l10n "Accepted connection on ~a from ~:[~a~;~*local process~].")
                  (socket-address->string server-address)
                  (= AF_UNIX (sockaddr:fam client-address))
                  (socket-address->string client-address))
                 (spawn-child-service connection
                                      server-address client-address)))))
        (loop))))

  (lambda args
    (let* ((endpoints (match endpoints
                        (((? endpoint?) ...) endpoints)
                        (address (list (endpoint address
                                                 #:style socket-style
                                                 #:backlog listen-backlog
                                                 #:socket-owner socket-owner
                                                 #:socket-group socket-group
                                                 #:socket-directory-permissions
                                                 socket-directory-permissions)))))
           (sockets   (open-sockets endpoints)))
      (for-each (lambda (endpoint socket)
                  (spawn-fiber
                   (accept-clients (endpoint-address endpoint)
                                   socket)))
                endpoints sockets)
      sockets)))

(define (make-inetd-destructor)
  "Return a procedure that terminates an inetd service."
  (lambda (sockets)
    (for-each close-port sockets)
    #f))


;;;
;;; systemd-style services.
;;;

(define (wait-for-readable ports)
  "Suspend the current task until one of @var{ports} is available for
reading."
  (suspend-current-task
   (lambda (sched k)
     (for-each (lambda (port)
                 (schedule-task-when-fd-readable sched
                                                 (port-read-wait-fd port)
                                                 k))
               ports))))

(define* (make-systemd-constructor command endpoints
                                   #:key
                                   (user #f)
                                   (group #f)
                                   (supplementary-groups '())
                                   (log-file #f)
                                   (directory (default-service-directory))
                                   (file-creation-mask #f)
                                   (create-session? #t)
                                   (environment-variables
                                    (default-environment-variables))
                                   (resource-limits '()))
  "Return a procedure that starts @var{command}, a program and list of
argument, as a systemd-style service listening on @var{endpoints}, a list of
@code{<endpoint>} objects.

@var{command} is started on demand on the first connection attempt on one of
@var{endpoints}.  It is passed the listening sockets for @var{endpoints} in
file descriptors 3 and above; as such, it is equivalent to an @code{Accept=no}
@uref{https://www.freedesktop.org/software/systemd/man/systemd.socket.html,systemd
socket unit}.  The following environment variables are set in its environment:

@table @env
@item LISTEN_PID
It is set to the PID of the newly spawned process.

@item LISTEN_FDS
It contains the number of sockets available starting from file descriptor
3---i.e., the length of @var{endpoints}.

@item LISTEN_FDNAMES
The colon-separated list of endpoint names.
@end table

This must be paired with @code{make-systemd-destructor}."
  (lambda args
    (let* ((ports     (open-sockets endpoints))
           (sockets   (map (lambda (endpoint socket)
                             (cons (endpoint-name endpoint) socket))
                           endpoints ports))
           (variables (list (string-append "LISTEN_FDS="
                                           (number->string (length sockets)))
                            (string-append "LISTEN_FDNAMES="
                                           (string-join
                                            (map endpoint-name endpoints)
                                            ":"))))
           (running   sockets))
      (spawn-fiber
       (lambda ()
         (wait-for-readable ports)
         (local-output (l10n "Spawning systemd-style service ~a.")
                       (match command
                         ((program . _) program)))
         (let ((pid (fork+exec-command command
                                       #:extra-ports ports
                                       #:user user
                                       #:group group
                                       #:supplementary-groups
                                       supplementary-groups
                                       #:log-file log-file
                                       #:directory directory
                                       #:file-creation-mask file-creation-mask
                                       #:create-session? create-session?
                                       #:environment-variables
                                       (append variables environment-variables)
                                       #:listen-pid-variable? #t
                                       #:resource-limits resource-limits)))
           (set! running pid)
           (for-each close-port ports))))
      (lambda () running))))

(define (make-systemd-destructor)
  "Return a procedure that terminates a systemd-style service as created by
@code{make-systemd-constructor}."
  (let ((destroy (make-kill-destructor)))
    (match-lambda
      ((? integer? pid)
       (destroy pid))
      (((_ . (? port? socks)) ...)
       (for-each close-port socks)))))


;; A group of service-names which can be provided (i.e. services
;; providing them get started) and unprovided (same for stopping)
;; together.  Not comparable with a real runlevel at all, but can be
;; used to emulate a simple kind of runlevel.
(define-syntax-rule (make-service-group NAME (SYM ...) ADDITIONS ...)
  (make <service>
    #:provides '(NAME)
    #:requires '(SYM ...)
    #:stop (lambda (running)
	     (for-each stop '(SYM ...))
	     #f)
    ADDITIONS ...))




;;; Registered services.

;; All registered services.
(define %services (make-hash-table 75))

;;; Perform actions with services:

(define (lookup-canonical-service name services)
  "Return service with canonical NAME from SERVICES list.
Return #f if service is not found."
  (find (lambda (service)
          (eq? name (canonical-name service)))
        services))

(define (fold-services proc init)
  "Apply PROC to the registered services to build a result, and return that
result.  Works in a manner akin to `fold' from SRFI-1."
  (hash-fold (lambda (name services acc)
               (let ((service (lookup-canonical-service name services)))
                 (if service
                     (proc service acc)
                     acc)))
             init %services))

(define (for-each-service proc)
  "Call PROC for each registered service."
  (hash-for-each (lambda (name services)
                   (and=> (lookup-canonical-service name services)
                          proc))
                 %services))

(define (service-list)
  "Return the list of services currently defined.  Note: The order of the list
returned in unspecified."
  (hash-fold (lambda (name services result)
               (let ((service (lookup-canonical-service name services)))
                 (if service
                     (cons service result)
                     result)))
             '()
             %services))

(define (find-service pred)
  "Return the first service that matches PRED, or #f if none was found."
  (call/ec
   (lambda (return)
     (hash-fold (lambda (name services _)
                  (and=> (find pred services)
                         return))
                #f
                %services)
     #f)))

(define (lookup-services name)
  "Return a (possibly empty) list of services that provide NAME."
  (hashq-ref %services name '()))

(define waitpid*
  (lambda (what flags)
    "Like 'waitpid', and return (0 . _) when there's no child left."
    (catch 'system-error
      (lambda ()
        (waitpid what flags))
      (lambda args
        (if (memv (system-error-errno args) (list ECHILD EINTR))
            '(0 . #f)
            (apply throw args))))))

(define* (handle-SIGCHLD #:optional (signum SIGCHLD))
  "Handle SIGCHLD, possibly by respawning the service that just died, or
otherwise by updating its state."
  (let loop ()
    (match (waitpid* WAIT_ANY WNOHANG)
      ((0 . _)
       ;; Nothing left to wait for.
       #t)
      ((pid . status)
       (let ((serv (find-service (lambda (serv)
                                   (and (enabled? serv)
                                        (match (service-running-value serv)
                                          ((? number? pid*)
                                           (= pid pid*))
                                          (_ #f)))))))

         ;; SERV can be #f for instance when this code runs just after a
         ;; service's 'stop' method killed its process and completed.
         (when serv
           (handle-service-termination serv status))

         ;; As noted in libc's manual (info "(libc) Process Completion"),
         ;; loop so we don't miss any terminated child process.
         (loop))))))

(define (handle-service-termination service status)
  "Handle the termination of the process associated with @var{service}, whose
PID is in its @code{running} slot; @var{status} is the process's exit status
as returned by @code{waitpid}.  This procedure is called right after the
process has terminated."
  ((slot-ref service 'handle-termination) service status))

(define (respawn-service serv)
  "Respawn a service that has stopped running unexpectedly. If we have
attempted to respawn the service a number of times already and it keeps dying,
then disable it."
  (slot-set! serv 'running #f)
  (if (and (respawn? serv)
           (not (respawn-limit-hit? (slot-ref serv 'last-respawns)
                                    (car respawn-limit)
                                    (cdr respawn-limit))))
      (if (not (slot-ref serv 'waiting-for-termination?))
          (begin
            ;; Everything is okay, start it.
            (local-output (l10n "Respawning ~a.")
                          (canonical-name serv))
            (slot-set! serv 'last-respawns
                       (cons (current-time)
                             (slot-ref serv 'last-respawns)))
            (start serv))
          ;; We have just been waiting for the
          ;; termination.  The `running' slot has already
          ;; been set to `#f' by `stop'.
          (begin
            (local-output (l10n "Service ~a terminated.")
                          (canonical-name serv))
            (slot-set! serv 'waiting-for-termination? #f)))
      (begin
        (local-output (l10n "Service ~a has been disabled.")
                      (canonical-name serv))
        (when (respawn? serv)
          (local-output (l10n "  (Respawning too fast.)")))
        (slot-set! serv 'enabled? #f)

        (when (transient? serv)
          (hashq-remove! %services (canonical-name serv))
          (local-output (l10n "Transient service ~a terminated, now unregistered.")
                        (canonical-name serv))))))

;; Add NEW-SERVICES to the list of known services.
(define (register-services . new-services)
  "Add NEW-SERVICES to the list of known services.  If a service has already
been registered, arrange to have it replaced when it is next stopped.  If it
is currently stopped, replace it immediately."
  (define (register-single-service new)
    ;; Sanity-checks first.
    (assert (list-of-symbols? (provided-by new)))
    (assert (list-of-symbols? (required-by new)))
    (assert (boolean? (respawn? new)))

    ;; FIXME: Just because we have a unique canonical name now doesn't mean it
    ;; will remain unique as other services are added. Whenever a service is
    ;; added it should check that it's not conflicting with any already
    ;; registered canonical names.
    (match (lookup-services (canonical-name new))
      (() ;; empty, so we can safely add ourselves
       (for-each (lambda (name)
		   (let ((old (lookup-services name)))
		     (hashq-set! %services name (cons new old))))
	         (provided-by new)))
      ((old . rest) ;; one service registered, it may be an old version of us
       (assert (null? rest))
       (assert (eq? (canonical-name new) (canonical-name old)))
       (if (running? old)
           (slot-set! old 'replacement new)
           (replace-service old new)))))

  (for-each register-single-service new-services))

(define (deregister-service service-name)
  "For each string in SERVICE-NAME, stop the associated service if
necessary and remove it from the services table.  If SERVICE-NAME is
the special string 'all', remove all services except of 'root'.

This will remove a service either if it is identified by its canonical
name, or if it is the only service providing the service that is
requested to be removed."
  (define (deregister service)
    (if (running? service)
        (stop service))
    ;; Remove services provided by service from the hash table.
    (for-each
     (lambda (name)
       (let ((old (lookup-services name)))
         (if (= 1 (length old))
             ;; Only service provides this service; remove it.
             (hashq-remove! %services name)
             ;; ELSE: remove service from providing services.
             (hashq-set! %services name
                         (remove
                          (lambda (lk-service)
                            (eq? (canonical-name service)
                                 (canonical-name lk-service)))
                          old)))))
     (provided-by service)))
  (define (service-pairs)
    "Return '(name . service) of all user-registered services."
    (filter identity
            (hash-map->list
             (lambda (key value)
               (match value
                 ((service)     ; only one service associated with KEY
                  (and (eq? key (canonical-name service))
                       (not (memq key '(root shepherd)))
                       (cons key service)))
                 (_ #f)))               ; all other cases: #f.
             %services)))

  (let ((name (string->symbol service-name)))
    (cond ((eq? name 'all)
           ;; Special 'remove all' case.
           (let ((pairs (service-pairs)))
             (local-output (l10n "Unloading all optional services: '~a'...")
                           (map car pairs))
             (for-each deregister (map cdr pairs))
             (local-output (l10n "Done."))))
          (else
           ;; Removing only one service.
           (match (lookup-services name)
             (()                        ; unknown service
              (raise (condition (&missing-service-error (name name)))))
             ((service)             ; only SERVICE provides NAME
              ;; Are we removing a user service…
              (if (eq? (canonical-name service) name)
                  (local-output (l10n "Removing service '~a'...") name)
                  ;; or a virtual service?
                  (local-output
                   "Removing service '~a' providing '~a'..."
                   (canonical-name service) name))
              (deregister service)
              (local-output (l10n "Done.")))
             ((services ...)            ; ambiguous NAME
              (local-output
               "Not unloading: '~a' names several services: '~a'."
               name (map canonical-name services))))))))

(define (load-config file-name)
  (local-output (l10n "Loading ~a.") file-name)
  ;; Every action is protected anyway, so no need for a `catch'
  ;; here.  FIXME: What about `quit'?
  (load-in-user-module file-name))

;;; Tests for validity of the slots of <service> objects.

;; Test if OBJ is a list that only contains symbols.
(define (list-of-symbols? obj)
  (cond ((null? obj) #t)
	((and (pair? obj)
	      (symbol? (car obj)))
	 (list-of-symbols? (cdr obj)))
	(else #f)))



;; The 'root' service.

(define (shutdown-services)
  "Shut down all the currently running services; update the persistent state
file when persistence is enabled."
  (let ((running-services '()))
    (for-each-service
     (lambda (service)
       (when (running? service)
         (stop service)
         (when persistency
           (set! running-services
                 (cons (canonical-name service)
                       running-services))))))

    (when persistency
      (call-with-output-file persistency-state-file
        (lambda (p)
          (format p "~{~a ~}~%" running-services))))))

(define (check-for-dead-services)
  "Poll each process that we expect to be running, and respawn any which have
unexpectedly stopped running. This procedure is used as a fallback on systems
where prctl/PR_SET_CHILD_SUBREAPER is unsupported."
  (define (process-exists? pid)
    (catch-system-error (kill pid 0) #t))

  (for-each-service (lambda (service)
                      (let ((running (service-running-value service)))
                        (when (and (integer? running)
                                   (not (process-exists? running)))
                          (local-output (l10n "PID ~a (~a) is dead!")
                                        running (canonical-name service))
                            (respawn-service service))))))

(define root-service
  (make <service>
    #:docstring "The root service is used to operate on shepherd itself."
    #:provides '(root shepherd)
    #:requires '()
    #:respawn? #f
    #:start (lambda args
	      (when (isatty? (current-output-port))
                (display-version))
	      #t)
    #:stop (lambda (unused . args)
	     (local-output (l10n "Exiting shepherd..."))
	     ;; Prevent that we try to stop ourself again.
	     (slot-set! root-service 'running #f)
             (shutdown-services)
	     (quit))
    ;; All actions here need to take care that they do not invoke any
    ;; user-defined code without catching `quit', since they are
    ;; allowed to quit, while user-supplied code shouldn't be.
    #:actions
    (make-actions
     (help
      "Show the help message for the 'root' service."
      (lambda _
        ;; A rudimentary attempt to have 'herd help' return something
        ;; sensible.
        "\
This is the help message for the 'root' service of the Shepherd.  The 'root'
service is used to control the Shepherd itself and it supports several
actions.  For instance, running 'herd status root' or simply 'herd status'
returns a summary of each service.

Try 'herd doc root list-actions' to see the list of available actions.
Run 'info shepherd' to access the user manual."))

     (status
      "Return an s-expression showing information about all the services.
Clients such as 'herd' can read it and format it in a human-readable way."
      (lambda (running)
        ;; Return the list of services.
        (service-list)))

     ;; Halt.
     (halt
      "Halt the system."
      (lambda (running)
        (catch 'quit
          (cut stop root-service)
          (lambda (key)
            (local-output (l10n "Halting..."))
            (halt)))))
     ;; Power off.
     (power-off
      "Halt the system and turn it off."
      (lambda (running)
        (catch 'quit
          (cut stop root-service)
          (lambda (key)
            (local-output (l10n "Shutting down..."))
            (power-off)))))
     ;; Evaluate arbitrary code.
     (load
      "Load the Scheme code from FILE into shepherd.  This is potentially
dangerous.  You have been warned."
      (lambda (running file-name)
        (load-config file-name)))
     (eval
      "Evaluate the given Scheme expression into the shepherd.  This is
potentially dangerous, be careful."
      (lambda (running str)
        (let ((exp (call-with-input-string str read)))
          (local-output (l10n "Evaluating user expression ~a.")
                        (call-with-output-string
                          (lambda (port)
                            (truncated-print exp port #:width 50))))
          (eval-in-user-module exp))))

     ;; Unload a service
     (unload
      "Unload the service identified by SERVICE-NAME or all services
except for 'root' if SERVICE-NAME is 'all'.  Stop services before
removing them if needed."
      (lambda (running service-name)
        (deregister-service service-name)))
     (reload
      "Unload all services, then load from FILE-NAME into shepherd.  This
is potentially dangerous.  You have been warned."
      (lambda (running file-name)
        (and (deregister-service "all") ; unload all services
             (load-config file-name)))) ; reload from FILE-NAME
     ;; Go into the background.
     (daemonize
      "Go into the background.  Be careful, this means that a new
process will be created, so shepherd will not get SIGCHLD signals anymore
if previously spawned children terminate.  Therefore, this action should
usually only be used (if at all) *before* children get spawned for which
we want to receive these signals."
      (lambda (running)
        (cond ((= 1 (getpid))
               (local-output (l10n "Running as PID 1, so not daemonizing.")))
              ((fold-services (lambda (service found?)
                                (or found?
                                    (and (running? service)
                                         (not (eq? service root-service)))))
                              #f)
               (local-output
                (l10n "Services already running, so not daemonizing."))
               #f)
              (else
               (if (zero? (primitive-fork))
                   (begin
                     (catch-system-error (prctl PR_SET_CHILD_SUBREAPER 1))
                     #t)
                   (primitive-exit 0))))))
     (persistency
      "Save the current state of running and non-running services.
This status gets written into a file on termination, so that we can
restore the status on next startup.  Optionally, you can pass a file
name as argument that will be used to store the status."
      (lambda* (running #:optional (file #f))
               (set! persistency #t)
               (when file
                 (set! persistency-state-file file))))
     (no-persistency
      "Don't save state in a file on exit."
      (lambda (running)
	(set! persistency #f)))
     (cd
      "Change the working directory of shepherd.  This only makes sense
when in interactive mode, i.e. with `--socket=none'."
      (lambda (running dir)
	(chdir dir)))
     ;; Restart it - that does not make sense, but
     ;; we're better off by implementing it due to the
     ;; default action.
     (restart
      "This does not work for the 'root' service."
      (lambda (running)
	(local-output (l10n "You must be kidding.")))))))

(register-services root-service)
