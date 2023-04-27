;; service.scm -- Representation of services.
;; Copyright (C) 2013-2023 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Järling <wolfgang@pro-linux.de>
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Copyright (C) 2016 Alex Kost <alezost@gmail.com>
;; Copyright (C) 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
;; Copyright (C) 2019 Ricardo Wurmus <rekado@elephly.net>
;; Copyright (C) 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;; Copyright (C) 2020 Oleg Pykhalov <go.wigust@gmail.com>
;; Copyright (C) 2023 Ulf Herrman <striness@tilde.club>
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
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers conditions)
  #:use-module (fibers scheduler)
  #:use-module (fibers timers)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module ((srfi srfi-35) #:hide (make-condition))
  #:use-module (rnrs io ports)
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:autoload   (ice-9 ports internal) (port-read-wait-fd)
  #:autoload   (ice-9 pretty-print) (truncated-print)
  #:use-module (shepherd support)
  #:use-module (shepherd comm)
  #:use-module (shepherd config)
  #:use-module (shepherd system)
  #:export (<service>
            service
            service?
            service-provision
            service-requirement
            one-shot-service?
            transient-service?
            respawn-service?
            service-documentation

            service-canonical-name
            service-running-value
            service-status
            service-running?
            service-stopped?
            service-enabled?
            service-respawn-times
            service-startup-failures
            service-status-changes
            service-replacement
            service-action-list
            lookup-service-action
            service-defines-action?
            with-service-registry
            lookup-service
            service-name-count

            action?

            enable-service
            disable-service
            start-service
            start-in-the-background
            stop-service
            perform-service-action

            lookup-running
            for-each-service
            respawn-service
            handle-SIGCHLD
            with-process-monitor
            spawn-command
            spawn-shell-command
            %precious-signals
            register-services

            default-service-termination-handler
            default-environment-variables
            make-forkexec-constructor
            make-kill-destructor
            default-process-termination-grace-period
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

            &service-error
            service-error?
            &missing-service-error
            missing-service-error?
            missing-service-name

            actions

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

            condition->sexp

            get-message*                      ;XXX: for lack of a better place

            ;; Deprecated bindings.
            provided-by
            required-by
            one-shot?
            transient?
            respawn?
            canonical-name
            running?
            stopped?
            enabled?
            enable
            disable
            start
            stop
            action
            action-list
            make-actions
            lookup-action
            defines-action?
            lookup-services))


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
(define-syntax actions
  (syntax-rules ()
    ((_ (name docstring proc) rest ...)
     (cons (make-action 'name proc docstring)
           (actions rest ...)))
    ((_ (name proc) rest ...)
     (cons (make-action 'name proc "[No documentation.]")
           (actions rest ...)))
    ((_)
     '())))

(define-syntax-rule (make-actions rest ...)
  "Deprecated alias for @code{actions}."
  (begin
    (issue-deprecation-warning "The 'make-actions' macro is deprecated; \
use 'actions' instead.")
    (actions rest ...)))

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

(define (default-service-termination-handler service pid status)
  "Handle the termination of @var{service} by respawning it if applicable.
Log abnormal termination reported by @var{status}."
  (unless (zero? status)
    ;; Most likely something went wrong; log it.
    (cond ((status:exit-val status)
           =>
           (lambda (code)
             (local-output (l10n "Service ~a (PID ~a) exited with ~a.")
                           (service-canonical-name service) pid code)))
          ((status:term-sig status)
           =>
           (lambda (signal)
             (local-output (l10n "Service ~a (PID ~a) terminated with signal ~a.")
                           (service-canonical-name service) pid signal)))
          ((status:stop-sig status)
           =>
           (lambda (signal)
             (local-output (l10n "Service ~a (PID ~a) stopped with signal ~a.")
                           (service-canonical-name service)
                           pid signal)))))

  (respawn-service service))

(define-class <service> ()
  ;; List of provided service-symbols.  The first one is also called
  ;; the `canonical name' and must be unique to this service.
  (provides #:init-keyword #:provides
	    #:getter service-provision)
  ;; List of required service-symbols.
  (requires #:init-keyword #:requires
	    #:init-value '()
	    #:getter service-requirement)
  ;; If true, the service is a "one-shot" service: it becomes marked as
  ;; stopped as soon as its 'start' method as completed, but services that
  ;; depend on it may be started.
  (one-shot? #:init-keyword #:one-shot?
             #:init-value #f
             #:getter one-shot-service?)
  ;; If true, the service is "transient": it is unregistered as soon as it
  ;; terminates, unless it is respawned.
  (transient? #:init-keyword #:transient?
              #:init-value #f
              #:getter transient-service?)
  ;; If `#t', then assume the `running' slot specifies a PID and
  ;; respawn it if that process terminates.  Otherwise `#f'.
  (respawn? #:init-keyword #:respawn?
	    #:init-value #f
	    #:getter respawn-service?)
  ;; The action to perform to start the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but it
  ;; must be possible to call it without any argument.  If the
  ;; starting attempt failed, it must return `#f'.  The return value
  ;; will be stored in the `running' slot.
  (start #:init-keyword #:start
         #:getter service-start
	 #:init-value (lambda () #t))
  ;; The action to perform to stop the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but must
  ;; be callable with exactly one argument, which will be the value of
  ;; the `running' slot.  Whatever the procedure returns will be
  ;; ignored.
  (stop #:init-keyword #:stop
        #:getter service-stop
	#:init-value (lambda (running) #f))
  ;; Additional actions that can be performed with the service.  This
  ;; currently is a list with each element (and thus each action)
  ;; being ``(name . (proc . docstring))'', but users should not rely
  ;; on this.
  (actions #:init-keyword #:actions
           #:getter service-actions
	   #:init-form (actions))
  ;; Procedure called to notify that the process associated with this service
  ;; has terminated.
  (handle-termination #:init-keyword #:handle-termination
                      #:getter service-termination-handler
                      #:init-value default-service-termination-handler)
  ;; A description of the service.
  (docstring #:init-keyword #:docstring
             #:getter service-documentation
	     #:init-value "[No description].")

  ;; Control channel that encapsulates the current state of the service; send
  ;; requests such as 'start' and 'stop' on this channels.
  (control #:init-value #f))

;; The procedure below supersedes (make <service> ...).
(define* (service provision
                  #:key
                  (requirement '())
                  (one-shot? #f)
                  (transient? #f)
                  (respawn? #f)
                  (start (lambda () #t))
                  (stop (lambda (running) #f))
                  (actions (actions))
                  (termination-handler default-service-termination-handler)
                  (documentation (l10n "[No description].")))
  "Return a new service with the given @var{provision}, a list of symbols
denoting what the service provides."
  (make <service>
    #:provides provision
    #:requires requirement
    #:one-shot? one-shot?
    #:transient? transient?
    #:respawn? respawn?
    #:start start
    #:stop stop
    #:actions actions
    #:handle-termination termination-handler
    #:docstring documentation))

(define (service-control service)
  "Return the controlling channel of @var{service}."
  ;; Spawn the controlling fiber lazily, hopefully once Fibers has actually
  ;; been initialized.
  (or (slot-ref service 'control)
      (begin
        (slot-set! service 'control
                   (spawn-service-controller service))
        (slot-ref service 'control))))

(define (spawn-service-controller service)
  "Return a channel over which @var{service} may be controlled."
  (let ((channel (make-channel)))
    (spawn-fiber
     (lambda ()
       ;; The controller writes to its current output port via 'local-output'.
       ;; Make sure that goes to the right port.  If the controller got a
       ;; wrong output port, it could crash and stop responding just because a
       ;; 'local-output' call raised an exception.
       (parameterize ((current-output-port (%current-service-output-port))
                      (current-error-port (%current-service-output-port)))
         (service-controller service channel))))
    channel))

(define %max-recorded-status-changes
  ;; Maximum number of service status changes that are recorded.
  30)

(define %max-recorded-startup-failures
  ;; Maximum number of service startup failures that are recorded.
  10)

(define (service-controller service channel)
  "Encapsulate @var{service} state and serve requests arriving on
@var{channel}."
  (define *service-started* (list 'service 'started!))
  (define (started-message? obj) (eq? *service-started* obj))
  (define *service-stopped* (list 'service 'stopped!))
  (define (stopped-message? obj) (eq? *service-stopped* obj))
  (define *change-value* (list 'change 'value!))
  (define (change-value-message? obj) (eq? *change-value* obj))

  (define (pid? obj)
    ;; Return true if OBJ looks like a PID.
    (and (integer? obj) (exact? obj) (> obj 1)))

  (let-loop loop ((status 'stopped)
                  (value #f)
                  (condition #f)
                  (enabled? #t)
                  (changes                     ;list of status/timestamp pairs
                   (ring-buffer %max-recorded-status-changes))
                  (failures                    ;list of timestamps
                   (ring-buffer %max-recorded-startup-failures))
                  (respawns '())               ;list of timestamps
                  (replacement #f))
    (define (update-status-changes status)
      ;; Add STATUS to CHANGES, the ring buffer of status changes.
      (ring-buffer-insert (cons status (current-time)) changes))

    (match (get-message channel)
      (('running reply)
       (put-message reply value)
       (loop))
      (('status reply)
       (put-message reply status)
       (loop))
      (('enabled? reply)
       (put-message reply enabled?)
       (loop))
      (('respawn-times reply)
       (put-message reply respawns)
       (loop))
      (('startup-failures reply)
       (put-message reply failures)
       (loop))
      (('status-changes reply)
       (put-message reply changes)
       (loop))

      ('enable                                    ;no reply
       (loop (enabled? #t)))
      ('disable                                   ;no reply
       (loop (enabled? #f)))

      (('start reply)
       ;; Attempt to start SERVICE, blocking if it is already being started.
       ;; Send #f on REPLY if SERVICE was already running or being started;
       ;; otherwise send a channel on which to send SERVICE's value one it
       ;; has been started.
       (cond ((eq? 'running status)
              ;; SERVICE is already running: send #f on REPLY.
              (put-message reply #f)
              (loop))
             ((eq? 'starting status)
              ;; SERVICE is being started: wait until it has started and
              ;; then send #f on REPLY.
              (spawn-fiber
               (lambda ()
                 (wait condition)
                 (put-message reply #f)))
              (loop))
             (else
              ;; Become the one that starts SERVICE.
              (let ((notification (make-channel)))
                (spawn-fiber
                 (lambda ()
                   (let ((running (get-message notification)))
                     (if running
                         (local-output (l10n "Service ~a started.")
                                       (service-canonical-name service))
                         (local-output (l10n "Service ~a failed to start.")
                                       (service-canonical-name service)))
                     (put-message channel
                                  (list *service-started* running)))))
                (local-output (l10n "Starting service ~a...")
                              (service-canonical-name service))
                (put-message reply notification)
                (loop (status 'starting)
                      (changes (update-status-changes 'starting))
                      (condition (make-condition)))))))
      (((? started-message?) new-value)           ;no reply
       ;; When NEW-VALUE is a procedure, call it to get the actual value and
       ;; pass it a call back so it can eventually change it.
       (let ((new-value (if (procedure? new-value)
                            (new-value
                             (lambda (value)
                               (put-message channel
                                            (list *change-value* value))))
                            new-value)))
        (when new-value
          (local-output (l10n "Service ~a running with value ~s.")
                        (service-canonical-name service) new-value))
        (when (pid? new-value)
          (monitor-service-process service new-value))

        (signal-condition! condition)
        (let ((new-status (if (and new-value (not (one-shot-service? service)))
                              'running
                              'stopped)))
          (loop (status new-status)
                (value (and (not (one-shot-service? service)) new-value))
                (changes (update-status-changes new-status))
                (condition #f)
                (failures (if new-value
                              failures
                              (ring-buffer-insert (current-time)
                                                  failures)))))))

      (((? change-value-message?) new-value)
       (local-output (l10n "Running value of service ~a changed to ~s.")
                     (service-canonical-name service) new-value)
       (when (pid? new-value)
         (monitor-service-process service new-value))
       (loop (value new-value)))

      (('stop reply)
       ;; Attempt to stop SERVICE, blocking if it is already being stopped.
       ;; Send #f on REPLY if SERVICE was already running or being stopped;
       ;; otherwise send a channel on which to send a notification once it
       ;; has been stopped.
       (cond ((eq? status 'stopping)
              ;; SERVICE is being stopped: wait until it is stopped and
              ;; then send #f on REPLY.
              (spawn-fiber
               (lambda ()
                 (wait condition)
                 (put-message reply #f)))
              (loop))
             ((not (eq? status 'running))
              ;; SERVICE is not running: send #f on REPLY.
              (put-message reply #f)
              (loop))
             (else
              ;; Become the one that stops SERVICE.
              (let ((notification (make-channel)))
                (spawn-fiber
                 (lambda ()
                   (let ((stopped? (get-message notification)))
                     ;; The STOPPED? boolean is supposed to indicate success
                     ;; or failure, but sometimes 'stop' method might return a
                     ;; truth value even though the service was successfully
                     ;; stopped, hence "might have failed" below.
                     (if stopped?
                         (local-output (l10n "Service ~a stopped.")
                                       (service-canonical-name service))
                         (local-output
                          (l10n "Service ~a might have failed to stop.")
                          (service-canonical-name service)))
                     (put-message channel *service-stopped*))))
                (local-output (l10n "Stopping service ~a...")
                              (service-canonical-name service))
                (put-message reply notification)
                (loop (status 'stopping)
                      (changes (update-status-changes 'stopping))
                      (condition (make-condition)))))))
      ((? stopped-message?)                       ;no reply
       (local-output (l10n "Service ~a is now stopped.")
                     (service-canonical-name service))
       (signal-condition! condition)
       (loop (status 'stopped)
             (changes (update-status-changes 'stopped))
             (value #f) (condition #f)
             (respawns '())
             (failures (ring-buffer %max-recorded-startup-failures))))

      ('notify-termination                        ;no reply
       (loop (status 'stopped)
             (changes (update-status-changes 'stopped))
             (value #f)))

      (('handle-termination pid exit-status)      ;no reply
       ;; Handle premature termination of this service's process, possibly by
       ;; respawning it, unless STATUS is 'stopping' or 'stopped' or PID
       ;; doesn't match VALUE (which happens with notifications of processes
       ;; terminated while stopping the service or shortly after).
       (if (or (memq status '(stopping stopped))
               (not (eqv? value pid)))
           (loop)
           (begin
             (spawn-fiber
              (lambda ()
                (false-if-exception
                 ((service-termination-handler service)
                  service value exit-status))))
             (loop (status 'stopped)
                   (changes (update-status-changes 'stopped))
                   (value #f) (condition #f)))))

      ('record-respawn-time                       ;no reply
       (loop (respawns (cons (current-time) respawns))))

      (('replace-if-running new-service reply)
       (if (eq? status 'running)
           (begin
             (local-output (l10n "Recording replacement for ~a.")
                           (service-canonical-name service))
             (put-message reply #t)
             (loop (replacement new-service)))
           (begin
             (put-message reply #f)
             (loop (replacement #f)))))
      (('replacement reply)
       (put-message reply replacement)
       (loop))

      ('terminate                                 ;no reply
       (if (eq? status 'stopped)
           (begin
             ;; Exit the loop, terminating this fiber.
             (slot-set! service 'control #f)
             #t)
           (begin
             ;; Oops, that shouldn't happen!
             (local-output
              (l10n "Attempt to terminate controller of ~a in ~a state!")
              (service-canonical-name service) status)
             (loop)))))))

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
             ,(service-canonical-name (unknown-action-service condition))))
    ((? action-runtime-error?)
     `(error (version 0) action-exception
             ,(action-runtime-error-action condition)
             ,(service-canonical-name (action-runtime-error-service condition))
             ,(action-runtime-error-key condition)
             ,(map result->sexp (action-runtime-error-arguments condition))))
    ((? service-error?)
     `(error (version 0) service-error))))

(define (service-canonical-name service)
  "Return the \"canonical\" name of @var{service}."
  (car (service-provision service)))

(define (service-control-message message)
  "Return a procedure to send @var{message} to the given service's control
channel and wait for its reply."
  (lambda (service)
    (let ((reply (make-channel)))
      (put-message (service-control service) (list message reply))
      (get-message reply))))

(define service-running-value
  ;; Return the "running value" of @var{service}.
  (service-control-message 'running))

(define service-status
  ;; Return the status of @var{service}, one of @code{stopped},
  ;; @code{starting}, @code{running}, or @code{stopping}.
  (service-control-message 'status))

(define service-respawn-times
  ;; Return the list of respawn times of @var{service}.
  (service-control-message 'respawn-times))

(define service-startup-failures
  ;; Return the list of recent startup failure times for @var{service}.
  (compose ring-buffer->list
           (service-control-message 'startup-failures)))

(define service-status-changes
  ;; Return the list of symbol/timestamp pairs representing recent state
  ;; changes for @var{service}.
  (compose ring-buffer->list
           (service-control-message 'status-changes)))

(define service-enabled?
  ;; Return true if @var{service} is enabled, false otherwise.
  (service-control-message 'enabled?))

(define service-replacement
  ;; Return the replacement of @var{service}, #f if there is none.
  (service-control-message 'replacement))

(define (enable-service service)
  "Enable @var{service}."
  (put-message (service-control service) 'enable))

(define (disable-service service)
  "Disable @var{service}."
  (put-message (service-control service) 'disable))

(define (record-service-respawn-time service)
  "Record the current time as the last respawn time for @var{service}."
  (put-message (service-control service) 'record-respawn-time))

(define (service-running? service)
  "Return true if @var{service} is not stopped."
  (not (service-stopped? service)))

(define (service-stopped? service)
  "Return true if @var{service} is stopped."
  (eq? 'stopped (service-status service)))

(define (service-action-list service)
  "Return the list of actions implemented by @var{service} (a list of
symbols)."
  (map action-name (service-actions service)))

(define (lookup-service-action service action)
  "Return the action @var{action} of @var{service} or #f if none was found."
  (find (match-lambda
          (($ <action> name)
           (eq? name action)))
        (service-actions service)))

(define (service-defines-action? service action)
  "Return whether @var{service} implements the action @var{action}."
  (and (lookup-service-action service action) #t))

(define %one-shot-services-started
  ;; Bookkeeping of one-shot services already started.
  (make-parameter #f))                            ;#f | hash table

(define (start-in-parallel services)
  "Start @var{services} in parallel--i.e., without waiting for each one to be
started before starting the next one.  Return the subset of @var{services}
that could not be started."
  ;; Use the hash table in %ONE-SHOT-SERVICES-STARTED to keep track of
  ;; one-shot services that have been started directly or indirectly by this
  ;; call.  That way, if several services depend on the same one-shot service,
  ;; its 'start' method is invoked only once.
  (parameterize ((%one-shot-services-started
                  (or (%one-shot-services-started)
                      (make-hash-table))))
    (let ((services (map (lambda (service)
                           (if (symbol? service)
                               (lookup-service service)
                               service))
                         services))
          (channel  (make-channel)))
      (for-each (lambda (service)
                  (spawn-fiber
                   (lambda ()
                     (let ((value
                            (guard (c ((action-runtime-error? c)
                                       (local-output
                                        (l10n "Exception caught \
while starting ~a: ~s")
                                        service
                                        (cons (action-runtime-error-key c)
                                              (action-runtime-error-arguments c)))
                                       #f))
                              (or (and (one-shot-service? service)
                                       (hashq-ref (%one-shot-services-started)
                                                  service))
                                  (begin
                                    (when (one-shot-service? service)
                                      (hashq-set! (%one-shot-services-started)
                                                  service #t))
                                    (start-service service))))))
                       (put-message channel (cons service value))))))
                services)
      (let loop ((i (length services))
                 (failures '()))
        (if (> i 0)
            (match (get-message channel)
              ((service . #f)
               (loop (- i 1) (cons service failures)))
              ((_ . _)
               (loop (- i 1) failures)))
            failures)))))

(define (start-service service . args)
  "Start @var{service} and its dependencies, passing @var{args} to its
@code{start} method."
  (if (service-enabled? service)
      ;; It is not running; go ahead and launch it.
      (let ((problems
	     ;; Resolve all dependencies.
	     (start-in-parallel (service-requirement service))))
        (define running
	  (if (pair? problems)
              (for-each (lambda (problem)
	                  (local-output (l10n "Service ~a depends on ~a.")
			                (service-canonical-name service)
			                problem))
                        problems)
              ;; Start the service itself.
              (let ((reply (make-channel)))
                (put-message (service-control service) `(start ,reply))
                (match (get-message reply)
                  (#f
                   ;; We lost the race: SERVICE is already running.
                   (service-running-value service))
                  ((? channel? notification)
                   ;; We won the race: we're responsible for starting SERVICE
                   ;; and sending its running value on NOTIFICATION.
                   (let ((running
                          (catch #t
                            (lambda ()
                              ;; Make sure the 'start' method writes
                              ;; messages to the right port.
                              (parameterize ((current-output-port
                                              (%current-service-output-port))
                                             (current-error-port
                                              (%current-service-output-port)))
                                (apply (service-start service) args)))
                            (lambda (key . args)
                              (put-message notification #f)
                              (report-exception 'start service key args)))))
                     (put-message notification running)
                     (local-output (if running
			               (l10n "Service ~a has been started.")
                                       (l10n "Service ~a could not be started."))
			           (service-canonical-name service))
                     running))))))

        running)
      (begin
        (local-output (l10n "Service ~a is currently disabled.")
		      (service-canonical-name service))
        (service-running-value service))))

(define (replace-service old-service new-service)
  "Replace OLD-SERVICE with NEW-SERVICE in the services registry.  This
completely removes all references to OLD-SERVICE before registering
NEW-SERVICE."
  (when new-service
    (put-message (current-registry-channel)
                 `(unregister ,(list old-service)))
    (register-services (list new-service))))

(define (required-by? service dependent)
  "Returns #t if DEPENDENT directly requires SERVICE in order to run.  Returns
#f otherwise."
  (and (find (lambda (dependency)
               (memq dependency (service-provision service)))
             (service-requirement dependent))
       #t))

;; Stop the service, including services that depend on it.  If the
;; latter fails, continue anyway.  Return `#f' if it could be stopped.
(define (stop-service service . args)
  "Stop @var{service} and any service that depends on it.  Return the list of
services that have been stopped (including transitive dependent services).

If @var{service} is not running, print a warning and return its canonical name
in a list."
  (if (service-stopped? service)
      (begin
        (local-output (l10n "Service ~a is not running.")
                      (service-canonical-name service))
        (list service))
      (let ((stopped-dependents
             (fold-services (lambda (other acc)
                              (if (and (service-running? other)
                                       (required-by? service other))
                                  (append (stop-service other) acc)
                                  acc))
                            '())))
        ;; Stop the service itself.
        (let ((reply (make-channel)))
          (put-message (service-control service) `(stop ,reply))
          (match (get-message reply)
            (#f
             #f)
            ((? channel? notification)
             (catch #t
               (lambda ()
                 (define stopped?
                   (not (apply (service-stop service)
                               (service-running-value service)
                               args)))
                 (put-message notification stopped?))
               (lambda (key . args)
                 ;; Special case: 'root' may quit.
                 (and (eq? root-service service)
                      (eq? key 'quit)
                      (apply quit args))
                 (put-message notification #f)
                 (caught-error key args))))))

        (when (transient-service? service)
          (put-message (current-registry-channel)
                       `(unregister ,(list service)))
          (local-output (l10n "Transient service ~a unregistered.")
                        (service-canonical-name service)))

        ;; Replace the service with its replacement, if it has one.
        (let ((replacement (service-replacement service)))
          (when replacement
            (replace-service service replacement)))

        (cons service stopped-dependents))))

(define (perform-service-action service the-action . args)
  "Perform @var{the-action} (a symbol such as @code{'restart} or @code{'status})
on @var{service}, passing it @var{args}.  The meaning of @var{args} depends on
the action."
  (define default-action
    ;; All actions which are handled here might be called even if the
    ;; service is not running, so they have to take this into account.
    (case the-action
      ;; Restarting is done in the obvious way.
      ((restart)
       (lambda (running . args)
         (let ((stopped-services (stop-service service)))
           (for-each start-service stopped-services)
           #t)))
      ((status)
       ;; Return the service itself.  It is automatically converted to an sexp
       ;; via 'result->sexp' and sent to the client.
       (lambda (_) service))
      ((enable)
       (lambda (_)
         (enable-service service)
         (local-output (l10n "Enabled service ~a.")
                       (service-canonical-name service))))
      ((disable)
       (lambda (_)
         (disable-service service)
         (local-output (l10n "Disabled service ~a.")
                       (service-canonical-name service))))
      ((doc)
       (lambda (_ . args)
         (apply display-service-documentation service args)))
      (else
       (lambda _
         ;; FIXME: Unknown service.
         (raise (condition (&unknown-action-error
                            (service service)
                            (action the-action))))))))

  (let ((proc (or (and=> (lookup-service-action service the-action)
                         action-procedure)
		  default-action)))
    ;; Invoking THE-ACTION is allowed even when the service is not running, as
    ;; it provides generally useful functionality and information.
    (catch #t
      (lambda ()
        ;; PROC may return any number of values (e.g., if PROC is
        ;; 'eval-in-user-module'), including zero values, but callers expect a
        ;; single value.  Deal with it gracefully.
        (call-with-values
            (lambda ()
              (apply proc (service-running-value service) args))
          (case-lambda
            (() *unspecified*)
            ((first . rest) first))))
      (lambda (key . args)
        ;; Special case: 'root' may quit.
        (and (eq? root-service service)
             (eq? key 'quit)
             (apply quit args))

        ;; Re-throw SRFI-34 exceptions that the caller will handle.
        (cond ((eq? key 'srfi-34)                 ;Guile 2.x
               (apply throw key args))
              ((eq? key '%exception)              ;Guile 3.x
               (raise-exception (car args)))
              (else
               (report-exception the-action service key args)))))))

;; Display documentation about the service.
(define (display-service-documentation service . args)
  (if (null? args)
      ;; No further argument given -> Normal level of detail.
      (local-output (service-documentation service))
    (case (string->symbol (car args)) ;; Does not work with strings.
      ((full)
       ;; FIXME
       (local-output (service-documentation service)))
      ((short)
       ;; FIXME
       (local-output (service-documentation service)))
      ((action)
       ;; Display documentation of given actions.
       (for-each
	(lambda (the-action)
          (let ((action-object
                 (lookup-service-action service (string->symbol the-action))))
            (unless action-object
              (raise (condition (&unknown-action-error
                                 (action the-action)
                                 (service service)))))
            (local-output "~a: ~a" the-action
                          (action-documentation action-object))))
        (cdr args)))
      ((list-actions)
       (local-output "~a ~a"
		     (service-canonical-name service)
		     (action-list service)))
      (else
       ;; FIXME: Implement doc-help.
       (local-output (l10n "Unknown keyword.  Try 'doc root help'."))))))

(define-record-type-serializer (service->sexp (service <service>))
  "Return a representation of SERVICE as an sexp meant to be consumed by
clients."
  `(service (version 0)                           ;protocol version
            (provides ,(service-provision service))
            (requires ,(service-requirement service))
            (respawn? ,(respawn-service? service))
            (docstring ,(service-documentation service))

            ;; Status.  Use 'result->sexp' for the running value to make sure
            ;; that whole thing is valid read syntax; we do not want things
            ;; like #<undefined> to be sent to the client.
            (enabled? ,(service-enabled? service))
            (running ,(result->sexp (service-running-value service)))
            (conflicts ())                        ;deprecated
            (last-respawns ,(service-respawn-times service))
            (status-changes ,(service-status-changes service))
            (startup-failures ,(service-startup-failures service))
            (status ,(service-status service))
            ,@(if (one-shot-service? service)
                  '((one-shot? #t))
                  '())
            ,@(if (transient-service? service)
                  '((transient? #t))
                  '())))


;;;
;;; Service registry.
;;;

(define (service-registry channel)
  "Encapsulate shepherd state (registered and running services) and serve
requests arriving on @var{channel}."
  (let loop ((registered vlist-null))
    (define (unregister services)
      ;; Terminate the controller of each of SERVICES and return REGISTERED
      ;; minus SERVICES.
      (for-each (lambda (service)
                  (put-message (service-control service) 'terminate))
                services)
      (vhash-fold (lambda (name service result)
                    (if (memq service services)
                        result
                        (vhash-consq name service result)))
                  vlist-null
                  registered))

    (define* (register service #:optional (registered registered))
      ;; Add SERVICE to REGISTER and return it.
      (fold (cut vhash-consq <> service <>)
            registered
            (service-provision service)))

    (match (get-message channel)
      (('register service)                        ;no reply
       ;; Register SERVICE or, if its name is provided by an
       ;; already-registered service, make it a replacement for that service.
       ;; There cannot be two services providing the same name.
       (match (any (lambda (name)
                     (vhash-assq name registered))
                   (service-provision service))
         (#f
          (loop (register service)))
         ((_ . old)
          (let ((reply (make-channel)))
            (put-message (service-control old)
                         `(replace-if-running ,service ,reply))
            (match (get-message reply)
              (#t (loop registered))
              (#f (loop (register service
                                  (unregister (list old))))))))))
      (('unregister services)                     ;no reply
       (match (remove service-stopped? services)
         (()
          (loop (unregister services)))
         (lst                                     ;
          (local-output
           (l10n "Cannot unregister service ~a, which is still running"
                 "Cannot unregister services~{ ~a,~} which are still running"
                 (length lst))
           (map service-canonical-name lst))
          (loop registered))))
      (('unregister-all)                          ;no reply
       (let ((root (cdr (vhash-assq 'root registered))))
         (loop (fold (cut vhash-consq <> root <>)
                     vlist-null
                     (service-provision root)))))
      (('lookup name reply)
       ;; Look up NAME and return it, or #f, to REPLY.
       (put-message reply
                    (match (vhash-assq name registered)
                      (#f #f)
                      ((_ . service) service)))
       (loop registered))
      (('service-list reply)
       (put-message reply (vlist->list registered))
       (loop registered))
      (('service-name-count reply)
       (put-message reply (vlist-length registered))
       (loop registered)))))

(define (essential-task-launcher name proc)
  "Return a thunk that runs @var{proc} in a fiber, endlessly (an essential
task is one that should never fail)."
  (lambda ()
    (define channel
      (make-channel))

    (spawn-fiber
     (lambda ()
       ;; PROC should never return.  If it does, log the problem and
       ;; desperately attempt to restart it.
       (let loop ()
         (catch #t
           (lambda ()
             (proc channel)
             (local-output (l10n "Essential task ~a exited unexpectedly.")
                           name))
           (lambda args
             (local-output
              (l10n "Uncaught exception in essential task ~a: ~s")
              name args)))

         ;; Restarting is not enough to recover because all state has been
         ;; lost, but it might be enough to halt the system.
         (loop))))

    channel))

(define spawn-service-registry
  (essential-task-launcher 'service-registry service-registry))

(define current-registry-channel
  ;; The channel to communicate with the current service monitor.
  (make-parameter #f))

(define (call-with-service-registry thunk)
  (parameterize ((current-registry-channel (spawn-service-registry)))
    (thunk)))

(define-syntax-rule (with-service-registry exp ...)
  "Spawn a new service monitor and evaluate @var{exp}... within that dynamic extent.
This allows @var{exp}... and their callees to send requests to delegate
service state and to send requests to the service monitor."
  (call-with-service-registry (lambda () exp ...)))



(define (remove pred lst)
  ;; In Guile <= 3.0.9, 'remove' is written in C and thus introduced a
  ;; continuation barrier.  Provide a Scheme implementation to address that.
  (let loop ((lst lst)
             (result '()))
    (match lst
      (()
       (reverse result))
      ((head . tail)
       (loop tail (if (pred head) result (cons head result)))))))

(cond-expand
 (guile-3.0 #t)
 (else
  ;; In Guile 2.2, SRFI-1 'find' is in C and thus introduces a continuation
  ;; barrier, which is a problem for 'launch-service'.  Provide a Scheme
  ;; implementation to address that.
  (define (find pred lst)
    (let loop ((lst lst))
      (and (not (null? lst))
           (let ((head (car lst)))
             (if (pred head)
                 head
                 (loop (cdr lst)))))))))

(define (start-in-the-background services)
  "Start the services named by @var{services}, a list of symbols, in the
background.  In other words, this procedure returns immediately without
waiting until all of @var{services} have been started.

This procedure can be useful in a configuration file because it lets you
interact right away with shepherd using the @command{herd} command."
  (spawn-fiber
   (lambda ()
     (match (start-in-parallel services)
       (()
        (local-output
         (l10n "Successfully started ~a service in the background."
               "Successfully started ~a services in the background."
               (length services))
         (length services)))
       (failures
        (local-output
         (l10n "The following service could not be started in the \
background:~{ ~a~}."
               "The following services could not be started in the \
background:~{ ~a~}."
               (length failures))
         (map service-canonical-name failures))))))

  ;; 'spawn-fiber' returns zero values, which can confuse callees; return one.
  *unspecified*)

(define (lookup-running name)
  "Return the running service that provides @var{name}, or false if none."
  (match (lookup-service name)
    (#f #f)
    (service
     (and (eq? 'running (service-status service))
          service))))


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

(define %logging-buffer-size
  ;; Size of the buffer for each line read by logging fibers.
  512)

(define (read-line! str port)
  "This is an interruptible version of the 'read-line!' procedure from (ice-9
rdelim)."
  ;; As of Guile 3.0.8, (@ (ice-9 rdelim) read-line!) calls
  ;; '%read-delimited!', which is in C and thus non-interruptible.
  (define len
    (string-length str))

  (let loop ((i 0))
    (and (< i len)
         (match (read-char port)
           ((? eof-object? eof)
            eof)
           ((or #\newline #\return)
            i)
           (chr
            (string-set! str i chr)
            (loop (+ i 1)))))))

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
          (define line
            (make-string %logging-buffer-size))

          (let loop ()
            (match (read-line! line input)
              ((? eof-object?)
               (close-port input)
               (close-port output))
              (count
               (let ((prefix (strftime default-logfile-date-format
                                       (localtime (current-time))))
                     (count  (or count (string-length line))))
                 ;; Avoid (ice-9 format) to reduce heap allocations.
                 (put-string output prefix)
                 (put-string output line 0 count)
                 (newline output)
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
    (define line
      (make-string %logging-buffer-size))

    (let loop ()
      (match (read-line! line input)
        ((? eof-object?)
         (close-port input))
        (count
         (let ((prefix (strftime (%current-logfile-date-format)
                                 (localtime (current-time))))
               (count  (or count (string-length line))))
           ;; TODO: Print the PID of COMMAND.  The actual PID is potentially
           ;; not known until after 'read-pid-file' has completed, so it would
           ;; need to be communicated.
           (simple-format (log-output-port) "~a[~a] "
                          prefix command)
           (put-string (log-output-port) line 0 count)
           (newline (log-output-port)))
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

     (catch 'system-error
       (lambda ()
         ;; File descriptors used internally are all marked as close-on-exec,
         ;; so we can fearlessly go ahead.
         (apply execlp program program args))
       (lambda args
         (format (current-error-port)
                 "exec of ~s failed: ~a~%"
                 program (strerror (system-error-errno args)))
         (primitive-exit 1)))))))

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

(define* (make-kill-destructor #:optional (signal SIGTERM)
                               #:key (grace-period
                                      (default-process-termination-grace-period)))
  "Return a procedure that sends @var{signal} to the process group of the PID
given as argument, where @var{signal} defaults to @code{SIGTERM}.  If the
process is still running after @var{grace-period} seconds, send it
@code{SIGKILL}.  The procedure returns once the process has terminated."
  (lambda (pid . args)
    ;; Kill the whole process group PID belongs to.  Don't assume that PID is
    ;; a process group ID: that's not the case when using #:pid-file, where
    ;; the process group ID is the PID of the process that "daemonized".  If
    ;; this procedure is called, between the process fork and exec, the PGID
    ;; will still be zero (the Shepherd PGID). In that case, use the PID.
    (let ((pgid (getpgid pid)))
      (if (= (getpgid 0) pgid)
          (terminate-process pid signal           ;don't kill ourself
                             #:grace-period grace-period)
          (terminate-process (- pgid) signal
                             #:grace-period grace-period)))
    #f))

(define (spawn-shell-command command)
  "Spawn @var{command} (a string) using the shell.

This is similar to Guile's @code{system} procedure but does not block while
waiting for the shell to terminate."
  (spawn-command (list (or (getenv "SHELL") "/bin/sh")
                       "-c" command)))

;; Produce a constructor that executes a command.
(define (make-system-constructor . command)
  (lambda args
    (zero? (status:exit-val
            (spawn-shell-command (string-concatenate command))))))

;; Produce a destructor that executes a command.
(define (make-system-destructor . command)
  (lambda (ignored . args)
    (not (zero? (status:exit-val
                 (spawn-shell-command (string-concatenate command)))))))


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

  (define (handle-child-termination service pid status)
    (set! connection-count (- connection-count 1))
    (local-output (l10n "~a connection still in use after ~a termination."
                        "~a connections still in use after ~a termination."
                        connection-count)
                  connection-count (service-canonical-name service))
    (default-service-termination-handler service pid status))

  (define (spawn-child-service connection server-address client-address)
    (let* ((name    (child-service-name))
           (service (service
                      (list name)
                      #:requirement requirements
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
                      #:termination-handler handle-child-termination
                      #:stop (make-kill-destructor))))
      (register-services (list service))
      (start-service service)))

  (define (accept-clients server-address sock)
    ;; Return a thunk that accepts client connections from SOCK.
    (lambda ()
      (let loop ()
        (match (accept sock SOCK_CLOEXEC)
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
                                   (lazy-start? #t)
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
@var{endpoints} when @var{lazy-start?} is true; otherwise it is started as
soon as possible.  It is passed the listening sockets for @var{endpoints} in
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
                                            ":")))))
      (lambda (change-service-value)
        ;; Return SOCKETS now as the first running value of the service, and
        ;; spawn a fiber to eventually change the value to the PID of the
        ;; process, once started.
        (spawn-fiber
         (lambda ()
           (if lazy-start?
               (wait-for-readable ports)

               ;; Hand the child process blocking ports: it may not be ready
               ;; to handle EAGAIN & co.
               (for-each blocking-port ports))
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
             (change-service-value pid)
             (for-each close-port ports))))

        sockets))))

(define (make-systemd-destructor)
  "Return a procedure that terminates a systemd-style service as created by
@code{make-systemd-constructor}."
  (let ((destroy (make-kill-destructor)))
    (match-lambda
      ((? integer? pid)
       (destroy pid))
      (((_ . (? port? socks)) ...)
       (for-each close-port socks)
       #f))))




;;; Registered services.

;;; Perform actions with services:

(define fold-services
  (let ((reply (make-channel)))
    (lambda (proc init)
      "Apply PROC to the registered services to build a result, and return that
result.  Works in a manner akin to `fold' from SRFI-1."
      (put-message (current-registry-channel)
                   `(service-list ,reply))
      (fold (match-lambda*
              (((name . service) result)
               (if (eq? name (service-canonical-name service))
                   (proc service result)
                   result)))
            init
            (get-message reply)))))

(define (for-each-service proc)
  "Call PROC for each registered service."
  (fold-services (lambda (service _)
                   (proc service)
                   *unspecified*)
                 *unspecified*))

(define (service-list)
  "Return the list of services currently defined.  Note: The order of the list
returned in unspecified."
  (fold-services cons '()))

(define (service-name-count)
  "Return the number of currently-registered service names."
  (let ((reply (make-channel)))
    (put-message (current-registry-channel)
                 `(service-name-count ,reply))
    (get-message* reply 5 'no-reply)))

(define find-service
  (let ((reply (make-channel)))
    (lambda (pred)
      "Return the first service that matches PRED, or #f if none was found."
      (call/ec
       (lambda (return)
         (fold-services (lambda (service _)
                          (and (pred service)
                               (return service)))
                        #f))))))

(define lookup-service
  (let ((reply (make-channel)))
    (lambda (name)
      "Return the service that provides @var{name}, @code{#f} if there is none."
      (put-message (current-registry-channel) `(lookup ,name ,reply))
      (get-message reply))))

(define (lookup-services name)
  "Deprecated.  Use @code{lookup-service} instead."
  (issue-deprecation-warning "The 'lookup-services' procedure is deprecated; \
use 'lookup-service' instead.")
  (match (lookup-service name)
    (#f '())
    (service (list service))))

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
       ;; Let the process monitor handle it.
       (put-message (current-process-monitor)
                    `(handle-process-termination ,pid ,status))

       ;; As noted in libc's manual (info "(libc) Process Completion"),
       ;; loop so we don't miss any terminated child process.
       (loop)))))

(define-syntax-rule (boxed-errors exps ...)
  (catch #t
    (lambda ()
      (call-with-values
          (lambda ()
            exps ...)
        (lambda results
          (list 'success results))))
    (lambda args
      (list 'exception args))))

(define unboxed-errors
  (match-lambda
    (('success vals)
     (apply values vals))
    (('exception args)
     (apply throw args))))

(define (process-monitor channel)
  "Run a process monitor that handles requests received over @var{channel}."
  (let loop ((waiters vlist-null))
    (match (get-message channel)
      (('handle-process-termination pid status)
       ;; Notify any waiters.
       (vhash-foldv* (lambda (waiter _)
                       (put-message waiter status)
                       #t)
                     #t pid waiters)

       ;; XXX: The call below is linear in the size of WAITERS, but WAITERS is
       ;; usually empty or small.
       (loop (vhash-fold (lambda (key value result)
                           (if (= key pid)
                               result
                               (vhash-consv key value result)))
                         vlist-null
                         waiters)))

      (('spawn arguments reply)
       ;; Spawn the command as specified by ARGUMENTS; send the spawn result
       ;; (PID or exception) to REPLY; send its exit status to REPLY when it
       ;; terminates.  This operation is atomic: the WAITERS table is updated
       ;; before termination of PID can possibly be handled.
       (let ((result (boxed-errors
                      (apply fork+exec-command arguments))))
         (put-message reply result)
         (match result
           (('exception . _)
            (loop waiters))
           (('success (pid))
            (loop (vhash-consv pid reply waiters))))))

      (('await pid reply)
       ;; Await the termination of PID and send its status on REPLY.
       (if (catch-system-error (kill pid 0))
           (loop (vhash-consv pid reply waiters))
           (begin                                 ;PID is gone
             (put-message reply 0)
             (loop waiters)))))))

(define spawn-process-monitor
  (essential-task-launcher 'process-monitor process-monitor))

(define current-process-monitor
  ;; Channel to communicate with the process monitoring fiber.
  (make-parameter #f))

(define (call-with-process-monitor thunk)
  (parameterize ((current-process-monitor (spawn-process-monitor)))
    (thunk)))

(define-syntax-rule (with-process-monitor exp ...)
  "Spawn a process monitoring fiber and evaluate @var{exp}... within that
context.  The process monitoring fiber is responsible for handling
@code{SIGCHLD} and generally dealing with process creation and termination."
  (call-with-process-monitor (lambda () exp ...)))

(define (spawn-via-monitor arguments)
  (let ((reply (make-channel)))
    (put-message (current-process-monitor)
                 `(spawn ,arguments ,reply))
    (unboxed-errors (get-message reply))
    (get-message reply)))

(define spawn-command
  (let ((warn-deprecated-form
         ;; In 0.9.3, this procedure took a rest list.
         (lambda ()
           (issue-deprecation-warning
            "This 'spawn-command' form is deprecated; use\
 (spawn-command '(\"PROGRAM\" \"ARGS\"...))."))))
    (case-lambda*
     ((command #:key
               (user #f)
               (group #f)
               (environment-variables (default-environment-variables))
               (directory (default-service-directory))
               (resource-limits '()))
      "Like @code{system*}, spawn @var{command} (a list of strings) but do not block
while waiting for @var{program} to terminate."
      (let ((command (if (string? command)
                         (begin
                           (warn-deprecated-form)
                           (list command))
                         command)))
        (if (current-process-monitor)
            (spawn-via-monitor
             (list command
                   #:user user #:group group
                   #:environment-variables environment-variables
                   #:directory directory
                   #:resource-limits resource-limits))
            (let ((pid (fork+exec-command
                        command
                        #:user user #:group group
                        #:environment-variables environment-variables
                        #:directory directory
                        #:resource-limits resource-limits)))
              (match (waitpid pid)
                ((_ . status) status))))))
     ((program . arguments)
      ;; The old form, which appeared in 0.9.3.
      (spawn-command (cons program arguments))))))

(define (monitor-service-process service pid)
  "Monitor process @var{pid} and notify @var{service} when it terminates."
  (let ((reply (make-channel)))
    (put-message (current-process-monitor)
                 `(await ,pid ,reply))
    (spawn-fiber
     (lambda ()
       (let ((status (get-message reply)))
         (handle-service-termination service pid status))))))

(define default-process-termination-grace-period
  ;; Default process termination "grace period" before we send SIGKILL.
  (make-parameter 5))

(define* (get-message* channel timeout #:optional default)
  "Receive a message from @var{channel} and return it, or, if the message hasn't
arrived before @var{timeout} seconds, return @var{default}."
  (call-with-values
      (lambda ()
        (perform-operation
         (choice-operation (get-operation channel)
                           (sleep-operation timeout))))
    (match-lambda*
      (()                               ;'sleep' operation returns zero values
       default)
      ((message)                            ;'get' operation returns one value
       message))))

(define* (terminate-process pid signal
                            #:key (grace-period
                                   (default-process-termination-grace-period)))
  "Send @var{signal} to @var{pid}, which can be negative to denote a process
group; wait for @var{pid} to terminate and return its exit status.  If
@var{pid} is still running @var{grace-period} seconds after @var{signal} has
been sent, send it @code{SIGKILL}."
  (let ((reply (make-channel)))
    (put-message (current-process-monitor) `(await ,(abs pid) ,reply))
    (kill pid signal)

    (match (get-message* reply grace-period #f)
      (#f
       (local-output
        (l10n "Grace period of ~a seconds is over; sending ~a SIGKILL.")
        grace-period pid)
       (catch-system-error (kill pid SIGKILL))
       (get-message reply))
      (status
       status))))

(define (handle-service-termination service pid status)
  "Handle the termination of the process @var{pid} associated with
@var{service}; @var{status} is the process's exit status as returned by
@code{waitpid}.  This procedure is called right after the process has
terminated."
  (put-message (service-control service)
               `(handle-termination ,pid ,status)))

(define (respawn-service serv)
  "Respawn a service that has stopped running unexpectedly. If we have
attempted to respawn the service a number of times already and it keeps dying,
then disable it."
  (if (and (respawn-service? serv)
           (not (respawn-limit-hit? (service-respawn-times serv)
                                    (car respawn-limit)
                                    (cdr respawn-limit))))
      (begin
        ;; Everything is okay, start it.
        (local-output (l10n "Respawning ~a.")
                      (service-canonical-name serv))
        (record-service-respawn-time serv)
        (start-service serv))
      (begin
        (local-output (l10n "Service ~a has been disabled.")
                      (service-canonical-name serv))
        (when (respawn-service? serv)
          (local-output (l10n "  (Respawning too fast.)")))
        (disable-service serv)

        (when (transient-service? serv)
          (put-message (current-registry-channel) `(unregister (,serv)))
          (local-output (l10n "Transient service ~a terminated, now unregistered.")
                        (service-canonical-name serv))))))

;; Add NEW-SERVICES to the list of known services.
(define register-services
  (let ((warn-deprecated-form
         ;; Up to 0.9.x, this procedure took a rest list.
         (lambda ()
           (issue-deprecation-warning
            "Passing 'register-services' services a rest list is \
now deprecated."))))
   (case-lambda
     ((services)
      "Register @var{services} so that they can be looked up by name, for instance
when resolving dependencies.

Each name uniquely identifies one service.  If a service with a given name has
already been registered, arrange to have it replaced when it is next stopped.
If it is currently stopped, replace it immediately."
      (define (register-single-service new)
        ;; Sanity-checks first.
        (assert (list-of-symbols? (service-provision new)))
        (assert (list-of-symbols? (service-requirement new)))
        (assert (boolean? (respawn-service? new)))

        (put-message (current-registry-channel) `(register ,new)))

      (let ((services (if (service? services)
                          (begin
                            (warn-deprecated-form)
                            (list services))
                          services)))
        (for-each register-single-service services)))
     (services
      (warn-deprecated-form)
      (register-services services)))))

(define (deregister-service service-name)
  "For each string in SERVICE-NAME, stop the associated service if
necessary and remove it from the services table.  If SERVICE-NAME is
the special string 'all', remove all services except of 'root'.

This will remove a service either if it is identified by its canonical
name, or if it is the only service providing the service that is
requested to be removed."
  (define (deregister service)
    (when (service-running? service)
      (stop-service service))
    ;; Remove services provided by service from the hash table.
    (put-message (current-registry-channel)
                 `(unregister ,(list service))))

  (let ((name (string->symbol service-name)))
    (cond ((eq? name 'all)
           ;; Special 'remove all' case.
           (put-message (current-registry-channel) `(unregister-all))
           #t)
          (else
           ;; Removing only one service.
           (match (lookup-service name)
             (#f
              (raise (condition (&missing-service-error (name name)))))
             (service
              ;; Are we removing a user service…
              (if (eq? (service-canonical-name service) name)
                  (local-output (l10n "Removing service '~a'...") name)
                  ;; or a virtual service?
                  (local-output
                   "Removing service '~a' providing '~a'..."
                   (service-canonical-name service) name))
              (deregister service)
              (local-output (l10n "Done."))))))))

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


;;;
;;; Deprecated aliases.
;;;

(define (issue-method-deprecation-warning name alias)
  (issue-deprecation-warning
   (format #f "GOOPS method '~a' is \
deprecated in favor of procedure '~a'"
           name alias)))

(define-syntax-rule (define-deprecated-method (name (service class) formals ...) alias)
  (define-method (name (service class) formals ...)
    (issue-method-deprecation-warning 'name 'alias)
    (alias service formals ...)))

(define-syntax-rule (define-deprecated-method/rest (name (service class)) alias)
  (define-method (name (service class) . rest)
    (issue-method-deprecation-warning 'name 'alias)
    (apply alias service rest)))

(define-syntax-rule (define-deprecated-service-getter name alias)
  (define-deprecated-method (name (service <service>)) alias))

(define-deprecated-service-getter provided-by service-provision)
(define-deprecated-service-getter required-by service-requirement)
(define-deprecated-service-getter one-shot? one-shot-service?)
(define-deprecated-service-getter transient? transient-service?)
(define-deprecated-service-getter respawn? respawn-service?)

(define-deprecated-service-getter canonical-name service-canonical-name)
(define-deprecated-service-getter action-list service-action-list)

(define-deprecated-service-getter running? service-running?)
(define-deprecated-service-getter stopped? service-stopped?)
(define-deprecated-service-getter enabled? service-enabled?)

(define-deprecated-service-getter enable enable-service)
(define-deprecated-service-getter disable disable-service)

(define-deprecated-method (lookup-action (service <service>) action)
  lookup-service-action)
(define-deprecated-method (defines-action? (service <service>) action)
  service-defines-action?)

(define-deprecated-method/rest (action (service <service>))
  perform-service-action)
(define-method (action (name <symbol>) the-action . args)
  "Perform THE-ACTION on all the services named OBJ.  Return the list of
results."
  (match (lookup-service name)
    (#f
     (raise (condition (&missing-service-error (name name)))))
    (service
     (list (apply action service the-action args)))))
(define-deprecated-method/rest (start (service <service>))
  start-service)
(define-method (start (name <symbol>) . args)
  "Try to start (with PROC) a service providing NAME; return #f on failure.
Used by `start'."
  (match (lookup-service name)
    (#f
     (raise (condition (&missing-service-error (name name)))))
    (service
     (if (eq? 'running (service-status service))
         service
         (apply start service args)))))
(define-deprecated-method/rest (stop (service <service>))
  stop-service)
(define-method (stop (name <symbol>) . args)
  (match (lookup-service name)
    (#f
     (raise (condition (&missing-service-error (name name)))))
    (service
     (if (service-stopped? service)
         '()
         (map service-canonical-name (apply stop service args))))))




;; The 'root' service.

(define (shutdown-services)
  "Shut down all the currently running services."
  ;; Note: Do not use 'for-each-service' since it introduces a continuation
  ;; barrier via 'hash-fold', thereby preventing the 'stop' method from
  ;; suspending via (@ (fibers) sleep), 'spawn-command', or similar.
  (for-each
   (lambda (service)
     (when (service-running? service)
       (stop-service service)))
   (service-list)))

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
                                        running (service-canonical-name service))
                            (respawn-service service))))))

(define root-service
  (service
   '(root shepherd)
    #:documentation
    (l10n "The root service is used to operate on shepherd itself.")
    #:requirement '()
    #:respawn? #f
    #:start (lambda args
	      (when (isatty? (current-output-port))
                (display-version))
	      #t)
    #:stop (lambda (unused . args)
	     (local-output (l10n "Exiting shepherd..."))

	     ;; Prevent that we try to stop ourself again.
	     (put-message (service-control root-service)
                          'notify-termination)

             (shutdown-services)
	     (quit))
    ;; All actions here need to take care that they do not invoke any
    ;; user-defined code without catching `quit', since they are
    ;; allowed to quit, while user-supplied code shouldn't be.
    #:actions
    (actions
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
                                    (and (service-running? service)
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
     ;; Restart it - that does not make sense, but
     ;; we're better off by implementing it due to the
     ;; default action.
     (restart
      "This does not work for the 'root' service."
      (lambda (running)
	(local-output (l10n "You must be kidding.")))))))

