;; shepherd.scm -- The daemon shepherd.
;; Copyright (C) 2013-2014, 2016, 2018-2020, 2022-2023 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;; Copyright (C) 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
;; Copyright (C) 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (shepherd)
  #:use-module ((fibers)
                #:hide (sleep))                   ;avoid Guile warning
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)   ;; Line-based I/O.
  #:use-module ((ice-9 threads) #:select (all-threads))
  #:use-module (oop goops)      ;; Defining classes and methods.
  #:use-module (srfi srfi-1)    ;; List library.
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (shepherd config)
  #:use-module (shepherd support)
  #:use-module (shepherd service)
  #:use-module (shepherd system)
  #:use-module (shepherd args)
  #:use-module (shepherd comm)
  #:export (main))


(define sleep (@ (fibers) sleep))

(define-syntax-rule (unwind-protect body ... conclude)
  "Evaluate BODY... and return its result(s), but always evaluate CONCLUDE
before leaving, even if an exception is raised.

This is *not* implemented with 'dynamic-wind' in order to play well with
delimited continuations and fibers."
  (let ((conclusion (lambda () conclude)))
    (catch #t
      (lambda ()
        (call-with-values
            (lambda ()
              body ...)
          (lambda results
            (conclusion)
            (apply values results))))
      (lambda args
        (conclusion)
        (apply throw args)))))

(define (call-with-server-socket file-name proc)
  "Call PROC, passing it a listening socket at FILE-NAME and deleting the
socket file at FILE-NAME upon exit of PROC.  Return the values of PROC."
  (let ((sock (catch 'system-error
                (lambda ()
                  (open-server-socket file-name))
                (lambda args
                  (match args
                    ((key proc . _)
                     (report-error (l10n "while opening socket '~a': ~a: ~a~%")
                                   file-name proc
                                   (strerror (system-error-errno args)))
                     ;; Stop services that were started from the config file
                     ;; and quit.
                     (stop-service root-service)))))))
    (unwind-protect (proc sock)
                    (begin
                      (close sock)
                      (catch-system-error (delete-file file-name))))))

(define (maybe-signal-port signals)
  "Return a signal port for SIGNALS, using 'signalfd' on GNU/Linux, or #f if
that is not supported."
  (catch 'system-error
    (lambda ()
      (let ((port (non-blocking-port (signalfd -1 signals))))
        ;; As per the signalfd(2) man page, block SIGNALS.  The tricky bit is
        ;; that SIGNALS must be blocked for all the threads; new threads will
        ;; inherit the signal mask, but we must ensure that neither Guile's
        ;; signal delivery thread nor its finalization thread are already
        ;; running, because if they do, they are not blocking SIGNALS.  The
        ;; signal delivery thread is started on the first call to 'sigaction'
        ;; so we arrange to not call 'sigaction' beforehand; as for the
        ;; finalization thread, use 'without-automatic-finalization' to
        ;; temporarily stop it.
        (without-automatic-finalization
         (let ((count (length (all-threads))))
           (if (= 1 count)
               (begin
                 (block-signals signals)
                 port)
               (begin
                 (local-output (l10n "warning: \
already ~a threads running, disabling 'signalfd' support")
                               count)
                 (close-port port)
                 #f))))))
    (lambda args
      (if (= ENOSYS (system-error-errno args))
          #f
          (apply throw args)))))

(define (handle-SIGINT)
  "Handle SIGINT by stopping the Shepherd, which means rebooting if we're PID 1."
  ;; Since 'stop' is synchronous and may block until SIGCHLD has been received
  ;; for the process it's waiting for, call it in a separate fiber so that
  ;; signals are still being processed in the meantime.
  (spawn-fiber
   (lambda ()
     (catch 'quit
       (lambda ()
         (stop-service root-service))
       quit-exception-handler))))

(define (signal-handler signal)
  "Return the signal handler for SIGNAL."
  (cond ((= signal SIGCHLD)
         (lambda _ (handle-SIGCHLD)))
        ((= signal SIGINT)
         (lambda _ (handle-SIGINT)))
        ((memv signal (list SIGTERM SIGHUP))
         (lambda _ (handle-SIGINT)))
        (else
         (const #f))))

(define (handle-signal-port port)
  "Read from PORT, a signalfd port, and handle the signal accordingly."
  (let ((signal (consume-signalfd-siginfo port)))
    ((signal-handler signal))))


(define (mark-as-close-on-exec)
  "Mark all the open file descriptors as close-on-exec."
  (define max-fd
    (max-file-descriptors))

  (let loop ((fd 3))
    (when (< fd max-fd)
      (catch-system-error
       (let ((flags (fcntl fd F_GETFD)))
         (when (zero? (logand flags FD_CLOEXEC))
           (fcntl fd F_SETFD (logior FD_CLOEXEC flags)))))
      (loop (+ fd 1)))))

(define* (run-daemon #:key (config-file (default-config-file))
                     socket-file pid-file signal-port poll-services?)
  (define signal-handler
    ;; Thunk that waits for signals (particularly SIGCHLD) and handles them.
    (if signal-port
        (lambda ()
          (let loop ()
            (handle-signal-port signal-port)
            (loop)))
        (lambda ()
          ;; When not using signalfd(2), there's always a time window before
          ;; 'select' during which a handler async can be queued but not
          ;; executed.  Work around it by exiting 'select' every few seconds.
          (let loop ()
            (sleep (if poll-services? 0.5 30))
            (when poll-services?
              (check-for-dead-services))
            (loop)))))

  ;; We might have file descriptors inherited from our parent, as well as file
  ;; descriptors wrongfully opened by Guile or Fibers (see
  ;; <https://bugs.gnu.org/57567> and
  ;; <https://github.com/wingo/fibers/commit/1f834cb81126dea2fd47d3d7ebb2d21f798a3c8b>);
  ;; mark them all as FD_CLOEXEC so child processes do not inherit them.
  (mark-as-close-on-exec)

  ;; This _must_ succeed.  (We could also put the `catch' around
  ;; `main', but it is often useful to get the backtrace, and
  ;; `caught-error' does not do this yet.)
  (catch #t
    (lambda ()
      (load-in-user-module (or config-file (default-config-file))))
    (lambda (key . args)
      (caught-error key args)
      (quit 1)))

  ;; Ignore SIGPIPE so that we don't die if a client closes the connection
  ;; prematurely.
  (sigaction SIGPIPE SIG_IGN)

  (if (not socket-file)
      ;; Get commands from the standard input port.
      (process-textual-commands (current-input-port))
      ;; Process the data arriving at a socket.
      (call-with-server-socket
       socket-file
       (lambda (sock)

         ;; Possibly write out our PID, which means we're ready to accept
         ;; connections.  XXX: What if we daemonized already?
         (match pid-file
           ((? string? file)
            (with-atomic-file-output pid-file
              (cute display (getpid) <>)))
           (#t (display (getpid)))
           (_  #t))

         ;; Spawn a signal handling fiber.
         (spawn-fiber
          (essential-task-thunk 'signal-handler signal-handler))

         ;; Enter some sort of a REPL for commands.
         (let next-command ()
           (match (accept sock (logior SOCK_NONBLOCK SOCK_CLOEXEC))
             ((command-source . client-address)
              (setvbuf command-source 'block 1024)
              (spawn-fiber
               (lambda ()
                 (process-connection command-source))))
             (_ #f))

           (next-command))))))

(define-syntax replace-core-bindings!
  (syntax-rules (<>)
    "Replace the given core bindings in the current process, restoring them upon
fork in the child process."
    ((_ () <> ((binding value) ...))
     (let ((real-primitive-fork primitive-fork))
       (set! primitive-fork
             (lambda ()
               (let ((result (real-primitive-fork)))
                 (when (zero? result)
                   (set! binding value)
                   ...
                   (set! primitive-fork real-primitive-fork))
                 result)))))
    ((_ ((binding value) rest ...) <> (saved-bindings ...))
     (let ((real binding))
       (set! binding value)
       (replace-core-bindings! (rest ...) <>
                               ((binding real) saved-bindings ...))))
    ((_ (binding value) ...)
     (replace-core-bindings! ((binding value) ...) <> ()))))


;; Main program.
(define (main . args)
  (define poll-services?
    ;; Do we need polling to find out whether services died?
    (and (not (= 1 (getpid)))                     ;if we're pid 1, we don't
         (catch 'system-error
           (lambda ()
             ;; Register for orphaned processes to be reparented onto us when
             ;; their original parent dies. This lets us handle SIGCHLD from
             ;; daemon processes that would otherwise have been reparented
             ;; under pid 1. Obviously this is unnecessary when we are pid 1.
             (prctl PR_SET_CHILD_SUBREAPER 1)
             #f)                                  ;don't poll
           (lambda args
             ;; We fall back to polling for services on systems that don't
             ;; support prctl/PR_SET_CHILD_SUBREAPER.
             (let ((errno (system-error-errno args)))
               (or (= ENOSYS errno)        ;prctl unavailable
                   (= EINVAL errno)        ;PR_SET_CHILD_SUBREAPER unavailable
                   (apply throw args)))))))

  (define signal-port
    ;; Attempt to create a "signal port" via 'signalfd'.  This must be called
    ;; before the 'sigaction' procedure is called, because 'sigaction' spawns
    ;; the signal thread.
    (maybe-signal-port %precious-signals))

  (define log-flags
    ;; Flags for 'open' when opening the log file.
    (logior O_CREAT O_APPEND O_WRONLY O_CLOEXEC))

  (initialize-cli)

  (let ((config-file #f)
	(socket-file default-socket-file)
        (pid-file    #f)
        (secure      #t)
        (logfile     #f))
    ;; Process command line arguments.
    (process-args (program-name) args
		  ""
		  (l10n "This is a service manager for Unix and GNU.")
		  not ;; Fail on unknown args.
		  (option
		    #:long-name "quiet"
		    #:takes-argument? #f
		    #:description (l10n "synonym for --silent")
		    #:action (lambda ()
                               ;; XXX: Currently has no effect.
                               #t))
		  (option
		    #:long-name "silent" #:short-name #\S
		    #:takes-argument? #f
		    #:description (l10n "don't do output to stdout")
		    #:action (lambda ()
                               ;; XXX: Currently has no effect.
                               #t))
		  (option
		    ;; It might actually be desirable to have an
		    ;; ``insecure'' setup in some circumstances, thus
		    ;; we provide it as an option.
		    #:long-name "insecure" #:short-name #\I
		    #:takes-argument? #f
		    #:description (l10n "don't ensure that the setup is secure")
		    #:action (lambda ()
                               (set! secure #f)))
		  (option
		    #:long-name "logfile" #:short-name #\l
		    #:takes-argument? #t #:argument-is-optional? #f
                    #:argument-name (l10n "FILE")
		    #:description (l10n  "log actions in FILE")
		    #:action (lambda (file)
			       (set! logfile file)))
		  (option
		    #:long-name "pid"
		    #:takes-argument? #t #:argument-is-optional? #t
                    #:argument-name (l10n "FILE")
		    #:description (l10n "when ready, write PID to FILE or stdout")
		    #:action (lambda (file)
			       (set! pid-file (or file #t))))
		  (option
		    #:long-name "config" #:short-name #\c
		    #:takes-argument? #t #:argument-is-optional? #f
                    #:argument-name (l10n "FILE")
		    #:description (l10n "read configuration from FILE")
		    #:action (lambda (file)
			       (set! config-file file)))
		  (option
		    #:long-name "socket" #:short-name #\s
		    #:takes-argument? #t #:argument-is-optional? #f
                    #:argument-name (l10n "FILE")
		    #:description
		    (l10n "get commands from socket FILE or from stdin (-)")
		    #:action (lambda (file)
			       (set! socket-file
				     (and (not (string=? file "-"))
					  file)))))
    ;; We do this early so that we can abort early if necessary.
    (and socket-file
         (verify-dir (dirname socket-file) #:secure? secure))

    ;; Enable logging as first action.
    (parameterize ((log-output-port
                    (cond (logfile
                           (buffering (open logfile log-flags)
                                      'line))
                          ((zero? (getuid))
                           (syslog-output-port))
                          (else
                           (buffering (open (user-default-log-file) log-flags)
                                      'line))))
                   (%current-logfile-date-format
                    (if (and (not logfile) (zero? (getuid)))
                        (format #f "shepherd[~d]: " (getpid))
                        default-logfile-date-format))
                   (%current-service-output-port
                    ;; Send output to log and clients.
                    (make-shepherd-output-port
                     (if (and (zero? (getuid)) (not logfile))
                         ;; By default we'd write both to /dev/kmsg and to
                         ;; stdout.  Redirect stdout to the bitbucket so we
                         ;; don't log twice.
                         (%make-void-port "w")
                         (current-output-port)))))

      (parameterize ((current-output-port (%current-service-output-port)))
        (set-port-encoding! (log-output-port) "UTF-8")

        (when (= 1 (getpid))
          ;; When running as PID 1, disable hard reboots upon ctrl-alt-del.
          ;; Instead, the kernel will send us SIGINT so that we can gracefully
          ;; shut down.  See ctrlaltdel(8) and kernel/reboot.c.
          (catch 'system-error
            (lambda ()
              (disable-reboot-on-ctrl-alt-del))
            (lambda args
              (let ((err (system-error-errno args)))
                ;; When in a separate PID namespace, we get EINVAL (see
                ;; 'reboot_pid_ns' in kernel/pid_namespace.c.)  We get EPERM in
                ;; a user namespace that lacks CAP_SYS_BOOT.
                (unless (member err (list EINVAL EPERM))
                  (apply throw args)))))

          ;; Load the SIGSEGV/SIGABRT handler.  This is what allows PID 1 to
          ;; dump core on "/", should something go wrong.
          (false-if-exception
           (dynamic-link (string-append %pkglibdir "/crash-handler"))))

        ;; Install signal handlers for everything but SIGCHLD, which is taken
        ;; care of in (shepherd services).
        (for-each (lambda (signal)
                    (sigaction signal (signal-handler signal)))
                  (delete SIGCHLD %precious-signals))

        ;; Run Fibers in such a way that it does not create any POSIX thread,
        ;; because POSIX threads and 'fork' cannot be used together.
        (run-fibers
         (lambda ()
           (with-service-registry

             ;; Register and start the 'root' service.
             (register-services (list root-service))
             (start-service root-service)

             (catch 'quit
               (lambda ()
                 (with-process-monitor
                   ;; Replace the default 'system*' binding with one that
                   ;; cooperates instead of blocking on 'waitpid'.  Replace
                   ;; 'primitive-load' (in C as of 3.0.9) with one that does
                   ;; not introduce a continuation barrier.
                   (replace-core-bindings!
                    (system* (lambda command
                               (spawn-command command)))
                    (system spawn-shell-command)
                    (primitive-load primitive-load*))

                   (run-daemon #:socket-file socket-file
                               #:config-file config-file
                               #:pid-file pid-file
                               #:signal-port signal-port
                               #:poll-services? poll-services?)))
               (case-lambda
                 ((key value . _)
                  (primitive-exit value))
                 ((key)
                  (primitive-exit 0))))))
         #:parallelism 1                          ;don't create POSIX threads
         #:hz 0)))))       ;disable preemption, which would require POSIX threads

(define (process-connection sock)
  "Process client connection SOCK, reading and processing commands."
  (catch 'system-error
    (lambda ()
      (match (read-command sock)
        ((? shepherd-command? command)
         (process-command command sock))
        (#f                                    ;failed to read a valid command
         #f))

      ;; Currently we assume one command per connection.
      (false-if-exception (close sock)))
    (lambda args
      ;; Maybe we got EPIPE while writing to SOCK, or something like that.
      (false-if-exception (close sock)))))

(define* (quit-exception-handler key #:optional value)
  "Handle the 'quit' exception, rebooting if we're running as root."
  ;; Note: The 'quit' exception does not necessarily have an associated value:
  ;; compare (exit 1) with (exit).

  ;; Most likely we're receiving 'quit' from the 'stop' method of
  ;; ROOT-SERVICE.  So, if we're running as 'root', just reboot.
  (if (and (zero? (getuid)) (= 1 (getpid)))
      (begin
        (local-output (l10n "Rebooting..."))
        (reboot))
      (begin
        (local-output (l10n "Exiting."))
        (primitive-exit 0))))              ;leave without going through Fibers

(define (call-with-command-message-port command proc)
  "Call @var{proc} passing it a procedure to retrieve the messages emitted
while evaluating @var{command}."
  (define message-port
    (with-fluids ((%default-port-encoding "UTF-8"))
      (open-output-string)))

  (define (get-messages)
    (let* ((str (get-output-string message-port))
           (lst (string-split str #\newline)))
      ;; 'string-tokenize' swallows empty lines, which is not great,
      ;; and 'string-split' doesn't distinguish between an empty line
      ;; and this empty string, which is not great either.  So we hack
      ;; our way the best we can.
      (cond ((string-null? str)
             '())
            ;; If STR ends in \n, drop the trailing empty string since
            ;; that would lead the client to print an extra newline.
            ((string-suffix? "\n" str)
             (drop-right lst 1))
            (else lst))))

  (parameterize ((%current-client-socket message-port))
    (proc get-messages)))

(define-syntax-rule (with-command-message-port command get-messages
                                               body ...)
  "Evaluate @var{command} and bind @var{get-messages} in the lexical extent of
@var{body} to a thunk to fetch messages emitted while evaluating
@var{command}."
  (call-with-command-message-port command
                                  (lambda (get-messages)
                                    body ...)))

(define (process-command command port)
  "Interpret COMMAND, a command sent by the user, represented as a
<shepherd-command> object.  Send the reply to PORT."
  (match command
    (($ <shepherd-command> version the-action service-symbol (args ...)
                           directory)             ;ignored

     ;; We have to catch `quit' so that we can send the terminator
     ;; line to herd before we actually quit.
     (catch 'quit
       (lambda ()
         (with-command-message-port command get-messages
           (guard (c ((service-error? c)
                      (write-reply (command-reply command #f
                                                  (condition->sexp c)
                                                  (get-messages))
                                   port)))

             (define service
               (or (lookup-service service-symbol)
                   (raise (condition
                           (&missing-service-error (name service-symbol))))))

             (define result
               (case the-action
                 ((start)
                  ;; Return #f or SERVICE: clients expect the service sexp.
                  (if (eq? 'running (service-status service))
                      (begin
                        (local-output (l10n "Service ~a is already running.")
		                      (service-canonical-name service))
                        service)
                      (and (apply start-service service args)
                           service)))
                 ((stop)
                  (if (service-stopped? service)
                      '()
                      (map service-canonical-name
                           (apply stop-service service args))))

                 ;; XXX: This used to return a list of action results, on the
                 ;; grounds that there could be several services called NAME.
                 ;; Clients like 'herd' expect a list so now we return a
                 ;; singleton.
                 (else (list (apply perform-service-action
                                    service the-action args)))))

             (write-reply (command-reply command result #f (get-messages))
                          port))))
       quit-exception-handler))
    (_
     (local-output (l10n "Invalid command.")))))

(define (process-textual-commands port)
  "Process textual commands from PORT.  'Textual' means that they're as you
would write them on the 'herd' command line."
  (let loop ((line (read-line port)))
    (if (eof-object? line)

        ;; Exit on `C-d'.
        (stop-service root-service)

        (begin
          (match (string-tokenize line)
            ((action service arguments ...)
             (process-command (shepherd-command (string->symbol action)
                                                (string->symbol service)
                                                #:arguments arguments)
                              (current-output-port)))
            (_
             (local-output (l10n "invalid command line") line)))
          (loop (read-line port))))))

;; Local Variables:
;; eval: (put 'with-command-message-port 'scheme-indent-function 2)
;; End:
