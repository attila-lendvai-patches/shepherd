;; comm.scm -- Communication between processes and general output.
;; Copyright (C) 2013, 2014, 2016, 2018, 2019, 2022, 2023 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
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

(define-module (shepherd comm)
  #:use-module (shepherd support)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (open-connection
            open-server-socket

            <shepherd-command>
            shepherd-command?
            shepherd-command
            shepherd-command-version
            shepherd-command-directory
            shepherd-command-action
            shepherd-command-service
            shepherd-command-arguments

            <command-reply>
            command-reply
            command-reply?
            command-reply-command
            command-reply-result
            command-reply-error
            command-reply-messages

            write-command
            read-command

            write-reply
            define-record-type-serializer
            result->sexp
            report-command-error

            log-output-port
            syslog-output-port
            make-shepherd-output-port

            %current-client-socket
            %current-logfile-date-format
            %current-service-output-port))


;; Command for shepherd.
(define-record-type <shepherd-command>
  (%shepherd-command version action service args directory)
  shepherd-command?
  (version   shepherd-command-version)            ; list of integers
  (action    shepherd-command-action)             ; symbol
  (service   shepherd-command-service)            ; symbol
  (args      shepherd-command-arguments)          ; list of strings
  (directory shepherd-command-directory))         ; directory name

(define %protocol-version
  ;; Current client protocol version.
  '(0))

(define* (shepherd-command action service
                           #:key
                           (version %protocol-version)
                           (arguments '()) (directory (getcwd)))
  "Return a new command for ACTION on SERVICE."
  (%shepherd-command version action service arguments directory))

(define* (open-connection #:optional (file default-socket-file))
  "Open a connection to the daemon, using the Unix-domain socket at FILE, and
return the socket."
  ;; The protocol is sexp-based and UTF-8-encoded.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let ((sock    (socket PF_UNIX SOCK_STREAM 0))
          (address (make-socket-address PF_UNIX file)))
      (catch 'system-error
        (lambda ()
          (connect sock address)
          (setvbuf sock 'block 1024))
        (lambda (key proc format-string format-args errno . rest)
          ;; Guile's 'connect' throws an exception that doesn't specify
          ;; FILE.  Augment it with this information.
          (apply throw
                 key proc
                 "~A: ~A" (list file (strerror (car errno)))
                 (list errno) rest)))
      sock)))

(define (open-server-socket file-name)
  "Open a socket at FILE-NAME, and listen for connections there."
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let ((sock    (socket PF_UNIX
                           (logior SOCK_STREAM SOCK_NONBLOCK SOCK_CLOEXEC)
                           0))
          (address (make-socket-address AF_UNIX file-name)))
      (catch-system-error (delete-file file-name))
      (bind sock address)
      (listen sock 10)
      sock)))

(define (read-command port)
  "Receive a command from PORT; return the command of #f if something went
wrong---premature end-of-file, invalid sexp, etc."
  (catch 'read-error
    (lambda ()
      (match (read port)
        (('shepherd-command ('version version ...)
                            ('action action)
                            ('service service)
                            ('arguments (args ...))
                            ('directory directory))
         (match version
           ((0 _ ...)
            (shepherd-command action service
                              #:version version
                              #:arguments args
                              #:directory directory))
           (_
            #f)))
        (_                                        ;EOF or unmatched sexp
         #f)))
    (lambda _
      ;; Invalid sexp.
      #f)))

(define (write-command command port)
  "Write COMMAND to PORT."
  (match command
    (($ <shepherd-command> version
        action service (arguments ...) directory)
     (write `(shepherd-command (version ,@version) ;protocol version
                               (action ,action)
                               (service ,service)
                               (arguments ,arguments)
                               (directory ,directory))
            port)

     ;; PORT may be buffered so make sure the command goes out.
     (force-output port))))


;; Replies to commands.

(define-record-type <command-reply>
  (command-reply command result error messages)
  command-reply?
  (command  command-reply-command)                ;command
  (result   command-reply-result)                 ;sexp | #f
  (error    command-reply-error)                  ;#f | sexp
  (messages command-reply-messages))              ;list of strings

(define (write-reply reply port)
  "Write REPLY to PORT."
  (match reply
    (($ <command-reply> command result error (messages ...))
     ;; Use 'result->sexp' to convert RESULT to an sexp.  We don't do that for
     ;; ERROR because using GOOPS methods doesn't work for SRFI-35 error
     ;; conditions, and that's what we're using here. (XXX)
     (write `(reply (version 0)
                    (result ,(result->sexp result))
                    (error ,error)
                    (messages ,messages))
            port)

     ;; PORT may be buffered so make sure the command goes out.
     (force-output port))))

(define %record-serializers
  ;; Hash table mapping record type descriptors (RTDs) to procedures that
  ;; "convert" instances to an sexp.
  (make-hash-table 3))

(define-syntax-rule (define-record-type-serializer (name (obj type))
                      body ...)
  "Define @var{name} as a procedure that, given @var{obj}, a record of
@var{type}, returns an sexp serialization of that record."
  (hashq-set! %record-serializers type
              (lambda (obj)
                body ...)))

(define (result->sexp obj)
  "Return the sexp representation of @var{obj}, a result meant to go in a
@code{<command-reply>} object."
  (cond ((or (boolean? obj) (number? obj) (symbol? obj)
             (string? obj) (keyword? obj))
         obj)
        ((list? obj)
         (map result->sexp obj))
        ((pair? obj)
         (cons (result->sexp (car obj)) (result->sexp (cdr obj))))
        ((struct? obj)
         (let ((serializer (hashq-ref %record-serializers
                                      (struct-vtable obj))))
           (if serializer
               (serializer obj)
               (object->string obj))))
        (else
         (object->string obj))))

(define (report-command-error error)
  "Report ERROR, an sexp received by a shepherd client in reply to COMMAND, a
command object."
  (match error
    (('error ('version 0 _ ...) 'service-not-found service)
     ;; TRANSLATORS: Strings occasionally contain escape sequences starting
     ;; with '~' (tilde).  For example, '~a' corresponds to '%s' in C printf
     ;; syntax and '~%' corresponds to '\n'.  These must be preserved as is.
     ;; See
     ;; <https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html>
     ;; for more info.
     (report-error (l10n "service '~a' could not be found")
                   service))
    (('error ('version 0 _ ...) 'action-not-found action service)
     (report-error (l10n "service '~a' does not have an action '~a'")
                   service action))
    (('error ('version 0 _ ...) 'action-exception action service
             key (args ...))
     (report-error (l10n "exception caught while executing '~a' \
on service '~a':")
                   action service)
     (print-exception (current-error-port) #f key args))
    (('error . _)
     (report-error (l10n "something went wrong: ~s")
                   error))
    (#f                                           ;not an error
     #t)))



(define log-output-port
  ;; Port for logging.  This must always be a valid port, never `#f'.
  (make-parameter (%make-void-port "w")))

(define %current-client-socket
  ;; Socket of the client currently talking to the daemon.
  (make-parameter #f))

(define %current-logfile-date-format
  ;; 'strftime' format strings for entries in the log file.
  (make-parameter default-logfile-date-format))

(define call-with-syslog-port
  (let ((port #f))                                ;connection to /dev/log
    (lambda (proc)
      "Call PROC with an open output port.  The output port corresponds to
/dev/log (aka. syslog) or, if that is unavailable, a degraded logging
mechanism."
      (define (call/syslog)
        (catch 'system-error
          (lambda ()
            (proc port))
          (lambda args
            (if (memv (system-error-errno args)
                      (list ENOTCONN ECONNREFUSED EPIPE))
                (begin
                  (set! port #f)
                  (call-with-syslog-port proc))
                (apply throw args)))))

      (or (and port (not (port-closed? port)) (call/syslog))
          (let ((sock (socket AF_UNIX
                              (logior SOCK_CLOEXEC SOCK_DGRAM)
                              0)))
            (catch 'system-error
              (lambda ()
                (connect sock AF_UNIX "/dev/log")
                (setvbuf sock 'line)
                (set! port sock)
                (call/syslog))
              (lambda args
                (close-port sock)
                (if (memv (system-error-errno args)
                          (list ENOENT ECONNREFUSED))
                    (catch 'system-error
                      (lambda ()
                        (call-with-output-file "/dev/kmsg"
                          (lambda (port)
                            (setvbuf port 'block)
                            (proc port))))
                      (lambda args
                        (if (memv (system-error-errno args)
                                  (list ENOENT EACCES EPERM))
                            (call-with-output-file "/dev/console"
                              (lambda (port)
                                (setvbuf port 'none)
                                (proc port)))
                            (apply throw args))))
                    (apply throw args)))))))))

(define (syslog-output-port)
  "Return the output port to write to syslog or /dev/kmsg, whichever is
available."
  (make-soft-port
   (vector
    (lambda (char)                                ;write char
      (call-with-syslog-port
       (lambda (port)
         (write-char char port))))
    (lambda (str)                                 ;write string
      (call-with-syslog-port
       (lambda (port)
         (display str port))))
    (const #t)                                    ;flush
    #f                                            ;get char
    (lambda ()                                    ;close
      (call-with-syslog-port close-port)))
   "w"))                                          ;output port

;; We provide our own output mechanism, because we have certain
;; special needs; most importantly, we want to send output to herd
;; sometimes.
(define* (make-shepherd-output-port
          #:optional (original-output-port (current-output-port)))
  (make-soft-port
   (vector

    ;; One character for output.
    (lambda (char)
      (display (string char)))

    ;; A string for output.
    (let ((buffer '())) ;; List of unwritten output strings.
      (lambda (str)
        ;; When herd is connected, send it the output; otherwise, in the
        ;; unlikely case nobody is listening, send to the standard output.
        (if (%current-client-socket)
            (catch-system-error
             (display str (%current-client-socket)))
            (display str original-output-port))

        ;; Logfile, buffer line-wise and output time for each
        ;; completed line.
        (if (not (string-index str #\newline))
            (set! buffer (cons str buffer))
            (let ((str (string-concatenate-reverse (cons str buffer))))
              (define prefix
                (strftime (%current-logfile-date-format)
                          (localtime (current-time))))

              ;; Note: We want to render as many newlinew as present in STR,
              ;; so neither 'string-split' nor 'string-tokenize' helps.
              (let loop ((str str))
                (let* ((index (string-index str #\newline))
                       (line  (if index (string-take str (+ 1 index)) str)))
                  (unless (string-null? str)
                    ;; Make exactly one 'display' call per line to make sure we
                    ;; don't create several entries for each line.
                    (display (string-append prefix line) (log-output-port))
                    (when index
                      (loop (string-drop str (+ index 1)))))))

              (set! buffer '())))))

    ;; Flush output.
    (lambda ()
      ;; FIXME: Do we need to do something?  Flush the logfile buffer?
      #t)

    ;; Get a character (unused).
    #f

    ;; Close the port.
    (lambda () #t))

   ;; It's an output-only port.
   "w"))

(define %current-service-output-port
  ;; The output port that services should write to.
  (make-parameter (current-output-port)))
