;; support.scm -- Various support facilities, used by herd and shepherd.
;; Copyright (C) 2014 A.Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Copyright (C) 2013-2014, 2016, 2018, 2020, 2022, 2023 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;; Copyright (C) 2016 Mathieu Lirzin <mthl@gnu.org>
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

(define-module (shepherd support)
  #:use-module (shepherd config)
  #:autoload   (shepherd colors) (color-output? color colorize-string)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:autoload   (srfi srfi-1) (take)
  #:use-module (srfi srfi-9)
  #:use-module ((system repl error-handling)
                #:select (call-with-error-handling)
                #:prefix guile:)
  #:export (caught-error
            assert
            let-loop

            ring-buffer
            ring-buffer?
            ring-buffer-limit
            ring-buffer-insert
            ring-buffer->list

            buffering
            catch-system-error
            with-system-error-handling
            call-with-error-handling
            with-atomic-file-output
            mkdir-p
            with-directory-excursion

            initialize-cli

            %gettext-domain
            l10n
            local-output
            display-version
            program-name
            report-error
            display-line
            non-blocking-port
            blocking-port

            log-output-port
            user-default-log-file
            default-logfile-date-format
            %current-logfile-date-format
            log-with-backtrace
            log.fatal
            log.error
            log.warning
            log.info
            log.debug
            log.dribble
            log-level.fatal
            log-level.error
            log-level.warning
            log-level.info
            log-level.debug
            log-level.dribble

            user-homedir
            default-config-file
            default-socket-dir
            default-socket-file
            %system-socket-file

            primitive-load*
            load-in-user-module
            eval-in-user-module

            %user-log-dir
            %user-config-dir
            %user-runtime-dir

            verify-dir))

;; Report the caught error.
;; FIXME: Needs some more work.
(define (caught-error key args)
  (case key
    ((wrong-number-of-args)
     (apply (lambda (subr fmt fmt-args data)
	      (format #t "In ~a: " subr)
	      (apply format #t fmt fmt-args)
	      (newline))
	    args))
    (else
     (display key)
     (write args)
     (newline))))

;; Assert that expression EXPR does not evaluate to `#f'.
(define-syntax-rule (assert EXPR)
  (and (not EXPR)
       (begin
	 (local-output (l10n "Assertion ~a failed.") 'EXPR)
	 (throw 'assertion-failed))))

(define-syntax-rule (let-loop loop ((variable value) ...)
                              body ...)
  "Similar to a named let, define @var{loop} as a procedure that takes the given
@var{variable}s and their initial @var{value}s.  The main difference is that
@var{loop} is in fact a macro that can be passed a subset of @var{variable}s.
The example below illustrates that:

@example
(let-loop loop ((x 1) (y 2) (z 3))
  (match (get-message channel)
    ('print-x
     (display x)
     (loop))                ;x, y, and z unchanged
    (('set-y value)
     (loop (y value)))))    ;only y gets a new value
@end example

That reduces the amount of boilerplate for loops with many variables."
  (let real-loop ((variable value) ...)
    (define-syntax extract-value
      (syntax-rules (variable ...)
        ;; Extract the value of the variable given as its first argument among
        ;; the given arguments.
        ((_ variable ((variable x) rest (... ...)))
         x)
        ...
        ((_ binding ((_ _) rest (... ...)))
         (extract-value binding (rest (... ...))))
        ((_ binding ())
         binding)))
    (letrec-syntax ((loop (syntax-rules (variable ...)
                            ((_ args (... ...))
                             (real-loop
                              (extract-value variable (args (... ...)))
                              ...)))))
      body ...)))

;; The poor developer's persistent "ring buffer": it holds between N and 2N
;; elements, but has O(1) insertion.
(define-record-type <ring-buffer>
  (%ring-buffer limit front-length front rear)
  ring-buffer?
  (limit         ring-buffer-limit)
  (front-length  ring-buffer-front-length)
  (front         ring-buffer-front)
  (rear          ring-buffer-rear))

(define (ring-buffer size)
  "Return an ring buffer that can hold @var{size} elements."
  (%ring-buffer size 0 '() '()))

(define-inlinable (ring-buffer-insert element buffer)
  "Insert @var{element} to the front of @var{buffer}.  If @var{buffer} is
already full, its oldest element is removed."
  (match buffer
    (($ <ring-buffer> limit front-length front rear)
     (if (< front-length limit)
         (let ((front-length (+ 1 front-length)))
           (%ring-buffer limit front-length
                         (cons element front)
                         (if (= limit front-length)
                             '()
                             rear)))
         (%ring-buffer limit 1
                       (list element) front)))))

(define (ring-buffer->list buffer)
  "Convert @var{buffer} into a list."
  (match buffer
    (($ <ring-buffer> limit front-length front rear)
     (if (= limit front-length)
         front
         (append front (at-most (- limit front-length) rear))))))

(define (at-most max-length lst)
  "If @var{lst} is shorter than @var{max-length}, return it and the empty list;
otherwise return its @var{max-length} first elements and its tail."
  (let loop ((len 0)
             (lst lst)
             (result '()))
    (match lst
      (()
       (values (reverse result) '()))
      ((head . tail)
       (if (>= len max-length)
           (values (reverse result) lst)
           (loop (+ 1 len) tail (cons head result)))))))

(define (buffering port type . args)
  "Return PORT after changing its buffering to TYPE and ARGS."
  (apply setvbuf port type args)
  port)

;; Evaluate `EXPR ...' until a system error occurs, then skip the
;; remaining code.
(define-syntax-rule (catch-system-error EXPR ...)
  (catch 'system-error
    (lambda ()
      EXPR ...)
    (lambda (key . args)
      #f)))

(define (with-atomic-file-output file proc)       ;copied from Guix
  "Call PROC with an output port for the file that is going to replace FILE.
Upon success, FILE is atomically replaced by what has been written to the
output port, and PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (let ((result (proc out)))
          (close out)
          (rename-file template file)
          result))
      (lambda (key . args)
        (catch-system-error (delete-file template))))))

(define* (mkdir-p dir #:optional mode)  ;copied from Guix
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (if mode
                 (mkdir path mode)
                 (mkdir path))
             (loop tail path))
           (lambda args
             ;; On GNU/Hurd we can get EROFS instead of EEXIST here.  Thus, if
             ;; we get something other than EEXIST, check whether PATH exists.
             ;; See <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg00049.html>.
             (if (or (= EEXIST (system-error-errno args))
                     (let ((st (stat path #f)))
                       (and st (eq? 'directory (stat:type st)))))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define-syntax-rule (with-directory-excursion dir body ...) ;copied from Guix
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
   (dynamic-wind
     (lambda ()
       (chdir dir))
     (lambda ()
       body ...)
     (lambda ()
       (chdir init)))))



(define %gettext-domain
  ;; The gettext message domain.
  "shepherd")

(define (initialize-cli)
  "Perform the usual initialization for stand-alone Shepherd commands."
  ;; By default don't annoy users with deprecation warnings.  In practice,
  ;; 'define-deprecated' in (ice-9 deprecated) arranges so that those warnings
  ;; are emitted at expansion-time only, but there are cases where they could
  ;; slip through, for instance when interpreting code.
  ;; (unless (getenv "GUILE_WARN_DEPRECATED")
  ;;   (debug-disable 'warn-deprecated))

  (bindtextdomain %gettext-domain %localedir)
  (textdomain %gettext-domain)
  (setvbuf (current-output-port) 'line)
  (setvbuf (current-error-port) 'line))

;; Localized version of STR.
(define-syntax l10n
  (syntax-rules ()
    ((_ str) (gettext str))
    ((_ singular plural n) (ngettext singular plural n))))

;; Display some text and a newline.
(define-syntax-rule (local-output format-string args ...)
  (begin
    (format #t format-string args ...)
    (newline)))

(define* (display-version #:optional (program-name (program-name)))
  (local-output "~a (~a) ~a" program-name package-name Version)
  ;; TRANSLATORS: '~a' is a placeholder for the copyright year.
  (local-output (l10n "Copyright (C) ~a the Shepherd authors") 2023)
  (local-output (l10n "\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.")))

(define program-name
  ;; Name of the program currently executing.
  (make-parameter "shepherd"))

(define (print-error-message message)
  "Print @var{message} to the current error port, prefixing it in standard GNU
error format."
  (define colorize
    (if (color-output? (current-error-port))
        (lambda (str)
          (colorize-string str (color BOLD RED)))
        identity))

  (format (current-error-port) "~a: ~a: ~a~%" (program-name)
          (colorize (l10n "error")) message))

(define-syntax report-error
  (lambda (s)
    "Report the given error message to stderr in standard GNU error format."
    (syntax-case s (l10n)
      ((_ (l10n message) args ...)
       (string? (syntax->datum #'message))

       #'(print-error-message (format #f (l10n message) args ...))))))

(define* (call-with-error-handling thunk error-handler)
  "Calls thunk and makes sure no exeptions are leaving this call.  If any error
happens inside thunk, then it calls the user supplied error-handler.  In case
of nested errors it tries to supply information in the log that helps
debugging the issue."
  (guile:call-with-error-handling
   thunk
   #:on-error
   (lambda args
     (guile:call-with-error-handling
      (lambda ()
        ;; Level 1: call the user's error handler.
        (apply error-handler args))
      #:on-error
      (lambda (error-key . error-args)
        (guile:call-with-error-handling
         (lambda ()
           ;; Level 2: by default we attempt to log a backtrace and the error
           ;; coming from the level 1 handler.
           (log-with-backtrace
            log-level.error
            error-key error-args
            "call-with-error-handling: nested error in the level 1 handler."))
         #:on-error
         (lambda _
           ;; Level 3: let's guard against any errors here, but still try to
           ;; emit some info that is useful for debugging.
           (let ((message "Shepherd: call-with-error-handling has reached level 3, giving up."))
             (unless (false-if-exception
                      (begin
                        (display message)
                        #t))
               (false-if-exception
                (warn message)))
             (values)))))))))

(define (call-with-system-error-handling thunk)
  "Call THUNK, catching any 'system-error' exception."
  (catch 'system-error
    thunk
    (lambda (key proc format-string format-args . rest)
      (report-error (l10n "~a")
                    (apply format #f format-string format-args))
      (quit 1))))

(define-syntax-rule (with-system-error-handling body ...)
  "Evaluate BODY in a context where 'system-error' throws are caught and
turned into user error messages."
  (call-with-system-error-handling
   (lambda ()
     body ...)))

(define* (display-line message #:optional (port (current-output-port)))
  "Display MESSAGE followed by a newline to PORT."
  (display message port)
  (newline port))

(define (non-blocking-port port)
  "Return PORT after putting it in non-blocking mode."
  (let ((flags (fcntl port F_GETFL)))
    (fcntl port F_SETFL (logior O_NONBLOCK flags))
    port))

(define (blocking-port port)
  "Return PORT after putting it in non-blocking mode."
  (let ((flags (fcntl port F_GETFL)))
    (fcntl port F_SETFL (logand (lognot O_NONBLOCK) flags))
    port))



;; Home directory of the user.
(define user-homedir
  ;; Look for $HOME first, to allow users to override the defaults.  This is
  ;; notably useful when shepherd is built in a Guix chroot.
  (or (getenv "HOME")

      ;; When bootstrapping and running as PID 1, /etc/{passwd,shadow} may be
      ;; unavailable.  Gracefully handle that.
      (false-if-exception (passwd:dir (getpwuid (getuid))))
      "/"))

(define %user-config-dir
  ;; Default config directory if shepherd is run as a normal user.
  (string-append (or (getenv "XDG_CONFIG_HOME")
                     (string-append user-homedir "/.config"))
                 "/shepherd"))

(define %user-log-dir
  ;; Default log directory if shepherd is run as a normal user.
  (string-append (or (getenv "XDG_STATE_HOME")
                     (string-append user-homedir "/.local/state"))
                 "/shepherd"))

(define %user-runtime-dir
  ;; Default runtime directory if shepherd is run as a normal user.
  (string-append (or (getenv "XDG_RUNTIME_DIR")
                     (format #f "/run/user/~s" (getuid)))))

(define (make-bare-init-file target)
  "Return #t if a bare init file was created at TARGET; #f otherwise.

TARGET should be a string representing a filepath + name."
  (with-output-to-file target
    (lambda ()
      (display
       (string-append
        ;; TRANSLATORS: Please keep double semicolons at the beginning of each
        ;; line: they introduce comments in the Scheme language.  Also, keep
        ;; "shepherd" untranslated.  Thank you!  :-)
        (l10n ";; init.scm -- default shepherd configuration file.\n")
        "\n"
        (l10n "\
;; Services known to shepherd:
;; Add new services (defined using 'service') to shepherd here by
;; providing them as arguments to 'register-services'.\n")
        "(register-services)\n\n"
        (l10n "\
;; Send shepherd into the background\n")
        "(perform-service-action 'shepherd 'daemonize)\n\n"
        (l10n "\
;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.\n")
        "(for-each start '())\n")))))

;; Logging.
(define log-output-port
  ;; Port for logging.  This must always be a valid port, never `#f'.
  (make-parameter (%make-void-port "w")))

(define default-logfile-date-format
  ;; 'strftime' format string to prefix each entry in the log.
  "%Y-%m-%d %H:%M:%S ")

(define %current-logfile-date-format
  ;; 'strftime' format strings for entries in the log file.
  (make-parameter default-logfile-date-format))

(define (user-default-log-file)
  "Return the file name of the user's default log file."
  (mkdir-p %user-log-dir #o700)
  (string-append %user-log-dir "/shepherd.log"))

(eval-when (expand load eval)
  (define log-level.fatal   0)
  (define log-level.error   1)
  (define log-level.warning 2)
  (define log-level.info    3)
  (define log-level.debug   4)
  (define log-level.dribble 5)

  ;; This can be used to drop entire log levels at compile time.  Dropping
  ;; dribble by default sounds like a good idea.  It can be enabled locally by
  ;; developers while debugging an elusive bug.
  (define %log-level-at-compile-time log-level.dribble)

  (define (logger-body-expander level)
    (lambda (s)
      (if (<= level %log-level-at-compile-time)
          (syntax-case s (level)
            ((_ format-string args ...)
             #`(%handle-log-entry #,level format-string args ...))
            ((_ format-string)
             #`(%handle-log-entry #,level format-string)))
          #'(values)))))

(define %log-level-at-runtime log-level.debug)

(define-syntax log.fatal   (logger-body-expander log-level.fatal))
(define-syntax log.error   (logger-body-expander log-level.error))
(define-syntax log.warning (logger-body-expander log-level.warning))
(define-syntax log.info    (logger-body-expander log-level.info))
(define-syntax log.debug   (logger-body-expander log-level.debug))
(define-syntax log.dribble (logger-body-expander log-level.dribble))

(define (%handle-log-entry level format-string . args)
  (when (<= level %log-level-at-runtime)
    (call-with-error-handling
     (lambda ()
       (let* ((prefix (strftime (%current-logfile-date-format)
                                (localtime (current-time))))
              (msg (apply format #f format-string args)))
         ;; NOTE when format was directed into the log-output-port directly,
         ;; then in some situations (e.g. kernel early log output) each char
         ;; was printed on a separate line.
         (display (string-append prefix msg "\n") (log-output-port))))

     (lambda (error . error-args)
       (log-with-backtrace
        log-level.error error error-args
        "An error from inside the logging infrastructure \
is being ignored:~% ~A: ~S~%" error error-args))))
  (values))

(define (log-with-backtrace log-level error-key error-args format-string . args)
  (apply %handle-log-entry log-level format-string args)
  (let ((port (log-output-port)))
    (display-backtrace (make-stack #t) port)
    (print-exception port
                     (stack-ref (make-stack #t) 1)
                     error-key error-args)))

;; Configuration file.
(define (default-config-file)
  "Return the default configuration file---either the user's file, or the
global system configuration file when running as 'root'.  As a side effect,
create a template configuration file if non exists."
  (if (zero? (getuid))
      (string-append %sysconfdir "/shepherd.scm")
      (let ((config-file (string-append %user-config-dir "/init.scm")))
        (mkdir-p %user-config-dir #o700)
        (if (not (file-exists? config-file))
            (make-bare-init-file config-file))
        config-file)))

;; Socket directory for the system's instance (PID 1).
(define %system-socket-dir
  (string-append %runstatedir "/shepherd"))

;; The directory where the socket resides.
(define default-socket-dir
  (if (zero? (getuid))
      %system-socket-dir
      (string-append %user-runtime-dir "/shepherd")))

;; Unix domain socket for receiving commands in shepherd.
(define default-socket-file
  (string-append default-socket-dir "/socket"))

;; Location of the socket of the system's instance (PID 1).
(define %system-socket-file
  (string-append %system-socket-dir "/socket"))

(define (make-user-module)
  "Return a new module, for use when evaluating the user's configuration,
which has essential bindings pulled in."
  (let ((m (make-fresh-user-module)))
    ;; The typical configuration file wants to do '(service ...)', and
    ;; '(register-services ...)', so provide the relevant bindings by default.
    (module-use! m (resolve-interface '(oop goops)))
    (module-use! m (resolve-interface '(shepherd service)))
    m))

(define (primitive-load* file)
  ;; Like 'primitive-load', but in Scheme, so that it does not introduce a
  ;; continuation barrier that would prevent code in FILE from suspending.
  (call-with-input-file file
    (lambda (port)
      (let loop ((result *unspecified*))
       (match (read port)
         ((? eof-object?)
          result)
         (exp
          (loop (primitive-eval exp))))))))

(define (load-in-user-module file)
  "Load FILE in a fresh user module that has essential bindings pulled in."
  (log.debug "load-in-user-module for file '~a'" file)
  (let ((user-module (make-user-module)))
    (log.dribble "load-in-user-module for file '~a' created module ~a" file user-module)
    (save-module-excursion
     (lambda ()
       (set-current-module user-module)
       (primitive-load* file)))))

(define (eval-in-user-module exp)
  "Eval EXP in a fresh user module that has essential bindings pulled in."
  (let ((user-module (make-user-module)))
    (save-module-excursion
     (lambda ()
       ;; Note: As of Guile 3.0.8, 'eval' is written in C, and is thus a
       ;; continuation barrier.  Use 'primitive-eval' to avoid that.
       (set-current-module user-module)
       (primitive-eval exp)))))

(define* (verify-dir dir #:key (secure? #t))
  "Check if the directory DIR exists and create it if it is the default
directory, but does not exist.  If SECURE? is false, permissions of the
directory are not checked."
  (and (string=? dir default-socket-dir)
       ;; If it exists already, this is fine, thus ignore errors.
       (catch-system-error
        (mkdir-p default-socket-dir #o700)))
  ;; Check for permissions.
  (when secure?
    (let ((dir-stat (stat dir)))
      (unless (and (= (stat:uid dir-stat) (getuid))
                   (= (stat:perms dir-stat) #o700))
        (report-error (l10n "~a: socket directory has insecure permissions")
                      dir)
        (exit 1)))))
