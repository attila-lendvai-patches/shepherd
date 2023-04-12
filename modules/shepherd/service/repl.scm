;; repl.scm -- Read-eval-print loop.
;; Copyright (C) 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (shepherd service repl)
  #:use-module (shepherd service)
  #:use-module (shepherd support)
  #:use-module ((shepherd comm) #:select (open-server-socket))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers io-wakeup)
  #:autoload   (system repl repl) (start-repl)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (default-repl-socket-file
            repl-service))

(define (spawn-child-service client id)
  "Register and start a new service that runs a REPL on @var{client}, a
socket.  Use @var{id} to create the service name."
  (letrec* ((name  (string->symbol
                    (string-append "repl-client-"
                                   (number->string id))))
            (child (service
                     (list name)
                     #:transient? #t
                     #:start (lambda ()
                               (spawn-fiber
                                (lambda ()
                                  (run-client-repl child client)))
                               client)
                     #:stop (lambda (client)
                              (close-port client)
                              #f))))
    (register-services child)
    (start child)))

(define* (run-repl-service socket)
  (let loop ((client-id 1))
    (match (accept socket (logior SOCK_NONBLOCK SOCK_CLOEXEC))
      ((client . client-address)
       ;; TRANSLATORS: "REPL" stands for "read-eval-print loop".
       (local-output (l10n "Accepting REPL connection.")
                     client-address)
       (spawn-child-service client client-id)
       (loop (+ client-id 1)))
      (_ #f))))

(define (spawn-repl-service socket)
  "Spawn a REPL service that accepts connection on @var{socket}."
  (spawn-fiber
   (lambda ()
     (run-repl-service socket)))
  #t)

(define user-module
  (let ((module (resolve-module '(shepherd-user) #f #f #:ensure #t)))
    (beautify-user-module! module)
    (module-set! module 'sleep (@ (fibers) sleep)) ;avoid that pitfall
    module))

(define (run-client-repl service client)
  "Return a REPL on @var{client}, a socket.  When the REPL terminates or
crashes, stop @var{service}."
  (catch #t
    (lambda ()
      (parameterize ((current-input-port client)
                     (current-output-port client)
                     (current-error-port client)
                     (current-warning-port client))
        (save-module-excursion
         (lambda ()
           (set-current-module user-module)
           (with-fluids ((*repl-stack* '()))
             (start-repl))))))
    (lambda args
      (local-output (l10n "Uncaught REPL exception: ~s.") args)))
  (stop-service service))

(define default-repl-socket-file
  ;; Default socket file for the REPL.
  (make-parameter (string-append default-socket-dir "/repl")))

(define* (repl-service #:optional
                       (socket-file (default-repl-socket-file)))
  "Return a REPL service that listens to @var{socket-file}."
  (service
    '(repl)
    #:documentation (l10n "Run a read-eval-print loop (REPL).")
    #:requirement '()
    #:start (lambda args
              (catch-system-error (delete-file socket-file))
              (let ((socket (open-server-socket socket-file)))
                (spawn-repl-service socket)
                socket))
    #:stop (lambda (socket)
             (close-port socket)
             #f)))
