;; monitor.scm -- Monitoring service.
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

(define-module (shepherd service monitoring)
  #:use-module (shepherd service)
  #:use-module (shepherd support)
  #:use-module ((fibers) #:hide (sleep))
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 ftw) (scandir)
  #:use-module (oop goops)
  #:export (monitoring-service))

(define (log-monitoring-stats)
  "Log info about useful metrics: heap size, open file descriptors, etc."
  (local-output (l10n "heap: ~,2f MiB; file descriptors: ~a")
                (/ (assoc-ref (gc-stats) 'heap-size) (expt 2. 20))
                (length
                 (or (scandir "/proc/self/fd"
                              (lambda (file)
                                (not (member file '("." "..")))))
                     '()))))

(define default-monitoring-period
  ;; Default logging period, in seconds.
  (make-parameter (* 20 60)))

(define* (run-monitoring-service channel
                                 #:key
                                 (period (default-monitoring-period)))
  (log-monitoring-stats)
  (let loop ((period period))
    (match (get-message* channel period 'log)
      ('stop
       (log-monitoring-stats)                     ;one last time
       (local-output (l10n "Terminating shepherd monitoring."))
       #f)
      ('log
       (log-monitoring-stats)
       (loop period))
      (('set-period period)
       (local-output (l10n "Monitoring logging period changed to ~a seconds.")
                     period)
       (loop period)))))

(define* (spawn-monitoring-service #:key (period (default-monitoring-period)))
  "Spawn the monitoring service and return a channel to communicate with it."
  (let ((channel (make-channel)))
    (spawn-fiber
     (lambda ()
       (run-monitoring-service channel
                               #:period period)))
    channel))

(define* (monitoring-service #:key
                             (period (default-monitoring-period)))
  "Return a service that will monitor shepherd resource usage by printing it
every @var{period} seconds."
  (service
    '(monitoring)
    #:documentation "Periodically log shepherd resource usage information."
    #:requirement '()
    #:start (lambda args
              (spawn-monitoring-service #:period period))
    #:stop (lambda (channel)
             (put-message channel 'stop)
             #f)
    #:actions
    (make-actions
     (period
      "Set the logging period (in minutes) of the monitoring system."
      (lambda (channel period)
        (define (positive-integer? n)
          (and (integer? n) (> n 0)))

        (match (string->number period)
          ((? positive-integer? period)
           (put-message channel `(set-period ,period)))
          (#f
           (local-output
            (l10n "~a: invalid number; expected a positive integer~%")
            period)))))
     (log
      "Log monitoring info right away."
      (lambda (channel)
        (log-monitoring-stats))))))
