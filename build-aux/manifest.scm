;;; manifest.scm -- Guix manifest for continuous integration.
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of the GNU Shepherd.
;;;
;;; The GNU Shepherd is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The GNU Shepherd is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (guix)
             (guix profiles)
             (shepherd-package)
             (gnu system)
             ((gnu system vm)
              #:select (virtual-machine
                        virtualized-operating-system))
             (gnu services shepherd)
             (gnu tests)
             (gnu tests base)
             (srfi srfi-1))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define native-builds
  (manifest
   (append-map (lambda (system)
                 (map (lambda (package)
                        (package->manifest-entry* package system))
                      (list shepherd
                            shepherd-from-tarball
                            guile2.2-shepherd)))
               '("x86_64-linux"
                 "i686-linux"
                 "aarch64-linux" "armhf-linux"
                 "powerpc64le-linux"))))

(define cross-builds
  (manifest
   (map (lambda (target)
          (package->manifest-entry* shepherd-from-tarball "x86_64-linux"
                                    #:target target))
        '(;; "i586-pc-gnu"          ;FIXME: requires Fibers 1.2.0+
          "aarch64-linux-gnu"
          "riscv64-linux-gnu"))))

(define (operating-system-with-latest-shepherd os)
  "Return @var{os}, running with the current Shepherd."
  (operating-system
    (inherit os)
    (essential-services
     (modify-services (operating-system-default-essential-services
                       this-operating-system)
       (shepherd-root-service-type
        config => (shepherd-configuration
                   (shepherd shepherd)))))))

(define system-test/base
  ;; "Base" system test running against the latest Shepherd.
  (system-test
   (name "system-test-base")
   (description "Test Guix System with the latest Shepherd.")
   (value
    (let* ((os (marionette-operating-system
                (operating-system-with-latest-shepherd %simple-os)
                #:imported-modules '((gnu services herd)
                                     (guix combinators))))
           (vm (virtual-machine os)))
      ;; XXX: Add call to 'virtualized-operating-system' to get the exact same
      ;; set of services as the OS in VM.
      (run-basic-test (virtualized-operating-system os '())
                      #~(list #$vm)
                      name)))))

(define (system-test->manifest-entry test)
  "Return a manifest entry for @var{test}, a system test."
  (manifest-entry
    (name (system-test-name test))
    (version "0")
    (item test)))

(define system-tests
  (manifest
   (map system-test->manifest-entry
        (list system-test/base))))

(concatenate-manifests (list native-builds cross-builds system-tests))
