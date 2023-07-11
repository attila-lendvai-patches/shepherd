;;; shepherd-package.scm -- Build the Shepherd with GNU Guix.
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

;; This file defines a Guix package.  It can be used to spawn an
;; interactive development environment:
;;
;;   guix shell
;;
;; Or it can be used to build Guile from a checkout in an isolated
;; environment:
;;
;;   guix build -f guix.scm
;;
;; Likewise, you may cross-compile it:
;;
;;   guix build -f guix.scm --target=x86_64-w64-mingw32
;;
;; … or perform a native build for another architecture, assuming
;; either offloading or transparent QEMU emulation is set up:
;;
;;   guix build -f guix.scm -s riscv64-linux

(define-module (shepherd-package)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system gnu) #:select (dist-package))
  #:use-module (guix modules)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo))

(define %shepherd-version "0.10.99")

(define source-checkout
  (let ((vcs-file? (or (git-predicate
                        (string-append (current-source-directory)
                                       "/../.."))
                       (const #t))))
    (local-file "../.." "shepherd-checkout"
                #:recursive? #t
                #:select? vcs-file?)))

(define development-packages
  ;; Packages needed when building from Git.
  '("autoconf" "automake" "gettext" "texinfo" "help2man"))

(define-public shepherd
  (package
    (name "shepherd")
    (version (string-append %shepherd-version "-git"))
    (source source-checkout)
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~'("--localstatedir=/var")
           #:phases
           (if (%current-target-system)
               #~(modify-phases %standard-phases
                   (add-before 'configure 'set-fibers-directory
                     (lambda _
                       ;; When cross-compiling, refer to the target
                       ;; Fibers, not the native one.
                       (substitute* '("herd.in" "shepherd.in")
                         (("%FIBERS_SOURCE_DIRECTORY%")
                          #$(file-append
                             (this-package-input "guile-fibers")
                             "/share/guile/site/3.0"))
                         (("%FIBERS_OBJECT_DIRECTORY%")
                          #$(file-append
                             (this-package-input "guile-fibers")
                             "/lib/guile/3.0/site-ccache"))))))
               #~%standard-phases)))

    (native-inputs
     ;; Use 'specification->package' to get the latest version of those
     ;; development tools.
     (append (map specification->package development-packages)
             (list pkg-config guile-3.0-latest
                   guile-fibers-1.3)))            ;for cross-compilation
    (inputs (list guile-3.0-latest guile-fibers-1.3))
    (synopsis "System service manager")
    (description
     "The GNU Shepherd is a daemon-managing daemon, meaning that it supervises
the execution of system services, replacing similar functionality found in
typical init systems.  It provides dependency-handling through a convenient
interface and is based on GNU Guile.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/shepherd/")))

(define source-tarball
  ;; Tarball make from the Git checkout.
  ;; XXX: Timestamps in the tarball are non-deterministic.
  (dist-package shepherd source-checkout
                #:phases #~(modify-phases %dist-phases
                             (replace 'build-dist
                               (lambda args
                                 ;; Run "make" before "make distcheck".
                                 (setenv "TAR_OPTIONS" "\
 --mtime=2023-01-01 --owner=guix --group=guix --sort=name")
                                 (apply (assoc-ref %dist-phases 'build-dist)
                                        #:build-before-dist? #t
                                        #:dist-target "dist"
                                        #:tests? #f
                                        args))))))

(define-public shepherd-from-tarball
  ;; Built from a tarball.  This is useful for two reasons: as some sort of a
  ;; "distcheck" verification, and to support cross-compilation (since
  ;; building man pages with help2man is not supported in a cross-compilation
  ;; context).
  (package
    (inherit shepherd)
    (version (string-append %shepherd-version "-tarball"))
    (source source-tarball)
    (arguments
     (substitute-keyword-arguments (package-arguments shepherd)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'unpack
              (lambda _
                (define source
                  #+(package-source this-package))

                ;; Locate a tarball within SOURCE and unpack it.
                (invoke "tar" "xvf"
                        (car (find-files source "\\.tar.gz$")))
                (let ((directory
                       (car (find-files "."
                                        (lambda (file stat)
                                          (and (string-prefix?
                                                "shepherd" (basename file))
                                               (eq? 'directory
                                                    (stat:type stat))))
                                        #:directories? #t))))
                  (format #t "changing directory to '~a'~%" directory)
                  (chdir directory))))))))
    (native-inputs
     (modify-inputs (package-native-inputs shepherd)
       (delete "autoconf" "automake" "gettext" "texinfo" "help2man")))))

(define-public guile2.2-shepherd
  (package
    (inherit shepherd)
    (name "guile2.2-shepherd")
    (native-inputs
     (append (map specification->package development-packages)
             (list pkg-config guile-2.2)))
    (inputs (list guile-2.2 guile2.2-fibers))))

;; Return the Shepherd package that lets you build from Git, for the benefit
;; of 'guix shell'.
shepherd
