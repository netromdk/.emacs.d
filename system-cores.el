;;; system-cores.el --- Find out how many processors and cores you have

;; Copyright (C) 2013 Aaron Miller. All rights reversed.
;; Share and Enjoy!

;; Last revision: Thursday, December 19, 2013, ca. 10:30.

;; Author: Aaron Miller <me@aaron-miller.me>

;; This file is not part of Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see `http://www.gnu.org/licenses'.

;;; Commentary:

;; Someone on Stack Overflow asked [1] whether there was a
;; platform-independent method of finding out how many processors a
;; given machine has. It turns out there's not, so I wrote one.

;; "Platform-independent" is stretching a point; the best I could come
;; up with was to define a function `system-cores', which uses a
;; lookup table (`system-cores-delegate-alist'), keyed by
;; `system-type' values, to find and invoke a function which knows how
;; to get the processor and core counts for a given platform. This
;; means that some platforms, namely those for which I was able to
;; find reliable instructions on how to obtain processor and core
;; counts, are supported quite well, while others aren't supported at
;; all. On the other hand, no truly platform-independent interface to
;; that information exists at any level, as far as I've been able to
;; find out, so this works about as well as I think anything could be
;; expected to do. (And if you use a system which isn't supported,
;; feel free to write a delegate and submit a pull request on Github,
;; or email me a diff!)

;; To use this code, drop this file into your Emacs load path, then
;; (require 'system-cores), and finally invoke the function
;; `system-cores' to retrieve processor and core count
;; information. See that function's documentation for additional
;; details on its invocation and behavior.

;; This library relies strongly on the `process-lines' function. If
;; that function can't do anything sensible in the context where you
;; need to call `system-cores', then `system-cores' can't either. For
;; example, if you're on Darwin and (getenv "PATH") doesn't contain
;; /usr/sbin, `process-lines' will bomb out with "Searching for
;; program: no such file or directory, system_profiler".

;;; Bugs/TODO:

;; No doubt you don't need telling that the information reported by
;; `system-cores' is only as accurate as the information your platform
;; makes available. In case you do need telling, though, you've been
;; told.

;; I don't have access to a true BSD system, but only one running
;; Darwin, which is similar but not quite the same. I've therefore
;; been unable to properly test the BSD delegate
;; (`system-cores-sysctl'). If you run BSD and want to use this code,
;; you are strongly advised to investigate your system's sysctl output
;; and modify `system-cores-sysctl' accordingly. (If different BSD
;; variants present the information differently, it may be necessary
;; to write a function which not only checks `system-type', but also
;; examines the value of `system-configuration', in order to identify
;; the variant in use.)

;;; Miscellany:

;; The canonical version of this file is hosted in my Github
;; repository [2]. If you didn't get it from there, great! I'm happy
;; to hear my humble efforts have achieved wide enough interest to
;; result in a fork hosted somewhere else. I'd be obliged if you'd
;; drop me a line to let me know about it.

;; [1]: http://stackoverflow.com/q/20666556/1713079
;; [2]: https://github.com/aaron-em/system-cores.el

(require 'cl)

(put 'system-cores-delegate-error
     'error-conditions '(error system-cores-delegate-error))

(defvar system-cores-delegate-alist
  '((gnu/linux      . system-cores-cpuinfo)
    (windows-nt     . system-cores-wmic)
    (cygwin         . system-cores-wmic)
    (darwin         . system-cores-profiler)
    (berkeley-unix  . system-cores-sysctl))
  "An alist whose cars are `system-type' values, and whose cdrs
are the corresponding function to call in order to find out how
many processors and cores a system of that type has
installed. The `system-cores' function uses this to choose which
delegate function to invoke for a given architecture.

  Delegates are expected to return an alist of the form:

  '((logical  . #)
    (physical . #))

where '#' represents a number greater than zero. A condition will
be signaled if a delegate returns a value not satisfying this
requirement.

  Currently defined delegates include:

  `system-cores-cpuinfo'

  Obtain processor and core information from /proc/cpuinfo,
typically found on Linux and its relatives.

  `system-cores-wmic'

  Obtain processor and core information via
the command-line Windows Management Instrumentation query tool,
found on systems of Windows NT derivation (Windows NT, Windows
2000, and all later versions).

  The WMIC delegate is also used when Emacs is running under
Cygwin; while Cygwin does have /proc/cpuinfo, its \"core id\"
member is absent, making it impossible to obtain the physical
processor count from that file.

  `system-cores-profiler'

  Obtain processor and core information from the output of
/usr/sbin/system_profiler, typically found on OS X (Darwin)
systems.

  `system-cores-sysctl'

  [Note well! This delegate has been tested
on a Darwin system, but not on a BSD system, and it is
consequently uncertain whether the information it returns will
be correct and complete for any BSD derivative. You are
strongly recommended to investigate your system's sysctl output
and modify the definition of `system-cores-sysctl'
accordingly!]

  Obtain processor and core information from the output of
/usr/sbin/sysctl, typically found on BSD and its
derivatives. (This includes Darwin systems, but on at least one
such system the processor counts returned by sysctl do not
agree with those from the Apple System Profiler; since the
latter may reasonably be expected, on an Apple system, to be
more accurate than the former, it is preferred by default on
that architecture.)")

(defun* system-cores (&key (logical  nil log-p)
                           (physical nil phy-p))
  "Return the number of processor cores, and the number of
physical processors, installed on the machine where Emacs is
running.

  Called without arguments, this function returns an alist of the
form:

  '((logical  . #)
    (physical . #))

where '#' represents a number greater than zero. Called with one
of the keywords :LOGICAL and :PHYSICAL, the function returns the
corresponding number. (Called with both keywords, the function
signals a WRONG-NUMBER-OF-ARGUMENTS condition.)

  Most of the actual work involved in obtaining this information
is done by one of several delegate functions, selected on the
basis of the system architecture where you are running as
identified by the value of `system-type'. For details on what
delegates are available, and which system types are supported,
see the documentation for `system-cores-delegate-alist'."
  (if (and log-p phy-p)
      (signal 'wrong-number-of-arguments '(system-cores 2)))
  (let ((delegate (cdr (assoc system-type system-cores-delegate-alist)))
        (result nil))
    (if delegate (setq result (funcall delegate))
      (signal 'system-cores-delegate-error
              (concat "No `system-cores' delegate available for a "
                      (symbol-name system-type) " system")))
    (if (or (equal (cdr (assoc 'logical  result)) 0)
            (equal (cdr (assoc 'physical result)) 0))
        (signal 'system-cores-delegate-error
                (concat "`" (symbol-name delegate) "'"
                        " failed to return valid information: "
                        (prin1-to-string result)))
      (cond
       (log-p
        (cdr (assoc 'logical  result)))
       (phy-p
        (cdr (assoc 'physical result)))
       (t
        result)))))

(defun system-cores-cpuinfo ()
  "Return the number of logical cores, and the number of
physical processors, listed in /proc/cpuinfo.

  The logical core count is obtained by counting the number of
lines in cpuinfo which begin with the key \"processor\"; the
count of physical cores is obtained by counting unique values
found in lines beginning with either \"core id\" or
\"physical id\".

  In at least cases where only one non-hyperthreading processor
is installed, no \"core id\" or \"physical id\" member is present
in the file. In this case, the count of logical processors is
also provided as the count of logical cores, and a message is
shown in the echo area.

  This function is a `system-cores' delegate."
  (let ((cpuinfo
         (map 'list #'(lambda (line) (split-string line "\\s-*\:\\s-*"))
              (process-lines "cat" "/proc/cpuinfo")))
        processors cores)
    (setq cores
          (reduce '+
                  (map 'list
                       #'(lambda (a) (if (string= "processor" (car a)) 1 0))
                       cpuinfo)))
    (setq processors
          (length (delete-dups
                   (remove-if 'null
                              (map 'list
                                   #'(lambda (a)
                                       (if (or (string= "core id" (car a))
                                               (string= "physical id" (car a)))
                                           (cadr a)
                                         nil))
                                   cpuinfo)))))
    (if (= 0 processors)
        (progn
          (message "system-cores-cpuinfo: Found no physical processor IDs; using logical core count")
          (setq processors cores)))
    `((logical  . ,cores)
      (physical . ,processors))))

(defun system-cores-wmic ()
  "Return the number of logical cores, and the number of
physical processors, listed in the output of a Windows Management
Instrumentation query.

  The logical core count is obtained from the value listed for
the key \"NumberOfCores\"; the count of physical cores is
obtained from the value listed for the key
\"NumberOfLogicalProcessors\".

  This function is a `system-cores' delegate."
  (let ((cpuinfo 
         (map 'list
              #'(lambda (s) (split-string s "="))
              (remove-if
               #'(lambda (s) (string= s ""))
               (process-lines
                "wmic" "cpu" "get" "NumberOfCores,NumberOfLogicalProcessors"
                "/format:List")))))
    `((logical .
               ,(string-to-number (cadr (assoc "NumberOfCores" cpuinfo))))
      (physical .
                ,(string-to-number (cadr (assoc "NumberOfLogicalProcessors" cpuinfo)))))))

(defun system-cores-profiler ()
  "Return the number of logical cores, and the number of
physical processors, listed in the output of the Apple System
Profiler.

  The logical core count is obtained from the value listed for
the key \"Total Number of Cores\" and will be doubled if
\"Hyper-Threading Technology: Enabled\"; the count of physical
cores is obtained from the value listed for the key \"Number of
Processors\".

  This function is a `system-cores' delegate."
  (let ((cpuinfo
         (map 'list
              #'(lambda (s) (split-string s ": "))
              (remove-if 'null
                         (map 'list #'(lambda (s)
                                        (when (string-match "^ +" s)
                                          (replace-match "" t t s)))
                              (process-lines "system_profiler"
                                             "SPHardwareDataType"))))))
    `((logical .
               ,(let ((ht (string-equal "Enabled"
                                        (car (last (assoc "Hyper-Threading Technology" cpuinfo))))))
                  (* (if ht 2 1)
                     (string-to-number (cadr (assoc "Total Number of Cores" cpuinfo))))))
      (physical .
                ,(string-to-number (cadr (assoc "Number of Processors" cpuinfo)))))))

(defun system-cores-sysctl ()
  "Return the number of logical cores, and the number of
physical processors, listed in the output of the sysctl command.

  The logical core count is obtained from the value listed for
the key \"hw.logicalcpu\"; the count of physical cores is
obtained from the value listed for the key \"hw.physicalcpu\".

  [Note well! This delegate has been tested on a Darwin system,
but not on a BSD system, and it is consequently uncertain whether
the information it returns will be correct and complete for any
BSD derivative. You are strongly recommended to investigate your
system's sysctl output and modify this function accordingly!]

  This function is a `system-cores' delegate."
  (let ((cpuinfo
         (map 'list
              #'(lambda (s) (split-string s ": " t))
              (process-lines "sysctl" "hw.physicalcpu" "hw.logicalcpu")))
  `((logical .
             ,(string-to-number (cadr (assoc "hw.logicalcpu" cpuinfo))))
    (physical .
              ,(string-to-number (cadr (assoc "hw.physicalcpu" cpuinfo))))))))

(provide 'system-cores)

;;; system-cores.el ends here
