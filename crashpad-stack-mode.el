;; Major mode to fontity Crashpad call stack files produced via minidump_stackwalk.

(defvar crashpad-stack-highlights nil "Highlights of crashpad-stack-mode.")

;; TODO: maybe define custom faces to use in this major mode?
;; (defface crashpad-delimiter-face
;;   '((t
;;      (:weight bold :foreground "#4CB5F5")))
;;   "Face for delimiters."
;;   :group 'crashpad-stack-faces)

;; Ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Backslash.html

;; Example file that it can fontify:
;;   Thread 136
;;    0  win32u.dll + 0x9a84
;;       rax = 0x0000000000001452   rdx = 0x0000000000000000
;;       rcx = 0x0000000000000000   rbx = 0x0000000000000000
;;       rsi = 0x000001ce51d17101   rdi = 0x0000000000000024
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077fc498
;;        r8 = 0x000001ce65a734d0    r9 = 0x000000001174ca91
;;       r10 = 0x00000000000005c0   r11 = 0x0000002e077fa370
;;       r12 = 0x0000000000000000   r13 = 0x0000000000000000
;;       r14 = 0x000001ce2939fb60   r15 = 0x000001ce51d17080
;;       rip = 0x00007ffb73ce9a84
;;       Found by: given as instruction pointer in context
;;    1  USER32.dll + 0x22a3d
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077fc4a0
;;       rip = 0x00007ffb75782a3d
;;       Found by: stack scanning
;;    2  Qt5Core.dll!QEventDispatcherWin32::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) [qeventdispatcher_win.cpp : 656 + 0x20]
;;
;;      654          if (canWait) {
;;      655              emit aboutToBlock();
;;     >656              waitRet = MsgWaitForMultipleObjectsEx(nCount, pHandles, INFINITE, QS_ALLINPUT, MWMO_ALERTABLE | MWMO_INPUTAVAILABLE);
;;      657              emit awake();
;;      658              if (waitRet - WAIT_OBJECT_0 < nCount) {
;;
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077fc4e0
;;       rip = 0x00007ffb5acc1696
;;       Found by: stack scanning
;;    3  Qt5Core.dll!QEventLoop::exec(QFlags<QEventLoop::ProcessEventsFlag>) [qeventloop.cpp : 225 + 0x24]
;;
;;      223
;;      224      while (!d->exit.loadAcquire())
;;     >225          processEvents(flags | WaitForMoreEvents | EventLoopExec);
;;      226
;;      227      ref.exceptionCaught = false;
;;
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077ff650
;;       rip = 0x00007ffb5ac73b7b
;;       Found by: call frame info
;;    4  Qt5Core.dll!QThread::exec() [qthread.cpp : 531 + 0xc]
;;
;;      529
;;      530      QEventLoop eventLoop;
;;     >531      int returnCode = eventLoop.exec();
;;      532
;;      533      locker.relock();
;;
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077ff6d0
;;       rip = 0x00007ffb5aadc0b2
;;       Found by: call frame info
;;    5  Qt5Core.dll!QThreadPrivate::start(void *) [qthread_win.cpp : 403 + 0x9]
;;
;;      401      emit thr->started(QThread::QPrivateSignal());
;;      402      QThread::setTerminationEnabled(true);
;;     >403      thr->run();
;;      404
;;      405      finish(arg);
;;
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077ff720
;;       rip = 0x00007ffb5aade9c9
;;       Found by: call frame info
;;    6  KERNEL32.DLL + 0x17bd4
;;       rbp = 0x0000002e077fc5e0   rsp = 0x0000002e077ff760
;;       rip = 0x00007ffb74b67bd4
;;       Found by: call frame info
;;    7  ntdll.dll + 0x6cee1
;;       rsp = 0x0000002e077ff790   rip = 0x00007ffb76b0cee1
;;       Found by: stack scanning

;; It will take the first regexp match so the order matters.
(setq crashpad-stack-highlights
      '(;; Threads, like
        ;; Thread 0 (crashed)
        ;; Thread 49
        ("Thread [0-9]+ (crashed)" . compilation-error-face)
        ("Thread [0-9]+" . compilation-warning-face)

        ;; Frame numbers, like
        ;;  0  Qt5Gui.dll!..
        ;; 10  keyshot + ..
        ;;  2  0x7ffb76aa0000 ..
        ;;  3  libcrypto-1_1-x64.dll..
        ("\\( \\)?[0-9]+  0x[0-9a-f]+" . compilation-line-face)
        ("\\( \\)?[0-9]+  \\w+\\(\\.*-*_*\\w*[0-9]*\\)*\\(!\\| \\+\\)" . compilation-line-face)

        ;; Frame source files and lines, like "qapplication.cpp : 543".
        ("\\([_\\.]?\\w[_\\.]?\\)+ : [0-9]+" . font-lock-constant-face)

        ;; Source code block middle line marker, like "  >42".
        ("  >[0-9]+" . compilation-line-face)

        ;; Code block line numbers.
        ("   [0-9]+  " . font-lock-constant-face)

        ;; Main loaded module, like
        ;; 0x7ff63c5e0000 - 0x7ff63e191fff  keyshot.exe  9.0.244.0  (main)
        ("\\([-\\._]*\\w[-\\._]*\\)+\\( \\)+[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\( \\)+(main)"
         . compilation-warning-face)

        ;; Comments in source blocks.
        ("//.+" . font-lock-comment-face)
        ("/\\*.*\\*/" . font-lock-comment-face)

        ;; Registers, like "rax", "r10" etc.
        ("\\b\\(rax\\|rdx\\|rcx\\|rbx\\|rsi\\|rdi\\|rbp\\|rsp\\|rip\\)\\b" . font-lock-builtin-face)
        ("\\b\\(r8\\|r9\\|r10\\|r11\\|r12\\|r13\\|r14\\|r15\\)\\b" . font-lock-builtin-face)

        ;; C++ preproc, like #define, #ifdef, #include etc.
        ("#\\w+" . font-lock-constant-face)

        ;; C++ keywords.
        ("\\b\\(for\\|while\\|if\\|else\\|do\\|switch\\|case\\|continue\\|break\\)\\b" ;; Flow
         . font-lock-keyword-face)
        ("\\b\\(struct\\|class\\|enum\\)\\b" . font-lock-keyword-face) ;; Class struct.
        ("\\b\\(public\\|private\\|protected\\|static\\|volatile\\)\\b" ;; Access.
         . font-lock-keyword-face)
        ("\\b\\(return\\|const\\|operator\\|throw\\|noexcept\\)\\b" ;; Misc.
         . font-lock-keyword-face)
        ("\\b\\(emit\\|slots\\)\\b" . font-lock-keyword-face) ;; Qt

        ;; C++ attributes, like [[nodiscard]]
        ("\\[\\[\\([-_]*\\w+[-_]*\\)+\\]\\]" . font-lock-keyword-face)

        ;; C++ types.
        ("\\b\\(int\\|long\\|short\\|char\\|float\\|double\\|wchar_t\\|unsigned\\|void\\|bool\\)\\b"
         . font-lock-type-face)
        ("__int[0-9]+" . font-lock-type-face) ;; __int(32|64..)
        ("HWND__" . font-lock-type-face)

        ;; KeyShot types.
        ("\\b\\(K\\([-_]*\\w+[-_]*\\)+\\)\\b" . font-lock-type-face)
        ("\\b\\(lux\\([-_]*\\w+[-_]*\\)+\\)\\b" . font-lock-type-face)

        ;; std[::types]+ types.
        ("\\b\\(std\\(::\\([-_]*\\w+[-_]*\\)\\)+\\)\\b" . font-lock-type-face)

        ;; Qt types.
        ("\\b\\(Q\\([-_]*\\w+[-_]*\\)+\\)\\b" . font-lock-type-face) ;; QPoint, Qt..
        ("qu?\\(int\\|short\\|long\\)[0-9]*" . font-lock-type-face) ;; quint64, qlong..

        ;; Booleans.
        ("\\b\\(true\\|false\\)\\b" . font-lock-constant-face)

        ;; Strings.
        ("\\(\\\"\\|'\\).+\\(\\\"\\|'\\)" . font-lock-string-face)

        ;; Delimiters.
        ("->\\|\\*\\|\\+\\|\\!\\|=\\|&\\||\\|,\\|:\\|;\\|-\\|(\\|)\\|\\[\\|\\]\\|{\\|}\\|\\.\\|<\\|>"
         . font-lock-string-face)

        ;; Numbers.
        ("\\b\\(-\\)?[0-9.]+\\b" . font-lock-constant-face)
        ))

(define-derived-mode crashpad-stack-mode text-mode "crashpad-stack"
  "major mode for viewing crashpad/breakpad call stack files."
  (setq font-lock-defaults '(crashpad-stack-highlights)))

