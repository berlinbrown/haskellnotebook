;;;--------------------------------------------------------------------
;;; Emacs mode configuration
;;;
;;; Updated for Win32
;;; 7/27/2009
;;;
;;; Note: you may need to included the
;;; module and libraries below.  Most are programming
;;; language modes.
;;;--------------------------------------------------------------------

;;******************
;; Emacs config:
;; Berlin Brown - 7/27/2009
;; Emacs version = GNU Emacs 23.1.1 (i386-mingw-nt5.1.2600)
;;                     of 2009-07-30 on SOFT-MJASON
;;
;; For Win32, one might launch from a VB script:
;; ---------------
;; @echo off
;; cscript "F:\downloads\emacs-23.1-bin-i386\emacs-23.1\bin\vb_emacs_start.vbs" %1 %2 %3 %4
;; exit
;;--------------------------------------------------------------------
;; Vbscript, win32:
;; for use with emacsclient and server
;; see: http://www.emacswiki.org/emacs/EmacsClient
;;--------------------------------------------------------------------
;;Set objShell = WScript.CreateObject("WScript.Shell")
;;Set fso = CreateObject("Scripting.FileSystemObject")
;;strComputer = "."
;;Set objWMIService = GetObject("winmgmts:\\" & strComputer & "\root\cimv2")
;;Set colItems = objWMIService.ExecQuery("Select * From Win32_Process")
;;Dim isRunning
;;isRunning = False  
;;For Each objItem in colItems
;;    If InStr(objItem.CommandLine, "emacs.exe") Then
;;      isRunning = True
;;    End If
;;Next
;;If WScript.Arguments.Count = 1 Then  
;;  If isRunning Then
;;    objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\emacsclientw.exe"" -n " & """" & WScript.Arguments(0) & """")
;;  Else
;;    objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\runemacs.exe"" " & """" & WScript.Arguments(0) & """")
;;  End If
;;Else
;;  ' ***** Print the Standard Out, the Usage ***********
;;  Wscript.StdOut.WriteLine "Running emacs startup (no arguments)"
;;  If isRunning Then
;;      objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\emacsclientw.exe"" -n -c")
;;  Else
;;      objShell.Run("""" & fso.GetParentFolderName(WScript.ScriptFullName) & "\runemacs.exe"" ")
;;  End If
;;End If
;;--------------------------------------------------------------------
;; End of External VB script, ignore if not using
;;--------------------------------------------------------------------
;;--------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;;------------------
;; Enable columns
;;------------------
(column-number-mode 1)

(setq-default truncate-lines 1)
;;(setq truncate-partial-width-windows nil)
;;(setq overflow-newline-into-fringe 1)

;;------------------
;; Key Bindings
;;------------------
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key "\C-c\C-c" "\C-q\t")
(global-set-key "\C-c\C-a" 'clipboard-kill-ring-save)
(global-set-key "\C-c\C-v" 'clipboard-yank)

(global-set-key [f4] 'shell)
(global-set-key [f5] "\C-x\h")
(global-set-key [f6] 'clipboard-kill-ring-save)
(global-set-key [f7] 'clipboard-yank)
(global-set-key [f8] 'htmlize-buffer)
(global-set-key [f9] (lambda () (interactive)			
                       (comint-previous-input 1)))

;;; Description:
;;;     So, to select-all and copy, do 'C-x h' 'C-c C-a'

(global-set-key "\C-z" 'advertised-undo)

;;******************
;;disable backup and autosave
;;******************
(setq backup-inhibited t)
(setq auto-save-default nil)

;;*****************************
;; haskell mode
;; Added 11/10/2007
;;*****************************
;;(load "~/lib/emacs/haskell-mode-2.3/haskell-site-file")
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;*****************
;; Enable scala mode
;;*****************
;;(add-to-list 'load-path "~/lib/emacs/scala")
;;(require 'scala-mode-auto)

;;******************
;; Ruby Mode
;;*****************
;;(load-file "~/lib/emacs/ruby-mode.el")
;;(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
;;(setq auto-mode-alist (cons '(".rb$" . ruby-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '(".rhtml$" . html-mode) auto-mode-alist))

;;******************
;; Python Mode
;;******************
;;(add-to-list 'load-path "~/lib/emacs/python")
;;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;(setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                       interpreter-mode-alist))
;;(autoload 'python-mode "python-mode" "Python editing mode." t)

;;(load-file "~/lib/emacs/htmlize.el")

;;******************
;; Clojure Emacs Mode
;;******************
;;(load-file "~/lib/emacs/clojure-mode.el")
;;(setq inferior-lisp-program
;;                                        ; Path to java implementation
;;      (let* ((java-path "java")
;;                                        ; Extra command-line options
;;                                        ; to java.
;;             (java-options "")
;;                                        ; Base directory to Clojure.
;;                                        ; Change this accordingly.
;;             (clojure-path "/cygdrive/c/projects/tools/home/projects/projects_ecl/botclient/botnetclient/clojure/")
;;                                        ; The character between
;;                                        ; elements of your classpath.
;;             (class-path-delimiter ":")
;;             (class-path (mapconcat (lambda (s) s)
;;                                        ; Add other paths to this list
;;                                        ; if you want to have other
;;                                        ; things in your classpath.
;;                                    (list (concat clojure-path "target/clojure.jar"))
;;                                    class-path-delimiter)))
;;        (concat java-path
;;                " " java-options
;;                " -cp " class-path
;;                " clojure.lang.Repl")))
;;
;; Require clojure-mode to load and associate it to all .clj files.
;;(require 'clojure-mode)
;;(setq auto-mode-alist
;;      (cons '("\\.clj$" . clojure-mode)
;;            auto-mode-alist))

;; These are extra key defines because I kept typing them.  
;; Within clojure-mode, have Ctrl-x Ctrl-e evaluate the last 
;; expression.
;; Ctrl-c Ctrl-e is also there, because I kept typoing it.
;;(add-hook 'clojure-mode-hook
;;          '(lambda ()
;;             (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)
;;             (define-key clojure-mode-map "\C-x\C-e" 'lisp-eval-last-sexp)))

;;******************
;; Set Tab for 4 spaces of indendation
;; Update: 5/5/08, replace all tabs with spaces.
;; Add file specific settings for Makefiles or use Ctrl-q
;;******************
(setq c-basic-offset 4)
(setq default-tab-width 4)

(setq-default indent-tabs-mode nil)

;; Launch the emacs server (newer version not gnuserver)
(cd "~/")
(server-start)

;; End of File
