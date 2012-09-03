;; Jar Tool
;; Elisp code to manage source files inside jar
(require 'dired-x)

;; mapcar-not-nil
(defun rec-mapcar-not-nil (f ol fl)
  (if (equal ol nil)
      (eval 'fl)
    (let ((first (car ol))
	  (rest (cdr ol)))
      (if (not (equal first nil))
	  (rec-mapcar-not-nil f
			      rest
			      (cons (funcall f first) fl))
	(rec-mapcar-not-nil f rest fl)))))
(defun mapcar-not-nil (f ol)
  "Applies map to each not nil element, ignore nils"
  (rec-mapcar-not-nil f ol '()))
;; TEST mapcar-not-nil
;; (mapcar-not-nil (lambda(x) (* 2 x)) '(nil nil 1 2 3 nil nil))
;; (mapcar-not-nil (lambda(x) (* 2 x)) '(1 2 3))
;; (mapcar-not-nil (lambda(c) (string c)) '(23 nil 56 nil 45))

;; list-to-string
(defun rec-list-to-string (l s)
  (if (equal l nil)
      (eval 's)
    (rec-list-to-string (cdr l) (concat s (string (car l))))))
(defun list-to-string (l)
  "Given a list of int representing chars, converts them into a string"
  (rec-list-to-string l ""))
;; TEST list-to-string
;;(list-to-string (string-to-list "pepaddd ddd"))

;; strim-blanks
(defun blank-to-nil (c)
  (case c
    (10 nil)
    (32 nil)
    (9 nil)
    (t c)))
(defun strim-blanks (string)
  "Removes each blank (\s\t\n) from a string"
  (let ((l (mapcar-not-nil (lambda(c) (eval 'c))
			   (mapcar 'blank-to-nil 
				   (string-to-list string)))))
    (list-to-string (reverse l))))
;; TEST strim-blanks
;; (strim-blanks "uno\t\tdos")

;; cmd-result
(defun cmd-result (cmd &rest args)
  "Executes a command and returns the string returned by it"
  (eshell-command-result (mapconcat 'string-to-list (cons cmd args) " ")))
;; TEST cmd-result
;; (cmd-result "ls" "dos" "tres" "cuatr")

;; jar-psource-from-class
(defun jar-class-source-path (cname jcfile jsfile)
  (let ((output (cmd-result "~/bin/javas"
			   jcfile
			   jsfile
			   cname)))
    (strim-blanks output)))
;; TEST jar-source-from-class
;; (jar-class-source-path "Command"
;; 		       "/home/eof/.ivy2/cache/com.github.m20o/scalatra-toys_2.9.1/jars/scalatra-toys_2.9.1-0.1.1-SNAPSHOT.jar"
;;		       "/home/eof/.ivy2/cache/com.github.m20o/scalatra-toys_2.9.1/srcs/scalatra-toys_2.9.1-0.1.1-SNAPSHOT-sources.jar")

;; jar-class-source-view
(defun jar-class-source-view (cname cjar sjar)
  (let ((cpath (jar-class-source-path cname cjar sjar)))
    (and
     (dired-x-find-file sjar)
     (search-forward cpath)
     (let ((cbuf (current-buffer)))
       (archive-view)
       (kill-buffer cbuf)
       (search-forward cname)
       ))))
;;(jar-class-source-view "Command" 
;;		       "/home/eof/.ivy2/cache/com.github.m20o/scalatra-toys_2.9.1/jars/scalatra-toys_2.9.1-0.1.1-SNAPSHOT.jar"
;;		       "/home/eof/.ivy2/cache/com.github.m20o/scalatra-toys_2.9.1/srcs/scalatra-toys_2.9.1-0.1.1-SNAPSHOT-sources.jar")
  
(defun ensime-jar-view-file (jarfile classname)
  (progn
    (dired-x-find-file jarfile)
    (search-forward classname)
    (let ((cbuf (current-buffer)))
      (archive-view)
      (kill-buffer cbuf)
      (goto-char 0))))
	  

(defun ensime-jar-pos-jar-file (pos)
  (plist-get pos :jarFile))

(defun ensime-jar-pos-source-file (pos)
  (plist-get pos :sourceFile))

(defun ensime-jar-decl-pattern (name type)
  (case type
    ('class (format "class %s" name))
    ('trait (format "trait %s" name))
    (otherwise (format "%s" name))))

;; ATuin
(defun ensime-jar-pos-valid-local-p (pos)
 (and (stringp (ensime-jar-pos-jar-file pos))
      (stringp (ensime-jar-pos-source-file pos))
      (file-exists-p (ensime-jar-pos-jar-file pos))))

(defun ensime-jar-goto-source-location (pos name decl-as &optional where)
  "Move to the source location POS. Don't open
 a new window or buffer if file is open and visible already.
 Just like ensime-goto-source-location but works for jar files.
 Requires dired-x"
  (let* ((jar (ensime-jar-pos-jar-file pos))
	 (file (ensime-jar-pos-source-file pos))
	 (jarfile (mapconcat (lambda(s) s) (list jar file) ":"))
	 (decl-pattern (ensime-jar-decl-pattern name decl-as))
	 (file-visible-window 
	  (ensime-window-showing-file jarfile)))

    ;; TODO: Add support for ensime-jar-view-file to open in a window
    ;; Add support for ensime-jar-view-file to check if file isnt contained in jar
    (when (not file-visible-window)
      (ecase where
	((nil)
	 (ensime-jar-view-file jar file))
	(window
	 (ensime-jar-view-file jar file))) ;; TODO: Add support to open in a window

      (setq file-visible-window
	    (ensime-window-showing-file jarfile)))
    (with-current-buffer (window-buffer file-visible-window)
      (search-forward decl-pattern))))
     
;;archive-files <- Contiene un vector con los fd
;; ensime-rpc-repl-config

(provide 'ensime-jar)
