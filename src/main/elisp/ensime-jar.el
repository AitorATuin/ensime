;;; ensime-jar.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aitor P. Iturri
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

;; Elisp code to manage source files inside jar
(require 'dired-x)

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
     
(provide 'ensime-jar)
