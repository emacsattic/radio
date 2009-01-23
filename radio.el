;;; radio.el --- text file tag navigator with etags search integration

;; Copyright (C) 2007, 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: convenience, hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Radio-mode lets you add plaintext topic tags to your code.

;; http://en.wikipedia.org/wiki/Memex

;; TODO other kinds of tags
;; TODO todo tags

;;; Usage: 

;; Add something like the following to your .emacs:
;;
;; (require 'radio)
;; (add-hook 'emacs-lisp-mode-hook #'radio-mode)

(require 'cl)

;;; Defining groups of files

;; A group is a set of files that are handled together.

(defstruct radio-group 
  ;; Required slots.
  name ;; String name of group. 
  base-directory ;; Directory to scan for files.
  format ;; A keyword like :lisp, :text, :org
  include ;; Regexp or list of files to include
  ;; Optional slots
  description ;; Optional string description.
  exclude  ;; Regexp or list of files to exclude
  etags-file  ;; etags filename (relative to base-directory).
              ;; The default is TAGS.
  etags-arguments ;; List of argument strings for etags.
  ;; Other slots
  selected-p ;; When non-nil, multi-group operations apply to this group. 
  files ;; Cached list of files in this group; updated on each file scan.
        ;; See also `radio-scan-group-files'.
  )

(defun radio-match-regexp-or-list (filename regexp-or-list)
  "Return non-nil if FILENAME is either a member of the list (or
matches the regexp) REGEXP-OR-LIST."
  (if (null regexp-or-list)
      nil
      (etypecase regexp-or-list
	(string (string-match regexp-or-list filename))
	(list (member filename regexp-or-list)))))

(defun radio-filter-files (files regexp-or-list)
  "Return FILES with any matching files removed.  If the second
argument is a regular expression, remove files matching the
regexp. If it's a list, remove files matching any filename in the
list. See also `radio-match-regexp-or-list'."
  (labels ((match (filename)
	     (radio-match-regexp-or-list filename regexp-or-list)))
    (remove-if #'match files)))

(defun radio-get-group-files (group)
  "Obtain a list of all the available files in the group GROUP."
  (let ((dir (radio-group-base-directory group)))
    (labels ((expand (filename)
	       (expand-file-name filename dir)))
      (let* ((include (radio-group-include group))
	     (files (etypecase include
		      (string (directory-files dir nil include))
		      (list (mapcar #'expand include)))))
	(mapcar #'expand 
		(remove-duplicates 
		 (radio-filter-files files (radio-group-exclude group))
		 :test 'equal))))))

(defun radio-load-group-files (group)
  (setf (radio-group-files group)
	(radio-get-group-files group)))

(defun radio-create-group (&rest forms)
  (let ((group (apply #'make-radio-group forms)))
    (prog1 group 
      (radio-load-group-files group))))

;;; Groups table

(defvar *radio-groups* nil)

(defun radio-init-groups ()
  (interactive)
  (setf *radio-groups* (make-hash-table :test 'equal)))

(when (null *radio-groups*)
  (radio-init-groups))

(defun radio-add-group (&rest args)
  (let ((group (apply #'radio-create-group args)))
    (setf (gethash (radio-group-name group)
		   *radio-groups*)
	  group)))

(defun radio-delete-group (name)
  (remhash name *radio-groups*))

(defun* radio-scan-group-files (&optional (group-name (radio-choose-group)))
  (radio-load-group-files (gethash group-name *radio-groups*))
  (message "Scanned group %s for files." group-name))

;;; Selecting and deselecting groups

(defun* radio-choose-group () 
  (completing-read "Choose a group: " *radio-groups* nil :require-match))

(defun* radio-select-group (&optional (group-name (radio-choose-group)))
  (interactive)
  (let ((group (gethash group-name *radio-groups*)))
    (prog1 group 
      (setf (radio-group-selected-p group) t))))

(defun* radio-deselect-group (&optional (group-name (radio-choose-group)))
  (interactive)
  (let ((group (gethash group-name *radio-groups*)))
    (prog1 group 
      (setf (radio-group-selected-p group) nil))))

(defun radio-get-selected-groups ()
  (let (groups)
    (maphash #'(lambda (name group)
		 (declare (ignore name))
		 (when (radio-group-selected-p group)
		   (push group groups)))
	     *radio-groups*)
    groups))

;;; Printing information about groups

(defun* radio-describe-group (&optional (group-name (radio-choose-group)))
  (interactive)
  (let ((group (gethash group-name *radio-groups*)))
    (message (radio-group-description group))))

(defun radio-show-selected-groups ()
  (interactive)
  (maphash #'(lambda (name group)
	       (radio-describe-group name))
	   *radio-groups*))

;;; Getting etags and topic tags index for a group

;; The topic search for Radio uses the existing Emacs tags indexing
;; facility, and is integrated with ordinary etags search. This means
;; that Radio tags can be used and searched alongside function and
;; variable definitions with Emacs commands like M-x tags-search and
;; M-x find-tag.

(defvar radio-etags-program "etags")

(defconst radio-etags-topic-regexp (concat "/.*<" ":[ ]*\\([^ :>]*\\)[ ]*:" ">/\\1/"))

(defvar radio-default-etags-file-name "TAGS")

(defun* radio-scan-group-etags (&optional (group-name (radio-choose-group)))
  (interactive)
  (radio-scan-group-files group-name)
  (let* ((group (gethash group-name *radio-groups*))
	 (tags-file (expand-file-name (or (radio-group-etags-file group)
					  radio-default-etags-file-name)
				      (file-name-as-directory
				       (radio-group-base-directory group))))
	 (args (append (list (format "--output=%s" tags-file)
			     (format "--regex=%s" radio-etags-topic-regexp))
		       (radio-group-etags-arguments group)
		       (radio-group-files group))))
    (if (= 0 (apply #'call-process radio-etags-program nil nil nil args))
	(message "Scanned etags for group %s with %d files." 
		 group-name (length (radio-group-files group)))
	(error "Failed to scan etags for group %s." group-name))))

;;; Reading and writing topic tags in a buffer

(defconst radio-tag-regexp 
  (concat "\\(<" ":\\)[[:space:]]+\\(.*\\)[[:space:]]+\\(:" ">\\)")
  "Regular expression matching tags.")

(defun radio-format-tag-regexp (string)
  "Make a regexp to find STRING as a tag."
  (concat "<" ":" "[[:space:]]+" string "[[:space:]]+" ":" ">"))

(defun radio-read-next-tag (&optional bound)
  "Find the next tag, if any, regardless of tag content."
  (interactive)
  (save-excursion
    (when (re-search-forward radio-tag-regexp bound :noerror)
      (match-string-no-properties 2))))

(defun radio-read-previous-tag (&optional bound)
  "Find the previous tag, if any, regardless of tag content."
  (interactive)
  (save-excursion
    (when (re-search-backward radio-tag-regexp bound :noerror)
      (match-string-no-properties 2))))

(defun radio-tag-on-current-line ()
  (save-excursion
    (goto-char (point-at-bol))
    (radio-read-next-tag (point-at-eol))))

(defun radio-auto-choose-tag ()
  (interactive)
  (or (radio-tag-on-current-line)
      (radio-read-previous-tag)))

;;; Finding topic tags

(make-variable-buffer-local (defvar *radio-tags* nil))

(defun* radio-all-tags-in-buffer (&optional (buffer (current-buffer)))
  (interactive)
  (save-excursion
    (let (tags)
      (goto-char (point-min))
      (while (re-search-forward radio-tag-regexp nil :noerror)
	(push (match-string-no-properties 2) tags))
      tags)))

(defun* radio-rescan-tags (&optional (buffer (current-buffer)))
  (interactive)
  (let ((tags (radio-all-tags-in-buffer buffer)))
    (setf *radio-tags* 
	  (sort (remove-duplicates tags :test 'equal)
		#'string<))))

(defun* radio-choose-tag ()
  (interactive)
  (when (null *radio-tags*)
    (radio-rescan-tags))
  (completing-read "Choose tag: " *radio-tags*))

;;; Navigating groups of tags within the current buffer

(defun* radio-next-tag-in-buffer (&optional (tag (radio-auto-choose-tag)) noerror nowrap bound)
  "Find the next tag with name TAG in the current buffer."
  (interactive)
  (let ((search-string (radio-format-tag-regexp tag))
	(match-point nil))
    (block searching
      (when (setf match-point (re-search-forward search-string bound t))
	(return-from searching match-point))
      ;; otherwise go to beginning and look again
      (unless nowrap
	(goto-char (point-min))
	(re-search-forward search-string nil noerror)))))

(defun* radio-previous-tag-in-buffer (&optional (tag (radio-auto-choose-tag)) noerror nowrap bound)
  "Find the previous tag with name TAG in the current buffer."
  (interactive)
  (let ((search-string (radio-format-tag-regexp tag))
	(match-point nil))
    (block searching
      (when (setf match-point (re-search-backward search-string bound t))
	(return-from searching match-point))
      (unless nowrap
	(goto-char (point-max))
	(re-search-backward search-string nil noerror)))))

;; (defun radio-find-tag ()
;;   (interactive)
;;   (radio-seek-tag (radio-choose-tag)))
;; TODO TODO TODO TODO TODO TODO TODO TODO
;; TODO TODO TODO TODO TODO TODO TODO TODO
;; TODO TODO TODO TODO TODO TODO TODO TODO
;; TODO TODO TODO TODO TODO TODO TODO TODO
;; TODO TODO TODO TODO TODO TODO TODO TODO
;; TODO TODO TODO TODO TODO TODO TODO TODO
;; TODO TODO TODO TODO TODO TODO TODO TODO

;;; Navigating to the next such tag in the group

(defun* radio-seek-tag (&optional (tag (radio-auto-choose-tag)) backward)
  (when tag
    ;; TODO write this
    (recenter)))
	    
(defun* radio-next-tag (&optional (tag (radio-auto-choose-tag)))
  (interactive)
  (radio-seek-tag tag))

(defun* radio-previous-tag (&optional (tag (radio-auto-choose-tag)))
  (interactive)
  (radio-seek-tag tag :backward))

;;; Font-locking the tags

(defface radio-annotation-delimiter-face
'((t (:foreground "gold3")))
  "Face for radio tags.")

(defvar radio-annotation-delimiter-face
'radio-annotation-delimiter-face)

(defface radio-annotation-delimiter-alt-face
    '((t (:foreground "gray30")))
  "Face for radio tags.")

(defvar radio-annotation-delimiter-alt-face
  'radio-annotation-delimiter-alt-face)

(defface radio-annotation-data-face
'((t (:foreground "gray70")))
  "Face for radio tag data.")

(defvar radio-annotation-data-face
  'radio-annotation-data-face)

(defface radio-attention-face
'((t (:foreground "yellow" :background "red")))
  "Face for things that should get your attention.")

(defvar radio-attention-face
  'radio-attention-face)

(defvar radio-font-lock-keywords 
  `((,radio-tag-regexp 
     (1 radio-annotation-delimiter-face prepend)
     (2 radio-annotation-data-face prepend)
     (3 radio-annotation-delimiter-alt-face prepend))))

(defun radio-do-font-lock (add-or-remove)
  (dolist (keyword radio-font-lock-keywords)
    (apply add-or-remove (list nil (list keyword)))))

(defun radio-enable ()
  (radio-do-font-lock 'font-lock-add-keywords)
  (font-lock-fontify-buffer))

(defun radio-disable ()
  (radio-do-font-lock 'font-lock-remove-keywords)
  (font-lock-fontify-buffer))

;;; Minor mode 

(defvar radio-keymap nil)
(when (null radio-keymap)
  (setq radio-keymap (make-sparse-keymap))
  (define-key radio-keymap (kbd "C-c , n") 'radio-next-tag)
  (define-key radio-keymap (kbd "C-c , p") 'radio-previous-tag)
  (define-key radio-keymap (kbd "C-c , c") 'radio-find-tag))
  ;; (define-key radio-keymap (kbd "C-c , t") 'radio-show-todo-list)
  ;; (define-key radio-keymap (kbd "C-c , b") 'radio-edit-binary-annotation))

(define-minor-mode radio-mode
  "Tag lines of a file with Org entries kept in another file."
  nil 				
  :lighter " Radio"
  :keymap radio-keymap
  (if radio-mode
      (radio-enable)
    (radio-disable)))

;; (add-hook 'emacs-lisp-mode-hook #'radio-enable)

;; <: foo :> 
;; <: bar :>
;; <: baz :>

(provide 'radio)
;;; radio.el ends here

