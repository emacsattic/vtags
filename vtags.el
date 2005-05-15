;;; vtags.el --- tags facility for Emacs

;; Copyright (C) 1994-2005 Edward Bishop

;; Keywords: tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;;;;;;;;;;;;;;
;; Usage:
;;;;;;;;;;;;;;
;;
;; 1) Create a tags file e.g. : % ctags -R .
;; You can get ctags from  http://ctags.sourceforge.net if you
;; do not already have it.

;; 2) Add the following to your startup file (e.g. .emacs)
;; with the []'s filled in with some whatever function keys 
;  you chose and with path set to point to your tags file:

;        (load "/path/to/vtags")
;        (vtags-set-tagfile "/path/to/my/tags")
;        (global-set-key [f5] 'vtags-find);        
;        (global-set-key [f6] 'vtags-prev-placeholder)
;        (global-set-key [f7] 'vtags-goto-current-placeholder)
;        (global-set-key [f8] 'vtags-next-placeholder)
;        (global-set-key [f9] 'vtags-point-to-placeholder)
;        (global-set-key [f10] 'vtags-reset-placeholders)

;; 3) Try vtags-find TAGNAME. If TAGNAME is a null string, 
;; the expression in the buffer around or before point is used as the tag name.
;; If there is more than one match, then you will find yourself
;; in the *Vtags-Buffer*. There you can select the tag entry that
;; you want using <RET>, f, or button-2. That is vtags-mode.
;; There is also a *Vtag-History* buffer with the same mode.

;; This code has been tested with Emacs 21-4, XEmacs 21.4, and 
;; Exuberant Ctags 5.2.2 but should work with almost any version of 
;; Emacs or ctags.
;;
;;;;;;;;;;;;;;;;;;;;;;
;; Troubleshooting:
;;;;;;;;;;;;;;;;;;;;;;
;;
;; If your tag file has very long lines (>512) then you 
;; should increase chunk-size.



;;;;;;;;;;;;;;;;;;;;;;
;; Todo
;;;;;;;;;;;;;;;;;;;;;;
;;
;; There are some esoteric features in etags that I don't plan
;; to implement unless I hear that people actually
;; use them. Example: nested TAGS files. What are they?
;; 
;; Anyone who wants a legacy feature to be carried forward
;; can help by providing test cases to illustrate how the
;; feature should work.


;;; Code:

(require 'ring)

(eval-when-compile
  (require 'cl))

;(require 'button)

;;;###autoload
(defvar vtags-file-name "~/tags"  
  "*File name of tags table.
To switch to a new tags table, setting this variable is sufficient.
If you set this variable, do not also set `vtags-table-list'.
Use the `tags' program to make a tags table file.")
;; Make M-x set-variable tags-file-name like M-x visit-tags-table.
;;;###autoload (put 'tags-file-name 'variable-interactive "fVisit tags table: ")

(defgroup vtags nil "Tags tables"
  :group 'tools)

(defcustom vtags-case-fold-search 'nil
  "*Whether TAGS operations should be case-sensitive.
A value of t means case-insensitive, a value of nil means case-sensitive.
Any other value means use the setting of `case-fold-search'.
Note: this only applies to unsorted TAGS files. 
If a tags file is sorted then the case-sensitivity
of the sort cannot be overridden."
  :group 'vtags
  :type '(choice (const :tag "Case-sensitive" nil)
		 (const :tag "Case-insensitive" t)
		 (other :tag "Use default" default))
  :version "21.1")

;;;###autoload
;; Use `visit-tags-table-buffer' to cycle through tags tables in this list.
(defcustom vtags-table-list nil
  "*List of file names of tags tables to search.
An element that is a directory means the file \"TAGS\" in that directory.
To switch to a new list of tags tables, setting this variable is sufficient.
If you set this variable, do not also set `vtags-file-name'.
Use the `ctags' program to make a tags table file."
  :group 'vtags
  :type '(repeat file))

;;;###autoload
(defcustom vtags-compression-info-list '("" ".Z" ".bz2" ".gz" ".tgz")
  "*List of extensions tried by vtags when jka-compr is used.
An empty string means search the non-compressed file.
These extensions will be tried only if jka-compr was activated
\(i.e. via customize of `auto-compression-mode' or by calling the function
`auto-compression-mode')."
  :type  '(repeat string)
  :group 'vtags)

;; !!! vtags-compression-info-list should probably be replaced by access
;; to directory list and matching jka-compr-compression-info-list. Currently,
;; this implementation forces each modification of
;; jka-compr-compression-info-list to be reflected in this var.
;; An alternative could be to say that introducing a special
;; element in this list (e.g. t) means : try at this point
;; using directory listing and regexp matching using
;; jka-compr-compression-info-list.


;;;###autoload
(defcustom vtags-add-tables 'ask-user
  "*Control whether to add a new tags table to the current list.
t means do; nil means don't (always start a new list).
Any other value means ask the user whether to add a new tags table
to the current list (as opposed to starting a new list)."
  :group 'vtags
  :type '(choice (const :tag "Do" t)
		 (const :tag "Don't" nil)
		 (other :tag "Ask" ask-user)))

(defcustom vtags-revert-without-query nil
  "*Non-nil means reread a TAGS table without querying, if it has changed."
  :group 'vtags
  :type 'boolean)

(defun vtags-table-computed-list ()
  "List of tags tables to search, computed from `vtags-table-list'.
Note: this is a function. Do not confuse it with the
etags variable tags-table-computed-list."
  (interactive)
  (mapcar 'vtags-get-tagfileinfo vtags-table-list))
      
(defun vtags-get-tagfileinfo (tagfilename)
  "Return tagfileinfo corresponding to tagfilename.
Parse the file header if necessary. Cache the info for later use.
TODO: when, if ever, should the cache be flushed?"
  (interactive)
  (let ((table-list nil)
	(tfi nil))
    (defvar tagfileinfolist nil) ; Local variable where info is cached.
    (setq table-list tagfileinfolist)
    ;; Is tagfilename already in tagfileinfolist ?
    (while table-list
	(setq tfi (car table-list))
	(setq table-list (cdr table-list))
	(if (equal tagfilename (tagfileinfo-file tfi))
	    (setq table-list nil)
	  (setq tfi nil)))
    ;; If not, parse the file header and add info to list
    (unless tfi 
      (setq tfi (vtags-get-tagfile-header tagfilename))
      (setq tagfileinfolist (cons tfi tagfileinfolist)))
    tfi))
      
(global-set-key [(shift f9)] (lambda () (interactive) (princ (vtags-table-computed-list))))
      
;;;###autoload
(defcustom vtags-find-tag-hook nil
  "*Hook to be run by \\[find-tag] after finding a tag.  See `run-hooks'.
The value in the buffer in which \\[find-tag] is done is used,
not the value in the buffer \\[find-tag] goes to."
  :group 'vtags
  :type 'hook)

;;;###autoload
(defcustom vtags-find-tag-default-function nil
  "*A function of no arguments used by \\[find-tag] to pick a default tag.
If nil, and the symbol that is the value of `major-mode'
has a `find-tag-default-function' property (see `put'), that is used.
Otherwise, `find-tag-default' is used."
  :group 'vtags
  :type '(choice (const nil) function))

(defcustom vtags-find-tag-marker-ring-length 64
  "*Length of marker rings `vtags-find-tag-marker-ring' and `vtags-location-ring'."
  :group 'vtags
  :type 'integer
  :version "20.3")

(defcustom vtags-tag-face 'default
  "*Face for tags in the output of `tags-apropos'."
  :group 'vtags
  :type 'face
  :version "21.1")

(defcustom vtags-apropos-verbose nil
  "If non-nil, print the name of the tags file in the *Tags List* buffer."
  :group 'vtags
  :type 'boolean
  :version "21.1")

(defcustom vtags-apropos-additional-actions nil
  "Specify additional actions for `tags-apropos'.

If non-nil, value should be a list of triples (TITLE FUNCTION
TO-SEARCH).  For each triple, `tags-apropos' processes TO-SEARCH and
lists tags from it.  TO-SEARCH should be an alist, obarray, or symbol.
If it is a symbol, the symbol's value is used.
TITLE, a string, is a title used to label the additional list of tags.
FUNCTION is a function to call when a symbol is selected in the
*Tags List* buffer.  It will be called with one argument SYMBOL which
is the symbol being selected.

Example value:

  '((\"Emacs Lisp\" Info-goto-emacs-command-node obarray)
    (\"Common Lisp\" common-lisp-hyperspec common-lisp-hyperspec-obarray)
    (\"SCWM\" scwm-documentation scwm-obarray))"
  :group 'vtags
  :type '(repeat (list (string :tag "Title")
		       function
		       (sexp :tag "Tags to search")))
  :version "21.1")

(defvar vtags-find-tag-marker-ring (make-ring vtags-find-tag-marker-ring-length)
  "Ring of markers which are locations from which \\[find-tag] was invoked.")

(defvar vtags-default-tags-table-function nil
  "If non-nil, a function to choose a default tags file for a buffer.
This function receives no arguments and should return the default
tags table file to use for the current buffer.")

(defvar vtags-location-ring (make-ring vtags-find-tag-marker-ring-length)
  "Ring of markers which are locations visited by \\[find-tag].
Pop back to the last location with \\[negative-argument] \\[find-tag].")

;; Tags table state.
;; These variables are local in tags table buffers.

(defvar vtags-table-files nil
  "List of file names covered by current tags table.
nil means it has not yet been computed; use `tags-table-files' to do so.")

(defvar vtags-completion-table nil
  "Obarray of tag names defined in current tags table.")

(defvar vtags-included-tables nil
  "List of tags tables included by the current tags table.")

(defvar vtags-next-file-list nil
  "List of files for \\[next-file] to process.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vtags-toggle-casefold ()
  "*Non-nil if searches should ignore case.
Note: case folding must
match that used in tags file generation."
  (interactive)
  (setq vtags-case-fold-search (not vtags-case-fold-search))
  (message "case folding is %s" (if vtags-case-fold-search "ON" "OFF")))

(defsubst vtags-insert-chunk (default-chunk-size)
  "Grab a chunk of the file. 
The chunk has to be
big enough that we are sure to get at least one complete line.
That means that the chunk must contain at least two newline characters."
  (let ((chunk-size default-chunk-size))
    (while (eq 0 (buffer-size))
      (insert-file-contents-literally 
       file nil beg (+ beg chunk-size))
      
      (unless (eq 0 (forward-line 2))
        (progn ;(beep)
          (warn "tag line length is greater than max %d. Fix your tag file or increase chunk-size in vtags.el" 
                chunk-size)
          (when (< chunk-size 16384)
            ;; try a bigger chunk
            (erase-buffer)
            (setq chunk-size (* 2 chunk-size)))))))
  (beginning-of-buffer))

;;; Example of tags header:
;;
;; !_TAG_FILE_FORMAT	2	/extended format; --format=1 will not append ;" to lines/
;; !_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted, 2=foldcase/
;; !_TAG_PROGRAM_AUTHOR	Darren Hiebert	/dhiebert@users.sourceforge.net/
;; !_TAG_PROGRAM_NAME	Exuberant Ctags	//
;; !_TAG_PROGRAM_URL	http://ctags.sourceforge.net	/official site/
;; !_TAG_PROGRAM_VERSION	5.5	//

(defstruct (tagfileinfo (:type list) :named) file size format sorted program-name program-url program-version)

(global-set-key [f9] (lambda () (interactive) (vtags-get-tagfile-header "/home/eeb/sandbox/test.tags")))

(defun vtags-get-tagfile-header (tagfilename)
  "Some tag files have headers. If this one does, then parse the header, 
put into tagfileinfo structure. If it doesn't have a header then
return default tagfileinfo."
  (interactive (list (read-file-name "File: " "/home/eeb/sandbox")))
  (let ((tfi nil)
        ;;(header-buf (get-buffer-create (concat (file-name-nondirectory tagfilename) "-header")))
        (header-buf (get-buffer-create "*Vtags-tagfile-header*"))
        (attr (file-attributes tagfilename)))

    ;; Create a tagfileinfo
    (setf tfi (make-tagfileinfo 
	       :file tagfilename 
	       :size (nth 7 attr)
	       :format ""
	       :sorted 0
	       :program-name "etags"
	       :program-url  "http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/etags.html"
	       :program-version "GNU Emacs 21.3"))

    (save-current-buffer
      (set-buffer header-buf)
      (erase-buffer)
      (insert-file-contents-literally tagfilename nil 0 1024)

      (beginning-of-buffer)
      (search-forward-regexp "!_TAG_FILE_FORMAT[ \t\n]*\\([0-9]+\\)")
      (setf (tagfileinfo-format tfi)  (match-string 1))
      (message "format is %s" (tagfileinfo-format tfi))

      (beginning-of-buffer)
      (search-forward-regexp "!_TAG_FILE_SORTED[ \t\n]*\\([0-9]+\\)")
      (setf (tagfileinfo-sorted tfi)  (match-string 1))
      (message "sorted is %s" (tagfileinfo-sorted tfi))


      (beginning-of-buffer)
      (search-forward-regexp "!_TAG_PROGRAM_NAME[ \t\n]*\\([^\n]+\\)")
      (setf (tagfileinfo-program-name tfi)  (match-string 1))
      (message "program name is %s" (tagfileinfo-program-name tfi))


      (beginning-of-buffer)
      (search-forward-regexp "!_TAG_PROGRAM_URL[ \t\n]*\\([^ \t\n]+\\)")
      (setf (tagfileinfo-program-url tfi)  (match-string 1))
      (message "program url is %s" (tagfileinfo-program-url tfi))


      (beginning-of-buffer)
      (search-forward-regexp "!_TAG_PROGRAM_VERSION[ \t\n]*\\([0-9\\.]+\\)")
      (setf (tagfileinfo-program-version tfi)  (match-string 1))
      (message "version is %s" (tagfileinfo-program-version tfi))

      tfi
      )))


;; vtags-look
(defun vtags-look  (tag tfi output-buffer-name) 
  "Like unix look command. Does a binary search on file looking for tag."
  (interactive 
   (list 
    (completing-read "Tag: " nil) 
    (vtags-get-tagfileinfo (completing-read "Tagfile : " nil ))
    (completing-read "Output Buffer : " nil )))

  (if (equal "0" (tagfileinfo-sorted tfi))
      (error "vtags-look is being called on unsorted file %s" (tagfileinfo-file tfi))
    (let ((vtags-look-buf (get-buffer-create "*Vtags-Look-Buffer*")) ; scratch buffer
	  (output-buf (get-buffer-create output-buffer-name))
	  (blksize 4096) ; big enough to hold all matching entries
	  (default-chunk-size 1024) ; twice the length of longest line
	  (max-block 0)
	  (min-block 0)
	  (mid 0)
	  (beg 0)
	  (done nil)
	  (tag-length (length tag))
	  (size (tagfileinfo-size tfi))
	  (file (tagfileinfo-file tfi))
	  (folding (equal "2" (tagfileinfo-sorted tfi)))
	  tmp-string tag-line)
      (setq max-block (truncate (/ size blksize)))
      (set-buffer vtags-look-buf)
      (setq buffer-read-only nil)
      (if folding (setq tag (upcase tag)))
      ;;(message "vtags-look file is %s, size is %d, sort is %d" file size folding);
      ;;
      ;; Do a binary search on the file.
      ;; Stop when we have narrowed the search down
      ;; to a particular block within the file.
      ;;
      (while (> max-block  (+ 1 min-block))
	(setq mid (truncate (/ (+ max-block min-block) 2)))
	;;(message "min-block is %d" min-block )
	;;(message "mid is %d" mid) 
	;;(message "max-block is %d"  max-block)
	
	(setq beg (truncate (* mid blksize)))
	(erase-buffer)
	
	;; Get a chunk of the tag file
	(vtags-insert-chunk default-chunk-size)
	
	;;  skip past partial line
	(forward-line 1)
	
	;; Put line into tmp-string
	(setq tmp-string 
	      (buffer-substring-no-properties (point) (min (point-max) (+ (point) tag-length))))
	
	;; Compare with tag
	(if folding (setq tmp-string (upcase tmp-string)))
	(if (string-lessp tmp-string tag)
	    (progn
	      (setq min-block mid)
	      (message "%s < %s"  tmp-string tag))
	  (message "%s >= %s"  tmp-string tag)
	  (setq max-block mid)))
      ;;
      ;; Begin linear search on block (actually 2 blocks since
      ;; matching lines could span block boundary)
      ;;
      (erase-buffer)
      (setq beg (* min-block blksize))
      ;; read the block into buffer
      (insert-file-contents-literally 
       file nil beg (+ beg (* 2 blksize )))
      (if min-block (forward-line)) ;; skip past partial line
      
      (setq case-fold-search folding)
      ;;(message "case-fold-search is %s" case-fold-search)
      (search-forward tag nil t)
      (beginning-of-line)
      (while (and (not (= (point) (point-max)))
		  (not done))
	
	;; read a line
	(let ((start-of-line (point)))
	  (end-of-line)
	  (setq tag-line (buffer-substring-no-properties start-of-line (point))))
	
	(if (< tag-length (length tag-line))
	    (progn 
	      ;; are we past all lines which could match ?
	      (setq tmp-string (substring tag-line 0 tag-length))
	      (if folding (setq tmp-string (upcase tmp-string)))
	      (if (string-lessp tag tmp-string)
		  (setq done t)
		;; save lines which match
		(if (string-equal tag tmp-string)
		    (vtags-insert-string-into-buffer (concat tag-line "\n") output-buf)))))
	
	(forward-line 1)))))


(defsubst vtags-insert-string-into-buffer (the-string the-buffer)
  "Like Xemacs insert-string.  GNU Emacs insert-string behaves
  differently, so we need this wrapper."
  (if (string-match "XEmacs" (emacs-version))
      (insert-string the-string the-buffer)
    (save-excursion 
      (set-buffer the-buffer)
      (insert-string the-string))))

  
(defconst vtags-history-buffer "*Vtags-History*")
(defconst vtags-buffer-name "*Vtags-Buffer*")

(defvar  vtags-the-return-point nil "Ugly global variable" )
(defvar  vtags-other-window nil "Ugly global variable" )
(defvar vtags-truncate-lines t) ; Default value for truncate-lines
(defvar vtags-reuse-buffer t)   ; Use the same buffer for all tag command

(defun vtags-find-in-tagfiles (&optional tagname  tagfileinfo-list)
  "Creates \"*Vtags-Buffer*\" and loads tag entries matching tagname.
If only one entry matches, then we go to the corresponding file location,
otherwise, we leave the user here in *Vtags-Buffer* where
the user can choose which entry to follow.
The tagfileinfo-list is a list of tagfileinfo structures to search"
  (interactive 
   (list 
    (completing-read "Tag: " nil) 
    (list (completing-read "Tagfile : " tag-array) )))
  
  (setq vtags-the-return-point  (point-marker))
  (let ((cur-buf (current-buffer))
        (tag-buf (get-buffer-create 
                  (if vtags-reuse-buffer
                      vtags-buffer-name
                    (concat "TAG:" tagname))))
        (count 0)
        (tfi-list tagfileinfo-list)
        (tfi nil))
    (set-buffer tag-buf)
    (setq buffer-read-only nil)
    (fundamental-mode)
    (setq truncate-lines vtags-truncate-lines)
    (erase-buffer)
    ;; look up tag in each tagfile
    (message "tagname is %s" tagname)
    (while tfi-list
      (setq tfi (car tfi-list))
      (setq tfi-list (cdr tfi-list))
      (if tfi 
          (vtags-look tagname tfi tag-buf) )
      (set-buffer tag-buf)
      (goto-char (point-max)))
    (goto-char (point-min))
    (skip-chars-forward " \n\t")
    (if (eq (point) (point-max))
        (progn
          (kill-buffer tag-buf)
          (switch-to-buffer cur-buf)
          (beep)
          (message (concat "tag \"" tagname "\" is not found")))
      (goto-char (point-max))
      (vtags-property)
      (while (and (eq (forward-line -1) 0))
        (if (vtags-property) (setq count (1+ count))))
      (switch-to-buffer tag-buf)
      (vtags-mode)
      (message "count is %d" count)
      ;; If only one entry matches, then we go to the corresponding location,
      ;; otherwise, we leave the user here in *Vtags-Buffer* where
      ;; the user can choose which entry to follow.
      (if (< count 2)
          (vtags-source nil)))))


;; Set the text property to highlight the tags within the buffer
;; The tag is delineated by beginning of line and the first tab.
(defun vtags-property ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point))
          end tabpos)
      (search-forward "\t" nil t)
      (backward-char 1)
      (setq end (point))
      (beginning-of-line)
      (setq tabpos (point))
      ;; set property on correct entry
      (if (eq beg tabpos) ; found a tab on this line
          (if (string-match "XEmacs" (emacs-version))
              (progn
                (put-text-property (point) end 'face 'vt-face)
                (put-text-property (point) end 'highlight t))
            (put-text-property tabpos end 'mouse-face 'highlight))
        ;; else delete incorrect entry
        (end-of-line)
        (or (eq (point) (point-max)) (forward-char 1))
        (delete-region beg (point))
        (setq beg (eq beg tabpos)))
      beg)))

;; Return a default tag to search for, based on the text at point.
(defun vtags-find-tag-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring-no-properties
                (point)
                (progn (forward-sexp -1)
                       (while (looking-at "\\s'")
                         (forward-char 1))
                       (point))))
      nil)))

;; Read a tag name from the minibuffer with defaulting and completion.
(defun vtags-find-tag-tag (string)
  (let* ((completion-ignore-case (if (memq vtags-case-fold-search '(t nil))
				     vtags-case-fold-search
				   case-fold-search))
	 (default (funcall (or vtags-find-tag-default-function
			       (get major-mode 'vtags-find-tag-default-function)
			       'vtags-find-tag-default)))
	 (spec (completing-read (if default
				    (format "%s (default %s): "
					    (substring string 0 (string-match "[ :]+\\'" string))
					    default)
				  string)
				'tags-complete-tag
				nil nil nil nil default)))
    (if (equal spec "")
	(or default (error "There is no default tag"))
      spec)))

;; Get interactive args for find-tag{-noselect,-other-window,-regexp}.
(defun vtags-find-tag-interactive (prompt &optional no-default)
  (if (and current-prefix-arg last-tag)
      (list nil (if (< (prefix-numeric-value current-prefix-arg) 0)
		    '-
		  t))
    (list (if no-default
	      (read-string prompt)
	    (vtags-find-tag-tag prompt)))))

(defun vtags-find (tagname)
  "*Find tag whose name contains TAGNAME. If TAGNAME is a null string, 
the expression in the buffer around or before point is used as the tag name.
If there is more than one match, then you will find yourself
in the *Vtags-Buffer*. There you can select the tag entry that
you want using <RET>, f, or button-2. That is vtags-mode.
There is also a *Vtag-History* buffer with the same mode."
  (interactive (vtags-find-tag-interactive "Find tag: "))
  (vtags-find-in-tagfiles tagname  (or (vtags-table-computed-list)  
                                       (list (vtags-get-tagfileinfo vtags-file-name)))))

(defun vtags-set-tagfile (tagfilename)
  "vtags-set-tagfile: set the tagfile used by vtags-find."
  (interactive 
   (let* 
       ((filename (read-file-name "tag file: " vtags-file-name nil nil nil)))
     (list filename)
     )
   )
  (setq vtags-file-name tagfilename)
  (message "tagfile is %s" vtags-file-name))


(defun vtags-mouse-source (event)
  (interactive "e")
  (goto-char (if (string-match "XEmacs" (emacs-version))
                 (mouse-set-point event)
               (posn-point (event-end event))))
  (vtags-source nil))

(defun vtags-source (tag-file-attributes)
  "Called from within a vtag buffer, find the tag nearest point
and go to the corresponding location. This is the function that
actually parses the tag entry."
  (interactive)
  (message "tag-source")
  (if (eq (point) (point-max))
      (forward-line -1))
  (beginning-of-line)
  (let ((beg (point))
        filepath)
    (search-forward "\t" nil t) ;; skip past tag
    (if  ; is this line a correct tag entry?
        (save-excursion
          (beginning-of-line)
          (not (eq beg (point))))
        (progn
          (message "This is not a correct tag entry")
          (beep))
      (vtags-history)
      (setq beg (point))
      (search-forward "\t" nil t) ;; skip past file
      (if (eq (point) (point-min))
          ()
        (setq filepath  (buffer-substring beg (1- (point))))
        (if   (looking-at "[0-9:]")
            ;;
            ;; line number given
            ;;
            (let ((lineno (string-to-int
                           (buffer-substring
                            (point)
                            (progn (skip-chars-forward "0-9") (point))))))
              (message "line number is %d" lineno)
              (bury-buffer)
              (if vtags-other-window
                  (find-file-other-window filepath)
                (progn 
                  (find-file filepath)))
              (goto-line lineno)
              )
          ;;
          ;; search string given
          ;;
          (let ((prev-char-was-backslash nil)
                (search_string "")
                (tmp_string ""))
            (message "search string given")
            (search-forward "/") ;; beginning of search string
            (setq tmp_string
                  (buffer-substring
                   (point)
                   (progn
                     (search-forward-regexp "$/;" nil t)  
                     (backward-char 3)
                     (if (looking-at "/")  (1- (point)) (point))
                     (if (looking-at "$")  (1- (point)) (point)))))
            
            (let ((i 0) (len (length tmp_string)) x)
              (while (< i len)
                ;; loop through the string adding backslashes as needed for
                ;; special characters.  
                ;; "loop"  would require loading cl lisp library
                ;; that is why we use "while" instead of ...
                ;;         (loop for x across tmp_string 
                ;;               do (progn
                (setq x (aref tmp_string i))
                (setq i (1+ i))
                ;; tags files sometimes use search patterns that
                ;; look like this: / ... / and sometimes they use
                ;; search patterns that look like this: / ... \\/.
                (if (and prev-char-was-backslash (not (eq x ?/ ) ))
                    (setq search_string (concat search_string "\\\\")))
                (setq prev-char-was-backslash (eq x ?\\ ))
                (if (not prev-char-was-backslash)
                    (setq search_string 
                          (concat search_string 
                                  (cond 
                                   ((eq x ?* ) "\\\*" )
                                   ((eq x ?? ) "\\\?" )
                                   ((eq x ?. ) "\\\." )
                                   ((eq x ?+ ) "\\\+" )
                                   ((eq x ?[ ) "\\\[" )
                                    ((eq x ?] ) "\\\]" )
                                   (t (char-to-string x))))))))
            (message "search_string is %s"  search_string)
            (bury-buffer)
            (if vtags-other-window
                (find-file-other-window filepath)
              (progn 
                (find-file filepath)))
            (goto-char (point-min))
            (search-forward-regexp search_string)
            (beginning-of-line)
            )
          )
        (message "path is %s" filepath)
        (vtags-set-placeholder  vtags-the-return-point)
        (vtags-set-placeholder  (point-marker))))))

(defun vtags-history () "Move tag-entry from tag-list into *Vtags-History* buffer"
  (save-excursion
    (message "tag-history")
    (let ((truncate-lines vtags-truncate-lines)
          (h-buf (get-buffer-create vtags-history-buffer))
          str beg end)
      (beginning-of-line 2)
      (setq end (point))
      (backward-char 1)
      (beginning-of-line)
      (setq beg (point))
      (setq str (buffer-substring beg end))
      (set-buffer h-buf)
      (goto-char (point-min))
      (if (search-forward str nil t)
          ()
	(setq buffer-read-only nil)
        (fundamental-mode)
        (goto-char (point-min))
        (insert str)
        (vtags-property))
      (vtags-mode))))

(defun vtags-goto-history () "Switchs current buffer to *Vtags-History* buffer"
  (interactive)
  (switch-to-buffer vtags-history-buffer))

(defvar vtags-keymap nil "Local keymap for vtags-menu buffers")
(if vtags-keymap
    nil
  (setq vtags-keymap (make-keymap))
  (suppress-keymap vtags-keymap)
  (define-key vtags-keymap "" (lambda () (interactive)(vtags-source nil)))
  (define-key vtags-keymap [button2] 'vtags-mouse-source)
  (define-key vtags-keymap [mouse-2] 'vtags-mouse-source)
  (define-key vtags-keymap "q" (lambda () (interactive) (bury-buffer) nil))
  (define-key vtags-keymap "f" (lambda () (interactive)(vtags-source nil))))

(defun vtags-mode ()
  "Set major-mode to vtags-mode"
  (interactive)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'vtags-mode)
  (setq mode-name "Vtags")
  (use-local-map vtags-keymap))

(if (string-match "XEmacs" (emacs-version))
    (copy-face 'default 'vt-face))


(defun vtags-get-tag ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point))
          tmp-string end tabpos)
      (search-forward "\t" nil t)
      (backward-char 1)
      (setq end (point))
      (beginning-of-line)
      (setq tabpos (point))
      ;; set property on correct entry
      (if (eq beg tabpos) ; found a tab on this line
          (progn (setq tmp-string 
                       (buffer-substring-no-properties tabpos end))
                 (message "tmp-string is %s" tmp-string))
        ;; else delete incorrect entry
        (end-of-line)
        (or (eq (point) (point-max)) (forward-char 1))
        (delete-region beg (point))
        (setq beg (eq beg tabpos)))
      (cons tmp-string nil))))

(defconst vtags-completion-buffer-name "*Vtags-Completion-Buffer*")

(defun vtags-find-and-return-table (pattern)
  "Creates \"*Vtags-Completion-Buffer*\" creates an alist of matches to pattern.
Use `vtags-table-list' if non-nil, otherwise use `vtags-file-name'."
  (save-excursion
    (let ((tag-buf (get-buffer-create vtags-completion-buffer-name))
          (tfi-list (or (vtags-table-computed-list)  
			(list (vtags-get-tagfileinfo vtags-file-name))))
          (tfi nil)
          (table nil))
      (set-buffer tag-buf)
      (setq buffer-read-only nil)
      (fundamental-mode)
      (setq truncate-lines vtags-truncate-lines)
      (erase-buffer)
      ;; look up tag in each tagfile
      ;;(message "pattern is %s" pattern)
      (while tfi-list
        (setq tfi (car tfi-list))
        (setq tfi-list (cdr tfi-list))
        (if tfi 
            (vtags-look pattern tfi tag-buf) )
        (set-buffer tag-buf)
        (goto-char (point-max)))
      (goto-char (point-min))
      (skip-chars-forward " \n\t")
      (unless (eq (point) (point-max))
        (goto-char (point-max))
        (setq table (cons (vtags-get-tag) table))
        (while (and (eq (forward-line -1) 0))
          (setq table (cons (vtags-get-tag) table))))
      ;; (message "table is %s" (car table))
      table)))


;;; next_t
;;; qs

(defun vtags-complete-symbol ()
  "The function used to do vtags-completion"
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		;;(while (= (char-syntax (following-char)) ?\')
		;;  (forward-char 1))
		(skip-syntax-forward "'")
		(point)))
	 (pattern (buffer-substring beg end))
	 (table (or (vtags-find-and-return-table pattern) obarray))
         ;;(tmt table)
	 (completion (try-completion pattern table)))
    ;; (message "completion is %s" completion)
    (cond ((eq completion t))
	  ((null completion)
	   (error "Can't find completion for \"%s\"" pattern))
	  ((not (string-equal pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern table)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

;;; The following functions are 
(defun vtags-list-tags ()
  "Like etags list-tags. Display list of tags in file FILE.
This searches only the first table in the list, and no included tables.
FILE should be as it appeared in the `ctags' command, usually without a
directory specification."
  (interactive)
  )

(defun vtags-tags-apropos ()
  "Like etags tags-apropos. Display list of all tags in tags table REGEXP matches."
  (interactive)
)

; xemacs only
; (defun vtags-ctypes-tags ()
;   "Like etags ctypes-tags."
;   (interactive)
; )

; xemacs only
; (defun vtags-smart-tags-file-list ()
;   "Like etags smart-tags-file-list.
;    Return appropriate tag files list for optional CURR-DIR-OR-FILENAME or for `default-directory'.
;    Optional NAME-OF-TAGS-FILE is the literal filename (no directory) for which
;    to look.  If no tags file is found, an error is signaled."
;   (interactive)
; )

; xemacs only
; (defun vtags-smart-tags-file-path ()
;   "Like etags smart-tags-file-path."
;   (interactive)
; )

(defun vtags-tags-query-replace ()
  "Like etags tags-query-replace.
`Query-replace-regexp' FROM with TO through all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-g or ESC), you can resume the query-replace
with the command M-,."
  (interactive)
)

(defun vtags-tags-search ()
  "Like etags tags-search."
  (interactive)
)

(defun vtags-visit-tags-table ()
  "Like etags visit-tags-table.
Tell tags commands to use tags table file FILE.
FILE should be the name of a file created with the `ctags' program.
A directory name is ok too; it means file TAGS in that directory.

Normally M-x visit-tags-table sets the global value of `tags-file-name'.
With a prefix arg, set the buffer-local value instead.
When you find a tag with M-., the buffer it finds the tag
in is given a local value of this variable which is the name of the tags
file the tag was in."
  (interactive)
)

(defun vtags-loop-continue ()
  "Like etags tags-loop-continue.
Continue last M-x tags-search or M-x tags-query-replace command.
Used noninteractively with non-nil argument to begin such a command (the
argument is passed to `next-file', which see).

Two variables control the processing we do on each file: the value of
`tags-loop-scan' is a form to be executed on each file to see if it is
interesting (it returns non-nil if so) and `tags-loop-operate' is a form to
evaluate to operate on an interesting file.  If the latter evaluates to
nil, we exit; otherwise we scan the next file."
  (interactive)
)

(defun vtags-select-tags-table ()
  "Like etags select-tags-table.
Select a tags table file from a menu of those you have already used.
The list of tags tables to select from is stored in `tags-table-set-list';
see the doc of that variable if you want to add names to the list."
  (interactive)
)

(defun vtags-push-tag-mark ()
  "Like etags push-tag-mark."
  (interactive)
  )

(defun vtags-pop-tag-mark ()
  "Like etags pop-tag-mark."
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;                      placeholder stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

; Placeholders are set at your departure and arrival points 
; when you jump to tags. You can navigate forward and back through
; the places to which you have tagged.


(defvar vtags-placeholder-alist nil
  "Alist of elements (key . CONTENTS), one for each vtags placeholder.")

(defvar vtags-current-placeholder nil
  "vtags-current-placeholder")

(defun vtags-prev-placeholder ()
  "vtags-prev-placeholder"
  (interactive)
  (vtags-jump-to-placeholder (1- 0)))

(defun vtags-goto-current-placeholder ()
  "vtags-goto-current-placeholder"
  (interactive)
  (vtags-jump-to-placeholder 0))

(defun vtags-next-placeholder ()
  "next-placeholder"
  (interactive)
  (vtags-jump-to-placeholder 1))

(defun vtags-reset-placeholders ()
  "reset-placeholders"
  (interactive)
  (setq vtags-placeholder-alist nil)
  (setq vtags-current-placeholder nil))

(defun vtags-current-char () "vtags-current-char" 
  (if vtags-current-placeholder (car (car vtags-current-placeholder)) 0))

(defun vtags-get-placeholder ()
  "Return contents of current placeholder, or nil if none."
  (if vtags-current-placeholder (car vtags-current-placeholder) nil))

(defun vtags-set-placeholder (value)
  "Store the marker in the vtags placeholder list"
  (interactive "S")
  (let (aelt char)
    (progn
      (setq char (1+ (vtags-current-char)))
      (setq aelt (cons char value))
      (if (not (equal vtags-placeholder-alist vtags-current-placeholder))
          (setq vtags-placeholder-alist vtags-current-placeholder))
      
      (setq vtags-placeholder-alist  (cons aelt vtags-placeholder-alist))
      (setq vtags-current-placeholder vtags-placeholder-alist))))

(defun vtags-point-to-placeholder ()
  "Store current location of point in placeholder PLACEHOLDER."
  (interactive)
  (vtags-set-placeholder  (point-marker)))

(defalias 'vtags-placeholder-to-point 'vtags-jump-to-placeholder)

(defun vtags-placeholder-find (item)
  "Find the first occurrence of ITEM in placeholder-alist.
   Return the sublist of placeholder-alist whose car is ITEM."
  (let ((tmp-list vtags-placeholder-alist))
    (while (and tmp-list (not (equal item (car (car tmp-list)))))
      (setq tmp-list (cdr tmp-list)))
    tmp-list))

(defun vtags-jump-to-placeholder (direction)
  "Move point to location stored in the next curr or prev (+ 0 -) placeholder."
  (interactive)
  ;; (message "direction is %d" direction)
  (cond 
   ((> 0 direction)
    (if (consp  (cdr vtags-current-placeholder))
        (setq vtags-current-placeholder (cdr vtags-current-placeholder))
      (message "At beginning of vtags-placeholder-alist")))
   ((< 0 direction)
    
    (let (
          ;; (tmp-placeholder (member*  (1+ (vtags-current-char)) placeholder-alist
          ;;                           :key 'car ))
          
          (tmp-placeholder (vtags-placeholder-find (1+ (vtags-current-char)))))
      (if tmp-placeholder 
          (setq vtags-current-placeholder tmp-placeholder)
        (message "At end of vtags-placeholder-alist")))))
  
  (let ((val (cdr (car vtags-current-placeholder))))
    (cond
     ((markerp val)
      (or (marker-buffer val)
	  (error "That placeholder's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     (t
      (error "Placeholder doesn't contain a buffer position")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regression testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO 
;;  Add tests for languages other than C++
;;  Add performance tests
;;

(defvar vtags-testing nil)
;(eval-when-compile
;  (setq vtags-testing t))

(defun test-vtags-find (tagname test-tagfile expected-file expected-line)
  "test whether vtags correctly locates tagname"
  (interactive)
  (save-window-excursion
    (let ((result t))
      (vtags-find-in-tagfiles tagname (list (vtags-get-tagfileinfo test-tagfile)))

      ;;(message "%s ?= %s, %d ?= %d" 
      ;;         (file-name-nondirectory (buffer-file-name)) expected-file
      ;;         (line-number) expected-line)
      (unless (equal (file-name-nondirectory (buffer-file-name)) expected-file)
	(progn 
	  (setq FAILURE-INDICATION 
		(format "expected file %s, got %s" (file-name-nondirectory (buffer-file-name)) expected-file))
	  (setq result nil)))
      
      (unless (equal  expected-line (1+ (count-lines 1 (point-at-bol))))
	(progn 
	  (setq FAILURE-INDICATION 
		(format "expected line %d, got %d" expected-line (1+ (count-lines 1 (point-at-bol)))))
	  (setq result nil)))
      
      result)))
  

(put 'vtags-find-tests 'regression-suite t)
(setq vtags-find-tests
      '("vtags-find tests"
	;; Each test in the suite is of the form:
	;;   ([description] probe grader)
	;;   DESCRIPTION - string
	;;   PROBE -  a sexp which runs the actual test
	;;   GRADER - the desired result or a sexp which determines
	;;   how we did
	
	("find tag setup_frame_gcs"
	 (test-vtags-find "setup_frame_gcs" "tags.emacs-21.4"
			  "widget.c" 
			  572)
	 t)

	("find tag MERGE_INSERTIONS "
	 (test-vtags-find "MERGE_INSERTIONS"  "tags.emacs-21.4"
                          "intervals.h" 
                          163)
	 t)

        ))

(defun test-vtags-look (test-tagname test-tagfile expected-output)
  "test whether vtags-look correctly locates tagname in tagfile"
  (let ((result t)
	(save-inhibit-read-only inhibit-read-only)
	test-output-string)
    
    (setq inhibit-read-only t)
    (save-current-buffer
      (set-buffer (get-buffer-create vtags-buffer-name))
      (erase-buffer))
    
    (vtags-look test-tagname (vtags-get-tagfileinfo test-tagfile) vtags-buffer-name)
    
    (setq inhibit-read-only save-inhibit-read-only)

    (save-current-buffer
      (set-buffer (get-buffer-create vtags-buffer-name))
	(setq test-output-string (buffer-substring-no-properties (point-min) (point-max))))
    
    (unless (equal expected-output test-output-string)
      (progn 
	(message "expected %s" expected-output)
	(message "got %s" test-output-string)
	(setq FAILURE-INDICATION 
	      (format "expected  %s  got  %s" 
		      expected-output 
		      test-output-string))
	(setq result nil)))
    
    result))


(put 'vtags-look-tests 'regression-suite t)
(setq vtags-look-tests
      '("vtags-look tests"
	;; Each test in the suite is of the form:
	;;   ([description] probe grader)
	;;   DESCRIPTION - string
	;;   PROBE -  a sexp which runs the actual test
	;;   GRADER - the desired result or a sexp which determines
	;;   how we did
	
	("look VALAMASK"
	 (test-vtags-look 
	  "VALAMASK" "tags.emacs-21.4" 
	  "VALAMASK	emacs-21.4/src/m/gec63.h	59;\"	d\n"
	  )
	 t)))

;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(when vtags-testing
  (autoload 'regress "regress" "run regression test suites" t)
  
  (if (featurep 'regress)
      (regress vtags-find-tests vtags-look-tests)))


(provide 'vtags)

