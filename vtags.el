;; Copyright (C) 1994-2004 Edward Bishop

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

;; This code has been tested with XEmacs 21.4 and Exuberant Ctags 5.2.2
;; but should work with almost any version of Emacs or ctags.
;;
;;;;;;;;;;;;;;;;;;;;;;
;; Troubleshooting:
;;;;;;;;;;;;;;;;;;;;;;
;;
;; If you get an error message like 
;;    Wrong type argument: number-char-or-marker-p, nil
;; then check that vtags-file is set correctly.
;;
;; If your tag file has very long lines (>512) then you 
;; should increase chunk-size.


(defvar vtags-case-fold nil)

(defun vtags-toggle-casefold ()
  "*Non-nil if searches should ignore case.
Note: case folding must
match that used in tags file generation."
  (interactive)
  (setq vtags-case-fold (not vtags-case-fold))
  (message "case folding is %s" (if vtags-case-fold "ON" "OFF")))

;; vtags-look
(defun vtags-look  (tag file output-buffer-name) 
  "Like unix look command. Does a binary search on file looking for tag."
  (interactive 
   (list 
    (completing-read "Tag: " nil) 
    (completing-read "Tagfile : " nil )
    (completing-read "Output Buffer : " nil )
    ))
  (save-excursion
    (let ((attr (file-attributes file))
          (vtags-look-buf (get-buffer-create "*Vtags-Look-Buffer*")) ; scratch buffer
          (output-buf (get-buffer-create output-buffer-name))
          (blksize 4096) ; big enough to hold all matching entries
          (chunk-size 1024) ; twice the length of longest line
          (max 0)
          (min 0)
          (mid 0)
          (beg 0)
          (done nil)
          (tag-length (length tag))
          (size 0)
          tmp-string tmp-string-beg tmp-string-end tag-line)
      (setq size  (nth 7 attr))
      (setq max (truncate (/ size blksize)))
      (set-buffer vtags-look-buf)
      (if vtags-case-fold (setq tag (upcase tag)))
      ;;(message "vtags-look file is %s, size is %d" file size);
      ;;
      ;; Do a binary search on the file.
      ;; Stop when we have narrowed the search down
      ;; to a particular block within the file.
      ;;
      (while (> max  (+ 1 min))
        (setq mid (truncate (/ (+ max min) 2)))
        ;;(message "min is %d" min )
        ;;(message "mid is %d" mid) 
        ;;(message "max is %d"  max)

        (setq beg (truncate (* mid blksize)))
        (erase-buffer)
        
	;; Grab a chunk of the file.  The chunk has to be
	;; big enough that we are sure to get at least one
	;; complete line.
        (insert-file-contents-literally 
         file nil beg (+ beg chunk-size))
        
        ;; skip past partial line
        (forward-line 1)
        
        (setq tmp-string-beg (point))
        (setq tmp-string-end (+ (point) tag-length))
        (if (> tmp-string-end chunk-size) (progn (beep)
              (error "tag line length is %d, max is %d. Fix your tag file or increase chunk-size in vtags.el" 
                     tmp-string-end chunk-size)))
        ;; Put line into tmp-string
        (setq tmp-string 
              (buffer-substring-no-properties tmp-string-beg tmp-string-end))
        
        ;; Compare with tag
        (if vtags-case-fold (setq tmp-string (upcase tmp-string)))
        (if (string-lessp tmp-string tag)
            (progn
              (setq min mid)
              ;; (message "%s < %s"  tmp-string tag)
              )
          ;; (message "%s >= %s"  tmp-string tag)
          (setq max mid)))
      ;;
      ;; Begin linear search on block (actually 2 blocks since
      ;; matching lines could span block boundary)
      ;;
      (erase-buffer)
      (setq beg (* min blksize))
      ;; read the block into buffer
      (insert-file-contents-literally 
       file nil beg (+ beg (* 2 blksize )))
      (if min (forward-line)) ;; skip past partial line

      (setq case-fold-search vtags-case-fold)
      ;;(message "case-fold-search is %s" case-fold-search)
      (search-forward tag)
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
              (if vtags-case-fold (setq tmp-string (upcase tmp-string)))
              (if (string-lessp tag tmp-string)
                  (setq done t)
                ;; save lines which match
                (if (string-equal tag tmp-string)
                    (vtags-insert-string-into-buffer (concat tag-line "\n") output-buf)))))

        (forward-line 1)))))


(defun vtags-insert-string-into-buffer (the-string the-buffer)
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

(defun vtags-find-in-tagfiles (&optional tagname  place-list)
  "Creates \"*Vtags-Buffer*\" and loads tag entries matching tagname.
The place-list is a list of tag files to search"
  (interactive 
   (list 
    (completing-read "Tag: " nil) 
    (list (completing-read "Tagfile : " tag-array) )
    ))
  (setq vtags-the-return-point  (point-marker))
  (let ((cur-buf (current-buffer))
        (tag-buf (get-buffer-create 
                  (if vtags-reuse-buffer
                      vtags-buffer-name
                    (concat "TAG:" tagname))))
        (count 0)
        (tmp-list place-list)
        (tag-path nil))
    (set-buffer tag-buf)
    (toggle-read-only 0)
    (fundamental-mode)
    (setq truncate-lines vtags-truncate-lines)
    (erase-buffer)
    ;; look up tag in each tagfile
    (message "tagname is %s" tagname)
    (while tmp-list
      (setq tag-path (car tmp-list))
      (setq tmp-list (cdr tmp-list))
      (if tag-path 
          (vtags-look tagname tag-path tag-buf) )
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
      (toggle-read-only 1)
      (message "count is %d" count)
      (if (< count 2)
          (vtags-source)))))


;; Set the text property to highlight the tags within the buffer
;; The tag is the delineated by beginning of line and the first tab.
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

(defun vtags-vt-token (prompt &optional charset)
  "The vtags-vt-token is used as an argument for interactive.
For example:
      (interactive (vtags-vt-token \"Find tag\"))
It find out one word around current point and use it as
default value for promt.

RETURN: default or value entered by user."
  (if (not charset)
      (setq charset "-_a-zA-Z0-9."))
  (let (def-tok val end (usemark nil))
    (save-excursion
      (setq usemark 	      (equal (point) (mark t)))
      (setq def-tok
	    (if (not usemark)
		(progn (skip-chars-forward " \t'(")
		       (skip-chars-forward charset)
		       (setq end (point))
		       (skip-chars-backward charset)
		       (buffer-substring (point) end))
	      (buffer-substring (point) (mark)))))
    (setq val
	  (completing-read (concat prompt
                                   (if (and def-tok (not (equal def-tok "")))
                                       (concat " (default " def-tok ")"))
                                   ": ") obarray))
    (list (if (equal val "") def-tok val))))


(defvar vtags-tagfile "~/tags")

(defun vtags-find (tagname)
  "*Find tag whose name contains TAGNAME. If TAGNAME is a null string, 
the expression in the buffer around or before point is used as the tag name.
If there is more than one match, then you will find yourself
in the *Vtags-Buffer*. There you can select the tag entry that
you want using <RET>, f, or button-2. That is vtags-mode.
There is also a *Vtag-History* buffer with the same mode."
  (interactive (vtags-vt-token "Find tag"))
  (vtags-find-in-tagfiles tagname  (list vtags-tagfile) ))

(defun vtags-set-tagfile (tagfile)
  "vtags-set-tagfile: set the tagfile used by vtags-find."
  (interactive 
   (let* ((filename (read-file-name "tag file: " 
                                    vtags-tagfile nil nil nil))
          start end)
     (list filename)
     ))
  (message "tagfile is %s" vtags-tagfile))

(defun vtags-mouse-source (event)
  (interactive "e")
  (goto-char (if (string-match "XEmacs" (emacs-version))
                 (mouse-set-point event)
               (posn-point (event-end event))))
  (vtags-source))

(defun vtags-source ()
  "Called from within a vtag buffer, find the tag nearest point
and go to the corresponding location. This is the function that
actually parses the tag entry."
  (interactive)
  (message "tag-source")
  (save-excursion
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
          (vtags-set-placeholder  (point-marker))
          )))))

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
        (toggle-read-only 0)
        (fundamental-mode)
        (goto-char (point-min))
        (insert str)
        (vtags-property))
      (vtags-mode)
      )))

(defun vtags-goto-history () "Switchs current buffer to *Vtags-History* buffer"
  (interactive)
  (switch-to-buffer vtags-history-buffer))

(defvar vtags-keymap nil "Local keymap for vtags-menu buffers")
(if vtags-keymap
    nil
  (setq vtags-keymap (make-keymap))
  (suppress-keymap vtags-keymap)
  (define-key vtags-keymap "" 'vtags-source)
  (define-key vtags-keymap [button2] 'vtags-mouse-source)
  (define-key vtags-keymap [mouse-2] 'vtags-mouse-source)
  (define-key vtags-keymap "f" 'vtags-source))

(defun vtags-mode ()
  "Set major-mode to vtags-mode"
  (interactive)
  (setq truncate-lines t)
  (toggle-read-only 1)
  (setq major-mode 'vtags-mode)
  (setq mode-name "Vtags")
  (use-local-map vtags-keymap)
  )

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
  "Creates \"*Vtags-Completion-Buffer*\" creates an alist of matches to pattern"
  (save-excursion
    (let ((tag-buf (get-buffer-create vtags-completion-buffer-name))
          (tmp-list (list vtags-tagfile))
          (tag-path nil)
          (table nil))
      (set-buffer tag-buf)
      (toggle-read-only 0)
      (fundamental-mode)
      (setq truncate-lines vtags-truncate-lines)
      (erase-buffer)
      ;; look up tag in each tagfile
      ;;(message "pattern is %s" pattern)
      (while tmp-list
        (setq tag-path (car tmp-list))
        (setq tmp-list (cdr tmp-list))
        (if tag-path 
            (vtags-look pattern tag-path tag-buf) )
        (set-buffer tag-buf)
        (goto-char (point-max)))
      (goto-char (point-min))
      (skip-chars-forward " \n\t")
      (if (eq (point) (point-max))
          nil
        (goto-char (point-max))
        (setq table (cons (vtags-get-tag) table))
        (while (and (eq (forward-line -1) 0))
          (setq table (cons (vtags-get-tag) table))))
      ;; (message "table is %s" (car table))
      table)))



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
    (message "completion is %s" completion)
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
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.

Normally M-x visit-tags-table sets the global value of `tags-file-name'.
With a prefix arg, set the buffer-local value instead.
When you find a tag with M-., the buffer it finds the tag
in is given a local value of this variable which is the name of the tags
file the tag was in."
  (interactive)
)

(defun tags-loop-continue ()
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

(defun select-tags-table ()
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
          
          (tmp-placeholder (vtags-placeholder-find (1+ (vtags-current-char))))
          
          )
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


