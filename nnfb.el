;; Attempt at running fb from gnus.

(require 'gnus)
(require 'nnoo)

(gnus-declare-backend "nnfb" 'none)
(nnoo-declare nnfb)

(defvar nnfb-sessions nil
  "List of sessions.
Sessions are identified with their fb login and the token is saved for
each session in an assoc list")

(defvar nnfb-status-string "" nil)

(defun nnfb-get-token (server)
  (let ((found (assoc server nnfb-sessions)))
    (if found
        (car (cdr found))
      (nnheader-report 'nnfb "Cannot find server %s" server))))

(defun nnfb-pyread ()
  "Read a python struct from the current buffer and return a lisp structure."
  (cond
   ((looking-at "\"")
    (read))
   ((looking-at "[0-9][.0-9]*")
    (prog1
        (string-to-number (buffer-substring (match-beginning 0) (match-end 0)))
      (goto-char (match-end 0))))
   ((looking-at "{")
    (forward-char)
    (let (found)
      (while (looking-at "\"")
        (let* ((id (read))
               (is-colon (looking-at ":"))
               (colon-point (prog1
                                (point)
                              (forward-char)))
               (value (nnfb-pyread)))
          (if is-colon
              (setq found (cons (cons id value) found))
            (nnheader-report 'nnfb
                             "Expecting colon, sees %s"
                             (buffer-substring colon-point (point-max)))))
        (cond
         ((looking-at ",")
          (forward-char))
         ((looking-at "}"))
         (t 
          (nnheader-report 'nnfb
                           "Expecting comma or closing brace, found %s"
                           (buffer-substring (point) (point-max))))))
      (forward-char)
      (reverse found)))
   ((looking-at (regexp-quote "["))
    (forward-char)
    (let (found)
      (while (not (looking-at (regexp-quote "]")))
        (setq found (cons (nnfb-pyread) found))
        (cond
         ((looking-at ",")
          (forward-char))
         ((looking-at (regexp-quote "]")))
         (t
          (nnheader-report 'nnfb "Expecting comma or closing bracket, found %s" (read)))))
      (forward-char)
      (apply (function vector) (reverse found))))
   ((looking-at "true")
    (prog1
        t
      (goto-char (match-end 0))))
   ((looking-at "false")
    (prog1
        nil
      (goto-char (match-end 0))))
   (t
    (nnheader-report 'nnfb "Unknown opening" (read)))))

    
(defun nnfb-get (what &optional token)
  "Retrieve the request WHAT from the server."
  (message "nnfb-get %s" what)
  (let* (done
         (add-for-token
          (if token
              (concat
               (if (string-match (regexp-quote "?") what)
                   "&"
                 "?")
               "access_token=" token)
            ""))
         (buffer
          (url-retrieve-synchronously
           (url-generic-parse-url
            (let ((url (concat "https://graph.facebook.com/"
                               what add-for-token)))
              (message "Retrieving: %s" url)
              url)))))
    (let (result)
      (save-excursion
        (set-buffer buffer)
        (goto-char 1)
        (if (search-forward "\n\n" (point-max) t)
            (let ((standard-input (current-buffer)))
              (setq result (nnfb-pyread)))
          (nnheader-report 'nnfb
                           "No contents was returned for %s. Result was %s"
                           what (buffer-string))))
      (message "Got: %s" result)
      ;; (kill-buffer buffer)
      (let ((error (assoc "error" result)))
        (if error
            (let* ((contents (cdr error))
                   (messpair (assoc "message" contents))
                   (mess (cdr messpair)))
              (message "%s" mess)
              (sit-for 1))))
      result)))

(defmacro nnfb-direct-output (&rest do)
  "Output things in nntp-server-buffer."
  `(let ((standard-output nntp-server-buffer))
     (save-excursion
       (set-buffer nntp-server-buffer)
       (erase-buffer))
     ,@do))
(put 'nnfb-direct-output 'lisp-indent-function 0)

(defvar nnfb-current-group-name nil)
(defvar nnfb-current-group-information (make-hash-table :test 'equal)
  "The hash table contains all id:s.

This is reset and recalculated from nnfb-all-group-mappings whenever
the current group is changed.")

(defvar nnfb-all-group-mappings nil
  "Assoc list with groups and vectors of id:s.

The vector's first element is the number of the first free element 
of the vector itself.")

(defun nnfb-set-current-group (name)
  "Change the group."
  (if (string= name nnfb-current-group-name)
      nil
    (setq nnfb-current-group-name nil)
    (clrhash nnfb-current-group-information)
    (let* ((a (assoc name nnfb-all-group-mappings))
           (mapping (cdr a)))
      (if mapping
          (let ((i 1))
            (while (< i (aref mapping 0))
              (puthash (aref mapping i) i nnfb-current-group-information)
              (setq i (1+ i))))))
    (setq nnfb-current-group-name name)))

(defun nnfb-get-index (id)
  "Get or allocate the gnus index number in the group.
This assumes that there is a current group already set."
  (if nnfb-current-group-name
      nil
    (signal 'no-current-group-set))
  (or (gethash id nnfb-current-group-information)
      (let* ((a (assoc nnfb-current-group-name nnfb-all-group-mappings))
             (mapping (cdr a))
             pos)
        ;; Create a new empty vector if not already
        (if mapping
            nil
          (setq mapping (make-vector 10 nil))
          (aset mapping 0 1))
        ;; Grow the vector if filled
        (if (aref mapping (1- (length mapping)))
            (setq mapping (apply (function vector)
                                 (append mapping (make-list 100 nil)))))
        ;; Add the new element
        (setq pos (aref mapping 0))
        (aset mapping pos id)
        (aset mapping 0 (1+ pos))
        ;; Store the new mapping back
        (if a
            (setcdr a mapping)
          (setq nnfb-all-group-mappings
                (cons (cons nnfb-current-group-name mapping)
                      nnfb-all-group-mappings)))
        ;; Store the id in the hash
        (puthash id pos nnfb-current-group-information)
        pos)))
        
(defun nnfb-get-group-information-count ()
  "Calculate the count of the current group."
  (hash-table-count nnfb-current-group-information))

(defun nnfb-get-group-information-first ()
  1)

(defun nnfb-get-group-information-last ()
  (nnfb-get-group-information-count))


;;; Interface functions
(nnoo-define-basics nnfb)

(defun nnfb-create-nov-entry (message articles fetch-old)
  "Convert the MESSAGE into a nov entry.
If the MESSAGE is in ARTICLES or FETCH-OLD is true.
Returns a sequence of ARTICLES without the found entry.

Expects the standard-output to be set up."
  (let* ((id (cdr (assoc "id" message)))
         (index (nnfb-get-index id))
         (text (or (cdr (assoc "message" message))
                   (cdr (assoc "description" message))
                   (cdr (assoc "story" message))))
         (beg (if (> (length text) 70)
                  (substring text 0 60)
                text))
         (first-line (if (string-match "\\(.*\\)\n" beg)
                         (substring beg (match-beginning 1) (match-end 1))
                       beg)))
    (if (or fetch-old
            (memq index articles))
        (princ (format "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
                         index
                         beg
                         (cdr (assoc "name" (cdr (assoc "from" message))))
                         "date"
                         id
                         "references"
                         "chars"
                         (1+ (/ (length text) 62))
                         ""             ; xref
                         "extra")))
    (remq index articles)))

(defun nnfb-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers."
  (message "nnfb-retrieve-headers %S %S %S %S" articles group server fetch-old)
  (let* ((token (nnfb-get-token server))
         (feed (nnfb-name-to-id group))
         (result (nnfb-get (format "%s?fields=feed" feed) token)))
    (nnfb-direct-output
      (while (and articles
                  result
                  (assoc "feed" result))
        (let* ((result-feed-pair (assoc "feed" result))
               (result-data-pair (assoc "data" (cdr result-feed-pair)))
               (arr (cdr result-data-pair)))
          (mapc (function
                 (lambda (mess)
                   (setq articles (nnfb-create-nov-entry mess 
                                                         articles
                                                         fetch-old))))
                arr)
          (let* ((result-paging-pair (assoc "paging" (cdr result-feed-pair)))
                 (result-next-pair (assoc "next" (cdr result-paging-pair)))
                 (next (cdr result-next-pair))
                 (str (progn
                        (string-match "^https?://[^/]*/\\(.*\\)$" next)
                        (match-string 1 next))))
            (setq result (nnfb-get str token))))))
    'nov))

(defun nnfb-open-server (server &rest definitions)
  "Required function"
  (message "nnfb-open-server %S %S" server definitions)
  (if (nnfb-server-opened server)
      t
    (let* (;; Test the connection
           (s (nnfb-get server)))
      (if s
          (let* ((token (read-string (format "Enter token for %s: " server)))
                 ;; Test the token
                 (me (nnfb-get "me" token))
                 (as (assoc "id" s))
                 (ame (assoc "id" me)))
            (cond
             ((null as)
              (nnheader-report 'nnfb
                               "No id found in %S" s))
             ((null ame)
              (nnheader-report 'nnfb
                               "No id found in %S" me))
             ((string-equal (cdr as)
                            (cdr ame))
              (setq nnfb-sessions (cons (list server token)
                                        nnfb-sessions)))
             (t (nnheader-report 'nnfb
                                 "Token %s is for %s while given user is %s"
                                 token ame as))))))))

(defun nnfb-close-server (&rest rest)
  "Required function"
  (message "nnfb-close-server %S" rest)
  (signal 'not-implemented-yet nil))

(defun nnfb-request-close ()
  "Required function"
  (message "nnfb-request-close")
  (setq nnfb-sessions nil))

(defun nnfb-server-opened (server)
  "Required function"
  (let ((pair (assoc server nnfb-sessions)))
    (cdr pair)))

(defun nnfb-join-string (elements)
  "Join the elements in ELEMENTS (a sequence) into a comma-separated string."
  (apply (function concat)
         (apply (function append)
                (list (car elements))
                (mapcar (function (lambda (element) (list ", " element)))
                        (cdr elements)))))

(defun nnfb-request-article (article &optional group server to-buffer)
  "Required function."
  (let* ((id (cond
              ((stringp article) article)
              ((integerp article) (aref (cdr (assoc nnfb-current-group-name 
                                                    nnfb-all-group-mappings))
                                        article))))
         (request (nnfb-get id (nnfb-get-token server)))
         (buffer (or to-buffer (get-buffer nntp-server-buffer))))
    (let ((standard-output buffer))
      (save-excursion
        (set-buffer buffer)
        (let (unhandled body)
          (mapc
           (function
            (lambda (pair)
              (cond
               ((string= "id" (car pair)))
               ((string= "comment" (car pair)))
               ((string= "actions" (car pair)))
               ((string= "from" (car pair))
                (princ "From: ")
                (princ (format "%s\n" (cdr (assoc "name" (cdr pair))))))
               ((string= "to" (car pair))
                (princ "To: ")
                (princ
                 (nnfb-join-string
                  (mapcar
                   (function
                    (lambda (person)
                      (cdr (assoc "name" person))))
                   (cdr (assoc "data" (cdr pair))))))
                (princ "\n"))
               ((string= "message" (car pair))
                (setq body (cdr pair)))
               ((string= "updated_time" (car pair))
                (princ "Date: ")
                (princ (cdr pair))
                (princ "\n"))
               ((string= "created_time" (car pair)))
               ((and (string= "application" (car pair))
                     (assoc "name" (cdr pair)))
                (princ "Application: ")
                (princ (cdr (assoc "name" (cdr pair))))
                (princ "\n"))
               ((and (string= "type" (car pair))
                     (string= "status" (cdr pair))))
               ((and (string= "privacy" (car pair))
                     (string= "value" (car (car (cdr pair))))))
               ((and (string= "likes" (car pair))
                     (assoc "data" (cdr pair)))
                (let ((arr (cdr (assoc "data" (cdr pair)))))
                  (princ "Likes: ")
                  (princ (nnfb-join-string
                          (mapcar
                           (function
                            (lambda (alike)
                              (cdr (assoc "name" alike))))
                           arr)))
                  (princ "\n")))
                
               (t
                (setq unhandled (cons pair unhandled))))))
           request)
          (princ "\n")
          (if body
              (progn
                (princ body)
                (princ "\n")
                (princ "\n")))
          (mapc 
           (function
            (lambda (pair)
              (princ (format "Unhandled data: %S\n" pair))))
           unhandled)
          t)))))


(defun nnfb-request-group (group &optional server fast info)
  "Required function"
  (message "nnfb-request-group %s %s %S %S" group server fast info)
  (nnfb-set-current-group group)
  (if fast
      t
    (let* ((token (nnfb-get-token server))
           (id (nnfb-name-to-id group))
           (first-result
            (nnfb-get (concat id
                              "?fields=feed.limit(200)."
                              "fields(comments.fields(id),id)")
                      token)))
      (message "Found %S" first-result)
      ;; Allocate id:s to local number.
      (mapc
       (function (lambda (l)
                   (nnfb-get-index (cdr (assoc "id" l)))
                   (let ((comments (cdr (assoc "comments" l))))
                     (if comments
                         (mapc (function nnfb-get-index)
                               (cdr (assoc "data" comments)))))))
       (cdr (assoc "data" (cdr (assoc "feed" first-result)))))
      (nnfb-direct-output
        (princ (format "211 %d %d %d %s\n"
                       (nnfb-get-group-information-count)
                       (nnfb-get-group-information-first)
                       (nnfb-get-group-information-last)
                       group))
        (nnfb-get-group-information-count)))))

(defun nnfb-close-group (&rest rest)
  "Required function")

(defun nnfb-friend-to-name (list)
  "Converts an fb friend list to a Gnus name."
  (concat (format "friend.%s." (cdr (assoc "id" list)))
          (mapconcat (function identity)
                     (split-string (cdr (assoc "name" list)))
                     ".")))

(defun nnfb-name-to-id (name)
  "Converts an Gnus group name to an id."
  (string-match "\\(friend\\)\\.\\([0-9][0-9]*\\)\\..*" name)
  (match-string 2 name))

(defun nnfb-request-list (server)
  "Required function"
  (message "nnfb-request-list %s" server)
  (let* ((token (nnfb-get-token server))
         (result (nnfb-get "me?fields=friends.fields(name,id)" token))
         (friends (cdr (assoc "friends" result)))
         (data (cdr (assoc "data" friends))))
    (message "Found %S" result)
    (nnfb-direct-output
      (mapc
       (function (lambda (ele)
                   (princ (format "%s 999999 000000 y\n"
                                  (nnfb-friend-to-name ele)))))
       data))
    result))

(defun nnfb-request-post (&rest rest)
  "Required function"
  (message "nnfb-request-post %S" rest)
  (signal 'not-implemented-yet nil))

(provide 'nnfb)
