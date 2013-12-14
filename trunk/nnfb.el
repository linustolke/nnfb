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
  "Read a python struct from the current buffer."
  (cond
   ((looking-at "\"")
    (read))
   ((looking-at "[0-9][0-9]*")
    (prog1
        (string-to-int (buffer-substring (match-beginning 0) (match-end 0)))
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
          nnheader-report 'nnfb "Expecting comma or closing bracket, found %s" (read))))
      (forward-char)
      (apply (function vector) (reverse found))))
   (t
    (nnheader-report 'nnfb "Unknown opening" (read)))))
    

(defun nnfb-get (what &optional token)
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
            (concat "https://graph.facebook.com/" what add-for-token)))))
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
      ;; (kill-buffer buffer)
      result)))

(defmacro nnfb-direct-output (&rest do)
  `(let ((standard-output nntp-server-buffer))
     (save-excursion
       (set-buffer nntp-server-buffer)
       (erase-buffer))
     ,@do))
(put 'nnfb-direct-output 'lisp-indent-function 0)

(defvar nnfb-current-group-name nil)
(defvar nnfb-current-group-information (make-hash-table :test 'equal)
  "The hash table contains all id:s")
(defvar nnfb-all-group-mappings nil
  "Assoc list with groups and vectors of id:s.

The vector's first element is the first free element of the vector itself.")

(defun nnfb-set-current-group (name)
  "Change the group"
  (if (string= name nnfb-current-group-name)
      nil
    (setq nnfb-current-group-name nil)
    (clrhash nnfb-current-group-information)
    (let* ((a (assoc name nnfb-all-group-mappings))
           (mapping (cdr a)))
      (if mapping
          (let ((i 1))
            (while (< i (aref mapping 0))
              (puthash id i nnfb-current-group-information)
              (setq i (1+ i))))))
    (setq nnfb-current-group-name name)))

(defun nnfb-get-index (id)
  "Get or allocate the gnus number in the group.
This assumes that there is a current group already set."
  (if nnfb-current-group-name
      nil
    (signal 'no-current-group-set))
  (or (gethash "id" nnfb-current-group-information)
      (let* ((a (assoc name nnfb-all-group-mappings))
             (mapping (cdr a))
             pos)
        ;; Create a new empty vector if not already
        (if mapping
            nil
          (setq mapping (make-vector 10 nil))
          (aset mapping 0 1))
        ;; Grow the vector if filled
        (if (aref (1- (length mapping)) mapping)
            (setq mapping (apply (function vector)
                                 (append mapping (make-vector 100 nil)))))
        ;; Add the new element
        (setq pos (aref mapping 0))
        (aset mapping pos id)
        (aset mapping 0 (1+ pos))
        ;; Store the new mapping back
        (if a
            (setcdr a mapping)
          (setq nnfb-all-group-mappings
                (cons (cons a mapping) nnfb-all-group-mappings)))
        ;; Store the id in the hash
        (puthash nnfb-current-group-information id pos)
        pos)))
        

;;; Interface functions
(nnoo-define-basics nnfb)

(defun nnfb-retrieve-headers (&rest rest)
  "Required function"
  (message "nnfb-retrieve-headers %S" rest)
  (signal 'not-implemented-yet nil))

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

(defun nnfb-request-article (&rest rest)
  "Required function"
  (signal 'not-implemented-yet nil))

(defun nnfb-request-group (group &optional server fast info)
  "Required function"
  (message "nnfb-request-group %s %s %S %S" group server fast info)
  (nnfb-set-current-group group)
  (if fast
      nil
    (let* ((token (nnfb-get-token server))
           (id (nnfb-name-to-id group))
           (first-result
            (nnfb-get (concat id
                              "?fields=feed.limit(200)."
                              "fields(comments.fields(id),id)")
                      token)))
      (message "Found %S" result)
      ;; Allocate id:s to local number.
      (mapc
       (function (lambda (l)
                   (nnfb-get-index (cdr (assoc "id" l)))
                   (let ((comments (cdr (assoc "comments" l))))
                     (if comments
                         (mapc (function nnfb-get-index)
                               (cdr (assoc "data" comments)))))))
       (cdr (assoc "data" (cdr (assoc "feed" first-result)))))
      (setq count (aref (cdr (assoc group nnfb-all-group-mappings)) 0))
      (nnfb-direct-output
        (princ (format "210 %d 1 %d %s\n"
                       count count
                       gnus-name)))
      count)))

(defun nnfb-close-group (&rest rest)
  "Required function"
  (signal 'not-implemented-yet nil))

(defun nnfb-friend-to-name (list)
  "Converts an fb friend list to a Gnus name."
  (concat (format "friend.%s." (cdr (assoc "id" list)))
          (mapconcat (function identity)
                     (split-string (cdr (assoc "name" list)))
                     ".")))

(defun nnfb-name-to-id (name)
  "Converts an Gnus group name to an id."
  (string-match "\\(friends\\)\\.\\([0-9][0-9]*\\)\\..*" name)
  (match-string 2))

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
