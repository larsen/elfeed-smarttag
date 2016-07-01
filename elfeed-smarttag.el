(require 'cl)
(require 'cl-lib) ;; FIXME which one is useless?
(require 'stem)
(require 'elfeed)

(defvar *punctuation-re* "[,.;:()]")

(defvar *elfeed-smarttag-directory* "~/.elfeed-smarttag/")
(defvar *common-words* (expand-file-name "commonwords" *elfeed-smarttag-directory*))
(defvar *english-common-words* "~/Dropbox/Projects/dev/google-10000-english/google-10000-english.txt")

(defvar elfeed-smarttag-common-words-db nil)

(defun elfeed-smarttag--init-common-words-db ()
  (setf elfeed-smarttag-common-words-db (get-common-words *english-common-words*)))

(defun get-common-words (filepath)
  (let ((common-words-hash (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents filepath)
      (loop for word in (split-string (buffer-string) "\n" t)
            do (puthash (car (stem-english word)) 1 common-words-hash)
            finally return common-words-hash))))

(defun html-dom (html-content)
  (with-temp-buffer
    (insert html-content)
    (libxml-parse-html-region (point-min) (point-max))))

(defun get-text-from-html (html-content)
  (with-temp-buffer
    (shr-insert-document
     (html-dom html-content))
    (buffer-string)))

(defun get-text-from-text (text-content)
  text-content)

(defun get-entry-text (entry-content entry-content-type)
  (if (eq entry-content-type 'html)
      (get-text-from-html entry-content)
    (get-text-from-text entry-content)))

(defun normalize-text (text)
  "Normalize chunk of text: no punctuation symbols, all lowercase"
  (downcase (replace-regexp-in-string *punctuation-re* "" text)))

;; Adapted from 
;; http://stackoverflow.com/questions/6050033/elegant-way-to-count-items
(defun count-unique (list)
  (loop
     with hash = (make-hash-table :test #'equal)
     for item in list
     do (incf (gethash item hash 0))
     finally (return
               (loop for key being each hash-key of hash
                  using (hash-value value)
                  collect (cons key value)))))

(defun is-common-word? (word)
  (gethash word elfeed-smarttag-common-words-db))

(defun text-to-bow (text)
  "Converts a text to bag-of-words representation"
  (count-unique
   (remove-if-not (lambda (word) (is-common-word? word))
                  (mapcar (lambda (word) (car (stem-english word)))
                          (split-string (normalize-text text))))))

(defun elfeed-smarttag-entry-bow (entry)
  "Converts a elfeed entry into a bag-of-words"
  (let* ((entry-title (elfeed-entry-title entry))
         (entry-content-type (elfeed-entry-content-type entry))
         (entry-content-text (get-entry-text
                              (elfeed-deref (elfeed-entry-content entry))
                              entry-content-type)))
    (if (not (null entry-content-text))
        (text-to-bow entry-content-text))))

(defun elfeed-smarttag-process-entry (entry feed)
  (let ((title (elfeed-entry-title entry)))
    (message title)
    (insert
     (format "%s\t%d\n" title
             (length (elfeed-smarttag-entry-bow entry))))))

(defun elfeed-smartag-processable? (entry feed)
  (let ((tags (elfeed-entry-tags entry)))
    (member 'interesting tags)))

(with-temp-file "/tmp/elfeed-smarttag.txt"
  (elfeed-smarttag--init-common-words-db)
  (with-elfeed-db-visit (entry feed)
    (when (elfeed-smartag-processable? entry feed)
      (elfeed-smarttag-process-entry entry feed))))

(provide 'elfeed-smarttag)
