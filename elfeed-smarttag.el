(require 'cl)
(require 'stem)

(defvar *punctuation-re* "[,.;:()]")

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

(defun text-to-bow (text)
  "Converts a text to bag-of-words representation"
  (count-unique
   (mapcar (lambda (word) (car (stem-english word)))
           (split-string (normalize-text text)))))

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
  ;; (insert (elfeed-feed-id feed))
  ;; (message (elfeed-feed-id feed))
  (message (elfeed-entry-title entry))
  (insert (pp (elfeed-smarttag-entry-bow entry))))

(with-temp-file "/tmp/elfeed-smarttag.txt" 
  (with-elfeed-db-visit (entry feed)
    (if (string= (elfeed-feed-id feed) "http://planet.haskell.org/rss20.xml")
        (elfeed-smarttag-process-entry entry feed))
    ;; (elfeed-db-return)
    ))

