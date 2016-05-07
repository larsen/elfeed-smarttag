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

(defun elfeed-smarttag-entry-bow (entry)
  (let* ((entry-title (elfeed-entry-title entry))
         (entry-content-type (elfeed-entry-content-type entry))
         (entry-content-text (get-entry-text
                              (elfeed-deref (elfeed-entry-content entry))
                              entry-content-type)))
    (if (not (null entry-content-text))
        (insert (format
                 "%s - %s[...]\n"
                 entry-title
                 (substring
                  entry-content-text 0 (min 40 (length entry-content-text))))))))

(defun elfeed-smarttag-process-entry (entry feed)
  (insert (elfeed-feed-id feed))
  (elfeed-smarttag-entry-bow entry))

(with-elfeed-db-visit (entry feed)
  (elfeed-smarttag-process-entry entry feed)
  ;; (elfeed-db-return)
  )

