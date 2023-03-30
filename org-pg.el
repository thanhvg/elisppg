(defun get-first-level-headings ()
  "Return a list of first level headings in the current Org mode buffer."
  (let (headings)
    (org-map-entries
     (lambda ()
       (when (= (org-outline-level) 1)
         (push (org-get-heading t t) headings)))
     nil 'tree)
    headings))


(defun get-node-body ()
  "Return the body of the current Org mode node."
  (let ((element (org-element-at-point)))
    (when (eq (org-element-type element) 'headline)
      (org-element-map element 'paragraph
        (lambda (paragraph)
          (org-element-interpret-data (org-element-property :value paragraph)))
        nil t))))

(setq thanh-org-data
      (with-temp-buffer
        (insert-file-contents "./chat.org")
        (org-element-parse-buffer)))

(length (org-element-map
            thanh-org-data
            'headline
          (lambda (hl)
            (and (= (org-element-property :level hl) 1)
                 (org-element-property :begin hl)
                 hl))))

;; get title and body of heading
(org-element-map
    (car (org-element-map
             thanh-org-data
             'headline
           (lambda (hl)
             (and (= (org-element-property :level hl) 1)
                  (org-element-property :begin hl)
                  hl))))
    'section
  (lambda (hl)
    (and (= (org-element-property :level hl) 1 )
         (org-element-property :begin hl)
         hl))
  nil t)

(org-element-property :raw-value
                      (car (org-element-map
                               thanh-org-data
                               'headline
                             (lambda (hl)
                               (and (= (org-element-property :level hl) 1)
                                    (org-element-property :begin hl)
                                    hl)))))

(org-element-map (car (org-element-map
                          thanh-org-data
                          'headline
                        (lambda (hl)
                          (and (= (org-element-property :level hl) 1)
                               (org-element-property :begin hl)
                               hl))))
    'paragraph

  (lambda (p) p))

(length (cdr (cdr thanh-org-data)))

;; headline list
(nthcdr 2 thanh-org-data)

(org-element-property :raw-value (car (cdr (cdr thanh-org-data))))

;; head line
(org-element-property :raw-value (car (cdr (cdr thanh-org-data))))


;; section
(nth 2 (car (cdr (cdr thanh-org-data))))

;; section has paragraph list
;; paragraph list
(nthcdr 2 (nth 2 (car (cdr (cdr thanh-org-data)))))

;; grab all paragraphs and combine them to string
(mapconcat (lambda (it) (substring-no-properties (nth 2 it)))
           (nthcdr 2 (nth 2 (car (cdr (cdr thanh-org-data)))))
           "\n")

;; after section comes anwser which is a head line
(nth 3 (car (cdr (cdr thanh-org-data))))


;; (org-data (properties list) data ...) 
;; 2 is the accessor number to data nth 2 or nthcdr 2

;; we skip this headline go to the section and
(nth 2 (nth 3 (car (cdr (cdr thanh-org-data)))))

;; again get all string in paragraph of section
(mapconcat (lambda (it) (substring-no-properties (nth 2 it)))
           (nthcdr 2  (nth 2 (nth 3 (car (cdr (cdr thanh-org-data))))))
           "\n")

;; the second headline
(nth 1 (nthcdr 2 thanh-org-data))


(org-element-property :raw-value (nth 1 (nthcdr 2 thanh-org-data)))

;; session
(nth 2 (nth 1 (nthcdr 2 thanh-org-data)))


;; again get all string in paragraph of section
(mapconcat (lambda (it) (substring-no-properties (nth 2 it)))
           (nthcdr 2  (nth 2 (nth 1 (nthcdr 2 thanh-org-data))))
           "\n")
;; answer
(nth 3 (nth 1 (nthcdr 2 thanh-org-data)))

;; section
;;
(nth 2 (nth 3 (nth 1 (nthcdr 2 thanh-org-data))))


(mapconcat (lambda (it) (cond ((eq 'paragraph (car it))
                               (substring-no-properties (nth 2 it)))
                              (t (org-element-property :value it))))
           (nthcdr 2 (nth 2 (nth 3 (nth 1 (nthcdr 2 thanh-org-data)))))
           "\n")

;; 
