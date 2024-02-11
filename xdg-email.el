;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; thunar
;; edit -> configure custom action
;; do the obvious
;; the command is: email-test.sh mailto:?attach=%f

;; (jrf/xdg-email-parser "mailto:?attach=/home/jeff/Desktop/21-7-21%20SMF.pdf") ;;;TEST 

(require 'dash)
(require 'mu4e)
(require 'org-msg)
(require 's)

(defun alist-append (key alist new-val &optional test-fn)
  "append NEW-VAL to KEY in ALIST."  
  (if-let ((val (alist-get key alist nil nil test-fn)))
      (setf (alist-get key alist nil nil test-fn)
	    (append (ensure-list (alist-get key alist nil nil test-fn))
		    (ensure-list new-val)))
    (setf (alist-get key alist nil nil #'test-fn)
	  new-val)))

(defcustom xdg-email-new-message-func #'xdg-email-create-org-msg
  "take an alist with 'to 'cc 'bcc 'subject 'body 'attach, etc.
keys and create a new email."

(defun xdg-email-parser (url)
  "parse a mailto url
note this won't parse multiple filenames and the like.

use "
  (let* ((to (cons 'to (--> url
			   (cadr (s-split ":" it))
			   (car (s-split "?" it))
			   (url-unhex-string it))))
	(rest (s-split "&" (cadr (s-split "?" url))))
	(rest (cl-loop for arg in rest
		       collect (cons (intern (url-unhex-string
				      (car (s-split "=" arg))))
				     (url-unhex-string
				      (cadr (s-split "=" arg)))))))
    ;; send it to mu4e
  (funcall xdg-email-new-message-func rest)))

;;; create a new message

(defun jrf/org-msg-add-to-field (field &optional no-complete)
  "FIELD is 'to, 'cc, 'bcc"
  (save-excursion
    (pcase field
      ('to (message-goto-to))
      ('cc (message-goto-cc))
      ('bcc (message-goto-bcc))
      ('subject (message-goto-subject))
      (_ (error
          "Invalid valid for field: %s; must be 'to 'cc 'bcc or 'subject"
          field)))
    (unless (save-excursion
              (backward-char 1)
              (looking-at " "))
      (insert ", "))
    (if no-complete
      (insert no-complete)
      (completion-at-point))))

(defmacro jrf/org-msg-generate-goto-funcs (places)
  `(progn 
     ,@(cl-loop for place in places
                collect `(defun ,(intern
                                  (concat "jrf/org-msg-goto-"
                                          (symbol-name place)))
                             (&optional no-complete)
                           (interactive)
                           (jrf/org-msg-add-to-field ',place no-complete)))))

(jrf/org-msg-generate-goto-funcs (to cc bcc subject))

(defun xdg-email-create-org-msg (args)
  "create a new email using org-msg"
  (interactive)
  (mu4e-compose-new)   
  (when-let ((to (alist-get 'to args)))
      (jrf/org-msg-goto-to to))
  (when-let ((cc (alist-get 'cc args)))
    (jrf/org-msg-goto-cc cc))
  (when-let ((bcc (alist-get 'bcc args)))
    (jrf/org-msg-goto-bcc bcc))
  (when-let ((subject (alist-get 'subject args)))
    (jrf/org-msg-goto-subject subject))
  (when-let ((attach (alist-get 'attach args)))
    (->> attach
	 ;; thunar adds this garbage prefix 
	 (string-remove-prefix "file://") 
	 (org-msg-attach-attach)))
  (jrf/org-msg-goto-to))
    
    
(provide 'xdg-email)













