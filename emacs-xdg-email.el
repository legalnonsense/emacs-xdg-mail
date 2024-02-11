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

(defcustom xdg-email-new-message-func #'xdg-email-create-msg
  "take an alist with 'to 'cc 'bcc 'subject 'body 'attach, etc.
keys and create a new email. write a function to use your email
client."

;;; parse mailto links 

(defun alist-append (key alist new-val &optional test-fn)
  "append NEW-VAL to KEY in ALIST."
  ;; this isn't used by I was going to use it to be able to
  ;; attach multiple file names but I don't think I'd ever
  ;; use such a feature because dired. 
  (if-let ((val (alist-get key alist nil nil test-fn)))
      (setf (alist-get key alist nil nil test-fn)
	    (append (ensure-list (alist-get key alist nil nil test-fn))
		    (ensure-list new-val)))
    (setf (alist-get key alist nil nil #'test-fn)
	  new-val)))
  
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

(defun xdg-email-create-msg (args)
  "create a new email using org-msg"
  (interactive)
  (mu4e-compose-new)   
  (when-let ((to (alist-get 'to args)))
    (message-goto-to)
    (insert to))
  (when-let ((cc (alist-get 'cc args)))
    (message-goto-cc)
    (insert cc))
  (when-let ((bcc (alist-get 'bcc args)))
    (message-goto-bcc)
    (insert bcc))
  (when-let ((subject (alist-get 'subject args)))
    (message-goto-subject)
    (insert subject))
  (when-let ((attach (alist-get 'attach args)))
    (->> attach
	 ;; thunar adds this garbage prefix 
	 (string-remove-prefix "file://") 
	 (org-msg-attach-attach)))
  (jrf/org-msg-goto-to))
    
(provide 'xdg-email)













