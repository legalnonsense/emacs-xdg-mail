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

(defcustom xdg-email-new-message-func #'compose-mail
  "Function to open new message buffer.")

(defcustom xdg-email-attach-function #'org-msg-attach-attach
  "function to attach a file to the current message buffer.  I use `org-msg'.
Another possibility is `mml-attach-file'.")

;;; parse mailto links 

(defun xdg-email-parser (url)
  "parse a mailto url
note this won't hanlde multple file names.
()
It will return an alist based on the keys and values
in the mailto url" 
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
    ;; (append (list to) rest)))
    (xdg-email-create-msg (append (list to) rest))))

;;; create a new message 

(defun xdg-email-create-msg (args)
  "create a new email using org-msg"
  (interactive)
  (funcall xdg-email-new-message-func)
  (save-excursion 
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
	 (funcall xdg-email-attach-function)))))

    
(provide 'xdg-email)













