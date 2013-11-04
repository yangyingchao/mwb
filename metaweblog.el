;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metaWeblog.newPost

(require 'xml-rpc)

(defun mwb-metaweblog-new-post (post publishp)
  (xml-rpc-method-call mwb-server-url
		       "metaWeblog.newPost"
		       "0" ;; Doesn't matter when posting new blogs.
		       mwb-user-name
		       mwb-user-passwd
		       post
		       publishp))

;; metaWeblog.newMediaObject
(defun mwb-metaweblog-new-media-object (media-object);todo: 失败返回nil
  "media-object是一个struct，返回媒体文件的URL"
  (cdr (car (xml-rpc-method-call-media-object mwb-server-url
		       "metaWeblog.newMediaObject"
		       mwb-blog-id
		       mwb-user-name
		       mwb-user-passwd
		       media-object
		       ))))

;; metaWeblog.getRecentPosts
(defun mwb-metaweblog-get-recent-posts (num-of-posts)
  (xml-rpc-method-call mwb-server-url
		       "metaWeblog.getRecentPosts"
		       mwb-blog-id
		       mwb-user-name
		       mwb-user-passwd
		       num-of-posts))


;; metaWeblog.getPost
(defun mwb-metaweblog-get-post (mwb-post-id)
  (xml-rpc-method-call mwb-server-url
		       "metaWeblog.getPost"
		       (if (integerp mwb-post-id)
			   (int-to-string mwb-post-id)
			 mwb-post-id)
		       mwb-user-name
		       mwb-user-passwd))


;; metaWeblog.getCategories
(defun mwb-metaweblog-get-categories ()
  (xml-rpc-method-call mwb-server-url
		       "metaWeblog.getCategories"
		       mwb-blog-id
		       mwb-user-name
		       mwb-user-passwd))


;; metaWeblog.editPost
(defun mwb-metaweblog-edit-post (mwb-post-id post publishp)
  (xml-rpc-method-call mwb-server-url
		       "metaWeblog.editPost"
		       (if (integerp mwb-post-id)
			   (int-to-string mwb-post-id)
			 mwb-post-id)
		       mwb-user-name
		       mwb-user-passwd
		       post
		       publishp))

;; blogger.getUsersBlogs
(defun mwb-metaweblog-get-users-blogs ()
  (xml-rpc-method-call mwb-server-url
		       "blogger.getUsersBlogs"
		       "appkey"
		       mwb-user-name
		       mwb-user-passwd))

;; blogger.deletePost
(defun mwb-metaweblog-delete-post (mwb-post-id publish);我还不明白这个publish有什么作用
  (xml-rpc-method-call mwb-server-url
		       "blogger.deletePost"
		       "appkey"
		       (if (integerp mwb-post-id)
			   (int-to-string mwb-post-id)
			 mwb-post-id)
		       mwb-user-name
		       mwb-user-passwd
		       publish))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xml-rpc-method-call-media-object (server-url method &rest params)
  "Call an XML-RPC method at SERVER-URL named METHOD with PARAMS as \
parameters."
  (let ((response
         (xml-rpc-method-call-async-media-object nil server-url method params)))
    (cond ((stringp response)
           (list (cons nil (concat "URL/HTTP Error: " response))))
          (t
           (xml-rpc-xml-to-response response)))))


(defun xml-rpc-method-call-async-media-object (async-callback-func server-url method
								   &rest params)
  "Call an XML-RPC method asynchronously at SERVER-URL named METHOD with \
PARAMS as parameters. When the method returns, ASYNC-CALLBACK-FUNC will be \
called with the result as parameter."
  (let* ((m-name (if (stringp method)
                     method
                   (symbol-name method)))
         (m-params (mapcar '(lambda (p)
                              `(param nil ,(car (xml-rpc-value-to-xml-list-media-object
                                                 p))))
                           (if async-callback-func
                               params
                             (car-safe params))))
         (m-func-call `((methodCall nil (methodName nil ,m-name)
                                    ,(append '(params nil) m-params)))))
    (when (> xml-rpc-debug 1)
      (print m-func-call (create-file-buffer "func-call")))
    (xml-rpc-request server-url m-func-call async-callback-func)))


(defun xml-rpc-value-to-xml-list-media-object (value)
  "专门为媒体文件写的转化函数"
  (let* ((xmlval (xml-rpc-value-to-xml-list value))
	 (inter-val (nthcdr 2(nth 2 (car xmlval)))))
    `((value nil ,(cons (if (listp (car inter-val))
			   'struct
			 'string)
		       (cons nil
			     (mapcar (lambda (member)
				       (when (listp member)
					 (when (equal (nth 2(nth 2 member)) "bits")
					   (setcar (nth 2 (nth 3 member)) 'base64)))
				       member)
				     inter-val)))))))
(provide 'metaweblog)
