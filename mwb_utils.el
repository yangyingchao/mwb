;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: mwb_utils.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;;; Time-stamp: <2013-11-06 by Yang,Ying-chao>
;;;
;;;
;;;

(require 'mwb)

(defun mwb-batch-post ()
  "description"
  (interactive)
  (mapc (lambda (x) "DOCSTRING" (interactive)
          (let ((content (with-temp-buffer
                           (insert-file-contents x)
                           (car (read-from-string (buffer-string))))))
            (message "Posting: %s" x)
            (mwb-metaweblog-new-post content  t)
            (delete-file x)
            )
          )
        (directory-files (concat mwb-file-root-path "posts") t
                         (rx (+ digit)))))


(defun mwb-batch-modify (func)
  "Batch update all posts.
`func` is a function which will accept the original content as parameter and
 process it then return the updated content."
  (interactive)
  (mapc (lambda (x) "DOCSTRING" (interactive)
          (let* ((postid (nth 2 x))
                 (postid (if (stringp postid) postid (format "%d" postid)))
                 (path (concat mwb-file-root-path "posts/" postid))
                 (content (if (file-exists-p path)
                              (funcall func
                                       (with-temp-buffer
                                         (insert-file-contents path)
                                         (car (read-from-string (buffer-string)))))
                            nil)))
            (if content
                (mwb-metaweblog-edit-post postid content t))))
        mwb-entry-list))


(defun remove-author (content)
  "description"
  (interactive)
  (let ((string (cdadr content))
        (bf ))
    (with-temp-buffer
      (get-buffer-create "AA")
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (while (search-forward "Author: <a href=\"http://www.cnblogs.com/yangyingchao\">Yang.Yang-chao</a>, " nil t)
        (replace-match "" nil t))
      (setq string (buffer-substring-no-properties (point-min) (point-max)))
      )
    (setf (cdadr content ) string)
    content))
;; (mwb-batch-modify 'remove-author)
(provide 'mwb_utils)
;;;;; mwb_utils.el ends here
