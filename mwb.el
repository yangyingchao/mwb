;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mwb.el --- Simple description
;;
;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'metaweblog)
(require 'cl)

(defgroup mwb nil
  "博客园客户端分组"
  :group 'emacs)

(defcustom mwb-server-url nil
  "MetaWeblog访问地址"
  :group 'mwb
  :type 'string)
(defcustom mwb-blog-id nil
  "博客ID"
  :group 'mwb
  :type 'string)
(defcustom mwb-user-name nil
  "登录用户名"
  :group 'mwb
  :type 'string)
(defvar mwb-user-passwd nil "Password")

(defcustom mwb-media-object-suffix-list '("jpg" "jpeg" "png" "gif" "mp4" "zip" "gz" "bz2")
  "希望处理的媒体文件类型"
  :group 'mwb
  :type 'list)
(defcustom mwb-src-file-extension-list '("org" "html")
  "希望处理的媒体文件类型"
  :group 'mwb
  :type 'list)
(defcustom mwb-template-head
  "#TITLE:    \n#KEYWORDS: \n#DATE:    \n"
  "博客头模板"
  :group 'mwb
  :type 'list)
(defcustom mwb-file-root-path "~/Documents/MetaWebBlog/"
  "数据文件的根目录"
  :group 'mwb
  :type 'string)

(defconst mwb-mime-types-mapping
  '(("3dm" "x-world/x-3dmf")
    ("3dmf" "x-world/x-3dmf")
    ("abc" "text/vnd.abc")
    ("acgi" "text/html")
    ("afl" "video/animaflex")
    ("aif" "audio/aiff")
    ("aif" "audio/x-aiff")
    ("aifc" "audio/aiff")
    ("aifc" "audio/x-aiff")
    ("aiff" "audio/aiff")
    ("aiff" "audio/x-aiff")
    ("aip" "text/x-audiosoft-intra")
    ("art" "image/x-jg")
    ("asf" "video/x-ms-asf")
    ("asm" "text/x-asm")
    ("asp" "text/asp")
    ("asx" "video/x-ms-asf")
    ("asx" "video/x-ms-asf-plugin")
    ("au" "audio/basic")
    ("au" "audio/x-au")
    ("avi" "video/avi")
    ("avi" "video/msvideo")
    ("avi" "video/x-msvideo")
    ("avs" "video/avs-video")
    ("bm" "image/bmp")
    ("bmp" "image/bmp")
    ("bmp" "image/x-windows-bmp")
    ("c" "text/plain")
    ("c" "text/x-c")
    ("c++" "text/plain")
    ("cc" "text/plain")
    ("cc" "text/x-c")
    ("com" "text/plain")
    ("conf" "text/plain")
    ("cpp" "text/x-c")
    ("csh" "text/x-script.csh")
    ("css" "text/css")
    ("cxx" "text/plain")
    ("def" "text/plain")
    ("dif" "video/x-dv")
    ("dl" "video/dl")
    ("dl" "video/x-dl")
    ("dot" "text/plain")
    ("dv" "video/x-dv")
    ("dwf" "drawing/x-dwf (old)")
    ("dwf" "model/vnd.dwf")
    ("dwg" "image/vnd.dwg")
    ("dwg" "image/x-dwg")
    ("dxf" "image/vnd.dwg")
    ("dxf" "image/x-dwg")
    ("el" "text/x-script.elisp")
    ("etx" "text/x-setext")
    ("f" "text/plain")
    ("f" "text/x-fortran")
    ("f77" "text/x-fortran")
    ("f90" "text/plain")
    ("f90" "text/x-fortran")
    ("fif" "image/fif")
    ("fli" "video/fli")
    ("fli" "video/x-fli")
    ("flo" "image/florian")
    ("flx" "text/vnd.fmi.flexstor")
    ("fmf" "video/x-atomic3d-feature")
    ("for" "text/plain")
    ("for" "text/x-fortran")
    ("fpx" "image/vnd.fpx")
    ("fpx" "image/vnd.net-fpx")
    ("funk" "audio/make")
    ("g" "text/plain")
    ("g3" "image/g3fax")
    ("gif" "image/gif")
    ("gl" "video/gl")
    ("gl" "video/x-gl")
    ("gsd" "audio/x-gsm")
    ("gsm" "audio/x-gsm")
    ("gzip" "multipart/x-gzip")
    ("h" "text/plain")
    ("h" "text/x-h")
    ("hh" "text/plain")
    ("hh" "text/x-h")
    ("hlb" "text/x-script")
    ("htc" "text/x-component")
    ("htm" "text/html")
    ("html" "text/html")
    ("htmls" "text/html")
    ("htt" "text/webviewhtml")
    ("htx" "text/html")
    ("ice" "x-conference/x-cooltalk")
    ("ico" "image/x-icon")
    ("idc" "text/plain")
    ("ief" "image/ief")
    ("iefs" "image/ief")
    ("iges" "model/iges")
    ("igs" "model/iges")
    ("isu" "video/x-isvideo")
    ("it" "audio/it")
    ("ivr" "i-world/i-vrml")
    ("jam" "audio/x-jam")
    ("jav" "text/plain")
    ("jav" "text/x-java-source")
    ("java" "text/plain")
    ("java" "text/x-java-source")
    ("jfif" "image/jpeg")
    ("jfif" "image/pjpeg")
    ("jfif-tbnl" "image/jpeg")
    ("jpe" "image/jpeg")
    ("jpe" "image/pjpeg")
    ("jpeg" "image/jpeg")
    ("jpeg" "image/pjpeg")
    ("jpg" "image/jpeg")
    ("jpg" "image/pjpeg")
    ("jps" "image/x-jps")
    ("jut" "image/jutvision")
    ("kar" "audio/midi")
    ("kar" "music/x-karaoke")
    ("ksh" "text/x-script.ksh")
    ("la" "audio/nspaudio")
    ("la" "audio/x-nspaudio")
    ("lam" "audio/x-liveaudio")
    ("list" "text/plain")
    ("lma" "audio/nspaudio")
    ("lma" "audio/x-nspaudio")
    ("log" "text/plain")
    ("lsp" "text/x-script.lisp")
    ("lst" "text/plain")
    ("lsx" "text/x-la-asf")
    ("m" "text/plain")
    ("m" "text/x-m")
    ("m1v" "video/mpeg")
    ("m2a" "audio/mpeg")
    ("m2v" "video/mpeg")
    ("m3u" "audio/x-mpequrl")
    ("mar" "text/plain")
    ("mcf" "image/vasa")
    ("mcf" "text/mcf")
    ("mht" "message/rfc822")
    ("mhtml" "message/rfc822")
    ("mid" "audio/midi")
    ("mid" "audio/x-mid")
    ("mid" "audio/x-midi")
    ("mid" "music/crescendo")
    ("mid" "x-music/x-midi")
    ("midi" "audio/midi")
    ("midi" "audio/x-mid")
    ("midi" "audio/x-midi")
    ("midi" "music/crescendo")
    ("midi" "x-music/x-midi")
    ("mime" "message/rfc822")
    ("mime" "www/mime")
    ("mjf" "audio/x-vnd.audioexplosion.mjuicemediafile")
    ("mjpg" "video/x-motion-jpeg")
    ("mod" "audio/mod")
    ("mod" "audio/x-mod")
    ("moov" "video/quicktime")
    ("mov" "video/quicktime")
    ("movie" "video/x-sgi-movie")
    ("mp2" "audio/mpeg")
    ("mp2" "audio/x-mpeg")
    ("mp2" "video/mpeg")
    ("mp2" "video/x-mpeg")
    ("mp2" "video/x-mpeq2a")
    ("mp3" "audio/mpeg3")
    ("mp3" "audio/x-mpeg-3")
    ("mp3" "video/mpeg")
    ("mp3" "video/x-mpeg")
    ("mpa" "audio/mpeg")
    ("mpa" "video/mpeg")
    ("mpe" "video/mpeg")
    ("mpeg" "video/mpeg")
    ("mpg" "audio/mpeg")
    ("mpg" "video/mpeg")
    ("mpga" "audio/mpeg")
    ("mv" "video/x-sgi-movie")
    ("my" "audio/make")
    ("nap" "image/naplps")
    ("naplps" "image/naplps")
    ("nif" "image/x-niff")
    ("niff" "image/x-niff")
    ("org" "text/plain")
    ("p" "text/x-pascal")
    ("pas" "text/pascal")
    ("pbm" "image/x-portable-bitmap")
    ("pct" "image/x-pict")
    ("pcx" "image/x-pcx")
    ("pdb" "chemical/x-pdb")
    ("pfunk" "audio/make")
    ("pfunk" "audio/make.my.funk")
    ("pgm" "image/x-portable-graymap")
    ("pgm" "image/x-portable-greymap")
    ("pic" "image/pict")
    ("pict" "image/pict")
    ("pl" "text/plain")
    ("pl" "text/x-script.perl")
    ("pm" "image/x-xpixmap")
    ("pm" "text/x-script.perl-module")
    ("png" "image/png")
    ("pnm" "image/x-portable-anymap")
    ("pov" "model/x-pov")
    ("ppm" "image/x-portable-pixmap")
    ("pvu" "paleovu/x-pv")
    ("py" "text/x-script.phyton")
    ("qcp" "audio/vnd.qcelp")
    ("qd3" "x-world/x-3dmf")
    ("qd3d" "x-world/x-3dmf")
    ("qif" "image/x-quicktime")
    ("qt" "video/quicktime")
    ("qtc" "video/x-qtc")
    ("qti" "image/x-quicktime")
    ("qtif" "image/x-quicktime")
    ("ra" "audio/x-pn-realaudio")
    ("ra" "audio/x-pn-realaudio-plugin")
    ("ra" "audio/x-realaudio")
    ("ram" "audio/x-pn-realaudio")
    ("ras" "image/cmu-raster")
    ("ras" "image/x-cmu-raster")
    ("rast" "image/cmu-raster")
    ("rexx" "text/x-script.rexx")
    ("rf" "image/vnd.rn-realflash")
    ("rgb" "image/x-rgb")
    ("rm" "audio/x-pn-realaudio")
    ("rmi" "audio/mid")
    ("rmm" "audio/x-pn-realaudio")
    ("rmp" "audio/x-pn-realaudio")
    ("rmp" "audio/x-pn-realaudio-plugin")
    ("rp" "image/vnd.rn-realpix")
    ("rpm" "audio/x-pn-realaudio-plugin")
    ("rt" "text/richtext")
    ("rt" "text/vnd.rn-realtext")
    ("rtf" "text/richtext")
    ("rtx" "text/richtext")
    ("rv" "video/vnd.rn-realvideo")
    ("s" "text/x-asm")
    ("s3m" "audio/s3m")
    ("scm" "text/x-script.guile")
    ("scm" "text/x-script.scheme")
    ("scm" "video/x-scm")
    ("sdml" "text/plain")
    ("sgm" "text/sgml")
    ("sgm" "text/x-sgml")
    ("sgml" "text/sgml")
    ("sgml" "text/x-sgml")
    ("sh" "text/x-script.sh")
    ("shtml" "text/html")
    ("shtml" "text/x-server-parsed-html")
    ("sid" "audio/x-psid")
    ("snd" "audio/basic")
    ("snd" "audio/x-adpcm")
    ("spc" "text/x-speech")
    ("ssi" "text/x-server-parsed-html")
    ("svf" "image/vnd.dwg")
    ("svf" "image/x-dwg")
    ("svr" "x-world/x-svr")
    ("talk" "text/x-speech")
    ("tcl" "text/x-script.tcl")
    ("tcsh" "text/x-script.tcsh")
    ("text" "text/plain")
    ("tif" "image/tiff")
    ("tif" "image/x-tiff")
    ("tiff" "image/tiff")
    ("tiff" "image/x-tiff")
    ("tsi" "audio/tsp-audio")
    ("tsp" "audio/tsplayer")
    ("tsv" "text/tab-separated-values")
    ("turbot" "image/florian")
    ("txt" "text/plain")
    ("uil" "text/x-uil")
    ("uni" "text/uri-list")
    ("unis" "text/uri-list")
    ("uri" "text/uri-list")
    ("uris" "text/uri-list")
    ("ustar" "multipart/x-ustar")
    ("uu" "text/x-uuencode")
    ("uue" "text/x-uuencode")
    ("vcs" "text/x-vcalendar")
    ("vdo" "video/vdo")
    ("viv" "video/vivo")
    ("viv" "video/vnd.vivo")
    ("vivo" "video/vivo")
    ("vivo" "video/vnd.vivo")
    ("voc" "audio/voc")
    ("voc" "audio/x-voc")
    ("vos" "video/vosaic")
    ("vox" "audio/voxware")
    ("vqe" "audio/x-twinvq-plugin")
    ("vqf" "audio/x-twinvq")
    ("vql" "audio/x-twinvq-plugin")
    ("vrml" "model/vrml")
    ("vrml" "x-world/x-vrml")
    ("vrt" "x-world/x-vrt")
    ("wav" "audio/wav")
    ("wav" "audio/x-wav")
    ("wbmp" "image/vnd.wap.wbmp")
    ("wmf" "windows/metafile")
    ("wml" "text/vnd.wap.wml")
    ("wmls" "text/vnd.wap.wmlscript")
    ("wrl" "model/vrml")
    ("wrl" "x-world/x-vrml")
    ("wrz" "model/vrml")
    ("wrz" "x-world/x-vrml")
    ("wsc" "text/scriplet")
    ("xbm" "image/x-xbitmap")
    ("xbm" "image/x-xbm")
    ("xbm" "image/xbm")
    ("xdr" "video/x-amt-demorun")
    ("xgz" "xgl/drawing")
    ("xif" "image/vnd.xiff")
    ("xm" "audio/xm")
    ("xml" "text/xml")
    ("xmz" "xgl/movie")
    ("xpm" "image/x-xpixmap")
    ("xpm" "image/xpm")
    ("x-png" "image/png")
    ("xsr" "video/x-amt-showrun")
    ("xwd" "image/x-xwd")
    ("xwd" "image/x-xwindowdump")
    ("xyz" "chemical/x-pdb")
    ("zip" "multipart/x-zip")
    ("zsh" "text/x-script.zsh")
    )
  "Mapping of file extension and mime types")

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mwb-posts-in-category nil
  "分类之后的博文，这是显示在Mwb-Manager缓冲区里的主要内容")

(defvar mwb-tag-list nil "Tags of blogs.")

(defvar mwb-file-post-path nil
  "博文内容文件根目录，其中的博文内容文件以博文ｉｄ命名")
(defvar mwb-category-list nil
  "博文分类列表")

(defvar mwb-blog-info nil
  "博客信息")
(defvar mwb-entry-list nil
  "本地博客列表")
(defvar mwb-category-list nil
  "分类列表")
(defvar mwb-post-list-window nil
  "博文列表窗口")

(defvar mwb-customize-checks nil
  "A list of checks defined by user to check if a buffer can be post publicly,
The check functions should take filepath as parameter and return t if it can be posted.
Checks will stop whenever one function returns nil.
You can call `add-to-list` to modify this list, for example, following code will only allow
org files to be published:
(add-to-list 'mwb-customize-checks (lambda(x) (string= (file-name-extension x) \"org\")))")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Menu;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mwb-mode-map
  (make-sparse-keymap "Mwb")
  "mwb博客客户端菜单")

(define-key mwb-mode-map [tags-getUsersBlogs]
  '(menu-item "User information" mwb-get-users-blogs
              :help "获取用户的博客信息"))

(define-key mwb-mode-map [tags-getRecentPosts]
  '(menu-item "Get recent posts" mwb-get-recent-posts
              :help "获取最近发布的N篇博客"))

(define-key mwb-mode-map [tags-getCategories]
  '(menu-item "Get(Update) categories" mwb-get-categories
              :help "获取并更新本地博客分类"))

(define-key mwb-mode-map [tags-getPost]
  '(menu-item "Get post" mwb-get-post
              :help "获取并更新本地指定的博客"))
(define-key mwb-mode-map [separator-mwb-tags]
  '(menu-item "--"))

(define-key mwb-mode-map [tags-editPost]
  '(menu-item "Update post" mwb-edit-post
              :help "更新已发布的博客"))

(define-key mwb-mode-map [tags-deletePost]
  '(menu-item "Delete post" mwb-delete-post
              :help "将当前缓冲区对应的博客删除"))

(define-key mwb-mode-map [tags-saveDraft]
  '(menu-item "Save draft" mwb-save-draft
              :help "将草稿保存到服务器，但状态为“未发布”"))

(define-key mwb-mode-map [tags-newPost]
  '(menu-item "Publish post" mwb-new-post
              :help "发布当前缓冲区"))

(define-key mwb-mode-map [C-S-mouse-1]
  mwb-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;KeyMap;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key mwb-mode-map (kbd "\C-c c p") 'mwb-new-post)
(define-key mwb-mode-map (kbd "\C-c c s") 'mwb-save-draft)
(define-key mwb-mode-map (kbd "\C-c c d") 'mwb-delete-post)
(define-key mwb-mode-map (kbd "\C-c c e") 'mwb-edit-post)
(define-key mwb-mode-map (kbd "\C-c c g") 'mwb-get-post)
(define-key mwb-mode-map (kbd "\C-c c c") 'mwb-get-categories)
(define-key mwb-mode-map (kbd "\C-c c r") 'mwb-get-recent-posts)
(define-key mwb-mode-map (kbd "\C-c c u") 'mwb-get-users-blogs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LoadData;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun mwb-request-password ()
  "Request password"
  (interactive)
  (if (not mwb-user-passwd)
      (setq mwb-user-passwd
            (read-passwd "Your password:" nil))))


(defun mwb-save-posted (postid content)
  "Save posted contents"
  (let* ((postid (if (numberp postid) (format "%d" postid) postid))
         (path (concat mwb-file-post-path postid)))
    (if (file-exists-p path)
        (delete-file path))
    (save-excursion
      (with-temp-file path
        (print content (current-buffer))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;底层函数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mwb-check-legal-for-publish (src-file)
  "Check if file is legal to publish. Return values:
   'PUBLISHED: if already published.
   'OK: OK to public.
   'FORBID: some policies forbid this file to be posted. This is controlled by
mwb-customize-checks, which is completely up to user."
  (if (equal (mwb-check-src-file-state src-file)
             "PUBLISHED")
      'PUBLISHED
    (if mwb-customize-checks
        (let ((i 0)
              (total (length mwb-customize-checks))
              (res t))
          (while (and res (< i total))
            (setq res (funcall (nth i mwb-customize-checks) src-file)
                  i (1+ i)))
          (if (not res) 'FORBID 'OK))
      'OK)))

(defun mwb-check-legal-for-delete (src-file)
  "检查文件是否可以删除相应的博文"
  (and
   (if (member (file-name-extension src-file)
	       mwb-src-file-extension-list)
       t
     (progn
       (message "Failed: UNSUPPORTED file!")
       nil))
   (if (equal (mwb-check-src-file-state (buffer-file-name))
	      "PUBLISHED")
       t
     (progn
       (message "This post has not been published, so you cann't delete it!")
       nil))))

(defun mwb-check-legal-for-edit (src-file)
  "检查文件是否可以更新"
  (and
   (if (member (file-name-extension src-file)
	       mwb-src-file-extension-list)
       t
     (progn
       (message "Failed: UNSUPPORTED file!")
       nil))

   (if (equal (mwb-check-src-file-state (buffer-file-name))
	      "PUBLISHED")
       t
     (progn
       (message "This post has not been published, you can't update it. You can publish it using M-x mwb-new-post")
       nil))))

(defun mwb-check-src-file-state (src-file)
  (let ((state nil))
    (mapc (lambda (entry)
	    (if (equal src-file (nth 4 entry))
		(setq state (nth 5 entry))))
	  mwb-entry-list)
    state))

(defun mwb-gen-id ()
  "给entry产生一个id，从１开始"
  (let ((id 0)
	(flag t))
    (while flag
      (progn
	(setq flag nil id (1+ id))

	(mapc (lambda (entry)
		(and (equal id
			    (car entry))
		     (setq flag t)))
	      mwb-entry-list)))
    id))


(defun mwb-push-post-to-entry-list (post)
  "将博文保存到mwb-entry-list变量中。但并不立即保存到文件中"

  (let ((title (cdr (assoc "title" post)))
        (postid (cdr (assoc "postid" post)))
        (categories (cdr (assoc "categories" post)))
        (done nil)
        (index 0))
    (progn
      (if (integerp postid)
          (setq postid (int-to-string postid)))

      ;;保存博文
      (with-temp-file (concat mwb-file-post-path postid)
        (print post (current-buffer)))

      ;;如果有相同标题的博文项，则提示是否合并到同一项中去，如果有已经存在多个相同的项，对每个都询问，直到回答是或者完
      (mapc (lambda (entry)
              (progn
                (or done
                    (not (equal title (nth 1 entry)))
                    (not (y-or-n-p (format "merge the post %s with entry %S" postid entry)))
                                        ;下面是将该博文合并到该项中
                    (progn
                      (setq done t)
                      (setcar (nthcdr index mwb-entry-list)
                                        ;id
                              (list (nth 0 entry)
                                        ;title
                                    title
                                        ;postid
                                    postid
                                        ;categories
                                    categories
                                        ;src-file
                                    (nth 4 entry)
                                        ;state
                                    "PUBLISHED"))))
                (setq index (1+ index))))
            mwb-entry-list)

                                        ;还没有插入则新建项
      (or done
          (push
                                        ;id
           (list (mwb-gen-id)
                                        ;title
                 title
                                        ;postid
                 postid
                                        ;categories
                 categories
                                        ;src-file
                 nil
                                        ;state
                 "PUBLISHED")

           mwb-entry-list)))))

(defun mwb-push-src-file-to-entry-list (src-file)
  "将一个源文件加入到博文项中，但并不立即保存博文项到文件中。"
  (if (mwb-check-file-in-entry-list src-file)
      t
    (let ((title
	   (with-temp-buffer
	     (insert-file-contents src-file)
	     (mwb-fetch-fields "TITLE")))
	  (done nil)
	  (index 0))
      (progn (mapc (lambda (entry)
		     (progn
		       (or done
			   (not title)
			   (not (equal title (nth 1 entry)))
			   (not (y-or-n-p (format "merge the file %s with entry %S" src-file entry)))
					;下面是将该文件合并到该项中
			   (progn
			     (setq done t)
			     (setcar (nthcdr 4 (nth index mwb-entry-list))
				     src-file)))
		       (setq index (1+ index))))
		   mwb-entry-list)

					;还没有插入则新建项
	     (or done
		 (push
					;id
		  (list (mwb-gen-id)
					;title
			title
					;postid
			nil
					;categories
			nil
					;src-file
			src-file
					;state
			"UNPUBLISHED")

		  mwb-entry-list))))))

(defun mwb-assign-post-to-file (post src-file)
  "将post合并到一个指定源文件的列表项中，成功返回t，不立即保存列表项"
  (condition-case()
      (progn
        (setq mwb-entry-list
              (mapcar (lambda (entry)
                        (if (equal src-file
                                   (nth 4 entry))
                            (list (nth 0 entry) ;id
                                  (cdr (assoc "title" post)) ;title
                                  (cdr (assoc "postid" post)) ;postid
                                  (cdr (assoc "categories" post)) ;categories
                                  src-file ;src-file
                                  "PUBLISHED")
                          entry))
                      mwb-entry-list))
        t)
    (error nil)))


(defun mwb-categories-string-to-list (categories-string)
  "将分类字符串按空白符分成字符串列表"
  (if (or (eq categories-string nil)
          (eq categories-string ""))
      nil
    (let ((idx1
           (string-match "[^　 \t]+"    ;圆角半角空格
                         categories-string)))
      (if (not idx1)
          nil
        (setq categories-string         ;圆角半角空格
              (substring categories-string idx1))
        (let ((idx2
               (string-match "[　 \t]+"
                             categories-string)))
          (if idx2
              (cons (substring categories-string
                               0
                               idx2)
                    (mwb-categories-string-to-list
                     (substring categories-string idx2)))
            (cons categories-string
                  nil)))))))


(defun mwb-fetch-fields (fmt &optional nf)
  "Fetch fields matched with fmt.
nf: number of fields in fmt."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (num (if nf nf 1))
        result)
    (if (string-match fmt content)
        (dotimes (x num)
          (setq result (cons (match-string num content) result))))
    result))

(defun mwb-concat-string-array (array &optional sep)
  "Concat string array"
  (when (listp array)
    (mapconcat (lambda (x) (identity x)) array sep)))


(defun mwb-make-media-object-file-data (media-path) ;todo: type可能要详细分类
  "根据给出的文件路径返回相应的FileData，文件不存在返回nil"
  (if (file-exists-p media-path)
      (list
       ;;media-path name
       (cons "name"
             (file-name-nondirectory media-path))

       ;; bits
       (cons "bits"
             (base64-encode-string
              (with-temp-buffer
                (insert-file-contents-literally media-path)
                (buffer-string))))
       (cons "type"
             (or (cadr (assoc (file-name-extension media-path) mwb-mime-types-mapping))
                 "image/jpg")))
    nil))


(defmacro mwb-mkfield-1 (x)
  `(rx bol ,x (* space) (group (+? nonl) eol)))

(defun mwb-b2p-org ()
  (delq nil(list
            ;; title
            (cons "title"
                  (or (car (mwb-fetch-fields (mwb-mkfield-1  "#+TITLE:")))
                      "Unamed"))
            ;; excerpt
            (cons "mt_excerpt"
                  (or (car (mwb-fetch-fields (mwb-mkfield-1 "#+DESCRIPTION:")))
                      ""))

            ;; categories
            (cons "categories"
                  (let ((categories-list
                         (mwb-categories-string-to-list
                          (car (mwb-fetch-fields (mwb-mkfield-1 "#+CATEGORIES:"))))))
                    (or
                     categories-list
                     '("未分类"))))
            ;; tags
            (cons "mt_keywords"
                  (or
                   (car (mwb-fetch-fields (mwb-mkfield-1 "#+KEYWORDS:")))
                   ""))

            ;; dateCreated
            (cons "dateCreated"
                  (list
                   :datetime
                   (let ((ctime (current-time)))
                     (cons (car ctime) (cadr ctime)))))

            ;; description
            (cons "description"
                  (with-current-buffer (org-export-to-buffer 'html "*Org HTML Export*")
                    (let ((buf-str
                           (mwb-replace-media-object-location
                            (buffer-substring-no-properties
                             (point-min)
                             (point-max)))))
                      (kill-buffer)
                      buf-str))))))


(defun mwb-b2p-html ()
  (delq nil
        (list
         ;; title
         (cons "title"
               (or (car (mwb-fetch-fields (rx (or "<title>" "<TITLE>") (group (+? anything))
                                              (or "</title>" "</TITLE>"))))
                   "Unamed"))

         ;; categories
         (cons "categories"
               (let ((categories-list
                      (mwb-categories-string-to-list
                       (car (mwb-fetch-fields "CATEGORIES")))))
                 (or
                  categories-list
                  '("Copies"))))

         ;; tags
         (cons "mt_keywords"
               (or
                (mwb-fetch-fields "KEYWORDS")
                ""))

         ;; dateCreated
         (cons "dateCreated"
               (list
                :datetime
                (let ((ctime (current-time)))
                  (cons (car ctime) (cadr ctime)))))
         ;; description
         (cons "description"
               (mwb-replace-media-object-location
                (buffer-substring-no-properties
                 (mwb-point-template-head-end)
                 (point-max)))))))

(defun mwb-b2p-other () ;todo: post还不完全
  (delq nil
	(list
	 ;; title
	 (cons "title" (buffer-name))

	 ;; categories
	 (cons "categories" (list "Unknown"))

	 ;; tags
	 (cons "mt_keywords" "")

	 ;; dateCreated
	 (cons "dateCreated"
	       (list
            :datetime
            (let ((ctime (current-time)))
              (cons (car ctime) (cadr ctime)))))
	 ;; description
	 (cons "description"
           (let* ((bf (htmlize-buffer))
                  (content (with-current-buffer bf
                             (buffer-substring-no-properties (point-min) (point-max)))))
             (kill-buffer bf)
             content)))))


(defun mwb-insert-template-head ()
  "插入头模板"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert mwb-template-head)))


(defun mwb-delete-entry-from-entry-list (postid)
  "通过postid删除博文项及posts目录下相应的文件，POSTID是string类型"
  (condition-case ()
      (progn
	(setq mwb-entry-list
	      (remove-if (lambda (entry)
			   (equal postid
				  (nth 2 entry)))
			 mwb-entry-list))
	(mwb-save-metadata)
	(and (file-exists-p (concat mwb-file-post-path postid))
	     (delete-file (concat mwb-file-post-path postid)))
	t)
    (error nil)))


(defun mwb-get-postid-by-title (title)
  "Return postid based on title, it returns:
nil: if title is invalid string
\"0\": if title is valid, but not found,
postid: if found."
  (let ((idx 0)
        (cpy mwb-entry-list)
        postid entry)
    (when (stringp title)
      (while (and (not postid)
                  (setq entry (pop cpy)))
        (when (equal title
                     (nth 1 entry))
          (let ((tmpid (nth 2 entry)))
            (setq postid (if tmpid
                             (if (numberp tmpid)
                                 (number-to-string tmpid)
                               tmpid)
                           "0"))))))
    postid))


(defun mwb-get-postid-by-src-file-name (filename)
  "Return postid based on filename, or return \"0\" if not found."
  (let ((postid nil))
    (mapc (lambda (entry)
	    (if (equal filename (nth 4 entry))
		(setq postid (nth 2 entry))))
	  mwb-entry-list)
    (or postid
	(setq postid "0"))
    postid))


(defun mwb-replace-media-object-location (buf-str)
  "处理BUF-STR中的媒体文件，返回处理后的字符串"
  (mapc (lambda (suffix)
          (let ((regexp
                 (concat "[file]*[:]?[/\\]*[a-z]?[:]?[^:*\"?<>|#]+."
                         suffix))
                (current 0))
            (while (string-match regexp
                                 buf-str
                                 current)
              (let* ((media-path (match-string 0
                                               buf-str))
                     (media-url
                      (save-match-data
                        (and (file-exists-p media-path)
                             (mwb-metaweblog-new-media-object
                              (mwb-make-media-object-file-data
                               media-path))))))

                (if media-url
                    (progn
                      (setq current
                            (+ (match-beginning 0)
                               (length media-url)))
                      (setq buf-str
                            (replace-match media-url
                                           t
                                           t
                                           buf-str)))
                  (setq current
                        (match-end 0)))))))
        mwb-media-object-suffix-list)
  buf-str)

(defun mwb-point-template-head-end ()
  (print  (save-excursion
	    (goto-char (point-min))
	    (forward-paragraph)
	    (point))))

(defvar mwb-b2p-method-alist
  '((org-mode mwb-b2p-org)
    (nxml-mode mwb-b2p-html)
    (html-mode mwb-b2p-html))
  "Buffer to Post method alist")

(defun mwb-current-buffer-to-post ()
  (let ((func (cadr (assoc major-mode mwb-b2p-method-alist))))
    (if func
        (funcall func)
      (mwb-b2p-other))))


(defun mwb-check-file-in-entry-list (src-file)
  "检查文件是否已经在列表项中"
  (let ((res nil))
    (mapc (lambda (entry)
	    (or res
		(setq res
		      (equal src-file (nth 4 entry)))))
	  mwb-entry-list)
    res))



(defun mwb-delete-post-from-entry-list (postid)
  "通过postid将相应的entry的postid设置为nil并删除posts目录下相应的文件，成功返回t.POSTID是string类型或者int类型"
  (if (integerp postid)
      (setq postid (int-to-string postid)))

  (condition-case ()
      (progn
	(setq mwb-entry-list
	      (mapcar (lambda (entry)
			(if (equal postid
				   (if (integerp (nth 2 entry))
				       (int-to-string (nth 2 entry))
				     (nth 2 entry)))
			    (progn
			      (setcar (nthcdr 2 entry) nil)
			      (setcar (nthcdr 3 entry) nil)
			      (setcar (nthcdr 5 entry) "UNPUBLISHED")))
			entry)
		      mwb-entry-list))
	(mwb-save-metadata)
	(and (file-exists-p (concat mwb-file-post-path postid))
	     (delete-file (concat mwb-file-post-path postid)))
	t)
    (error nil)))

(defun mwb-import-directory (directory)
  ;; 滤掉所有以.开头的文件，这样就没有了..和.以及所有的隐藏文件
  ;; 滤掉所有以~结尾的文件，这样就没有了自动备份

  (let ((files (directory-files directory t "^[^\.].*[^~]$" t)))
    (mapc (lambda (file)
					;目录
	    (cond ((file-directory-p file)
		   (mwb-import-directory file))
					;合法文件
		  ((member (file-name-extension file) mwb-src-file-extension-list)
		   (mwb-push-src-file-to-entry-list file))))
	  files)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;功能函数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mwb-import-current-file ()
  "将当前文件加入到库中（增加到博文项mwb-entry-list中）"
  (interactive)
  (let ((src-file (buffer-file-name)))
    (if (member (file-name-extension src-file)
		mwb-src-file-extension-list)
	(progn
	  (mwb-push-src-file-to-entry-list src-file)
	  (mwb-save-metadata)
	  (message "Succeed!"))
      (message "Failed: UNSUPPORTED file!"))))

(defun mwb-import-file ()
  "添加一个文件加入到库中（增加到博文项mwb-entry-list中）"
  (interactive)
  (let ((src-file (read-file-name "Import file: ")))
    (if (member (file-name-extension src-file)
		mwb-src-file-extension-list)
	(progn
	  (mwb-push-src-file-to-entry-list src-file)
	  (mwb-save-metadata)
	  (message "Succeed!"))
      (message "Failed: UNSUPPORTED file!"))))


(defun mwb-import-folder ()
  "递归添加一个目录中的所有合法文件到库中，这个是给用户用的，主要是调用mwb-import-directory"
  (interactive)
  (let ((directory (read-directory-name "Import folder: ")))
    (mwb-import-directory directory)
    (mwb-save-metadata)))

(defun mwb-setup-blog ()
  (interactive)
  (setq mwb-server-url
        (read-string "Your MetaWebBlog Address:" nil nil))
  (setq mwb-blog-id
        (read-string "Blog ID (if any):" nil nil))
  (setq mwb-user-name
        (read-string "Your username:" nil nil))
  (setq mwb-user-passwd
        (read-passwd "Your password:" nil ))
  (message "Please save password into ~/.emacs.d/rc/100-private.el")
  (setq mwb-category-list
        (mwb-metaweblog-get-categories))
  (setq mwb-blog-info
        (mwb-metaweblog-get-users-blogs))
  (if mwb-blog-info
      (progn
        (customize-save-variable 'mwb-server-url mwb-server-url)
        (customize-save-variable 'mwb-user-name mwb-user-name)
        (customize-save-variable 'mwb-blog-id mwb-blog-id)

        ;; Create necessary directories if they do not exist.
        (or (file-directory-p mwb-file-root-path)
            (make-directory mwb-file-root-path))
        (or (file-directory-p mwb-file-post-path)
            (make-directory mwb-file-post-path))
        (message "Set up finished. Password is not saved in customize file,
        you can save it on your own."))
    (message "Failed to setup.")))

;;;###autoload
(defun mwb-new-post ()
  (interactive)
  (let ((fn (buffer-file-name))
        postid post)

    (case (mwb-check-legal-for-publish fn)
      ((PUBLISHED)
       (message "This post has been published, you can update it, using M-x mwb-edit-post(C-c c e)"))
      ((FORBID) (message "One of `mwb-customize-checks` prevent this post!"))
      (t
       (message "Trying to post new article ... ")
       (mwb-request-password)
       (setq postid (mwb-metaweblog-new-post (mwb-current-buffer-to-post) t)
             postid (if (integerp postid) (int-to-string postid) postid)
             post (mwb-metaweblog-get-post postid)) ;; Get posted content based on postid

       (mwb-save-posted postid post) ;; Save posted content

       ;; Save entry
       (if (mwb-check-file-in-entry-list (buffer-file-name))
           (mwb-assign-post-to-file post (buffer-file-name))
         (push

          (list (mwb-gen-id) (cdr (assoc "title" post))  postid
                (cdr (assoc "categories" post)) (buffer-file-name) "PUBLISHED")
          mwb-entry-list))
       (mwb-save-metadata)
       (message "Post published！")))))

(defun mwb-save-draft ()
  (interactive)
  (let ((postid
	 (mwb-metaweblog-new-post (mwb-current-buffer-to-post)
				      nil)))
    (setq mwb-entry-list
	  (cons
	   (mwb-metaweblog-get-post postid)
	   mwb-entry-list))
    (mwb-save-metadata))
  (message "保存草稿成功！"))




(defun mwb-delete-post ()
  (interactive)
  (if (mwb-check-legal-for-delete (buffer-file-name))
      (let ((postid
	     (mwb-get-postid-by-src-file-name (buffer-file-name))))
	(if (and postid
		 (yes-or-no-p "Are you sure?")
		 (mwb-metaweblog-delete-post postid t)
		 (mwb-delete-post-from-entry-list postid)
		 (mwb-save-metadata))

	    (message "Succeed！")
	  (message "Failed！")))))


(defun mwb-edit-post ()
  (interactive)
  (mwb-request-password)
  (if (mwb-check-legal-for-edit (buffer-file-name))
      (let ((postid
             (mwb-get-postid-by-src-file-name
              (buffer-file-name)))
            content)
        (when (or (and (stringp postid)
                       (string= "0" postid))
                  (not (yes-or-no-p "Are you sure to update?"))
                  (not (setq content (mwb-current-buffer-to-post))))
          (error "Failed"))

        ;; Post modification to server.
        (mwb-metaweblog-edit-post postid content t)

        ;; Save entry list.
        (mwb-assign-post-to-file (mwb-metaweblog-get-post postid)
                                     (buffer-file-name))
        (mwb-save-metadata)

        ;; Save modified content.
        (mwb-save-posted postid content)
        (message "Succeed!")
        )))

(defun mwb-get-post ()
  (interactive)
  (let* ((postid
	  (read-string "Post ID："))
	 (post
	  (condition-case ()
	      (mwb-metaweblog-get-post postid)
	    (error nil))))
    (if (and postid
	     (mwb-delete-post-from-entry-list postid)
	     post
	     (setq mwb-entry-list
		   (cons post mwb-entry-list)))
	(message "获取成功！")
      (message "获取失败"))))

;; 获取并保存分类
(defun mwb-get-categories ()
  (interactive)
  (mwb-request-password)
  (setq mwb-category-list
        (condition-case ()
            (mwb-metaweblog-get-categories)
          (error nil)))
  (if  (not mwb-category-list)
      (message "Failed to get categories.")
    (mwb-save-metadata)
    (message "Succeeded in getting categories.")))



(defun mwb-get-recent-posts ()
  (interactive)
  (mwb-request-password)
  (let* ((num (read-number "Input number of posts to retrieve:"
                           1))
         (posts (condition-case ()
                    (mwb-metaweblog-get-recent-posts num)
                  (error nil))))
    (if (not posts)
        (message "Failed to retrieve posts.")
      (progn
        (mapc (lambda (post)
                (mwb-push-post-to-entry-list post))
              posts)
        (mwb-save-metadata)
        (message "Succeeded.")))))



(defun mwb-get-users-blogs ()
  (interactive)
  (setq mwb-blog-info
        (condition-case ()
            (prog1
                (mwb-metaweblog-get-users-blogs)
              (message "获取用户博客信息成功！"))
          (error mwb-blog-info))))

(defun mwb-add-props (str plist)
  "将faces属性plist赋给str，并返回这个str"
  (set-text-properties 0 (length str) plist str)
  str)

(defun mwb-select-insert-category ()
  "Select and insert category at current position"
  (interactive)

  )


(defun mwb-get-metadata ()
  "Get metadata of mwb"
  ;; Load variables if metadata exists.
  (setq mwb-entry-list nil
        mwb-category-list nil
        meb-tag-list nil)

  (let ((meta-file (concat mwb-file-root-path "mwb-metadata"))
        metadata)
    (when (file-exists-p meta-file)
      (setq metadata
            (with-temp-file (make-temp-file "mwb-metadata")
              (insert-file-contents (concat mwb-file-root-path "mwb-metadata"))
              (read (set-marker (make-marker) 0 (current-buffer)))))
      (setq mwb-entry-list (pop metadata)
            mwb-category-list (pop metadata)
            meb-tag-list (pop metadata)))))

(defun mwb-save-metadata ()
  "Save metadata"
  (with-temp-file (concat mwb-file-root-path "mwb-metadata")
    (erase-buffer)
    (print (list mwb-entry-list mwb-category-list mwb-tag-list) (current-buffer))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;mode设置;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 下面是关于minor mode的内容
(defun mwb-init ()
  "Initialize mwb."

  ;; Ensure mwb-file-root-path exists first.
  (if (not (file-exists-p mwb-file-root-path))
      (make-directory mwb-file-root-path))

  (setq mwb-file-post-path (concat mwb-file-root-path "posts/"))
  (if (not (file-exists-p mwb-file-post-path))
      (make-directory mwb-file-post-path))

  (mwb-get-metadata))


(defun mwb-reload ()
  "Reload configuration of mwb"
  (interactive)
  (mwb-init))

;; 定义菜单
(define-key mwb-mode-map [menu-bar menu-bar-mwb-menu]
  (cons "Mwb" mwb-mode-map))

;;;###autoload
(define-minor-mode mwb-minor-mode
  "mwb-minor-mode"
  :init-value nil
  :lighter " Mwb"
  :keymap mwb-mode-map
  :group Mwb
  (mwb-init))

(provide 'mwb)
