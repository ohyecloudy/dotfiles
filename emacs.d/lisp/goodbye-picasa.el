(require 'el-mock)
(require 'cl-lib)

(defun goodbye-picasa (filename)
  ;; 다운로드할 picasa 주소 추출
  ;; 해당 주소를 대체할 local path 생성
  ;; 다운로드
  ;; 버퍼에서 picasa 주소를 local path로 대체
  )

;; 이걸 string이나 혹은 file로 바꾸고 싶은데, 가능하나?
(defun bye-picasa--extract-picasa-images-url (string)
  (with-temp-buffer
    (insert string) ; string을 추상화해서 file에서 읽거나 test용 string을 읽게 해야하는 걸까?
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (url-list ()))
      (while (search-forward-regexp
              "\\([^][ \"\']*googleusercontent[^][ \"\']*\\)"
              nil
              t)
        (add-to-list 'url-list (match-string 1)))
      (reverse url-list))))

(ert-deftest bye-picasa--extract-picasa-images-url-test ()
  (let* ((to-extract-1 "https://lh5.googleusercontent.com/-1Txefxmwfdg/TxuFylM_MCI/AAAAAAAAKLA/WANDTRNQdgY/s640/thumbnail-1.jpg")
         (test-string-1 (concat "[[" to-extract-1 "][test]]"))
         (test-string-2 (concat "href=\"" to-extract-1 "\">"))
    (should (equal (bye-picasa--extract-picasa-images-url test-string-1)
                   (cons to-extract-1 ())))
    (should (equal (bye-picasa--extract-picasa-images-url test-string-2)
                   (cons to-extract-1 ()))))))

(defun bye-picasa--make-img-base-path (path)
  (let ((from "_posts")
        (to "assets"))
    (unless (string-match from path)
      (throw 'path-should-be-contains-a-_posts-directory path))
    (replace-regexp-in-string
     from
     to
     (concat
      (file-name-directory path)
      (file-name-base path)
      "-"))))

(ert-deftest bye-picasa--make-img-base-path-throw-test ()
  (should-error (bye-picasa--make-img-base-path "not-contains-_post")
                :type 'no-catch))
(ert-deftest bye-picasa--make-img-base-path-test ()
  (should (equal (bye-picasa--make-img-base-path
                  "~/project/lifelog/_posts/2009-02-08-523.org")
                 "~/project/lifelog/assets/2009-02-08-523-")))

(defun bye-picasa--full-img-path (base-path count predicate)
  (when (> count 99)
    (throw 'a-count-variable-should-not-exceed-99 count))
  (let (ret '())
    (dotimes (i 100)
      (let ((full-path (concat
                        base-path
                        (if (< i 10) "0" "")
                        (number-to-string i))))
        (when (funcall predicate full-path)
          (add-to-list 'ret full-path))))
    (butlast (reverse ret) (- (length ret) count))))

(ert-deftest bye-picasa--full-img-path-throw-test  ()
  (should-error (bye-picasa--full-img-path "b-" 100 nil)
                :type 'no-catch))

(ert-deftest bye-picasa--full-img-path-test ()
  (should (equal
           (bye-picasa--full-img-path
            "b-"
            3
            '(lambda (path) (and
                         (not (string-match "-00$" path))
                         (not (string-match "-02$" path)))))
           (list "b-01" "b-03" "b-04"))))

(defun bye-picasa--img-src-dest-lists (path contents file-exist?)
  (let* ((img-src-list (bye-picasa--extract-picasa-images-url contents))
         (base-path (bye-picasa--make-img-base-path path))
         (dest-path-list (bye-picasa--full-img-path base-path
                                                    (length img-src-list)
                                                    file-exist?)))
    (cl-mapcar '(lambda (src dest)
                  (list src (concat dest "." (file-name-extension src))))
               img-src-list
               dest-path-list)))

(ert-deftest bye-picasa--img-src-dest-lists-test ()
  (let* ((img-src-1 "https://lh5.googleusercontent.com/-AZbhW9g5lyo/TxuF05Ay8II/AAAAAAAAKLI/M1Z6xKzULpE/s640/thumbnail1.jpg")
         (img-src-2 "https://lh5.googleusercontent.com/-1Txefxmwfdg/TxuFylM_MCI/AAAAAAAAKLA/WANDTRNQdgY/s640/thumbnail-1.png")
         (test-buffer (concat "[[" img-src-1 "][test]]\n"
                              "a href=\"" img-src-2 "\">"))
         (test-file-base "~/project/lifelog/_posts/2009-02-08-523")
         (test-file-asset-base "~/project/lifelog/assets/2009-02-08-523")
         (test-path (concat test-file-base ".org"))
         (file-exist '(lambda (path) (and
                         (not (string-match "-01$" path))
                         (not (string-match "-02$" path))))))
    (should (equal
             (bye-picasa--img-src-dest-lists test-path test-buffer file-exist)
             (list
              (list img-src-1 (concat test-file-asset-base "-00.jpg"))
              (list img-src-2 (concat test-file-asset-base "-03.png")))))))

;; TODO full-img-path 이건 헷갈린다. 어차피 img 라서. dest와 source path로 구분하자
