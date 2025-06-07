;; -*- lexical-binding: t; -*-
(defun current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

;; orez 文学编程中代码片段处理
(defun orez-search-bottom-boundry ()
  (save-excursion
    (catch 'break
      (while (< (point) (point-max))
	(if (string-match "^[ \t]*@[ \t]*$" (current-line))
	    (throw 'break (point))
	  (forward-line)))
      nil)))
(defun orez-search-top-boundry ()
  (save-excursion
    (let ((acc ""))
      (catch 'break
	(while (> (point) (point-min))
	  (when (string-match "^[ \t]*@[^#]+#" acc)
	    (throw 'break (point)))
	  (setq acc (concat (current-line) acc))
	  (forward-line -1))
	nil))))
(defun orez-beginning ()
  (interactive)
  (save-excursion
    (let ((x (orez-search-top-boundry)))
      (when x
	(goto-char x)
	(catch 'break
	  (while t
	    (if (string-match "#" (current-line))
		(progn
		  (forward-line)
		  (when (string-match "^[ \t]*<[^>]+>[ \t]*$" (current-line))
		    (forward-line))
		  (throw 'break (point)))
	      (forward-line))))))))

(defun orez-end ()
  (interactive)
  (- (orez-search-bottom-boundry) 1))

(defun snippet-buffer (x y)
  (let ((orez-snippet-buf (get-buffer "orez-snippet")))
    (defvar orez-main-buf (current-buffer))
    (when (not orez-snippet-buf)
      (setq orez-snippet-buf (generate-new-buffer "orez-snippet")))
    (let (text)
      (select-window (split-window-below))
      (when (and x y)
	(setq text (buffer-substring-no-properties x y))
	(delete-region x y))
      (setq buffer-read-only t)
      (switch-to-buffer orez-snippet-buf)
      (erase-buffer)
      (when text
	(insert text)))))
;; 进入 c 模式，其他模式可仿照该函数定义
(defun orez-c ()
  (interactive)
  (let (x y)
    (setq x (orez-beginning))
    (setq y (orez-end))
    (snippet-buffer x y)
    (c-mode)))
;; 提交编辑结果
(defun orez-ret ()
  (interactive)
  (let ((tmp-buf (current-buffer))
	(delta (point-max)))
    (with-current-buffer orez-main-buf
      (setq buffer-read-only nil)
      (insert-buffer tmp-buf)
      (delete-window)
      (forward-char (- delta 1)))))

;; markdown 中的代码片段处理
(defun mkd-end ()
  (save-excursion
    (catch 'break
      (while (< (point) (point-max))
	(if (string-match "^```[ \t]*$" (current-line))
	    (throw 'break (- point 1))
	  (forward-line)))
      nil)))
(defun mkd-beginning ()
  (save-excursion
    (let ((acc ""))
      (catch 'break
	(while (> (point) (point-min))
	  (when (string-match "^```[ \t]*$" acc)
            (forward-line)
	    (throw 'break (point)))
	  (setq acc (concat (current-line) acc))
	  (forward-line -1))
	nil))))
;; 进入 c 模式
(defun mkd ()
  (interactive)
  (let (x y lang)
    (setq x (mkd-beginning))
    (setq y (mkd-end))
    ;; 提取代码片段的语言标记
    (save-excursion
      (let (head)
        (goto-char x)
        (forword-line -1)
        (setq head (current-line))
        (when (string-match "^```[ \t]*\\(.*\\)$" head)
          (setq lang (match-string 1 head)))))
    (snippet-buffer x y)
    (when lang
      (funcall (make-symbol format("%s-mode" lang))))))
;; 提交结果
(defun mkd-ret ()
  (interactive)
  (orez-ret))
