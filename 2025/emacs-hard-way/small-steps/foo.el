滚滚长江东逝水。

(defun walker (acc)
  (let (x water?)
    (setq x (char-after (point)))
    (setq water? (char-equal ?水 x))
    (when water?
      acc)
    (when (not water?)
      (setq acc (concat acc (string x)))
      (forward-char)
      (message "%s" acc)
      (walker acc))))

(progn
  (goto-char (point-min))
  (message "%s" (walker "")))

(char-equal ?水 (char-after 7))


(let ((x ?火))
  (if (char-equal x ?水)
      (progn
        (message "true")
        (message "x 是水"))
    (progn
      (message "false")
      (message "水火不容"))))

(catch 'here
  (message "step 1")
  (throw 'here "step 1")
  (message "step 2")
  (message "step 3"))

(defun walker ()
  (goto-char (point-min))
  (let ((acc "") x)
    (catch 'break
      (while (< (point) (point-max))
	(setq x (char-after (point)))
	(when (char-equal x ?水)
	  (throw 'break acc))
	(setq acc (concat acc (string x)))
	(forward-char)))))

(walker)

(defun walker (acc)
  (if (< (point) (point-max))
      (progn
        (let ((x (char-after (point))))
          (if (char-equal x ?水)
              acc
            (progn
              (setq acc (concat acc (string x)))
              (forward-char)
              (walker acc)))))
    acc))
