滚滚长江东逝水。

(let ((x ?火))
  (if (char-equal x ?水)
      (progn
	(message "true")
	(message "x 是水"))
      (progn
	(message "false")
	(message "水火不容"))))

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

(progn
  (goto-char (point-min))
  (walker ""))


void walker(String acc) {
  if (point() < point_max()) {
    Char x = char_after(point());
    if (x 为 '水') {
      return acc;
    } else {
      acc = concat(acc, string(x));
      forward_char();
      walker(acc);
    }
  } else {
    return acc;
  }
}
