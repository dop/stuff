(defvar *dp/src-table-example* nil)

(setq *dp/src-table-example* "| *Before: Hard to Test*                          | *After: Testable and Flexible Design*                 |
| #+BEGIN_EXAMPLE                               | #+BEGIN_EXAMPLE                                     |
| // Basic new operators called directly in     | class House {                                       |
|                                               | Kitchen kitchen;                                    |
| //   preventing a seam to create different    | Bedroom bedroom;                                    |
| //   kitchen and bedroom collaborators).      |                                                     |
| class House {                                 | // Have Guice create the objects                    |
| Kitchen kitchen = new Kitchen();              | //   and pass them in                               |
| Bedroom bedroom;                              | @Inject                                             |
|                                               | House(Kitchen k, Bedroom b) {                       |
| House() {                                     | kitchen = k;                                        |
| bedroom = new Bedroom();                      | bedroom = b;                                        |
| }                                             | }                                                   |
|                                               | // ...                                              |
| // ...                                        | }                                                   |
| }                                             | #+END_EXAMPLE                                       |
| #+END_EXAMPLE                                 |                                                     |
| #+BEGIN_EXAMPLE                               | #+BEGIN_EXAMPLE                                     |
| // An attempted test that becomes pretty hard | // New and Improved is trivially testable, with any |
|                                               | //   test-double objects as collaborators.          |
| class HouseTest extends TestCase {            |                                                     |
| public void testThisIsReallyHard() {          | class HouseTest extends TestCase {                  |
| House house = new House();                    | public void testThisIsEasyAndFlexible() {           |
| // Darn! I'm stuck with those Kitchen and     | Kitchen dummyKitchen = new DummyKitchen();          |
| //   Bedroom objects created in the           | Bedroom dummyBedroom = new DummyBedroom();          |
| //   constructor.                             |                                                     |
|                                               |  House house =                                      |
| // ...                                        | new House(dummyKitchen, dummyBedroom);              |
| }                                             |                                                     |
| }                                             | // Awesome, I can use test doubles that             |
| #+END_EXAMPLE                                 | //   are lighter weight.                            |
|                                               |                                                     |
|                                               | // ...                                              |
|                                               | }                                                   |
|                                               | }                                                   |
|                                               | #+END_EXAMPLE                                       |
")

(defun dp/example-table-to-src-blocks (input)
  (let ((matches (-drop 1 (s-match-strings-all "|\\([^|]+\\)|\\([^|]+\\)|\n" input)))
        (good-lines "")
        (bad-lines ""))
    (cl-labels ((skip (line)
                      (or (s-matches? "#\\+\\(BEGIN\\|END\\)_EXAMPLE" line)
                          (string-empty-p (s-trim line)))))
      (loop
       for (bad good) in (-map #'cdr matches)
       do (progn
            (setq good-lines (concat good-lines (if (skip good) "" (concat (s-trim-right good) "\n"))))
            (setq bad-lines (concat bad-lines (if (skip bad) "" (concat (s-trim-right bad) "\n"))))))
    (list bad-lines good-lines))))

(dp/example-table-to-src-blocks *dp/src-table-example*)

(defun dp/src-blocks-to-org-src-blocks (bad good)
  (concat
   (format "#+CAPTION: Before: Hard to Test\n#+BEGIN_SRC java\n%s\n#+END_SRC" bad) "\n\n"
   (format "#+CAPTION: After: Testable and Flexible Design\n#+BEGIN_SRC java\n%s\n#+END_SRC" good) "\n"))

(defun dp/indent-java-src-block (input)
  (with-temp-buffer
    (insert input)
    (java-mode)
    (mark-page)
    (c-indent-region (point-min) (point-max))
    (buffer-string)))

(defun dp/org-table-bounds ()
  (let (start end)
    (save-excursion
      (beginning-of-line)
      (while (and (looking-at "|") (= 0 (forward-line -1))))
      (when (not (looking-at "|")) (forward-line))
      (setq start (point))
      (while (and (looking-at "|") (= 0 (forward-line))))
      (when (looking-at "|") (goto (end-of-buffer)))
      (setq end (point))
      (when (= start end)
        (error "No org table in sight!"))
      (cons start end))))

(defun dp/convert-example-table-to-src-block ()
  (interactive)
  (dp/with-region-or
   #'dp/org-table-bounds
   (lambda (start end)
     (destructuring-bind (bad good)
         (dp/example-table-to-src-blocks (buffer-substring start end))
       (save-excursion
         (delete-region start end)
         (insert
          (dp/src-blocks-to-org-src-blocks (dp/indent-java-src-block bad)
                                           (dp/indent-java-src-block good))))))))
