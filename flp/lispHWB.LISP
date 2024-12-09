;; Mathematical model for myAppend
;; myAppend(l1l2...ln, p1p2...pm) =
;; = p1p2...pm, if n = 0  ;; If the first list is empty, return the second list 
;; = {l1} U myAppend(l2...ln, p1p2...pm), otherwise  ;; Otherwise   ;; if the list is not empty (n>0) then we take the first element and prepennt it to the result of recursively calling myAppend on the rest of the list and the second list

(defun myAppend (l p)
  (cond
    ((null l) p)  ;; If the first list is empty, return the second list.
    (t (cons (car l)  ;; Otherwise, prepend the first element of the first list.
             (myAppend (cdr l) p)))))  ;; Recurse with the rest of the first list and the second list.

;; Test cases for myAppend
;;(myAppend '() '(4 5 6))  ;; Expected Output: (4 5 6)
;;(myAppend '(A B C) '(D E F G)) ;; Expected Output: (A B C D E F G)




;; Mathematical model for association
;; association(l1l2...ln, p1p2...pm) = 
;; = nil, if n = 0 and m = 0  ;; Both lists are empty, return nil.
;; = myAppend(association(nil, p2...pm), list({nil} U {p1})), if n = 0  ;; If the first list is empty, pair nil with each element of the second list.
;; = myAppend(association(l2...ln, nil), list({l1} u {nil})), if m = 0  ;; If the second list is empty, pair each element of the first list with nil.
;; = myAppend(association(l2...ln, p2...pm), list({l1} U {p1})), otherwise  ;; Otherwise ;; we create a pair (l1.p1) by pairing the first element of the first list with the first element of the second list

(defun association (l p)
  (cond
    ((and (null l) (null p)) nil)  ;; Case 1: Both lists are empty, return nil.
    ((null l)  ;; Case 2: First list is empty, pair nil with each element of second list.
     (myAppend 
      (list (cons nil (car p)))  ;; Pair nil with the first element of the second list.
      (association nil (cdr p))))  ;; Recurse with the rest of the second list.
    ((null p)  ;; Case 3: Second list is empty, pair each element of the first list with nil.
     (myAppend 
      (list (cons (car l) nil))  ;; Pair the first element of the first list with nil.
      (association (cdr l) nil)))  ;; Recurse with the rest of the first list.
    (t  ;; Case 4: Both lists have elements, pair corresponding elements.
     (myAppend 
      (list (cons (car l) (car p)))  ;; Pair the first element from both lists.
      (association (cdr l) (cdr p))))))  ;; Recurse with the rest of both lists.

;; Test cases for association
;;(association '(A B C) '(X Y Z))  ;; Expected Output: ((A . X) (B . Y) (C . Z))
;;(association '(A B) '(X))  ;; Expected Output: ((A . X) (B . nil))
;;(association '(A B C) '(X Y))  ;; Expected Output: ((A . X) (B . Y) (C . nil))
;;(association nil nil)  ;; Expected Output: nil
