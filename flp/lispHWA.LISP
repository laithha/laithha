
;; mathematical model 
;; twiceNthElem(l1l2...ln,n,pos)=
;; = nil, if m =0 if the length or the elements in the list is zero then it will return nil(null)
;; = {l1} U {l1} twiceNthElem(l2...lm,n,pos+1), if n = pos ;; this is when the current position matches n (the function duplicates the element l1 . it does this by adding l1 twice at this positionin the result and then it continues with the resot of the list 
;; = {l1} U twiceNthElem(l2...lm,n,pos+1), otherwise ;; this is when n doesnt match the current position thee function simply adds the element l1 to the result and continues with the next elements by calling the function with the rest of the list
;; l1 is the first element in the list
;;l2 is the second element in the list 
;;m is the length of the list 
;; n is the element you want to duplicate
;; pos is the current position in the list (it starts at 0 and increments as the function proccesses)

(defun twiceNthElem(l n pos)
  (cond
    ((null l) nil) ;; Base case: if the list is empty, return nil.(null)
    ((equal n pos) ;; If current position matches n:
     (cons (car l) ;; Add the current element.
           (cons (car l) ;; Add the current element again.
                 (twiceNthElem (cdr l) n (+ 1 pos))))) ;; Continue with the rest of the list.
    (t (cons (car l) ;; Otherwise, just add the current element.
             (twiceNthElem (cdr l) n (+ 1 pos)))))) ;; Continue with the rest of the list.

(defun mainA(l n)
  (twiceNthElem l n 0)) ;; Call twiceNthElem with position starting at 0
.


;;(mainA '(10 20 30 40 50) 2) for running it
  ;; Call the function with an empty list and any value for n.
;;for an empty list

