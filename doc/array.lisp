(load "../lib/stdlib")

;; See Also: sequence.lisp

;; simple vector (one-dimensional array)
;; {{{

(defparameter *vector* (make-array 4 :initial-element 'a))
;;<=>
(defparameter *vector* (vector 'a 'a 'a 'a))

;; accesser
(svref *vector* 0)    ; faster
;;<=>
(aref *vector* 0)
;; => 'a

;; }}}

;; adjustable vector
;; {{{

(defparameter *vector* (make-array 5
                                   :fill-pointer 0
                                   :adjustable t
                                   :element-type 'character))

(vector-push #\a *vector*)
*vector*
;; => "a" 

(vector-push #\b *vector*)
(vector-push #\c *vector*)
*vector*
;; => "abc" 

(vector-pop *vector*)
*vector*
;; =>"ab" 

(vector-push-extend #\c *vector*)

;;}}}

;; two-dimensional array.
;; {{{
(defparameter *arr* (make-array '(2 3) :initial-element 0))

;; accesser
(aref *arr* 0 0)
;; => 0

;; substitute element.
(setf (aref *arr* 0 1) 10)
(aref *arr* 0 1)
;; => 10 

*arr*
;; => #2a((0 10 0) (0 0 0))

;; }}}


;; Function MAKE-ARRAY {{{
;; Syntax:
;; make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset
;; => new-array
;; 
;; Arguments and Values:
;; dimensions---a designator for a list of valid array dimensions.
;; element-type---a type specifier. The default is t.
;; initial-element---an object.
;; initial-contents---an object.
;; adjustable---a generalized boolean. The default is nil.
;; fill-pointer---a valid fill pointer for the array to be created, or t or nil. The default is nil.
;; displaced-to---an array or nil. The default is nil. This option must not be supplied if either initial-element or initial-contents is supplied.
;; displaced-index-offset---a valid array row-major index for displaced-to. The default is 0. This option must not be supplied unless a non-nil displaced-to is supplied.
;; new-array---an array.
;; 
;; Description:
;; Creates and returns an array constructed of the most specialized type that can accommodate elements of type given by element-type. If dimensions is nil then a zero-dimensional array is created.
;; Dimensions represents the dimensionality of the new array.
;; element-type indicates the type of the elements intended to be stored in the new-array. The new-array can actually store any objects of the type which results from upgrading element-type; see Section 15.1.2.1 (Array Upgrading).
;; If initial-element is supplied, it is used to initialize each element of new-array. If initial-element is supplied, it must be of the type given by element-type. initial-element cannot be supplied if either the :initial-contents option is supplied or displaced-to is non-nil. If initial-element is not supplied, the consequences of later reading an uninitialized element of new-array are undefined unless either initial-contents is supplied or displaced-to is non-nil.
;; initial-contents is used to initialize the contents of array. For example:
;;  (make-array '(4 2 3) :initial-contents
;;              '(((a b c) (1 2 3))
;;               ((d e f) (3 1 2))
;;               ((g h i) (2 3 1))
;;               ((j k l) (0 0 0))))
;; initial-contents is composed of a nested structure of sequences. The numbers of levels in the structure must equal the rank of array. Each leaf of the nested structure must be of the type given by element-type. If array is zero-dimensional, then initial-contents specifies the single element. Otherwise, initial-contents must be a sequence whose length is equal to the first dimension; each element must be a nested structure for an array whose dimensions are the remaining dimensions, and so on. Initial-contents cannot be supplied if either initial-element is supplied or displaced-to is non-nil. If initial-contents is not supplied, the consequences of later reading an uninitialized element of new-array are undefined unless either initial-element is supplied or displaced-to is non-nil.
;; If adjustable is non-nil, the array is expressly adjustable (and so actually adjustable); otherwise, the array is not expressly adjustable (and it is implementation-dependent whether the array is actually adjustable).
;; If fill-pointer is non-nil, the array must be one-dimensional; that is, the array must be a vector. If fill-pointer is t, the length of the vector is used to initialize the fill pointer. If fill-pointer is an integer, it becomes the initial fill pointer for the vector.
;; If displaced-to is non-nil, make-array will create a displaced array and displaced-to is the target of that displaced array. In that case, the consequences are undefined if the actual array element type of displaced-to is not type equivalent to the actual array element type of the array being created. If displaced-to is nil, the array is not a displaced array.
;; The displaced-index-offset is made to be the index offset of the array. When an array A is given as the :displaced-to argument to make-array when creating array B, then array B is said to be displaced to array A. The total number of elements in an array, called the total size of the array, is calculated as the product of all the dimensions. It is required that the total size of A be no smaller than the sum of the total size of B plus the offset n supplied by the displaced-index-offset. The effect of displacing is that array B does not have any elements of its own, but instead maps accesses to itself into accesses to array A. The mapping treats both arrays as if they were one-dimensional by taking the elements in row-major order, and then maps an access to element k of array B to an access to element k+n of array A.
;; If make-array is called with adjustable, fill-pointer, and displaced-to each nil, then the result is a simple array. If make-array is called with one or more of adjustable, fill-pointer, or displaced-to being true, whether the resulting array is a simple array is implementation-dependent.
;; When an array A is given as the :displaced-to argument to make-array when creating array B, then array B is said to be displaced to array A. The total number of elements in an array, called the total size of the array, is calculated as the product of all the dimensions. The consequences are unspecified if the total size of A is smaller than the sum of the total size of B plus the offset n supplied by the displaced-index-offset. The effect of displacing is that array B does not have any elements of its own, but instead maps accesses to itself into accesses to array A. The mapping treats both arrays as if they were one-dimensional by taking the elements in row-major order, and then maps an access to element k of array B to an access to element k+n of array A.
;; 
;; Examples:
;;  (make-array 5) ;; Creates a one-dimensional array of five elements.
;;  (make-array '(3 4) :element-type '(mod 16)) ;; Creates a 
;;                 ;;two-dimensional array, 3 by 4, with four-bit elements.
;;  (make-array 5 :element-type 'single-float) ;; Creates an array of single-floats.
;;  (make-array nil :initial-element nil) =>  #0ANIL
;;  (make-array 4 :initial-element nil) =>  #(NIL NIL NIL NIL)
;;  (make-array '(2 4) 
;;               :element-type '(unsigned-byte 2) 
;;               :initial-contents '((0 1 2 3) (3 2 1 0)))
;; =>  #2A((0 1 2 3) (3 2 1 0))
;;  (make-array 6
;;               :element-type 'character 
;;               :initial-element #\a 
;;               :fill-pointer 3) =>  "aaa"
;; The following is an example of making a displaced array.
;;  (setq a (make-array '(4 3))) 
;; =>  #<ARRAY 4x3 simple 32546632>
;;  (dotimes (i 4)
;;    (dotimes (j 3)
;;      (setf (aref a i j) (list i 'x j '= (* i j)))))
;; =>  NIL
;;  (setq b (make-array 8 :displaced-to a
;;                        :displaced-index-offset 2))
;; =>  #<ARRAY 8 indirect 32550757>
;;  (dotimes (i 8)
;;    (print (list i (aref b i))))
;; >>  (0 (0 X 2 = 0)) 
;; >>  (1 (1 X 0 = 0)) 
;; >>  (2 (1 X 1 = 1)) 
;; >>  (3 (1 X 2 = 2)) 
;; >>  (4 (2 X 0 = 0)) 
;; >>  (5 (2 X 1 = 2)) 
;; >>  (6 (2 X 2 = 4)) 
;; >>  (7 (3 X 0 = 0)) 
;; =>  NIL
;; The last example depends on the fact that arrays are, in effect, stored in row-major order.
;;  (setq a1 (make-array 50))
;; =>  #<ARRAY 50 simple 32562043>
;;  (setq b1 (make-array 20 :displaced-to a1 :displaced-index-offset 10))
;; =>  #<ARRAY 20 indirect 32563346>
;;  (length b1) =>  20
;; 
;;  (setq a2 (make-array 50 :fill-pointer 10))
;; =>  #<ARRAY 50 fill-pointer 10 46100216>
;;  (setq b2 (make-array 20 :displaced-to a2 :displaced-index-offset 10))
;; =>  #<ARRAY 20 indirect 46104010>
;;  (length a2) =>  10
;;  (length b2) =>  20
;; 
;;  (setq a3 (make-array 50 :fill-pointer 10))
;; =>  #<ARRAY 50 fill-pointer 10 46105663>
;;  (setq b3 (make-array 20 :displaced-to a3 :displaced-index-offset 10
;;                          :fill-pointer 5))
;; =>  #<ARRAY 20 indirect, fill-pointer 5 46107432>
;;  (length a3) =>  10
;;  (length b3) =>  5
;; }}}
;; Accessor AREF {{{
;; 
;; Syntax:
;; aref array &rest subscripts => element
;; (setf (aref array &rest subscripts) new-element)
;; 
;; Arguments and Values:
;; array---an array.
;; subscripts---a list of valid array indices for the array.
;; element, new-element---an object.
;; 
;; Description:
;; Accesses the array element specified by the subscripts. If no subscripts are supplied and array is zero rank, aref accesses the sole element of array.
;; aref ignores fill pointers. It is permissible to use aref to access any array element, whether active or not.
;; 
;; Examples:
;; If the variable foo names a 3-by-5 array, then the first index could be 0, 1, or 2, and then second index could be 0, 1, 2, 3, or 4. The array elements can be referred to by using the function aref; for example, (aref foo 2 1) refers to element (2, 1) of the array.
;; 
;;  (aref (setq alpha (make-array 4)) 3) =>  implementation-dependent
;;  (setf (aref alpha 3) 'sirens) =>  SIRENS
;;  (aref alpha 3) =>  SIRENS
;;  (aref (setq beta (make-array '(2 4) 
;;                     :element-type '(unsigned-byte 2)
;;                     :initial-contents '((0 1 2 3) (3 2 1 0))))
;;         1 2) =>  1
;;  (setq gamma '(0 2))
;;  (apply #'aref beta gamma) =>  2
;;  (setf (apply #'aref beta gamma) 3) =>  3
;;  (apply #'aref beta gamma) =>  3
;;  (aref beta 0 2) =>  3
;; }}}
;; Function VECTOR {{{
;;
;; Syntax:
;; vector &rest objects => vector
;; 
;; Arguments and Values:
;; object---an object.
;; vector---a vector of type (vector t *).
;; 
;; Description:
;; Creates a fresh simple general vector whose size corresponds to the number of objects.
;; The vector is initialized to contain the objects.
;; 
;; Examples:
;; 
;;  (arrayp (setq v (vector 1 2 'sirens))) =>  true
;;  (vectorp v) =>  true
;;  (simple-vector-p v) =>  true
;;  (length v) =>  3
;; 
;; }}}
;; Function VECTOR-PUSH, VECTOR-PUSH-EXTEND {{{
;;
;; Syntax:
;; vector-push new-element vector => new-index-p
;; vector-push-extend new-element vector &optional extension => new-index
;; 
;; Arguments and Values:
;; new-element---an object.
;; vector---a vector with a fill pointer.
;; extension---a positive integer. The default is implementation-dependent.
;; new-index-p---a valid array index for vector, or nil.
;; new-index---a valid array index for vector.
;; 
;; Description:
;; vector-push and vector-push-extend store new-element in vector. vector-push attempts to store new-element in the element of vector designated by the fill pointer, and to increase the fill pointer by one. If the (>= (fill-pointer vector) (array-dimension vector 0)), neither vector nor its fill pointer are affected. Otherwise, the store and increment take place and vector-push returns the former value of the fill pointer which is one less than the one it leaves in vector.
;; vector-push-extend is just like vector-push except that if the fill pointer gets too large, vector is extended using adjust-array so that it can contain more elements. Extension is the minimum number of elements to be added to vector if it must be extended.
;; vector-push and vector-push-extend return the index of new-element in vector. If (>= (fill-pointer vector) (array-dimension vector 0)), vector-push returns nil.
;; 
;; Examples:
;;  (vector-push (setq fable (list 'fable))
;;               (setq fa (make-array 8 
;;                                    :fill-pointer 2
;;                                    :initial-element 'first-one))) =>  2 
;;  (fill-pointer fa) =>  3 
;;  (eq (aref fa 2) fable) =>  true
;;  (vector-push-extend #\X
;;                     (setq aa 
;;                           (make-array 5
;;                                       :element-type 'character
;;                                       :adjustable t
;;                                       :fill-pointer 3))) =>  3 
;;  (fill-pointer aa) =>  4 
;;  (vector-push-extend #\Y aa 4) =>  4 
;;  (array-total-size aa) =>  at least 5 
;;  (vector-push-extend #\Z aa 4) =>  5 
;;  (array-total-size aa) =>  9 ;(or more)
;;
;; }}}
;; Function VECTOR-POP {{{
;; 
;; Syntax:
;; vector-pop vector => element
;; Arguments and Values:
;; vector---a vector with a fill pointer.
;; element---an object.
;; 
;; Description:
;; Decreases the fill pointer of vector by one, and retrieves the element of vector that is designated by the new fill pointer.
;; 
;; Examples:
;;  (vector-push (setq fable (list 'fable))
;;               (setq fa (make-array 8
;;                                    :fill-pointer 2
;;                                    :initial-element 'sisyphus))) =>  2 
;;  (fill-pointer fa) =>  3 
;;  (eq (vector-pop fa) fable) =>  true
;;  (vector-pop fa) =>  SISYPHUS 
;;  (fill-pointer fa) =>  1 
;;
;; Side Effects:
;; The fill pointer is decreased by one.
;; 
;; Affected By:
;; The value of the fill pointer.
;; 
;; Exceptional Situations:
;; An error of type type-error is signaled if vector does not have a fill pointer.
;; If the fill pointer is zero, vector-pop signals an error of type error.
;; 
;; 
;; }}}
;; Accessor FILL-POINTER {{{
;;
;; Syntax:
;; fill-pointer vector => fill-pointer
;; (setf (fill-pointer vector) new-fill-pointer)
;; Arguments and Values:
;; vector---a vector with a fill pointer.
;; fill-pointer, new-fill-pointer---a valid fill pointer for the vector.
;; 
;; Description:
;; Accesses the fill pointer of vector.
;; 
;; Examples:
;;  (setq a (make-array 8 :fill-pointer 4)) =>  #(NIL NIL NIL NIL)
;;  (fill-pointer a) =>  4
;;  (dotimes (i (length a)) (setf (aref a i) (* i i))) =>  NIL
;;  a =>  #(0 1 4 9)
;;  (setf (fill-pointer a) 3) =>  3
;;  (fill-pointer a) =>  3
;;  a =>  #(0 1 4)
;;  (setf (fill-pointer a) 8) =>  8
;;  a =>  #(0 1 4 9 NIL NIL NIL NIL)
;;
;; }}}
;; Function ADJUST-ARRAY {{{
;; 
;; Syntax:
;; adjust-array array new-dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset
;; => adjusted-array
;; 
;; Arguments and Values:
;; array---an array.
;; new-dimensions---a valid array dimension or a list of valid array dimensions.
;; element-type---a type specifier.
;; initial-element---an object. Initial-element must not be supplied if either initial-contents or displaced-to is supplied.
;; initial-contents---an object. If array has rank greater than zero, then initial-contents is composed of nested sequences, the depth of which must equal the rank of array. Otherwise, array is zero-dimensional and initial-contents supplies the single element. initial-contents must not be supplied if either initial-element or displaced-to is given.
;; fill-pointer---a valid fill pointer for the array to be created, or t, or nil. The default is nil.
;; displaced-to---an array or nil. initial-elements and initial-contents must not be supplied if displaced-to is supplied.
;; displaced-index-offset---an object of type (fixnum 0 n) where n is (array-total-size displaced-to). displaced-index-offset may be supplied only if displaced-to is supplied.
;; adjusted-array---an array.
;; 
;; Description:
;; adjust-array changes the dimensions or elements of array. The result is an array of the same type and rank as array, that is either the modified array, or a newly created array to which array can be displaced, and that has the given new-dimensions.
;; New-dimensions specify the size of each dimension of array.
;; Element-type specifies the type of the elements of the resulting array. If element-type is supplied, the consequences are unspecified if the upgraded array element type of element-type is not the same as the actual array element type of array.
;; If initial-contents is supplied, it is treated as for make-array. In this case none of the original contents of array appears in the resulting array.
;; If fill-pointer is an integer, it becomes the fill pointer for the resulting array. If fill-pointer is the symbol t, it indicates that the size of the resulting array should be used as the fill pointer. If fill-pointer is nil, it indicates that the fill pointer should be left as it is.
;; If displaced-to non-nil, a displaced array is created. The resulting array shares its contents with the array given by displaced-to. The resulting array cannot contain more elements than the array it is displaced to. If displaced-to is not supplied or nil, the resulting array is not a displaced array. If array A is created displaced to array B and subsequently array B is given to adjust-array, array A will still be displaced to array B. Although array might be a displaced array, the resulting array is not a displaced array unless displaced-to is supplied and not nil. The interaction between adjust-array and displaced arrays is as follows given three arrays, A, B, and C:
;; 
;; A is not displaced before or after the call
;;  (adjust-array A ...)
;; The dimensions of A are altered, and the contents rearranged as appropriate. Additional elements of A are taken from initial-element. The use of initial-contents causes all old contents to be discarded.
;; 
;; A is not displaced before, but is displaced to C after the call
;;  (adjust-array A ... :displaced-to C)
;; None of the original contents of A appears in A afterwards; A now contains the contents of C, without any rearrangement of C.
;; 
;; A is displaced to B before the call, and is displaced to C after the call
;;  (adjust-array A ... :displaced-to B)
;;  (adjust-array A ... :displaced-to C)
;; B and C might be the same. The contents of B do not appear in A afterward unless such contents also happen to be in C If displaced-index-offset is not supplied in the adjust-array call, it defaults to zero; the old offset into B is not retained.
;; 
;; A is displaced to B before the call, but not displaced afterward.
;;  (adjust-array A ... :displaced-to B)
;;  (adjust-array A ... :displaced-to nil)
;; A gets a new ``data region,'' and contents of B are copied into it as appropriate to maintain the existing old contents; additional elements of A are taken from initial-element if supplied. However, the use of initial-contents causes all old contents to be discarded.
;; If displaced-index-offset is supplied, it specifies the offset of the resulting array from the beginning of the array that it is displaced to. If displaced-index-offset is not supplied, the offset is 0. The size of the resulting array plus the offset value cannot exceed the size of the array that it is displaced to.
;; If only new-dimensions and an initial-element argument are supplied, those elements of array that are still in bounds appear in the resulting array. The elements of the resulting array that are not in the bounds of array are initialized to initial-element; if initial-element is not provided, the consequences of later reading any such new element of new-array before it has been initialized are undefined.
;; If initial-contents or displaced-to is supplied, then none of the original contents of array appears in the new array.
;; The consequences are unspecified if array is adjusted to a size smaller than its fill pointer without supplying the fill-pointer argument so that its fill-pointer is properly adjusted in the process.
;; If A is displaced to B, the consequences are unspecified if B is adjusted in such a way that it no longer has enough elements to satisfy A.
;; If adjust-array is applied to an array that is actually adjustable, the array returned is identical to array. If the array returned by adjust-array is distinct from array, then the argument array is unchanged.
;; 
;; Note that if an array A is displaced to another array B, and B is displaced to another array C, and B is altered by adjust-array, A must now refer to the adjust contents of B. This means that an implementation cannot collapse the chain to make A refer to C directly and forget that the chain of reference passes through B. However, caching techniques are permitted as long as they preserve the semantics specified here.
;; 
;; Examples:
;;  (adjustable-array-p
;;   (setq ada (adjust-array
;;               (make-array '(2 3)
;;                           :adjustable t
;;                           :initial-contents '((a b c)
;;                                               (1 2 3)))
;;               '(4 6)))) =>  T 
;;  (array-dimensions ada) =>  (4 6) 
;;  (aref ada 1 1) =>  2 
;;  (setq beta (make-array '(2 3) :adjustable t))
;; =>  #2A((NIL NIL NIL) (NIL NIL NIL)) 
;;  (adjust-array beta '(4 6) :displaced-to ada)
;; =>  #2A((A B C NIL NIL NIL)
;;        (1 2 3 NIL NIL NIL)
;;        (NIL NIL NIL NIL NIL NIL) 
;;        (NIL NIL NIL NIL NIL NIL))
;;  (array-dimensions beta) =>  (4 6)
;;  (aref beta 1 1) =>  2 
;; Suppose that the 4-by-4 array in m looks like this:
;; 
;; #2A(( alpha     beta      gamma     delta )
;;     ( epsilon   zeta      eta       theta )
;;     ( iota      kappa     lambda    mu    )
;;     ( nu        xi        omicron   pi    ))
;; Then the result of
;;  (adjust-array m '(3 5) :initial-element 'baz)
;; is a 3-by-5 array with contents
;; #2A(( alpha     beta      gamma     delta     baz )
;;     ( epsilon   zeta      eta       theta     baz )
;;     ( iota      kappa     lambda    mu        baz ))
;;
;; }}}
;; Function ADJUSTABLE-ARRAY-P {{{
;;
;; Syntax:
;; adjustable-array-p array => generalized-boolean
;;
;; Arguments and Values:
;; array---an array.
;; generalized-boolean---a generalized boolean.
;;
;; Description:
;; Returns true if and only if adjust-array could return a value which is identical to array when given that array as its first argument.
;; 
;; Examples:
;;  (adjustable-array-p 
;;    (make-array 5
;;                :element-type 'character 
;;                :adjustable t 
;;                :fill-pointer 3)) =>  true
;;  (adjustable-array-p (make-array 4)) =>  implementation-dependent
;;
;; Exceptional Situations:
;; Should signal an error of type type-error if its argument is not an array.
;;
;; }}}



