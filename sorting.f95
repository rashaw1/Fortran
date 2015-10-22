PROGRAM sortTest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Purpose: To test several sorting algorithms !!
!! Date: 09/08/15                              !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
! Parameters
INTEGER, PARAMETER :: LIST_SIZE = 10000
! Variable declarations
REAL, DIMENSION(8) :: vals = (/ 12., -5.6, 1.8, 1.9, -4.3, 9., 14., 1.2 /)
REAL, DIMENSION(LIST_SIZE) :: vals2, temp
REAL :: start, finish
LOGICAL :: ordered
INTEGER :: err, i

CALL RANDOM_SEED
CALL RANDOM_NUMBER(vals2)

WRITE (*, *) ' Heap sort: '
temp(1:LIST_SIZE) = vals2(1:LIST_SIZE)
CALL CPU_TIME(start)
CALL heapSort(temp, LIST_SIZE)
CALL CPU_TIME(finish)
CALL checkOrder(temp, LIST_SIZE, ordered)
WRITE (*, 100) ordered, finish-start
100 FORMAT(1X, 'The array is sorted: ', L5, /,&
           1X, 'The algorithm took ', F12.5, ' seconds.')

WRITE (*, *) ' Selection sort: '
temp(1:LIST_SIZE) = vals2(1:LIST_SIZE)
CALL CPU_TIME(start)
CALL selectionSort(temp, LIST_SIZE)
CALL CPU_TIME(finish)
CALL checkOrder(temp, LIST_SIZE, ordered)
WRITE (*, 100) ordered, finish-start

WRITE (*, *) ' Insertion sort: '
temp(1:LIST_SIZE) = vals2(1:LIST_SIZE)
CALL CPU_TIME(start)
CALL insertionSort(temp, LIST_SIZE)
CALL CPU_TIME(finish)
CALL checkOrder(temp, LIST_SIZE, ordered)
WRITE (*, 100) ordered, finish-start

WRITE (*, *) ' Bubble sort: '
temp(1:LIST_SIZE) = vals2(1:LIST_SIZE)
CALL CPU_TIME(start)
CALL bubbleSort(temp, LIST_SIZE)
CALL CPU_TIME(finish)
CALL checkOrder(temp, LIST_SIZE, ordered)
WRITE (*, 100) ordered, finish-start

WRITE (*, *) ' Merge sort: '
temp(1:LIST_SIZE) = vals2(1:LIST_SIZE)
CALL CPU_TIME(start)
CALL mergeSort(temp, LIST_SIZE, LIST_SIZE/2)   
CALL CPU_TIME(finish)
CALL checkOrder(temp, LIST_SIZE, ordered)
WRITE (*, 100) ordered, finish-start

WRITE (*, *) ' Quick sort: '
temp(1:LIST_SIZE) = vals2(1:LIST_SIZE)
CALL CPU_TIME(start)
CALL quickSort(temp, LIST_SIZE)                                               
CALL CPU_TIME(finish)
CALL checkOrder(temp, LIST_SIZE, ordered)
WRITE (*, 100) ordered, finish-start

END PROGRAM sortTest

!************************************************

SUBROUTINE checkOrder(arr, n, isOrdered)
!
!  Purpose: check that the array arr is in ascending order
!

! Arguments
INTEGER, INTENT(IN) :: n ! Size of array
REAL, DIMENSION(n), INTENT(INOUT) :: arr
LOGICAL, INTENT(OUT) :: isOrdered

! Local variables
INTEGER :: i
isOrdered = .TRUE.
main: DO i = 2, n
   IF (arr(i-1) > arr(i)) THEN
      isOrdered = .FALSE.
      EXIT main
   END IF
END DO main
END SUBROUTINE checkOrder

!*************************************************

SUBROUTINE swap (arr, n, pos1, pos2)
!
!  Purpose: swaps elements at pos1 and pos2 of array arr
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n, pos1, pos2 ! Size of array, and swap indices
REAL, DIMENSION(n), INTENT(INOUT) :: arr

! Local variables
REAL :: temp ! Placeholder variable

bounds: IF ((pos1 > n) .OR. (pos2 > n)&
           .OR. (pos1 < 0) .OR. (pos2 < 0)) THEN
ELSE bounds
   temp = arr(pos1)
   arr(pos1) = arr(pos2)
   arr(pos2) = temp
END IF bounds
END SUBROUTINE swap

!****************************************************

SUBROUTINE insertionSort (arr, n)
!
!  Purpose: sorts the array arr into ascending order
!           using an insertion sort algorithm
!
!  Algorithm: Start with list of n elements, i=2
!           1. Left-list : right-list have i-1 : n-i+1 items
!           2. Compare first item of right-list with
!              each item of left-list in reverse order.
!           3. Swap if smaller than that item - if bigger,
!              then it is in the correct position.
!           4. Increment i.
!           5. Repeat steps 1 - 4 until end of list is reached.
!
IMPLICIT NONE

! Arguments
INTEGER, INTENT(IN) :: n ! Number of values in array
REAL, DIMENSION(n), INTENT(INOUT) :: arr !Array to be sorted
! Local variables
INTEGER :: i, k ! Index counters

! Start algorithm
outer: DO i=2, n
   inner: DO k=i, 2, -1
      ! Insert first value in the right-list into 
      ! correct position in the left-list
      IF (arr(k) < arr(k-1)) THEN 
         CALL swap(arr, n, k, k-1)
      ELSE
         CYCLE outer
      END IF
   END DO inner
END DO outer

END SUBROUTINE insertionSort

!*************************************************
SUBROUTINE selectionSort (arr, n)
!
!  Purpose: to sort the array arr using the selection sort
!           algorithm
!
!  Algorithm: Start with array of n elements. i=1
!            Let k denote the position of the smallest element.
!           1. The first i-1 elements are sorted. Set k=i.
!           2. Compare each of the elements i through n pairwise.
!           3. Set k to the position of the smaller element each time -
!              this way the smallest element is found.
!           4. Swap element k with element i, increment i.
!           5. Repeat steps 1-4 until end of list is reached.
!           
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n ! Size of arr
REAL, DIMENSION(n), INTENT(INOUT) :: arr ! Array of values

! Local variables
INTEGER :: i, j, k ! Counting indices

! Begin algorithm
outer: DO i=1, n
   k = i
   ! Find the smallest element
   inner: DO j=i+1, n
      IF (arr(j) < arr(k)) THEN
         k = j
      END IF 
   END DO inner
   CALL swap(arr, n, i, k) ! Swap with front of sub-list
END DO outer
END SUBROUTINE selectionSort

!************************************************

SUBROUTINE bubbleSort(arr, n)
!
!  Purpose: to sort array arr using the (reverse) bubble sort algorithm
!           This is pretty much always the slowest of the sorting algorithms!
!
!  Algorithm: Start with array of n elements. Set i = 1.
!            1. Compare each element pairwise, in reverse, from n to i.
!            2. If right-element is smaller, swap. This way, small
!               elements `bubble' down the list, or equivalently, 
!               big elements `bubble' up. In particular, for this version,
!               the small-est element ends up at the left.
!            3. Increment i, and repeat until end of list is reached.
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n ! Size of arr
REAL, DIMENSION(n), INTENT(INOUT) :: arr

! Local variables
INTEGER :: i, j ! Counting indices
LOGICAL :: swapped ! Flag for whether swap occurred

! Begin algorithm
outer: DO i = 1, n
   swapped = .FALSE. ! If this remains false, arr is sorted
   inner: DO j = n, i+1, -1
      ! Smallest value bubbles down to bottom
      ! Then can repeat on sub-list with that element removed
      IF (arr(j) < arr(j-1)) THEN
         CALL swap(arr, n, j, j-1)
         swapped = .TRUE.
      END IF
   END DO inner
   IF ( .NOT. swapped ) THEN
      EXIT outer
   END IF 
END DO outer
END SUBROUTINE bubbleSort

!***********************************************
!
! For the following three subroutines, note that a heap is defined
! as a binary tree with two additional properties:
!    Shape - all levels of the tree are filled, except possibly the last,
!            in which case, the level is partially-filled from left to right.
!    Heapness - all nodes are greater than or equal to each of their children.
!

SUBROUTINE sink(arr, n, start, end)
!
!  Purpose: this is necessary for `repairing' the heap structure
!           in heapify and the heap-sort algorithm - i.e. enforces
!           the heapness property above.
!
!  Algorithm:  Start with heap of n elements.
!              Set the root element as start. Let s = root.
!             1. If left-child > s, set s = left-child.
!             2. If right-child is before end, and s < right-child,
!                set s = right-child. 
!             3. If s = root still, then the heap is valid.
!             4. Otherwise, swap values of s and root, and
!                set root = s.
!             5. Repeat until root has no more children, or end is reached.
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n, start, end
REAL, DIMENSION(n), INTENT(INOUT) :: arr

! Local variables
INTEGER :: root, s, child
root = start ! Root node is at position start
! Start algorithm
! Loop until no more children on root node.
main: DO 
   child = root * 2 ! This is the position of the left-child
   s = root ! This keeps track of the child to swap with
   
   ! Check left-child
   lchild: IF (arr(s) < arr(child)) THEN
      s = child
   END IF lchild
   ! Check right-child, if it exists - the right child should be swapped
   ! preferentially, so as to maintain left-to-right heap structure.
   rchild: IF ((child+1 <= end) .AND. (arr(s) < arr(child+1))) THEN
      s = child + 1
   END IF rchild
   ! If swap = root, then heap is correctly structured (assuming children
   ! are already correctly heaped).
   needswap: IF (s == root) THEN
      EXIT main
   ! Otherwise, swap the appropriate child with the root
   ELSE needswap
      CALL swap(arr, n, root, s)
      root = s ! Continue sinking with the child
   END IF needswap

   ! Exit while loop, if the end is reached.
   endcheck: IF (root * 2 >= end) THEN
      EXIT main
   END IF endcheck
END DO main

END SUBROUTINE sink

!***********************************************

SUBROUTINE heapify(arr, n)
!
!  Purpose: turns the list arr into a heap data-structure
!           for use by the heap-sort algorithm
! 
!  Algorithm: Start with array of n values. Last value of array is
!             at n-1. Parent of this element should be at
!             start = floor((n-1) / 2). While start > 0:
!                 1. `sink' down the node at start until all nodes below
!                    are in heap order.
!                 2. Decrement start and repeat.
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n
REAL, DIMENSION(n), INTENT(INOUT) :: arr
! Local variables
INTEGER :: start
! Begin algorithm
DO start=(n/2), 1, -1
   CALL sink(arr, n, start, n)
END DO

END SUBROUTINE heapify

!***********************************************

SUBROUTINE heapSort(arr, n) 
!
!  Purpose: to sort the array arr using a heapsort algorithm.
!           UNSTABLE, but much quicker than any of the above
!           Second only to quicksort.
!
!  Algorithm: Start with array of n elements. Set i = n. 
!             1. Heapify the array.
!             2. Swap the root element and element i.
!             3. Repair the now-broken heap structure
!             4. Decrement i, and repeat until end of array.
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n 
REAL, DIMENSION(n), INTENT(INOUT) :: arr

! Local variables
INTEGER :: i
REAL :: temp

!Heap it up
CALL heapify(arr, n)

! Sort
DO i=n, 2, -1
   CALL swap(arr, n, 1, i)
   CALL sink(arr, n, 1, i-1)
END DO

temp = arr(1)
arr(1) = arr(2)
arr(2) = temp

END SUBROUTINE heapSort

!************************************************

RECURSIVE SUBROUTINE mergeSort(arr, n, m)
!
!  Purpose: to sort the array arr in ascending order using
!           a merge sort algorithm. 
!           When stability is required, this is almost always
!           the best sorting algorithm.
!
!  Algorithm: Start with an array of n values.
!             1. Set m = n/2
!             2. Sort arr(1 : m), sort arr(m+1 : n)
!             3. Set b = copy of arr(1 : m)
!             4. Set i = k = 1, j = m+1
!             5. Set arr(k) to be the lesser of arr[j] and b[i]
!             6. Increment k, and whichever of j, i was lesser.
!             7. Repeat steps 5 and 6 until i = m and j = n.
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n, m 
REAL, DIMENSION(n), INTENT(INOUT) :: arr
! Local variables
INTEGER :: i, j, k ! Counter indices
REAL, DIMENSION(m) :: b ! Temp array for first half
REAL, DIMENSION(n-m) :: c ! Temp array for second half
IF (n==1) THEN
   RETURN
END IF 
! Deep copies into temp arrays
b(1 : m) = arr(1 : m) 
c(1 : n-m) = arr(m+1 : n)
! Recursive steps
CALL mergeSort(b, m, m/2)
CALL mergeSort(c, n-m, (n-m)/2)

! Merge steps
i = 1 ! Counter for b
j = 1 ! Counter for c
k = 1 ! Counter for return array

main: DO
   IF (c(j) < b(i)) THEN
      arr(k) = c(j)
      j = j+1
   ELSE 
      arr(k) = b(i)
      i = i+1
   END IF
   k = k+1
   IF ( (i > m) .OR. (j > n-m) .OR. (k > n) ) THEN
      EXIT main
   END IF
END DO main
! Copy any remaining values into return array
IF ( i > m ) THEN
   DO 
      arr(k) = c(j)
      j = j+1
      k = k+1
      IF ( j > n-m) THEN
         EXIT
      END IF
   END DO
ELSE IF ( j > n-m ) THEN
   DO
      arr(k) = b(i)
      i = i+1
      k = k+1
      IF ( i > m) THEN
         EXIT
      END IF
   END DO
END IF
END SUBROUTINE mergeSort

!****************************************************

RECURSIVE SUBROUTINE quickSort(arr, n)
!
!  Purpose: to sort in ascending order an array arr using the
!           the quick sort algorithm with 3-way partitioning
!           UNSTABLE, but for most purposes, this is by far
!           the optimal method - hence the fitting name.
!
!  Algorithm: Start with an array of n elements. 
!             1. Choose a pivot, by swapping arr(n) with a random element of arr
!             2. Set i = k = 1, p = n
!             3. If arr(i) < arr(n), swap arr(i) and arr(k). Increment i and k.
!                Else, if arr(i) = arr(n), swap arr(i) and arr(p-1). Decrement p.
!                Else, increment i.
!             4. Repeat 3 until i = p.
!             5. Move pivots to the centre - set m = min(p-k, n-p+1),
!                and swap arr(k : k+m-1) and arr(n-m+1 : n)
!             6. Sort arr(1 : k - 1), sort arr(n-p+k+1 : n)
!
IMPLICIT NONE
! Arguments
INTEGER, INTENT(IN) :: n
REAL, DIMENSION(n), INTENT(INOUT) :: arr
! Local variables
INTEGER :: i, k, p, m, j
REAL, DIMENSION(n) :: b, c ! Temp arrays
REAL :: random_real ! Random number
INTEGER :: random_int ! Integer version for index

IF (n==1) THEN
   RETURN
END IF
! Choose pivot
CALL RANDOM_SEED
CALL RANDOM_NUMBER(random_real) ! Returns number between 0 and 1
random_int = CEILING(random_real * n) ! Scale random to an integer at most n
CALL swap(arr, n, n, random_int) ! Swap pivot value
! 3-way partition
i = 1
k = 1
p = n
main: DO
   IF (arr(i) < arr(n)) THEN
      CALL swap(arr, n, i, k)
      i = i + 1
      k = k + 1
   ELSE IF (arr(i) - arr(n) < 1E-12) THEN
      CALL swap(arr, n, i, p-1)
      p = p - 1
   ELSE 
      i = i + 1
   END IF
   IF ( i >= p ) THEN 
      EXIT main
   END IF
END DO main
! Move pivots to center
m = MIN(p-k, n-p+1)
IF (m > 0) THEN
   DO j = 1, m
      CALL swap(arr, n, j+k-1, n-m+j)
   END DO
END IF
! Recursive sorts
IF (k>1) THEN
   CALL quickSort(arr(1:k-1), k-1)
END IF
IF (m>0) THEN
   CALL quickSort(arr(n-p+k+1: n), p-k) 
END IF
END SUBROUTINE quickSort
