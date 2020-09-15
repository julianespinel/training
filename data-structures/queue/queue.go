package queue

// Node in a linked list.
type node struct {
	value interface{}
	next  *node
}

// Queue implemented using a linked list.
// The unit tests are in file: queue_tests.go
//
// Complexity analysis of the queue operations:
//
// Time:
// * Enqueue: O(1), add an element at the end of the queue.
// * Dequeue: O(1), remove the first element of the queue.
// * Search:  O(n), traverse the queue until you find the required element.
// 				    In the worst case scenario you need to traverse the whole queue,
// 				    therefore the time complexity is O(n)
//
// Space:
// The space required to hold the queue in memory is O(n).
type Queue struct {
	size  int
	first *node
	last  *node
}

// isEmpty returns true if the queue is empty, otherwise returns false.
func (q *Queue) isEmpty() bool {
	return q.first == nil
}

// Enqueue adds an element at the end of the queue.
func (q *Queue) Enqueue(value interface{}) {
	n := &node{value: value, next: nil}
	if q.isEmpty() {
		q.first = n
		q.last = n
	} else {
		q.last.next = n
		q.last = n
	}
	q.size++
}

// Dequeue returns the first element and removes it from the queue.
func (q *Queue) Dequeue() interface{} {
	if q.isEmpty() {
		return nil
	}
	result := q.first
	q.first = result.next
	q.size--
	return result.value
}

// Size returns number of elements in the queue.
func (q *Queue) Size() int {
	return q.size
}
