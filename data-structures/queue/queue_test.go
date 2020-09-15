package queue

import (
	"testing"
)

func TestEnqueueToEmptyQueue(t *testing.T) {
	q := Queue{}
	verifyInt(t, q.Size(), 0)
	q.Enqueue(1)
	verifyInt(t, q.Size(), 1)
}

func TestEnqueueToNonEmptyQueue(t *testing.T) {
	q := Queue{}
	q.Enqueue(1)
	q.Enqueue(2)
	q.Enqueue(3)
	q.Enqueue(4)
	q.Enqueue(5)
	verifyInt(t, q.Size(), 5)
}

func TestDequeueEmptyQueue(t *testing.T) {
	q := Queue{}
	verifyInt(t, q.Size(), 0)

	element := q.Dequeue()
	if element != nil {
		t.Errorf("Element should be nil, it's %s", element)
	}

	// Size does not go below zero
	verifyInt(t, q.Size(), 0)
}

func TestDequeueNonEmptyQueue(t *testing.T) {
	q := Queue{}
	verifyInt(t, q.Size(), 0)

	q.Enqueue(1)
	q.Enqueue(2)
	q.Enqueue(3)
	q.Enqueue(4)
	q.Enqueue(5)
	verifyInt(t, q.Size(), 5)

	one := q.Dequeue()
	verifyInt(t, one, 1)
	verifyInt(t, q.Size(), 5-1)

	two := q.Dequeue()
	verifyInt(t, two, 2)
	verifyInt(t, q.Size(), 5-2)

	three := q.Dequeue()
	verifyInt(t, three, 3)
	verifyInt(t, q.Size(), 5-3)

	four := q.Dequeue()
	verifyInt(t, four, 4)
	verifyInt(t, q.Size(), 5-4)

	five := q.Dequeue()
	verifyInt(t, five, 5)
	verifyInt(t, q.Size(), 5-5)

	// Size does not go below zero
	q.Dequeue()
	q.Dequeue()
	q.Dequeue()
	q.Dequeue()
	verifyInt(t, q.Size(), 0)
}

func verifyInt(t *testing.T, real interface{}, expected int) {
	realInt := real.(int)
	if realInt != expected {
		t.Errorf("Expected %d, real %d", expected, real)
	}
}
