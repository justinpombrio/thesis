// Stresses the compiler infinitely
// from: http://www.fefe.de/c++/c%2b%2b-talk.pdf
template<class T> struct Loop { Loop<T*> operator->(); };
Loop<int> i, j = i->hooray;
