//
//  main.cpp
//  CppTemplFun
//
//  Created by Thomas Dickerson on 4/12/18.
//  Copyright © 2018 Brown. All rights reserved.
//

#include <iostream>
#include <type_traits>

struct LessThanTag {};
struct GreaterThanTag {};

template<class ComparatorTag> struct GenComparator {};

template<> struct GenComparator<LessThanTag> {
	template <class L, class R> struct templ {
		constexpr static const bool value = L::value < R::value;
	};
};
template<> struct GenComparator<GreaterThanTag> {
	template <class L, class R> struct templ {
		constexpr static const bool value = L::value > R::value;
	};
};


template<bool If, class ThenT, class ElseT> using Cond = std::conditional<If, ThenT, ElseT>;

template<template<class L, class R> class Compare, class L, class R> using FirstByOrder = Cond<Compare<L,R>::value, L, R>;



template<template<class L, class R> class BinOp, class Init, class ...Args> struct Fold {
	using type = Init;
};

template<template<class L, class R> class BinOp, class Init, class Head, class ...Tail> struct Fold<BinOp, Init, Head, Tail...> {
	using type = typename Fold<BinOp, typename BinOp<Init, Head>::type, Tail...>::type;
};




namespace foos {
	struct SmallFoo {
		constexpr static const size_t value = 0;
	};
	
	struct BigFoo {
		constexpr static const size_t value = 1;
	};
	
	struct BiggestFoo {
		constexpr static const size_t value = 3;
	};
};


template<class L, class R> using GreaterThan = GenComparator<GreaterThanTag>::template templ<L, R>;
template<class L, class R> using Max = FirstByOrder<GreaterThan, L, R>;

int main() {
	
	using TestCond = typename Cond<true, foos::SmallFoo, foos::BiggestFoo>::type;
	using TestMax = typename Max<foos::SmallFoo, foos::BiggestFoo>::type;
	using BiggerFoo = typename Fold<Max, foos::SmallFoo, foos::BigFoo, foos::BiggestFoo>::type;

	std::cout << TestCond::value << std::endl;
	std::cout << TestMax::value << std::endl;
	std::cout << BiggerFoo::value << std::endl;
	return 0;
}