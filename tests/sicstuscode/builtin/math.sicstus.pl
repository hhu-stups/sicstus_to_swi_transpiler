test_comparison :-
	15 =:= 0b01111,
	0o17 =:= 0xf,
	0'A =:= 0'\x41\,
	0'\101\ =:= 65,
	3.0 =\= 187.6E12,
	-0.0234e15 < 12.0E-2,
	56 >= 5,
	5 > 3,
	45 =< 67.

test_arithmetic :-
	A is +4-4,
	B is A+34,
	B is -B+68,
	6.8 is 3.4*2,
	0.9 is 4.5/5,
	15 is 78//5,
	13 is 78 div 6,
	4 is -8 mod 6,
	0 is 78 rem 6,
	0 is integer(-0.8),
	float_integer_part(-0.8) =:= 0,
	float_fractional_part(-1.8) =:= -0.8,
	5 is 0b111/\0b1101,
	15 is 0b111\/0b1101,
	6 is xor(3, 5),
	-8 is \(-3, 5),
	5 is \(-6),
	80 is 5 << 4,
	-3250 is -13000 >> 2,
	8 is "r"-"j",
	4 is abs(-4),
	-1 is sign(-8),
	8 is gcd(-8, 16),
	4 is min(4, 6),
	8 is max(3, 8),
	3 is msb(9),
	-4 is round(-4.5),
	0 is ceiling(-0.1),
	1.0 is asin(sin(1)),
	0.2500000000000002 is acos(cos(1/4)),
	0.25 is atan(tan(1/4)),
	1.0000000000000002 is cot(acot(1)),
	1.0 is asinh(sinh(1)),
	1.0 is acosh(cosh(1)),
	0.9999999999999999 is atanh(tanh(1)),
	1.0 is acoth(coth(1)),
	0.7853981633974483 is atan2(1, 1),
	4.0 is sqrt(16),
	2.0794415416798357 is log(8),
	4.0 is log(2, 16),
	54.598150033144236 is exp(4),
	0.015625 is 4**(-3),
	0.015625 is exp(4, -3),
	31.54428070019754 is 3^pi.

	test_integer :-
		A = integer(-0.6),
		1.0 =:= sin(A)+1.

	test_xor :-
		11 >=  pi+ \(3,4).

	test_floor :-
		round(-5.5)+1 > -5.

	test_log :-
		log(2, 4)=\= 4.
