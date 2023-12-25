package egcd;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;

import gcd.GCD;

import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(OrderAnnotation.class)
class EGCDTest {
	private int fib(int n) {
		if (n < 0)
			return 0;
		int a = 0;
		int b = 1;
		int c;
		while (0 < n--) {
			c = a;
			a = b;
			b = b + c;
		}
		return b;
	}

	@Test

	@Order(2)
	void ringFib1000() {
		int a = fib(21);
		int b = fib(22);

		for (int i = 0; i < 100_000_000; i++) {
			EGCD.ring(a, b);
		}
		EGCD.Tuple<Integer> result = EGCD.ring(a, b);
		assertEquals(GCD.ring(a,b), result.fst() * a + result.snd() * b);

	}

	@Test
	@Order(1)
	void euclidFib1000() {
		int a = fib(21);
		int b = fib(22);

		for (int i = 0; i < 100_000_000; i++) {
			EGCD.euclid(a, b);
		}
		EGCD.Tuple<Integer> result = EGCD.euclid(a, b);
		assertEquals(GCD.euclid(a, b), result.fst() * a + result.snd() * b);
	}
}
