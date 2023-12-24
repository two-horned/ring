package gcd;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(OrderAnnotation.class)
class GCDTest {
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
	@Order(1)
	void ringFib1000() {
		int a = fib(44);
		int b = fib(45);

		for (int i = 0; i < 100_000_000; i++) {
			GCD.ring(a, b);
		}

		assertEquals(1, GCD.ring(a, b));
	}

	@Test
	@Order(2)
	void euclidFib1000() {
		int a = fib(44);
		int b = fib(45);

		for (int i = 0; i < 100_000_000; i++) {
			GCD.euclid(a, b);
		}

		assertEquals(1, GCD.euclid(a, b));
	}
}
