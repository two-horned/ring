package egcd;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
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
		int a = fib(23);
		int b = fib(24);

		for (int i = 0; i < 100_000_000; i++) {
			EGCD.ring(a, b);
		}

		EGCD.Tuple<Integer> result = EGCD.ring(a, b);
		System.out.println(result.fst() + " and " + result.snd());
		assertEquals(1, result.fst() * a, result.snd() * b);
	}

	@Test
	@Order(1)
	void euclidFib1000() {
		int a = fib(23);
		int b = fib(24);

		for (int i = 0; i < 100_000_000; i++) {
			EGCD.euclid(a, b);
		}
		EGCD.Tuple<Integer> result = EGCD.euclid(a, b);
		assertEquals(1, result.fst() * a + result.snd() * b);
	}
}
