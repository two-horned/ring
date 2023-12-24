package gcd;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(OrderAnnotation.class)
class MainTest {
	private int fib(int n) {
		if(n < 0)
			return 0;
		int a = 0;
		int b = 1;
		int c;
		while(0 < n--) {
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
		
		for(int i=0; i<100_000_000; i++) {
			Main.ring(a, b);
		}
		
		assertEquals(1, Main.ring(a, b));
	}

	@Test
	@Order(2)
	void euclidFib1000() {
		int a = fib(44);
		int b = fib(45);
		
		for(int i=0; i<100_000_000; i++) {
			Main.euclid(a, b);
		}
		
		assertEquals(1, Main.euclid(a, b));
	}
}
