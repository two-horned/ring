package gcd;

public class GCD {
	public static int euclid(int a, int b) {
		a = Math.abs(a);
		b = Math.abs(b);

		int temp;
		while (a != 0) {
			temp = a;
			a = b % a;
			b = temp;
		}
		return b;
	}

	public static int ring(int a, int b) {
		a = Math.abs(a);
		b = Math.abs(b);

		int temp;
		while (a != 0) {
			if (b < a) {
				temp = a;
				a = b;
				b = temp;
			} else {
				b = b % a;
				a = a - b;
			}
		}
		return b;
	}
}
