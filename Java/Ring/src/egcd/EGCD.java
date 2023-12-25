package egcd;

public class EGCD {
	static Tuple<Integer> euclid(int a, int b) {
		int signumA = pseudoSig(a);
		
		if (b == 0) {
			return new Tuple<Integer>(signumA,0);
		}

		int newR = Math.abs(a);
		int oldR = Math.abs(b);

		int newS = 1;
		int oldS = 0;

		int quot;
		int temp;

		while (newR != 0) {
			quot = oldR / newR;

			temp = newR;
			newR = oldR - quot * newR;
			oldR = temp;

			temp = newS;
			newS = oldS - quot * newS;
			oldS = temp;
		}

		newS = signumA * oldS;            // use unused variable for s
		oldS = (oldR - newS * a) / b;     // use unused variable for t

		Tuple<Integer> result = new Tuple<>(newS, oldS);
		return result;
	}

	static Tuple<Integer> ring(int a, int b) {
		int signumB = pseudoSig(b);
		
		if (a == 0) {
			return new Tuple<Integer>(0,signumB);
		}
		
		
		int tempA = Math.abs(a);
		int tempB = Math.abs(b);
		
		int temp;
		int quot;
		
		int tempT1 = 0;
		int tempT2 = 1;
		while (tempA != 0) {
			if (tempB < tempA) {
				temp = tempA;
				tempA = tempB;
				tempB = temp;
				
				temp = tempT1;
				tempT1 = tempT2;
				tempT2 = temp;
			} else {
				quot = tempB / tempA;
				tempB -= quot * tempA;
				tempA -= tempB;
				
				temp = tempT1 * quot;
				tempT1 += temp - tempT2;
				tempT2 -= temp;
			}
		}
		
		tempT1 = (tempB - tempT2 * Math.abs(b)) / a; // use unused variable for s
		tempT2 *= signumB;                   // use unused variable for t
		
		Tuple<Integer> result = new Tuple<>(tempT1, tempT2);
		return result;
	}

	static int pseudoSig(int x) {
		return x < 0 ? -1 : 1;
	}

	static class Tuple<T> {
		private T fst;
		private T snd;

		Tuple(T fst, T snd) {
			this.fst = fst;
			this.snd = snd;
		}

		T fst() {
			return fst;
		}

		T snd() {
			return snd;
		}
	}
}
