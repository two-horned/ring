package egcd;

public class EGCD {
	static Tuple<Integer> euclid(int a, int b) {
		int signumA = pseudoSig(a);
		int signumB = pseudoSig(b);

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

		int s = signumA * oldS;
		int t = b != 0 ? signumB * (oldR - s * a) / b : 0;

		Tuple<Integer> result = new Tuple<>(s, t);
		return result;
	}

	static Tuple<Integer> ring(int a, int b) {
		int signumA = pseudoSig(a);
		int signumB = pseudoSig(b);

		int invR = Math.abs(a);
		int theR = Math.abs(b);

		int newS = 1;
		int oldS = 0;

		int newT = 0;
		int oldT = 1;

		int quot;
		int temp;

		boolean flipped = false;
		while (invR != 0) {
			if (theR < invR) {
				temp = theR;
				theR = invR;
				invR = temp;

				flipped = !flipped;
			} else {
				quot = theR / invR;
				theR = theR - quot * invR;
				invR = invR - theR;

				temp = newT;
				if (flipped) {
					newT = oldS - quot * oldT;
					oldT = temp;

					temp = newS;
					newS = oldT - newT;
					oldS = temp;
				} else {
					newT = oldT - quot * oldS;
					oldT = temp;

					temp = newS;
					newS = oldS - newT;
					oldS = temp;
				}
			}
		}

		int s = signumA * oldS;
		int t = signumB * oldT;

		Tuple<Integer> result = new Tuple<>(s, t);

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
