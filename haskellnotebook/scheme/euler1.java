
public class euler1 {
	public static int euler1(final int n) {
		int sum = 0;
		for (int i = 0; i < n; i++) {
			if (((i % 3) == 0) || ((i % 5) == 0)) {
				sum += i;
			}
		}
		return sum;
	}
	public static void main(final String [] args) {
		System.out.println("Running");
		System.out.println("-->" + euler1(1000));
 	}
} 
