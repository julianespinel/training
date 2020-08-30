import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// https://www.hackerrank.com/challenges/angry-professor/problem
public class AngryProfessor {

    private static final String SPACE = " ";

    private class Case {

        private final int minStudentsOnTime;
        private final int[] arrivals;

        public Case(int totalStudents, int minStudentsOnTime, int[] arrivals) {
            this.minStudentsOnTime = minStudentsOnTime;
            this.arrivals = arrivals;

            if (totalStudents != arrivals.length) {
                String errorMessage = String.format("Total students expected to be %s, but is %s",
                    totalStudents, arrivals.length);
                throw new AssertionError(errorMessage);
            }
        }

        private int getStudentsOnTime() {
            int studentsOnTime = 0;
            for (int arrival : arrivals) {
                studentsOnTime += (arrival <= 0) ? 1 : 0;
            }
            return studentsOnTime;
        }

        String classWillBeCancelled() {
            int studentsOnTime = getStudentsOnTime();
            return (studentsOnTime >= minStudentsOnTime) ? "NO" : "YES";
        }
    }

    private Case readCase(BufferedReader reader) throws IOException {
        String[] split = reader.readLine().split(SPACE);
        int totalStudents = Integer.parseInt(split[0]);
        int minStudentsOnTime = Integer.parseInt(split[1]);
        String[] arrivalsStr = reader.readLine().split(SPACE);
        int[] arrivals = Arrays.stream(arrivalsStr).mapToInt(Integer::parseInt).toArray();
        return new Case(totalStudents, minStudentsOnTime, arrivals);
    }

    private List<Case> readCases() throws IOException {
        List<Case> cases = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int totalCases = Integer.parseInt(reader.readLine());
            for (int i = 0; i < totalCases; i++) {
                Case inputCase = readCase(reader);
                cases.add(inputCase);
            }
        }
        return cases;
    }

    public static void main(String[] args) throws IOException {
        AngryProfessor angryProfessor = new AngryProfessor();
        List<Case> cases = angryProfessor.readCases();
        for (Case inputCase : cases) {
            System.out.println(inputCase.classWillBeCancelled());
        }
    }
}
