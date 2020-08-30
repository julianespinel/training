import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// https://www.hackerrank.com/challenges/angry-professor/problem
public class AngryProfessor {

    private static final String SPACE = " ";

    enum Answer {
        YES, NO
    }

    private class Case {

        private final int totalStudents;
        private final int minStudentsOnTime;
        private final int[] arrivals;

        public Case(int totalStudents, int minStudentsOnTime, int[] arrivals) {
            this.totalStudents = totalStudents;
            this.minStudentsOnTime = minStudentsOnTime;
            this.arrivals = arrivals;
        }

        private void validate() {
            if (totalStudents != arrivals.length) {
                String errorMessage = String.format("Total students expected to be %s, but is %s",
                    totalStudents, arrivals.length);
                throw new AssertionError(errorMessage);
            }
        }

        private int countEarly() {
            int studentsOnTime = 0;
            for (int arrival : arrivals) {
                studentsOnTime += (arrival <= 0) ? 1 : 0;
            }
            return studentsOnTime;
        }

        private Answer isClassCancelled() {
            int studentsOnTime = countEarly();
            return (studentsOnTime >= minStudentsOnTime) ? Answer.NO : Answer.YES;
        }

        public Answer solve() {
            validate();
            return isClassCancelled();
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

    private List<Answer> solve() throws IOException {
        List<Answer> answers = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int totalCases = Integer.parseInt(reader.readLine());
            for (int i = 0; i < totalCases; i++) {
                Case aCase = readCase(reader);
                answers.add(aCase.solve());
            }
        }
        return answers;
    }

    public static void main(String[] args) throws IOException {
        AngryProfessor angryProfessor = new AngryProfessor();
        List<Answer> answers = angryProfessor.solve();
        for (Answer answer : answers) {
            System.out.println(answer);
        }
    }
}
