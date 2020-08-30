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

        public int getTotalStudents() {
            return totalStudents;
        }

        public int getMinStudentsOnTime() {
            return minStudentsOnTime;
        }

        public int[] getArrivals() {
            return arrivals;
        }
    }

    private void validate(Case aCase) {
        int totalStudents = aCase.getTotalStudents();
        int[] arrivals = aCase.getArrivals();
        if (totalStudents != arrivals.length) {
            String errorMessage = String.format("Total students expected to be %s, but is %s",
                totalStudents, arrivals.length);
            throw new AssertionError(errorMessage);
        }
    }

    private int countEarly(Case aCase) {
        int studentsOnTime = 0;
        for (int arrival : aCase.getArrivals()) {
            studentsOnTime += (arrival <= 0) ? 1 : 0;
        }
        return studentsOnTime;
    }

    private Answer isClassCancelled(Case aCase) {
        int studentsOnTime = countEarly(aCase);
        return (studentsOnTime >= aCase.getMinStudentsOnTime()) ? Answer.NO : Answer.YES;
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
                validate(aCase);
                answers.add(isClassCancelled(aCase));
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
