import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

// https://www.hackerrank.com/challenges/jumping-on-the-clouds/problem
public class JumpingOnTheClouds {

    private static final String SPACE = " ";
    private static final int SENTINEL = -1;
    private static final int CUMULUS = 0;

    private static int[] readClouds() throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            reader.readLine(); // ignore first line
            String[] split = reader.readLine().split(SPACE);
            int[] clouds = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
            return clouds;
        }
    }

    private static int jumpClouds(int index, int jumps, int[] clouds) {
        int updatedIndex = index + jumps;
        if (updatedIndex >= clouds.length) {
            return SENTINEL;
        }
        int cloud = clouds[updatedIndex];
        return (cloud == CUMULUS) ? updatedIndex : SENTINEL;
    }

    private static int getMinStepsToLastCloud(int[] clouds) {
        int steps = 0;
        int i = 0;
        while (i < clouds.length) {
            if (i == clouds.length - 1) {
                return steps;
            }
            int cloud = clouds[i];
            if (cloud == 1) {
                throw new IllegalStateException("Current cloud could not be 1. Index is: " + i);
            }
            int updatedIndex = jumpClouds(i, 2, clouds);
            i = (updatedIndex != SENTINEL) ? updatedIndex : jumpClouds(i, 1, clouds);
            steps += 1;
        }
        return steps;
    }

    public static void main(String[] args) throws IOException {
        int[] clouds = readClouds();
        int minSteps = getMinStepsToLastCloud(clouds);
        System.out.println(minSteps);
    }
}
