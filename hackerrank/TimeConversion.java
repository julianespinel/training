import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

// https://www.hackerrank.com/challenges/time-conversion/problem
public class TimeConversion {

    private static final String PM = "PM";

    private static String readInput() throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            return reader.readLine();
        }
    }

    private static void printOutput(String result) {
        System.out.println(result);
    }

    private static String convertTo24HourTime(String twelveHourTime) {
        String hour = twelveHourTime.substring(0, 2);
        String amOrPm = twelveHourTime.substring(8);
        String minutesAndSeconds = twelveHourTime.substring(2, 8);
        if (hour.equalsIgnoreCase("12")) {
            return amOrPm.equalsIgnoreCase(PM) ? "12" + minutesAndSeconds : "00" + minutesAndSeconds;
        }
        String correctedHour = hour;
        if (amOrPm.equalsIgnoreCase(PM)) {
            correctedHour = String.valueOf(Integer.parseInt(hour) + 12);
        }
        return correctedHour + minutesAndSeconds;
    }

    public static void main(String[] args) throws IOException {
        String twelveHourTime = readInput();
        String twentyFourHourTime = convertTo24HourTime(twelveHourTime);
        printOutput(twentyFourHourTime);
    }
}
