package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import org.junit.Test;

public class DateUtilsDiffblueTest {
  /**
  * Method under test: {@link DateUtils#format(long, String)}
  */
  @Test
  public void testFormat() {
    // Arrange, Act and Assert
    assertEquals("+", DateUtils.format(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, "+"));
  }

  /**
   * Method under test: {@link DateUtils#format(Date, String)}
   */
  @Test
  public void testFormat2() {
    // Arrange
    LocalDateTime atStartOfDayResult = LocalDate.of(1970, 1, 1).atStartOfDay();

    // Act and Assert
    assertEquals("+", DateUtils.format(Date.from(atStartOfDayResult.atZone(ZoneId.of("UTC")).toInstant()), "+"));
  }

  /**
   * Method under test: {@link DateUtils#formatElapsedTime(long)}
   */
  @Test
  public void testFormatElapsedTime() {
    // Arrange, Act and Assert
    assertEquals("0 seconds", DateUtils.formatElapsedTime(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Method under test: {@link DateUtils#getPhaseOfMoon(Calendar)}
   */
  @Test
  public void testGetPhaseOfMoon() {
    // Arrange, Act and Assert
    assertEquals(4, DateUtils.getPhaseOfMoon(new GregorianCalendar(1, 1, 1)));
    assertEquals(4, DateUtils.getPhaseOfMoon(new GregorianCalendar(1, 1, Integer.MIN_VALUE)));
    assertEquals(3, DateUtils.getPhaseOfMoon(new GregorianCalendar(Integer.MIN_VALUE, 0, 1)));
  }

  /**
   * Method under test: {@link DateUtils#parseIso8601Date(String)}
   */
  @Test
  public void testParseIso8601Date() throws ParseException {
    // Arrange and Act
    Date actualParseIso8601DateResult = DateUtils.parseIso8601Date("2020-03-01");

    // Assert
    assertEquals("2020-03-01",
        (new SimpleDateFormat(DateUtils.ISO8601_DATE_PATTERN)).format(actualParseIso8601DateResult));
  }

  /**
   * Method under test: {@link DateUtils#parseIso8601DateTimeOrDate(String)}
   */
  @Test
  public void testParseIso8601DateTimeOrDate() throws ParseException {
    // Arrange and Act
    Date actualParseIso8601DateTimeOrDateResult = DateUtils.parseIso8601DateTimeOrDate("2020-03-01");

    // Assert
    assertEquals("2020-03-01",
        (new SimpleDateFormat(DateUtils.ISO8601_DATE_PATTERN)).format(actualParseIso8601DateTimeOrDateResult));
  }

  /**
   * Method under test: {@link DateUtils#parseLenientDateTime(String)}
   */
  @Test
  public void testParseLenientDateTime() throws ParseException {
    // Arrange, Act and Assert
    assertThrows(ParseException.class, () -> DateUtils.parseLenientDateTime("2020-03-01"));
  }
}

