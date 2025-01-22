package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import org.junit.Test;

public class DateUtilsDiffblueTest {
  /**
   * Test {@link DateUtils#format(Date, String)} with {@code Date}, {@code String}.
   * <ul>
   *   <li>Then return {@code +}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateUtils#format(Date, String)}
   */
  @Test
  public void testFormatWithDateString_thenReturnPlusSign() {
    // Arrange, Act and Assert
    assertEquals("+",
        DateUtils.format(Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()), "+"));
  }

  /**
   * Test {@link DateUtils#format(long, String)} with {@code long}, {@code String}.
   * <ul>
   *   <li>When {@code +}.</li>
   *   <li>Then return {@code +}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateUtils#format(long, String)}
   */
  @Test
  public void testFormatWithLongString_whenPlusSign_thenReturnPlusSign() {
    // Arrange, Act and Assert
    assertEquals("+", DateUtils.format(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, "+"));
  }

  /**
   * Test {@link DateUtils#formatElapsedTime(long)}.
   * <p>
   * Method under test: {@link DateUtils#formatElapsedTime(long)}
   */
  @Test
  public void testFormatElapsedTime() {
    // Arrange, Act and Assert
    assertEquals("0 seconds", DateUtils.formatElapsedTime(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link DateUtils#getPhaseOfMoon(Calendar)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateUtils#getPhaseOfMoon(Calendar)}
   */
  @Test
  public void testGetPhaseOfMoon_thenReturnThree() {
    // Arrange, Act and Assert
    assertEquals(3, DateUtils.getPhaseOfMoon(new GregorianCalendar(Integer.MIN_VALUE, 0, 1)));
  }

  /**
   * Test {@link DateUtils#getPhaseOfMoon(Calendar)}.
   * <ul>
   *   <li>When {@link GregorianCalendar#GregorianCalendar(int, int, int)} with one and one and {@link Integer#MIN_VALUE}.</li>
   *   <li>Then return four.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateUtils#getPhaseOfMoon(Calendar)}
   */
  @Test
  public void testGetPhaseOfMoon_whenGregorianCalendarWithOneAndOneAndMin_value_thenReturnFour() {
    // Arrange, Act and Assert
    assertEquals(4, DateUtils.getPhaseOfMoon(new GregorianCalendar(1, 1, Integer.MIN_VALUE)));
  }

  /**
   * Test {@link DateUtils#getPhaseOfMoon(Calendar)}.
   * <ul>
   *   <li>When {@link GregorianCalendar#GregorianCalendar(int, int, int)} with one and one and one.</li>
   *   <li>Then return four.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateUtils#getPhaseOfMoon(Calendar)}
   */
  @Test
  public void testGetPhaseOfMoon_whenGregorianCalendarWithOneAndOneAndOne_thenReturnFour() {
    // Arrange, Act and Assert
    assertEquals(4, DateUtils.getPhaseOfMoon(new GregorianCalendar(1, 1, 1)));
  }

  /**
   * Test {@link DateUtils#parseIso8601Date(String)}.
   * <p>
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
   * Test {@link DateUtils#parseIso8601DateTimeOrDate(String)}.
   * <p>
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
   * Test {@link DateUtils#parseLenientDateTime(String)}.
   * <ul>
   *   <li>When {@code 2020-03-01}.</li>
   *   <li>Then throw {@link ParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateUtils#parseLenientDateTime(String)}
   */
  @Test
  public void testParseLenientDateTime_when20200301_thenThrowParseException() throws ParseException {
    // Arrange, Act and Assert
    assertThrows(ParseException.class, () -> DateUtils.parseLenientDateTime("2020-03-01"));
  }
}
