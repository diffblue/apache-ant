package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class TimeComparisonDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TimeComparison#TimeComparison()}
   *   <li>{@link TimeComparison#getValues()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    TimeComparison actualTimeComparison = new TimeComparison();
    String[] actualValues = actualTimeComparison.getValues();

    // Assert
    assertNull(actualTimeComparison.getValue());
    assertEquals(-1, actualTimeComparison.getIndex());
    assertArrayEquals(new String[]{"before", "after", "equal"}, actualValues);
  }

  /**
   * Test {@link TimeComparison#TimeComparison(String)}.
   * <ul>
   *   <li>When {@code before}.</li>
   *   <li>Then return Value is {@code before}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#TimeComparison(String)}
   */
  @Test
  public void testNewTimeComparison_whenBefore_thenReturnValueIsBefore() {
    // Arrange and Act
    TimeComparison actualTimeComparison = new TimeComparison("before");

    // Assert
    assertEquals("before", actualTimeComparison.getValue());
    assertEquals(0, actualTimeComparison.getIndex());
    assertArrayEquals(new String[]{"before", "after", "equal"}, actualTimeComparison.getValues());
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#AFTER}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenAfter_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TimeComparison.AFTER.evaluate(1L, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#AFTER}.</li>
   *   <li>When {@link Resource#UNKNOWN_SIZE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenAfter_whenUnknown_size_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TimeComparison.AFTER.evaluate(Resource.UNKNOWN_SIZE, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#BEFORE}.</li>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenBefore_whenMax_value_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TimeComparison.BEFORE.evaluate(Long.MAX_VALUE, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#BEFORE}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenBefore_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TimeComparison.BEFORE.evaluate(1L, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#EQUAL}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenEqual_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TimeComparison.EQUAL.evaluate(1L, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#EQUAL}.</li>
   *   <li>When {@link Resource#UNKNOWN_SIZE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenEqual_whenUnknown_size_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TimeComparison.EQUAL.evaluate(Resource.UNKNOWN_SIZE, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>Given {@link TimeComparison#TimeComparison()}.</li>
   *   <li>When one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long, long)}
   */
  @Test
  public void testEvaluateWithT1T2G_givenTimeComparison_whenOne_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TimeComparison()).evaluate(1L, 1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#AFTER}.</li>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenAfter_whenMax_value_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TimeComparison.AFTER.evaluate(Long.MAX_VALUE, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#AFTER}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenAfter_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TimeComparison.AFTER.evaluate(1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#BEFORE}.</li>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenBefore_whenMax_value_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TimeComparison.BEFORE.evaluate(Long.MAX_VALUE, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#BEFORE}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenBefore_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TimeComparison.BEFORE.evaluate(1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#EQUAL}.</li>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenEqual_whenMax_value_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TimeComparison.EQUAL.evaluate(Long.MAX_VALUE, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#EQUAL}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenEqual_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TimeComparison.EQUAL.evaluate(1L, 1L));
  }

  /**
   * Test {@link TimeComparison#evaluate(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>Given {@link TimeComparison#TimeComparison()}.</li>
   *   <li>When one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#evaluate(long, long)}
   */
  @Test
  public void testEvaluateWithT1T2_givenTimeComparison_whenOne_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TimeComparison()).evaluate(1L, 1L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long, long)}
   */
  @Test
  public void testCompareWithT1T2G_whenFive_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(5L, 5L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long, long)}
   */
  @Test
  public void testCompareWithT1T2G_whenOne_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(1L, 5L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>When {@link Resource#UNKNOWN_DATETIME}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long, long)}
   */
  @Test
  public void testCompareWithT1T2G_whenUnknown_datetime_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(Resource.UNKNOWN_DATETIME, 5L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long, long)} with {@code t1}, {@code t2}, {@code g}.
   * <ul>
   *   <li>When {@link Resource#UNKNOWN_SIZE}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long, long)}
   */
  @Test
  public void testCompareWithT1T2G_whenUnknown_size_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1, TimeComparison.compare(Resource.UNKNOWN_SIZE, 5L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long)}
   */
  @Test
  public void testCompareWithT1T2_whenFive_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(5L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long)}
   */
  @Test
  public void testCompareWithT1T2_whenMax_value_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, TimeComparison.compare(Long.MAX_VALUE, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>When one thousand.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long)}
   */
  @Test
  public void testCompareWithT1T2_whenOneThousand_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(1000L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long)}
   */
  @Test
  public void testCompareWithT1T2_whenOne_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(1L, 5L));
  }

  /**
   * Test {@link TimeComparison#compare(long, long)} with {@code t1}, {@code t2}.
   * <ul>
   *   <li>When {@link Resource#UNKNOWN_SIZE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TimeComparison#compare(long, long)}
   */
  @Test
  public void testCompareWithT1T2_whenUnknown_size_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TimeComparison.compare(Resource.UNKNOWN_SIZE, 5L));
  }
}
