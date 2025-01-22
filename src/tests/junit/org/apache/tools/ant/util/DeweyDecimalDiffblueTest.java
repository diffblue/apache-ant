package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class DeweyDecimalDiffblueTest {
  /**
   * Test {@link DeweyDecimal#DeweyDecimal(int[])}.
   * <p>
   * Method under test: {@link DeweyDecimal#DeweyDecimal(int[])}
   */
  @Test
  public void testNewDeweyDecimal() {
    // Arrange, Act and Assert
    assertEquals(4, (new DeweyDecimal(new int[]{1, 0, 1, 0})).getSize());
  }

  /**
   * Test {@link DeweyDecimal#DeweyDecimal(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return Size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#DeweyDecimal(String)}
   */
  @Test
  public void testNewDeweyDecimal_when42_thenReturnSizeIsOne() throws NumberFormatException {
    // Arrange, Act and Assert
    assertEquals(1, (new DeweyDecimal("42")).getSize());
  }

  /**
   * Test {@link DeweyDecimal#DeweyDecimal(String)}.
   * <ul>
   *   <li>When {@code 42.}.</li>
   *   <li>Then throw {@link NumberFormatException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#DeweyDecimal(String)}
   */
  @Test
  public void testNewDeweyDecimal_when42_thenThrowNumberFormatException() throws NumberFormatException {
    // Arrange, Act and Assert
    assertThrows(NumberFormatException.class, () -> new DeweyDecimal("42."));
  }

  /**
   * Test {@link DeweyDecimal#getSize()}.
   * <p>
   * Method under test: {@link DeweyDecimal#getSize()}
   */
  @Test
  public void testGetSize() {
    // Arrange, Act and Assert
    assertEquals(1, JavaEnvUtils.getParsedJavaVersion().getSize());
  }

  /**
   * Test {@link DeweyDecimal#get(int)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is array of {@code int} with one and zero.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#get(int)}
   */
  @Test
  public void testGet_givenDeweyDecimalWithComponentsIsArrayOfIntWithOneAndZero_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new DeweyDecimal(new int[]{1, 0, 1, 0})).get(1));
  }

  /**
   * Test {@link DeweyDecimal#isEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(
        new int[]{JavaEnvUtils.VERSION_1_7, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertFalse(deweyDecimal.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is array of {@code int} with one and {@link JavaEnvUtils#VERSION_1_7}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual_givenDeweyDecimalWithComponentsIsArrayOfIntWithOneAndVersion_1_7() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertFalse(deweyDecimal.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is empty array of {@code int}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual_givenDeweyDecimalWithComponentsIsEmptyArrayOfInt_thenReturnFalse() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertFalse(deweyDecimal.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given ParsedJavaVersion.</li>
   *   <li>When ParsedJavaVersion.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual_givenParsedJavaVersion_whenParsedJavaVersion_thenReturnTrue() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThan(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(
        new int[]{JavaEnvUtils.VERSION_1_7, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertFalse(deweyDecimal.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThan(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is array of {@code int} with one and {@link JavaEnvUtils#VERSION_1_7}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan_givenDeweyDecimalWithComponentsIsArrayOfIntWithOneAndVersion_1_7() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertTrue(deweyDecimal.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThan(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is empty array of {@code int}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan_givenDeweyDecimalWithComponentsIsEmptyArrayOfInt_thenReturnTrue() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertTrue(deweyDecimal.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThan(DeweyDecimal)}.
   * <ul>
   *   <li>Given ParsedJavaVersion.</li>
   *   <li>When ParsedJavaVersion.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan_givenParsedJavaVersion_whenParsedJavaVersion_thenReturnFalse() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThan(DeweyDecimal)}.
   * <ul>
   *   <li>When {@link DeweyDecimal#DeweyDecimal(int[])} with components is array of {@code int} with one and {@link JavaEnvUtils#VERSION_1_7}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan_whenDeweyDecimalWithComponentsIsArrayOfIntWithOneAndVersion_1_7() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion
        .isLessThan(new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7})));
  }

  /**
   * Test {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertTrue(deweyDecimal.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual2() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion
        .isLessThanOrEqual(new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7})));
  }

  /**
   * Test {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual3() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(
        new int[]{JavaEnvUtils.VERSION_1_7, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertFalse(deweyDecimal.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is empty array of {@code int}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual_givenDeweyDecimalWithComponentsIsEmptyArrayOfInt() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertTrue(deweyDecimal.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given ParsedJavaVersion.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual_givenParsedJavaVersion_thenReturnTrue() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertFalse(deweyDecimal.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan2() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion
        .isGreaterThan(new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7})));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan3() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(
        new int[]{JavaEnvUtils.VERSION_1_7, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertTrue(deweyDecimal.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is empty array of {@code int}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan_givenDeweyDecimalWithComponentsIsEmptyArrayOfInt() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertFalse(deweyDecimal.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}.
   * <ul>
   *   <li>Given ParsedJavaVersion.</li>
   *   <li>When ParsedJavaVersion.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan_givenParsedJavaVersion_whenParsedJavaVersion_thenReturnFalse() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertFalse(deweyDecimal.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual2() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion
        .isGreaterThanOrEqual(new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7})));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}.
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual3() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(
        new int[]{JavaEnvUtils.VERSION_1_7, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertTrue(deweyDecimal.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given {@link DeweyDecimal#DeweyDecimal(int[])} with components is empty array of {@code int}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual_givenDeweyDecimalWithComponentsIsEmptyArrayOfInt() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertFalse(deweyDecimal.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}.
   * <ul>
   *   <li>Given ParsedJavaVersion.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual_givenParsedJavaVersion_thenReturnTrue() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#compareTo(DeweyDecimal)} with {@code DeweyDecimal}.
   * <ul>
   *   <li>Given ParsedJavaVersion.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareToWithDeweyDecimal_givenParsedJavaVersion_thenReturnZero() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(0, parsedJavaVersion.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#compareTo(DeweyDecimal)} with {@code DeweyDecimal}.
   * <ul>
   *   <li>Then return minus seventeen.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareToWithDeweyDecimal_thenReturnMinusSeventeen() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertEquals(-17, deweyDecimal.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#compareTo(DeweyDecimal)} with {@code DeweyDecimal}.
   * <ul>
   *   <li>Then return minus sixteen.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareToWithDeweyDecimal_thenReturnMinusSixteen() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertEquals(-16, deweyDecimal.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#compareTo(DeweyDecimal)} with {@code DeweyDecimal}.
   * <ul>
   *   <li>Then return {@link JavaEnvUtils#VERSION_1_7}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareToWithDeweyDecimal_thenReturnVersion_1_7() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(
        new int[]{JavaEnvUtils.VERSION_1_7, JavaEnvUtils.VERSION_1_7, 1, JavaEnvUtils.VERSION_1_7});

    // Act and Assert
    assertEquals(JavaEnvUtils.VERSION_1_7, deweyDecimal.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}, and {@link DeweyDecimal#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DeweyDecimal#equals(Object)}
   *   <li>{@link DeweyDecimal#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();
    DeweyDecimal parsedJavaVersion2 = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(parsedJavaVersion, parsedJavaVersion2);
    int expectedHashCodeResult = parsedJavaVersion.hashCode();
    assertEquals(expectedHashCodeResult, parsedJavaVersion2.hashCode());
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}, and {@link DeweyDecimal#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DeweyDecimal#equals(Object)}
   *   <li>{@link DeweyDecimal#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(parsedJavaVersion, parsedJavaVersion);
    int expectedHashCodeResult = parsedJavaVersion.hashCode();
    assertEquals(expectedHashCodeResult, parsedJavaVersion.hashCode());
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, 0, 1, 0});

    // Act and Assert
    assertNotEquals(deweyDecimal, JavaEnvUtils.getParsedJavaVersion());
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{JavaEnvUtils.VERSION_1_7, 0, 1, 0});

    // Act and Assert
    assertNotEquals(deweyDecimal, JavaEnvUtils.getParsedJavaVersion());
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertNotEquals(deweyDecimal, JavaEnvUtils.getParsedJavaVersion());
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(JavaEnvUtils.getParsedJavaVersion(), null);
  }

  /**
   * Test {@link DeweyDecimal#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(JavaEnvUtils.getParsedJavaVersion(), "Different type to DeweyDecimal");
  }
}
