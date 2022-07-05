package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class DeweyDecimalDiffblueTest {
  /**
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareTo() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(0, parsedJavaVersion.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareTo2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{2, 2, 2, 2});

    // Act and Assert
    assertEquals(1, deweyDecimal.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareTo3() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, deweyDecimal.compareTo(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#compareTo(DeweyDecimal)}
   */
  @Test
  public void testCompareTo4() throws NumberFormatException {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(1, parsedJavaVersion.compareTo(new DeweyDecimal("")));
  }

  /**
   * Method under test: {@link DeweyDecimal#DeweyDecimal(String)}
   */
  @Test
  public void testConstructor() throws NumberFormatException {
    // Arrange, Act and Assert
    assertEquals(1, (new DeweyDecimal("42")).getSize());
    assertThrows(NumberFormatException.class, () -> new DeweyDecimal("42."));
    assertEquals(4, (new DeweyDecimal(new int[]{1, 1, 1, 1})).getSize());
  }

  /**
  * Method under test: {@link DeweyDecimal#equals(Object)}
  */
  @Test
  public void testEquals() {
    // Arrange, Act and Assert
    assertNotEquals(JavaEnvUtils.getParsedJavaVersion(), null);
    assertNotEquals(JavaEnvUtils.getParsedJavaVersion(), "Different type to DeweyDecimal");
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link DeweyDecimal#equals(Object)}
   *   <li>{@link DeweyDecimal#hashCode()}
   * </ul>
   */
  @Test
  public void testEquals2() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(parsedJavaVersion, parsedJavaVersion);
    int expectedHashCodeResult = parsedJavaVersion.hashCode();
    assertEquals(expectedHashCodeResult, parsedJavaVersion.hashCode());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link DeweyDecimal#equals(Object)}
   *   <li>{@link DeweyDecimal#hashCode()}
   * </ul>
   */
  @Test
  public void testEquals3() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();
    DeweyDecimal parsedJavaVersion1 = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertEquals(parsedJavaVersion, parsedJavaVersion1);
    int expectedHashCodeResult = parsedJavaVersion.hashCode();
    assertEquals(expectedHashCodeResult, parsedJavaVersion1.hashCode());
  }

  /**
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals4() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, 1, 1, 1});

    // Act and Assert
    assertNotEquals(deweyDecimal, JavaEnvUtils.getParsedJavaVersion());
  }

  /**
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals5() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{1, 8, 1, 1});

    // Act and Assert
    assertNotEquals(deweyDecimal, JavaEnvUtils.getParsedJavaVersion());
  }

  /**
   * Method under test: {@link DeweyDecimal#equals(Object)}
   */
  @Test
  public void testEquals6() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertNotEquals(deweyDecimal, JavaEnvUtils.getParsedJavaVersion());
  }

  /**
   * Method under test: {@link DeweyDecimal#get(int)}
   */
  @Test
  public void testGet() {
    // Arrange, Act and Assert
    assertEquals(8, JavaEnvUtils.getParsedJavaVersion().get(1));
  }

  /**
   * Method under test: {@link DeweyDecimal#getSize()}
   */
  @Test
  public void testGetSize() {
    // Arrange, Act and Assert
    assertEquals(2, JavaEnvUtils.getParsedJavaVersion().getSize());
  }

  /**
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{2, 2, 2, 2});

    // Act and Assert
    assertFalse(deweyDecimal.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual3() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertFalse(deweyDecimal.isEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isEqual(DeweyDecimal)}
   */
  @Test
  public void testIsEqual4() throws NumberFormatException {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isEqual(new DeweyDecimal("")));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{2, 2, 2, 2});

    // Act and Assert
    assertTrue(deweyDecimal.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan3() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isGreaterThan(new DeweyDecimal(new int[]{2, 2, 2, 2})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan4() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertFalse(deweyDecimal.isGreaterThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThan(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThan5() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isGreaterThan(new DeweyDecimal(new int[]{})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{2, 2, 2, 2});

    // Act and Assert
    assertTrue(deweyDecimal.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual3() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isGreaterThanOrEqual(new DeweyDecimal(new int[]{2, 2, 2, 2})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual4() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertFalse(deweyDecimal.isGreaterThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isGreaterThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsGreaterThanOrEqual5() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isGreaterThanOrEqual(new DeweyDecimal(new int[]{})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{2, 2, 2, 2});

    // Act and Assert
    assertFalse(deweyDecimal.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan3() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isLessThan(new DeweyDecimal(new int[]{2, 2, 2, 2})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan4() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertTrue(deweyDecimal.isLessThan(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThan(DeweyDecimal)}
   */
  @Test
  public void testIsLessThan5() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isLessThan(new DeweyDecimal(new int[]{})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual2() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{2, 2, 2, 2});

    // Act and Assert
    assertFalse(deweyDecimal.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual3() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertTrue(parsedJavaVersion.isLessThanOrEqual(new DeweyDecimal(new int[]{2, 2, 2, 2})));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual4() {
    // Arrange
    DeweyDecimal deweyDecimal = new DeweyDecimal(new int[]{});

    // Act and Assert
    assertTrue(deweyDecimal.isLessThanOrEqual(JavaEnvUtils.getParsedJavaVersion()));
  }

  /**
   * Method under test: {@link DeweyDecimal#isLessThanOrEqual(DeweyDecimal)}
   */
  @Test
  public void testIsLessThanOrEqual5() {
    // Arrange
    DeweyDecimal parsedJavaVersion = JavaEnvUtils.getParsedJavaVersion();

    // Act and Assert
    assertFalse(parsedJavaVersion.isLessThanOrEqual(new DeweyDecimal(new int[]{})));
  }
}

