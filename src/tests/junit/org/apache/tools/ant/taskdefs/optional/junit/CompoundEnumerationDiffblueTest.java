package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import org.junit.Test;

public class CompoundEnumerationDiffblueTest {
  /**
   * Test {@link CompoundEnumeration#hasMoreElements()}.
   * <ul>
   *   <li>Given {@link StringTokenizer#StringTokenizer(String)} with empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompoundEnumeration#hasMoreElements()}
   */
  @Test
  public void testHasMoreElements_givenStringTokenizerWithEmptyString_thenReturnFalse() {
    // Arrange
    CompoundEnumeration<Object> compoundEnumeration = new CompoundEnumeration<>(new StringTokenizer(""));

    // Act and Assert
    assertFalse(compoundEnumeration.hasMoreElements());
  }

  /**
   * Test {@link CompoundEnumeration#hasMoreElements()}.
   * <ul>
   *   <li>Given {@link StringTokenizer#StringTokenizer(String)} with {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompoundEnumeration#hasMoreElements()}
   */
  @Test
  public void testHasMoreElements_givenStringTokenizerWithFoo_thenReturnTrue() {
    // Arrange
    CompoundEnumeration<Object> compoundEnumeration = new CompoundEnumeration<>(new StringTokenizer("foo"));

    // Act and Assert
    assertTrue(compoundEnumeration.hasMoreElements());
  }

  /**
   * Test {@link CompoundEnumeration#hasMoreElements()}.
   * <ul>
   *   <li>Given {@link StringTokenizer#StringTokenizer(String)} with {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompoundEnumeration#hasMoreElements()}
   */
  @Test
  public void testHasMoreElements_givenStringTokenizerWithFoo_thenReturnTrue2() {
    // Arrange
    CompoundEnumeration<Object> compoundEnumeration = new CompoundEnumeration<>(
        new CompoundEnumeration<>(new StringTokenizer("foo")));

    // Act and Assert
    assertTrue(compoundEnumeration.hasMoreElements());
  }

  /**
   * Test {@link CompoundEnumeration#nextElement()}.
   * <ul>
   *   <li>Given {@link StringTokenizer#StringTokenizer(String)} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompoundEnumeration#nextElement()}
   */
  @Test
  public void testNextElement_givenStringTokenizerWithEmptyString() throws NoSuchElementException {
    // Arrange
    CompoundEnumeration<Object> compoundEnumeration = new CompoundEnumeration<>(new StringTokenizer(""));

    // Act and Assert
    assertThrows(NoSuchElementException.class, () -> compoundEnumeration.nextElement());
  }

  /**
   * Test {@link CompoundEnumeration#nextElement()}.
   * <ul>
   *   <li>Given {@link StringTokenizer#StringTokenizer(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompoundEnumeration#nextElement()}
   */
  @Test
  public void testNextElement_givenStringTokenizerWithFoo_thenReturnFoo() throws NoSuchElementException {
    // Arrange
    CompoundEnumeration<Object> compoundEnumeration = new CompoundEnumeration<>(new StringTokenizer("foo"));

    // Act and Assert
    assertEquals("foo", compoundEnumeration.nextElement());
  }

  /**
   * Test {@link CompoundEnumeration#nextElement()}.
   * <ul>
   *   <li>Given {@link StringTokenizer#StringTokenizer(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompoundEnumeration#nextElement()}
   */
  @Test
  public void testNextElement_givenStringTokenizerWithFoo_thenReturnFoo2() throws NoSuchElementException {
    // Arrange
    CompoundEnumeration<Object> compoundEnumeration = new CompoundEnumeration<>(
        new CompoundEnumeration<>(new StringTokenizer("foo")));

    // Act and Assert
    assertEquals("foo", compoundEnumeration.nextElement());
  }
}
