package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class LazyHashtableDiffblueTest {
  /**
   * Test new {@link LazyHashtable} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LazyHashtable}
   */
  @Test
  public void testNewLazyHashtable() {
    // Arrange and Act
    LazyHashtable<Object, Object> actualObjectObjectMap = new LazyHashtable<>();

    // Assert
    assertTrue(actualObjectObjectMap.isEmpty());
  }

  /**
   * Test {@link LazyHashtable#isEmpty()}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor) {@code 42} is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenLazyHashtable42Is42_thenReturnFalse() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertFalse(objectObjectMap.isEmpty());
  }

  /**
   * Test {@link LazyHashtable#isEmpty()}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenLazyHashtable_thenReturnTrue() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();

    // Act and Assert
    assertTrue(objectObjectMap.isEmpty());
  }

  /**
   * Test {@link LazyHashtable#size()}.
   * <p>
   * Method under test: {@link LazyHashtable#size()}
   */
  @Test
  public void testSize() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();

    // Act and Assert
    assertEquals(0, objectObjectMap.size());
  }

  /**
   * Test {@link LazyHashtable#contains(Object)}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor) {@code 42} is {@code 42}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#contains(Object)}
   */
  @Test
  public void testContains_givenLazyHashtable42Is42_when42_thenReturnTrue() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.contains("42"));
  }

  /**
   * Test {@link LazyHashtable#contains(Object)}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor).</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#contains(Object)}
   */
  @Test
  public void testContains_givenLazyHashtable_whenValue_thenReturnFalse() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();

    // Act and Assert
    assertFalse(objectObjectMap.contains("Value"));
  }

  /**
   * Test {@link LazyHashtable#containsKey(Object)}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor) {@code 42} is {@code 42}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey_givenLazyHashtable42Is42_when42_thenReturnTrue() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsKey("42"));
  }

  /**
   * Test {@link LazyHashtable#containsKey(Object)}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor).</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey_givenLazyHashtable_whenValue_thenReturnFalse() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();

    // Act and Assert
    assertFalse(objectObjectMap.containsKey("Value"));
  }

  /**
   * Test {@link LazyHashtable#containsValue(Object)}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor) {@code 42} is {@code 42}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue_givenLazyHashtable42Is42_when42_thenReturnTrue() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsValue("42"));
  }

  /**
   * Test {@link LazyHashtable#containsValue(Object)}.
   * <ul>
   *   <li>Given {@link LazyHashtable} (default constructor).</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue_givenLazyHashtable_whenValue_thenReturnFalse() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();

    // Act and Assert
    assertFalse(objectObjectMap.containsValue("Value"));
  }
}
