package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class LazyHashtableDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link LazyHashtable}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertTrue((new LazyHashtable<>()).isEmpty());
  }

  /**
   * Method under test: {@link LazyHashtable#contains(Object)}
   */
  @Test
  public void testContains() {
    // Arrange, Act and Assert
    assertFalse((new LazyHashtable<>()).contains("Value"));
  }

  /**
   * Method under test: {@link LazyHashtable#contains(Object)}
   */
  @Test
  public void testContains2() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.contains("42"));
  }

  /**
   * Method under test: {@link LazyHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey() {
    // Arrange, Act and Assert
    assertFalse((new LazyHashtable<>()).containsKey("Value"));
  }

  /**
   * Method under test: {@link LazyHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey2() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsKey("42"));
  }

  /**
   * Method under test: {@link LazyHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue() {
    // Arrange, Act and Assert
    assertFalse((new LazyHashtable<>()).containsValue("Value"));
  }

  /**
   * Method under test: {@link LazyHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue2() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsValue("42"));
  }

  /**
   * Method under test: {@link LazyHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty() {
    // Arrange, Act and Assert
    assertTrue((new LazyHashtable<>()).isEmpty());
  }

  /**
   * Method under test: {@link LazyHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty2() {
    // Arrange
    LazyHashtable<Object, Object> objectObjectMap = new LazyHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertFalse(objectObjectMap.isEmpty());
  }

  /**
   * Method under test: {@link LazyHashtable#size()}
   */
  @Test
  public void testSize() {
    // Arrange, Act and Assert
    assertEquals(0, (new LazyHashtable<>()).size());
  }
}

