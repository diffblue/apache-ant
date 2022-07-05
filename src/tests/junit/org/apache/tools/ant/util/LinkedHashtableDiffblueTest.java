package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class LinkedHashtableDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link LinkedHashtable#LinkedHashtable()}
  *   <li>{@link LinkedHashtable#toString()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals("{}", (new LinkedHashtable<>()).toString());
    assertTrue((new LinkedHashtable<>(1)).isEmpty());
    assertTrue((new LinkedHashtable<>(1, 10.0f)).isEmpty());
    assertTrue((new LinkedHashtable<>(new HashMap<>())).isEmpty());
  }

  /**
   * Method under test: {@link LinkedHashtable#contains(Object)}
   */
  @Test
  public void testContains() {
    // Arrange, Act and Assert
    assertFalse((new LinkedHashtable<>(1)).contains("Value"));
  }

  /**
   * Method under test: {@link LinkedHashtable#contains(Object)}
   */
  @Test
  public void testContains2() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>(1);
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.contains("42"));
  }

  /**
   * Method under test: {@link LinkedHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey() {
    // Arrange, Act and Assert
    assertFalse((new LinkedHashtable<>(1)).containsKey("Value"));
  }

  /**
   * Method under test: {@link LinkedHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey2() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>(1);
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsKey("42"));
  }

  /**
   * Method under test: {@link LinkedHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue() {
    // Arrange, Act and Assert
    assertFalse((new LinkedHashtable<>(1)).containsValue("Value"));
  }

  /**
   * Method under test: {@link LinkedHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue2() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>(1);
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsValue("42"));
  }

  /**
   * Method under test: {@link LinkedHashtable#entrySet()}
   */
  @Test
  public void testEntrySet() {
    // Arrange, Act and Assert
    assertTrue((new LinkedHashtable<>(1)).entrySet().isEmpty());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#equals(Object)}
   * </ul>
   */
  @Test
  public void testEquals() {
    // Arrange, Act and Assert
    assertNotEquals(new LinkedHashtable<>(), null);
    assertNotEquals(new LinkedHashtable<>(), "Different type to LinkedHashtable");
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#hashCode()}
   * </ul>
   */
  @Test
  public void testEquals2() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertEquals(objectObjectMap, objectObjectMap);
    int expectedHashCodeResult = objectObjectMap.hashCode();
    assertEquals(expectedHashCodeResult, objectObjectMap.hashCode());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#hashCode()}
   * </ul>
   */
  @Test
  public void testEquals3() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    LinkedHashtable<Object, Object> objectObjectMap1 = new LinkedHashtable<>();

    // Act and Assert
    assertEquals(objectObjectMap, objectObjectMap1);
    int expectedHashCodeResult = objectObjectMap.hashCode();
    assertEquals(expectedHashCodeResult, objectObjectMap1.hashCode());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#equals(Object)}
   * </ul>
   */
  @Test
  public void testEquals4() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertNotEquals(objectObjectMap, new LinkedHashtable<>());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#hashCode()}
   * </ul>
   */
  @Test
  public void testEquals5() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    LinkedHashtable<Object, Object> objectObjectMap1 = new LinkedHashtable<>();
    objectObjectMap1.put("42", "42");

    // Act and Assert
    assertEquals(objectObjectMap, objectObjectMap1);
    int expectedHashCodeResult = objectObjectMap.hashCode();
    assertEquals(expectedHashCodeResult, objectObjectMap1.hashCode());
  }

  /**
   * Method under test: {@link LinkedHashtable#get(Object)}
   */
  @Test
  public void testGet() {
    // Arrange, Act and Assert
    assertNull((new LinkedHashtable<>(1)).get("42"));
  }

  /**
   * Method under test: {@link LinkedHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty() {
    // Arrange, Act and Assert
    assertTrue((new LinkedHashtable<>(1)).isEmpty());
  }

  /**
   * Method under test: {@link LinkedHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty2() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>(1);
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertFalse(objectObjectMap.isEmpty());
  }

  /**
   * Method under test: {@link LinkedHashtable#keySet()}
   */
  @Test
  public void testKeySet() {
    // Arrange, Act and Assert
    assertTrue((new LinkedHashtable<>(1)).keySet().isEmpty());
  }

  /**
   * Method under test: {@link LinkedHashtable#put(Object, Object)}
   */
  @Test
  public void testPut() {
    // Arrange, Act and Assert
    assertNull((new LinkedHashtable<>(1)).put("42", "42"));
  }

  /**
   * Method under test: {@link LinkedHashtable#remove(Object)}
   */
  @Test
  public void testRemove() {
    // Arrange, Act and Assert
    assertNull((new LinkedHashtable<>(1)).remove("42"));
  }

  /**
   * Method under test: {@link LinkedHashtable#size()}
   */
  @Test
  public void testSize() {
    // Arrange, Act and Assert
    assertEquals(0, (new LinkedHashtable<>(1)).size());
  }

  /**
   * Method under test: {@link LinkedHashtable#values()}
   */
  @Test
  public void testValues() {
    // Arrange, Act and Assert
    assertTrue((new LinkedHashtable<>(1)).values().isEmpty());
  }
}

